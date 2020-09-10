
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(dplyr)
library(ggplot2)
library(VIM)
library(stringr)
library("ggthemes")
library("randomForest")
library("ROCR")
library("mice")
library('scales')
# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv", stringsAsFactors=F)
test  <- read.csv("../input/test.csv", stringsAsFactors=F)

glimpse(train)
glimpse(test)

#############################
#When column-binding, rows are matched by position,
#not value so all data frames must have the same number of rows.
#To match by value, not position, see left_join etc. When row-binding,
#columns are matched by name, and any values that don't 
#match will be filled with NA
##############################

data1<-bind_rows(train, test)

##data1$Survived[is.na(data1$Survived)]<-0

visual_na<- aggr(data1, numbers=TRUE, labels=names(data1),cex.axis=.5, cex.numbers=.5, sortVars=T)

##############
#ok there is lot of NA in Cabin & Deck
#whats going on
##############

data1$Cabin[data1$Cabin=='']<-NA
sum(is.na(data1$Cabin))

str(data1)

#############
#So we given 0 for all test$Survived
#meaning they are all dead
###############

#############
#ok lets tackkle names
#I find 2 things interest
#their surname &
#their title
##############

#############
#ok first lets extract
#title from their names
#############


data1$Title <- gsub('(.*,)|(\\..*)', '', data1$Name)
data1$Title<-str_trim(data1$Title)

distinct(data1, Title)

################
#Ok we have 19 distinct Titles
################

data1$Title <- gsub('Ms', 'Miss', data1$Title, ignore.case = T)

data1$Title <- gsub('Mlle', 'Miss', data1$Title, ignore.case = T)

no_title <- c('Master', 'Don', 'Rev', 'Dr', 'Mme', 'Major', 'Lady', 'Sir', 'Col', 'Capt', 'the Countess', 'Jonkheer', 'Dona')

data1$Title[data1$Title %in% no_title]<-'No Title'

distinct(data1, Title)
####################
#ok we have now only 4
#titles
###################

data1$Surname <- sapply(data1$Name, function(x) strsplit(x,'[,.]' )[[1]][1])

#################
#lets sort out guy without fare
#since only 1 person in all data with no fare
#################


filter(data1, is.na(Fare))



str(data1)

data1$Cabin_class <- sapply(data1$Cabin, function(x) substr(x, 1,1)[[1]][1])

#################
#this dude has fare value missing but we know he has Pclass 3 &
# Embarked from S
################

distinct(data1, Embarked)

#################################
#Hist doest work; 
###ggplot(filter(data1, Pclass==3 & Embarked=='S'), aes(Fare))+geom_histogram(bins=20, na.rm = T)+
 ##       geom_vline(aes(xintercept=mean(Fare, na.rm=T)))+
 ###       scale_x_continuous(labels=dollar_format())
#####################################

ggplot(filter(data1, Pclass==3 & Embarked=='S'), aes(Fare))+
        geom_density(fill='green', alpha=.5)+
        scale_x_continuous(labels=dollar_format(),breaks = c(0,10,20,30,40,50,60,70))+
        geom_vline(aes(xintercept=median(Fare, na.rm=TRUE)), color='red', linetype='dashed')

data1[is.na(data1$Fare),]
#####################
# so now we can confirm row no. 1044 matches passenger ID
#so safe to use code below
####################

data1[is.na(data1$Fare),]$Fare<- median(data1[data1$Pclass==3 & data1$Embarked=='S',]$Fare, na.rm=TRUE)

data1[1044,]

sum(is.na(data1$Embarked))
glimpse(data1)
summary(as.factor(data1$Embarked))

#####################
#I should have made empty cells all NA in the beginning
# anyways here we go
######################

data1[data1==''] <-NA

#####################
#Now we can clearly see NAs in Embarked
####################

filter(data1, is.na(Embarked))

######################
#2 girls with no Embarked value but they have Fare and Cabin_class common
#####################

ggplot(data1, aes(Embarked, Fare))+geom_boxplot()+geom_hline(aes(yintercept=80), col="red", linetype= 'dashed', lwd=.5)+
        facet_grid(~Pclass)+theme_bw()

median(data1[data1$Embarked=='C' & data1$Pclass==1,]$Fare, na.rm = T)

###################
# the median Fare for people with Pclass 1 with Embarked C is $77 which is close to what
# those two girls payed 
#based on those estimates I think they embarked from C
###################

data1[data1$PassengerId==62 | data1$PassengerId==830,]$Embarked<- 'C'
data1[data1$PassengerId==62 | data1$PassengerId==830,]



########################
#lets look at family size
#sibsp           Number of Siblings/Spouses Aboard
#parch           Number of Parents/Children Aboard
#so combing those 2 will give us family size
#########################

data1$Family_size <-data1$SibSp+data1$Parch+1


#######################
#now we got many Age with NA almost 20 % of all age
#& lots of Cabin_class with NAs
# we need strong statistical inference method to predict those values
##########################


##all age variable from test data only

str(data1)

data1$Sex <- as.factor(data1$Sex)
data1$Ticket <-as.factor(data1$Ticket)
data1$Title <-as.factor(data1$Title)
data1$Embarked <-as.factor(data1$Embarked)
data1$Survived <-as.factor(data1$Survived)
data1$Pclass <-as.factor(data1$Pclass)
data1$Family_size <-as.factor(data1$Family_size)
################################
####library(help="datasets") 
#see all inbuilt datasets for future reference
#################################



############################################################################################

#train_data <- data1[1:891,]
#test_data <- data1[892:1309,]
#test_data$Survived<-NA

###################


##########################################################################################

sum(is.na(data1$Age))
##263 NAs 

########################
#lets explore whats going on with age 
########################

hist(data1$Age, na.rm=T , freq=F)
summary(data1$Age)

####how can age be 0

data1 %>% filter(Age<0)

##############
#ok its all good
#############


summary(data1)
#################
#lets explore fare of $0
################

data1 %>% filter(Fare<5)

data1[data1$Fare<5,]

distinct(data1, Ticket)
ggplot(data1[1:418,], aes(Embarked, Fare))+geom_boxplot()


train %>% group_by(Embarked, Pclass) %>% summarise(median(Fare))

#####################################::::::::::::::::
#ok lots of $0 Fare
#we'll save this work for later
######################################:::::::::::::::

visual_na2<- aggr(data1[892:1309,], numbers=TRUE, labels=names(data1),cex.axis=.5, cex.numbers=.5, sortVars=T)




age_pred <- lm(Age~Fare+Sex+Pclass, data=data1[!is.na(data1$Age),])
age_pred
summary(age_pred)

#train_data$Age[is.na(train_data$Age)] <- predict(age_pred, train_data)[is.na(train_data$Age)]
#train_data$Age <- round(train_data$Age)

data1$Age[is.na(data1$Age)]<-predict(age_pred, data1)[is.na(data1$Age)]
data1$Age <-round(data1$Age)

train_data <- data1[1:891,]
test_data <- data1[892:1309,]

set.seed(007)
str(train_data)
model <- randomForest(factor(Survived)~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Family_size, data=train_data)

model$importance

plot(model)


importance <- importance(model)
VarImportance <- data.frame(Variables=row.names(importance),
                            Importance=round(importance[,'MeanDecreaseGini'],2))


rank_importance <- VarImportance %>% mutate(Rank=paste0('#', dense_rank(desc(Importance))))
rank_importance

ggplot(rank_importance, aes(x=reorder(Variables, Importance), y=Importance, fill=Importance ))+
        geom_bar(stat = 'identity')+coord_flip()+geom_text(aes(x=Variables, label=Rank, y=5))+
        labs(x='Variables')





# Predict using the test set
prediction <- predict(model, test_data)
summary(prediction)
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test_data$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'test_solution2.csv', row.names = F)





