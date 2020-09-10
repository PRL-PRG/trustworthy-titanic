
# load packages for visualization 
library('ggplot2')
#install.packages('ggthemes')
library('ggthemes')
library('scales')
library('dplyr')
#install.packages("mice")
library('mice')
library('randomForest')

# load classifier package 
install.packages("randomForest")

## LOADING THE DATA ### 

train <- read.csv("train.csv")
test <- read.csv("test.csv")

                ## DATA PREPROCESSING ##
                        ###########
bindata <- bind_rows(train, test)
str(bindata)
                ## Analyzing the names ## 

bindata$Title <- gsub('(.*,)|(\\..*)','', bindata$Name)
#table(bindata$Sex, bindata$Title

' Some of the titles are very less frequent than the others'
        'combine them in a single vector'

rare_title <- c('Dona', 'Lady', 'the Countess',
                          'Capt', 'Col', 'Don', 
                          'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

        #note: MLLE - MISS ( French equivalent)
         #       MME - Mrs. (French)
          #      MS - MISS(Other abbreviation)
bindata$Title[bindata$Title== 'Mlle'] <- 'Miss'
table(bindata$Sex, bindata$Title)

        #### Pulling in the surnames 
bindata$Surname <- sapply(bindata$Name, function(x)
                        strsplit(x, split='[,.]')[[1]][[1]])

unique_surname <- nlevels(factor(bindata$Surname))
# at this point we have 875 unique surnames

        ########### ANALYZING FAMILIES ########
  # to get family size, parch + sibsp 
bindata$famsize <- bindata$SibSp + bindata$Parch

bindata$family <- paste(bindata$Surname, bindata$famsize, sep = ' ')

        # use ggplot2 to visualize relations between fami size and survival
ggplot(bindata[1:891, ], aes(x= famsize, fill=factor(Survived)))+
        geom_bar(state='count', position='dodge')+
        scale_x_continuous(breaks=c(1:11)) +
        labs(x='Fam size')+
        theme_few()
# OBS: Solo people and large families had a less survival 

        ##### ANALYZING THE DECK ##### 

bindata$Cabin   # we can see that Cabin has a lot of missing data 

                first character is the number of the Deck actually 
strsplit(bindata$Cabin[2], NULL)[[1]] # "C" "8" "5"
        # C is the deck number 
bindata$Cabin[10]  # is empty
                # we create a deck variable to store decks for all passengers
bindata$Deck <- factor(sapply(bindata$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

bindata$Embarked[62] # empty 
bindata$Pclass[830]   
                ## nothing is missing 
                # use GGPLOT2 to visualize embarkemtn and PClass and median fare
ggplot(bindata, aes(x=Embarked, y=Fare, fill=factor(Pclass)))+
        geom_boxplot()+
        geom_hline(aes(yintercept=90),
                   colour='red', linetype='dashed', lwd=2)+
        scale_y_continuous(labels=dollar_format())+
        theme_few()
        
        #obs: median fare for a first class passenger departing from
        # charbourg coincides with $80 paid by embarkment deficit passengers
        # thus, we can replace the missing values for embarkment with C
bindata$Embarked[c(62, 830)]<- 'C'

                # ANALYZING FARE 
is.na(bindata$Fare)
# passenger sitting at row 1044 has fare value NA
bindata[1044,] 
        # pclass = 3
        # Embarked = 'S'   Southampton
ggplot(bindata[bindata$Pclass=='3'& bindata$Embarked=='S',],
       aes(x=Fare))+
        geom_density(fill='#98d6ff')+
        geom_vline(aes(xintercept=median(Fare,na.rm=T)),
                   colour='red', linetype='dashed',lwd=1)+
        scale_x_continuous(labels=dollar_format())+
        theme_few()
myclass <- bindata[bindata$Pclass=='3'& bindata$Embarked=='S',]
median_fare <-median(myclass$Fare,na.rm=T)

        # thus median fare for pclass 3 and embarked S is 8.05
        # can replace missing fare values with 8.05 

# thus, 
bindata$Fare[1044]<-median_fare
                
        ## Analyzing fare concludes ### 

#### ## # # MAKING PREDICTIVE INROADS ####### 
is.na(bindata$Age)
        # Age has several NULL values within it. 
sum(is.na(bindata$Age))
                # using mice imputattion

factors_used<-c('PassengerId','Pclass','Sex','Embarked',
                'Title','family')
bindata[factors_used]<-lapply(bindata[factors_used], function(x) as.factor(x))
set.seed(20)

#performing mice imputation filtering out the non-usefull daata

mice_impute<-mice(bindata[,!names(bindata) %in% c('PassengerId', 'Name', 'Ticket',
                                                  'Cabin', 'Family', 'Surname',
                                                  'Survived')], method='rf')

mice_output <- complete(mice_impute)
        #compare the actual and predicted values of age 
par(mfrow=c(1,2))
hist(bindata$Age, freq=F, col='darkblue', ylim=c(0, 0.04), main='Actual age values')

hist(mice_output$Age, freq=F, main='Mice output', 
     col='lightblue', ylim=c(0,0.04))

        ## not any striking differences 
        # replace age vector with mice output 
bindata$Age<- mice_output$Age
sum(is.na(bindata$Age)) # 0 
        # NO MISSING VALUES IN AGE NOW 

                ### feature engineering ROUND 2 

## create child and mother 
ggplot(bindata[1:891,], aes(Age, fill=factor(Survived)))+
        geom_histogram()+
        facet_grid(.~Sex)+
        theme_few()
                # create children 
bindata$Child[bindata$Age<18]<-'Child'
bindata$Child[bindata$Age>=18]<-'Adult'

table(bindata$Child, bindata$Survived)
                # create mothers 

bindata$Mother <- 'Not Mother'
bindata$Mother[bindata$Sex=='female'& bindata$Parch>0 &
                       bindata$Age >  18 & bindata$Title!='Miss']<-'Mother'

table(bindata$Mother, bindata$Survived)

        # factorize both child and mother
bindata$Child <- factor(bindata$Child)
bindata$Mother <- factor(bindata$Mother)

md.pattern(bindata)

                ##### PREDICTION ##### 
# step 1: split back to train test
train<- bindata[1:891,]
test <- bindata[892:1309,]
sum(is.na(train))

#step 2: build the model; no point in using all variables
set.seed(120)
model <- randomForest(factor(Survived)~Pclass+Sex+
        Age+SibSp+Parch+Fare+Embarked+Title+Child+Mother
        , data=train)

# calc. model error rate
error_rate <- model$err.rate
plot(model, ylim=c(0, 0.36))

legend('topright',colnames(error_rate), col=1:3, fill = 1:3)

## Importance of variables 
importance <- importance(model)

        # FINAL PREDICTION ON TEST FILE
predict_final <- predict(model, test)

result <- data.frame(PassengerID = test$PassengerId, Survived = predict_final)

write.csv(result, file='model_prediction.csv', row.names= F)

