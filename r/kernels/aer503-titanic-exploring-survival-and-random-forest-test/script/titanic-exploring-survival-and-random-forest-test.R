
#my submission for Random Forest Titanic
#I'm new to R. Please do not judge strictly. Any comments, help, tips will be highly appreciated.
#My hypothesis is that married women who were travled first class had better chance to survive.
#Let's see what our prediction results will show us.
library(dplyr)
library(randomForest)
library(rpart)
library(ggplot2)


# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

set.seed(111)
#merge into a dataframe
train$Pclass <- as.numeric(as.character(train$Pclass))
test$Pclass <-as.numeric(as.character(test$Pclass))
all_data <- bind_rows(train,test)
all_data<-as.data.frame(all_data,na.rm=TRUE)
str(all_data) #Used for finding the all information
names(all_data)#Used for finding the header names

#let us see what we have
ggplot(all_data,  aes(x = factor("Survived"), fill = Sex) )+ 
  geom_bar()

#check data frame for missinf data
is.na(all_data)
#we have missed some data in Fare 
is.na(all_data$Fare[1044])#cheaking for missing data 
#this is TRUE, passenger on row 1044 has NA. We can replace it with the median Fare value.
all_data$Fare[1044]<- median(all_data$Fare, na.rm=TRUE)

#cheaking for missing data in Embarked
is.na(all_data$Embarked)
is.na(all_data$Embarked[c(62,830)])
# we give them "S", relying on the majority
all_data$Pclass<-as.factor(all_data$Pclass)
all_data$Embarked[c(62,830)]<-"S"
all_data$Embarked<-factor(all_data$Embarked)#very important step to factorize embarkment codes 
#(if you factorize it. Thus, the code will work, even with missing factors, which were letters "S", "N", e.i. characters )
#We also have some missing data in Age
# we use prediction method as decision-tree with method "anova" for continious variable, to fill it
prediction_age <- rpart(Age~Pclass+Sex+SibSp+Parch+Fare+Embarked, data=all_data[!is.na(all_data$Age),], 
                        method="anova")
all_data$Age[is.na(all_data$Age)]<-predict(prediction_age, all_data[is.na(all_data$Age),])
#to cheak all colon Age for missing data, after our prediction, we choose range of rows
is.na(all_data$Age[c(1:750)])
is.na(all_data$Age[c(751:1309)])
is.na(all_data[1:300,])
is.na(all_data[,1:3])
# let's create new column Title by grabing title from passenger Names  
all_data$Title <- gsub('(.*, )|(\\..*)', '', all_data$Name)
table(all_data$Title)
#now we have new column 
str(all_data)
#we can visualize it as ggplot
ggplot(all_data, aes(Title, fill = Title ) ) +
  geom_bar()
#Let's see what we have regarding our hypothesis 
table(all_data$Sex, all_data$Title)
table(all_data$Sex, all_data$Pclass)
table(all_data$Pclass, all_data$Title)

# to clarify the information we have got, let's reassign some specific titles to their English equvalent
all_data$Title[all_data$Title == 'Mlle']        <- 'Miss' 
all_data$Title[all_data$Title == 'Ms']          <- 'Miss'
all_data$Title[all_data$Title == 'Mme']         <- 'Mrs' 
all_data$Title[all_data$Title == 'Dona']         <- 'Mrs'
all_data$Title[all_data$Title == 'Don']          <- 'Mr'
# for Passenger, where we are not sure about there marriege status let's create "question_status"
question_status <- c('Dr', 'Lady', 'the Countess','Capt', 'Col', 
                'Major', 'Rev', 'Sir', 'Jonkheer')
all_data$Title[all_data$Title %in% question_status]  <- 'question status'
#let's see our plot now
table(all_data$Title, all_data$Sex)
ggplot(all_data, aes(Title, fill = Title ) ) +
  geom_bar()
#We can clearly see that there were comparatively more married men  than married women. 
#However, let's see who could survive. Is there a regularity?

#let's create new virable , such as Married_Woman 
all_data$Married_Woman <- 'Not Married'
all_data$Married_Woman[all_data$Sex == 'female' & all_data$Title != 'Mrs'] <- 'Married_Woman'
str(all_data)
table(all_data$Sex, all_data$Married_Woman)

#don't forget to factorize new virables and Survived (if didn't before)  
all_data$Survived<-as.factor(all_data$Survived)
all_data$Title<-as.factor(all_data$Title)
all_data$Married_Woman<-as.factor(all_data$Married_Woman)
str(all_data)
#split data again to train and test
train_again<-all_data[1:891,]
test_again<-all_data[892:1309,]

#Random Forest Algorithm
set.seed(112)
my_forest<-randomForest(as.factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Married_Woman,
                        data=train_again, importance=TRUE, ntree=1000)

#do the prediction
my_prediction<- predict(my_forest, newdata = test_again)
# get importance
importance    <- importance(my_forest)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
#check the importance
varImpPlot(my_forest)
# create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()
#create a data frame with two columns PassengerID&Survived
my_solution<-data.frame(PassengerId=test_again$PassengerId, Survived = my_prediction)
write.csv(my_solution, file="my_RF_solution.csv", row.names = FALSE)



