
#Introduction
#This is my first Kaggle Script. I went through the few kernals from kaggle members
#before coming up with with mine. Tried few Data Explorations, Visualizations, Feature Engineering
#Missing Data Injection using Mice and predict survival using Random Forest Model
#Download titanic_random_forest_r_submission.csv from the output below
#and submit it through https://www.kaggle.com/c/titanic-gettingStarted/submissions/attach
#to enter this getting started competition!

#STEP1: Load Libraries and Data
#Loading Libraries
library(dplyr)
library(ggplot2)
library(ggthemes)
library(randomForest)
library(mice)

#Load the Train and Test Data Set
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")
full <- bind_rows(train,test)

#Check the Data Type
str(train)
str(test)
str(full)
summary(full)

#STEP2:Feature Engineering 1
#Feature Engineering
#Convert as Factors
full$Survived <- as.factor((full$Survived))
full$Pclass <- as.factor(full$Pclass)
full$Embarked <- as.factor(full$Embarked)

#Does title of the Passengers got any influence on the survival.
full$title <- gsub('(.*, )|(\\..*)','',full$Name)
table(full$title,full$Survived)
table(full$Sex,full$title)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

full$title[full$title == 'Mlle']        <- 'Miss' 
full$title[full$title == 'Ms']          <- 'Miss'
full$title[full$title == 'Mme']         <- 'Mrs' 
full$title[full$title %in% rare_title]  <- 'Others'
full$title <- as.factor(full$title)

#Does Family Together Survived. Lets Create a family variable to analyse
full$Family <- full$Parch + full$SibSp + 1
prop.table(table(full$Family,full$Survived),1)

ggplot(full[1:891,],aes(x=Family,fill=Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  theme_few()


#Looks like family has some effect on the survival. Lets try classify the family into single, small
#and Large to get better understanding.
#Lets Create a Family Type
full$FType <- 'NA'
full$FType <- ifelse(full$Family==1,"single",ifelse(full$Family>1 & full$Family<=4,"small","Large"))
full$FType <- as.factor(full$FType)
prop.table(table(full$FType,full$Survived),1)

ggplot(full[1:891,],aes(x=FType,fill=Survived)) +
  geom_bar(stat='count', position='dodge') 

#Looks like Small family got better chance of survival
#Lets now analyse whether Gender got any influence on the Survival.
table(full$Sex,full$Survived)
prop.table(table(full$Sex,full$Survived),1)

ggplot(full[1:891,],aes(x=Sex,fill=Survived)) +
  geom_bar(stat = "count",position="dodge")

#Looks like Survival of Female Gender is more than male. So definitely there is an influence of Gender
#while predicting the survival.
#Does Fare got any influence on the survival. Let's check it out.
prop.table(table(full$Fare,full$Survived),1)

ggplot(full[1:891,],aes(x=Fare,fill=Survived)) +
  geom_density()


#Does Passenger Class got any influence on the survival. Let's check it out.
prop.table(table(full$Pclass,full$Survived),1)

ggplot(full[1:891,],aes(x=Pclass,fill=Survived)) +
  geom_bar(stat = "count",position="dodge")

#1st and 2nd Class got better survial chances them 3rd Class

# STEP 3: 
# Missing Value Imputation
# Lets now populate the missing values
# Here are the List of columns that are having missing values "NA"
# Age - 263 nos
# Fare - 1 nos
# Embarked - 2 nos

# Treating Embarked Columns
full$Embarked <- as.factor(full$Embarked)
prop.table(table(full$Embarked))

ggplot(full,aes(x=Embarked,y=Fare,Fill=factor(Pclass))) +
geom_boxplot() +
  geom_hline(yintercept = 80,colour='blue') +
  theme_grey()

# Median of Embarked C closely matches the value of Null Embarked. 
# So we can safely assume Missing Embarked values to "C"

full[full$Embarked=='',]$Embarked <- 'C'

# Treating Fare Column
full_embarked_s <- full[full$Embarked=='S' & full$Pclass==3,]
full_embarked_s <- full_embarked_s[!is.na(full_embarked_s$Fare),]

ggplot(full_embarked_s,aes(x=Fare)) +
  geom_density(fill='lightblue') +
  geom_vline(aes(xintercept = median(Fare)),color='Red') +
  theme_base()

# From this Graph we understand that most of the Passenge Embarked in in S and Class 3 got fare close 
# to their median of fare which is 8.05. So its reasonable to input missing Fare value for row 1044 with 
# median value 8.05

full$Fare[1044] <- median(full[full$Embarked=='S' & full$Pclass==3,]$Fare,na.rm = T)


# Imputing Age Column
# Interested in trying MICE package to impute the values.
set.seed(111)

mice_model <- mice(full[,c("Pclass","Sex","Age","SibSp","Parch","Fare","Family","title","FType","Embarked")],method='rf')
mice_output <- complete(mice_model)

par(mfrow=c(1,2))
hist(mice_output$Age,main='Mice Output')
hist(full$Age,main='OriginalData Output')

# There is not much change in the histogram of the imputed Age vs the Age from the Original Data
# Hence lets goahead and replace orignal Age with the Imputed Age.
full$Age <- mice_output$Age

#Lets Verify any NA's Exist
summary(full)

# STEP 4: 
# Feature Engineering 2
# With now Age populated. Lets try to derive few more addtional columns from that to predict the survival.
# Lets classify passenger as Major and Minor based on the AGe and analyse the probability of survival
full$Adulch <- 'NA'
full$Adulch <- ifelse(full$Age>=18,"Major","Minor")
full$Adulch <- as.factor(full$Adulch)
table(full$Adulch,full$Survived)
prop.table(table(full$Adulch,full$Survived),1)

ggplot(full[1:891,],aes(x=Adulch,fill=factor(Survived))) +
  geom_bar(stat = "count",position="dodge")

# Looks like Minor (Age < 18) got almost 50% probability of survival.
# Lets combine Gender and Major/Minor to see the survial
full$SexAdulCh <- 'NA'
full$SexAdulCh <- paste(full$Sex,full$Adulch)
full$SexAdulCh <- as.factor(full$SexAdulCh)
prop.table(table(full$SexAdulCh,full$Survived),1)

ggplot(full[1:891,],aes(x=SexAdulCh,fill=Survived)) +
  geom_bar(stat = "count",position="dodge")

# Intersting Female Major and Female Minor Got much better chance of Survival than male.
# so definitely Sex and Age got lot if influence in survival.


# Step:5
# Time Build the Model using Random Forest
# Lets Split the Data into Training and Test set

train1 <- full[1:891,]
test1 <- full[892:1309,]

# Step5.1: Build the Model using Training Data Set

set.seed(112)
str(train1)

#Lets choose only the independent variables that are relevant and compare against the Survived Variable for prediction.
rf_model <- randomForest(Survived~title+SexAdulCh+Pclass+Fare+Embarked+FType,train1)

#Lets plot to see the Error Rate
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# Error Rate of Survival is less than 30% and Died is less than 10% 
# And overall Error Rate (black) is less than 20%

# Lets Get the importance of the independent variable in predicting the Survival.
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Use ggplot2 to visualize the relative importance of variables
ggplot(varImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()
# Sex is the most significant factor along with Fare and Age in predicting the survival
# Family and FType too used for prediction.

# STEP 6
# Submission of the Result
Survival_Predicted <- data.frame(PassengerId = test1$PassengerId)
Survival_Predicted$Survived <- predict(rf_model, test1)
getwd()
write.csv(Survival_Predicted, file = "titanic_random_forest_r_submission.csv", row.names=FALSE)
