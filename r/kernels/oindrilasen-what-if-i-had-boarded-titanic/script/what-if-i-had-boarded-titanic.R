## ----include = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
######################################
# 1. Load Data
######################################
# Set Working Directory
#setwd("/Users/oindrilasen/WORK_AREA/Data Science/kaggle/Titanic")
# Read train.csv data file
titanic_train<-read.csv('../input/train.csv',
                  header = TRUE,
                  na.strings = "",
                  stringsAsFactors = FALSE)

# Read test.csv data file
titanic_test<-read.csv('../input/test.csv',
                        header = TRUE,
                        na.strings = "",
                        stringsAsFactors = FALSE)

# Add column Survived in Test DataSet
titanic_test$Survived <- NA
titanic_clean <- rbind(titanic_train,titanic_test)


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(titanic_train) # 891,12
dim(titanic_test) # 418, 11
glimpse(titanic_clean) # 1309, 12


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check which variables are Factors
sapply(titanic_clean, function(x) length(unique(x)))


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Transforming categorical Variables to factors:
to_factor <- c(
  'Survived',
  'Pclass',
  'Sex',
  'Embarked'
)
for (col in to_factor) {
  titanic_clean[[col]] <- factor(titanic_clean[[col]])
}


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# check for NA values
sapply(titanic_clean, function(x) sum(is.na(x)))


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Convert Age column to Numeric
titanic_clean$Age <- as.integer((titanic_clean$Age))
# Relace NA values for Age with the mean
titanic_clean$Age[is.na(titanic_clean$Age)] <- mean(titanic_clean$Age,na.rm = TRUE)


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Replace Cabin# with None for NA records
titanic_clean$Cabin[is.na(titanic_clean$Cabin)] <- "None"


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check for Embarked variable
table(titanic_clean$Embarked)
# Relace the Embarked value with the most common value i.e S
titanic_clean$Embarked[is.na(titanic_clean$Embarked)] <- "S"


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Convert Fare column to Integer
titanic_clean$Fare <- as.integer((titanic_clean$Fare))
# Relace NA values for Fare with the Mean
titanic_clean$Fare[is.na(titanic_clean$Fare)] <- mean(titanic_clean$Fare,na.rm = TRUE)


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Again check for NA values
sapply(titanic_clean, function(x) sum(is.na(x)))


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Change the levels to meaningful values
# 1. Pclass
levels(titanic_clean$Pclass)[levels(titanic_clean$Pclass)== "1"] <- "1st Class"
levels(titanic_clean$Pclass)[levels(titanic_clean$Pclass)== "2"] <- "2nd Class"
levels(titanic_clean$Pclass)[levels(titanic_clean$Pclass)== "3"] <- "3rd Class"

# 2. Embarked
levels(titanic_clean$Embarked)[levels(titanic_clean$Embarked)== "C"] <- "Cherbourg"
levels(titanic_clean$Embarked)[levels(titanic_clean$Embarked)== "Q"] <- "Queenstown"
levels(titanic_clean$Embarked)[levels(titanic_clean$Embarked)== "S"] <- "Southampton"


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(titanic_clean$Fare)
titanic_clean$Fare_Group <-factor(ifelse(titanic_clean$Fare >= 0 & titanic_clean$Fare <= 15, "Low",
                           ifelse(titanic_clean$Fare > 15 & titanic_clean$Fare <=100, "Medium",
                           ifelse(titanic_clean$Fare >100 ,"High",NA
                                               ))))



## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Add new feature Age_Group
summary(titanic_clean$Age)

titanic_clean$Age_Group <-factor(ifelse(titanic_clean$Age<= 3, "Baby",
                          ifelse(titanic_clean$Age> 3 & titanic_clean$Age<=12, "Kid",
                          ifelse(titanic_clean$Age> 12 & titanic_clean$Age<=18, "Teen",
                          ifelse(titanic_clean$Age> 18, "Adult",NA
                                 ))))
)


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Add new feature with_family
titanic_clean$with_family <-factor(ifelse(titanic_clean$Parch == 0 & titanic_clean$SibSp ==0, "no","yes"))


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Understand the Variables Individually
ggplot(titanic_clean, aes(x = Pclass)) +
  geom_bar(fill= "light blue")+
  scale_y_continuous(limits = c(0,1400), breaks = seq(0,1400,100))+
  ggtitle("Class vs Total Passengers") 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table((table(titanic_clean$Sex)))
ggplot(titanic_clean, aes(x = Sex)) +
    geom_bar(fill= "light green")+
    scale_y_continuous(limits = c(0,1400), breaks = seq(0,1400,100))+
  ggtitle("Sex vs Total Passengers") 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_clean, aes(x = Embarked)) +
    geom_bar(fill= "light yellow")+
    scale_y_continuous(limits = c(0,1400), breaks = seq(0,1400,100))+
  ggtitle("Embarked vs Total Passengers") 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_clean, aes(x = Age_Group)) +
    geom_bar(fill= "light green")+
    scale_y_continuous(limits = c(0,1400), breaks = seq(0,1400,100))+
  ggtitle("Age_Group vs Total Passengers") 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_clean, aes(x = Fare_Group)) +
    geom_bar(fill= "light blue")+
    scale_y_continuous(limits = c(0,1000), breaks = seq(0,1000,100))+
  ggtitle("Fare_Group vs Total Passengers") 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(titanic_clean$with_family))

ggplot(titanic_clean, aes(x = with_family)) +
    geom_bar(fill= "light yellow")+
    scale_y_continuous(limits = c(0,1000), breaks = seq(0,1000,100))+
  ggtitle("with_family vs Total Passengers")   


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  table(titanic_clean$with_family, titanic_clean$Age_Group)
   kids_without_family<-
    titanic_clean%>%
    filter(with_family=="no",Age_Group=="Kid")
  
  kids_without_family


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(550)
titanic_train <- titanic_clean[1:891,]
titanic_test <- titanic_clean[892:1309,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(titanic_train$Survived,titanic_train$Age_Group)
ggplot(titanic_train, aes(Age_Group, ..count..)) + 
geom_bar(aes(fill = Survived), position = "dodge", na.rm = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(titanic_train$Survived,titanic_train$Sex)
ggplot(titanic_train, aes(Sex, ..count..)) + 
  geom_bar(aes(fill = Survived), position = "dodge", na.rm = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(titanic_train$Survived,titanic_train$with_family)
ggplot(titanic_train, aes(with_family, ..count..)) + 
  geom_bar(aes(fill = Survived), position = "dodge", na.rm = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(titanic_train$Survived,titanic_train$Fare_Group))
ggplot(titanic_train, aes(Fare_Group, ..count..)) + 
  geom_bar(aes(fill = Survived), position = "dodge", na.rm = FALSE)


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
my_chances <-
  titanic_train %>%
  filter(Sex=="female",
         Age_Group == "Teen",
         Parch >0 ,
         SibSp >0 
         )
prop.table(table(my_chances$Survived))


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
######################################
# 5. Create a Model
######################################
lm_survival_model <- glm(Survived ~ Pclass+Sex+
                                    Age+SibSp+Parch
                           +with_family+Age_Group,
                           data = titanic_train,
                           family = binomial(link=logit)
)

summary(lm_survival_model)


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Prediction
predict_survival <- round(predict(lm_survival_model,
                                  titanic_test,type =  "response"))
titanic_test$Survived <- predict_survival
table(titanic_test$Survived)


## ----comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Write the Final Solution
final_solution <- titanic_test%>%
                  select(PassengerId,Survived)
#write.csv(final_solution, file = 'Final_Solution.csv', row.names = F)

