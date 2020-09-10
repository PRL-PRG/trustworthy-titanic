# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

list.files("../input")

# Any results you write to the current directory are saved as output.

set.seed(123)

library(plyr)
library(dplyr)
library(stringr)
library(bindrcpp)
library(ROCR)
library(Metrics)


#################################
######### Reading data ##########
#################################

train <- read.csv("../input/train.csv",
                  header = TRUE,
                  na.strings = c("NA"," ","",NA),
                  stringsAsFactors = FALSE)

test <- read.csv("../input/test.csv",
                 header = TRUE,
                 na.strings = c("NA"," ","",NA),
                 stringsAsFactors = FALSE)

gender <- read.csv("../input/gendermodel.csv",
                 header = TRUE,
                 na.strings = c("NA"," ","",NA),
                 stringsAsFactors = FALSE)



#################################
###### Analysing Dataset ########
#################################

head(train)
no.of.row <- nrow(train)
no.of.col <- ncol(train)

sapply(train, class)
str(train)

# Checking number of missing values in each column.
colNA <- apply(train, 2, function(x) {length(which(is.na(x)==TRUE))})

# Another way of checking missing values
colSums(is.na(train))
colSums(is.na(test))

# Checking for unique values in column
# The removed columns are continues variable which would not be of much help.
sapply(train[,-c(1,4,6,9,10,11,12)], unique)
sapply(test[,-c(1,3,5,8,9,10,11)], unique)

# Getting the summary details of Age and Fare.
summary(train$Age)
summary(test$Age)

summary(train$Fare)
summary(test$Fare)



#################################
######### Combine Data ##########
#################################

# Combining both the data would help in cleaning it efficiently. 
data <- bind_rows(train, test)


#################################
####### Data Manipulation #######
#################################

# This function is used to get the title of the Names.
newVar <- function(string){

  tit <- str_split(string, "[.,]")[[1]][2]
  tit <- str_trim(tit, side = "both")
  return(tit)

}

data$Title <- sapply(data$Name, newVar)

# Replacing the titles of the people in the ship with their appropriate gender title.
tab <- table(data$Title)

for( i in 1:nrow(data)){
  ch <- names(tab)[i]
  if(ch %in% c("Lady","Mlle","Mme","Ms","the Countess","Dr")){
    data <- mutate(data, Title = str_replace(Title, ch, "Mrs"))
  } else if(ch %in% c("Capt","Col","Jonkheer","Major","Rev","Sir")){
    data <- mutate(data, Title = str_replace(Title, ch, "Mr"))
  }
}
data <- mutate(data, Title = str_replace(Title,"Dona","Mrs"))
data <- mutate(data, Title = str_replace(Title,"Don","Mr"))

# Splitting the ticket value between 0 and 1.
# 0 for those ticket whose value starts with a number.
# 1 for those ticket whose value starts with an alphabet.

ticketVar <- function(string){
  tv <- length(str_split(string, "[ ]")[[1]])
  return(ifelse(tv>1, 1, 0))
}
data$newTicket <- sapply(data$Ticket, ticketVar)


#################################
######## Data Cleaning ##########
#################################

# Changing the missing values of Age to median.
data$Age[is.na(data$Age)==TRUE] <- median(data$Age, na.rm = TRUE)

# Removing the row containing the missing value of Embarked.
data <- data %>% filter(is.na(Embarked) == FALSE)

# Changing the missing values of Fare to median.
data$Fare[is.na(data$Fare) == TRUE] <- median(data$Fare, na.rm = TRUE)

# Changing the missing values of Cabin to Miss.
data$Cabin[is.na(data$Cabin) == TRUE] <- "Miss"

# Converting the Fare value to log() for easier modelling.
data$Fare <- log(data$Fare + 1)

# Removing the extra level of Parch from the data.
data$Parch[which(data$Parch == 9)] <- 0


#################################
######### Dividing Data #########
#################################

train <- data %>% filter(is.na(Survived) == FALSE)
train$Survived <- as.factor(train$Survived)

test <- data %>% filter(is.na(Survived) == TRUE)
test$Survived <- NULL


#################################
######## Creating Model #########
#################################

# Creating a model using Generalized Linear Regression.
model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = train[,-c(1,4,9)], control = list(maxit = 50))
anova(model, test = "Chisq")

# Creating a second model after the checking the p-values from the above model.
model2 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Fare + Title,
              data = train,
              family = binomial(link = "logit"),
              control = list(maxit = 50))
summary(model2)

# Comparing the 2 created models.
anova(model, model2, test = "Chisq")

#################################
# Predicting the data on train ##
#################################

# Splitting the train data to predict whether the model created is 
# effective in getting the survivors.
split <- sample(seq_len(nrow(train)), size = floor(0.75 * nrow(train)))
new.train <- train[split,]
new.test <- train[-split,]

model.random <- glm(Survived ~ Pclass + Sex + Age + SibSp + Fare + Title,
              data = new.train,
              family = binomial(link = "logit"),
              control = list(maxit = 50))

predict.random <- predict(model.random, newdata = new.test, type = "response")
predict.random <- ifelse(predict.random > 0.5, 1, 0)

# Using the ROCR Package to get the AUC value to know whether 
# the model is effective.
pr <- prediction(predict.random, new.test$Survived)
perf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(perf)

auc(new.test$Survived, predict.random)
# AUC = 0.842

#################################
#### Solution from the model ####
#################################


# Applying the model in the test data.
predict.model <- predict(model2, newdata = test, type = "response")
predict.model <- ifelse(predict.model > 0.5, 1, 0)

# Saving the predicted the values in another file.
solution <- data.frame(PassengerID = test$PassengerId, Survived = predict.model)
write.csv(solution, file = "solution.csv", row.names = FALSE)



#################################
########## THANK YOU ############
#################################
