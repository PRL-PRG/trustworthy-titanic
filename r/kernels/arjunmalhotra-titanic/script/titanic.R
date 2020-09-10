# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

list.files("../input")

# Any results you write to the current directory are saved as output.
#Loading the data and creating factors for sex
train <- read.csv("../input/train.csv", header = TRUE)
train$Sex <- as.factor(train$Sex)
test <- read.csv("../input/test.csv", header = TRUE)
test$Sex <- as.factor(test$Sex)

#Cleaning the Data

train.age.median <- median(train$Age, na.rm = TRUE)
test$Age[is.na(test$Age)] <- train.age.median

#Logistic Regression

train.logit_1 <- glm(Survived ~ Pclass + Sex + Age + SibSp, family = binomial(link = "logit"), data = train)
Survived.Prob <- predict(train.logit_1, newdata = test, type = "response")
Survived.Prob.Frame <- as.data.frame(Survived.Prob)
Survived.Predict <- ifelse(Survived.Prob.Frame>=.5, 1, 0)

#Creating Final Data

PassengerId <- test$PassengerId
Entry.df <- as.data.frame(PassengerId)
Entry.df$Survived <- Survived.Predict

#Export

write.csv(Entry.df, file="Kaggle_Titanic_Submission.csv", row.names = FALSE)