# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")

# Exploring the datasets
str(train)
table(train$Survived)
prop.table(table(train$Survived))

# Morbid prediction (they all perish)
test$Survived <- rep(0,418)

submit <- data.frame(Passengerid = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

# Utilizing other response variables (Age-Sex)
table(train$Sex)
round(prop.table(table(train$Sex,train$Survived),1) * 100, 2)

# All females survive
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

submit <- data.frame(Passengerid = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "maleperish.csv", row.names = FALSE)

# All childen and females survive
summary(train$Age)

hist(train$Age, col = c("red", "steelblue"), freq = F, main = "Distribution of Age", xlab = "Age")
rug(jitter(train$Age), col = "darkgrey")
lines(density(train$Age, na.rm = T), col = "yellow", lwd = 3)
box()

train$Child <- 0
train$Child[train$Age < 18] <- 1

aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
aggregate(Survived ~ Child + Sex, data = train, FUN = length)

aggregate(Survived ~ Child + Sex, data = train, FUN = function(x){sum(x)/length(x)})

train$Fare2 <- '30+'
train$Fare2[train$Fare >= 10 & train$Fare < 20] = '10-20'
train$Fare2[train$Fare >= 20 & train$Fare < 30] = '20-30'
train$Fare2[train$Fare < 10] = '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x){sum(x)/length(x)})

# All females survive expect ones with Pclass = 3 and paid fare >= $20
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

submit <- data.frame(Passengerid = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "higherclassfemalesurvive.csv", row.names = FALSE)
