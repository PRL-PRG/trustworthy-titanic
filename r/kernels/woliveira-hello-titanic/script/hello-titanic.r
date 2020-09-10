
# Consumo dos dados

library(ggplot2)
library(dplyr)
library(randomForest)

train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)

train$isTrain <- T
test$isTrain <- F

full <- bind_rows(train, test)
glimpse(full)

# Analise das variaveis

summary(full)

full$isFemale <- F
full$isFemale[full$Sex == "female"] <- T

ggplot(full[full$isTrain,], aes(x = isFemale, fill = factor(Survived))) +
    geom_bar(stat = 'count', position='dodge')

full$underTen <- F
full$underTen[full$Age > 0 &  full$Age < 10] <- T

ggplot(full[full$isTrain,], aes(x = underTen, fill = factor(Survived))) +
    geom_bar(stat = 'count', position='dodge')

# ricos


full$Fare <- round(full$Fare)

full[full$isTrain,] %>%
    group_by(Survived) %>%
    summarise(TotalFare = sum(Fare), count = n(), Mean = mean(Fare))

full$FamilySize <-  full$SibSp + full$Parch + 1

ggplot(full[full$isTrain,], aes(FamilySize, fill = factor(Survived))) + 
    geom_bar(stat='count', position="dodge")

full$FamilySizeFactor[full$FamilySize == 1] <- 'alone'
full$FamilySizeFactor[full$FamilySize < 5 & full$FamilySize > 1] <- 'small'
full$FamilySizeFactor[full$FamilySize > 4] <- 'large'

full$FamilySizeFactor <- factor(full$FamilySizeFactor)


ggplot(full[full$isTrain,], aes(Pclass, fill = factor(Survived))) + 
    geom_bar(stat='count', position="dodge")

full$Pclass <- factor(full$Pclass)

# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

full <-
    full %>%
    select(PassengerId, Survived, Title, Pclass, FamilySizeFactor, Fare, underTen, isFemale, isTrain)

summary(full)

full$Fare[is.na(full$Fare)] <- 14
full$Title <- factor(full$Title)

summary(full)

set.seed(42)

totalrows <- nrow(full[full$isTrain,])
trainrows <- sample(c(T,F), totalrows, replace = T, prob = c(0.8, 0.2))

trainningSet <- full[full$isTrain,][trainrows,]
testingSet <- full[full$isTrain,][!trainrows,]

model <- randomForest(factor(Survived) ~ Title + Pclass + FamilySizeFactor + Fare + underTen + isFemale,
                     data = trainningSet, ntree = 50)

predicted <- predict(model, testingSet)
glimpse(predicted)

confusion_matrix <- table(testingSet$Survived, predicted)
confusion_matrix

TP <- confusion_matrix[2, 2]
TN <- confusion_matrix[1, 1]

acc <- (TP + TN) / (nrow(testingSet))
acc

final <- predict(model, full[!full$isTrain,])

solution <- data.frame(PassengerId = full[!full$isTrain,]$PassengerId, Survived = final)

write.csv(solution, file= 'out.csv', row.names = F)

# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

list.files("../input")

# Any results you write to the current directory are saved as output.
