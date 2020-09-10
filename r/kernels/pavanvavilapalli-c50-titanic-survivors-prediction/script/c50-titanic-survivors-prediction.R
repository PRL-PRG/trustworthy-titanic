library(C50)
library(caret)

titanic <- read.csv("../input/train.csv", stringsAsFactors = F)
target <- read.csv("../input/test.csv", stringsAsFactors = F)
target$Survived <- 0
combined <- rbind(titanic, target)
rm(titanic)
rm(target)

variables <- c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Cabin", "Embarked")

combined$Survived <- as.factor(combined$Survived)
combined$Pclass <- as.factor(combined$Pclass)
combined$Sex <- as.factor(combined$Sex)

# In place of all NA's in Age, distributing them equally over all age groups

combined$Age[is.na(combined$Age)] <- -1
combined$Age[combined$Age==-1] <- seq(0, max(combined$Age), by = sum(combined$Age==-1)/max(combined$Age))

combined$Cabin[combined$Cabin==""] <- "missing"
combined$Embarked[combined$Embarked==""] <- "missing"

combined$Cabin <- as.factor(combined$Cabin)
combined$Embarked <- as.factor(combined$Embarked)

# Assigning Train and Test data
train <- combined[seq(1,891),variables]
test <- combined[-seq(1,891), variables]

train_c50 <- C5.0(Survived~., data = train, control = C5.0Control(CF = 0.90))

survival_prediction <- data.frame(PassengerId = c(seq(892, 1309)))
predicted_test <- predict(train_c50, test)

survival_prediction$Survived <- predicted_test

write.csv(survival_prediction, file = "c50_Titanic_survivors_submission.csv", row.names=FALSE)