library(dplyr)
library(ggplot2)
library(neuralnet)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
train <- bind_rows(train, test)
train$Title <- gsub("(.*, )|(\\..*)", "", train$Name)
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
train$Title[train$Title == "Mlle"] <- "Miss"
train$Title[train$Title == "Ms"] <- "Miss"
train$Title[train$Title == "Mme"] <- "Mrs"
train$Title[train$Title %in% rare_title] <- "RareTitle"
train$family <- train$SibSp + train$Parch + 1
train$Fare[is.na(train$Fare)] <- median(train$Fare, na.rm = TRUE)
train$Age[is.na(train$Age)] <- median(train$Age, na.rm = TRUE)
train$Parch[is.na(train$Parch)] <- median(train$Parch, na.rm = TRUE)
train$SibSp[is.na(train$SibSp)] <- median(train$SibSp, na.rm = TRUE)
train$Embarked[is.na(train$Embarked)] <- "S"
train$Title <- as.factor(train$Title)
train$family <- as.factor(train$family)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)
train$Parch <- as.factor(train$Parch)
train$SibSp <- as.factor(train$SibSp)
train$Survived <- as.factor(train$Survived)
scale_fare <- scale(train$Fare, center = TRUE, scale = TRUE)
train$Fare <- scale_fare[, 1]
test <- train[892:1309, ]
train <- train[1:891, ]
m_train <- model.matrix(~Survived + family + Sex + Age + Fare + Title, data = train)
nn <- neuralnet(Survived1 ~ family2 + family3 + family4 + family5 + family6 + family7 + family8 + family11 + Sexmale + Age + Fare + TitleMiss + TitleMr + TitleMrs + TitleRareTitle, data = m_train, hidden = c(4, 2), linear.output = FALSE, stepmax = 1e+06)
m_test <- model.matrix(~family + Sex + Age + Fare + Title, data = test)
write.csv(m_train, file = "designmatrix_train.csv", row.names = F)
write.csv(m_test, file = "designmatrix_test.csv", row.names = F)
prediction <- compute(nn, m_test[, 2:16])
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction$net.result)
solution$Survived <- ifelse(solution$Survived > 0.5, 1, 0)
write.csv(solution, file = "solution.csv", row.names = F)
table(solution$Survived)
