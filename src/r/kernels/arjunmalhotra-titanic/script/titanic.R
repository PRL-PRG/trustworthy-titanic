library(ggplot2)
library(readr)
list.files("../input")
train <- read.csv("../input/train.csv", header = TRUE)
train$Sex <- as.factor(train$Sex)
test <- read.csv("../input/test.csv", header = TRUE)
test$Sex <- as.factor(test$Sex)
train.age.median <- median(train$Age, na.rm = TRUE)
test$Age[is.na(test$Age)] <- train.age.median
train.logit_1 <- glm(Survived ~ Pclass + Sex + Age + SibSp, family = binomial(link = "logit"), data = train)
Survived.Prob <- predict(train.logit_1, newdata = test, type = "response")
Survived.Prob.Frame <- as.data.frame(Survived.Prob)
Survived.Predict <- ifelse(Survived.Prob.Frame >= 0.5, 1, 0)
PassengerId <- test$PassengerId
Entry.df <- as.data.frame(PassengerId)
Entry.df$Survived <- Survived.Predict
write.csv(Entry.df, file = "Kaggle_Titanic_Submission.csv", row.names = FALSE)
