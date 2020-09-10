data_train <- read.csv("../input/train.csv", sep = ",",
  header = TRUE, stringsAsFactors = FALSE, na.strings = "")
data_train$Survived <- factor(data_train$Survived,
  levels = c(1, 0), labels = c("Yes", "No"))
data_train$Pclass <- factor(data_train$Pclass, levels = c(1, 2, 3))
data_train$Sex <- factor(data_train$Sex, levels = c("male", "female"))
data_train$Age[data_train$Age < 12] <- "Child"
data_train$Age[is.na(data_train$Age)] <- "N"
data_train$Age[data_train$Age != "N" & data_train$Age != "Child"] <- "Adult"
data_train$Age <- factor(data_train$Age, levels = c("Adult", "Child", "N"))
data_train$Embarked <- factor(data_train$Embarked,
  levels = c("C", "Q", "S"))
fit <- glm(Survived ~ Pclass + Sex + Age + Embarked,
  family = binomial, data = data_train)
data_test <- read.csv("../input/test.csv", sep = ",",
  header = TRUE, stringsAsFactors = FALSE, na.strings = "")
data_test$Pclass <- factor(data_test$Pclass, levels = c(1, 2, 3))
data_test$Sex <- factor(data_test$Sex, levels = c("male", "female"))
data_test$Age[data_test$Age < 12] <- "Child"
data_test$Age[is.na(data_test$Age)] <- "N"
data_test$Age[data_test$Age != "N" & data_test$Age != "Child"] <- "Adult"
data_test$Age <- factor(data_test$Age, levels = c("Adult", "Child", "N"))
data_test$Embarked <- factor(data_test$Embarked,
  levels = c("C", "Q", "S"))
data_test$Survived <- round(1 - predict(fit, data_test, type = "response"), 0)
data_res <- data_test[, c(1, 12)]
write.table(data_res, file = "Titanic_logistic.csv", sep = ",", row.names = FALSE)