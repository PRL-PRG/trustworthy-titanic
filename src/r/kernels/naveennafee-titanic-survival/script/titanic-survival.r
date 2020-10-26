library("tidyverse")
library("ggthemes")
library("caret")
library("e1071")
library("rpart")
library("rpart.plot")
library("randomForest")
training <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
testing <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
testing$Survived <- NA
full <- rbind(training, testing)
str(full)
colSums(is.na(full))
colSums(full == "")
full$Cabin <- NULL
str(full)
table(full$Embarked)
full[full$Embarked == "", ]$Embarked <- "S"
table(full$Embarked)
table(full$Embarked, full$Survived)
full$Fare[1044] <- mean(full[full$Pclass == "3", ]$Fare, na.rm = T)
head(full$Name)
full$title <- gsub("(.*,)|(\\..*)", "", full$Name)
unique(full$title)
full$title <- trimws(full$title)
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
full[full$title == "Mlle", ]$title <- "Miss"
full[full$title == "Ms", ]$title <- "Miss"
full[full$title == "Mme", ]$title <- "Mr"
full[full$title %in% rare_title, ]$title <- "Rare"
table(full$title)
factor_check <- sapply(full, function(x) (length(unique(x))))
factor_check
factor_var <- c("Survived", "Sex", "Embarked", "Pclass")
for (i in factor_var) {
    full[, i] <- as.factor(full[, i])
}
str(full)
ggplot(full[1:891, ], aes(x = Embarked, fill = Survived)) + geom_bar(position = "dodge") + ggtitle("Port of Embarkation vs Survived") + ylab("Survived")
ggplot(full[1:891, ], aes(x = title, fill = Survived)) + geom_bar() + ggtitle("Title of the Person vs Survived") + ylab("Survived")
ggplot(full[1:891, ], aes(x = Sex, fill = Survived)) + geom_bar(position = "dodge") + ggtitle("Sex vs Survived") + ylab("Survived")
full$Family_count <- full$SibSp + full$Parch + 1
table(full$Family_count)
ggplot(full[1:891, ], aes(x = Family_count, fill = Survived)) + geom_bar(position = "dodge") + ggtitle("Family Count vs Survived") + ylab("Survived") + coord_flip() + scale_x_reverse(breaks = c(1:11)) + theme_light()
full$Family_size_ratio[full$Family_count <= 2] <- "Small"
full$Family_size_ratio[full$Family_count >= 3 & full$Family_count <= 5] <- "Medium"
full$Family_size_ratio[full$Family_count >= 6] <- "Big"
ggplot(full[1:891, ], aes(x = Family_size_ratio, fill = Survived)) + geom_bar(position = "dodge") + ggtitle("Family Size Ratio vs Survived") + ylab("Survived")
head(full)
full$Age <- Hmisc::impute(full$Age, mean)
ggplot(full[1:891, ], aes(x = Survived, y = Age)) + geom_boxplot(color = c("blue", "red")) + theme_few() + ggtitle("Age vs Survived")
ggplot(full[1:891, ], aes(x = Pclass, fill = Survived)) + geom_bar(position = "dodge") + ggtitle("PClass vs Survived")
str(full)
full$Family_size_ratio <- as.factor(full$Family_size_ratio)
full$title <- as.factor(full$title)
train_model <- full[1:891, c("Survived", "Age", "Sex", "Family_count", "Family_size_ratio", "Fare", "title")]
test_model <- full[892:1309, c("Survived", "Age", "Sex", "Family_count", "Family_size_ratio", "Fare", "title")]
rf_model <- randomForest(Survived ~ ., data = train_model, importance = T)
rf_model
predicted <- predict(rf_model, train_model)
confusionMatrix(predicted, train_model$Survived)
plot(rf_model)
legend("topright", colnames(rf_model$err.rate), col = 1:3, fill = 1:3)
varImpPlot(rf_model, main = "RANDOM FOREST MODEL")
rp_model <- rpart(Survived ~ ., train_model, method = "class")
summary(rp_model)
rpart.plot(rp_model, tweak = 0.8)
rp_predicted <- predict(rp_model, train_model, type = "class")
confusionMatrix(rp_predicted, train_model$Survived)
submission <- data.frame(PassengerId = testing$PassengerId)
submission$Survived <- predict(rf_model, test_model)
write.csv(submission, file = "random_forest_r_submission.csv", row.names = FALSE)
