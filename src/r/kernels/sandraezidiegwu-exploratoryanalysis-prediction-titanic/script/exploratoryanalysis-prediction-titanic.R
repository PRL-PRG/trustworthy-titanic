library(rpart)
library(rpart.plot)
library(ggplot2)
library(dplyr)
test <- read.csv("../input/test.csv")
train <- read.csv("../input/train.csv")
names(test)
names(train)
test$Survived <- 0
all.pass <- rbind(train, test)
str(all.pass)
summary(all.pass)
all.pass$Title <- gsub("(.*, )|(\\..*)", "", all.pass$Name)
table(all.pass$Sex, all.pass$Title)
all.pass$Title[all.pass$Title %in% c("Mlle", "Ms")] <- "Miss"
all.pass$Title[all.pass$Title %in% c("Dona", "the Countess", "Lady", "Don", "Jonkheer", "Major", "Sir")] <- "Affluent"
all.pass$Title[all.pass$Title %in% c("Master", "Rev", "Capt", "Col")] <- "Mr"
all.pass$Title[all.pass$Title %in% "Mme"] <- "Mrs"
table(all.pass$Sex, all.pass$Title)
ggplot(all.pass, aes(x = Title, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge")
all.pass$FSize <- all.pass$SibSp + all.pass$Parch + 1
all.pass$FName <- paste(all.pass$Surname, all.pass$FSize, sep = "_")
ggplot(all.pass, aes(x = FSize, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size") + theme_linedraw()
ggplot(all.pass, aes(x = FSize, fill = factor(Title))) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size") + theme_linedraw()
par(mfrow = c(1, 1))
mosaicplot(table(all.pass$Title, all.pass$Survived), main = "Survival by Title", shade = TRUE)
sum(is.na(all.pass$Fare))
na.fare <- all.pass[is.na(all.pass$Fare), ]
na.fare
all.pass$Fare[153] <- median(all.pass[all.pass$Pclass == "3" & all.pass$Embarked == "S", ]$Fare, na.rm = T)
par(mfrow = c(1, 1))
hist(all.pass$Age, main = "Age Distribution aboard the Titanic", xlab = "Age", col = "lightblue")
sum(is.na(all.pass$Age))
actual <- all.pass
actual2 <- all.pass
age.part <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Ticket + Fare + Cabin + FSize + Title, data = all.pass[!is.na(all.pass$Age), ], method = "anova")
age.pred <- predict(age.part, all.pass[is.na(all.pass$Age), ])
actual$Age[is.na(actual$Age)] <- age.pred
age.prune <- prune.rpart(age.part, cp = 0.1)
age.pred2 <- predict(age.prune, all.pass[is.na(all.pass$Age), ])
actual2$Age[is.na(actual2$Age)] <- age.pred2
mean(actual2$Age != actual$Age)
par(mfrow = c(1, 3))
hist(all.pass$Age, col = "blue", main = "Actual Age Values", xlab = "Age")
hist(actual$Age, col = "lightblue", main = "Predicted Age Values", xlab = "Age")
hist(actual2$Age, col = "lightblue", main = "Pruned Predicted Age Values", xlab = "Age")
all.pass$Age[is.na(all.pass$Age)] <- age.pred2
sum(is.na(all.pass$Age))
all.pass$AgeDist[all.pass$Age < 18] <- "Child"
all.pass$AgeDist[all.pass$Age >= 18] <- "Adult"
all.pass$AgeDist <- factor(all.pass$AgeDist)
table(all.pass$AgeDist, all.pass$Survived)
test_data <- all.pass[892:1309, ]
train_data <- all.pass[1:891, ]
test_data$Survived <- NULL
model_part <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Fare + Title + FSize + AgeDist, data = train_data, method = "class", control = rpart.control(cp = 1e-04))
par(mfrow = c(1, 1))
rpart.plot(model_part)
model_pred <- predict(model_part, test_data, type = "class")
solution <- data.frame(PassengerID = test_data$PassengerId, Survived = model_pred)
solution[1:10, ]
model_prune <- prune(model_part, cp = 0.1)
model_prune.pred <- predict(model_prune, test_data, type = "class")
prune_solution <- data.frame(PassengerID = test_data$PassengerId, Survived = model_prune.pred)
prune_solution[1:10, ]
write.csv(prune_solution, file = "rpart-solution.csv", row.names = F)
final_solution <- read.csv("rpart-solution.csv", sep = ",", header = TRUE)
final_solution[1:10, ]
