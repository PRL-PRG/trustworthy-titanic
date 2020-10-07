library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(mice)
library(randomForest)
library(caret)
titanictrain <- read.csv("../input/train.csv", header = TRUE, stringsAsFactors = FALSE)
titanictest <- read.csv("../input/test.csv", header = TRUE, stringsAsFactors = FALSE)
titanic = bind_rows(titanictrain, titanictest)
titanic$Surname = gsub("(,.*)", "", titanic$Name)
titanic$Title = gsub("(.*, )|(\\..*)", "", titanic$Name)
table(titanic$Title)
titanic[titanic$Title %in% c("Ms", "Mlle"), ]$Title = "Miss"
table(titanic$Title)
titanic[!(titanic$Title %in% c("Miss", "Mr", "Mrs", "Master")), ]$Title = "Special"
table(titanic$Title)
titanic$FamilySize = titanic$SibSp + titanic$Parch + 1
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Title", "Survived")
titanic[factor_vars] <- lapply(titanic[factor_vars], function(x) as.factor(x))
str(titanic)
summary(titanic)
sum(is.na(titanic$Fare))
pcl <- titanic[which(is.na(titanic$Fare)), ]$Pclass
pcl
fillfare <- mean(titanic[!is.na(titanic$Fare) & titanic$Pclass == pcl, ]$Fare)
fillfare
titanic[is.na(titanic$Fare), ]$Fare <- fillfare
sum(is.na(titanic$Fare))
sum(is.na(titanic$Embarked))
titanic[is.na(titanic$Embarked), ]
titanic[is.na(titanic$Embarked), ]$Fare
plot = ggplot(titanic[!is.na(titanic$Embarked), ], aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot() + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", lwd = 2) + theme_few()
titanic[is.na(titanic$Embarked), ]$Embarked
sum(is.na(titanic$Embarked))
sum(is.na(titanic$Age))
md.pattern(titanic)
library(VIM)
aggr_plot = aggr(titanic, col = c("navyblue", "red"), numbers = TRUE, sortVars = TRUE, labels = names(data), cex.axis = 0.7, gap = 3, ylab = c("Histogram of missing data", "Pattern"))
temp_titanic = mice(titanic[, !names(titanic) %in% c("PassengerId", "Name", "Ticket", "Cabin", "FamilySize", "Surname", "Survived")], method = "rf", m = 5, maxit = 50, seed = 502)
complete_titanic = complete(temp_titanic)
densityplot(temp_titanic)
par(mfrow = c(1, 2))
hist(titanic$Age, freq = F, col = "Red")
hist(complete_titanic$Age, freq = F, col = "Blue")
titanic$Age = complete_titanic$Age
sum(is.na(titanic$Age))
titanic$Child[titanic$Age >= 18] = 0
titanic$Child[titanic$Age < 18] = 1
titanic$Child = as.factor(titanic$Child)
train = titanic[1:891, ]
test = titanic[-(1:891), ]
glm.fit = glm(factor(Survived) ~ Pclass + Sex + Age + Embarked + SibSp + Parch + Fare + FamilySize + Child + Title, train, family = binomial, maxit = 100)
summary(glm.fit)
pred = predict(glm.fit, test, type = "response")
glm.pred = rep(0, 418)
glm.pred[pred > 0.5] = 1
glm.solution <- data.frame(PassengerID = test$PassengerId, Survived = glm.pred)
glm.solution
write.csv(file = "GLMSolution.csv", glm.solution, row.names = F)
rf.fit = randomForest(factor(Survived) ~ Pclass + Sex + Age + Embarked + SibSp + Parch + Fare + FamilySize + Child + Title, train, importance = TRUE)
summary(rf.fit)
rf.pred = predict(rf.fit, test)
rf.solution <- data.frame(PassengerID = test$PassengerId, Survived = rf.pred)
rf.solution
write.csv(file = "RFSolution.csv", rf.solution, row.names = F)
tran = titanic[1:891, ]
test = titanic[-(1:891), ]
fit.control = trainControl(method = "repeatedcv", number = 5, repeats = 5)
glm.fit = train(y = tran$Survived, x = tran[, c("Pclass", "Sex", "Age", "Fare", "FamilySize", "Title", "Embarked", "SibSp", "Parch", "Child")], method = "glm", trControl = fit.control)
print(glm.fit)
glm.pred = predict.train(glm.fit, test)
glm.solution <- data.frame(PassengerID = test$PassengerId, Survived = glm.pred)
glm.solution
write.csv(file = "GLMSolutionCaret.csv", glm.solution, row.names = F)
rf.fit = train(y = tran$Survived, x = tran[, c("Pclass", "Sex", "Age", "Fare", "FamilySize", "Title", "Embarked", "SibSp", "Parch", "Child")], method = "rf", trControl = fit.control)
print(rf.fit)
rf.pred = predict.train(rf.fit, test)
rf.solution <- data.frame(PassengerID = test$PassengerId, Survived = rf.pred)
rf.solution
write.csv(file = "RFSolutionCaret.csv", rf.solution, row.names = F)
