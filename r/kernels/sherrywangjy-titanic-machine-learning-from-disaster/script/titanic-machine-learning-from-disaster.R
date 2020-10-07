library(randomForest)
library(mice)
library(lattice)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
test$Survived <- 0
dataTita <- rbind(train, test)
str(dataTita)
dataTita$Name <- as.character(dataTita$Name)
dataTita$title <- sapply(dataTita$Name, FUN = function(x) {
    strsplit(x, "[,.]")[[1]][[2]]
})
dataTita$title <- gsub(" ", "", dataTita$title)
dataTita$title[dataTita$title %in% c("Mlle", "Ms", "Lady")] <- "Miss"
dataTita$title[dataTita$title == "Mme"] <- "Mrs"
dataTita$title[dataTita$title %in% c("Capt", "Col", "Don", "Dona", "Dr", "Jonkheer", "Master", "Major", "Rev", "Sir", "theCountess")] <- "rare title"
dataTita$Family <- apply(dataTita[, names(dataTita) %in% c("SibSp", "Parch")], 1, sum)
apply(dataTita, 2, FUN = function(x) {
    sum(is.na(x))
})
apply(dataTita, 2, FUN = function(x) {
    sum(x == "")
})
dataTita$Embarked[c(62, 830)] <- "S"
dataTita$Fare[1044] <- mean(dataTita[which(dataTita$Pclass == 3 & dataTita$PassengerId != 1044), 9])
imp <- mice(dataTita[, names(dataTita) %in% c("Pclass", "Age", "SibSp", "Parch", "Fare")], m = 5, meth = "rf", seed = 1234)
densityplot(imp)
dataTita <- cbind(dataTita[, !names(dataTita) %in% c("Pclass", "Age", "SibSp", "Parch", "Fare")], complete(imp, 1))
dataTita$title <- as.factor(dataTita$title)
dataTita$Embarked <- as.factor(dataTita$Embarked)
dataTita$Sex <- as.factor(dataTita$Sex)
train <- dataTita[1:891, ]
test <- dataTita[892:1309, !names(dataTita) %in% c("Survived")]
train$title <- as.factor(train$title)
train$Embarked <- as.factor(train$Embarked)
train$Sex <- as.factor(train$Sex)
train.rf <- randomForest(factor(Survived) ~ Age + Pclass + Family + Fare + title + Embarked + Sex, data = train, ntree = 50)
train.rf
test.rf <- predict(train.rf, test)
testresult <- data.frame(PassengerId = test$PassengerId, Survived = test.rf)
write.csv(testresult, "result.csv", row.names = FALSE)
