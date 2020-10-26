library(randomForest)
train <- read.csv("../input/train.csv")
test1 <- read.csv("../input/test.csv")
summary(train)
train$isTrain <- TRUE
test1$isTrain <- FALSE
test1$Survived <- NA
titanicFull <- rbind(train, test1)
m1 <- median(titanicFull$Age, na.rm = T)
titanicFull[is.na(titanicFull$Age), "Age"] <- m1
titanicFull[titanicFull$Embarked == "", "Embarked"] <- "S"
m2 <- median(titanicFull$Fare, na.rm = T)
titanicFull[is.na(titanicFull$Fare), "Fare"] <- m2
titanicFull$Pclass <- as.ordered(titanicFull$Pclass)
titanicFull$Sex <- as.factor(titanicFull$Sex)
titanicFull$Embarked <- as.factor(titanicFull$Embarked)
train <- titanicFull[titanicFull$isTrain == TRUE, ]
test1 <- titanicFull[titanicFull$isTrain == FALSE, ]
train$Survived <- as.factor(train$Survived)
fml <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Fare"
fml <- as.formula(fml)
titanic.model <- randomForest(fml, train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(train))
Survived <- predict(titanic.model, test1)
PassengerId <- test1$PassengerId
op <- as.data.frame(PassengerId)
op$Survived <- Survived
write.csv(op, file = "Titanic_socio.csv", row.names = F)
