library(randomForest)
library(dplyr)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
test$Survived <- NA
full <- rbind(train, test)
prop.table(table(train$Sex, train$Survived), 1)
full$Title <- gsub("(.*, )|(\\..*)", "", full$Name)
table(full$Title)
rare <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
full$Title[full$Title == "Mlle"] <- "Miss"
full$Title[full$Title == "Ms"] <- "Miss"
full$Title[full$Title == "Mme"] <- "Mrs"
full$Title[full$Title %in% rare] <- "Rare"
table(full$Title)
full$Embarked[full$Embarked == ""] = "S"
full$Fsize <- full$SibSp + full$Parch + 1
full$FsizeD[full$Fsize == 1] <- "Single"
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- "Small"
full$FsizeD[full$Fsize > 4] <- "Big"
full$Pclass = factor(full$Pclass)
full$Sex = factor(full$Sex)
full$Embarked = factor(full$Embarked)
full$Title = factor(full$Title)
full$FsizeD = factor(full$FsizeD)
train1 <- full[1:891, ]
test1 <- full[892:1309, ]
set.seed(1234)
rf <- randomForest(factor(Survived) ~ Pclass + Sex + Embarked + Title + FsizeD, data = train1, ntree = 100)
varImpPlot(rf)
plot(rf)
prediction <- predict(rf, test1)
result <- data.frame(PassengerID = test1$PassengerId, Survived = prediction)
write.csv(result, file = "Titanic.csv", row.names = F)
