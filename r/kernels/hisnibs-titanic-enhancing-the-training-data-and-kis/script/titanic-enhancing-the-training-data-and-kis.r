train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
train$Set <- rep("Train", nrow(train))
test$Set <- rep("Test", nrow(test))
test$Survived <- NA
titanic <- rbind(train, test)
titanic$NGrp <- table(titanic$Ticket)[titanic$Ticket]
titanic$Deck <- tapply(substr(as.character(titanic$Cabin), 1, 1), titanic$Ticket, max)[titanic$Ticket]
attach(titanic)
sumNotNA <- function(x) sum(x[!is.na(x)])
titanic$KSIG <- tapply(Survived, Ticket, sumNotNA)[Ticket] - ifelse(is.na(Survived), 0, Survived)
titanic$KDIG <- tapply(1 - Survived, Ticket, sumNotNA)[Ticket] - ifelse(is.na(Survived), 0, 1 - Survived)
titanic$UOIG <- tapply(Survived, Ticket, function(x) sum(is.na(x)))[Ticket] - is.na(Survived)
titanic$ESRIG <- titanic$KSIG/(titanic$KSIG + titanic$KDIG)
detach()
Title <- substring(lapply(strsplit(as.character(titanic$Name), ", "), "[", 2), 1, 4)
Title[Title == "Capt" | Title == "Majo" | Title == "Col."] <- "Army"
Title[Title == "Don."] <- "Mr. "
Title[Title == "Dona"] <- "Mrs."
Title[Title == "Mlle"] <- "Miss"
Title[Title == "Mme."] <- "Mrs."
Title[Title == "Ms. "] <- "Miss"
Title[Title == "the "] <- "Lady"
Title[Title == "Jonk"] <- "Sir."
titanic$Title <- Title
rm(Title)
library(rpart)
fm <- rpart(Survived ~ Sex + NGrp + Pclass + Age + SibSp + Parch + Fare + Deck + Title + Embarked + KSIG + KDIG + UOIG + ESRIG, data = titanic, subset = titanic$Set == "Train", method = "class")
pred <- round(predict(fm, newdata = titanic)[, 2])
submission <- data.frame(PassengerId = titanic$PassengerId, Survived = pred)[titanic$Set == "Test", ]
write.csv(submission, file = "rpartTheLot.csv", row.names = F, quote = F)
fm
table(titanic$Survived, pred)
