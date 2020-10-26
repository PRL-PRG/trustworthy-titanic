library(caret)
library(caretEnsemble)
test <- read.csv("../input/test.csv", colClasses = c("numeric", rep("factor", 3), rep("numeric", 3), "factor", "numeric", rep("factor", 2)))
train <- read.csv("../input/train.csv", colClasses = c("numeric", rep("factor", 4), rep("numeric", 3), "factor", "numeric", rep("factor", 2)))
summary(train)
train$Embarked[train$Embarked == ""] <- "S"
hist(train$Age)
train$Age[is.na(train$Age)] <- median(train$Age, na.rm = TRUE)
boxplot(train$PassengerId ~ train$Survived)
temp <- chisq.test(train$Pclass, train$Survived)
summary(train$Pclass[train$Survived == 0])
summary(train$Pclass[train$Survived == 1])
temp$observed
temp$expected
chisq.test(train$Sex, train$Survived)
boxplot(train$Age ~ train$Survived)
wilcox.test(train$Age ~ train$Survived)
boxplot(train$SibSp ~ train$Survived)
wilcox.test(train$SibSp ~ train$Survived)
mean(train$SibSp[train$Survived == 1])
mean(train$SibSp[train$Survived == 1])
train$SibSpBinary <- 0
train$SibSpBinary[train$SibSp > 0] <- 1
chisq.test(train$SibSpBinary, train$Survived)
boxplot(train$Parch ~ train$Survived)
wilcox.test(train$Parch ~ train$Survived)
boxplot(train$Fare ~ train$Survived)
wilcox.test(train$Fare ~ train$Survived)
kruskal.test(train$Fare ~ train$Pclass)
boxplot(train$Fare ~ train$Pclass)
temp <- chisq.test(train$Embarked, train$Survived)
train$observed
train$expected
train$Title <- gsub("\\..*", "", train$Name)
train$Title <- as.factor(gsub(".*, ", "", train$Title))
summary(train$Title)
train$Title[train$Title == "Mlle" | train$Title == "Ms"] <- "Miss"
train$Title[train$Title == "Mme"] <- "Mrs"
train$Title <- as.character(train$Title)
train$Title[train$Title != "Mrs" & train$Title != "Miss" & train$Title != "Mr" & train$Title != "Master"] <- "Other"
train$Title <- as.factor(train$Title)
temp <- chisq.test(train$Title, train$Survived)
temp$observed
temp$expected
train$Alias <- 0
train$Alias[grepl("\\(", train$Name)] <- 1
train$Alias <- as.factor(train$Alias)
temp <- chisq.test(train$Alias, train$Survived)
temp$observed
temp$expected
temp <- chisq.test(train$Alias, train$Title)
temp$observed
train$NickName <- 0
train$NickName[grepl("\"", train$Name)] <- 1
temp <- chisq.test(train$NickName, train$Survived)
temp$observed
train$Surname <- as.factor(gsub(",.*", "", train$Name))
summary(train$Surname)
train$SurnameBinary <- 0
train$SurnameBinary[duplicated(train$Surname)] <- 1
chisq.test(train$SurnameBinary, train$Survived)
train$Cabin <- as.factor(substr(train$Cabin, 0, 1))
summary(train$Cabin)
train$Cabin[train$Cabin == ""] <- NA
chisq.test(train$Cabin, train$Survived)
train$TicketPrefix <- toupper(gsub(" .*$", "", train$Ticket))
train$TicketPrefix <- gsub("\\.", "", train$TicketPrefix)
train$TicketPrefix <- gsub("/", "", train$TicketPrefix)
train$TicketPrefix <- as.factor(gsub("[0-9]", "", train$TicketPrefix))
train$TicketNumber <- as.numeric(gsub("[^0-9]", "", train$Ticket))
summary(train$TicketPrefix)
train$HasPrefix <- 0
train$HasPrefix[train$TicketPrefix != ""] <- 1
chisq.test(train$HasPrefix, train$Survived)
model <- train(Survived ~ Pclass + Sex + Parch + Fare + Title, data = train, method = "rf")
summary(test)
test$Embarked[test$Embarked == ""] <- "S"
test$Age[is.na(test$Age)] <- median(test$Age, na.rm = TRUE)
test$Fare[is.na(test$Fare)] <- median(test$Fare, na.rm = TRUE)
test$Title <- as.character(gsub("\\..*", "", test$Name))
test$Title <- gsub(".*, ", "", test$Title)
test$Title[test$Title == "Mlle" | test$Title == "Ms"] <- "Miss"
test$Title[test$Title != "Mrs" & test$Title != "Miss" & test$Title != "Mr" & test$Title != "Master"] <- "Other"
test$Title <- as.factor(test$Title)
test$Survived <- predict(model, test)
write.csv(test[c(1, 13)], "predictions.csv", row.names = FALSE)
