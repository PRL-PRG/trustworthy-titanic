train <- read.csv("../input/train.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""))
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""))
str(train)
str(test)
library(plyr)
total <- rbind.fill(train, test)
str(total)
total$Survived <- factor(total$Survived)
total$Pclass <- factor(total$Pclass)
total$Sex <- factor(total$Sex)
total$FamilySize <- 1 + total$SibSp + total$Parch
table(total$FamilySize)
table(total$FamilySize, total$Survived)
total$FamilySize1 <- "N"
total$FamilySize1[total$FamilySize <= 1] <- "alone"
total$FamilySize1[total$FamilySize >= 2 & total$FamilySize <= 3] <- "small_family"
total$FamilySize1[total$FamilySize >= 4 & total$FamilySize <= 7] <- "medium_family"
total$FamilySize1[total$FamilySize >= 8] <- "large_family"
table(total$FamilySize1)
table(total$FamilySize1, total$Sex, total$Survived)
table(total$FamilySize1, total$Survived)
total$FamilySize1 <- factor(total$FamilySize1)
library(stringr)
total$title <- str_sub(total$Name, str_locate(total$Name, ",")[, 1] + 2, str_locate(total$Name, "\\.")[, 1] - 1)
table(total$title)
table(total$title, total$Survived)
Rare_Title <- c("Mme", "Mlle", "Don", "Lady", "Dona", "Jonkheer", "the Countess", "Sir")
Profession <- c("Capt", "Col", "Dr", "Major", "Rev")
Female_title <- c("Miss", "Ms")
Married_Female_title <- c("Mrs")
Master_title <- c("Master")
Male_title <- c("Mr")
total$title[total$title %in% Rare_Title] <- "Rare_Title"
total$title[total$title %in% Profession] <- "Profession"
total$title[total$title %in% Female_title] <- "Female_title"
total$title[total$title %in% Married_Female_title] <- "Married_Female_title"
total$title[total$title %in% Master_title] <- "Master_title"
total$title[total$title %in% Male_title] <- "Male_title"
str(total)
total$title <- factor(total$title)
sapply(total, function(x) sum(is.na(x)))
library(Amelia)
require(Amelia)
missmap(total, main = "Missing Map")
library(mice)
init = mice(total, maxit = 0)
meth = init$method
predM = init$predictorMatrix
predM[, c("PassengerId", "Survived", "Name", "Ticket", "Cabin", "title")] = 0
meth[c("Survived", "Cabin")] = ""
meth[c("Pclass")] = "pmm"
meth[c("Sex")] = "logreg"
meth[c("SibSp")] = "norm"
meth[c("Parch")] = "norm"
meth[c("Fare")] = "norm"
meth[c("Embarked")] = "pmm"
set.seed(103)
imputed <- mice(total, method = meth, predictorMatrix = predM, m = 5)
imputed <- complete(imputed)
sapply(imputed, function(x) sum(is.na(x)))
table(imputed$Embarked)
imputed$Embarked[is.na(imputed$Embarked)] <- "S"
Clean_data <- na.omit(total$Age)
actual <- Clean_data
predicted <- imputed$Age
mean(actual)
mean(predicted)
hist(Clean_data, main = "Clean data for Age", xlab = "Age", col = "green")
hist(imputed$Age, main = "Predicted Age", xlab = "Age", col = "blue")
table(imputed$Age, imputed$Survived)
imputed$child_adult <- "N"
str(imputed)
imputed$child_adult[imputed$Age <= 12] <- "child"
imputed$child_adult[imputed$Age >= 13 & imputed$Age <= 19] <- "teenager"
imputed$child_adult[imputed$Age >= 20] <- "adult"
table(imputed$Age)
table(imputed$child_adult, imputed$Survived)
imputed$Embarked <- factor(imputed$Embarked)
imputed$child_adult <- factor(imputed$child_adult)
train1 <- imputed[1:891, ]
test1 <- imputed[892:1309, ]
library(caTools)
set.seed(3000)
spl = sample.split(train1$Survived, SplitRatio = 0.7)
Tra = subset(train1, spl == TRUE)
Tst = subset(train1, spl == FALSE)
str(Tra)
str(Tst)
library(randomForest)
TitanicForest = randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + child_adult + Embarked + FamilySize + FamilySize1 + Fare + title, data = Tra, ntree = 500, nodesize = 25)
PredictForest = predict(TitanicForest, newdata = Tst)
table(Tst$Survived, PredictForest)
(149 + 68)/(149 + 16 + 35 + 68)
importance(TitanicForest)
prediction1 <- predict(TitanicForest, test1)
solution <- data.frame(PassengerID = test1$PassengerId, Survived = prediction1)
write.csv(solution, file = "RandomForest_mod.csv", row.names = FALSE)
