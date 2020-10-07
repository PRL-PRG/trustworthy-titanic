knitr::opts_chunk$set(echo = TRUE)
library(caret)
library(randomForest)
library(plyr)
library(dplyr)
library(ggplot2)
library(mice)
library(gridExtra)
library(stringr)
library(knitr)
training <- read.table("../input/train.csv", sep = ",", header = TRUE, na.strings = c("", "NA"))
testing <- read.table("../input/test.csv", sep = ",", header = TRUE, na.strings = c("", "NA"))
str(training)
training$Survived <- factor(training$Survived)
training$Pclass <- factor(training$Pclass)
mosaicplot(table(training$Survived, training$Sex))
round(prop.table(table(training[, c("Survived", "Sex")])), 2)
mosaicplot(table(training$Survived, training$Pclass))
round(prop.table(table(training[, c("Survived", "Pclass")])), 2)
mosaicplot(table(training$Survived, training$Embarked))
round(table(training[, c("Pclass", "Embarked")]), 2)
training$Embarked[is.na(training$Embarked)] <- "C"
training$Title <- factor(str_extract(training$Name, "\\w+(?=\\.)"))
cbind(table(training$Title, training$Survived))
levels(training$Title) <- c(levels(training$Title), "HonM")
single_ladies <- c("Mlle", "Lady", "Ms")
hon_male <- c("Sir", "Major", "Col", "Jonkheer", "Capt", "Don", "Col")
mrs <- c("Mme", "Countess", "Dona")
training$Title[training$Title %in% single_ladies] <- "Miss"
training$Title[training$Title %in% mrs] <- "Mrs"
training$Title[training$Title %in% hon_male] <- "HonM"
training$Title <- droplevels(training$Title)
cbind(table(training$Title, training$Survived))
training %>% filter(Cabin != "") %>% group_by(Pclass) %>% dplyr::summarize(n = n())
training$BerthAssgn <- factor(ifelse(is.na(training$Cabin), 0, 1))
prop.table(table(training$BerthAssgn, training$Survived))
training$Deck <- substr(training$Cabin, 1, 1)
training$Deck[is.na(training$Deck)] <- "Missing"
training$Deck <- factor(training$Deck)
table(training$Deck, training$Survived)
par(mfrow = c(1, 2))
mosaicplot(table(training$Survived, training$SibSp))
mosaicplot(table(training$Survived, training$Parch))
training$FamilySize <- training$SibSp + training$Parch
ggplot(training, aes(x = FamilySize, fill = Survived)) + geom_histogram(position = "dodge")
training$FamCat <- NA
training$FamCat[training$FamilySize == 0] <- "solo"
training$FamCat[training$FamilySize == 1] <- "pair"
training$FamCat[training$FamilySize == 2] <- "trio"
training$FamCat[training$FamilySize == 3] <- "quartet"
training$FamCat[training$FamilySize >= 4] <- "large"
training$FamCat <- factor(training$FamCat, levels = c("solo", "pair", "trio", "quartet", "large"), ordered = TRUE)
ggplot(training, aes(x = FamCat, fill = Survived)) + geom_bar(position = "dodge")
ggplot(training, aes(y = Fare, x = Survived)) + geom_boxplot() + coord_flip()
summary(subset(training, training$Survived == 0 & training$Fare < 300)$Fare)
summary(subset(training, training$Survived == 1 & training$Fare < 300)$Fare)
ggplot(filter(training, Fare < 400), aes(x = Fare, color = Survived)) + geom_density() + geom_vline(xintercept = 13)
ggplot(filter(training, Fare < 400), aes(x = Fare, color = Pclass)) + geom_density() + geom_vline(xintercept = 13)
training$CheapTix <- factor(ifelse(training$Fare <= 13, 1, 0))
training$FareBin <- cut(training$Fare, breaks = 4, ordered_result = TRUE)
training %>% arrange(Fare) %>% select(Survived, Pclass, Sex, Fare) %>% head(15)
training$Fare[training$Fare == 0] <- 0.01
training$FareLog <- log(training$Fare)
ggplot(training, aes(x = Age, y = Pclass, color = Survived)) + geom_jitter(height = 0.1, alpha = 0.3) + facet_grid(Sex ~ .)
training %>% filter(!is.na(Age)) %>% group_by(Survived) %>% dplyr::summarize(n = n(), survival = n()/(424 + 290))
training %>% group_by(Survived) %>% dplyr::summarize(n = n(), survival = n/nrow(training))
set.seed(417)
tempData <- mice(training[, !names(training) %in% c("PassengerId", "Name", "Ticket", "Cabin")], method = "pmm")
mice_output <- complete(tempData)
p1 <- ggplot(training, aes(x = Age)) + geom_histogram()
p2 <- ggplot(mice_output, aes(x = Age)) + geom_histogram()
grid.arrange(p1, p2, ncol = 2)
summary(training$Age)
summary(mice_output$Age)
training$Age <- mice_output$Age
sum(is.na(training$Age))
ggplot(training, aes(x = Age, color = Survived)) + geom_density() + geom_vline(xintercept = 14)
training$Child <- factor(ifelse(training$Age <= 14, 1, 0))
training$AgeBin <- cut(training$Age, breaks = 5, ordered_result = TRUE)
drops <- c("PassengerId", "Ticket", "Cabin", "Name")
training <- training[, !(names(training) %in% drops)]
nearZero <- nearZeroVar(training, saveMetrics = TRUE)
nearZero[nearZero$nzv == TRUE, ]
table(training$FareBin)
training <- select(training, -FareBin)
fit.back <- step(glm(Survived ~ ., data = training, family = "binomial"), direction = "backward")
null <- glm(Survived ~ 1, data = training, family = "binomial")
full <- glm(Survived ~ ., data = training, family = "binomial")
fit.forward <- step(null, scope = list(lower = null, upper = full), direction = "forward")
fit.both <- step(glm(Survived ~ ., data = training, family = "binomial"), direction = "both")
summary(fit.back)$call
summary(fit.both)$call
summary(fit.forward)$call
form1 <- formula(Survived ~ Pclass + Age + Title + FamCat + FareLog)
form2 <- formula(Survived ~ Pclass + Age + Title + FamCat + FareLog + BerthAssgn)
set.seed(417)
intrain <- createDataPartition(y = training$Survived, p = 0.6, list = FALSE)
train <- training[intrain, ]
test <- training[-intrain, ]
dim(train)
dim(test)
round(prop.table(table(train$Survived)), 3)
round(prop.table(table(test$Survived)), 3)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
methods <- c("glm", "glmnet", "rpart", "ranger", "svmRadial", "knn")
fits <- vector("list")
pred <- vector("list")
acc <- vector("list")
for (m in methods) {
    set.seed(417)
    fits[[m]] <- train(form1, data = train, method = m, trControl = ctrl, tuneLength = 5)
}
for (mod in names(fits)) {
    pred[[mod]] <- predict(fits[[mod]], test)
    acc[[mod]] <- mean(test$Survived == pred[[mod]])
}
acc.df1 <- as.data.frame(acc)
pred.df1 <- as.data.frame(pred)
acc.df1
fits <- vector("list")
pred <- vector("list")
acc <- vector("list")
for (m in methods) {
    set.seed(417)
    fits[[m]] <- train(form2, data = train, method = m, trControl = ctrl, tuneLength = 5)
}
for (mod in names(fits)) {
    pred[[mod]] <- predict(fits[[mod]], test)
    acc[[mod]] <- mean(test$Survived == pred[[mod]])
}
acc.df2 <- as.data.frame(acc)
pred.df2 <- as.data.frame(pred)
acc.df2
acc.all <- rbind(acc.df1, acc.df2)
acc.all
testing$Pclass <- factor(testing$Pclass)
testing$Title <- factor(str_extract(testing$Name, "\\w+(?=\\.)"))
levels(testing$Title) <- c(levels(testing$Title), "HonM")
single_ladies <- c("Mlle", "Lady", "Ms")
hon_male <- c("Sir", "Major", "Col", "Jonkheer", "Capt", "Don", "Col")
mrs <- c("Mme", "Countess", "Dona")
testing$Title[testing$Title %in% single_ladies] <- "Miss"
testing$Title[testing$Title %in% mrs] <- "Mrs"
testing$Title[testing$Title %in% hon_male] <- "HonM"
testing$Title <- droplevels(testing$Title)
testing$BerthAssgn <- factor(ifelse(is.na(testing$Cabin), 0, 1))
testing$Deck <- substr(testing$Cabin, 1, 1)
testing$Deck[is.na(testing$Deck)] <- "Missing"
testing$Deck <- factor(testing$Deck)
testing$FamilySize <- testing$SibSp + testing$Parch
testing$FamCat <- NA
testing$FamCat[testing$FamilySize == 0] <- "solo"
testing$FamCat[testing$FamilySize == 1] <- "pair"
testing$FamCat[testing$FamilySize == 2] <- "trio"
testing$FamCat[testing$FamilySize == 3] <- "quartet"
testing$FamCat[testing$FamilySize >= 4] <- "large"
testing$FamCat <- factor(testing$FamCat, levels = c("solo", "pair", "trio", "quartet", "large"), ordered = TRUE)
testing$Fare[is.na(testing$Fare)] <- mean(testing$Fare, na.rm = TRUE)
testing$CheapTix <- factor(ifelse(testing$Fare <= 13, 1, 0))
testing$Fare[testing$Fare == 0] <- 0.01
testing$FareLog <- log(testing$Fare)
set.seed(417)
tempData <- mice(testing[, !names(testing) %in% c("PassengerId", "Name", "Ticket", "Cabin")], method = "pmm")
mice_output <- complete(tempData)
testing$Age <- mice_output$Age
testing$Child <- factor(ifelse(testing$Age <= 14, 1, 0))
testing$AgeBin <- cut(testing$Age, breaks = 5, ordered_result = TRUE)
drops <- c("Ticket", "Cabin", "Name")
testing <- testing[, !(names(testing) %in% drops)]
form2
set.seed(417)
fit.best <- train(form2, data = train, method = "ranger", trControl = ctrl, tuneLength = 5)
pred.best <- predict(fit.best, test)
table(test$Survived, pred.best)
mean(test$Survived == pred.best)
set.seed(417)
testing$Survived <- predict(fit.best, testing)
submission <- testing[, c("PassengerId", "Survived")]
write.csv(submission, file = "submission.csv", row.names = FALSE)
