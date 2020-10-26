library("ggplot2")
library("ggthemes")
library("scales")
library("dplyr")
library("mice")
library("randomForest")
library("plyr")
library("stringr")
library("caret")
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
full <- bind_rows(train, test)
str(full)
full$Survived <- revalue(factor(full$Survived), c(`1` = "Survived", `0` = "Perished"))
full$Title <- gsub("(.*, )|(\\..*)", "", full$Name)
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
full$Title[full$Title == "Mlle"] <- "Miss"
full$Title[full$Title == "Ms"] <- "Miss"
full$Title[full$Title == "Mme"] <- "Mrs"
full$Title[full$Title %in% rare_title] <- "Rare Title"
full$Fsize <- full$SibSp + full$Parch + 1
full$FsizeD[full$Fsize == 1] <- "singleton"
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- "small"
full$FsizeD[full$Fsize > 4] <- "large"
full$Deck <- factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
full$Embarked[c(62, 830)] <- "C"
full$Fare[1044] <- median(full[full$Pclass == "3" & full$Embarked == "S", ]$Fare, na.rm = TRUE)
isEven <- function(x) x %in% c("0", "2", "4", "6", "8")
isOdd <- function(x) x %in% c("1", "3", "5", "7", "9")
full$Side <- NA
full$Cabin.last.digit <- str_sub(full$Cabin, -1)
full$Side[which(isEven(full$Cabin.last.digit))] <- "port"
full$Side[which(isOdd(full$Cabin.last.digit))] <- "starboard"
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Title", "FsizeD", "Side")
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
set.seed(129)
mice_mod <- mice(full[, !names(full) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Survived")], method = "rf")
mice_output <- complete(mice_mod)
full$Age <- mice_output$Age
full$Deck <- mice_output$Deck
full$Side <- mice_output$Side
full$Child[full$Age < 16] <- "Child"
full$Child[full$Age >= 16] <- "Adult"
full$Mother <- "Not Mother"
full$Mother[full$Sex == "female" & full$Parch > 0] <- "Mother"
full$Child <- factor(full$Child)
full$Mother <- factor(full$Mother)
train <- full[1:891, ]
test <- full[892:1309, ]
set.seed(754)
rf_model <- train(Survived ~ Sex:I(Pclass == "3") + Age + I(exp(Fare)) + Title + Sex * Child + Pclass:Child + Mother * I(FsizeD == "large") + I(FsizeD == "large") * Sex + Sex:I(Embarked == "Q") + Side:I(Pclass == "1"), method = "glm", data = train)
print(rf_model)
prediction <- predict(rf_model, test)
prediction <- revalue(prediction, c(Survived = "1", Perished = "0"))
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = "rf_mod_Solution.csv", row.names = F)
write.csv(solution, file = "rf_mod_Solution.csv", row.names = F)
