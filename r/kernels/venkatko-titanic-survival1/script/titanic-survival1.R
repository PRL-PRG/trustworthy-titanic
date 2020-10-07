library("ggplot2")
library("ggthemes")
library("scales")
library("dplyr")
library("mice")
library("randomForest")
system("ls ../input")
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
full <- bind_rows(train, test)
str(full)
full$Title <- gsub("(.*, )|(\\..*)", "", full$Name)
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
full$Title[full$Title == "Mlle"] <- "Miss"
full$Title[full$Title == "Ms"] <- "Miss"
full$Title[full$Title == "Mme"] <- "Mrs"
full$Title[full$Title %in% rare_title] <- "Rare Title"
full$Surname <- sapply(full$Name, function(x) strsplit(x, split = "[,.]")[[1]][1])
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep = "_")
full$FsizeD[full$Fsize == 1] <- "singleton"
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- "small"
full$FsizeD[full$Fsize > 4] <- "large"
full$Cabin[1:28]
strsplit(full$Cabin[2], NULL)[[1]]
full$Deck <- factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
full[c(62, 830), "Embarked"]
embark_fare <- full %>% filter(PassengerId != 62 & PassengerId != 830)
full$Embarked[c(62, 830)] <- "C"
full[1044, ]
full$Fare[1044] <- median(full[full$Pclass == "3" & full$Embarked == "S", ]$Fare, na.rm = TRUE)
sum(is.na(full$Age))
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Title", "Surname", "Family", "FsizeD")
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
set.seed(129)
mice_mod <- mice(full[, !names(full) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Family", "Surname", "Survived")], method = "rf")
mice_output <- complete(mice_mod)
full$Age <- mice_output$Age
full$Child[full$Age < 18] <- "Child"
full$Child[full$Age >= 18] <- "Adult"
full$Mother <- "Not Mother"
full$Mother[full$Sex == "female" & full$Parch > 0 & full$Age > 18 & full$Title != "Miss"] <- "Mother"
full$Child <- factor(full$Child)
full$Mother <- factor(full$Mother)
md.pattern(full)
train <- full[1:891, ]
test <- full[892:1309, ]
set.seed(754)
str(full)
temp <- full[, c(3, 5, 6, 7, 8, 10, 12, 13, 17, 18, 19, 20)]
str(temp)
X <- as.matrix(sapply(temp, as.numeric))
str(X)
write.csv(X, file = "test.csv", row.names = F)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child + Mother, data = train)
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = "rf_mod_Solution.csv", row.names = F)
