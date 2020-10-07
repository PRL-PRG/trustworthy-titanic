library(mice)
library(dplyr)
library(randomForest)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE, na.string = "")
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE, na.string = "")
full <- bind_rows(train, test)
full$Title <- gsub("^.*, |\\. .*$", "", full$Name)
Mr <- c("Col", "Don", "Jonkheer", "Sir")
Mrs <- c("Dona", "Lady", "Mme", "the Countess")
Miss <- c("Mlle", "Ms")
full$Title[full$Title %in% Mr] <- "Mr"
full$Title[full$Title %in% Mrs] <- "Mrs"
full$Title[full$Title %in% Miss] <- "Miss"
full$Fsize <- full$SibSp + full$Parch + 1
full$FsizeD[full$Fsize == 1] <- "Singleton"
full$FsizeD[full$Fsize > 1 & full$Fsize < 5] <- "Small"
full$FsizeD[full$Fsize > 4] <- "Large"
factor_var <- c("Survived", "Pclass", "Sex", "Embarked", "Title", "FsizeD")
full[factor_var] <- lapply(full[factor_var], factor)
mice_mod <- mice(full[, c(3, 5:8, 10, 12)], method = "rf")
mice_output <- complete(mice_mod)
full[, c(6, 10, 12)] <- mice_output[, c(3, 6, 7)]
full$Child <- factor(ifelse(full$Age <= 12, 1, 0))
train <- full[1:nrow(train), ]
test <- full[(nrow(train) + 1):nrow(full), ]
rf_mod <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child, data = train)
test$Survived <- predict(rf_mod, test)
write.csv(test[, c(1, 2)], file = "Titanic_randomForest.csv", row.names = FALSE)
