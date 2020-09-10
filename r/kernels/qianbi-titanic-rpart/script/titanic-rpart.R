library(dplyr)
library(rpart)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE,
  na.strings = "")
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE,
  na.strings = "")
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
full$FsizeD[full$Fsize >1 & full$Fsize < 5] <- "Small"
full$FsizeD[full$Fsize > 4] <- "Large"
full$Child <- ifelse(full$Age <= 12, 1, 0)
factor_var <- c("Survived", "Pclass", "Sex", "Embarked",
  "Title", "FsizeD", "Child")
full[factor_var] <- lapply(full[factor_var], factor)
train <- full[1 : 891, ]
test <- full[892 : 1309, ]
fit1 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch +
  Fare + Embarked + Title + Fsize + FsizeD + Child,
  data = train, method = "class")
res <- predict(fit1, newdata = test)
test$Survived <- ifelse(res[, 1] < res[, 2], 1, 0)
write.csv(test[, c(1, 2)], file = "Titanic_rpart.csv",
  row.names = FALSE)
