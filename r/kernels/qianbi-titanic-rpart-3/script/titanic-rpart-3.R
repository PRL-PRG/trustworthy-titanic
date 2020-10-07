library(dplyr)
library(rpart)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE, na.strings = "")
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE, na.strings = "")
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
full$Child <- ifelse(full$Age <= 12, 1, 0)
full$Mother <- ifelse(full$Sex == "female" & full$Parch > 0 & full$Age > 18 & full$Title != "Miss", 1, 0)
full$nTicket[!is.na(full$Ticket)] <- sapply(full$Ticket[!is.na(full$Ticket)], function(e) table(full$Ticket)[[e]])
full$fTicket <- full$Ticket
full$fTicket[full$nTicket == 1] <- NA
full$fTicket[!full$fTicket %in% full$fTicket[1:891]] <- NA
full$nCabin[!is.na(full$Cabin)] <- sapply(full$Cabin[!is.na(full$Cabin)], function(e) table(full$Cabin)[[e]])
full$fCabin <- full$Cabin
full$fCabin[full$nCabin == 1] <- NA
full$fCabin[!full$fCabin %in% full$fCabin[1:891]] <- NA
full$Fare[full$Fare[!is.na(full$Fare)] == 0] <- NA
factor_var <- c("Survived", "Pclass", "Sex", "Embarked", "Title", "FsizeD", "Child", "Mother", "fTicket", "fCabin")
full[factor_var] <- lapply(full[factor_var], factor)
train <- full[1:891, ]
test <- full[892:1309, ]
fit <- rpart(Survived ~ Pclass + Sex + Age + Fare + Embarked + Title + FsizeD + Child + Mother + fTicket + fCabin, data = train, method = "class")
res <- predict(fit, newdata = test)
test$Survived <- ifelse(res[, 1] < res[, 2], 1, 0)
write.csv(test[, c(1, 2)], file = "Titanic_rpart_3.csv", row.names = FALSE)
