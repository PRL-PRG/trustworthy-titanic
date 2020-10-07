library(caret)
library(randomForest)
library(lava)
library(dplyr)
library(rpart)
library(ggplot2)
library(data.table)
library(xgboost)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
test$Survived = NA
full = rbind(train, test)
full$LastName <- gsub("^(.*), .*?\\..*$", "\\1", full$Name)
full$Title <- trim(gsub("^.*,(.*?)\\..*$", "\\1", full$Name))
full$Title <- lava::trim(full$Title)
full$FirstName <- gsub("^.*,.*?\\.[[:space:]]([[:graph:]]*).*$", "\\1", full$Name)
full$FirstName <- gsub("[\\(|\\)]", "", full$FirstName)
full$OtherName <- ifelse(grepl("^.+\\({1}.+[[:space:]](\\w*)\\){1}$", full$Name), gsub("^.+\\({1}.+[[:space:]](\\w*)\\){1}$", "\\1", full$Name), "")
set.seed(1232313)
full$Ticket <- as.character(full$Ticket)
InGroups <- full %>% group_by_at(vars(LastName, Ticket)) %>% dplyr::mutate(GroupNumber = sample(1:1e+06, 1))
set.seed(1232313)
InGroups <- full %>% group_by(Ticket) %>% dplyr::mutate(GroupNumber = sample(1:1e+06, 1))
AdjustGroupNumber <- function(GroupNumber, LastName) {
    NumGroupNumbers <- length(unique(GroupNumber))
    if (NumGroupNumbers == 1) {
        GroupNumber
    }
    else {
        newGroupNumber = sample(1:1e+06, 1)
        gn <- unique(GroupNumber)
        for (each in gn) {
            InGroups[which(InGroups$GroupNumber == each), ]$GroupNumber <- newGroupNumber
        }
        newGroupNumber
    }
}
InGroups <- InGroups %>% group_by(LastName) %>% mutate(GroupNumber = AdjustGroupNumber(GroupNumber, LastName))
InGroups <- InGroups %>% group_by(GroupNumber) %>% mutate(NumInGroupNumber = n())
InGroups <- InGroups %>% group_by_at(vars(SibSp, Parch, Sex)) %>% mutate(Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), ImputedAge = median(Age, na.rm = TRUE))
ticketNumbers <- plyr::count(InGroups, c("Ticket"))
colnames(ticketNumbers)[2] <- "PeoplePerTicket"
InGroups <- merge(InGroups, ticketNumbers, by = "Ticket", all.x = TRUE)
InGroups <- InGroups %>% dplyr::mutate(FarePerPerson = Fare/PeoplePerTicket)
MedianFare <- median(subset(InGroups, Pclass == 3 & FarePerPerson < 19)$FarePerPerson)
InGroups$Fare[is.na(InGroups$Fare)] = MedianFare
InGroups$FarePerPerson[is.na(InGroups$FarePerPerson)] = MedianFare
InGroups$Pclass <- as.factor(InGroups$Pclass)
InGroups$Survived <- as.factor(InGroups$Survived)
InGroups$GroupNumber <- as.factor(InGroups$GroupNumber)
test <- InGroups[is.na(InGroups$Survived), ]
train <- InGroups[!is.na(InGroups$Survived), ]
test$Set <- "test"
train$Set <- "train"
fit <- rpart(Survived ~ Pclass + Sex + Age + NumInGroupNumber + FarePerPerson, data = train)
predict <- predict(fit, test)
predict <- as.data.frame(predict)
test$Survived <- round(predict$"1")
output <- test %>% select(PassengerId, Survived)
write.csv(output, "output.csv", row.names = FALSE)
