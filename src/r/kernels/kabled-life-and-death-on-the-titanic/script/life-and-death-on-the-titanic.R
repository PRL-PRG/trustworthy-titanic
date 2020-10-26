knitr::opts_chunk$set(echo = TRUE)
library("ggplot2")
library("dplyr")
library("stringr")
library("randomForest")
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
full <- bind_rows(train, test)
str(full)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + geom_bar(position = "dodge") + labs(x = "Pclass") + ggtitle("Figure 1", subtitle = "Survival by Passenger Class")
head(full$Name, 15)
extractTitle <- function(name) {
    name <- as.character(name)
    if (length(grep("Miss.", name)) > 0) {
        return("Miss.")
    }
    else if (length(grep("Master.", name)) > 0) {
        return("Master.")
    }
    else if (length(grep("Mrs.", name)) > 0) {
        return("Mrs.")
    }
    else if (length(grep("Mr.", name)) > 0) {
        return("Mr.")
    }
    else {
        return("Other")
    }
}
titles <- NULL
for (i in 1:nrow(full)) {
    titles <- c(titles, extractTitle(full[i, "Name"]))
}
full$Title <- as.factor(titles)
ggplot(full[1:891, ], aes(x = Title, fill = factor(Survived))) + geom_bar(position = "dodge") + labs(x = "Title") + ggtitle("Figure 2", subtitle = "Survival by Title")
full$Fsize <- as.factor(1 + full$SibSp + full$Parch)
ggplot(full[1:891, ], aes(x = Fsize, fill = factor(Survived))) + geom_bar(position = "dodge") + labs(x = "Fsize") + ggtitle("Figure 3", subtitle = "Survival by Family Size")
extractT1 <- function(ticket) {
    ticket <- as.character(ticket)
    library("stringi")
    temp <- NULL
    if (is.na(stri_extract_first_regex(ticket, "[0-9]+"))) {
        temp <- 0
    }
    else {
        temp <- stri_extract_first_regex(ticket, "[0-9]+")
    }
    temp <- as.integer(substr(temp, 1, 1))
    return(temp)
}
tickets <- NULL
for (i in 1:nrow(full)) {
    tickets <- c(tickets, extractT1(full[i, "Ticket"]))
}
full$T1 <- as.factor(tickets)
ggplot(full[1:891, ], aes(x = T1, fill = factor(Survived))) + geom_bar(position = "dodge") + labs(x = "T1") + ggtitle("Figure 4", subtitle = "Survival by T1")
summary(full$Fare)
which(is.na(full$Fare))
full[1044, ]
full$Fare[1044] <- median(full$Fare[which(full$Fsize == "1" & full$Pclass == "3" & full$Embarked == "S")], na.rm = TRUE)
full$Fare[1044]
full$Embarked <- as.factor(full$Embarked)
summary(full$Embarked)
which(full$Embarked == "")
full$Embarked[c(62, 830)] <- "S"
full$Embarked <- factor(full$Embarked)
rf.train.1 <- full[1:891, c("Pclass", "Title", "Fsize")]
rf.label <- as.factor(train$Survived)
set.seed(1357)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)
rf.train.2 <- full[1:891, c("Pclass", "Title", "Fsize", "Embarked")]
set.seed(1357)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)
full$Sex <- as.factor(full$Sex)
rf.train.3 <- full[1:891, c("Pclass", "Title", "Fsize", "Sex")]
set.seed(1357)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)
rf.train.4 <- full[1:891, c("Pclass", "Title", "Fsize", "Sex", "T1")]
set.seed(1357)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)
rf.train.5 <- full[1:891, c("Pclass", "Title", "Fsize", "Sex", "T1", "Fare")]
set.seed(1357)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)
train <- full[1:891, ]
test <- full[892:1309, ]
set.seed(1357)
rfm <- randomForest(factor(Survived) ~ Pclass + Title + Fsize + Sex + T1, data = train, importance = TRUE, ntree = 1000)
prediction <- predict(rfm, test)
submit <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(submit, file = "titanic_rf.csv", row.names = F)
