## ----libraries, include=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2) #gprahs
library(knitr)
library(dplyr)#piping operator + data manipulation
library(gridExtra) #plot multiple charts 
library(Amelia) #plot a heatmap with missinn values
library(caret) #models
library(randomForest)


## ----data, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full <- read.csv("../input/train.csv", stringsAsFactors = T) #1:891 - fulling set
test <- read.csv("../input/test.csv", stringsAsFactors = T) #892:1309 - test set
full <- bind_rows(full, test)


## ----giveninfo, echo = F---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kable(data.frame(Variable = c("PassengerId", "Survived", "Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Ticket", "Fare", "Cabin", "Embarked"),
           Definition = c("PassengerId", "Survival", "Ticket Class", "Name", "Sex", "Age", "# of siblings / spouses aboard", "# of parents / children aboard", 
                          "Ticket number", "Passenger fare", "Cabin number", "Port of Embarkation"),
           Key = c("", "1 = Yes, 0 = No", "1 = 1st, 2 = 2nd, <br>3 = 3rd", "", "", "", "", "", "", "", "", "C = Cherbourg, <br>Q = Queenstown, <br>S = Southampton"),
           Notes = c("", "", "A proxy for socio-economic status (SES) <br>1st = Upper, 2nd = Middle, 3rd = Lower", "", "", 
                     "Age is fractional if less than 1. <br>If the age is estimated, is it in the form of xx.5", 
                     "Sibling = brother, sister, stepbrother, stepsister, <br>Spouse = husband, wife (mistresses and fiancés were ignored)",
                     "Parent = mother, father, <br>Child = daughter, son, stepdaughter, stepson, <br>Some children travelled only with a nanny, therefore parch=0 for them",
                     "", "", "", "")))


## ----summary---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(full)
summary(full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(full)
tail(full)


## ----NA plot, fig.width=7, fig.height=3------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missmap(full, col = c("white","darkblue"), legend = F,  main = "Missing Values")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Embarked[full$Embarked == ""] <- "S"
full$Fare[is.na(full$Fare)] <- round(mean(full$Fare, na.rm = T),0)
full$Pclass <- as.factor(full$Pclass)
full$Survived <- as.factor(full$Survived)
full$Embarked <- as.factor(full$Embarked)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Age.old <- full$Age
fit <- train(Age~Pclass + Sex + Embarked + SibSp + Parch + Fare, data = full[!is.na(full$Age),], method="rf", trControl=trainControl(method="cv", number=10))
new <- cbind(full[is.na(full$Age),], predict(fit, full[is.na(full$Age),]))
full$Age[is.na(full$Age)] <- new[,14]
full$Age <- as.numeric(full$Age)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(full$Age)
summary(full$Embarked)


## ----fig.height=4----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = full[!is.na(full$Survived),], aes(as.factor(Pclass), fill = as.factor(Survived))) +
     geom_bar(position = 'fill') +
     labs(x = "Ticket Class") +
     ggtitle("Survivals by the Ticket Class") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))


## ----fig.height=4----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = full[!is.na(full$Survived),], aes(Sex, fill = as.factor(Survived))) +
     geom_bar(position = 'fill') +
     labs(x = "Sex") +
     ggtitle("Survivals by the sex") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))


## ----fig.width=10, fig.height=5--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p1 <- ggplot(data = full[!is.na(full$Survived),], aes(as.factor(SibSp), fill = as.factor(Survived))) +
     geom_bar(position = 'fill') +
     labs(x = "Number of Siblings") +
     ggtitle("Survivals by the number of siblings/spouses") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))

p2 <- ggplot(data = full[!is.na(full$Survived),], aes(as.factor(Parch), fill = as.factor(Survived))) +
     geom_bar(position = 'fill') +
     labs(x = "Number of Parents/Children") +
     ggtitle("Survivals by the number of Parents/Children") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))

p3 <- ggplot(data = full[!is.na(full$Survived),], aes(as.factor(SibSp), fill = as.factor(Survived))) +
     geom_bar() +
     labs(x = "Number of Siblings") +
     ggtitle("Survivals by the number of siblings/spouses") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))

p4 <- ggplot(data = full[!is.na(full$Survived),], aes(as.factor(Parch), fill = as.factor(Survived))) +
     geom_bar() +
     labs(x = "Number of Parents/Children") +
     ggtitle("Survivals by the number of Parents/Children") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))
grid.arrange(p1,p2,p3,p4, nrow = 2, ncol = 2)


## ----fig.height=4----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = full[!is.na(full$Survived),], aes(x = Age, fill = as.factor(Survived))) +
     geom_histogram(bins = 15, aes(y = ..density..)) +
     labs(x = "Number of Siblings") +
     ggtitle("Survivals by age") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5)) +
     stat_function(fun = dnorm, 
                args = list(mean = mean(full$Age[full$Survived == "0"]), sd = sd(full$Age[full$Survived == "0"])), 
                lwd = 1, 
                col = 'red') + 
     stat_function(fun = dnorm, 
                args = list(mean = mean(full$Age[full$Survived == "1"]), sd = sd(full$Age[full$Survived == "1"])), 
                lwd = 1, 
                col = 'darkgreen')


## ----fig.width=9, fig.height=4---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p5 <- ggplot(data = full[!is.na(full$Survived),], aes(Embarked, fill = as.factor(Survived))) +
     geom_bar(position = 'fill') +
     labs(x = "Port of Embarkation") +
     ggtitle("Survivals by port of Embarkation") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))

p6 <- ggplot(data = full[!is.na(full$Survived),], aes(Embarked, fill = as.factor(Survived))) +
     geom_bar() +
     labs(x = "Port of Embarkation") +
     ggtitle("Survivals by port of Embarkation") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))

grid.arrange(p5,p6, ncol = 2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(full$Name, n = 10)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Title[grepl("Mr\\.", full$Name, ignore.case = T)] <- "Mr"
full$Title[grepl("Miss\\.|Mlle\\.|Ms\\.|Mme\\.", full$Name, ignore.case = T)] <- "Miss" 
full$Title[grepl("Mrs\\.", full$Name, ignore.case = T)] <- "Mrs"   
full$Title[grepl("Master\\.", full$Name, ignore.case = T)] <- "Master"  
full$Title[grepl("Don\\.", full$Name, ignore.case = T)] <- "Rare"
full$Title[grepl("Rev\\.", full$Name, ignore.case = T)] <- "Rare"
full$Title[grepl("Dr\\.", full$Name, ignore.case = T)] <- "Rare"
full$Title[grepl("Capt\\.", full$Name, ignore.case = T)] <- "Rare"
full$Title[grepl("Col\\.", full$Name, ignore.case = T)] <- "Rare"
full$Title[grepl("Sir\\.", full$Name, ignore.case = T)] <- "Rare"
full$Title[grepl("Major\\.", full$Name, ignore.case = T)] <- "Rare"
full$Title[grepl("Jonkheer\\.", full$Name, ignore.case = T)] <- "Rare"
full$Title[grepl("Countess\\.", full$Name, ignore.case = T)] <- "Rare"
full$Title[grepl("Lady\\.", full$Name, ignore.case = T)] <- "Rare"
full$Title[grepl("Dona\\.", full$Name, ignore.case = T)] <- "Rare"
full$Title <- as.factor(full$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(full$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = full[!is.na(full$Survived),], aes(as.factor(Title), fill = as.factor(Survived))) +
     geom_bar(position = 'fill') +
     labs(x = "Title") +
     ggtitle("Survivals by the Title") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full %>% group_by(Title) %>% summarise(mean.age = mean(Age))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(full$Ticket)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Ticket.n[grepl("^1", gsub("[^0-9\\|]", "", full$Ticket))] <- 1
full$Ticket.n[grepl("^2", gsub("[^0-9\\|]", "", full$Ticket))] <- 2
full$Ticket.n[grepl("^3", gsub("[^0-9\\|]", "", full$Ticket))] <- 3
full$Ticket.n[grepl("^4", gsub("[^0-9\\|]", "", full$Ticket))] <- 4
full$Ticket.n[grepl("^5", gsub("[^0-9\\|]", "", full$Ticket))] <- 5
full$Ticket.n[grepl("^6", gsub("[^0-9\\|]", "", full$Ticket))] <- 6
full$Ticket.n[grepl("^7", gsub("[^0-9\\|]", "", full$Ticket))] <- 7
full$Ticket.n[grepl("^8", gsub("[^0-9\\|]", "", full$Ticket))] <- 8
full$Ticket.n[grepl("^9", gsub("[^0-9\\|]", "", full$Ticket))] <- 9
full$Ticket.n[full$Ticket == "LINE"] <- "Line"
full$Ticket.n <- as.factor(full$Ticket.n)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(full$Ticket.n)

## ----fig.width=15, fig.height=4--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p7 <- ggplot(data = full[!is.na(full$Survived),], aes(as.factor(Ticket.n), fill = as.factor(Survived))) +
     geom_bar(position = 'fill') +
     labs(x = "Ticket Number") +
     ggtitle("Survivals by the Ticket") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))

p8 <- ggplot(data = full[!is.na(full$Survived),], aes(as.factor(Ticket.n), fill = as.factor(Survived))) +
     geom_bar() +
     labs(x = "Ticket Number") +
     ggtitle("Survivals by the Ticket") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))

p9 <- ggplot(data = full[!is.na(full$Survived),], aes(as.factor(Ticket.n), fill = as.factor(Survived))) +
     geom_bar() +
     labs(x = "Ticket Number") +
     ggtitle("Survivals by the Ticket") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5)) +
     facet_grid(.~Pclass)

grid.arrange(p7,p8,p9, ncol = 3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Age.n[full$Age < 5] <- "<5"
full$Age.n[full$Age < 10 & full$Age >= 5] <- "5-10"
full$Age.n[full$Age < 20 & full$Age >= 10] <- "10-20"
full$Age.n[full$Age < 25 & full$Age >= 20] <- "20-25"
full$Age.n[full$Age < 28 & full$Age >= 25] <- "25-27"
full$Age.n[full$Age < 30 & full$Age >= 28] <- "28-29"
full$Age.n[full$Age < 33 & full$Age >= 30] <- "30-31"
full$Age.n[full$Age < 35 & full$Age >= 33] <- "32-34"
full$Age.n[full$Age < 40 & full$Age >= 35] <- "35-40"
full$Age.n[full$Age < 50 & full$Age >= 40] <- "40-50"
full$Age.n[full$Age < 100 & full$Age >= 50] <- "50+"
full$Age.n <- as.factor(full$Age.n)
full$Age.n <- factor(full$Age.n, levels = c("<5", "5-10", "10-20", "20-25", "25-27", "28-29", "30-31", "32-34", "35-40", "40-50", "50+"))


## ----fig.width=9, fig.height=4---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p10 <- ggplot(data = full[!is.na(full$Survived),], aes(as.factor(Age.n), fill = as.factor(Survived))) +
     geom_bar(position = 'fill') +
     labs(x = "Age groups") +
     ggtitle("Survivals by the Age groups") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))

p11 <- ggplot(data = full[!is.na(full$Survived),], aes(as.factor(Age.n), fill = as.factor(Survived))) +
     geom_histogram(stat="count") +
     labs(x = "Age groups") +
     ggtitle("Survivals by the Age groups") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))


grid.arrange(p10,p11, ncol = 2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Kid <- "Adult"
full$Kid[full$Age < 14] <- "Kid"
full$Kid <- as.factor(full$Kid)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = full[!is.na(full$Survived),], aes(Kid, fill = as.factor(Survived))) +
     geom_bar(position = 'fill') +
     labs(x = "Kid or Adult") +
     ggtitle("Survivals by Kid/Adult") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$FamilySize <- full$SibSp + full$Parch
full$FamilySize <- as.factor(full$FamilySize)

full$Family <- ifelse(as.numeric(full$FamilySize) >= 2, "Yes","No") #Family = Yes if passenger has at least one family member aboard
full$Family <- as.factor(full$Family)


## ----fig.width=9, fig.height=4---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p12 <- ggplot(data = full[!is.na(full$Survived),], aes(FamilySize, fill = as.factor(Survived))) +
     geom_bar(position = 'fill') +
     labs(x = "Familty Size") +
     ggtitle("Survivals by Family Size") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))

p13 <- ggplot(data = full[!is.na(full$Survived),], aes(as.factor(Family), fill = as.factor(Survived))) +
     geom_bar(position = 'fill') +
     labs(x = "Family aboard") +
     ggtitle("Survivals by Family Aboard") +
     scale_fill_discrete(name = "Survived",
                         breaks = c(0,1),
                         labels = c("No", "Yes")) +
     theme(legend.position="top", plot.title = element_text(hjust = 0.5))


grid.arrange(p12,p13, ncol = 2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- full[1:891,]
test <- full[892:1309,]
fullIndex <- createDataPartition(train$Survived, p=0.8, list=FALSE) #Split the data 20/80
data_train <- train[ fullIndex,]
data_val <- train[-fullIndex,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(7)
fit.lda <- train(as.factor(Survived)~Pclass+Title+Age+Fare, data = data_train, method="lda", metric=metric, trControl=control)
fit.lda


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(7)
fit.cart <- train(as.factor(Survived)~Pclass+Title+Age+Fare, data = data_train, method="rpart", metric=metric, trControl=control)
fit.cart


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(7)
fit.knn <- train(as.factor(Survived)~Pclass+Title+Age+Fare, data = data_train, method="knn", metric=metric, trControl=control)
fit.knn


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(7)
fit.svm <- train(as.factor(Survived)~Pclass+Title+Age+Fare, data = data_train, method="svmRadial", metric=metric, trControl=control)
fit.svm


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(7)
fit.rf <- train(as.factor(Survived)~Pclass+Title+Age+Fare, data = data_train, method="rf", metric=metric, trControl=control)
fit.rf


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(7)
fit.glm <- train(as.factor(Survived)~Pclass+Title+Age+Fare, data = data_train, method="glm", metric=metric, trControl=control)
fit.glm


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf, glm = fit.glm))
summary(results)
dotplot(results)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(7)
fit.rf1 <- randomForest(as.factor(Survived)~Pclass+Title+Age+Fare+Sex+FamilySize+Ticket.n+Family, data = data_train, ntree = 50)
fit.rf1

(fit.rf1$confusion[1,1]+fit.rf1$confusion[2,2])/714 #Accuracy


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
confusionMatrix(predict(fit.rf, data_val), data_val$Survived)
confusionMatrix(predict(fit.svm, data_val), data_val$Survived)
confusionMatrix(predict(fit.cart, data_val), data_val$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fit.final <- randomForest(as.factor(Survived)~Pclass+Title+Age+Fare+Sex+FamilySize+Ticket.n+Family, data = train, ntree = 50)
predictions <- predict(fit.final, test)
x <- cbind(test$PassengerId, as.data.frame(predictions))
colnames(x) <- c("PassengerID", "Survived")
write.csv(x, "submission.csv", row.names = F)

