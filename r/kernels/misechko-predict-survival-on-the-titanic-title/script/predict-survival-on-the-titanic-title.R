## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##load packages
packages <- c("plyr", "ggplot2", "caret", "Hmisc", "mice", "stringr", "gridExtra", "tabplot", "kernlab")
packages <- lapply(packages, FUN = function(x) {
     if (!require(x, character.only = TRUE)) {
          install.packages(x)
          suppressPackageStartupMessages(library(x, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE))
     }
})


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trn <- read.table("../input/train.csv", header = TRUE, sep = ",", dec = ".")
tst <- read.table("../input/test.csv", header = TRUE, sep = ",", dec = ".")
tst$Survived <- 0
full <- rbind(trn, tst)
str(full)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Survived <- as.factor(full$Survived)
full$Survived <- revalue(full$Survived, c("1"="Survived", "0"="Deceased"))
full$Pclass <- as.factor(full$Pclass)
full$Pclass <- revalue(full$Pclass, c("1"="1-st", "2"="2-nd", "3"="3-rd"))
full$Embarked[c(62, 830)] <- "S"
full$Embarked <- factor(full$Embarked)
full$Fare[c(1044)] <- median(full$Fare, na.rm = TRUE)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Check for missing value in Age
summary(full$Age)
#Make imputation with 'mice' package, method - Classification and regression trees
tt <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Survived')], m = 1,meth='cart', maxit = 1,seed=500)

imp <- complete(tt)
gc()

#Check for results of imputation
summary(imp$Age)
#Accept resulting dataframe with imputed data.
full$Age <- imp$Age


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$family_size <- full$SibSp + full$Parch + 1
full$IsAlone <- 0
full$IsAlone[!is.na(str_extract(full$family_size, "1"))] <- "Yes"
full$IsAlone[is.na(str_extract(full$family_size, "1"))] <- "No"
full$IsAlone[!is.na(str_extract(full$family_size, "11"))] <- "No"
full$IsAlone <- as.factor(full$IsAlone)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$title <- 0
full$title[!is.na(str_extract(full$Name, "Col"))] <- "Mr"
full$title[!is.na(str_extract(full$Name, "Dr"))] <- "Mr"
full$title[!is.na(str_extract(full$Name, "Don"))] <- "Mr"
full$title[!is.na(str_extract(full$Name, "Mr"))] <- "Mr"
full$title[!is.na(str_extract(full$Name, "Mrs"))] <- "Mrs"
full$title[!is.na(str_extract(full$Name, "Mme"))] <- "Mrs"
full$title[!is.na(str_extract(full$Name, "Miss"))] <- "Miss"
full$title[!is.na(str_extract(full$Name, "Ms"))] <- "Miss"
full$title[!is.na(str_extract(full$Name, "Mlle"))] <- "Miss"
full$title[!is.na(str_extract(full$Name, "Capt"))] <- "Mr"
full$title[!is.na(str_extract(full$Name, "Major"))] <- "Mr"
full$title[!is.na(str_extract(full$Name, "Master"))] <- "Master"
full$title[!is.na(str_extract(full$Name, "Rev"))] <- "Mr"
full$title[!is.na(str_extract(full$Name, "Countess"))] <- "Mrs"
full$title[!is.na(str_extract(full$Name, "Jonkheer"))] <- "Mr"
full$title[!is.na(str_extract(full$Name, "Dona"))] <- "Mrs"
full$title[full$PassengerId == 797] <- 'Mrs'
full$title[full$PassengerId == 545] <- 'Mr'
full$title <- as.factor(full$title)

# Show title counts by sex
table(full$title, full$Sex)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Remove columns with many factors
full <- full[,-c(4,9,11)]
# Split the data back into train and test set
train <- full[1:891,]
test <- full[892:1309,]
test$Survived <- NULL


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tabplot)
tableplot(train[,-1])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p1 <- ggplot(train, aes(Pclass, fill = Survived)) + geom_bar()+
         labs(title = "Passanger class by Survived", x = "Passanger class", y = "Count of Passanger")
p2 <- ggplot(train, aes(Sex, fill = Survived)) + geom_bar()+
         labs(title = "Passanger class by Survived", x = "Sex", y = "Count of Passanger")
p3 <- ggplot(train, aes(title, fill = Survived)) + geom_bar()+
         labs(title = "Passanger Title by Survived", x = "Title", y = "Count of Passanger")
p4 <- ggplot(train, aes(IsAlone, fill = Survived)) + geom_bar()+
         labs(title = "Family vs Alone by Survived", x = "Is alone", y = "Count of Passanger")
grid.arrange(p1, p2, p3, p4, nrow=2)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
svmRadial <- train(Survived ~ ., train, seed=7, preProcess=c("center", "scale", "nzv"), metric = "Accuracy", method = "svmRadial", trControl =trainControl(method="repeatedcv", number=10, repeats=10))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
svmRadial$results


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prediction <- predict(svmRadial, test)
my_solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
my_solution$Survived <- revalue(my_solution$Survived, c("Survived"="1", "Deceased"="0"))
write.csv(my_solution, file = "my_svmRadial.csv" , row.names = F)

