## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(caret)
allData <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
str(allData)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(allData$Name)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(is.na(allData$Age))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(allData$Cabin=="")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
preProcess <- function(x) {
        temp <- strsplit(as.character(x$Name), ", ")
        #Title
        title <- NULL
        for (i in 1:length(temp)) {
                temp2 <- strsplit(temp[[i]][2], " ")
                temp3 <- temp2[[1]][1]
                title <- c(title, temp3)
        }
        
        wom <- c("Mme.|Ms.")
        title <- gsub(wom, "Mrs.", title)
        title <- gsub("Mlle.","Miss.", title)
        highGrade <- c("Master.|Don.|Rev.|Dr.|Major.|Lady.|Sir.|Col.|Capt.|
                       Jonkheer.|the")
        title <- gsub(highGrade, "Noble.", title)
        
        x <- cbind(x, title)
        
        #Age
        index <- is.na(x$Age)
        x$Age[index] <- median(x$Age, na.rm=TRUE)
        
        #Cabin
        x <- select(x, -c(Cabin, Ticket, Name))
        
        #classes
        x$title <- as.character(x$title)
        x
}

allData <- preProcess(allData)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(12345)
inTrain <- createDataPartition(allData$PassengerId, p=.7, list=FALSE)
training <- allData[inTrain,]
testing <- allData[-inTrain,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
training$Survived <- as.factor(training$Survived)
test1 <- select(testing, -Survived)
#random forest model
control <- trainControl(method="cv", number=3, verboseIter=FALSE)
fit1 <- train(Survived~., data=training, method="rf", trControl=control)
pred1 <- predict(fit1, test1)
accu1 <- confusionMatrix(testing$Survived, pred1)
accu1$overall


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
input <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
input <- preProcess(input)
#repairing a missing title and a missing fare
input$title[415] <- "Mrs."
input$Fare[153] <- median(input$Fare, na.rm=TRUE)
output <- predict(fit1, input)

#Write the solution to a file
solution <- data.frame(PassengerId=input$PassengerId, Survived=output)
write.csv(solution, file="solution.csv", row.names=FALSE)

