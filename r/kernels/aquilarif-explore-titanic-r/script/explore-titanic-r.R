# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(plyr)
library(dplyr)
library('randomForest')
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

list.files("../input")

train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

full  <- bind_rows(train, test) # test + train
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
officer <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
royalty <- c('Dona', 'Lady', 'the Countess','Sir', 'Jonkheer')

# Reassign mlle, ms, and mme, and rare
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% royalty]  <- 'Royalty'
full$Title[full$Title %in% officer]  <- 'Officer'
full$Embarked[c(62, 830)] <- 'S'
full$Fsize <- full$SibSp + full$Parch + 1
full$Fare[1044] <- median(full[full$Pclass == '3', ]$Fare, na.rm = TRUE)
full$FarePP <- full$Fare/full$Fsize
fareClassAvg <-aggregate(x=full$FarePP,by=list(full$Pclass,full$Embarked), FUN=mean)

a<-merge(full,fareClassAvg, by.x=c("Pclass","Embarked"), by.y=c("Group.1","Group.2"),all.x=TRUE)
full$meanFareForClass <- a$x


full$FareClass[full$FarePP>=full$meanFareForClass]<-'High'
full$FareClass[full$FarePP<full$meanFareForClass]<-'Low'

full$Deck <- lapply(full$Cabin, function(x) {
		if(!is.na(x)&&x!="") {
  			if(length(grep(" ", x))>0) { 
				return(substr(x, 1, 1))
  			 } else {
      			return(substr(x, 1, 1))
			}
		} else {
               return("X")
		}
})
title.age <- aggregate(full$Age,by = list(full$Title), FUN = function(x) median(x, na.rm = T))

full[is.na(full$Age), "Age"] <- apply(full[is.na(full$Age), ] , 1, function(x) title.age[title.age[, 1]==x["Title"], 2])

full$AgeClass <- cut(full$Age, 10, labels=1:10, ordered_result=TRUE)
full$AgeCat <- lapply(full$Age, function(x) {
        if(x<10) return ("Child")
        else if(x>60) return ("Senior")
        else if((x>10)&&(x<30)) return ("Young")
        else return("Adult")
})

full$Deck <- unlist(full$Deck)
full$Pclass  <- factor(full$Pclass)
full$Sex  <- factor(full$Sex)
full$Embarked  <- factor(full$Embarked)
full$Title  <- factor(full$Title)
full$Fsize  <- factor(full$Fsize)
full$Deck <- factor(full$Deck) 
full$FareClass <- factor(full$FareClass) 
full$AgeCat <- unlist(full$AgeCat)
full$AgeCat <- factor(full$AgeCat) 
full$FareClass <- factor(full$AgeClass) 


a <- full[,-9]
final_mod <- a[,-10]
train <- final_mod[1:891,]
test <- final_mod[892:1309,]
#summary(train$Deck)
set.seed(100)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + FareClass + Embarked + Title + Fare + Age +
                   Fsize + AgeCat, data = train)
# prediction
rf.fitted = predict(rf_model)
ans_rf = rep(NA,891)
for(i in 1:891){
  ans_rf[i] = as.integer(rf.fitted[[i]]) - 1
}

table(ans_rf)

print(rf_model)
mean(ans_rf == train$Survived)
varImpPlot(rf_model, main = "RF_MODEL")
prediction <- predict(rf_model, test)

# Solution 2 columns (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# .csv
write.csv(solution, file = 'rf_model_sol.csv', row.names = F)

