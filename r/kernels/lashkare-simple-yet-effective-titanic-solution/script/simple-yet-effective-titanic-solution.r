
#remove everything in the working environment.
rm(list =ls())
cat("\14") #clears console


#To install packages simply type install.packages("packageName")
#library loads installed packages  
library(ggplot2)  #Data Visualisations
library(randomForest)

#set working directory 
#setwd("D:\\Projects\\Titanic") #specify path where you have your data

# read.csv reads a file in table format and creates a data frame from it 
# the default for stringsAsFactors is true, as we do not wish to convert character vectors to factors we make it false 

train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)  
test <- read.csv("../input/test.csv",stringsAsFactors = FALSE)

#view data 
head(train) 
head(test)

#adding survived variable to test set 
test$Survived <- NA
#combining both test and train dataframe by rows
fullset <- rbind(train,test)

#get the basic structure and description of fullset  
str(fullset) 

summary(fullset) 

fullset$Pclass <- as.factor(fullset$Pclass) #converting to factor
 #Visualizing Pclass 
ggplot(fullset[1:891,], aes(x = Pclass, fill = factor(Survived))) +   
  geom_bar() +  
  ggtitle("Impact of Class on Survival")

fullset$Sex <- as.factor(fullset$Sex) 
summary(fullset$Sex)
 
ggplot(fullset[1:891,], aes(x = Sex, fill = factor(Survived))) +  
 geom_bar() + 
 ggtitle("Do females have higher survival rate?")


ggplot(fullset[1:891,], aes(Sex)) +
  facet_wrap(~Pclass) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(Survived)), stat= "count")+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = -.25) +
  ggtitle("Class") + labs(y = "percent")

head(fullset$Name)

#creating new title variable from Name  
fullset$Title <- sapply(fullset$Name,FUN = function(x){strsplit(x,"[,.]")[[1]][2]}) 
fullset$Title <- sub(' ', '', fullset$Title) 
fullset$Title <- as.factor(fullset$Title) 

summary(fullset$Title)

table(fullset$Sex,fullset$Title)

# There are many titles having less frequency and they can be grouped.
fullset$Title <- as.character(fullset$Title) 
fullset$Title[fullset$Title %in% c("Mlle","Ms")] <- "Miss" 
fullset$Title[fullset$Title == "Mme"] <- "Mrs" 
fullset$Title[fullset$Title %in% c( "Don", "Sir", "Jonkheer","Rev","Dr")] <- "Sir" 
fullset$Title[fullset$Title %in% c("Dona", "Lady", "the Countess")] <- "Lady" 
fullset$Title[fullset$Title %in% c("Capt","Col", "Major")] <- "Officer" 
fullset$Title <- as.factor(fullset$Title) 

summary(fullset$Title) 

ggplot(fullset[1:891,], aes(x = Age)) +  
 geom_histogram(aes(y = ..density.., color = Title, fill = Title), alpha = 0.4, position = "identity") + 
  geom_density(aes(color = Title), size =1)

fullset$FamSize <- fullset$SibSp + fullset$Parch + 1

ggplot(fullset[1:891,], aes(x = FamSize, fill = factor(Survived))) +
 geom_bar(stat='count', position='dodge') +
 scale_x_continuous(breaks=c(1:11)) +
 labs(x = 'Family Size')+ ggtitle("Family")

fullset$FamGroup[fullset$FamSize == 1] <- 'Individual' 
fullset$FamGroup[fullset$FamSize < 5 & fullset$FamSize > 1] <- 'small' 
fullset$FamGroup[fullset$FamSize > 4] <- 'large' 
fullset$FamGroup <- as.factor(fullset$FamGroup)

sum(is.na(fullset$Fare))

which(is.na(fullset$Fare))

#Imputing median Fare value of Pclass = 3 and Emabrked = S 
fullset$Fare[1044] <- median(fullset[fullset$Pclass == '3' & fullset$Embarked == 'S', ]$Fare, na.rm = TRUE)

n <- data.frame(table(fullset$Ticket))
fullset <- merge(fullset,n, by.x="Ticket", by.y="Var1", x.all=T) # Assign the frequency of each ticket appearance
fullset$Fare2 <- fullset$Fare / fullset$Freq

fullset <- fullset[order(fullset$PassengerId),]

which(fullset$Cabin == '')

which(fullset$Embarked == '')

fullset[c(62,830),]

fullset$Embarked <- as.factor(fullset$Embarked) 
fullset[fullset$Fare2 >= 39 & fullset$Fare2 <= 41 & fullset$Pclass == 1,]

summary(fullset[fullset$Fare2 >= 39 & fullset$Fare2 <= 41 & fullset$Pclass == 1,"Embarked"])

fullset$Embarked <- as.character(fullset$Embarked) 
fullset$Embarked[fullset$Embarked %in% c("","")] <- "C" 
fullset$Embarked <- as.factor(fullset$Embarked)

sum(is.na(fullset$Age)) 

title.age <- aggregate(fullset$Age,by = list(fullset$Title), FUN = function(x) median(x, na.rm = T)) 
fullset[is.na(fullset$Age), "Age"] <- apply(fullset[is.na(fullset$Age), ] , 1, function(x) title.age[title.age[, 1]==x["Title"], 2])

sum(is.na(fullset$Age))

ggplot(fullset[1:891,], aes(Age, fill = factor(Survived))) +  
 facet_grid(.~Sex) + 
 geom_dotplot(binwidth = 2)

fullset$Title <- as.character(fullset$Title) 
fullset[fullset$Sex == "female" & fullset$Age < 18,"Title"] <- "Miss2" 
fullset$Title <- as.factor(fullset$Title)

summary(fullset$Title)

fullset$isMinor[fullset$Age < 18] <- 'Minor' 
fullset$isMinor[fullset$Age >= 18] <- 'Adult' 
fullset$isMinor <- as.factor(fullset$isMinor)
fullset$Survived <- as.factor(fullset$Survived) 

#We will again split the data to train and test set
train <- fullset[1:891,] 
test <- fullset[892:1309,]

set.seed(786) # to get a reproducible random result.
model <- randomForest(factor(Survived) ~ Pclass + Fare + Title + Embarked + FamGroup + Sex + isMinor,
 data = train, importance = TRUE, ntree = 1000,mtry=2)
#ntree is number of trees to grow. 
#mtry is number of variables randomly sampled as candidates at each split.
#importance: importance of predictors

model

varImpPlot(model)

importance(model)

#predicting on test data
prediction <- predict(model,test) 
#creating a submission data frame of required format
submission <- data.frame(PassengerID = test$PassengerId, Survived = prediction) 
#write a csv output file
write.csv(submission, file = 'Submission.csv', row.names = F)
