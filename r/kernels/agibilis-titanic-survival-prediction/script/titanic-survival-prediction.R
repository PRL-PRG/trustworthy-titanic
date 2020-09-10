## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# data assessment/visualizations
library(ggplot2)
# data wrangling
library(tidyverse)
# model 
library(mice)
library('randomForest')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Useful data quality function for missing values

checkColumn = function(df,colname){
  
  testData = df[[colname]]
  numMissing = max(sum(is.na(testData)|is.nan(testData)|testData==''),0)

  
  if (class(testData) == 'numeric' | class(testData) == 'Date' | class(testData) == 'difftime' | class(testData) == 'integer'){
    list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = sum(is.infinite(testData)), 'avgVal' = mean(testData,na.rm=TRUE), 'minVal' = round(min(testData,na.rm = TRUE)), 'maxVal' = round(max(testData,na.rm = TRUE)))
  } else{
    list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = NA,  'avgVal' = NA, 'minVal' = NA, 'maxVal' = NA)
  }
  
}
checkAllCols = function(df){
  resDF = data.frame()
  for (colName in names(df)){
    resDF = rbind(resDF,as.data.frame(checkColumn(df=df,colname=colName)))
  }
  resDF
}



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv("../input/train.csv",header = T, stringsAsFactors = F,
                  comment.char = "",na.strings=c("NA","NaN", ""))
#str(train)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test  <- read.csv('../input/test.csv', header = T, stringsAsFactors = F,
                  comment.char = "",na.strings=c("NA","NaN", ""))
#str(test)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$set <- "train"
test$set  <- "test"
test$Survived <- NA
full <- rbind(train, test)
head(full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# dataset dimensions
dim(full)
# Unique values per column
lapply(full, function(x) length(unique(x))) 
head(full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missing_values <- full %>% summarize_all(funs(sum(is.na(.))/n()))
missing_values


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# pivot the rows by feature (colnames) 
missing_values <- gather(missing_values, key="feature", value="missing_pct")
# ploting the missing NA values percentage
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  theme_bw()+coord_flip()+
  scale_size_area()+
  ylab("Missing %")+xlab("Var/Cols")
  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$FamSize <- full$SibSp + full$Parch + 1


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Surname <- sapply(full$Name,  
                      function(x) strsplit(x, split = '[,.]')[[1]][1])




## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Title <- gsub("^.*, (.*?)\\..*$", "\\1", full$Name)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(full$Sex,full$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data=full,aes(y=Age,x=Title, fill=Sex))+
      geom_boxplot(alpha=0.7) +
      #   geom_boxplot(outlier.colour = "red",outlier.shape = 8, outlier.size = 3)+
        theme(axis.text.x = element_text(angle=65, vjust=0.6)) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Engineer features based on all the passengers with the same ticket
ticket.unique <- rep(0, nrow(full))
tickets <- unique(full$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(full$Ticket == current.ticket)
  
  
  for (k in 1:length(party.indexes)) {
    ticket.unique[party.indexes[k]] <- length(party.indexes)
  }
}

full$ticket.unique <- ticket.unique


full$ticket.size[full$ticket.unique == 1]   <- 'Single'
full$ticket.size[full$ticket.unique < 5 & full$ticket.unique>= 2]   <- 'Small'
full$ticket.size[full$ticket.unique >= 5]   <- 'Big'


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full[full$Title %in% 'the Countess',]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full[full$Embarked %in% NA,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data=full[full$Pclass %in% '1',],aes(y=Fare,x=Embarked,fill=Embarked))+
      geom_boxplot(alpha=0.5) +
        theme(axis.text.x = element_text(angle=65, vjust=0.6))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#temporal <- subset(full, Pclass==1 & Fare>78 & Fare <84)
#temporal[order(temporal$Surname),]
#temporal <- subset(full, Ticket=="12749")
#temporal[order(temporal$Surname),]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Embarked <- replace(full$Embarked, which(is.na(full$Embarked)), 'C') 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full[full$Fare %in% NA,]
summary(full[full$Fare %in% NA,])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full[full$Pclass == '3' & full$Embarked == 'S' & full$ticket.size =='Single', ], 
  aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
    colour='red', linetype='dashed', lwd=1) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mean(full[full$Pclass == '3' & full$Embarked == 'S' & full$ticket.size =='Single',]$Fare,na.rm=T,trim=0.2)
full$Fare[1044] <- mean(full[full$Pclass == '3' & full$Embarked == 'S' & full$ticket.size =='Single',]$Fare,na.rm=T,trim=0.2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full <- full %>% mutate( Age = ifelse(is.na(Age), mean(full$Age, na.rm=TRUE), Age),
  `Age Group` = case_when(  Age < 13 ~ "Age.0012", 
                             Age >= 13 & Age < 18 ~ "Age.1317",
                             Age >= 18 & Age < 60 ~ "Age.1859",
                             Age >= 60 ~ "Age.60Ov"))




## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set a random seed
set.seed(1023)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% 
                      c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived','Embarked','ticket.size')],
                 method='rf') 
# Save the complete output 
mice_output <- complete(mice_mod)
head(mice_output)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,1))
# We check the age assignment in the case of the Masters and should be younger than 18
ggplot(mice_output[mice_output$Title %in% c('Master'),], aes(Age, fill = factor(Title))) +   geom_histogram()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod2 <- mice(full[, c('Title','Pclass','Age')],
                 method='rf') 
# Save the complete output 
mice_output2 <- complete(mice_mod2)
head(mice_output2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Let’s compare the results we get with the original distribution of passenger ages to ensure that nothing has gone completely awry.

# Plot age distributions
par(mfrow=c(1,3))
hist(full$Age, freq=F, main='Age: Original Data', 
  col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
  col='lightgreen', ylim=c(0,0.04))
hist(mice_output2$Age, freq=F, main='Age2: MICE Output', 
  col='lightgreen', ylim=c(0,0.04))
  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,2))
# We check the age assignment in the case of the Masters
ggplot(mice_output[mice_output$Title %in% c('Master'),], aes(Age, fill = factor(Title))) +   geom_histogram()
ggplot(mice_output2[mice_output2$Title %in% c('Master'),], aes(Age, fill = factor(Title))) +   geom_histogram()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Replace Age variable from the mice model.
full$Age <- mice_output$Age


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title == 'Lady']          <- 'Miss'
full$Title[full$Title == 'Dona']          <- 'Miss'
full$Title[full$Title == 'Don']   <- 'Mr'
full$Title[full$Title == 'Sir']   <- 'Mr'
full$Title[full$Title == 'the Countess']   <- 'Mrs'
full$Title[full$Title == 'Jonkheer']   <- 'Mr' 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###lets prepare and keep data in the proper format
feauter1<-full[1:891, c("Pclass", "Title","Sex","Embarked","FamSize","ticket.size")]
response <- as.factor(train$Survived)
feauter1$Survived=as.factor(train$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###For Cross validation purpose will keep 20% of data aside from my orginal train set
##This is just to check how well my data works for unseen data
set.seed(500)
ind=caret::createDataPartition(feauter1$Survived,times=1,p=0.8,list=FALSE)
train_val=feauter1[ind,]
test_val=feauter1[-ind,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
####check the proprtion of Survival rate in orginal training data, current traing and testing data
round(prop.table(table(train$Survived)*100),digits = 1)

round(prop.table(table(train_val$Survived)*100),digits = 1)

round(prop.table(table(test_val$Survived)*100),digits = 1)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full.train <- subset(full,set=='train')
full.test <- subset(full,set=='test')
full.train=full.train %>% mutate_if(is.character, as.factor)
full.test=full.test %>% mutate_if(is.character, as.factor)
full.test$Survived <- 0
full.test$Survived <- as.integer(full.test$Survived)
str(full.train)
str(full.test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Title + Pclass + Sex + Age  + FamSize + ticket.size + Fare,
                                            data = full.train)
rf_model


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
varImpPlot(rf_model,main='RandomForestTree')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance),  
    y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
    hjust=0, vjust=0.55, size = 4, colour = 'red')  + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
common <- intersect(names(full.train), names(full.test)) 
for (p in common) { 
  if (class(full.train[[p]]) == "factor") { 
    levels(full.test[[p]]) <- levels(full.train[[p]]) 
  } 
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Predict using the test set
prediction <- predict(rf_model, full.test)
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = full.test$PassengerId, Survived = prediction)
# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

