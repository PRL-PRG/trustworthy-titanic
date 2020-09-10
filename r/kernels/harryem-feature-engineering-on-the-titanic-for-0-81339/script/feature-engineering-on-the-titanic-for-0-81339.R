## ----message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Loading libraries
library(ggplot2) #charting
library(plyr) #data wrangling
library(dplyr) #data wrangling
library(Hmisc) #data wrangling
library(mice) #imputing variables
library(randomForest) #modelling
library(caret) #modelling


traindata <- read.csv('../input/train.csv', stringsAsFactors = F)
testdata <- read.csv('../input/test.csv', stringsAsFactors = F)

c(object.size(traindata),object.size(testdata)) ## data is small so performance shouldn't be an issue

## I want to combine training and test sets because this makes it easier to perform the same 
## operations on both sets

testdata$Survived <- "NA"
merged <- rbind(traindata,testdata)

length(unique(merged$PassengerId)) == length(merged$PassengerId) ## check no duped entries


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(merged)
colSums(is.na(merged))
colSums(merged=="")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
merged$Pclass <- as.factor(merged$Pclass)
merged$Sex <- as.factor(merged$Sex)

g <- ggplot(merged[1:891,], aes(x=Pclass,fill=factor(Survived))) + geom_bar() + labs(fill = "Survived")
g <- g + labs(title="Survivor split by ticket class")
g

g <- g + facet_wrap(~Sex) + labs(title="Survivor split by ticket class and gender")
g


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
qplot(merged$Age)

agebrackets <- c(0,13,18,30,55)
merged$Agebracket <- findInterval(merged$Age,agebrackets)

agetable <- data.frame(Agebracket=c(1,2,3,4,5),Age_range=c("<13","13-17","18-29","30-54","55+"))
merged <- join(merged,agetable,by="Agebracket")
merged$Agebracket <- as.factor(merged$Agebracket)

g <- ggplot(merged[1:891,], aes(x=Age_range,fill=factor(Survived))) + geom_bar() + labs(fill = "Survived")
g <- g + labs(title="Survivor split by age group")
g

g <- g + facet_wrap(~Sex) + labs(title="Survivor split by age and gender")
g


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(merged$Cabin,30)
length(unique(merged$Cabin))/length(merged$Cabin) ## only 14% are unique so there are a lot shared.
merged$Cabin[28] # this looks strange, multiple cabins on one ticket
subset(merged,Cabin == "C23 C25 C27") # it was one family, the Fortunes

merged$HasCabin <- as.factor(!(merged$Cabin==""))

g <- ggplot(merged[1:891,], aes(x=HasCabin,fill=factor(Survived))) + geom_bar()
g <- g +facet_wrap(~Pclass) + labs(title="Survivor split by class and Cabin")
g


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
qplot(merged$Fare,bins=150)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
subset(merged,is.na(merged$Fare))
merged[1044,]$Fare <- mean(subset(merged,Pclass==3)$Fare,na.rm=TRUE)

merged$Farebracket <- as.factor(cut2(merged$Fare,g=5))

g <- ggplot(merged[1:891,], aes(x=Farebracket,fill=factor(Survived))) + geom_bar()
g <- g +facet_wrap(~Pclass) + labs(title="Survivor split by class and Fare Bracket")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g 

subset(merged,Fare==0)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(order(merged$Fare,decreasing = TRUE))
merged[259,]

subset(merged,Fare==merged$Fare[259])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
grep("Dawson",merged$Name)
merged[grep("Rose",merged$Name),]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
merged$Title <- gsub('(.*, )|(\\..*)', '', merged$Name)

count(merged,Title)

VIP <- c("Capt","Col","Don","Dona","Dr","Jonkheer","Lady","Major",
         "Mlle", "Mme","Rev","Sir","the Countess")

merged$Title[merged$Title %in% VIP] <- "VIP"
merged$Title <- as.factor(merged$Title)

count(merged,Title)

## I'm not that keen on only having 2 in the "Ms" camp
merged[merged$Title=="Ms",]$Title <- "Mrs"

g <- ggplot(merged[1:891,], aes(x=Title,fill=factor(Survived))) + geom_bar()
g <- g +facet_wrap(~Pclass) + labs(title="Survivor split by class and Title")
g


## ----message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(wru) # who r u - library to guess race from surname

merged$surname<- gsub("([A-Za-z]+).*", "\\1", merged$Name)

raceprobs <- predict_race(merged,surname.only = TRUE)
racepreds <- suppressWarnings(colnames(raceprobs)[apply(raceprobs,1,which.max)])
merged$Race <- as.factor(sub('.*\\.', '',racepreds))

g <- ggplot(merged[1:891,], aes(x=Race,fill=factor(Survived))) + geom_bar()
g <- g +facet_wrap(~Pclass) + labs(title="Survivor split by class and Race")
g


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
merged <- ddply(merged,.(Ticket),transform,Ticketsize=length(Ticket))
merged$Ticketsize <- as.factor(merged$Ticketsize)
merged <- merged[order(merged$PassengerId),] # ddply mixes up order


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
count(merged,Embarked)
subset(merged,Embarked == "")
merged[c(62,830),"Embarked"] <- "S"
merged$Embarked <- as.factor(merged$Embarked)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
factors <- c("Pclass","Sex","Agebracket","Title")
set.seed(1234)

m1 <- merged[, !names(merged) %in% c("Agebracket","Age_range")]
mice_ages <- mice(m1[, !names(m1) %in% factors], method='rf')
mice_out <- complete(mice_ages)

# I am creating a new variable for the imputed ages because I want to later compare to 
# training a model both for entries with and without ages.


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mergedages <- merged[,!names(merged) == "Age_range"]
mergedages$Age <- mice_out$Age

mergedages$Agebracket <- findInterval(mergedages$Age,agebrackets)
mergedages <- join(mergedages,agetable,by="Agebracket")
mergedages$Agebracket <- as.factor(mergedages$Agebracket)

colSums(is.na(mergedages))
colSums(mergedages=="")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mergedagestrain <- mergedages[1:891,]
mergedagestest <- mergedages[892:1309,]
mergedagestrain$Survived <- as.factor(traindata$Survived)

set.seed(414)
inTrain<- createDataPartition(y=mergedagestrain$Survived,p=0.75, list=FALSE)
train <- mergedagestrain[inTrain,]
test <- mergedagestrain[-inTrain,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(414)
logreg <- glm(Survived ~ Pclass + Sex + Agebracket +
                      Farebracket + HasCabin + Ticketsize
               + Title + Embarked, family = binomial(link=logit), 
              data = train)

summary(logreg)

prediction <- predict(logreg,newdata=test,type="response")
prediction <- ifelse(prediction > 0.5,1,0)
misClasificError <- mean(prediction != test$Survived)
print(paste('Accuracy',1-misClasificError)) ## Accuracy 81%


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(414)
library(glmnet)
x <- model.matrix(Survived ~ Pclass + Sex + Farebracket + HasCabin + Ticketsize + Embarked + Title,train)
cv.out <- cv.glmnet(x,y=train$Survived,alpha=1,family="binomial",type.measure = "mse") #select lambda -4

#best value of lambda
lambda_1se <- cv.out$lambda.1se

xtest <- model.matrix(Survived ~ Pclass + Sex + Farebracket + HasCabin + Ticketsize + Embarked + Title,test)
lasso_prob <- predict(cv.out,newx = xtest,s=lambda_1se,type="response")
#translate probabilities to predictions

lasso_predict <- rep("0",nrow(test))
lasso_predict[lasso_prob>.5] <-"1"
#confusion matrix
table(pred=lasso_predict,true=test$Survived)

mean(lasso_predict==test$Survived) #82% accuracy


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(414)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Agebracket +
                                 Farebracket + Race + HasCabin + Ticketsize +
                                 Embarked + Title,
                         data = mergedagestrain,na.action = na.pass)

rf_model
rf_model$confusion
varImpPlot(rf_model)
importance(rf_model)

set.seed(414)
rf_model2 <- randomForest(factor(Survived) ~ Pclass + Sex + Farebracket + HasCabin + Ticketsize + Embarked + Title,
                         data = mergedagestrain,na.action = na.pass,nodesize=20)

rf_model2 #looks the best
plot(rf_model2)
varImpPlot(rf_model2)
importance(rf_model2)


## ----message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
features <- c("Survived","Pclass","Sex","SibSp","Parch","HasCabin","Farebracket","Title","Ticketsize","Age_range")
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)

set.seed(414)
gbm1 <- suppressWarnings(train(as.factor(Survived) ~ ., data = train[features], 
              method = "gbm", trControl = fitControl,verbose = FALSE))
prediction <- predict(gbm1, test[features],type= "prob")
prediction <- data.frame(ifelse(prediction[,2] > 0.5,1,0))
mean(prediction[,1] == test$Survived) #83% accuracy

set.seed(414)
fitControl2 <- trainControl(method = "repeatedcv", number = 6, repeats = 4)
gbm2 <- suppressWarnings(train(as.factor(Survived) ~ ., data = train[features], 
              method = "gbm", trControl = fitControl,verbose = FALSE))
prediction <- predict(gbm2, test[features],type= "prob")
prediction <- data.frame(ifelse(prediction[,2] > 0.5,1,0))
mean(prediction[,1] == test$Survived) #83% accuracy


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
row.names(merged) <- merged$PassengerId
mergedtrain <- merged[1:891,]
mergedtest <- merged[892:1309,]
mergedtrain$Survived <- as.factor(mergedtrain$Survived)

sum(is.na(mergedtrain$Age))
sum(is.na(mergedtest$Age))

rf_model_ages <- randomForest(factor(Survived) ~ Pclass + Sex + Farebracket + HasCabin + Agebracket + Ticketsize + Embarked + Title,
                              data = mergedtrain[!is.na(mergedtrain$Age),],nodesize=20)

rf_model_ages
rf_model_ages$confusion
varImpPlot(rf_model_ages)
importance(rf_model_ages)


rf_model_noages <- randomForest(factor(Survived) ~ Pclass + Sex +
                                      Farebracket + Race + HasCabin + Ticketsize +
                                      Embarked + Title,
                              data = mergedtrain[is.na(mergedtrain$Age),],nodesize=20)

rf_model_noages
rf_model_noages$confusion
varImpPlot(rf_model_noages)
importance(rf_model_noages)

p1 <- predict(rf_model_ages, mergedtest[!is.na(mergedtest$Age),])
p2 <- predict(rf_model_noages, mergedtest[is.na(mergedtest$Age),])
prediction <- join(data.frame(PassengerId=names(p1),Survived=p1),data.frame(PassengerId=names(p2),Survived=p2))

# This actually didn't help my position on the leaderboard, pretty surprised by this


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prediction <- predict(rf_model2, mergedagestest)
submission <- data.frame(PassengerId=names(prediction),Survived=prediction)
if(!file.exists("./predictions.csv")) {
        write.csv(submission, file = "./predictions.csv",row.names = F)}

