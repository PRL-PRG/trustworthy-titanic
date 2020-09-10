
library(ggplot2)


set.seed(1)
train <- read.csv("../input/train.csv", stringsAsFactors=FALSE)
test  <- read.csv("../input/test.csv",  stringsAsFactors=FALSE)
train$Cat <- 'train'
test$Cat <- 'test'
agemodel <- glm(Age ~ Pclass + Fare + Pclass:Fare,data=train)
train$Age <- ifelse(is.na(train$Age),predict(agemodel,train[is.na(train$Age),]),train$Age)
test$Age <- ifelse(is.na(test$Age),predict(agemodel,test[is.na(test$Age),]),test$Age)
train$Fare[is.na(train$Fare)] <- median(train$Fare, na.rm=TRUE)
test$Fare[is.na(test$Fare)] <- median(train$Fare, na.rm=TRUE)
train$Title<-sapply(train$Name,function(x) strsplit(x,'[.,]')[[1]][2])
test$Title<-sapply(test$Name,function(x) strsplit(x,'[.,]')[[1]][2])
test$Survived <- NA
full <- rbind(train,test)
full$Title<-gsub(' ','',full$Title)
full$Title[full$Title %in% c('Capt','Col','Don','Sir','Jonkheer','Major')]<-'Mr'
full$Title[full$Title %in% c('Lady','Ms','theCountess','Mlle','Mme','Ms','Dona')]<-'Miss'
full$Cabin <- substr(full$Cabin,1,1)
full$Cabin      <- as.factor(full$Cabin)
full$Title      <- as.factor(full$Title)
train <- full[full$Cat == 'train', ]
test <- full[full$Cat == 'test', ]



extractFeatures <- function(data) {
  features <- c("Survived",
                "Cabin" ,
                "Title",
                "Pclass",
                "Age",
                "Fare",
                "SibSp",
                "Sex")
  fea <- data[,features]
  
#fea$Embarked[fea$Embarked==""] = "S"
  fea$Sex      <- as.factor(fea$Sex)

#fea$Embarked <- as.factor(fea$Embarked)
  fea$Survived <- as.factor(fea$Survived)
  return(fea)
}
extractFeatures2 <- function(data) {
  features <- c("Cabin" ,
                "Title",
                "Pclass",
                "Age",
                "Fare",
                "SibSp",
                "Sex"
                )
  fea <- data[,features]
#fea$Embarked[fea$Embarked==""] = "S"
  fea$Sex      <- as.factor(fea$Sex)
fea$Cabin      <- as.factor(fea$Cabin)
fea$Title      <- as.factor(fea$Title)
#fea$Embarked <- as.factor(fea$Embarked)
  return(fea)
}
thedata <- extractFeatures(train)
glmout <-  glm(Survived ~ Pclass +Sex +SibSp +Cabin+Title+Fare+ Pclass:Sex + Age,family = binomial(logit), data = thedata)
trainsubtest <- data.frame(PassengerId = train$PassengerId)
trainsubtest$Survived <- predict(glmout, extractFeatures2(train))
botvalue <- 0.35
trainsubtest$Survived <-ifelse(trainsubtest$Survived> botvalue,1,0)
correct <- sum(train$Survived==trainsubtest$Survived)
possible <- length(train$Survived)
print(paste(correct,"of ",possible,"running model on training data or ",100*correct/possible ," with ", botvalue ))
submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(glmout, extractFeatures2(test))
submission$Survived <-ifelse(submission$Survived>botvalue,1,0)
write.csv(submission, file = "1_glm_submission.csv", row.names=FALSE)