
library(tidyverse) # metapackage with lots of helpful functions
library(randomForest)
list.files(path = "../input")


t1 <- read.csv("../input/train.csv",stringsAsFactors = F)
t2 <- read.csv("../input/test.csv",stringsAsFactors = F)
all <- bind_rows(t1,t2)

colSums(is.na(all))
all$Fare[all$Fare==0] <- NA
all$Fare <- as.numeric(all$Fare)
all$Embarked[all$Embarked==''] <- 'S'
# Change Embarked.
all$Embarked[all$Embarked=='C'] <- 0
all$Embarked[all$Embarked=='Q'] <- 1
all$Embarked[all$Embarked=='S'] <- 2
all$Embarked <- as.numeric(all$Embarked)
# Make sex numeric
all$Sex[all$Sex=='female'] <- 1
all$Sex[all$Sex=='male'] <- 0
all$Sex <- as.numeric(all$Sex)
str(all)

cor(all[c('Fare','Age','SibSp','Parch','Pclass','Sex','Embarked','Survived')],use = 'c')

not.missing <- which(!is.na(all[,c('Fare','Age')]))
missing.fare <- which(is.na(all[,c('Fare')]))
missing.fare

model <- glm(Fare ~ Pclass+ Embarked, data = all[not.missing,])
pred <- predict(model, all[missing.fare,])
data.frame(pred)
all$Fare[missing.fare] <- pred

not.missing <- which(!is.na(all$Age))
missing <- which(is.na(all$Age))
model <- glm(Age ~  Pclass+ Embarked, data = all[not.missing,])
pred <- predict(model, all[missing,])
data.frame(pred=head(pred))
all$Age[missing] <- pred

train.val <-as.integer(891*0.70)
test.val <- 891-train.val
# make all$Survived factor
all$Survived <- as.factor(all$Survived)

train <- all[1:train.val,]
test <- all[624:891,]

model <- randomForest(Survived ~ Sex+ Pclass+ Fare+ Embarked, data =train)
pred <- predict(model, test)
# data.frame(actual=test$Survived[1:100], pred=pred)
table(pred=pred,actual=test$Survived)
mean(pred==test$Survived)

model

options(repr.plot.width=7, repr.plot.height=5)
varImpPlot(model, sort = T)

pred <- predict(model,all[892:1309,])
submit <- data.frame(PassengerId=892:1309,Survived=pred)
write.csv(submit, file = 'RF_submission.csv', row.names = F)


