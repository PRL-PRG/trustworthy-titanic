# rm(list=ls(all=TRUE))

library(tidyverse)
library(dplyr)

# Read data

train <- read.table("../input/train.csv", header = TRUE, sep=",")
test<- read.table("../input/test.csv", header = TRUE, sep=",")

str(train)

summary(train)

# writing to file

i<-1
write_to_file <- function(pred_vec, type)
{
submit1 <- data.frame(test2$PassengerId, pred_vec)
colnames(submit1)[1]<-"PassengerId"
submit1$Survived <- submit1$pred_vec
str(submit1)

table(submit1$Survived)

submit1$pred_vec <- NULL

str(submit1)
filename <- paste("submission_",type,i,".csv",sep="")

write.csv(submit1, filename, row.names = FALSE)
i<-i+1
}

# Pre-processing

sum(is.na(train))

missing <- colSums(is.na(train))
sort(missing, decreasing=TRUE)

library(mice)

md.pattern(train)

library(VIM)

mice_plot <- aggr(train, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(train), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))


# Less than 2% missing values

library(DMwR)
train2<- centralImputation(train) #Central Imputation

# train2 <- knnImputation(train,scale=T,k=10) #KNN Imputation
# sum(is.na(train2))

sum(is.na(train2))

# train2 <- na.omit(train)
# str(train2)
# sum(is.na(train2))



#Looking at data

str(train2)

train3 <- train2

# train3$Pclass <- as.factor(train3$Pclass)

# str(train3)

train3$Survived <- as.factor(train3$Survived)
str(train3)

hist(train3$Age)
hist(train3$SibSp)
hist(train3$Parch)
hist(train3$Fare)

# cor(train3$Pclass, train3$Fare)

train4 <- train3[,c("Pclass","Sex","Age","SibSp", "Survived", "Parch", "Fare", "Embarked")]

set.seed(123)
library(caTools)
spl <- sample.split(train4, 0.9)
train5 <- train4[spl==TRUE,]
valid <- train4[spl==FALSE,]

# Predictions on test data

# Removing null values

sum(is.na(test))

library(DMwR)
test2<- centralImputation(test) #Cenral Imputation

test3 <- test2[,c("Pclass","Sex","Age","SibSp", "Parch", "Fare", "Embarked")]

# CART

# using data train 5


library(rpart)
model_cart <- rpart(Survived~., data=train5, method = "class", control = rpart.control(cp=0.019, minbucket = 5))
summary(model_cart)

library(rpart.plot)
prp(model_cart,varlen=10)
plotcp(model_cart)

library(caret)

pred_train <- predict(model_cart, train5, type='class')
confusionMatrix(pred_train, train5$Survived)

pred_valid <- predict(model_cart, newdata=valid, type = 'class')
confusionMatrix(pred_valid, valid$Survived)

pred_cart <- predict(model_cart, newdata=test3, type = 'class')

# writing to file

write_to_file(pred_cart, "CART_new")

# Random Forest

library(randomForest)
model_random <- randomForest(Survived~., data = train5, importance = TRUE, ntree = 55)
summary(model_random)
plot(model_random)

importance(model_random)

library(caret)

pred_rf <- predict(model_random, train5, type = 'class')
confusionMatrix(pred_rf, train5$Survived)

pred_valid_rf <- predict(model_random, valid)
confusionMatrix(pred_valid_rf, valid$Survived)

levels(test3$Embarked) <- levels(train5$Embarked)

pred_rft <- predict(model_random, test3)

# Writing to file

write_to_file(pred_rft,"RandomF")