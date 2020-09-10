## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('../input/train.csv', header = TRUE)
test <- read.csv('../input/test.csv', header = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Looking to see where the NAs are
sapply(train,function(x) sum(is.na(x)))
sapply(test,function(x) sum(is.na(x)))

#For train there are 177 missing values in Age, but Age seems to be important, so I can
#set their age to the average age of the people to "fix" the NA coersion problem
train$Age[is.na(train$Age)] <- mean(train$Age,na.rm=T)

#For test, there are 86 missing age values and 1 missing fare value, which I will
#replace with the average once again
test$Age[is.na(test$Age)] <- mean(test$Age,na.rm=T)
test$Fare[is.na(test$Fare)] <- mean(test$Fare,na.rm=T)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#deleting columns
train$Name <- NULL
test$Name <- NULL
train$Ticket <- NULL
test$Ticket <- NULL
train$Cabin <- NULL
test$Cabin <- NULL

#making variables into numbers
train$Sex <- as.numeric(train$Sex)
test$Sex <- as.numeric(test$Sex)
train$Embarked <- as.numeric(train$Embarked)
test$Embarked <- as.numeric(test$Embarked)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mod <- glm(Survived ~ ., family=binomial, data = train)
step(mod)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
n = nrow(train)
step(mod, k = log(n))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
m1 = glm(Survived ~ Pclass + Sex + Age + SibSp + Embarked, family = binomial, data = train) 
m2 = glm(Survived ~ Pclass + Sex + Age + SibSp, family = binomial, data = train) 

cv.scores = rep(-999, 2)
cv.scores[1] = sum((m1$residuals^2)/((1 - influence(m1)$hat)^2))
cv.scores[2] = sum((m2$residuals^2)/((1 - influence(m2)$hat)^2))
cv.scores
which.min(cv.scores)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
m1 = glm(Survived ~ Pclass + Sex + Age + SibSp + Embarked, family = binomial, data = train)
summary(m1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predictions <- predict(m1,test, type = 'response')
predictions <- ifelse(predictions > 0.5,1,0)
write.csv(predictions, file = 'predictions.csv', row.names = F)

