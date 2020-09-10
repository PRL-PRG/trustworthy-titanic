## ----echo=FALSE,include=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.train <- read.csv("../input/train.csv")
titanic.test <- read.csv(("../input/test.csv"))


## ----echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test <- titanic.test
test$Survived <- NA
test1 <- test %>% select(PassengerId,
    Survived,
    Pclass:Embarked)
titanic.full <- rbind(titanic.train,test1)
rm(test1)
rm(test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(titanic.full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.full$Fare <- ifelse(is.na(titanic.full$Fare),
                            mean(titanic.full$Fare,na.rm = TRUE),
                            titanic.full$Fare)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.full$Age <- ifelse(
  is.na(titanic.full$Age), 
  median(titanic.full$Age,
  na.rm = TRUE),
  titanic.full$Age
  )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.full$SibSp <- ifelse(titanic.full$SibSp > 0, 1,0)
titanic.full$Parch <- ifelse(titanic.full$Parch > 0, 1,0)
titanic.full$Family <- ifelse((titanic.full$Parch + titanic.full$SibSp) > 0,1,0)
titanic.full <- subset(titanic.full,select = -c(SibSp,Parch,Name))
titanic.full$Cabin <-substr(titanic.full$Cabin,1,1)
titanic.full$Cabin <-ifelse(titanic.full$Cabin =='',"X",titanic.full$Cabin)  # No Cabin (X)
titanic.full$Embarked <-as.factor(ifelse(as.character(titanic.full$Embarked) =="","S",as.character(titanic.full$Embarked)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.train.clean <- titanic.full[1:891,]
titanic.test.clean <- titanic.full[892:1309,]
titanic.test.clean <- titanic.test.clean[,!(names(titanic.test.clean) %in% c("Survived"))]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(prop.table(table(titanic.train.clean$Survived)),2) # 38% servived


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(prop.table(table(titanic.train.clean$Sex)),2)  # 35/65
round(prop.table(table(titanic.train.clean$Survived,titanic.train.clean$Sex)),2)
library(ggplot2)
ggplot(data = titanic.train.clean,aes(x = Age)) +
  geom_histogram(aes(fill = as.factor(Survived)),binwidth = 2) +
  facet_wrap(~ Sex)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(prop.table(table(titanic.train.clean$Pclass)),2)
round(prop.table(table(titanic.train.clean$Survived,titanic.train.clean$Pclass)),2)

ggplot(data = titanic.train.clean,aes(x = Age)) +
  geom_histogram(aes(fill = as.factor(Survived)),binwidth = 2) +
  facet_wrap(~ Pclass)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(prop.table(table(titanic.train.clean$Cabin)),2)
round(prop.table(table(titanic.train.clean$Survived,titanic.train.clean$Cabin)),2)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(prop.table(table(titanic.train.clean$Embarked)),2)
round(prop.table(table(titanic.train.clean$Survived,titanic.train.clean$Embarked)),2)

ggplot(data = titanic.train.clean,aes(x = Age)) +
  geom_histogram(aes(fill = as.factor(Survived)),binwidth = 2) +
  facet_wrap(~ Embarked)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(prop.table(table(titanic.train.clean$Family)),2)
round(prop.table(table(titanic.train.clean$Survived,titanic.train.clean$Family)),2)

ggplot(data = titanic.train.clean,aes(x = Age)) +
  geom_histogram(aes(fill = as.factor(Survived)),binwidth = 2) +
  facet_wrap(~ Family)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2017)
library(rpart)
train.sample <- sample(891,730)
train.set <- titanic.train.clean[train.sample,]
test.set <- titanic.train.clean[-train.sample,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train.model <- rpart(Survived ~ Pclass + Sex + Age + Family + Fare + Embarked +Cabin, 
                       data = train.set, 
                       method = "class",
                       control = rpart.control(minsplit = 50, cp = 0))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(rpart.plot)
rpart.plot(train.model,digits = 3,fallen.leaves = TRUE,type = 3,extra = 101)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#test.set <- subset(test.set,select = c(-Survived))
pred.survived <- predict(train.model, newdata = test.set,type = "class")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(gmodels)
CrossTable(test.set$Survived,
           pred.survived,
           chisq = FALSE,
           prop.c = FALSE,
           prop.r = FALSE,
           dnn = c("Actual Value","Predicted Value"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred <- predict(train.model, newdata = titanic.test.clean, type = "class")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
solution <- data.frame(PassengerId = titanic.test.clean$PassengerId, 
                       Survived = pred)

#write.csv(file = "solution.csv", x= solution)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls())
gc()

