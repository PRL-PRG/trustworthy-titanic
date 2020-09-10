library(randomForest)
library(rpart)

train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

extract_title <- function(name) {
  title <- strsplit(as.character(name), split='[,.]')[[1]][2]
  gsub(' ', '', title)
}

extract_surname <- function(name) {
  strsplit(as.character(name), split=',')[[1]][1]
}

clean_ages <- function(age) {
  age = round(age)
  if (age < 1) {
    age = 1
  }
  age
}

get_mode <- function(column) {
  tmp <- table(as.vector(column))
  mode <- names(tmp)[tmp == max(tmp)]
}

fill_age <- function(all) {
  #ctl = rpart.control(minsplit=30) 
  #tree <- rpart(formula=Age ~ Title + Pclass + Fare + FamilySize + SibSp + Parch,
   #             data=all[!is.na(all$Age),],
    #            method="anova")
  fit <- lm(Age ~ FamilySize + SibSp + Parch + Fare + Sex + Pclass,
            data=all[!is.na(all$Age),])
  
  predictions <-sapply(predict(fit, all[is.na(all$Age),]), FUN=clean_ages)
  all$Age[is.na(all$Age)] <- predictions
  all
}

#Merge the two datasets to make feature engineering easier.
#If done separately there would be differing levels of some factors,
#which would then need to be unified later.
test$Survived <- NA
merged <- rbind(train, test)

merged$Title <- sapply(merged$Name, FUN=extract_title)
merged$FamilySize <- merged$SibSp + merged$Parch + 1

#Combine some titles that mean the same things in different languages, or imply the same status
merged$Title[merged$Title == 'Mlle'] <- 'Miss'
merged$Title[merged$Title %in% c('Mme', 'Dona')] <- 'Mrs'
merged$Title[merged$Title %in% c('Don', 'Jonkheer')] <- 'Sir'


merged$Title = factor(merged$Title)

#Things like dr, master have some importance so they could reflect on survival

#Fill out missing values
#Use a decision tree to predict missing ages 
#and the mode for others as there aren't as many missing

merged <- fill_age(merged)
merged$Fare[which(is.na(merged$Fare))] <- median(merged$Fare, na.rm=TRUE)
merged$Embarked[which(is.na(merged$Embarked))] <- get_mode(merged$Embarked)

#Mother and child variables
merged$Child <- 0
merged$Child[merged$Age < 18] <- 1
merged$Mother <- 0
merged$Mother[merged$Sex == 'female' & merged$Age >= 18 & merged$Parch > 0 & merged$Title != 'Miss'] <- 1

print(colnames(merged))
train_len <- length(train$PassengerId)
train <- merged[1:train_len,]
test <- merged[(train_len + 1):length(merged$PassengerId),]
forest <- randomForest(as.factor(Survived) ~ Title + Pclass + Sex + Age + Fare + FamilySize + Embarked + Parch + SibSp + Child + Mother,
                       data=train,
                       importance=TRUE,
                       ntree=3000)


predictions <- predict(forest, test)

varImpPlot(forest)
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = predictions)
write.csv(my_solution, file="my_solution.csv", row.names = FALSE)