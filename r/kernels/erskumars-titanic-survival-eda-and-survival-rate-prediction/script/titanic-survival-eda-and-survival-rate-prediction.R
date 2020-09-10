# import the required packages
library(dplyr)
library(ggplot2)

library(rpart) # for regression trees
library(randomForest) # for random forests

########## Read the training and test data
train <- read.csv("../input/train.csv", stringsAsFactors=FALSE)
#View(train)
summary(train)
nrow(train)

test <- read.csv("../input/test.csv", stringsAsFactors=FALSE)
#View(test)
summary(test)
nrow(test)

#### Check for missing values
train %>%
  summarise_all(funs(sum(is.na(.))))

test %>%
  summarise_all(funs(sum(is.na(.))))

### Check for blank values
sapply(train, function(x) length(which(x == "")))
## There are 687 blank values in Cabin and 2 blank values in Embarked

sapply(test, function(x) length(which(x == "")))

sum(is.na(train$Age))
sum(is.na(test$Age))


train$Age[which(is.na(train$Age))] <- median(train$Age, na.rm = T)
test$Age[which(is.na(test$Age))] <- median(test$Age, na.rm = T)

# There are no Duplicate entries
sum(duplicated(train$PassengerId))
sum(duplicated(test$PassengerId))

# Age is having missing values , there are 20% missing values for Age
# Clean the Age column, it contains decimal values

train$Age <- gsub('.00', '', train$Age)
train$Survived <- factor(train$Survived)
train$Sex <- factor(train$Sex)
train$SibSp <- factor(train$SibSp)
train$Pclass <- factor(train$Pclass)
train$Embarked <- factor(train$Embarked)


train$Age_Category <- sapply(train$Age, function(x){
  if(x >= 0 & x <= 12){
    return ("Child")
  } else if(x > 12 & x <=24){
    return ("Very Young")
  } else if(x > 24 & x <= 36){
    return("Young")
  } else if(x > 36 & x <=48){
    return ("Middle Aged")
  } else if(x > 48){
    return("Old")
  } else {
    return("NA")
  }
})


######
test$Age <- gsub('.00', '', test$Age)
test$Sex <- factor(test$Sex)
test$SibSp <- factor(test$SibSp)
test$Pclass <- factor(test$Pclass)
test$Embarked <- factor(test$Embarked)

test$Age_Category <- sapply(test$Age, function(x){
  if(x >= 0 & x <= 12){
    return ("Child")
  } else if(x > 12 & x <=24){
    return ("Very Young")
  } else if(x > 24 & x <= 36){
    return("Young")
  } else if(x > 36 & x <=48){
    return ("Middle Aged")
  } else if(x > 48){
    return("Old")
  } else {
    return("NA")
  }
})

#######


ncol(train)

# Univariate Analysis

summary(train$Fare)

ggplot(train, aes(y=train$Fare, x = train$Pclass)) + geom_boxplot(fill = "steelblue") + 
  labs(x = "Class", y = "Fare", title = "Class wise Fare distribution")

# Average Fare is higher for class 1 and then class 2 and class 3 has the least fare

#Checking what percentage of Survivals are defaulted 
prop.table(table(train$Survived)) # About 38% Survived

ggplot(train, aes(x= train$Survived)) + geom_bar(fill = "orange") +
  labs(x = "Survived", y = "Passenger Count") + scale_y_continuous("Survived", breaks = seq(0, 500, by = 50)) +
  geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                label = paste0(prop.table(..count..) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.9), 
            size = 4, vjust = -0.9)

ggplot(train, aes(x = train$Sex, fill = train$Survived)) + geom_bar(position = "fill")
#Females survived more than men.

ggplot(train, aes(x= train$Pclass, fill = train$Survived)) + geom_bar(position = "fill")
#Class 1 passenger survived more


train$Age <- as.numeric(train$Age)
summary(train$Age)

data <- subset(train, train$Survived == 1)

ggplot(data, aes(x = data$Sex, fill = data$Pclass)) + geom_bar(position = "dodge")



ggplot(train, aes(x = train$Age_Category, fill = train$Survived)) + geom_bar(position = "fill")

Adults <- subset(train, Age > 12)
ggplot(Adults, aes(x = Adults$Sex, fill = Adults$Survived)) + geom_bar(position = "fill")
# Women survived more than Men

str(train$SibSp)
ggplot(train, aes(x = train$SibSp, fill = train$Survived)) + geom_bar(position = "fill")

ggplot(train, aes(x = factor(train$Parch), fill = train$Survived)) + geom_bar(position = "fill")


#Features of Interest
# Age Category
# Sex
# PClass
#SibSp

############## Machine learning Part #####################

fit <- rpart(Survived ~ Age_Category + Sex + Pclass + SibSp, data = train)
#fit <- randomForest(Survived ~ Age_Category + Sex + Pclass + SibSp, data = train)

#fit <- rpart(Survived ~ Age + Sex + Pclass + SibSp, data = train)

print(predict(fit, head(train)))
print(head(train$Survived))

val <- as.data.frame(predict(fit, test))

colnames(val) <- c("NotSurvived", "Survived")

val$score <- ifelse(val$NotSurvived > val$Survived, 0, 1)

new <- data.frame(PassengerId = test$PassengerId, Survived = val$score) 

write.csv(new, "predict_survival.csv", row.names = F)




########################################
#View(test)

nrow(new)
