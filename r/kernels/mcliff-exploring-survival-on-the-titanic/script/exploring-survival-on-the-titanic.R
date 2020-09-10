## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('randomForest') # classification algorithm
library('forcats')
library('caret')
library('dummies')

getMissingRows <- function(total_data,column.with.gaps){
    missingrows <- integer(0)
    count <- 0
    for (i in 1:nrow(total_data)){
        if(is.na(total_data[i,column.with.gaps])){
            count <- count+1
            missingrows[count] <- i
            
        }
    }
    print(paste(column.with.gaps,"has",count,"record(s) with a missing value"))
    return(missingrows)
}


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
raw_train <- read.csv('../input/train.csv')
raw_test  <- read.csv('../input/test.csv')

# will need this to write output
testPassengerId <- raw_test$PassengerId

combine  <- bind_rows(raw_train, raw_test) # bind training & test data
summary(combine)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
names(combine)
combine <- select(combine, -Name, -Ticket) %>% 
    mutate(Pclass = factor(Pclass), 
           Embarked = factor(Embarked),
           Sex = fct_recode(Sex, "0" = "male", "1" = "female"),
           family = SibSp + Parch
    )
names(combine)


## ----exploreCabin----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combine$Cabin[is.na(combine$Cabin)] <- "X"
combine$Cabin <- substring(combine$Cabin, 1, 1)
combine$Cabin <- as.factor(combine$Cabin)

# look at things there we have survival info and cabin info
ggplot(combine[(!is.na(combine$Survived) & combine$Cabin != "X"),], aes(x=Cabin, fill=Survived)) +
    geom_bar(stat="count") +
    labs(title="Survival rate by Cabin assignment")


# delete cabin now
combine <- select(combine, -Cabin) 


## ----gplotAgePre-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(combine[(!is.na(combine$Survived) & !is.na(combine$Age)),], aes(x=Age, fill=Survived)) +
    geom_density(alpha=0.5, aes(fill=factor(Survived))) +
    labs(title="Survival/Age Density plot") +
    scale_x_continuous(breaks=scales::pretty_breaks(n=10))


## ----cleanAge--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ageMissingRows <- getMissingRows(combine, "Age")
idx_na <- is.na(combine$Age)
age_train <- combine[!idx_na, ]
age_test <- combine[idx_na, ]
cvidx <- rep(1:5, each = ceiling(nrow(age_train)/5))
cvidx <- sample(cvidx, nrow(age_train))
norm01 <- function(x) x/max(x, na.rm=TRUE)

age_train <- select(age_train, PassengerId, Age, Survived, Pclass, Sex, SibSp, Parch, Fare, Embarked) %>%
    mutate(cvidx=cvidx, SibSp = norm01(SibSp), Parch = norm01(Parch), Fare = norm01(Fare))

age_test <- select(age_test, PassengerId, Age, Survived, Pclass, Sex, SibSp, Parch, Fare, Embarked) %>%
    mutate(SibSp = norm01(SibSp), Parch = norm01(Parch), Fare = norm01(Fare))

modelAgePSS <- lm(Age ~ Pclass + SibSp + Fare, data = age_train )
age_test$Age <- predict(modelAgePSS, newdata = age_test)
combine[combine$PassengerId %in% age_test$PassengerId, "Age"] <- age_test$Age
ageMissingRows <- getMissingRows(combine, "Age")


## ----gplotAge--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(combine[(!is.na(combine$Survived) & !is.na(combine$Age)),], aes(x=Age, fill=Survived)) +
    geom_density(alpha=0.5, aes(fill=factor(Survived))) +
    labs(title="Survival/Age Density plot") +
    scale_x_continuous(breaks=scales::pretty_breaks(n=10))


## ----cleanFare-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
names(combine)
FareMissingRows <- getMissingRows(combine, "Fare")
idx_na <- is.na(combine$Fare)
fare_train <- combine[!idx_na, ]
fare_test <- combine[idx_na, ]
fare_train <- select(fare_train, PassengerId, Age, Survived, Pclass, Sex, SibSp, Parch, Fare, Embarked) %>%
    mutate(SibSp = norm01(SibSp), Parch = norm01(Parch))


fare_test <- select(fare_test, PassengerId, Age, Survived, Pclass, Sex, SibSp, Parch, Fare, Embarked)

#modelFarePSS <- lm(Fare ~ Pclass + SibSp + Age + Sex + Embarked, data = fare_train )
modelFarePSS <- lm(Fare ~ Age, data = fare_train )
fare_test$Fare <- predict(modelFarePSS, newdata = fare_test)
combine[combine$PassengerId %in% fare_test$PassengerId, "Fare"] <- fare_test$Fare

FareMissingRows <- getMissingRows(combine, "Fare")


## ----cleanFinal------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combine <- dummy.data.frame(combine)
combine <- select(combine, -PassengerId, -SibSp, -Parch) %>%
    mutate(Survived = factor(Survived))
names(combine)

combine <- as.data.frame(combine)
idx_na <- is.na(combine$Survived)
train <- combine[!idx_na, ]
test <- combine[idx_na, ]
summary(train)
summary(test)


## ----modelBuild------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
threshold <- 0.6
pcaTransform <- preProcess(train[-1], method=c("center", "scale", "pca"), thresh = threshold)

# translate the training and test data to the pca-coordinates to train run the model
pc_train <- predict(pcaTransform, train[,-1])
pc_test <- predict(pcaTransform, test[,-1])

# train the model
pc_train$Survived <- train$Survived
model <- train(Survived ~ ., data = pc_train)


## ----modelPredict----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred_train <- predict(model, newdata = pc_train[,-7])
confusionMatrix(pred_train, train$Survived)


pred_test <- predict(model, newdata = pc_test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = testPassengerId, Survived = pred_test)

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

