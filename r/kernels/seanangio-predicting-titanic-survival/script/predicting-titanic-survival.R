## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# load required libraries
library(caret)
library(randomForest)
library(plyr)
library(dplyr)
library(ggplot2)
library(mice)
library(gridExtra)
library(stringr)
library(knitr)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# load data
training <- read.table("../input/train.csv", sep = ",", header = TRUE,
                       na.strings = c("", "NA"))
testing <- read.table("../input/test.csv", sep = ",", header = TRUE,
                         na.strings = c("", "NA"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# examine structure of dataset
str(training)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# convert factor variables in training set
training$Survived <- factor(training$Survived)
training$Pclass <- factor(training$Pclass)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# plot survival by sex
mosaicplot(table(training$Survived, training$Sex))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# prop table of survival by sex
round(prop.table(table(training[,c("Survived", "Sex")])),2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# plot survival by Pclass
mosaicplot(table(training$Survived, training$Pclass))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# prop table of survival by Pclass
round(prop.table(table(training[,c("Survived", "Pclass")])),2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# plot survival by embarked
mosaicplot(table(training$Survived, training$Embarked))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# prop table of survival by embarked
round(table(training[,c("Pclass", "Embarked")]),2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# impute C to missing embarked values
training$Embarked[is.na(training$Embarked)] <- "C"


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# extract titles and compare survival status
training$Title <- factor(str_extract(training$Name, "\\w+(?=\\.)"))
cbind(table(training$Title, training$Survived))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# first create the new level
levels(training$Title) <- c(levels(training$Title), "HonM")

# define reassignments
single_ladies <- c("Mlle","Lady","Ms")
hon_male <- c("Sir","Major","Col","Jonkheer","Capt","Don","Col")
mrs <- c("Mme", "Countess", "Dona")

# relabel factors
training$Title[training$Title %in% single_ladies] <- "Miss"
training$Title[training$Title %in% mrs] <- "Mrs"
training$Title[training$Title %in% hon_male] <- "HonM"

# drop unused levels
training$Title <- droplevels(training$Title)

# confirm results
cbind(table(training$Title, training$Survived))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Pclass distribution where Cabin is not missing
training %>%
    filter(Cabin != "") %>%
    group_by(Pclass) %>%
    dplyr::summarize(n = n())


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create a variable based on if passenger assigned a cabin
training$BerthAssgn <- factor(ifelse(is.na(training$Cabin), 0, 1))
prop.table(table(training$BerthAssgn, training$Survived))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create Deck variable
training$Deck <- substr(training$Cabin, 1,1)
training$Deck[is.na(training$Deck)] <- "Missing"
training$Deck <- factor(training$Deck)
table(training$Deck, training$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# mosaic plot of surival by sibsp and parch
par(mfrow = c(1,2))
mosaicplot(table(training$Survived, training$SibSp))
mosaicplot(table(training$Survived, training$Parch))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create a FamilySize variable
training$FamilySize <- training$SibSp + training$Parch


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# plot count of survivors and victims by FamilySize
ggplot(training, aes(x = FamilySize, fill = Survived)) +
    geom_histogram(position = "dodge")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create FamCat variable
training$FamCat <- NA
training$FamCat[training$FamilySize == 0] <- "solo"
training$FamCat[training$FamilySize == 1] <- "pair"
training$FamCat[training$FamilySize == 2] <- "trio"
training$FamCat[training$FamilySize == 3] <- "quartet"
training$FamCat[training$FamilySize >= 4] <- "large"
training$FamCat <- factor(training$FamCat, levels = c("solo","pair","trio","quartet","large"), ordered = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# plot count of survivors and victims by FamCat
ggplot(training, aes(x = FamCat, fill = Survived)) +
    geom_bar(position = "dodge")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Surival plotted by fare
ggplot(training, aes(y = Fare, x = Survived)) + 
    geom_boxplot() +
    coord_flip()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# summary stats of survival by fare
summary(subset(training, 
               training$Survived == 0 & training$Fare < 300)$Fare)
summary(subset(training, 
               training$Survived == 1 & training$Fare < 300)$Fare)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# fare plotted by surivors excluding 3 first class outliers
ggplot(filter(training, Fare < 400), 
       aes(x = Fare, color = Survived)) +
    geom_density() +
    geom_vline(xintercept = 13)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# fare plotted by class excluding 3 first class outliers
ggplot(filter(training, Fare < 400), aes(x = Fare, color = Pclass)) +
    geom_density() +
    geom_vline(xintercept = 13)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create CheapTix variable
training$CheapTix <- factor(ifelse(training$Fare <= 13, 1, 0))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create 4 bins for Fare
training$FareBin <- cut(training$Fare, breaks = 4, 
                        ordered_result = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 15 cheapest fares
training %>%
    arrange(Fare) %>%
    select(Survived, Pclass, Sex, Fare) %>%
    head(15)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# change fares of 0 to 0.01
training$Fare[training$Fare == 0] <- 0.01

# log transform Fare
training$FareLog <- log(training$Fare)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Survival plotted by age and class, faceted by sex
ggplot(training, aes(x = Age, y = Pclass, color = Survived)) + 
    geom_jitter(height = 0.1, alpha = 0.3) +
    facet_grid(Sex ~ .)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# survival rate with missing Ages removed
training %>%
    filter(!is.na(Age)) %>%
    group_by(Survived) %>%
    dplyr::summarize(n = n(),
              survival = n()/(424+290))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# survival rate across training data
training %>%
    group_by(Survived) %>%
    dplyr::summarize(n = n(),
              survival = n/nrow(training))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# impute missing Age values
set.seed(417)
tempData <- mice(training[,!names(training) %in% c("PassengerId", "Name", "Ticket", "Cabin")], method = "pmm")
mice_output <- complete(tempData)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# compare missing and imputed age histograms
p1 <- ggplot(training, aes(x = Age)) + geom_histogram()
p2 <- ggplot(mice_output, aes(x = Age)) + geom_histogram()
grid.arrange(p1, p2, ncol = 2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# compare pre and post age distributions
summary(training$Age)
summary(mice_output$Age)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# replace age variable with mice output
training$Age <- mice_output$Age
sum(is.na(training$Age))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# density curves of surivors and victims by age
ggplot(training, aes(x=Age, color = Survived)) + 
    geom_density() +
    geom_vline(xintercept = 14)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create Child variable
training$Child <- factor(ifelse(training$Age <= 14, 1, 0))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# bin Ages in a new variable
training$AgeBin <- cut(training$Age, breaks = 5, 
                       ordered_result = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# drop unuseful predictors
drops <- c("PassengerId","Ticket", "Cabin", "Name")
training <- training[ , !(names(training) %in% drops)]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# check for near zero variance predictors
nearZero <- nearZeroVar(training, saveMetrics = TRUE)
nearZero[nearZero$nzv == TRUE,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# distribution of FareBin
table(training$FareBin)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# remove FareBin
training <- select(training, -FareBin)


## ----results="hide"--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# stepwise backward elimination
fit.back <- step(glm(Survived ~ ., data = training, family = "binomial"), direction = 'backward')

# stepwise forward elimination
null <- glm(Survived ~ 1, data = training, family = "binomial")
full <- glm(Survived ~ ., data = training, family = "binomial")
fit.forward <- step(null, scope = list(lower=null, upper=full), direction = "forward")

# stepwise both direction
fit.both <- step(glm(Survived ~ ., data = training, family = "binomial"), direction = 'both')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# call of backward, forward and both step models
summary(fit.back)$call
summary(fit.both)$call
summary(fit.forward)$call


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
form1 <- formula(Survived ~ Pclass + Age + Title + FamCat + FareLog)
form2 <- formula(Survived ~ Pclass + Age + Title + FamCat + FareLog + BerthAssgn)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(417)
# 60/40 split of training data
intrain <- createDataPartition(y = training$Survived, p = 0.6, 
                               list = FALSE)
train <- training[intrain,]
test <- training[-intrain,]

# confirm dimensions
dim(train); dim(test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# survivor proportions in train and test sets
round(prop.table(table(train$Survived)),3)
round(prop.table(table(test$Survived)),3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# set cv for all models
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

# vector of algorithms we want to try
methods <- c("glm", "glmnet", "rpart", "ranger", "svmRadial", "knn")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# initialize empty lists of model fits, predictions, and accuracies
fits <- vector("list")
pred <- vector("list")
acc <- vector("list")

# fit a series of models with different methods
for (m in methods) {
    set.seed(417)
    fits[[m]] <- train(form1, data = train, method = m, 
               trControl = ctrl, tuneLength = 5)
}

# generate predictions and accuracy for each model fits
for (mod in names(fits)) {
    pred[[mod]] <- predict(fits[[mod]], test)
    acc[[mod]] <- mean(test$Survived == pred[[mod]])
}

# save accuracies and predictions in dfs
acc.df1 <- as.data.frame(acc)
pred.df1 <- as.data.frame(pred)
acc.df1


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# initialize empty lists of model fits, predictions, and accuracies
fits <- vector("list")
pred <- vector("list")
acc <- vector("list")

# fit a series of models with different methods
for (m in methods) {
    set.seed(417)
    fits[[m]] <- train(form2, data = train, method = m, 
               trControl = ctrl, tuneLength = 5)
}

# generate predictions and accuracy for each model fits
for (mod in names(fits)) {
    pred[[mod]] <- predict(fits[[mod]], test)
    acc[[mod]] <- mean(test$Survived == pred[[mod]])
}

# save accuracies and predictions in dfs
acc.df2 <- as.data.frame(acc)
pred.df2 <- as.data.frame(pred)
acc.df2


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# combine accuracy results
acc.all <- rbind(acc.df1, acc.df2)
acc.all


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# convert factor variables in testing set
testing$Pclass <- factor(testing$Pclass)

# extract titles 
testing$Title <- factor(str_extract(testing$Name, "\\w+(?=\\.)"))
levels(testing$Title) <- c(levels(testing$Title), "HonM")
# define reassignments
single_ladies <- c("Mlle","Lady","Ms")
hon_male <- c("Sir","Major","Col","Jonkheer","Capt","Don","Col")
mrs <- c("Mme", "Countess", "Dona")
# relabel factors
testing$Title[testing$Title %in% single_ladies] <- "Miss"
testing$Title[testing$Title %in% mrs] <- "Mrs"
testing$Title[testing$Title %in% hon_male] <- "HonM"

# drop unused levels
testing$Title <- droplevels(testing$Title)

# create a variable based on if passenger assigned a cabin
testing$BerthAssgn <- factor(ifelse(is.na(testing$Cabin), 0, 1))

# create Deck variable
testing$Deck <- substr(testing$Cabin, 1,1)
testing$Deck[is.na(testing$Deck)] <- "Missing"
testing$Deck <- factor(testing$Deck)

# create a FamilySize variable
testing$FamilySize <- testing$SibSp + testing$Parch

# create FamCat variable
testing$FamCat <- NA
testing$FamCat[testing$FamilySize == 0] <- "solo"
testing$FamCat[testing$FamilySize == 1] <- "pair"
testing$FamCat[testing$FamilySize == 2] <- "trio"
testing$FamCat[testing$FamilySize == 3] <- "quartet"
testing$FamCat[testing$FamilySize >= 4] <- "large"
testing$FamCat <- factor(testing$FamCat, levels = c("solo","pair","trio","quartet","large"), ordered = TRUE)

# impute 1 missing Fare value with median
testing$Fare[is.na(testing$Fare)] <- mean(testing$Fare, na.rm = TRUE)

# create CheapTix variable
testing$CheapTix <- factor(ifelse(testing$Fare <= 13, 1, 0))

# change fares of 0 to 0.01
testing$Fare[testing$Fare == 0] <- 0.01

# log transform Fare
testing$FareLog <- log(testing$Fare)

# impute missing Age values
set.seed(417)
tempData <- mice(testing[,!names(testing) %in% c("PassengerId", "Name", "Ticket", "Cabin")], method = "pmm")
mice_output <- complete(tempData)

# replace age variable with mice output
testing$Age <- mice_output$Age

# create Child variable
testing$Child <- factor(ifelse(testing$Age <= 14, 1, 0))

# bin Ages in a new variable
testing$AgeBin <- cut(testing$Age, breaks = 5, 
                       ordered_result = TRUE)
# drop unuseful predictors
drops <- c("Ticket", "Cabin", "Name")
testing <- testing[ , !(names(testing) %in% drops)]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
form2


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(417)
fit.best <- train(form2, data = train, method = "ranger", 
               trControl = ctrl, tuneLength = 5)

# Make predictions on the test dataset
pred.best <- predict(fit.best, test)

# Examine the confusion matrix
table(test$Survived, pred.best)

# Compute the accuracy on the test dataset
mean(test$Survived == pred.best)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(417)
# generate predictions on testing dataset
testing$Survived <- predict(fit.best, testing)

# keep requested columns
submission <- testing[,c("PassengerId", "Survived")]

# Write the solution to file
write.csv(submission, file = 'submission.csv', row.names = FALSE)

