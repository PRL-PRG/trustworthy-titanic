## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Loading library packages needed
library('randomForest') # classification algorithm


## ---- message = FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Import
titanic.train <- read.csv(file = "../input/train.csv", header = T, stringsAsFactors = F)
titanic.test <- read.csv(file = "../input/test.csv", header = T, stringsAsFactors = F)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# View data in different ways
str(titanic.train)
head(titanic.train)
tail(titanic.train)

str(titanic.test)
head(titanic.test)
tail(titanic.test)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# creating variable 
titanic.test$Survived <- NA

# binding data row wise
titanic.combined <- rbind(titanic.train, titanic.test)

# check whether data ia bined correctly or not
head(titanic.combined)
tail(titanic.combined)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a new variable which contains siblings, spouse and individuals
titanic.combined$Family.size <- titanic.combined$SibSp + titanic.combined$Parch + 1


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check missing values
sapply(titanic.combined, function(x) {sum(is.na(x))})


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# fill blan rows with 'S'

titanic.combined[titanic.combined$Embarked == '', "Embarked"] <- 'S' 


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# modeling for age
age.formula <- "Age ~ Pclass + Sex"
age.model <- lm(
  formula = age.formula,
  data = titanic.combined
)
age.row <- titanic.combined[is.na(titanic.combined$Age), c("Pclass", "Sex")]

# predict age for NA filled rows
age.predict <-predict(age.model, newdata = age.row)

# assign value of age into combined dataset
titanic.combined[is.na(titanic.combined$Age), "Age"] <- age.predict


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# find median of fare
fare.median <- median(titanic.combined$Fare, na.rm = T)

# assign value to blank rows
titanic.combined[is.na(titanic.combined$Fare), "Fare"] <- fare.median


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.combined$Pclass <- as.factor(titanic.combined$Pclass)
titanic.combined$Sex <- as.factor(titanic.combined$Sex)
titanic.combined$Embarked <- as.factor(titanic.combined$Embarked)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# split dataset
titanic.train <- titanic.combined[1:891,]
titanic.test <- titanic.combined[892:1309,]


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# To get Survived data in levels
titanic.train$Survived <- as.factor(titanic.train$Survived)

# Set a random seed
set.seed(675)

# Modeling data 
survived.formula <- as.formula("Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family.size")
survive.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test),  keep.forest = TRUE)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Prediction 
Survived <- predict(survive.model, newdata = titanic.test)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# new data set with two variables
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived
table(output.df$Survived)
# write csv file

write.csv(output.df, file = "titanic_submission.csv", row.names = FALSE)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# error visuals
plot(survive.model, ylim=c(0,0.40))
legend('topright', colnames(survive.model$err.rate), col=1:3, fill=1:3)

