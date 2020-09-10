## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 10, fig.align = "center")


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)            # For data frame manipulation and the %>%  pipe
library(ggplot2)          # For visualizations
library(gridExtra)        # For plotting multiple plots
library(mice)             # multivariate imputation by chained equations
library(randomForest)     # random forest model


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

train <- read.csv("../input/train.csv")   # For initial EDA and training of the predictive model



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(train, give.attr = FALSE)


## ---- message = TRUE, fig.align = "center"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# setting standard colors for consistency in plots
group_colors <- c("0" = "tomato", "1" = "limegreen", "male" = "skyblue", "female" = "pink")

train$Survived <- factor(train$Survived)
ggplot (train, aes(x = Survived)) + 
  geom_bar(fill = c("0" = "tomato", "1" = "limegreen")) +
  labs(title = "Survival on the Titanic", x = "Survival", y = "Number of Passengers")



## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = Sex)) + 
  geom_bar(aes(fill = Survived), position = "fill") +
  scale_fill_manual(values = group_colors) +
  labs(title = "Survival by Sex", x = "Sex", y = "Proportion of Passengers")



## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = Age)) + 
  geom_histogram(aes(fill = Sex), binwidth = 2) +
  scale_fill_manual(values = group_colors) +
  labs(title = "Distribution of Passenger Age by Sex", x = "Age", y = "Number of Passengers")



## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot (train, aes(x = Age)) + 
  geom_histogram(aes(fill = Survived), binwidth = 2) +
  scale_fill_manual(values = group_colors) +
  labs(title = "Distribution of Passenger Age by Survival", x = "Age", y = "Number of Passengers")



## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot (train, aes(x = Pclass)) + 
  geom_bar(aes(fill = Survived)) +
  scale_fill_manual(values = group_colors) +
  labs(title = "Distribution of Passenger Class by Survival", x = "Passenger Class", y = "Number of Passengers")



## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot (train, aes(x = Fare)) + 
  geom_histogram(aes(fill = Survived), binwidth = 10) +
  scale_fill_manual(values = group_colors) +
  labs(title = "Distribution of Journey Fare by Survival", x = "Fare Paid", y = "Number of Passengers")



## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot (train, aes(x = Embarked)) + 
  geom_bar(aes(fill = Survived)) +
  scale_fill_manual(values = group_colors) +
  labs(title = "Distribution of Journey Origin by Survival", x = "Origin (port of embarkment)", y = "Number of Passengers")



## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test <- read.csv("../input/test.csv")   # For running the predictive model
test$Survived <- NA                 # Adding the missing varriable to the test set
combined <- bind_rows(train, test)  # For performing feature engineering on the entire data set


## ---- message = TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(combined, give.attr = FALSE)


## ---- message = TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sapply(combined, function(x) sum(is.na(x)))



## ---- message = TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

combined$Survived <- factor(combined$Survived)
combined$Pclass <- factor(combined$Pclass)
combined$Sex <- factor(combined$Sex)
combined$Embarked <- factor(combined$Embarked)
str(combined, give.attr = FALSE)



## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

set.seed(1234)    # set seed for reproduceible results

imputes <- mice(combined[c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")], method = "rf") # imputed using random forest methods
imputes_output <- complete(imputes)


## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
impute_age <- ggplot(imputes_output, aes(x = Age)) + 
  geom_histogram(binwidth = 2, fill = "thistle") +
  labs(x = "Imputed Age")
age <- ggplot(train, aes(x = Age)) + geom_histogram(binwidth = 2)
grid.arrange(age, impute_age, ncol = 2)


## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
impute_fare <- ggplot(imputes_output, aes(x = Fare)) + 
  geom_histogram(binwidth = 10, fill = "thistle") +
  labs(x = "Imputed Fare Paid")
fare <- ggplot(train, aes(x = Fare)) + geom_histogram(binwidth = 10) + labs(x = "Fare Paid")
grid.arrange(fare, impute_fare, ncol = 2)


## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
impute_embarked <- ggplot(imputes_output, aes(x = Embarked)) + 
  geom_bar(fill = "thistle") +
  labs(x = "Imputed Origin")
embarked <- ggplot(train, aes(x = Embarked)) + geom_bar() + labs(x = "Origin")
grid.arrange(embarked, impute_embarked, ncol = 2)


## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

combined$Age <- imputes_output$Age
combined$Fare <- imputes_output$Fare
combined$Embarked <- imputes_output$Embarked



## ---- message = TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sapply(combined, function(x) sum(is.na(x)))



## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

combined$Title <- factor(gsub('(.*, )|(\\..*)', '', combined$Name))
table(combined$Title)



## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

combined$FamSize <- combined$SibSp + combined$Parch + 1 

ggplot(combined, aes(x = FamSize)) + 
  geom_bar() +
  labs(x = "Family Size", y = "Number of Passengers", title = "Family Size of Passengers")



## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

combined$child <- NA
combined$child[combined$Age <= 16] <- TRUE
combined$child[combined$Age > 16] <- FALSE

str(combined, give.attr = FALSE)



## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

train <- combined[1:891,]
test <- combined[892:1309,]



## ---- message = TRUE, warning = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

rf_titanic <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamSize + child, data = train, ntree = 1000)



## ---- message = TRUE, warning = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

rf_titanic
plot(rf_titanic)


## ---- message = TRUE, warning = FALSE, fig.width= 10, fig.height=10--------------------------------------------------------------------------------------------------------------------------------------------------------------

vimp <- importance(rf_titanic)
vimp_df <- data.frame(Var = row.names(vimp), vimp)
vimp_df %>% arrange(desc(MeanDecreaseGini))



## ---- message = TRUE, warning = FALSE, fig.width= 10, fig.height=10--------------------------------------------------------------------------------------------------------------------------------------------------------------

predicted <- predict(rf_titanic, newdata = test)

solution <- data.frame(PassengerID = test$PassengerId, Survived = predicted)

write.csv(solution, "solution.csv", row.names = FALSE)



## ---- message = TRUE, warning = FALSE, fig.width= 10, fig.height=10--------------------------------------------------------------------------------------------------------------------------------------------------------------

set.seed(1234)    # set seed for reproduceible results

combined2 <- combined

imputetest <- mice(combined2[c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")], method = "rf") # imputed using random forest methods
imputetest_output <- complete(imputetest)

combined2$Survived <- imputetest_output$Survived

solution2 <- data.frame(PassengerId = combined2$PassengerId, Survived = combined2$Survived)

write.csv(solution2[892:1309,], "solution2.csv", row.names = FALSE)


