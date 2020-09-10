## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# load packages and data
library(rpart)
library(rpart.plot)
library(lme4)
library(magrittr)

setwd("../input") # for loading data in Kaggle
data <- read.csv("train.csv")

# make output more clear later
levels(data$Embarked) <- c("", "Cherbourg", "Queenstown", "Southampton")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Index data for training/validation split
n <- nrow(data)
set.seed(10)
idx.train <- sample(1:n, n * 0.8)
idx.val <- sample((1:n)[-idx.train])


## ----1---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tree_cols <- c(3, 5, 7, 8, 12)
x_train <- data[idx.train, tree_cols]
x_val <- data[idx.val, tree_cols]
y_train <- data[idx.train, 2]
y_val <- data[idx.val, 2]

x <- cbind(x_train, y_train)

# grow tree 
fit <- rpart(y_train ~ ., data = x, method="class")

# summarize and plot tree
print(fit)
prp(fit, extra = 4)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predicted_dt <- predict(fit, x_val)
predicted_dt <- ifelse(predicted_dt[,1] <= 0.5, 1, 0) # reformat to binary

# calculate accuracy of prediction
acc_dt <- sum(predicted_dt == y_val, na.rm = 1) / length(y_val)
cat(paste0("Validation accuracy for the decision tree is ",
           round(acc_dt,3), 
           "."))


## ----2a--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# fit logistic regression model
fit_lr <- glm(y_train ~ ., data = x, family = binomial(link = "logit"))
summary(fit_lr)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predicted_lr = predict(fit_lr, x_val)
predicted_lr <- ifelse(predicted_lr >= 0, 1, 0) # reformat to binary

# calculate accuracy of prediction
acc_lr <- sum(predicted_lr == y_val, na.rm = 1) / length(y_val)
cat(paste0("Validation accuracy for the category-only logistic regression is ",
           round(acc_lr,3), 
           "."))


## ----2b--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Logistic regression w. all data incl. continuous ----------------------------
x_train_all <- data[idx.train, c(3, 5:8, 10, 12)] # still omits names and tickets
x_val_all <- data[idx.val, c(3, 5:8, 10, 12)]

# complete gaps in  data (could remove rows with NA, but decided to complete w. dummy data)
# function to replace NAs with column mean
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
NA2mean_df <- function(df){
  nums <- unlist(lapply(df, is.numeric)) # index numeric columns
  done <- replace(df, nums, lapply(df[, nums], NA2mean))
  return(done)
}

# function to fill empty character cells with most frequent factor
fill_facs <- function(df) {
  for (i in 1:ncol(df)) {
    levels(df[, i])[levels(df[, i]) == ""] <-
      names(which.max(table(df[, i])))
  }
  return(df)
}

# replace NAs with column mean and replace empty cells with most frequent factor
x_train_all %<>% NA2mean_df() %>% fill_facs()
x_val_all %<>% NA2mean_df() %>% fill_facs()

# paste together
x_all <- cbind(x_train_all, y_train)

# fit model
fit_lr_all <- glm(y_train ~ ., data = x_all, family = binomial(link = "logit"))
summary(fit_lr_all)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

predicted_lr_all = predict(fit_lr_all, x_val_all) 
predicted_lr_all <- ifelse(predicted_lr_all >= 0, 1, 0) # reformat to binary

# calculate accuracy of prediction
acc_lr_all <- sum(predicted_lr_all == y_val, na.rm = 1) / length(y_val)
cat(paste0("Validation accuracy for the full logistic regression is ",
           round(acc_lr_all,3), 
           "."))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# fit model with interactions
fit_lr_fact <-
  glm(
  y_train ~ . + (Sex * Pclass) + (Sex * Embarked) + (Pclass * Embarked),
  data = x_all,
  family = binomial(link = "logit")
  )

summary(fit_lr_fact)

predicted_lr_fact = predict(fit_lr_fact, x_val_all) 
predicted_lr_fact <- ifelse(predicted_lr_fact >= 0, 1, 0) # reformat to binary

# calculate accuracy of prediction
acc_lr_fact <- sum(predicted_lr_fact == y_val, na.rm = 1) / length(y_val)
cat(paste0("Validation accuracy for the full logistic regression is ",
           round(acc_lr_fact, 3), 
           "."))

