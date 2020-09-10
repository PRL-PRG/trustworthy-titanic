
#Libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(caret)
library(ROCR)
library(keras)
library(MLmetrics)
library(RWeka)
library(xgboost)
library(viridis)

#Loading data 
train_data <- read_csv("../input/train.csv")
submission_test <- read_csv("../input/test.csv")

submission_test$Survived <- NA
data_titanic <- rbind(train_data, submission_test)

#Data structure
glimpse(data_titanic)

count_na <- function(x){
  sum(is.na(x))
}
sapply(data_titanic, count_na)

#Check how the names looks like
head(data_titanic$Name, 5)

data_titanic_clean <- data_titanic %>%
  mutate(Name_c = Name) %>%
  separate(Name_c, c("First_name", "Title_last_name"), sep = "," ) %>%
  separate("Title_last_name", c("Title", "last_name"), sep = "\\.") %>%
  mutate(Title = str_trim(Title)) %>%
  select(- First_name, - last_name)

unique(data_titanic_clean$Title)

ggplot(data_titanic_clean, aes(x = Title, y = Age))+
  geom_boxplot(na.rm = TRUE)

title_Mr <- c ("Don", "Rev", "Dr", "Major", "Sir", "Col", "Capt", "Jonkheer")
title_Mrs <- c("Lady", "Mme", "the Countess", "Dona")
title_Miss <- c("Ms", "Mlle")

data_titanic_clean$Title[data_titanic_clean$Title %in% title_Mr] <- "Mr"
data_titanic_clean$Title[data_titanic_clean$Title %in% title_Mrs] <- "Mrs"
data_titanic_clean$Title[data_titanic_clean$Title %in% title_Miss] <- "Miss"

unique(data_titanic_clean$Title)

#Extract the medians
meds <- data_titanic_clean %>%
  group_by(Title) %>%
  summarise(med = median (Age, na.rm = TRUE))
#Simple visualization
ggplot(data_titanic_clean, aes(x = Title, y = Age))+
  geom_boxplot(na.rm = TRUE) +
  geom_text(data = meds, aes(x = Title, y = med, label = med), 
            size = 3, vjust = -0.5)

#Check which title contains NA value in *Age*
data_titanic_clean %>%
  filter(is.na(Age)) %>%
  select(Title) %>%
  distinct()

#Simple replacing function
fun_age <- function (Tit, Age_med) {
data_titanic_clean[(data_titanic_clean$Title == Tit & is.na(data_titanic_clean$Age)), "Age"] <- Age_med
data_titanic_clean
}

data_titanic_clean <- fun_age("Mr", 30)
data_titanic_clean <- fun_age("Mrs", 35.5)
data_titanic_clean <- fun_age("Miss", 22)
data_titanic_clean <- fun_age("Master", 4)


#Data summary
summary(data_titanic_clean)

data_titanic_clean <- data_titanic_clean %>%
  mutate(Pclass = as.factor(Pclass))

meds_fare <- data_titanic_clean %>%
  group_by(Pclass) %>%
  summarise(med = median (Fare, na.rm = TRUE))
           
ggplot(data_titanic_clean, aes(x = Pclass, y = Fare))+
  geom_boxplot(na.rm = TRUE) +
  geom_text(data = meds_fare, aes(x = Pclass, y = med, label = med), 
            size = 3, vjust = -0.5)

data_titanic_clean[is.na(data_titanic_clean$Fare) & data_titanic_clean$Pclass == 3, "Fare"] <-0

fun_fare <- function (class, Fare_med){
  data_titanic_clean[data_titanic_clean$Fare == 0 & data_titanic_clean$Pclass == class, "Fare"] <- Fare_med
  data_titanic_clean
}

data_titanic_clean  <- fun_fare(1, Fare_med = 60)
data_titanic_clean  <- fun_fare(2, Fare_med = 14.25)
data_titanic_clean  <- fun_fare(3, Fare_med = 8.05)

data_titanic_clean <- data_titanic_clean %>%
  select(-Ticket, -Name)

data_titanic_clean <- data_titanic_clean %>%
  mutate(Family_size = SibSp + Parch + 1)

data_titanic_clean <- data_titanic_clean %>%
  mutate(Cabin = substr(data_titanic_clean$Cabin, start = 1, stop = 1 ))

data_titanic_clean[is.na(data_titanic_clean$Cabin), 9] <- "none"
unique(data_titanic_clean$Cabin)


data_titanic_clean[is.na(data_titanic_clean$Embarked), 10] <- "S"

Pclass <- ggplot(data_titanic_clean)+
  geom_bar(aes(x = factor(Survived), fill = factor(Pclass) ), position = "fill")+
  scale_fill_viridis(discrete = TRUE)

Sex <- ggplot(data_titanic_clean)+
  geom_bar(aes(x = factor(Survived), fill = factor(Sex) ), position = "fill")+
  scale_fill_viridis(discrete = TRUE)

Age <- ggplot(data_titanic_clean)+
  geom_violin(aes(x = factor(Survived), y = Age, fill = factor(Survived)))+
  scale_fill_viridis(discrete = TRUE)

Fare <- ggplot(data_titanic_clean)+
  geom_violin(aes(x = factor(Survived), y = Fare, fill = factor(Survived)))+
  scale_fill_viridis(discrete = TRUE)

Fam_size <- ggplot(data_titanic_clean)+
  geom_bar(aes(x = factor(Survived), fill = factor(Family_size) ), position = "fill")+
  scale_fill_viridis(discrete = TRUE)

Embarked <- ggplot(data_titanic_clean)+
  geom_bar(aes(x = factor(Survived), fill = factor(Embarked) ), position = "fill")+
  scale_fill_viridis(discrete = TRUE)

Cabin <- ggplot(data_titanic_clean)+
  geom_bar(aes(x = factor(Survived), fill = factor(Cabin) ), position = "fill")+
  scale_fill_viridis(discrete = TRUE)

Title <- ggplot(data_titanic_clean)+
  geom_bar(aes(x = factor(Survived), fill = factor(Title) ), position = "fill")+
  scale_fill_viridis(discrete = TRUE)

grid.arrange(Pclass, Sex, Fam_size, Cabin, Embarked, Title, Age, Fare, 
             nrow = 4, ncol = 2)

#Creating dummy variables
data_titanic_clean <- data_titanic_clean %>%
  mutate(Survived = as.factor(if_else(Survived == 0, "No", "Yes")),
         Pclass1 = if_else(as.numeric(Pclass) == 1, 1, 0),
         Pclass3 = if_else(as.numeric(Pclass) == 3, 1, 0),
         Female = if_else(Sex == "female", 1, 0),
         Embarked_C = if_else(Embarked == "C",1,0),
         Embarked_S = if_else(Embarked == "S", 1, 0),
         Title_Miss = if_else(Title == "Miss",1,0),
         Title_Mr = if_else(Title == "Mr", 1, 0),
         Title_Mrs = if_else(Title == "Mrs", 1, 0),
         alone = if_else(Family_size == 1, 1, 0),
         med_fam = if_else(Family_size %in% c(2,3),1,0),
         Cabin_y_n = if_else(Cabin == "none", 1, 0)
         ) %>%
  select(- Parch, - SibSp, - Cabin, - Family_size, - Title, -Pclass, -Sex, -Embarked)

summary(data_titanic_clean)
        

set.seed(150)

data_titanic_clean$Age <- scale(data_titanic_clean$Age, center = TRUE, scale = TRUE)
data_titanic_clean$Fare<- scale(data_titanic_clean$Fare, center = TRUE, scale = TRUE)

submission <- data_frame(PassengerId = submission_test$PassengerId, Survived = NA)
train_index <- 1:nrow(train_data)
complete_train_data <- data_titanic_clean[train_index,-1]
submission_test <- data_titanic_clean[- train_index, c(-1, -2)]

#Extract the labels
complete_train_labels <- complete_train_data$Survived



#Vlidation parameters 
train_ctrl <- trainControl(method = "cv", 
                           number = 10, 
                           selectionFunction = "oneSE", 
                           classProb = TRUE, 
                           summaryFunction = prSummary)

set.seed(301)
model_log <- caret::train(Survived ~., data = complete_train_data, method = "glm", 
                          metric = "F", trControl = train_ctrl)

Model_performance <- tibble(F_1_score = NA, AUC = NA, Model = NA)

#Saving best model results
Model_performance[1, 1]<- model_log$results$F
Model_performance[1, 2] <- model_log$results$AUC
Model_performance[1, 3] <- "Log_regression"

cat("AUC = ", model_log$results$AUC, "\n")
cat("F1 = ",  model_log$results$F)

k_grid <- data.frame("k" = c(1:50))

set.seed(301)
model_KNN <- caret::train(Survived ~., data = complete_train_data, method = "knn", 
                          trControl = train_ctrl, tuneGrid = k_grid, metric = "F")

#Saving best model results
best_k <- model_KNN$bestTune$k

Model_performance[2, 1] <- model_KNN$results[best_k, ]$F
Model_performance[2, 2] <- model_KNN$results[best_k, ]$AUC
Model_performance[2, 3] <- "K_NN"

cat("AUC = ", model_KNN$results[best_k, ]$AUC, "\n")
cat("F1 = ",  model_KNN$results[best_k, ]$F)



set.seed(301)
model_LDA <- train(Survived ~., data = complete_train_data, 
                   trControl = train_ctrl, metric = "F", method = "lda")

#Saving best model results
Model_performance[3, 1] <- model_KNN$results[best_k, ]$F
Model_performance[3, 2] <- model_LDA$results$F
Model_performance[3, 3] <- "LDA"

cat("AUC = ", model_LDA$results$AUC, "\n")
cat("F1 = ",  model_LDA$results$F)

grid <- expand.grid(NumOpt = c(1:7),
                    NumFolds = c(1,2,3),
                    MinWeights = c(1,2,3)
                    )

model_RIP <- train(Survived ~., data = complete_train_data, trControl = train_ctrl, 
                   method = "JRip", metric = "F", tuneGrid = grid)

#Saving best model results
best <- as.numeric(rownames(model_RIP$bestTune))

best_k <- model_KNN$bestTune$k

Model_performance[4, 1] <- model_RIP$results[best, ]$F
Model_performance[4, 2] <- model_RIP$results[best, ]$AUC
Model_performance[4, 3] <- "RIPPER"

cat("AUC = ", model_RIP$results[best, ]$AUC, "\n")
cat("F1 = ",  model_RIP$results[best, ]$F)

grid <- expand.grid(model = "tree",
                    trials = c(1:30),
                    winnow = "FALSE")

set.seed(301)
model_Tree <- train(Survived ~ ., data = complete_train_data,
                           method = "C5.0",metric = "F", 
                           trControl = train_ctrl, tuneGrid = grid)

#Saving best model results
best <- as.numeric(rownames(model_Tree$bestTune))

Model_performance[5, 1] <- model_Tree$results[best, ]$F
Model_performance[5, 2] <- model_Tree$results[best, ]$AUC
Model_performance[5, 3] <- "Decision_Tree"

cat("AUC = ", model_Tree$results[best, ]$AUC, "\n")
cat("F1 = ",  model_Tree$results[best, ]$F)

grid <- expand.grid(mtry = c(1:13))
                    
set.seed(301)
model_RF <- train(Survived ~., data = complete_train_data, method = "rf", 
                  metric = "F", trControl = train_ctrl, tuneGrid = grid)

#Saving best model results
best <- as.numeric(rownames(model_RF$bestTune))

Model_performance[6, 1] <- model_RF$results[best, ]$F
Model_performance[6, 2] <- model_RF$results[best, ]$AUC
Model_performance[6, 3] <- "Random_Forest"

cat("AUC = ", model_RF$results[best, ]$AUC, "\n")
cat("F1 = ",  model_RF$results[best, ]$F)


#Data preparation
train_xgb_x <- xgb.DMatrix(as.matrix(complete_train_data[,-1]))
submission_xgb_x <- xgb.DMatrix(as.matrix(submission_test))

#Tuning parameters
grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c (2, 6, 10),
  eta = c (0.01, 0.1),
  gamma = c(0, 0.05),
  min_child_weight = c(0.5, 1),
  colsample_bytree = c(0.1, 0.4, 1),
  subsample = c(0.5, 1)
)

#Training model
set.seed(301)
model_XGB <- caret::train(
  x = train_xgb_x,
  y = complete_train_labels,
  trControl = train_ctrl,
  method = "xgbTree",
  tuneGrid = grid,
  metric = "F"
)

#Saving best model results
best <- as.numeric(rownames(model_XGB$bestTune))

Model_performance[7, 1] <- model_XGB$results[best, ]$F
Model_performance[7, 2] <- model_XGB$results[best, ]$AUC
Model_performance[7, 3] <- "XGboosting"

cat("AUC = ", model_XGB$results[best, ]$AUC, "\n")
cat("F1 = ",  model_XGB$results[best, ]$F)

grid <- expand.grid(
  cost = seq(from = 1, to = 6, by = 1)
)

set.seed(301)
model_SVM <- train(
  x = as.matrix(complete_train_data[,-1]),
  y = complete_train_labels,
  trControl = train_ctrl,
  method = "svmLinear2",
  tuneGrid = grid,
  metric = "F"
)

#Saving best model results
best <- as.numeric(rownames(model_SVM$bestTune))

Model_performance[8, 1] <- model_SVM$results[best, ]$F
Model_performance[8, 2] <- model_SVM$results[best, ]$AUC
Model_performance[8, 3] <- "SVM_linear"

cat("AUC = ", model_SVM$results[best, ]$AUC, "\n")
cat("F1 = ",  model_SVM$results[best, ]$F)

#Data preparation
complete_train_data_nn <- array(as.matrix(complete_train_data[,-1]), 
                                dim = c(891, 13))
levels(complete_train_labels) <- c(0, 1)
complete_train_labels_nn <- array(complete_train_labels, dim = c(891))
submission_nn <- array(as.matrix(submission_test), dim = c(418,13))

#Training model
model_NN <-  keras_model_sequential() %>%
    layer_dense(units =  5, activation = "relu",
                input_shape = dim(complete_train_data_nn )[[2]]) %>%
    layer_dense(units = 5, activation = "relu") %>%
    layer_dense(units = 5, activation = "relu") %>%
    layer_dense(units = 1, "sigmoid")

model_NN %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
  )

train_history <- model_NN %>% fit(complete_train_data_nn, complete_train_labels_nn,
                epochs = 10, batch_size = 1, validation_split = 0.3)

Model_performance
cat("Neural Network ACC = ", train_history$metrics$val_acc[10], "\n")


pred_prob_LOG <- predict(model_log, newdata = submission_test, type = "prob")[,2]
pred_prob_LDA <- predict(model_LDA, newdata = submission_test, type = "prob")[,2]
pred_prob_RF <- predict(model_RF, newdata = submission_test, type = "prob")[,2]
pred_prob_XGB <- predict(model_XGB, newdata = submission_xgb_x, type = "prob")[,2]
pred_prob_RIP <- predict(model_RIP, newdata = submission_test , type = "prob")[,2]
model_prob_NN <- model_NN %>% predict(submission_nn)
pred_prob_KNN <- predict(model_KNN, newdata = submission_test, type = "prob")[,2]
pred_prob_TREE <- predict(model_Tree, newdata = submission_test, type = "prob")[,2]
pred_prob_SVM <- predict(model_SVM, newdata = submission_test, type = "prob")[,2]

final_prob <- (pred_prob_KNN + 
               pred_prob_RF + 
               pred_prob_RIP + 
               pred_prob_LOG  ) / 4


final_prob <- if_else(final_prob > 0.5,"1", "0")

submission$Survived <- as.numeric(final_prob)

write.csv(submission, "Submission.csv", col.names = TRUE, row.names = FALSE)
