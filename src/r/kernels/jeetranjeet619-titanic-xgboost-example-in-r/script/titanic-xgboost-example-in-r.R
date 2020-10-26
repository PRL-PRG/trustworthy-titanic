library(dplyr)
library(caret)
library(mice)
library(ggplot2)
library(ggthemes)
library(caTools)
library(glue)
library(xgboost)
library(Matrix)
train = read.csv("../input/train.csv", stringsAsFactors = FALSE)
test = read.csv("../input/test.csv", stringsAsFactors = FALSE)
complete_data = bind_rows(train, test)
complete_data$Survived = as.factor(complete_data$Survived)
complete_data$Pclass = as.ordered(complete_data$Pclass)
complete_data$Sex = as.factor(complete_data$Sex)
complete_data$Age = as.numeric(complete_data$Age)
complete_data$Embarked = as.factor(complete_data$Embarked)
cbind(colSums(is.na(complete_data)))
which(complete_data$Fare %in% NA)
complete_data[1044, ]
glue("Mean of Fare for Pclass = 3 and Embarked = S : {mean(complete_data$Fare[complete_data$Pclass == '3' & complete_data$Embarked == 'S'], na.rm = T)}")
glue("Median of Fare for Pclass = 3 and Embarked = S : {median(complete_data$Fare[complete_data$Pclass == '3' & complete_data$Embarked == 'S'], na.rm = T)}")
ggplot(complete_data[complete_data$Pclass == "3" & complete_data$Embarked == "S", ], aes(x = Fare)) + geom_density(fill = "green", alpha = 0.9) + geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = "red", linetype = "dashed", lwd = 1) + geom_vline(aes(xintercept = mean(Fare, na.rm = T)), colour = "blue", linetype = "dashed", lwd = 1) + theme_few() + labs(title = "Density Plot for Fare Distribution with Pclass = 3 and Embarked = S")
complete_data$Fare[1044] = median(complete_data$Fare[complete_data$Pclass == 3 & complete_data$Embarked == "S"], na.rm = T)
glue("Total NAs in Age : {sum(is.na(complete_data$Age))}.")
glue("Mean of Age :  {mean(complete_data$Age, na.rm = T)}")
glue("Median of Age :  {median(complete_data$Age, na.rm = T)}")
ggplot(data = complete_data, aes(x = Age, y = Age)) + geom_boxplot() + labs(title = "Age Distribution", x = "Age")
ggplot(data = complete_data, aes(x = Age)) + geom_density(fill = "#B86997", alpha = 0.8) + geom_vline(aes(xintercept = median(Age, na.rm = T)), colour = "black", linetype = "dashed", lwd = 1) + geom_vline(aes(xintercept = mean(Age, na.rm = T)), colour = "blue", linetype = "dashed", lwd = 1) + labs(title = "Fare Distribution")
complete_data$Age[is.na(complete_data$Age) == T] = median(complete_data$Age, na.rm = TRUE)
sum(is.na(complete_data$Cabin) | complete_data$Cabin == "")
glue("NA/empty rows in Embarked Column : {sum(is.na(complete_data$Embarked) | complete_data$Embarked == '')}")
which(is.na(complete_data$Embarked) | complete_data$Embarked == "")
complete_data[c(62, 830), ]
table(complete_data$Embarked[complete_data$Pclass == 1 & complete_data$Fare >= 75 & complete_data$Fare <= 85 & complete_data$Sex == "female"])
mean(complete_data$Fare[complete_data$Pclass == 1 & complete_data$Sex == "female"])
median(complete_data$Fare[complete_data$Pclass == 1 & complete_data$Sex == "female"])
mean(complete_data$Fare[complete_data$Embarked == "C" & complete_data$Pclass == 1 & complete_data$Sex == "female"])
mean(complete_data$Fare[complete_data$Embarked == "S" & complete_data$Pclass == 1 & complete_data$Sex == "female"])
median(complete_data$Fare[complete_data$Embarked == "C" & complete_data$Pclass == 1 & complete_data$Sex == "female"])
median(complete_data$Fare[complete_data$Embarked == "S" & complete_data$Pclass == 1 & complete_data$Sex == "female"])
ggplot(complete_data[complete_data$Sex == "female", ], aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot() + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", lwd = 2) + theme_few()
complete_data$Embarked[c(62, 830)] = "S"
ggplot(complete_data[1:891, ], aes(x = Pclass, fill = Survived)) + geom_bar(stat = "count", position = "dodge", color = "black") + labs(title = "Survival By Passenger Class", x = "Passenger Class") + theme_few() + geom_label(stat = "count", position = position_dodge(width = 1), hjust = "center", aes(label = ..count..))
for (i in 1:nrow(complete_data)) {
    if (grepl(pattern = "Mr. ", x = complete_data$Name[i], ignore.case = TRUE) == 1) {
        complete_data$Title[i] = "Mr"
    }
    else if (grepl(pattern = "Mrs. ", x = complete_data$Name[i], ignore.case = TRUE) == 1) {
        complete_data$Title[i] = "Mrs"
    }
    else if (grepl(pattern = "Miss. ", x = complete_data$Name[i], ignore.case = TRUE) == 1) {
        complete_data$Title[i] = "Miss"
    }
    else if (grepl(pattern = "Master. ", x = complete_data$Name[i], ignore.case = TRUE) == 1) {
        complete_data$Title[i] = "Master"
    }
    else {
        complete_data$Title[i] = "Rare"
    }
}
complete_data$Title = as.factor(complete_data$Title)
sum(is.na(complete_data$Title))
cbind(table(complete_data$Title))
ggplot(complete_data[1:891, ], aes(x = Title, fill = Survived)) + geom_histogram(stat = "count", position = "dodge", color = "black") + labs(title = "Title Wise Survival", x = "Title", y = "Count") + geom_label(stat = "count", position = position_dodge(width = 1), hjust = "middle", aes(label = ..count..)) + theme_few()
complete_data$Surname = sapply(complete_data$Name, function(x) strsplit(x, "[,.]")[[1]][1])
complete_data$Surname = as.factor(complete_data$Surname)
glue("Number of Distinct Surname : {n_distinct(complete_data$Surname)}")
complete_data$Fsize = complete_data$SibSp + complete_data$Parch + 1
ggplot(complete_data[1:891, ], aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge", color = "black") + scale_x_continuous(breaks = c(min(complete_data$Fsize):max(complete_data$Fsize))) + labs(title = "Survival Based on Family Size", x = "Family Size", label = "Survived") + theme_few()
complete_data$FsizeDiscrete[complete_data$Fsize == 1] = "Singleton"
complete_data$FsizeDiscrete[complete_data$Fsize <= 5 & complete_data$Fsize > 1] = "Small"
complete_data$FsizeDiscrete[complete_data$Fsize > 5] = "Large"
complete_data$FsizeDiscrete = as.factor(complete_data$FsizeDiscrete)
mosaicplot(table(complete_data$FsizeDiscrete, complete_data$Survived), main = "Survival by Family Size ", shade = TRUE)
complete_data$Solo = "No"
complete_data$Solo[complete_data$SibSp == 0 & complete_data$Parch == 0] = "Yes"
complete_data$Solo = as.factor(complete_data$Solo)
ggplot(data = complete_data[1:891, ], aes(x = Solo, fill = as.factor(Survived))) + geom_bar(stat = "Count", position = "dodge", color = "black") + labs(title = "Solo Traveller Survival", x = "Solo Traveller ?") + geom_label(stat = "count", position = position_dodge(width = 1), hjust = "center", aes(label = ..count..))
for (i in 1:nrow(complete_data)) {
    if (complete_data$Age[i] <= 4) {
        complete_data$AgeGroup[i] = "Infant"
    }
    else if (complete_data$Age[i] > 4 & complete_data$Age[i] <= 10) {
        complete_data$AgeGroup[i] = "Child"
    }
    else if (complete_data$Age[i] > 10 & complete_data$Age[i] <= 18) {
        complete_data$AgeGroup[i] = "Young"
    }
    else if (complete_data$Age[i] > 18 & complete_data$Age[i] <= 50) {
        complete_data$AgeGroup[i] = "Adults"
    }
    else {
        complete_data$AgeGroup[i] = "Old"
    }
}
complete_data$AgeGroup = as.factor(complete_data$AgeGroup)
ggplot(complete_data[1:891, ], aes(x = AgeGroup, fill = Survived)) + geom_bar(stat = "count", position = "stack", color = "black") + labs(title = "Survival By Age Group", x = "Age Group") + theme_few()
ggplot(complete_data[1:891, ], aes(x = AgeGroup, fill = Survived)) + geom_bar(stat = "count", position = "dodge", color = "black") + labs(title = "Survival By Age Group", x = "Age Group") + theme_few() + geom_label(stat = "count", position = position_dodge(width = 1), hjust = "center", aes(label = ..count..))
complete_data$Mother = "Not Mother"
complete_data$Mother[complete_data$Sex == "female" & complete_data$Parch > 0 & complete_data$Age > 18 & complete_data$Title != "Miss"] = "Mother"
complete_data$Mother = as.factor(complete_data$Mother)
ggplot(complete_data[1:891, ], aes(x = Mother, fill = Survived)) + geom_bar(stat = "count", position = "dodge", color = "black") + labs(title = "Survival for Mothers", x = "Mother") + geom_label(stat = "count", position = position_dodge(width = 1), hjust = "center", aes(label = ..count..)) + theme_few()
ggplot(complete_data[1:891, ], aes(Age, fill = Survived)) + geom_histogram(color = "black") + facet_grid(. ~ Sex) + theme_few() + ggtitle("Survival by Age and Gender")
ggplot(data = complete_data[1:891, ], aes(x = Survived, y = Fare, fill = Survived)) + geom_boxplot() + xlab("Fare Box plot")
ggplot(data = complete_data[1:891, ], aes(x = Fare, fill = Survived)) + geom_density(alpha = 0.7) + labs(title = "Fare Density Plot", x = "Fare")
glimpse(complete_data)
md.pattern(complete_data[1:891, ])
cbind(sapply(complete_data, function(x) {
    is.factor(x)
}))
xgb_data = complete_data[, c("Survived", "Age", "SibSp", "Parch", "Fare", "Fsize")]
xgb_data$Plass_1 = ifelse(complete_data$Pclass == 1, 1, 0)
xgb_data$Plass_2 = ifelse(complete_data$Pclass == 2, 1, 0)
xgb_data$Plass_3 = ifelse(complete_data$Pclass == 3, 1, 0)
xgb_data$Sex_Male = ifelse(complete_data$Sex == "male", 1, 0)
xgb_data$Embarked_C = ifelse(complete_data$Embarked == "C", 1, 0)
xgb_data$Embarked_Q = ifelse(complete_data$Embarked == "Q", 1, 0)
xgb_data$Embarked_S = ifelse(complete_data$Embarked == "S", 1, 0)
xgb_data$Title_Mr = ifelse(complete_data$Title == "Mr", 1, 0)
xgb_data$Title_Mrs = ifelse(complete_data$Title == "Mrs", 1, 0)
xgb_data$Title_Miss = ifelse(complete_data$Title == "Miss", 1, 0)
xgb_data$Title_Master = ifelse(complete_data$Title == "Master", 1, 0)
xgb_data$Title_Rare = ifelse(complete_data$Title == "Rare", 1, 0)
xgb_data$FsizeDiscrete_Singleton = ifelse(complete_data$FsizeDiscrete == "Singleton", 1, 0)
xgb_data$FsizeDiscrete_Small = ifelse(complete_data$FsizeDiscrete == "Small", 1, 0)
xgb_data$FsizeDiscrete_Large = ifelse(complete_data$FsizeDiscrete == "Large", 1, 0)
xgb_data$Solo_Yes = ifelse(complete_data$Solo == "Yes", 1, 0)
xgb_data$AgeGroup_Infant = ifelse(complete_data$AgeGroup == "Infant", 1, 0)
xgb_data$AgeGroup_Child = ifelse(complete_data$AgeGroup == "Child", 1, 0)
xgb_data$AgeGroup_Young = ifelse(complete_data$AgeGroup == "Young", 1, 0)
xgb_data$AgeGroup_Adult = ifelse(complete_data$AgeGroup == "Adults", 1, 0)
xgb_data$AgeGroup_Old = ifelse(complete_data$AgeGroup == "Old", 1, 0)
xgb_data$Mother_Yes = ifelse(complete_data$Mother == "Mother", 1, 0)
training_data = xgb_data[1:891, ]
testing_data = xgb_data[892:1309, ]
testing_data$Survived = NULL
paramList <- list(eta = 0.1, gamma = 0, max.depth = 10, min_child_weight = 5, subsample = 1, colsample_bytree = 0.2)
xgb_fit = xgboost(data = as.matrix(training_data[, -which(colnames(training_data) == "Survived")]), label = as.matrix(training_data$Survived), params = paramList, missing = NA, nrounds = 50000, verbose = 1, eval_metric = "error", early_stopping_rounds = 2000, objective = "binary:logistic")
importance <- xgb.importance(feature_names = colnames(testing_data), model = xgb_fit)
xgb.plot.importance(importance)
ggplot(data = importance, aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) + geom_bar(stat = "identity", color = "black") + coord_flip() + labs(title = "Feature Importance By XGBoost", x = "Features", y = "Gain") + theme_calc()
evaluation_log = as.data.frame(xgb_fit$evaluation_log)
colnames(evaluation_log)
ggplot(evaluation_log, mapping = aes(x = iter)) + geom_line(aes(y = train_error, color = "Traning Error")) + labs(title = "Execution Error log", x = "Iteration", y = "Error") + theme_classic()
xgb_pred = predict(xgb_fit, as.matrix(testing_data))
xgb_result = data.frame(PassengerId = complete_data$PassengerId[892:1309], Survived = xgb_pred)
xgb_result$Survived = ifelse(xgb_result$Survived > 0.5, 1, 0)
write.csv(xgb_result, "xgb_sub.csv", row.names = F)
len = round(nrow(training_data) * 0.8, 0)
dtrain = xgb.DMatrix(data = as.matrix(training_data[1:len, -which(colnames(training_data) == "Survived")]), label = as.matrix(training_data$Survived[1:len]))
dtest = xgb.DMatrix(data = as.matrix(training_data[(len + 1):891, -which(colnames(training_data) == "Survived")]), label = as.matrix(training_data$Survived[(len + 1):891]))
watch_list = list(train = dtrain, test = dtest)
xgb_train_fit = xgb.train(data = dtrain, eta = 0.1, gamma = 0, max.depth = 10, min_child_weight = 5, subsample = 1, colsample_bytree = 0.2, watchlist = watch_list, missing = NA, nrounds = 50000, early_stopping_rounds = 2000, verbose = 1, eval_metric = "error", objective = "binary:logistic")
importance2 <- xgb.importance(feature_names = colnames(testing_data), model = xgb_train_fit)
xgb.plot.importance(importance2)
ggplot(data = importance2, aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) + geom_bar(stat = "identity", color = "black") + coord_flip() + labs(title = "Feature Importance By XGBoost with Watchlist and Early Stopping", x = "Features", y = "Gain") + theme_calc()
evaluation_log = as.data.frame(xgb_train_fit$evaluation_log)
ggplot(evaluation_log, mapping = aes(x = iter)) + geom_line(aes(y = train_error, color = "Traning Error")) + geom_line(aes(y = test_error, color = "Testing Error")) + labs(title = "Execution Error log", x = "Iteration", y = "Error") + theme_classic()
xgb_pred2 = predict(xgb_train_fit, as.matrix(testing_data))
xgb_result2 = data.frame(PassengerId = complete_data$PassengerId[892:1309], Survived = xgb_pred2)
xgb_result2$Survived = ifelse(xgb_result2$Survived > 0.5, 1, 0)
write.csv(xgb_result2, "xgb_sub2.csv", row.names = F)
