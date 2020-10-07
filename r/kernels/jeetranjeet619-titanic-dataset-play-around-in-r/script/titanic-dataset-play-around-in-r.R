library(dplyr)
library(randomForest)
library(caret)
library(mice)
library(ggplot2)
library(ggthemes)
library(caTools)
library(gbm)
library(e1071)
library(rpart)
library(glue)
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
ggplot(complete_data[1:891, ], aes(x = Pclass, fill = Survived)) + geom_bar(stat = "count", position = "dodge") + labs(title = "Survival By Passenger Class", x = "Passenger Class") + theme_few() + geom_label(stat = "count", position = position_dodge(width = 1), hjust = "center", aes(label = ..count..))
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
ggplot(complete_data[1:891, ], aes(x = Title, fill = Survived)) + geom_histogram(stat = "count", position = "dodge") + labs(title = "Title Wise Survival", x = "Title", y = "Count") + geom_label(stat = "count", position = position_dodge(width = 1), hjust = "middle", aes(label = ..count..)) + theme_few()
complete_data$Surname = sapply(complete_data$Name, function(x) strsplit(x, "[,.]")[[1]][1])
complete_data$Surname = as.factor(complete_data$Surname)
glue("Number of Distinct Surname : {n_distinct(complete_data$Surname)}")
complete_data$Fsize = complete_data$SibSp + complete_data$Parch + 1
ggplot(complete_data[1:891, ], aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(min(complete_data$Fsize):max(complete_data$Fsize))) + labs(title = "Survival Based on Family Size", x = "Family Size", label = "Survived") + theme_few()
complete_data$FsizeDiscrete[complete_data$Fsize == 1] = "Singleton"
complete_data$FsizeDiscrete[complete_data$Fsize <= 5 & complete_data$Fsize > 1] = "Small"
complete_data$FsizeDiscrete[complete_data$Fsize > 5] = "Large"
complete_data$FsizeDiscrete = as.factor(complete_data$FsizeDiscrete)
mosaicplot(table(complete_data$FsizeDiscrete, complete_data$Survived), main = "Survival by Family Size ", shade = TRUE)
complete_data$Solo = "No"
complete_data$Solo[complete_data$SibSp == 0 & complete_data$Parch == 0] = "Yes"
complete_data$Solo = as.factor(complete_data$Solo)
ggplot(data = complete_data[1:891, ], aes(x = Solo, fill = as.factor(Survived))) + geom_bar(stat = "Count", position = "dodge") + labs(title = "Solo Traveller Survival", x = "Solo Traveller ?") + geom_label(stat = "count", position = position_dodge(width = 1), hjust = "center", aes(label = ..count..))
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
ggplot(complete_data[1:891, ], aes(x = AgeGroup, fill = Survived)) + geom_bar(stat = "count", position = "stack") + labs(title = "Survival By Age Group", x = "Age Group") + theme_few()
ggplot(complete_data[1:891, ], aes(x = AgeGroup, fill = Survived)) + geom_bar(stat = "count", position = "dodge") + labs(title = "Survival By Age Group", x = "Age Group") + theme_few() + geom_label(stat = "count", position = position_dodge(width = 1), hjust = "center", aes(label = ..count..))
complete_data$Mother = "Not Mother"
complete_data$Mother[complete_data$Sex == "female" & complete_data$Parch > 0 & complete_data$Age > 18 & complete_data$Title != "Miss"] = "Mother"
complete_data$Mother = as.factor(complete_data$Mother)
ggplot(complete_data[1:891, ], aes(x = Mother, fill = Survived)) + geom_bar(stat = "count", position = "dodge") + labs(title = "Survival for Mothers", x = "Mother") + geom_label(stat = "count", position = position_dodge(width = 1), hjust = "center", aes(label = ..count..)) + theme_few()
ggplot(complete_data[1:891, ], aes(Age, fill = Survived)) + geom_histogram() + facet_grid(. ~ Sex) + theme_few() + ggtitle("Survival by Age and Gender")
ggplot(data = complete_data[1:891, ], aes(x = Survived, y = Fare, fill = Survived)) + geom_boxplot() + xlab("Fare Box plot")
ggplot(data = complete_data[1:891, ], aes(x = Fare, fill = Survived)) + geom_density(alpha = 0.7) + labs(title = "Fare Density Plot", x = "Fare")
glimpse(complete_data)
md.pattern(complete_data[1:891, ])
train_set = complete_data[1:891, ]
test_set = complete_data[892:nrow(complete_data), ]
test_set$Survived = NULL
glm_model = glm(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Fsize + FsizeDiscrete + AgeGroup + Mother + Solo, data = train_set, family = "binomial")
summary(glm_model)
plot(glm_model)
glm_pred = predict(glm_model, test_set)
glm_sub = data.frame(PassengerId = test_set$PassengerId, Survived = glm_pred)
nb_fit = naiveBayes(formula = factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Fsize + FsizeDiscrete + AgeGroup + Mother + Solo, data = train_set)
summary(nb_fit)
nb_pred = predict(nb_fit, test_set)
nb_result = data.frame(PassengerId = test_set$PassengerId, Survived = nb_pred)
write.csv(nb_result, "nb_sub.csv", row.names = F)
svm_fit = svm(formula = factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Fsize + FsizeDiscrete + AgeGroup + Mother + Solo, data = train_set, kernel = "linear", cost = 10, scale = FALSE)
summary(svm_fit)
svm_pred = predict(svm_fit, test_set)
svm_result = data.frame(PassengerId = test_set$PassengerId, Survived = svm_pred)
write.csv(svm_result, "svm_linear_result.csv", row.names = F)
train_control = trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "grid")
tune_grid = expand.grid(.mtry = c(1:13))
start_time <- Sys.time()
rf_model = train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Fsize + FsizeDiscrete + AgeGroup + Mother + Solo, data = train_set, method = "rf", trControl = train_control, tuneGrid = tune_grid, ntree = 70, verbose = TRUE, metric = "Accuracy")
end_time <- Sys.time()
duration <- (end_time - start_time)
print(duration)
rf_model$results
summary(rf_model)
plot(rf_model)
pred1 = predict(rf_model, test_set)
submission_rf1 = data.frame(PassengerId = test_set$PassengerId, Survived = pred1)
write.csv(submission_rf1, "submission_rf1.csv", row.names = FALSE)
set.seed(123)
split = sample.split(Y = train_set$Survived, SplitRatio = 0.8)
training_set = subset(train_set, split == TRUE)
validation_set = subset(train_set, split = FALSE)
rf_model2 = randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Fsize + FsizeDiscrete + AgeGroup + Mother + Solo, data = training_set)
plot(rf_model2)
legend("topright", colnames(rf_model2$err.rate), col = 1:3, fill = 1:3)
pred2 = predict(rf_model2, validation_set)
confusionMatrix(validation_set$Survived, pred2)
result = predict(rf_model2, test_set)
submission_rf2 = data.frame(PassengerId = test_set$PassengerId, Survived = result)
write.csv(submission_rf2, "submission_rf2.csv", row.names = FALSE)
F_importance = importance(rf_model2)
F_importance = data.frame(Features = row.names(F_importance), Importance = round(F_importance[, "MeanDecreaseGini"], 2))
ggplot(F_importance, aes(x = reorder(Features, Importance), y = Importance, fill = Importance)) + geom_bar(stat = "identity") + coord_flip() + geom_text(aes(x = Features, label = Importance, color = "orange"), position = position_stack(vjust = 0.5)) + labs(title = "Feature Importance")
confusionMatrix(pred1, result)
gbmControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "grid")
gbmGrid <- expand.grid(interaction.depth = c(1, 3, 5), n.trees = c(500, 1000, 2000, 5000), shrinkage = c(0.01, 0.005, 0.001, 1e-04), n.minobsinnode = c(1, 3, 5, 10))
options(warn = -1)
start = Sys.time()
gbm_caret_model = train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Fsize + FsizeDiscrete + AgeGroup + Mother, data = train_set, method = "gbm", distribution = "bernoulli", metric = "Accuracy", trControl = gbmControl, tuneGrid = gbmGrid, train.fraction = 0.5, bag.fraction = 0.5, verbose = T)
end = Sys.time()
print(end - start)
gbm_caret_result = predict(gbm_caret_model, test_set)
gbm_caret_sub = data.frame(PassengerId = test_set$PassengerId, Survived = gbm_caret_result)
write.csv(gbm_caret_sub, "gbm_caret_sub.csv", row.names = F)
