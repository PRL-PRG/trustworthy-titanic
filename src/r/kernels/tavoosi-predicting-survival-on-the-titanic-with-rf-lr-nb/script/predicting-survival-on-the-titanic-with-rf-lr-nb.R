knitr::opts_chunk$set(message = FALSE, warning = FALSE)
library(tidyverse)
library(ggthemes)
library(corrplot)
library(VIM)
library(caret)
library(RANN)
library(reshape2)
train_data = read.csv("../input/train.csv", na.strings = "")
test_data = read.csv("../input/test.csv", na.strings = "")
full_data <- bind_rows(train_data, test_data)
head(full_data)
ggplot(full_data[1:891, ], aes(x = factor(Survived), fill = factor(Survived))) + geom_bar(position = "dodge") + scale_x_discrete() + labs(title = "Survival on the Titanic", x = "Outcome", y = "Count") + scale_fill_discrete(name = "Outcome", labels = c("Died", "Survived")) + theme(legend.position = "right") + theme_classic()
ggplot(full_data[1:891, ], aes(x = factor(Sex), fill = factor(Survived))) + geom_bar(position = "dodge") + scale_x_discrete() + labs(title = "Survival by Gender", x = "Gender", y = "Rate") + scale_fill_discrete(name = "Outcome", labels = c("Died", "Survived")) + theme(legend.position = "right") + theme_classic()
ggplot(full_data[1:891, ], aes(x = factor(Pclass), fill = factor(Survived))) + geom_bar(position = "dodge") + scale_x_discrete() + labs(title = "Survival by Passenger Class", x = "Passenger Class", y = "Count") + scale_fill_discrete(name = "Outcome", labels = c("Died", "Survived")) + theme(legend.position = "right") + theme_classic()
ggplot(full_data[1:891, ], aes(x = factor(Embarked), fill = factor(Survived))) + geom_bar(position = "dodge") + scale_x_discrete() + labs(title = "Survival by Embarkment", x = "Embarkment", y = "Count") + scale_fill_discrete(name = "Outcome", labels = c("Died", "Survived")) + theme(legend.position = "right") + theme_classic()
ggplot(full_data[1:891, ]) + geom_freqpoly(aes(x = Age, color = factor(Survived)), binwidth = 1) + theme_classic() + theme(legend.position = "none") + labs(title = "Survival by Age", x = "Age", y = "Count")
ggplot(full_data[1:891, ]) + geom_freqpoly(aes(x = Fare, color = factor(Survived)), binwidth = 0.05) + scale_x_log10() + theme_classic() + theme(legend.position = "none") + labs(title = "Survival by Fare (log10)", x = "Fare (log10)", y = "Count")
full_data$Sex <- as.numeric(full_data$Sex)
corrplot.mixed(corr = cor(full_data[c("Survived", "Fare", "Sex", "Pclass", "Age")], use = "complete.obs"), tl.col = "black", upper = "ellipse")
full_data$family_size = full_data$SibSp + full_data$Parch + 1
ggplot(full_data[1:891, ], aes(x = factor(family_size), fill = factor(Survived))) + geom_bar(position = "dodge") + scale_x_discrete() + labs(title = "Survival by Family Size on Board", x = "Number of family members on board", y = "Count") + scale_fill_discrete(name = "Outcome", labels = c("Died", "Survived")) + theme(legend.position = "right") + theme_classic()
full_data$family_size_range = cut(full_data$family_size, c(0, 1, 4, 15), include.lowest = TRUE)
levels(full_data$family_size_range) = c("1", "2-4", "5+")
ggplot(full_data[1:891, ], aes(x = factor(family_size_range), fill = factor(Survived))) + geom_bar(position = "dodge") + scale_x_discrete() + labs(title = "Survival by Family Size on Board", x = "Family size", y = "Count") + scale_fill_discrete(name = "Outcome", labels = c("Died", "Survived")) + theme(legend.position = "right") + theme_classic()
full_data$Title <- gsub("(.*, )|(\\..*)", "", full_data$Name)
table(full_data$Title)
rare_title = c("Capt", "Col", "Don", "Jonkheer", "Lady", "Major", "Rev", "Sir", "the Countess", "Dr")
full_data$Title[full_data$Title %in% rare_title] <- "Rare title"
full_data$Title[full_data$Title == "Mlle"] <- "Miss"
full_data$Title[full_data$Title == "Ms"] <- "Miss"
full_data$Title[full_data$Title == "Dona"] <- "Miss"
full_data$Title[full_data$Title == "Mme"] <- "Mrs"
ggplot(full_data[1:891, ], aes(x = Title, fill = factor(Survived))) + geom_bar(position = "dodge") + scale_x_discrete() + labs(title = "Survival by Title", x = "Title", y = "Count") + scale_fill_discrete(name = "Outcome", labels = c("Died", "Survived")) + theme(legend.position = "right") + theme_classic()
full_data$Cabin_letter <- gsub("[0-9].*", "", full_data$Cabin)
full_data$Cabin_letter[full_data$Cabin_letter == "E"] <- "EFGT"
full_data$Cabin_letter[full_data$Cabin_letter == "F"] <- "EFGT"
full_data$Cabin_letter[full_data$Cabin_letter == "F E"] <- "EFGT"
full_data$Cabin_letter[full_data$Cabin_letter == "F G"] <- "EFGT"
full_data$Cabin_letter[full_data$Cabin_letter == "G"] <- "EFGT"
full_data$Cabin_letter[full_data$Cabin_letter == "T"] <- "EFGT"
full_data$Cabin_letter[is.na(full_data$Cabin_letter)] <- "Blank"
ggplot(full_data[1:891, ], aes(x = factor(Cabin_letter), fill = factor(Survived))) + geom_bar(position = "dodge") + scale_x_discrete() + labs(title = "Survival by Cabin", x = "Cabin", y = "Count") + scale_fill_discrete(name = "Outcome", labels = c("Died", "Survived")) + theme_classic()
full_data$cabin_presence[full_data$Cabin_letter == "Blank"] <- "No cabin"
full_data$cabin_presence[is.na(full_data$cabin_presence)] <- "Cabin"
ggplot(full_data[1:891, ], aes(x = factor(cabin_presence), fill = factor(Survived))) + geom_bar(position = "dodge") + scale_x_discrete() + labs(title = "Survival by Cabin", x = "Cabin", y = "Count") + scale_fill_discrete(name = "Outcome", labels = c("Died", "Survived")) + theme(legend.position = "right") + theme_classic()
full_data$Ticket_number <- gsub("[^0-9]", "", full_data$Ticket)
table(full_data$Ticket_number == "")
full_data$Ticket_number[full_data$Ticket_number == ""] <- 0
full_data$Ticket_number <- as.integer(full_data$Ticket_number)
ggplot(full_data[1:891, ]) + geom_freqpoly(aes(x = Ticket_number, color = factor(Survived)), binwidth = 0.1) + scale_x_log10() + scale_color_discrete(name = "Outcome", labels = c("Died", "Survived")) + theme_classic() + labs(title = "Survival by Ticket number", x = "Ticket number", y = "Count")
cor(full_data$Ticket_number, as.numeric(full_data$Survived), use = "complete.obs")
full_data_relevant <- subset(full_data, select = c(Survived, Pclass, Sex, Age, Fare, Title, cabin_presence, family_size_range))
full_data_relevant$Survived <- as.factor(full_data_relevant$Survived)
full_data_relevant$Pclass <- factor(full_data_relevant$Pclass, ordered = TRUE)
full_data_relevant$Survived <- as.factor(full_data_relevant$Survived)
full_data_relevant$Title <- as.factor(full_data_relevant$Title)
full_data_relevant$cabin_presence <- as.factor(full_data_relevant$cabin_presence)
aggr(full_data_relevant, sortVars = TRUE, prop = FALSE, cex.axis = 0.6, numbers = TRUE)
md_prediction <- preProcess(full_data_relevant[c(2:8)], method = c("knnImpute", "center", "scale"))
print(md_prediction)
full_data_complete <- predict(md_prediction, newdata = full_data_relevant[c(2:8)])
full_data_final <- data.frame(full_data_complete, full_data$Survived)
full_data_final <- cbind(full_data$PassengerId, full_data_final)
full_data_final <- rename(full_data_final, Survived = full_data.Survived, PassengerId = `full_data$PassengerId`)
full_data_final$Survived <- as.factor(full_data_final$Survived)
train <- full_data_final[1:891, ]
test <- full_data_final[892:1309, ]
set.seed(222)
rf_model <- train(Survived ~ ., method = "rf", data = train)
print(rf_model)
confusionMatrix(rf_model)
rf_err_model <- as.data.frame(rf_model[["finalModel"]][["err.rate"]])
rf_err_model$sequence <- seq(1:500)
rf_err_model <- rename(rf_err_model, Died = "0", Survived = "1")
rf_err_model <- melt(rf_err_model, id = "sequence")
ggplot(rf_err_model, aes(x = sequence, y = value, color = variable)) + geom_line() + scale_colour_manual(values = c("black", "red2", "forestgreen")) + theme_classic() + labs(title = "Error rate in prediction", x = "Sequence", y = "Error rate")
rf_importance <- varImp(rf_model)
ggplot(rf_importance, aes(x = reorder(variable, importance), y = importance)) + geom_bar(stat = "identity") + labs(title = "Importance of predictors", x = "Predictors", y = "Importance") + theme_light()
prediction_rf <- predict(rf_model, test)
solution_rf <- data.frame(PassengerID = test$PassengerId, Survived = prediction_rf)
write.csv(solution_rf, file = "rf_Titanic_Solution.csv", row.names = F)
fitControl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
set.seed(222)
lr_model <- train(factor(Survived) ~ ., data = train, method = "glm", family = binomial(), trControl = fitControl)
print(lr_model)
confusionMatrix(lr_model)
lr_importance <- varImp(lr_model)
ggplot(lr_importance, aes(x = reorder(variable, importance), y = importance)) + geom_bar(stat = "identity") + labs(title = "Importance of predictors", x = "Predictors", y = "Importance") + theme_light()
prediction_lr <- predict(lr_model, test)
solution_lr <- data.frame(PassengerID = test$PassengerId, Survived = prediction_lr)
write.csv(solution_lr, file = "lr_Titanic_Solution.csv", row.names = F)
train_features <- full_data_final[1:891, -13]
train_response <- full_data_final[1:891, 13]
fitControl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
set.seed(222)
nb_model <- train(Survived ~ ., data = train, method = "nb", trControl = fitControl)
print(nb_model)
confusionMatrix(nb_model)
nb_importance <- varImp(nb_model)
ggplot(nb_importance, aes(x = reorder(variable, importance), y = importance)) + geom_bar(stat = "identity") + labs(title = "Importance of predictors", x = "Predictors", y = "Importance") + theme_light()
prediction_nb <- predict(nb_model, test)
solution_nb <- data.frame(PassengerID = test$PassengerId, Survived = prediction_nb)
write.csv(solution_nb, file = "nb_Titanic_Solution.csv", row.names = F)
