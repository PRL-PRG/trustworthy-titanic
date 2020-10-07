library("dplyr")
library("mice")
library("randomForest")
library("e1071")
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
full <- bind_rows(train, test)
full$Title <- gsub("(.*, )|(\\..*)", "", full$Name)
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
full$Title[full$Title == "Mlle"] <- "Miss"
full$Title[full$Title == "Ms"] <- "Miss"
full$Title[full$Title == "Mme"] <- "Mrs"
full$Title[full$Title %in% rare_title] <- "Rare Title"
full$Surname <- sapply(full$Name, function(x) strsplit(x, split = "[,.]")[[1]][1])
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep = "_")
full$FsizeD[full$Fsize == 1] <- "singleton"
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- "small"
full$FsizeD[full$Fsize > 4] <- "large"
strsplit(full$Cabin[2], NULL)[[1]]
full$Deck <- factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
full[c(62, 830), "Embarked"]
embark_fare <- full %>% filter(PassengerId != 62 & PassengerId != 830)
full$Embarked[c(62, 830)] <- "C"
full$Fare[1044] <- median(full[full$Pclass == "3" & full$Embarked == "S", ]$Fare, na.rm = TRUE)
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Title", "Surname", "Family", "FsizeD")
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
set.seed(42)
mice_mod <- mice(full[, !names(full) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Family", "Surname", "Survived")], method = "rf")
mice_output <- complete(mice_mod)
full$Age <- mice_output$Age
full$Child[full$Age < 18] <- "Child"
full$Child[full$Age >= 18] <- "Adult"
full$Child <- factor(full$Child)
md.pattern(full)
full$Survived <- as.factor(full$Survived)
train <- full[1:891, ]
test <- full[892:1309, ]
train$Deck <- NULL
test$Deck <- NULL
test$Survived <- NULL
logit_model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child, data = train, family = binomial)
logit_probabilities <- predict(logit_model, test, type = "response")
logit_prediction <- data.frame(PassengerID = test$PassengerId, Survived = ifelse(logit_probabilities > 0.5, 1, 0))
write.csv(logit_prediction, file = "logit_prediction.csv", row.names = FALSE)
svc_cost <- 10^seq(-2, 2, 1)
svc_linear_cv <- rep(0, length(svc_cost))
for (i in 1:length(svc_cost)) {
    set.seed(2017)
    svc_linear <- svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child, data = train, kernel = "linear", cost = svc_cost[i], probability = TRUE, cross = 10)
    svc_linear_cv[i] <- svc_linear$tot.accuracy
}
svc_linear <- svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child, data = train, kernel = "linear", cost = svc_cost[which.max(svc_linear_cv)], probability = TRUE, cross = 10)
svc_linear_prob <- predict(svc_linear, test, probability = TRUE)
svc_linear_prob <- attr(svc_linear_prob, "probabilities")[, 2]
svc_linear_pred <- data.frame(PassengerID = test$PassengerId, Survived = ifelse(svc_linear_prob > 0.5, 1, 0))
write.csv(svc_linear_pred, file = "svc_linear_pred.csv", row.names = FALSE)
svc_poly_cv <- rep(0, 9)
for (deg in 2:9) {
    set.seed(2017)
    svc_poly <- svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child, data = train, kernel = "polynomial", degree = deg, probability = TRUE, cross = 10)
    svc_poly_cv[deg] <- svc_poly$tot.accuracy
}
svc_poly_degree <- which.max(svc_poly_cv)
svc_poly_cv <- rep(0, length(svc_cost))
for (i in 1:length(svc_poly_cv)) {
    set.seed(2017)
    svc_poly <- svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child, data = train, kernel = "polynomial", degree = svc_poly_degree, cost = svc_cost[i], probability = TRUE, cross = 10)
    svc_poly_cv[i] <- svc_poly$tot.accuracy
}
svc_poly <- svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child, data = train, kernel = "polynomial", degree = svc_poly_degree, cost = svc_cost[which.max(svc_poly_cv)], probability = TRUE, cross = 10)
svc_poly_prob <- predict(svc_poly, test, probability = TRUE)
svc_poly_prob <- attr(svc_poly_prob, "probabilities")[, 2]
svc_poly_pred <- data.frame(PassengerID = test$PassengerId, Survived = ifelse(svc_poly_prob > 0.5, 1, 0))
write.csv(svc_poly_pred, file = "svc_poly_pred.csv", row.names = FALSE)
set.seed(2017)
rf_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child, data = train)
rf_probabilities <- predict(rf_model, test, type = "prob")[, 2]
rf_prediction <- data.frame(PassengerID = test$PassengerId, Survived = ifelse(rf_probabilities > 0.5, 1, 0))
write.csv(rf_prediction, file = "rf_prediction.csv", row.names = FALSE)
