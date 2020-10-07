library(tidyverse)
train = read.csv("../input/train.csv", stringsAsFactors = FALSE, header = TRUE)
test = read.csv("../input/test.csv", stringsAsFactors = FALSE, header = TRUE)
train$IsTrain = TRUE
test$IsTrain = FALSE
test$Survived = NA
full = rbind(train, test)
full = full[!(full$Embarked == ""), ]
fare_mean = summary(full$Fare)[4]
full[is.na(full$Fare), ]$Fare = fare_mean
age_outlier_filter = full$Age < boxplot.stats(full$Age)$stats[5]
age_model = lm(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = full[age_outlier_filter, ])
age_missing = full[is.na(full$Age), c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Embarked")]
age_pred = predict(age_model, newdata = age_missing)
full[is.na(full$Age), "Age"] = age_pred
full$Pclass = as.factor(full$Pclass)
full$Sex = as.factor(full$Sex)
full$Embarked = as.factor(full$Embarked)
train = full[full$IsTrain == TRUE, ]
test = full[full$IsTrain == FALSE, ]
logit_model_null = glm(Survived ~ 1, data = train, family = binomial)
summary(logit_model_null)
logit_model_prime = glm(Survived ~ . - (Name + Ticket + Cabin), data = train, family = binomial)
summary(logit_model_prime)
logit_model = glm(Survived ~ Pclass + Sex + Age + SibSp, data = train, family = binomial)
summary(logit_model)
anova(logit_model_null, logit_model_prime, logit_model, test = "Chisq")
logit_prob = predict(logit_model, test, type = "response")
logit_pred = rep(0, dim(test)[1])
plot(logit_prob)
logit_pred[logit_prob > 0.5] = 1
PassengerId = test$PassengerId
output_df = as.data.frame(PassengerId)
output_df$Survived = logit_pred
write.csv(output_df, file = "titanic_logit_submission.csv", row.names = FALSE)
