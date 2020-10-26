set.seed(300)
library(data.table)
library(rpart)
train <- fread("../input/train.csv", na.strings = c("NA", ""))
test <- fread("../input/test.csv", na.strings = c("NA", ""))
str(train)
str(test)
all <- rbind(train, test[, `:=`(Survived, 0)])
apply(all, 2, function(x) sum(is.na(x)))
all[is.na(Embarked), `:=`(Embarked, "S")]
all[is.na(Fare), `:=`(Fare, median(all[, Fare], na.rm = TRUE))]
all[, `:=`(FamSize, SibSp + Parch)]
all[, `:=`(Embarked, as.factor(Embarked))]
all[, `:=`(Sex, as.factor(Sex))]
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + FamSize, data = all[!is.na(Age), ], method = "anova")
all[is.na(Age), `:=`(Age, predict(predicted_age, all[is.na(Age), ]))]
apply(all, 2, function(x) sum(is.na(x)))
all[, `:=`(Sex, as.factor(Sex))]
all[, `:=`(Embarked, as.factor(Embarked))]
all[, `:=`(Cabin, NULL)]
train <- all[1:891, ]
test <- all[892:1309, ]
model <- glm(Survived ~ Pclass + Age + Sex, family = binomial(link = "logit"), data = train)
summary(model)
fitted.results <- predict(model, newdata = test[, .(Pclass, Age, Sex)], type = "response")
test[, `:=`(Survived, ifelse(fitted.results > 0.5, 1, 0))]
my_solution <- test[, .(PassengerId, Survived)]
write.csv(my_solution, row.names = FALSE, file = "my_solution.csv")
str(train)
str(test)
all <- rbind(train, test[, `:=`(Survived, 0)])
apply(all, 2, function(x) sum(is.na(x)))
all[is.na(Embarked), `:=`(Embarked, "S")]
all[is.na(Fare), `:=`(Fare, median(all[, Fare], na.rm = TRUE))]
all[, `:=`(FamSize, SibSp + Parch)]
all[, `:=`(Title, gsub(" ", "", strsplit(Name, split = "[,.]")[[1]][2])), by = PassengerId]
all[Title %in% c("Mme", "Mlle"), `:=`(Title, "Mlle")]
all[Title %in% c("Capt", "Don", "Major", "Sir"), `:=`(Title, "Sir")]
all[Title %in% c("Dona", "Lady", "the Countess", "Jonkheer"), `:=`(Title, "Lady")]
all[, `:=`(Title, as.factor(Title))]
all[, `:=`(Embarked, as.factor(Embarked))]
all[, `:=`(Sex, as.factor(Sex))]
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamSize, data = all[!is.na(Age), ], method = "anova")
all[is.na(Age), `:=`(Age, predict(predicted_age, all[is.na(Age), ]))]
apply(all, 2, function(x) sum(is.na(x)))
all[, `:=`(Sex, as.numeric(factor(Sex)))]
all[, `:=`(Title, as.numeric(factor(Title)))]
all[, `:=`(Embarked, as.numeric(factor(Embarked)))]
model <- glm(Survived ~ Pclass + Age + Sex, family = binomial(link = "logit"), data = train)
summary(model)
fitted.results <- predict(model, newdata = test[, .(Pclass, Age, Sex)], type = "response")
test[, `:=`(Survived, ifelse(fitted.results > 0.5, 1, 0))]
my_solution <- test[, .(PassengerId, Survived)]
write.csv(my_solution, row.names = FALSE, file = "my_solution.csv")
