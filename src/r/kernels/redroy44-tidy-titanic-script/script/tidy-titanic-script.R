library(tidyverse)
library(forcats)
library(stringr)
library(caret)
library(mice)
library(doMC)
set.seed(29082012)
file_train <- "../input/train.csv"
file_test <- "../input/test.csv"
train_data <- read_csv(file_train)
test_data <- read_csv(file_test)
full_data = bind_rows(train_data, test_data)
full_data %>% summarise_each(funs(sum(is.na(.)))) %>% print
full_data <- full_data %>% mutate(Sex = as.factor(Sex), Embarked = as.factor(Embarked)) %>% separate(Name, into = c("Surname", "FirstName"), sep = ",") %>% separate(FirstName, into = c("Title", "FirstName"), sep = "\\.", extra = "merge") %>% mutate(Title = as.factor(str_trim(Title)), Survived = as.factor(Survived)) %>% mutate(FamilySize = Parch + SibSp + 1, Pclass = as.factor(Pclass)) %>% mutate(Pclass = fct_recode(Pclass, first = "1", second = "2", third = "3")) %>% mutate(Survived = fct_recode(Survived, 
    no = "0", yes = "1")) %>% select(-FirstName, -Parch, -SibSp, -Cabin)
imputed_data <- complete(mice(select(full_data, -Survived)))
full_data$Age <- round(imputed_data$Age)
full_data$Fare <- imputed_data$Fare
full_data$Embarked <- imputed_data$Embarked
attr(full_data$Embarked, "contrasts") <- NULL
full_data %>% summarise_each(funs(sum(is.na(.)))) %>% print
full_data %>% select(Title) %>% group_by(Title) %>% summarize(count = n()) %>% print()
titles <- full_data %>% select(Title) %>% mutate(Title = fct_collapse(full_data$Title, Sir = c("Don", "Jonkheer", "Sir"), Lady = c("Dona", "Lady", "the Countess")))
full_data$Title <- titles$Title
sex_age <- full_data %>% select(Sex, Age)
sex_age <- sex_age %>% mutate(mfc = as.factor(ifelse(Age < 18, "child", as.character(Sex))))
levels(sex_age$mfc)
full_data <- full_data %>% mutate(mfc = sex_age$mfc)
family_unique <- full_data %>% select(Surname) %>% unique()
full_data <- full_data %>% mutate(group = as.factor(Surname), ticket_group = as.factor(Ticket)) %>% select(-Ticket, -Surname)
train_data <- full_data %>% filter(!is.na(Survived))
test_data <- full_data %>% filter(is.na(Survived)) %>% select(-Survived)
inTrain <- createDataPartition(train_data$Survived, p = 0.75, list = F)
training <- train_data %>% slice(inTrain)
testing <- train_data %>% slice(-inTrain)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, classProbs = TRUE)
trGrid <- expand.grid(.alpha = c(0.01, 0.05), .lambda = (1:10) * 0.1)
glmFit <- train(Survived ~ ., data = select(training, -PassengerId), method = "glmnet", trControl = fitControl, metric = "Accuracy", tuneGrid = trGrid)
glmFit
ggplot(glmFit)
predictions <- predict(glmFit, select(testing, -PassengerId))
confusionMatrix(predictions, testing$Survived)
model <- glmFit
Prediction <- predict(model, newdata = select(test_data, -PassengerId))
submit <- data.frame(PassengerId = test_data$PassengerId, Survived = ifelse(Prediction == "yes", 1, 0))
write.csv(submit, file = "mysubmission.csv", row.names = FALSE)
