
suppressMessages(library(data.table))
suppressMessages(library(DMwR))
suppressMessages(library(dplyr))
suppressMessages(library(rpart))
suppressMessages(library(purrr))
suppressMessages(library(rpart.plot))
suppressMessages(library(caret))
suppressMessages(library(DataExplorer))
suppressMessages(library(funModeling))

train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")

str(train)

table(train$Survived)

#combining dataset to impute missing values
test$Survived<- "X"
total <- rbind(train,test)
head(total)

#Missing values
map_df(total, function(x) sum(is.na(x)))
map_df(total, function(x) {sum(is.na(x))/length(x)*100})
df_status(total)
       
#This function does not tells which records have empty values. Only describes NA's or zero values.

summary(total)

total[which(is.na(total$Fare)),]
total %>% group_by(Embarked) %>% summarise(mean(Fare, na.rm=T), median(Fare, na.rm=T))

#Replacing the fare with the median
total$Fare <- replace(total$Fare, which(is.na(total$Fare)), 13)

#Locating empty records
total[which(total$Embarked == ""),]

#Let us determine which other passengers had same ticket number 

total[which(total$Ticket == 113572),]

#Well, let's see if we can estimate that which passengers have same fare
total[which(total$Fare==80),]

#We can either use imputation or replace empty values with most frequent. Since there are just 2 records, we are going to replace with the maximum frequency obervation.
table(total$Embarked)

total$Embarked <- replace(total$Embarked, which(total$Embarked== ""), "S")

total[which(total$Fare==0),]


total$Embarked <- droplevels(total$Embarked)

str(total)

data_knn<- knnImputation(total[, !names(total) %in% c("Survived", "Cabin")])

data_knn <- cbind(data_knn, total$Cabin, total$Survived)

names(data_knn)[11] <- "Cabin"
names(data_knn)[12] <- "Survived"
summary(data_knn)

data_rpart<- total

control <- trainControl(method="repeatedcv", number=7, repeats=7) 

imp_rpart <- train(Age ~. -Survived -Cabin, data =data_rpart[!is.na(data_rpart$Age),],
                    trControl = control, method="rpart", na.action = na.pass, tuneLength = 5, metric = "RMSE");
print(imp_rpart$results)

age_missing<- which(is.na(data_rpart$Age))

data_rpart$Age[age_missing] <- predict(imp_rpart, data_rpart[age_missing,])

data_rpart$Survived <- as.factor(data_rpart$Survived)

#let's visually analyze the model decision
rpart.plot(imp_rpart$finalModel, type=4, box.palette = "Greens")

library(forcats)
library(corrplot)

total[c(1:891),] %>% select(Age, Fare, Sex, Pclass, Ticket) %>% 
                    mutate(Sex = fct_recode(Sex,"1" = "male", "2" = "female")) %>%
                    mutate(Sex = as.integer(Sex), Pclass = as.integer(Pclass),Ticket=as.integer(Ticket)) %>% 
                    cor(use="complete.obs") %>% corrplot(type="lower", diag=F, method="square", addCoef.col = "black")

data_knn$Embarked <- droplevels(data_knn$Embarked)
data_rpart$Embarked <- droplevels(data_rpart$Embarked)

set.seed(213)
breaks<- sample(2, nrow(data_knn[c(1:891),]), replace =T, prob = c(0.8,0.2))
train_knn <- data_knn[c(1:891),][breaks==1,]
test_knn <- data_knn[c(1:891),][breaks==2,]

train_knn$Survived <- droplevels(train_knn$Survived)
test_knn$Survived <- droplevels(test_knn$Survived)

set.seed(782)
breaks<- sample(2, nrow(data_rpart[c(1:891),]), replace =T, prob = c(0.8,0.2))
train_rpart <- data_rpart[c(1:891),][breaks==1,]
test_rpart <- data_rpart[c(1:891),][breaks==2,]

train_rpart$Survived <- droplevels(train_rpart$Survived)
test_rpart$Survived <- droplevels(test_rpart$Survived)

library(Boruta)

set.seed(554)
boruta_knn <- Boruta(Survived ~., data=train_knn, doTrace=1, maxRuns=100)

#Result: All vars are important, except PassengerID

set.seed(435)
boruta_rpart <- Boruta(Survived ~., data=train_rpart, doTrace=1, maxRuns=100)


par(mfrow=c(2,2))
plot(boruta_knn, las=2, cex.axis=0.6)
plotImpHistory(boruta_knn)
plot(boruta_rpart, las=2, cex.axis=0.6)
plotImpHistory(boruta_rpart)

#Result: All vars are important

#Model Building for KNN training dataset, excluding PassengerID, Cabin, Name and Ticket variable.

set.seed(198)
model_knn<-train(Survived~. -PassengerId -Name -Cabin -Ticket,
                 data=train_knn, method="knn",
                 trControl=trainControl(method = "repeatedcv", number =10, repeats =7), metric = "Accuracy")

model_rf <- train(Survived~. -PassengerId -Name -Cabin -Ticket,
                 data=train_knn,method="ranger",
                  trControl=trainControl(method = "repeatedcv", number =10, repeats =7), 
                  metric = "Accuracy")

model_glmnet <- train(Survived~. -PassengerId -Name -Cabin -Ticket,
                 data=train_knn,method="glmnet",trControl=trainControl(method = "repeatedcv", 
                                                                   number =10, repeats =7), metric = "Accuracy")


result<-resamples(list(kNN=model_knn, RandomForest=model_rf, glm= model_glmnet))

dotplot(result)
print(model_knn)
print(model_rf)

knn_predict <- predict(model_knn, test_knn)
rf_predict <- predict(model_rf, test_knn)
glmnet_predict <- predict(model_glmnet, test_knn)

confusionMatrix(knn_predict, test_knn$Survived)
confusionMatrix(rf_predict, test_knn$Survived)
confusionMatrix(glmnet_predict, test_knn$Survived)

library(caTools)

knn_predict_eval <- predict(model_knn, test_knn, type ="prob")
glmnet_predict_eval <- predict(model_glmnet, test_knn, type ="prob")

colAUC(knn_predict_eval, test_knn$Survived, plotROC= T)
colAUC(glmnet_predict_eval, test_knn$Survived, plotROC= T)

#Model Building for rpart training dataset, excluding PassengerID, Cabin, Name and Ticket variable.

set.seed(444)
model_knn_r<-train(Survived~. -PassengerId -Name -Cabin -Ticket,
                 data=train_rpart, method="knn",
                 trControl=trainControl(method = "repeatedcv", number =10, repeats =7), metric = "Accuracy")

model_rf_r <- train(Survived~. -PassengerId -Name -Cabin -Ticket,
                 data=train_rpart,method="ranger",
                  trControl=trainControl(method = "repeatedcv", number =10, repeats =7), 
                  metric = "Accuracy")

model_glmnet_r <- train(Survived~. -PassengerId -Name -Cabin -Ticket,
                 data=train_rpart,method="glmnet",trControl=trainControl(method = "repeatedcv", 
                                                                   number =10, repeats =7), metric = "Accuracy")


result_b<-resamples(list(kNN=model_knn_r, RandomForest=model_rf_r, glm= model_glmnet_r))

dotplot(result_b)
print(model_knn_r)
print(model_rf_r)
print(model_glmnet_r)

knn_predict_b <- predict(model_knn_r, test_rpart)
rf_predict_b <- predict(model_rf_r, test_rpart)
glmnet_predict_b <- predict(model_glmnet_r, test_rpart)

confusionMatrix(knn_predict_b, test_rpart$Survived)
confusionMatrix(rf_predict_b, test_rpart$Survived)
confusionMatrix(glmnet_predict_b, test_rpart$Survived)
