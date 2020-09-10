## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(stringr)
library(forcats)
library(caret)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test <- read.csv('../input/test.csv', stringsAsFactors = F)

ix_train <- seq(1, nrow(train))
ix_test <- seq((tail(ix_train,n=1)+1), (nrow(train)+nrow(test)))
y_train <- train$Survived
train$Survived <- NULL

full_data <- rbind(train,test)

full_data$Cabin <- NULL
full_data$Ticket <- NULL
full_data$Pclass <- as.factor(full_data$Pclass)
full_data$Sex <- as.factor(full_data$Sex)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(full_data)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
apply(sapply(full_data, is.na),2,sum)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
get_mode <- function(v){
  tab <- table(v)
  out <- tab[tab == max(tab)]
  return(as.numeric(names(out)))
}

full_data %>% mutate(Age = round(Age)) -> full_data

full_data %>% ggplot(aes(Age)) + geom_density() + geom_vline(aes(xintercept = get_mode(Age)), linetype="22") + labs(title="Age distribution", subtitle="Dashed line indicates mode (24)") + theme_bw()
full_data %>% replace_na(list(Age=get_mode(.$Age))) -> full_data


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_data %>% filter(is.na(.$Fare))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_data %>% filter(!is.na(Fare)) %>% group_by(Embarked, Pclass) %>% summarise(Median_fare=median(Fare))
full_data %>% replace_na(list(Fare=8.05)) -> full_data


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_data %>% filter(Embarked=="")
full_data$Embarked[full_data$Embarked==""] <- "C"


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_data$Title <- str_split(str_split(full_data$Name, ".*, ", simplify = T)[,2], ". .*", simplify = T)[,1]
full_data %>% select(Title) %>% table
full_data$Title[full_data$Title=="Ms"] <- "Miss"
full_data %>% mutate(Title=fct_lump(.$Title, 4)) -> full_data
full_data$Name <- NULL
full_data %>% ggplot(aes(Title)) + geom_histogram(stat="count") + theme_bw() + labs(title="Histogram of Title")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_data %>% mutate(Fsize=SibSp+Parch+1) -> full_data
full_data %>% ggplot(aes(Fsize)) + geom_histogram(bins=15) + theme_bw() + labs(title="Histogram of family Sizes")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
x_train <- full_data[ix_train,]
x_test <- full_data[ix_test,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

model_data <- x_train
model_data$Class <- as.factor(y_train)
model_data$Class <- fct_recode(model_data$Class, Survived="1", Dead="0")
model_data$PassengerId <- NULL


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_data %>% ggplot(aes(x=Sex, fill=Class)) + geom_histogram(stat="count",position=position_dodge()) + theme_bw() + labs(title="Survival by Sex")
model_data %>% ggplot(aes(x=Fsize, fill=Class)) + geom_histogram(stat="count",position=position_dodge()) + theme_bw() + labs(title="Survival by Family Size")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ind <- createDataPartition(y_train, p=2/3, list=F)
model_data_train <- model_data[ind,]
model_data_val <- model_data[-ind,]

ctr <- trainControl(method="cv", number=5, savePredictions = T, classProbs = T)
parameters <- expand.grid(eta=0.1, colsample_bytree=c(0.3,0.5, 0.7), max_depth=c(3,6,9,12), nrounds=100, gamma=1, min_child_weight=2, subsample=c(0.3, 0.5,1))

xgb <- train(Class~., data=model_data_train, method="xgbTree", trControl=ctr, tuneGrid=parameters)


xgb$results %>% filter(Accuracy==max(.$Accuracy)) #Output model parameters with max Accuracy


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred <- predict(xgb, model_data_val)
table(Actual=model_data_val$Class, Predicted=pred)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
xgb_full <- train(Class~., data=model_data, trControl=ctr, tuneGrid=parameters, method="xgbTree")
xgb_full$results %>% filter(Accuracy == max(.$Accuracy))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
xgb_full$results %>% ggplot(aes(x=as.factor(subsample), y=Accuracy)) + geom_point() + theme_bw()
xgb_full$results %>% ggplot(aes(x=as.factor(max_depth), y=Accuracy)) + geom_point() + theme_bw()
xgb_full$results %>% ggplot(aes(x=as.factor(colsample_bytree), y=Accuracy)) + geom_point() + theme_bw()



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
x_test %>% select(-PassengerId) -> x_test_final
test_predictions <- predict(xgb_full, x_test_final)
submission_df <- x_test
submission_df$Survived <- fct_recode(test_predictions, "0"="Dead", "1"="Survived")
submission_df %>% select(PassengerId, Survived) -> submission_df
write.csv(submission_df, "solution.csv", row.names = F)

