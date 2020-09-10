## ---- include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE, collapse = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2) # visualization
library(rms) # model evaluation
library(MASS) # data modelling
library(class) # data modelling
library(e1071) # data modelling
library(dummies) # data modelling
library(randomForest) # data modelling
library(ROCR) # model evaluation


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test$Survived <- NA
c_data <- rbind(train,test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(c_data)


## ----echo=FALSE, results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(knitr)
kable(c_data[is.na(c_data$Fare),])


## ---- include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
c_data$Ticket <- as.character(c_data$Ticket)


## ----echo=FALSE, results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(knitr)
kable(c_data[c_data$Ticket == "3701",])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
s_p3_solo <- c_data[c_data$Pclass == 3 & c_data$Embarked == "S" & c_data$SibSp == 0 & c_data$Parch == 0,]
s_p3_solo_clean <- s_p3_solo[!is.na(s_p3_solo$Fare),] # $7.90
c_data$Fare[1044] <- median(s_p3_solo_clean$Fare)


## ----echo=FALSE, results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(knitr)
kable(c_data[c_data$Embarked == "",])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
embark <- c_data[c_data$Embarked != "",]

ggplot(embark, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  theme_bw()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
c_data$Embarked[c(62,830)] <- 'C'


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
c_data$Title <- gsub('(.*, )|(\\..*)','',c_data$Name)
unique(c_data$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
c_data$Title[c_data$Title == "Mme"] <- "Mrs"
c_data$Title[c_data$Title == "Mlle"] <- "Miss"
c_data$Title[c_data$Title == "Ms"] <- "Miss"

other_list <- unique(c_data$Title)
other_list <- other_list[5:15]
c_data$Title[c_data$Title %in% other_list] <- "Other"


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# family size variable
c_data$nFamily <- c_data$SibSp + c_data$Parch + 1

# shared ticket variable
c_data$sharedTix <- ifelse(c_data$Ticket %in% c_data$Ticket[duplicated(c_data$Ticket)], 1, 0)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Mother variable
c_data$mother <- ifelse(c_data$Parch > 0 & c_data$Title == "Mrs",1,0)

# Child variable using Title as a proxy when Age is missing
c_data$child <- ifelse(c_data$Age > 18 & !is.na(c_data$Age) | c_data$Title %in% c("Other", "Mr", "Mrs"),0,1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_new <- c_data[1:891,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train_new, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(min(train_new$Pclass):max(train_new$Pclass))) +
  labs(x = 'P class') +
  theme_bw()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train_new, aes(x = nFamily, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(min(train_new$nFamily):max(train_new$nFamily))) +
  labs(x = 'Family Size') +
  theme_bw()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(train_new$sharedTix, train_new$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
c_data$travelParty <- ifelse(c_data$sharedTix == 0 & c_data$nFamily == 1, "Single",
                             ifelse(c_data$sharedTix == 1 & c_data$nFamily == 1 |   c_data$nFamily < 5,"Small","Large"))

train_new <- c_data[1:891,]

ggplot(train_new, aes(x = travelParty, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Travel Party Size') +
  theme_bw()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fact_var <- c('Survived','Pclass', 'Sex', 'SibSp', 'Parch', 'Embarked', 'Title', 'nFamily', 'travelParty')

train_new[fact_var] <- lapply(train_new[fact_var], function(x) as.factor(x))

all_vars <- c('Pclass', 'Sex','SibSp', 'Parch','Fare','Embarked','sharedTix', 'Title', 
              'nFamily','travelParty','mother', 'child')

n_train <- round((nrow(train_new)*2)/3,0)

set.seed(1912)

model_train <- sample(1:nrow(train_new), n_train)

cv <- train_new[-model_train,]


## ---- results='hide'-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fm_all <- as.formula(paste("Survived ~ ", paste(all_vars, collapse= "+")))
logit_model_all <- glm(fm_all, data = train_new, family = binomial, subset = model_train)
step_fit <- step(logit_model_all)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
vars_low <- c('Pclass', 'Sex', 'Fare', 'Embarked', 'Title', 'travelParty')
fm_low <- as.formula(paste("Survived ~ ", paste(vars_low, collapse= "+")))
logit_model_low <- glm(fm_low, data = train_new, family = binomial, subset = model_train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Preform one hot encoding
onehot_var <- c('Pclass', 'Sex', 'Embarked', 'Title', 'travelParty')
onehot_encode <- dummy.data.frame(train_new, names = onehot_var, sep="_")

# Scale fare variable
onehot_encode$Fare <- scale(onehot_encode$Fare)

# Take out extra variables
delete <- c("Survived","PassengerId", "Name", "Age", "Ticket", "Cabin")
onehot_encode_preds <- onehot_encode[,!(names(onehot_encode) %in% delete)]
low_preds <- c(names(onehot_encode_preds)[grepl("Psize",names(onehot_encode_preds))], 
               names(onehot_encode_preds)[grepl("Sex",names(onehot_encode_preds))],
               names(onehot_encode_preds)[grepl("Embarked",names(onehot_encode_preds))], 
               names(onehot_encode_preds)[grepl("Title",names(onehot_encode_preds))],
               names(onehot_encode_preds)[grepl("travelParty",names(onehot_encode_preds))], "Fare")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Define KNN class labels and predictors
knn_class_train <- onehot_encode[,2][model_train]
knn_class_cv <- onehot_encode[,2][-model_train]
knn_preds_train <- onehot_encode_preds[model_train,low_preds]
knn_preds_cv <- onehot_encode_preds[-model_train,low_preds]

set.seed(1912)
knn_model <- knn(knn_preds_train,knn_preds_cv,knn_class_train, k=12)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
svm_train <- data.frame(cbind(knn_class_train, knn_preds_train))
svm_cv <- data.frame(cbind(knn_class_cv, knn_preds_cv))

colnames(svm_train)[1] <- "Survived" 
colnames(svm_cv)[1] <- "Survived"

set.seed(1912)
svm_low_fm <- as.formula(paste("Survived ~ ", paste(low_preds, collapse= "+")))
svm_low <- tune(svm, svm_low_fm, data = svm_train, kernel="radial",
                ranges=list(cost=c(0.1,1,10,100,1000),
                            gamma=c(0.5,1,2,3,4)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1912)
rf_model <- randomForest(fm_low, data=train_new, subset=model_train, importance=TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logit_model_low_probs <- predict(logit_model_low, cv, type = "response")

logit_modellow_preds <- rep(0,297)
logit_modellow_preds[logit_model_low_probs>.5] <- 1

logit_tp <- mean(logit_modellow_preds ==cv$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knn_tp <- mean(knn_class_cv==knn_model)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
svm_low_preds <- predict(svm_low$best.model, newdata = svm_cv)
svm_tp <- mean(svm_low_preds ==svm_cv$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rf_pred <- predict(rf_model,newdata=cv,type="class")
rf_tp <- mean(rf_pred == cv$Survived)


## ----echo=FALSE, results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(knitr)
tp_rates <- data.frame(c("Logit", "KNN", "SVM", "RF"))
tp_rates$Accuracy <- c(logit_tp, knn_tp, svm_tp, rf_tp)
colnames(tp_rates)[1] <- "Model"
tp_rates <- tp_rates[order(tp_rates$Accuracy, decreasing = TRUE),]
kable(tp_rates)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logitpred <- prediction(logit_modellow_preds, cv$Survived)
logitperf <- performance(logitpred, "tnr", "fnr")

svmpred <- prediction(as.numeric(svm_low_preds)-1, cv$Survived)
svmperf <- performance(svmpred, "tnr", "fnr")

rfpred <- prediction(as.numeric(rf_pred)-1, cv$Survived)
rfperf <- performance(rfpred, "tnr", "fnr")

plot(logitperf,col="green",lwd=2,main="ROC Curve for Titanic Survival Models")
plot(svmperf, add = TRUE, col="blue")
plot(rfperf, add=TRUE, col="red")
abline(a=0,b=1,lwd=2,lty=2,col="gray")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_feat <- c_data[c(892:1309),]
test_feat[fact_var] <- lapply(test_feat[fact_var], function(x) as.factor(x))

set.seed(1912)
rf_final <- randomForest(fm_low, data=train_new)
rf_final_pred <- predict(rf_final,test_feat)
rf_solution <- data.frame(PassengerID = test_feat$PassengerId, Survived = rf_final_pred)

