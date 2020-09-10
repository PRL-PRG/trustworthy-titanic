
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(caret)
library(dplyr)
library(stringr)
library(xgboost)
library(mice)
library(randomForest)
library(Amelia)
library(missForest)


# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")


# Examine structure of dataframe
str(train)

# Look at number of people who survived
table(train$Survived)
prop.table(table(train$Survived))

# Data wrangling

comb <- rbind(train, test %>% mutate(Survived = NA))
comb <- comb %>% mutate(title = ifelse(grepl('Mr. ',Name),'Mr',
                              ifelse(grepl('Mrs.',Name),'Mrs',
                              ifelse(grepl('Rev.',Name),'Rev',
                              ifelse(grepl('Col.',Name),'Col',
                              ifelse(grepl('Miss.',Name),'Miss',
                              ifelse(grepl('Dr.',Name),'Dr',
                              ifelse(grepl('Master',Name), 'Master','Nothing'))))))),
                        Ticket_ = ifelse(grepl('SC',Ticket),'SC',
                                       ifelse(grepl('PC',Ticket),'PC',
                                              ifelse(grepl('S.A.',Ticket),'S.A.',
                                                     ifelse(grepl('SOTON',Ticket), 'SOTON',
                                                            ifelse(grepl('A/5',Ticket),'A/5', 
                                                                   ifelse(grepl('PP',Ticket),'PP', 
                                                                          ifelse(grepl('W.C.',Ticket),'WC',
                                                                                 ifelse(grepl('C.A.',Ticket),'CA',substr(Ticket,1,1))))))))),                    
                        surname = word(Name,1, sep = ", "),
                        FamSize = SibSp + Parch + 1, 
                        Cabin_ = ifelse(substr(Cabin,1,1)=='','Z',substr(Cabin,1,1)),
                        Age_ = ntile(Age, 5))
comb <- comb %>% select(-SibSp , -Parch)

# write.csv(comb, 'combo.csv')
# some plots
ggplot(comb, aes(Survived), color = factor(Sex) ) + geom_bar() + facet_grid(Sex ~ .)
ggplot(comb, aes(Survived) ) + geom_bar() + facet_grid(Age_ ~ .)
ggplot(comb, aes(Survived) ) + geom_bar() + facet_grid(Embarked ~ .)
ggplot(comb, aes(Survived) ) + geom_bar() + facet_grid(Ticket_ ~ .)

# inpute age
age_ave <- comb %>% group_by(title) %>% summarize(ave_age = as.integer(median(Age, na.rm=TRUE)))
comb <- comb %>% left_join(age_ave) %>% mutate(Age = ifelse(is.na(Age), ave_age, Age )) %>% select(-ave_age)

# train and validation

comb_ <- comb  %>% mutate(Pclass = as.numeric(Pclass)-1,
                         Sex = as.numeric(Sex)-1, Age, Fare,
                         Cabin = as.numeric(Cabin)-1,
                         Embarked = as.numeric(Embarked)-1,
                         title = as.numeric(as.factor(title))-1,
                         Cabin_ = as.numeric(as.factor(Cabin_))-1,
                         Ticket_ = as.numeric(Ticket)-1,
                         surname = as.numeric(as.factor(surname))-1) %>%
  select(-PassengerId, -Ticket, -Age_, -Name, -Cabin)

train_ <- comb_ %>% filter(!is.na(Survived))  %>% mutate(Survived = as.numeric(Survived))
test_ <- comb_ %>% filter(is.na(Survived)) 

train_t <- sample_frac(train_, 0.8)
train_v <- train_ %>% anti_join(train_t)

# XGBOOST

train_m <- as.matrix(train_t %>% select(-Survived))
train_m_l <- as.matrix(train_t %>% select(Survived))
val_m <- as.matrix(train_v %>% select(-Survived))
val_m_l <- as.matrix(train_v %>% select(Survived))
test_m <- as.matrix(test_ %>% select(-Survived))

dtrain <- xgb.DMatrix(data = train_m, label= train_m_l, missing=NA)
dval <- xgb.DMatrix(data = as.matrix(train_v %>% select(-Survived)), label=as.matrix(train_v %>% select(Survived)), missing=NA)
dtest <- xgb.DMatrix(data = as.matrix(test_ %>% select(-Survived)), label=as.matrix(test_ %>% select(Survived)), missing=NA)

param <- list("objective" = "binary:logistic")
nround  = 20

watchlist <- list(val = dval, train = dtrain)
bst <- xgb.train(param, dtrain, nthread = 12, nround = 20, watchlist, metric="error")
names <- dimnames(train_m)[[2]]
importance_matrix <- xgb.importance(names, model = bst)
ggplot(data = as.data.frame(importance_matrix), aes(x= Feature, y= Gain)) + geom_bar(stat = "identity")

pred_bst_test <- predict(bst, test_m, missing = NA)
predict <- as.data.frame(pred_bst_test) %>% mutate(Survived = ifelse(pred_bst_test>0.5,1,0)) %>% select(-pred_bst_test)
#predict <- cbind(predict, over_name_tst) %>% mutate(Survived = ifelse(is.na(ss),Survived,ss)) %>% select(Survived)
predict <- cbind(test$PassengerId, predict)
colnames(predict) <- c('PassengerId','Survived')

write.csv(predict, 'bst_Titanic3.csv')

# Prepare the file

comb1_ <-  comb %>% mutate(Pclass = factor(Pclass),
                           title = factor(title),
                           Ticket_ = factor(Ticket_),
                           surname = factor(surname),
                           Cabin_ = factor(Cabin_))

set.seed(144)
vars.for.imputation = setdiff(names(comb1_), c("PassengerId", "Survived",  "Age_", "Name", "Ticket", "Cabin", "surname"))
comb1_.imp <- missForest(comb1_[vars.for.imputation], verbose = TRUE)
comb1_[vars.for.imputation] <- comb1_.imp$ximp

train1_ <- comb1_ %>% filter(!is.na(Survived))  %>% mutate(Survived = as.numeric(Survived))
test1_ <- comb1_ %>% filter(is.na(Survived)) 

train1_t <- sample_frac(train1_, 0.8)
train1_v <- train1_ %>% anti_join(train1_t)

# Random Forest
set.seed(123)
library('randomForest')

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + FamSize + Fare +
                      Embarked + title + Ticket_,
                    data=train1_t, 
                    importance=TRUE, 
                    ntree=3000)
val_pred <- as.data.frame(predict(fit, train1_v))
val_comp <- cbind(pred = val_pred, Survived = train1_v$Survived)
colnames(val_comp) <-c('pred','act')
val_comp %>% mutate(correct = ifelse(pred==act,1,0)) %>% summarize(ratio = sum(correct)/n())

varImpPlot(fit)

Prediction <- predict(fit, test1_)
submit <- cbind(test$PassengerId,  Prediction)
write.csv(submit, 'rf_Titanic5.csv', row.names=FALSE)


