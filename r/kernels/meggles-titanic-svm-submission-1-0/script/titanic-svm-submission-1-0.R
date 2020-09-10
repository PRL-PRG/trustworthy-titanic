# Load libraries
library("ggplot2") # data visualization
library("caret") # multiple model training/testing functions
library("readr") # CSV file I/O, e.g. the read_csv function
library("dplyr") # several Hadley Wickham data-wrangling packages
library("mice") # imputing missing values
library("stringr") # feature engineering
library("arules") # feature engineering
library("corrplot") # correlogram 
library("randomForest") # random forest model
library("e1071") # support vector machine model

options(scipen=999)

# Read in train and test data
train <- read_csv('../input/train.csv')
test <- read_csv('../input/test.csv')
test_ids <- test$PassengerId

# Preprocessing of train and test data
train <- mutate(train,
                Cabin_Deck = str_sub(Cabin,1,1),
                Ticket_Digit = nchar(Ticket),
                Ticket_Alpha = str_detect(Ticket, '[[:alpha:]]'),
                Family_Size = Parch+SibSp,
                Name_Family = gsub(",.*$", "", Name),
                Title = str_sub(Name, 
                                str_locate(Name, ",")[ , 1] + 2, 
                                str_locate(Name, "\\.")[ , 1] - 1)
               )
    # credit to https://www.kaggle.com/c/titanic/discussion/30733 for Title regex

    train_sub <- select(train,
                          Survived,Pclass,Sex,Age,SibSp,Parch,Fare,
                          Embarked,Cabin_Deck,Ticket_Digit,Ticket_Alpha,Name_Family,
                          Title,Family_Size)

    train_mm <- model.matrix(~Pclass+Sex+Age+SibSp+Parch+Fare+
                               Embarked+Cabin_Deck+Title+Family_Size+Ticket_Alpha,
                                          train_sub)

    train_imp <- mice(train_sub, 
                        m = 1,
                        method = "cart",
                        seed = 5,
                        printFlag=F)

    train <- complete(train_imp)

    train <- mutate(train, 
                    Cabin_Deck_i = ifelse(!is.na(Cabin_Deck),
                                    Cabin_Deck,
                                    ifelse(Pclass == 1,# read in the data
                                           'ABCD', 
                                            # not including T because only one passenger
                                            # in the training set was assigned cabin T
                                           ifelse(Pclass == 2,
                                                  'E',
                                                 'F'))))

    train_Pclass1 <- filter(train, Pclass == 1) 

    cuts <- discretize(train_Pclass1$Fare,
                       method = 'cluster',
                       categories = 4,
                       ordered = T,
                       onlycuts = T)

    train <- mutate(train, Cabin_Deck_i2 = ifelse(Cabin_Deck_i != "ABCD",
                                           Cabin_Deck_i,
                                           ifelse(Fare < cuts[2],
                                                 "D",
                                                 ifelse(Fare < cuts[3],
                                                       "C",
                                                       ifelse(Fare < cuts[4],
                                                             "B", 
                                                             "A")))))
    train <- mutate(train, Cabin_Deck_i3 = ifelse(Cabin_Deck_i2 == 'A',1,
                                    ifelse(Cabin_Deck_i2 == 'B',2,
                                          ifelse(Cabin_Deck_i2 == 'C',3,
                                                ifelse(Cabin_Deck_i2 == 'D',4,
                                                      ifelse(Cabin_Deck_i2 == 'E',5,
                                                            ifelse(Cabin_Deck_i2 == 'F',6,
                                                                  ifelse(Cabin_Deck_i2 == 'G',7,8))))))))
    train <- mutate(train, 
                    Embarked = ifelse(is.na(Embarked),
                                     'S', Embarked))

test <- mutate(test,
                Cabin_Deck = str_sub(Cabin,1,1),
                Ticket_Digit = nchar(Ticket),
                Ticket_Alpha = str_detect(Ticket, '[[:alpha:]]'),
                Family_Size = Parch+SibSp,
                Name_Family = gsub(",.*$", "", Name),
                Title = str_sub(Name, 
                                str_locate(Name, ",")[ , 1] + 2, 
                                str_locate(Name, "\\.")[ , 1] - 1)
               )
    # credit to https://www.kaggle.com/c/titanic/discussion/30733 for Title regex
    test_sub <- select(test,
                          Pclass,Sex,Age,SibSp,Parch,Fare,
                          Embarked,Cabin_Deck,Ticket_Digit,Ticket_Alpha,Name_Family,
                          Title,Family_Size)

    test_mm <- model.matrix(~Pclass+Sex+Age+SibSp+Parch+Fare+
                               Embarked+Cabin_Deck+Title+Family_Size+Ticket_Alpha,
                                          test_sub)

    test_imp <- mice(test_sub, 
                        m = 1,
                        method = "cart",
                        seed = 5,
                        printFlag=F)

    test <- complete(test_imp)
    test <- mutate(test, 
                    Cabin_Deck_i = ifelse(!is.na(Cabin_Deck),
                                    Cabin_Deck,
                                    ifelse(Pclass == 1,# read in the data
                                           'ABCD', 
                                            # not including T because only one passenger
                                            # in the testing set was assigned cabin T
                                           ifelse(Pclass == 2,
                                                  'E',
                                                 'F'))))

    test_Pclass1 <- filter(test, Pclass == 1) 

    cuts <- discretize(test_Pclass1$Fare,
                       method = 'cluster',
                       categories = 4,
                       ordered = T,
                       onlycuts = T)

    test <- mutate(test, Cabin_Deck_i2 = ifelse(Cabin_Deck_i != "ABCD",
                                           Cabin_Deck_i,
                                           ifelse(Fare < cuts[2],
                                                 "D",
                                                 ifelse(Fare < cuts[3],
                                                       "C",
                                                       ifelse(Fare < cuts[4],
                                                             "B", 
                                                             "A")))))
    test <- mutate(test, Cabin_Deck_i3 = ifelse(Cabin_Deck_i2 == 'A',1,
                                    ifelse(Cabin_Deck_i2 == 'B',2,
                                          ifelse(Cabin_Deck_i2 == 'C',3,
                                                ifelse(Cabin_Deck_i2 == 'D',4,
                                                      ifelse(Cabin_Deck_i2 == 'E',5,
                                                            ifelse(Cabin_Deck_i2 == 'F',6,
                                                                  ifelse(Cabin_Deck_i2 == 'G',7,8))))))))
    test <- mutate(test, 
                    Embarked = ifelse(is.na(Embarked),
                                     'S', Embarked))
test <- mutate(test,
                Cabin_Deck_i2 = 
                    ifelse(!(Cabin_Deck_i2 %in% as.character(train$Cabin_Deck_i2)),
                        "F",
                        Cabin_Deck_i2
                ),
                Title = 
                    ifelse(!(Title %in% as.character(train$Title)),
                        ifelse(Sex == "male",
                            "Mr.",
                            "Mrs."),
                        Title
                    ),
                Ticket_Digit = as.numeric(Ticket_Digit)
                )
                
# Model train on train set
train_svm <- select(train,Survived,
    Pclass,Sex,Age,SibSp,Parch,Fare,
    Embarked,Ticket_Digit,
    Ticket_Alpha, Family_Size) 
    
test_svm <- select(test,
    Pclass,Sex,Age,SibSp,Parch,Fare,
    Embarked,Ticket_Digit,
    Ticket_Alpha,Family_Size) 


svm_model <- svm(Survived~.,
           data = train_svm)

# Model predictions on test set
svm_pred <- predict(svm_model,
                   test_svm)

svm_pred_complete <- as.data.frame(cbind(test_ids, svm_pred)) 

svm_pred_complete <- svm_pred_complete %>%
    mutate(
        svm_pred = ifelse(svm_pred >= 0.5, 1, 0)
    )

mean(svm_pred_complete$svm_pred)

names(svm_pred_complete) <- c("PassengerId", "Survived")

write.csv(svm_pred_complete, 'titanic_pred.csv',
row.names = F)



# Model train on train set
train_rf <- train %>% select(Survived, Pclass, Age, SibSp,
Parch, Family_Size, Ticket_Digit, Fare)
rf_model <- randomForest(Survived~Pclass + Age + SibSp +
Parch + Family_Size + Ticket_Digit + Fare,
                      data=train_rf)
test_rf <- test %>% select(Pclass, Age, SibSp,
Parch, Family_Size, Ticket_Digit, Fare)
# Model predictions on test set
rf_pred <- predict(rf_model,
                   test_rf)
                   
rf_pred_complete <- as.data.frame(cbind(test_ids, rf_pred)) 

rf_pred_complete <-rf_pred_complete %>%
    mutate(
        rf_pred = ifelse(rf_pred >= 0.5, 1, 0)
    )

mean(rf_pred_complete$rf_pred)

names(rf_pred_complete) <- c("PassengerId", "Survived")

write.csv(rf_pred_complete, 'titanic_pred_rf.csv',
row.names = F)


library("xgboost")

# Model train on train set
xgb_train <- as.matrix(train_rf)
xgb_test <- as.matrix(test_rf)
xgb_model <- xgboost(data = xgb_train,
label = xgb_train[,1],
booster = "gblinear",
max_depth = 7,
eta = 0.1,
nthread = 2,
nrounds = 5,
objective = "binary:logistic")

# Model predictions on test set
xgb_pred <- predict(xgb_model,
                   xgb_test)
                   
xgb_pred_complete <- as.data.frame(cbind(test_ids, xgb_pred)) 

xgb_pred_complete <- xgb_pred_complete %>%
    mutate(
        xgb_pred = ifelse(xgb_pred >= 0.5, 1, 0)
    )

mean(xgb_pred_complete$xgb_pred)

names(xgb_pred_complete) <- c("PassengerId", "Survived")

write.csv(xgb_pred_complete, 'titanic_pred_xgb.csv',
row.names = F)