# Infra and Data Setup
###################################################################
  rm(list=ls())
  
#  data_train = read.csv("../input/trainG.csv")
#  data_test = read.csv("../input/testG.csv")
  
  data_train = read.csv("../input/titanic-data-with-family-and-ticket-groups/trainG.csv")
  data_test = read.csv("../input/titanic-data-with-family-and-ticket-groups/testG.csv")
  
  # setwd("F:/Ram/Data Science/GitHub/Data-Science/R Programming/Kaggle/Titanic")
  # data_train = read.csv("train1.csv")
  # data_test = read.csv("test1.csv")
  library(caret)
  library(ggplot2)
  library(randomForest)
  library(splitstackshape)
    

# PreProcessing
###################################################################
  summary(data_train)
  str(data_train)

  # Removing Unnecessary variables
    
    #Training Data set    
      temp_train_PassengerId = data_train$PassengerId
      data_train$Survived = as.factor(data_train$Survived)
      data_train$Pclass = as.factor(data_train$Pclass)
      data_train$PassengerId = NULL
      data_train$Name = NULL
      data_train$SurName = NULL
      data_train$Ticket = NULL
      
      head(data_train)
      str(data_test)
    
    #Test Data set  
      temp_test_PassengerId = data_test$PassengerId#     
      data_test$Pclass = as.factor(data_test$Pclass)
      data_test$PassengerId = NULL
      data_test$Ticket = NULL
      data_test$Name = NULL
      data_test$SurName = NULL 
      
      head(data_test)
  
    #Impute Center and Scale
      model_impute = preProcess(data_train, method = c("knnImpute", "center", "scale"))

         #install.packages("RANN")
          library(RANN)
          data_train_pp = predict(model_impute, newdata = data_train)
          str(data_train_pp)
                          
      sum(is.na(data_train))
      sum(is.na(data_train_pp))

        data_train_pp_old = data_train_pp
        dmy = dummyVars(" ~ .", data = data_train_pp, fullRank = T)
        data_train_pp = data.frame(predict(dmy, newdata = data_train_pp))
        data_train_pp$Survived.1 = as.factor(data_train_pp$Survived.1)

      summary(data_train_pp)
      str(data_train)
      str(data_train_pp)
      head(data_train_pp)
      data_train_pp$Title.Col
        
  # Test Dataset
      
      #Impute Center and Scale
      model_impute_test = preProcess(data_test, method = c("knnImpute", "center", "scale"))
      
      # install.packages("RANN")
      # library(RANN)
      data_test_pp = predict(model_impute_test, newdata = data_test)
      
      sum(is.na(data_test))
      sum(is.na(data_test_pp))
      
      data_test_pp_old = data_test_pp
      dmy1 = dummyVars(" ~ .", data = data_test_pp, fullRank = T)
      data_test_pp = data.frame(predict(dmy1, newdata = data_test_pp))
      
      summary(data_test_pp)
      str(data_test)
      str(data_test_pp)

      trainCols = names(data_train_pp)
      testCols = names(data_test_pp)
      
      table(data_train_pp$Cabin.Category.T)
      table(data_train_pp$Embarked.C)
      table(data_train_pp$Title.Col)
      table(data_train_pp$Title.Don)
      table(data_train_pp$Title.Jonkheer)
      table(data_train_pp$Title.Lady)
      table(data_train_pp$Title.Major)
      table(data_train_pp$Title.Mlle)
      table(data_train_pp$Title.Mme)
      table(data_train_pp$Title.Sir)
      table(data_train_pp$Title.the.Countess)
      
      data_train_pp$Cabin.Category.T = NULL
      data_train_pp$Embarked.C = NULL
      data_train_pp$Title.Col = NULL
      data_train_pp$Title.Don = NULL
      data_train_pp$Title.Jonkheer = NULL
      data_train_pp$Title.Lady = NULL
      data_train_pp$Title.Major = NULL
      data_train_pp$Title.Mlle = NULL
      data_train_pp$Title.Mme = NULL
      data_train_pp$Title.Sir = NULL
      data_train_pp$Title.the.Countess = NULL
    
      trainCols = names(data_train_pp)
      testCols = names(data_test_pp)      
      
      head(data_train_pp)
      str(data_train_pp)
      
      data_train_pp$Pclass.2            = as.factor(data_train_pp$Pclass.2)
      data_train_pp$Pclass.3            = as.factor(data_train_pp$Pclass.3)
      data_train_pp$Sex.male            = as.factor(data_train_pp$Sex.male)
      data_train_pp$Embarked.Q          = as.factor(data_train_pp$Embarked.Q)
      data_train_pp$Embarked.S          = as.factor(data_train_pp$Embarked.S)
      data_train_pp$Cabin.Category.A    = as.factor(data_train_pp$Cabin.Category.A)
      data_train_pp$Cabin.Category.B    = as.factor(data_train_pp$Cabin.Category.B)
      data_train_pp$Cabin.Category.C    = as.factor(data_train_pp$Cabin.Category.C)
      data_train_pp$Cabin.Category.D    = as.factor(data_train_pp$Cabin.Category.D)
      data_train_pp$Cabin.Category.E    = as.factor(data_train_pp$Cabin.Category.E)
      data_train_pp$Cabin.Category.F    = as.factor(data_train_pp$Cabin.Category.F)
      data_train_pp$Cabin.Category.G    = as.factor(data_train_pp$Cabin.Category.G)
      data_train_pp$Title.Dr            = as.factor(data_train_pp$Title.Dr)
      data_train_pp$Title.Master        = as.factor(data_train_pp$Title.Master)
      data_train_pp$Title.Miss          = as.factor(data_train_pp$Title.Miss)
      data_train_pp$Title.Mr            = as.factor(data_train_pp$Title.Mr)
      data_train_pp$Title.Mrs           = as.factor(data_train_pp$Title.Mrs)
      data_train_pp$Title.Ms            = as.factor(data_train_pp$Title.Ms)
      data_train_pp$Title.Rev           = as.factor(data_train_pp$Title.Rev)
      data_train_pp$Ticket.Group.Ticket = as.factor(data_train_pp$Ticket.Group.Ticket)
      data_train_pp$Family.Group.Family = as.factor(data_train_pp$Family.Group.Family)

      data_test_pp$Pclass.2            = as.factor(data_test_pp$Pclass.2)
      data_test_pp$Pclass.3            = as.factor(data_test_pp$Pclass.3)
      data_test_pp$Sex.male            = as.factor(data_test_pp$Sex.male)
      data_test_pp$Embarked.Q          = as.factor(data_test_pp$Embarked.Q)
      data_test_pp$Embarked.S          = as.factor(data_test_pp$Embarked.S)
      data_test_pp$Cabin.Category.A    = as.factor(data_test_pp$Cabin.Category.A)
      data_test_pp$Cabin.Category.B    = as.factor(data_test_pp$Cabin.Category.B)
      data_test_pp$Cabin.Category.C    = as.factor(data_test_pp$Cabin.Category.C)
      data_test_pp$Cabin.Category.D    = as.factor(data_test_pp$Cabin.Category.D)
      data_test_pp$Cabin.Category.E    = as.factor(data_test_pp$Cabin.Category.E)
      data_test_pp$Cabin.Category.F    = as.factor(data_test_pp$Cabin.Category.F)
      data_test_pp$Cabin.Category.G    = as.factor(data_test_pp$Cabin.Category.G)
      data_test_pp$Title.Dr            = as.factor(data_test_pp$Title.Dr)
      data_test_pp$Title.Master        = as.factor(data_test_pp$Title.Master)
      data_test_pp$Title.Miss          = as.factor(data_test_pp$Title.Miss)
      data_test_pp$Title.Mr            = as.factor(data_test_pp$Title.Mr)
      data_test_pp$Title.Mrs           = as.factor(data_test_pp$Title.Mrs)
      data_test_pp$Title.Ms            = as.factor(data_test_pp$Title.Ms)
      data_test_pp$Title.Rev           = as.factor(data_test_pp$Title.Rev)
      data_test_pp$Ticket.Group.Ticket = as.factor(data_test_pp$Ticket.Group.Ticket)
      data_test_pp$Family.Group.Family = as.factor(data_test_pp$Family.Group.Family)   


# Split Data
###################################################################
  
  index = createDataPartition(data_train_pp$Survived, p = 0.8, list = FALSE)
  
  data_train_pp_train = data_train_pp[index,]
  data_train_pp_test = data_train_pp[-index,]

  str(data_train_pp_train)
  str(data_train_pp_test)

    # Feature Selection
    ###################################################################
      outcome = "Survived.1"
      predictors = names(data_train_pp_train)[!names(data_train_pp_train) %in% outcome]
#      control_fs = rfeControl(functions = rfFuncs, method = "repeatedCv", repeats = 5, verbose = FALSE)
      
      # install.packages("e1071")
      library(e1071)  
      # install.packages("randomForest")
      library(randomForest)
    
        nrow(data_train_pp_train[,predictors])
        nrow(data_train_pp_train[,outcome])
      
#      model_fs = rfe(data_train_pp_train[,c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "SurName", "Title")], data_train_pp_train[,"Survived"], rfeControl = control_fs)
#      model_fs = rfe(data_train_pp_train[,predictors], data_train_pp_train[,outcome], rfeControl = control_fs)
#      print(model_fs)
      
    #  predictors = c("Sex.male","Pclass.3","Fare","Age", "Title.Mr")

######################################################################################################################################
# Random Forest
######################################################################################################################################
#
    # Train Model & Parameter tuning
    ###################################################################
      model_rf = train(data_train_pp_train[,predictors], data_train_pp_train[,outcome],method = 'rf', tuneLength = 10)
      summary(model_rf)
      print(model_rf)
  
        # Variable Importance
        ###################################################################
          varImp(model_rf)
  
        # Prediction
        ###################################################################
            prediction = predict.train(object = model_rf, newdata = data_train_pp_test, type = "raw")  
            confMat_rf = confusionMatrix(prediction,data_train_pp_test$Survived)
            confMat_rf        

    # Train Model with Full Data
    ###################################################################
      model_rf_full = train(data_train_pp[,predictors], data_train_pp[,outcome],method = 'rf', tuneLength = 10)
      summary(model_rf_full)
      print(model_rf_full)

        # Variable Importance
        ###################################################################
          varImp(model_rf_full)
          
          str(data_train_pp)
          str(data_test_pp)

        # Prediction
        ###################################################################
          prediction = predict.train(object = model_rf_full, newdata = data_test_pp, type = "raw")  
          PassengerID = temp_test_PassengerId
          Survived_rf = prediction
          output_file = data.frame(PassengerID,Survived_rf)
          write.csv(output_file,"titanic_caret_rf.csv",row.names = FALSE)
  
######################################################################################################################################
# Logistic Regression
######################################################################################################################################

    # Train Model & Parameter tuning
    ###################################################################
      model_glm = train(data_train_pp_train[,predictors], data_train_pp_train[,outcome],method = 'glm', tuneLength = 10)
      summary(model_glm)
      print(model_glm)
  
        # Variable Importance
        ###################################################################
          varImp(model_glm)
  
        # Prediction
        ###################################################################
            prediction = predict.train(object = model_glm, newdata = data_train_pp_test, type = "raw")  
            confMat_glm = confusionMatrix(prediction,data_train_pp_test$Survived)
            confMat_glm
    
    modelLookup(model = "glm")

      # Train Model with Full Data
    ###################################################################
      model_glm_full = train(data_train_pp[,predictors], data_train_pp[,outcome],method = 'glm', tuneLength = 5)
      summary(model_glm_full)
      print(model_glm_full)

        # Variable Importance
        ###################################################################
           varImp(model_glm_full)

        # Prediction
        ###################################################################
          prediction = predict.train(object = model_glm_full, newdata = data_test_pp, type = "raw")  
          PassengerID = temp_test_PassengerId
          Survived_glm = prediction
          output_file = data.frame(PassengerID,Survived_glm)
          write.csv(output_file,"titanic_caret_glm.csv",row.names = FALSE)

######################################################################################################################################
# GBM 
######################################################################################################################################
    
    # Train Model & Parameter tuning
    ###################################################################
      model_gbm = train(data_train_pp_train[,predictors], data_train_pp_train[,outcome],method = 'gbm', tuneLength = 10)
      summary(model_gbm)
      print(model_gbm)
  
        # Variable Importance
        ###################################################################
          #varImp(model_gbm)
  
        # Prediction
        ###################################################################
            prediction = predict.train(object = model_gbm, newdata = data_train_pp_test, type = "raw")  
            confMat_gbm = confusionMatrix(prediction,data_train_pp_test$Survived)
            confMat_gbm
    
    modelLookup(model = "gbm")

      # Train Model with Full Data
    ###################################################################
      model_gbm_full = train(data_train_pp[,predictors], data_train_pp[,outcome],method = 'gbm', tuneLength = 5)
      summary(model_gbm_full)
      print(model_gbm_full)

        # Variable Importance
        ###################################################################
         # varImp(model_gbm_full)

        # Prediction
        ###################################################################
          prediction = predict.train(object = model_gbm_full, newdata = data_test_pp, type = "raw")  
          PassengerID = temp_test_PassengerId
          Survived_gbm = prediction
          output_file = data.frame(PassengerID,Survived_gbm)
          write.csv(output_file,"titanic_caret_gbm.csv",row.names = FALSE)
          
          
######################################################################################################################################
# NNET
######################################################################################################################################

    # Train Model & Parameter tuning
    ###################################################################
      model_nnet = train(data_train_pp_train[,predictors], data_train_pp_train[,outcome],method = 'nnet', tuneLength = 10)
      summary(model_nnet)
      print(model_nnet)
  
        # Variable Importance
        ###################################################################
          varImp(model_nnet)
  
        # Prediction
        ###################################################################
            prediction = predict.train(object = model_nnet, newdata = data_train_pp_test, type = "raw")  
            confMat_nnet = confusionMatrix(prediction,data_train_pp_test$Survived)
            confMat_nnet

    modelLookup(model = "nnet")

      # Train Model with Full Data
    ###################################################################
      model_nnet_full = train(data_train_pp[,predictors], data_train_pp[,outcome],method = 'nnet', tuneLength = 5)
      summary(model_nnet_full)
      print(model_nnet_full)

        # Variable Importance
        ###################################################################
         # varImp(model_gbm_full)

        # Prediction
        ###################################################################
          prediction = predict.train(object = model_nnet_full, newdata = data_test_pp, type = "raw")  
          PassengerID = temp_test_PassengerId
          Survived_nnet = prediction
          output_file = data.frame(PassengerID,Survived_nnet)
          write.csv(output_file,"titanic_caret_nnet.csv",row.names = FALSE)
          print("Done")
          
          paste("Accurance RF : ", confMat_rf)
          paste("Accurance RF : ", confMat_glm)
          paste("Accurance RF : ", confMat_gbm)
          paste("Accurance RF : ", confMat_nnet)
          
          
            output_file = data.frame(PassengerID, Survived_rf, Survived_glm, Survived_gbm, Survived_nnet)
            