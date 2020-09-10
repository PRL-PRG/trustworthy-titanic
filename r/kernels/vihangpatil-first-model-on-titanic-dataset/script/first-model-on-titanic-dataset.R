## ---- include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Multiple plot function 
# from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# 
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifing the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(caret)
library(caretEnsemble)


## ---- message = FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test             <- read.csv("../input/test.csv",  stringsAsFactors = F, header = T, na.strings = c('NA',''))
train_valid_data <- read.csv("../input/train.csv", stringsAsFactors = F, header = T, na.strings = c('NA',''))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(train_valid_data)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_valid_data$PassengerId <- as.character(train_valid_data$PassengerId)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_valid_data$Survived <- as.factor(ifelse(train_valid_data$Survived == 1, 'Survived', 'Perished'))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(prop.table(table(train_valid_data$Survived)),2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(train_valid_data$Ticket, 15)
train_valid_data$Ticket <- NULL


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_valid_data$Pclass <- as.factor(train_valid_data$Pclass)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(prop.table(table(train_valid_data$Survived, train_valid_data$Pclass), margin = 2),2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_valid_data$Cabin <- substr(train_valid_data$Cabin,0,1)
train_valid_data$Cabin <- ifelse(is.na(train_valid_data$Cabin), 'X', train_valid_data$Cabin)
train_valid_data$Cabin <- ifelse(train_valid_data$Cabin %in% c('A', 'B', 'C', 'D', 'E'), train_valid_data$Cabin, 'X')
train_valid_data$Cabin <- as.factor(train_valid_data$Cabin)

table(train_valid_data$Cabin, train_valid_data$Pclass)

table(train_valid_data[train_valid_data$Pclass=='1',]$Cabin, train_valid_data[train_valid_data$Pclass=='1',]$Survived)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train_valid_data, aes(x = Fare, fill = Pclass)) +
  geom_density(alpha = 0.5) +
  lims(x = c(0, 100))


## ---- message=FALSE, warning=FALSE, fig.height=10--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p1 = ggplot(train_valid_data, aes(x = Fare, fill = Survived)) + 
      geom_density(alpha = 0.5) +
      geom_vline(xintercept = c(15, 25, 50), size = 1.2, col = 'blue') +
      ggtitle("Distribution of survivors respect to the fare") +
      lims(x = c(0, 100))

p2 = ggplot(train_valid_data, aes(x = Fare, fill = Survived)) + 
      geom_density(alpha = 0.5, position = 'fill') +
      geom_vline(xintercept = c(15, 25, 50), size = 1.2, col = 'blue') +
      ggtitle("Proportion of survivors respect to the age") +
      lims(x = c(0, 100))

multiplot(p1, p2, cols=1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
if(anyNA(train_valid_data$Fare)) train_valid_data[is.na(train_valid_data$Fare),]$Fare <- mean(train_valid_data$Fare, na.rm = T)

train_valid_data$Fare_type <- as.factor(ifelse(train_valid_data$Fare <= 15, "Low", 
                                        ifelse(train_valid_data$Fare >  15 & train_valid_data$Fare <= 25, "Medium", 
                                        ifelse(train_valid_data$Fare >  25 & train_valid_data$Fare <= 50, "High", "Very_High"))))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(prop.table(table(train_valid_data$Survived, train_valid_data$Fare_type), margin = 2),2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_valid_data$Sex <- as.factor(train_valid_data$Sex)
round(prop.table(table(train_valid_data$Survived, train_valid_data$Sex), margin = 2),2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(train_valid_data$Name, 15)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_valid_data$Name_title <- as.factor(ifelse(grepl(pattern = " miss.", x = train_valid_data$Name, ignore.case = T), "miss",
                                ifelse(grepl(pattern = " mrs.", x = train_valid_data$Name, ignore.case = T), "mrs", 
                                ifelse(grepl(pattern = " mr.", x = train_valid_data$Name, ignore.case = T), "mr", 
                                ifelse(grepl(pattern = " master.", x = train_valid_data$Name, ignore.case = T), "master", 
                                ifelse((as.character.factor(train_valid_data$Sex)=='male') & (train_valid_data$Age>=13), "mr", 
                                ifelse((as.character.factor(train_valid_data$Sex)=='male') & (train_valid_data$Age<13), "master", "mrs")))))))

train_valid_data$Name_title[is.na(train_valid_data$Name_title)] <- ifelse((is.na(train_valid_data$Name_title)) & (as.character.factor(train_valid_data$Sex) == 'male'), 'mr', 'mrs')

train_valid_data$Name <- NULL

round(prop.table(table(train_valid_data$Name_title)),2)


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_valid_data$family_size <- train_valid_data$Parch + train_valid_data$SibSp + 1

train_valid_data$family_size_group <- as.factor(ifelse(train_valid_data$family_size == 1, "1",
                                          ifelse(train_valid_data$family_size >= 2 & train_valid_data$family_size < 4, "2_3",
                                          ifelse(train_valid_data$family_size >= 4, "4+", NA))))

train_valid_data$Parch_group <-  as.factor(ifelse(train_valid_data$Parch == 0, "0",
                                          ifelse(train_valid_data$Parch == 1, "1",
                                          ifelse(train_valid_data$Parch >= 2, "2+", NA))))

train_valid_data$SibSp_group <-  as.factor(ifelse(train_valid_data$SibSp == 0, "0",
                                          ifelse(train_valid_data$SibSp == 1, "1",
                                          ifelse(train_valid_data$SibSp >= 2, "2+", NA))))


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(train_valid_data$Age)


## ---- warning=FALSE, fig.height=10-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p1 <- ggplot(train_valid_data, aes(x = Age, fill = Survived)) + 
        geom_density(alpha = 0.5) +
        geom_vline(xintercept = c(13, 30, 50), size = 1.2, col = 'blue') +
        ggtitle("Distribution of survivors respect to the age") +
        lims(x = c(0, 80)) 

p2 <- ggplot(train_valid_data, aes(x = Age, fill = Survived)) + 
        geom_density(alpha = 0.5, position = "fill") +
        geom_vline(xintercept = c(13, 30, 50), size = 1.2, col = 'blue') +
        ggtitle("Proportion of survivors respect to the age") +
        lims(x = c(0, 80))

multiplot(p1, p2, cols=1)


## ----age_model, warning=FALSE, message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Age_model <- train(Age ~ Sex + Pclass + SibSp + Parch + Fare + Name_title,
                 data = train_valid_data[complete.cases(train_valid_data),], 
                 method = "rf")

train_valid_data[is.na(train_valid_data$Age),]$Age <- predict(object = Age_model, 
                                                              newdata = train_valid_data[is.na(train_valid_data$Age),])


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_valid_data$Age_group <- as.factor(ifelse(train_valid_data$Age > 0  & train_valid_data$Age <= 13,  "g0_13", 
                                        ifelse(train_valid_data$Age > 13 & train_valid_data$Age <= 30,  "g13_30", 
                                        ifelse(train_valid_data$Age > 30 & train_valid_data$Age <= 50,  "g30_50", 
                                        ifelse(train_valid_data$Age > 50, "g50_inf", NA)))))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(prop.table(table(train_valid_data$Survived, train_valid_data$Age_group), margin = 2),2)


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(prop.table(table(train_valid_data$Embarked)), 2)
if(anyNA(train_valid_data$Embarked)) train_valid_data[is.na(train_valid_data$Embarked),]$Embarked <- "S" 
train_valid_data$Embarked <- as.factor(train_valid_data$Embarked)


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_valid_data$Fare_family_div <- train_valid_data$Fare / train_valid_data$family_size
train_valid_data$Age_Fare_mult <- train_valid_data$Age * train_valid_data$Fare
train_valid_data$SibSp_Parch_mult <- train_valid_data$SibSp * train_valid_data$Parch


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_valid_data$Survived <- as.factor(ifelse(as.character.factor(train_valid_data$Survived) == 'Survived', '1', '0'))
str(train_valid_data)


## ---- warning = FALSE, message=FALSE, results="hide", echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------

test$PassengerId <- as.character(test$PassengerId)

test$Pclass      <- as.factor(test$Pclass)

test$Cabin <- substr(test$Cabin,0,1)
test$Cabin <- ifelse(is.na(test$Cabin), 'X', test$Cabin)
test$Cabin <- ifelse(test$Cabin %in% c('A', 'B', 'C', 'D', 'E'), test$Cabin, 'X')
test$Cabin <- as.factor(test$Cabin)

test$Sex         <- as.factor(test$Sex)

if(anyNA(test$Fare)) test[is.na(test$Fare),]$Fare <- mean(train_valid_data$Fare, na.rm = T)

test$Fare_type   <- as.factor(ifelse(test$Fare <= 15, "Low", 
                              ifelse(test$Fare >  15 & test$Fare <= 25, "Medium", 
                              ifelse(test$Fare >  25 & test$Fare <= 50, "High", 
                              "Very_High"))))

test$Name_title <- as.factor(ifelse(grepl(pattern = " miss.", x = test$Name, ignore.case = T), "miss",
                                ifelse(grepl(pattern = " mrs.", x = test$Name, ignore.case = T), "mrs", 
                                ifelse(grepl(pattern = " mr.", x = test$Name, ignore.case = T), "mr", 
                                ifelse(grepl(pattern = " master.", x = test$Name, ignore.case = T), "master", 
                                ifelse((as.character.factor(test$Sex)=='male') & (test$Age>=13), "mr", 
                                ifelse((as.character.factor(test$Sex)=='male') & (test$Age<13), "master", "mrs")))))))

test$Name_title[is.na(test$Name_title)] <- ifelse((is.na(test$Name_title)) & (as.character.factor(test$Sex) == 'male'), 'mr', 'mrs')

test$family_size <- test$Parch + test$SibSp + 1

test$family_size_group <- as.factor(ifelse(test$family_size == 1, "1", 
                              ifelse(test$family_size >= 2 & test$family_size < 4, "2_3", 
                              ifelse(test$family_size >= 4, "4+", NA))))

test$Parch_group <-  as.factor(ifelse(test$Parch == 0, "0",
                               ifelse(test$Parch == 1, "1",
                               ifelse(test$Parch >= 2, "2+", NA))))

test$SibSp_group <-  as.factor(ifelse(test$SibSp == 0, "0",
                               ifelse(test$SibSp == 1, "1",
                               ifelse(test$SibSp >= 2, "2+", NA))))
                                          
if(anyNA(test$Embarked)) test[is.na(test$Embarked),]$Embarked <- "S" 

test$Embarked <- as.factor(test$Embarked)
                          
test[is.na(test$Age),]$Age <- predict(object = Age_model, newdata = test[is.na(test$Age),])

test$Age_group <- as.factor(ifelse(test$Age > 0  & test$Age <= 13,  "g0_13", 
                            ifelse(test$Age > 13 & test$Age <= 30,  "g13_30", 
                            ifelse(test$Age > 30 & test$Age <= 50,  "g30_50", 
                            ifelse(test$Age > 50, "g50_inf", NA)))))
                            
test$Fare_family_div <- test$Fare / test$family_size
test$Age_Fare_mult <- test$Age * test$Fare
test$SibSp_Parch_mult <- test$SibSp * test$Parch



## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(88)
trainIndex  <- createDataPartition(train_valid_data$Survived, p = .6, list = FALSE, times = 1)
training    <- train_valid_data[ trainIndex,]
validation  <- train_valid_data[-trainIndex,]


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(prop.table(table(training$Survived)),2)  


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
training_list <- list(training_original   = training,
                      #training_upSample   = upSample(x = training[, -1], y = training$Survived, yname = "Survived"),
                      training_downSample = downSample(x = training[, -1], y = training$Survived, yname = "Survived"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
algorithms <- c("plr", "lda", "glm", "rpart", "C5.0", "rf")


## ----model_1, message=FALSE, warning = FALSE, results="hide"---------------------------------------------------------------------------------------------------------------------------------------------------------------------

results <- list()

modelFit_final <- NULL
best_accuracy  <- 0

for(i in seq(algorithms)){ 
  for(j in seq(training_list)){
    
    dataset <- training_list[[j]]
    
    set.seed(88)
    modelFit    <- train(Survived ~ Sex + Pclass + Name_title + Parch + SibSp + SibSp_Parch_mult + Age + Fare + Embarked,
                         #Survived ~ Sex + Pclass + Cabin + Name_title + family_size + family_size_group + 
                         #           SibSp + SibSp_group + Parch + Parch_group + Age + Age_group + 
                         #           Fare + Fare_type + Embarked + Fare_family_div + Age_Fare_mult + SibSp_Parch_mult,
                         data = dataset,
                         preProcess = c("center", "scale"),
                         trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3),
                         tuneLength = 3,
                         method = algorithms[i])
    
    validation$my_prediction <- predict(object = modelFit, newdata = validation, type = "raw")
    validation$result        <- ifelse(validation$my_prediction == validation$Survived, 1, 0)
    accuracy                  <- round(mean(validation$result),3)
    dataname                  <- names(training_list)[j]
    results                   <- c(results, list(c(modelFit$modelInfo$label, algorithms[i], dataname, accuracy)))
  
    if(accuracy >= best_accuracy){
      modelFit_final <- modelFit
      best_accuracy <- accuracy
    }
  }
}

results <- data.frame(t(data.frame(results)))
names(results) <- c("model", "name", "data", "accuracy")
rownames(results) <- NULL
results$model <- as.character.factor(results$model)
results$name <- as.character.factor(results$name)
results$accuracy <- as.numeric(as.character.factor(results$accuracy))
results$data <- substr(x = results$data, start = 10, stop = 20)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = results) + 
  geom_point(mapping = aes(y = model, x = accuracy, col = data, shape = data), size = 3) + 
  lims(x = c(0.7, 0.9))


## ----message=FALSE, warning = FALSE, echo=FALSE, results='asis'------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::kable(arrange(results, desc(accuracy)) %>% top_n(n = 5))


## ---- warning = FALSE, message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
modelFit_final


## ----model_2, warning = FALSE, message=FALSE, results="hide"---------------------------------------------------------------------------------------------------------------------------------------------------------------------
## model 1
set.seed(88)
model_M <-  train(Survived ~ Pclass + Name_title + Parch + SibSp + SibSp_Parch_mult + Age + Fare + Embarked,
                  data        = train_valid_data[as.character.factor(train_valid_data$Sex)=='male',],
                  trControl   = trainControl(),
                  preProcess = c("center", "scale"),
                  tuneLength  = 3,
                  method      = "rf")
 
## model 2
set.seed(88)
model_F <- train(Survived ~ Pclass + Name_title + Parch + SibSp + SibSp_Parch_mult + Age + Fare + Embarked,
                 data        = train_valid_data[as.character.factor(train_valid_data$Sex)!='male',],
                 trControl   = trainControl(),
                 preProcess = c("center", "scale"),
                 tuneLength  = 3,
                 method      = "rf")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_M
model_F


## ----model_3, warning = FALSE, message=FALSE, results="hide"---------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_valid_data$Survived   <- as.factor(ifelse(as.character.factor(train_valid_data$Survived) == '1', 'Survived', 'Perished'))
## CREATING MODELOS
set.seed(0)
models1 <- caretList(Survived ~ Pclass + Name_title + Parch + SibSp + SibSp_Parch_mult + Age + Fare + Embarked,
                    data       = train_valid_data[as.character.factor(train_valid_data$Sex)=='male',],
                     preProcess = c("center", "scale"),
                    trControl  = trainControl(classProbs=TRUE),
                    methodList = c("rpart", "glmnet", "svmRadial", "C5.0", "rf"))

models2 <- caretList(Survived ~  Pclass + Name_title + Parch + SibSp + SibSp_Parch_mult + Age + Fare + Embarked,
                    data       = train_valid_data[as.character.factor(train_valid_data$Sex)!='male',],
                     preProcess = c("center", "scale"),
                    trControl  = trainControl(classProbs=TRUE),
                    methodList = c("rpart", "glmnet", "svmRadial", "C5.0", "rf"))
                    
## GREEDY ENSEMBLE
set.seed(0)
greedy.model_M <- caretEnsemble(models1, trControl = trainControl(classProbs=TRUE))
greedy.model_F <- caretEnsemble(models2, trControl = trainControl(classProbs=TRUE))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(greedy.model_M)
summary(greedy.model_F)


## ---- warning = FALSE, message=FALSE, results="hide"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
my_prediction <- predict(modelFit_final, newdata = test, type = "raw")
solution <- data.frame(PassengerID = test$PassengerId, Survived = my_prediction)
write.csv(solution, file = 'my_solution1.csv', row.names = F, quote = F)


## ---- warning = FALSE, message=FALSE, results="hide"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_M  <- test[as.character.factor(test$Sex)=='male',]
test_M$my_prediction <- predict(object = model_M, newdata = test_M, type = "raw")
solution1 <- data.frame(PassengerID = test_M$PassengerId, Survived = test_M$my_prediction)

test_F <- test[as.character.factor(test$Sex)!='male',]
test_F$my_prediction <- predict(object = model_F, newdata = test_F, type = "raw")
solution2 <- data.frame(PassengerID = test_F$PassengerId, Survived = test_F$my_prediction)

write.csv(arrange(rbind(solution1, solution2), PassengerID), file = 'my_solution2.csv', row.names = F, quote = F)


## ---- warning = FALSE, message=FALSE, results="hide"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

test_M  <- test[as.character.factor(test$Sex)=='male',]
test_M$my_prediction <- predict(object = greedy.model_M, newdata = test_M, type = "raw")
test_M$my_prediction <- ifelse(as.character.factor(test_M$my_prediction) == 'Survived', 1, 0)
if(mean(test_M$my_prediction>0.5)) test_M$my_prediction <- ifelse(test_M$my_prediction==1,0,1)
solution1 <- data.frame(PassengerID = test_M$PassengerId, Survived = test_M$my_prediction)

test_F <- test[as.character.factor(test$Sex)!='male',]
test_F$my_prediction <- predict(object = model_F, newdata = test_F, type = "raw")
test_F$my_prediction <- ifelse(as.character.factor(test_F$my_prediction) == 'Survived', 1, 0)
if(mean(test_F$my_prediction<0.5)) test_F$my_prediction <- ifelse(test_F$my_prediction==1,0,1)
solution2 <- data.frame(PassengerID = test_F$PassengerId, Survived = test_F$my_prediction)

write.csv(arrange(rbind(solution1, solution2), PassengerID), file = 'my_solution3.csv', row.names = F, quote = F)

