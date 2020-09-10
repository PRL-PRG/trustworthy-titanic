
library(tidyverse)
library(xgboost)
list.files(path = "../input")

train_data = read.csv('../input/train.csv', na.strings = c("", " ", "NA"))
test_data  = read.csv('../input/test.csv',  na.strings = c("", " ", "NA"))
train_data$Train  <- TRUE
test_data$Train  <- FALSE
test_tmp  <- test_data
test_tmp$Survived = FALSE

# combine all of the data 
all_data = rbind(train_data, test_tmp)

all_data$Title <- gsub('(.*, )|([.].*)', '', all_data$Name) # Get ride of anything that ends in a comma or anything that starts with a period

# Just to check how many people we have with each title
all_data %>%
    group_by(Title, Sex) %>%
    summarize(n = n()) %>%
    arrange(Title)

all_data$Title[all_data$Title == 'Mlle']  <- 'Miss'
all_data$Title[all_data$Title == 'Ms']    <- 'Miss'
all_data$Title[all_data$Title == 'Mme']   <- 'Mrs'

special_people  <- c('Capt', 'Col', 'Dona', 'Dr', 'Jonkheer', 'Lady', 'Major', 'Rev', 'Sir', 'the Countess', 'Don')
all_data$Title[all_data$Title %in% special_people]  <- 'Special'
# all_data$Title[all_data$Title == 'Dr' & all_data$Sex == 'male']    <- 'Mr'
# all_data$Title[all_data$Title == 'Dr' & all_data$Sex == 'female']  <- 'Mrs'

all_data %>%
    group_by(Title, Sex) %>%
    summarize(n = n()) %>%
    arrange(Title)

all_data$Name %>%
    gsub(',.*$','', .) -> all_data$Surname

cat('Unique number of surnames: ',nlevels(factor(all_data$Surname)))

all_data %>%
    mutate(Fam_Size = SibSp + Parch + 1) -> all_data

names(all_data) # veiw results

all_data %>%
    select(-c(PassengerId, Name, Ticket, Cabin, Surname)) -> all_data

head(all_data) # veiw results

factor_vars <- c('Pclass','Sex','Embarked',
                 'Title')

all_data[factor_vars] <- lapply(all_data[factor_vars], function(x) as.factor(x))

all_data %>%
    fastDummies::dummy_cols(remove_most_frequent_dummy = TRUE) %>%
    select(-c(Pclass, Sex, Embarked, Title)) -> all_data

library(missForest)

set.seed(555)
all_data_mf <- missForest(all_data[, -9], maxiter = 100)
                                
all_data_mf$ximp$Train <- all_data$Train
                           
all_data_mf$ximp %>%
    filter(Train) %>%
    select(-Train) -> train_mf

all_data_mf$ximp %>%
    filter(!Train) %>%
    select(-Train) -> test_mf

rownames(test_mf) <- test_data$PassengerId

train_mf %>%
    select(-Survived) %>%
    as.matrix() -> input_x

train_mf$Survived <- factor(train_mf$Survived)
input_y <- train_mf$Survived

test_mf %>%
    as.matrix() -> test_x

library(randomForest)
rf  <- randomForest(x = input_x,
                    y = input_y,
                    ntree = 1001,
                    importance = TRUE)

varImpPlot(rf, main="Variable Importance Plots")

partialPlot(rf, x.var = Age, pred.data = input_x, which.class = 1)
partialPlot(rf, x.var = Sex_female, pred.data = input_x, which.class = 1)

head(all_data_mf$ximp)

ggplot(all_data_mf$ximp, aes(x = Age, fill = factor(Survived))) + 
    facet_wrap(~Sex_female) + 
    geom_histogram()

all_data_mf$ximp$Age_Group[all_data_mf$ximp$Age <= 19] <- 'child'
all_data_mf$ximp$Age_Group[all_data_mf$ximp$Age >= 40] <- 'old'
all_data_mf$ximp$Age_Group[all_data_mf$ximp$Age > 19 & all_data_mf$ximp$Age < 40] <- 'adult'

head(all_data_mf$ximp)

all_data_mf$ximp$Age_Group_old[all_data_mf$ximp$Age_Group == 'old'] <- 1
all_data_mf$ximp$Age_Group_old[all_data_mf$ximp$Age_Group != 'old'] <- 0

all_data_mf$ximp$Age_Group_child[all_data_mf$ximp$Age_Group == 'child'] <- 1
all_data_mf$ximp$Age_Group_child[all_data_mf$ximp$Age_Group != 'child'] <- 0

all_data_mf$ximp %>%
    select(-c(Age, Age_Group)) -> all_data_mf$ximp

head(all_data_mf$ximp)

all_data_mf$ximp %>%
    filter(Train) %>%
    select(-Train) -> train_mf

all_data_mf$ximp %>%
    filter(!Train) %>%
    select(-Train) -> test_mf

rownames(test_mf) <- test_data$PassengerId

train_mf %>%
    select(-Survived) %>%
    as.matrix() -> input_x

train_mf$Survived <- factor(train_mf$Survived)
input_y <- train_mf$Survived

test_mf %>%
    as.matrix() -> test_x

rf  <- randomForest(x = input_x,
                    y = input_y,
                    ntree = 1001,
                    importance = TRUE)

varImpPlot(rf)

partialPlot(rf, x.var = Age_Group_child, pred.data = input_x, which.class = 1)
partialPlot(rf, x.var = Age_Group_old, pred.data = input_x, which.class = 1)

rf_predictions <- data.frame(PassengerId = rownames(test_x), Survived = as.numeric(as.character(predict(rf, test_x))))

rf_predictions %>%
    group_by(Survived) %>%
    summarize(n = n(), p = n() / nrow(test_x))

write.csv(rf_predictions, file = 'rf_Solution.csv', row.names = F)

library(caret)
xgb_grid = expand.grid(
    nrounds = seq(from = 200, to = 1000, by = 100),
    max_depth = c(2, 3, 4, 5, 6),
    eta = c(0.025, 0.05, 0.1, 0.3),
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
)

xgb_trcontrol = trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 3,
    allowParallel = TRUE,
)

xgb_train = train(
    x = input_x,
    y = input_y,
    trControl = xgb_trcontrol,
    tuneGrid = xgb_grid,
    method = 'xgbTree'
)

plot_fit <- function(x) {
  ggplot(x) +
    coord_cartesian(ylim = c(0.6, 1))
}

plot_fit(xgb_train)

xgb_train$bestTune
# nrounds=800, max_depth=3, eta=0.025, gamma=0, colsample_bytree=1, min_child_weight=1 subsample=1

xgb_grid = expand.grid(
    nrounds = seq(from = 200, to = 1000, by = 100),
    max_depth = (xgb_train$bestTune$max_depth - 1):(xgb_train$bestTune$max_depth + 1),
    eta = xgb_train$bestTune$eta,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = c(1, 2, 3),
    subsample = 1
)

xgb_train = train(
    x = input_x,
    y = input_y,
    trControl = xgb_trcontrol,
    tuneGrid = xgb_grid,
    method = 'xgbTree'
)

plot_fit(xgb_train)

xgb_train$bestTune
# nrounds=500, max_depth=4, eta=0.1, gamma=0.2, colsample_bytree=0.45, min_child_weight=1 subsample=.5
# new nrounds=400, max_depth=3, eta=0.1, gamma=0.6, colsample_bytree=0.45, min_child_weight=1 subsample=.5

xgb_grid = expand.grid(
    nrounds = seq(from = xgb_train$bestTune$nrounds - 50, to = xgb_train$bestTune$nrounds + 50, by = 50),
    max_depth = xgb_train$bestTune$max_depth,
    eta = xgb_train$bestTune$eta,
    gamma = 0,
    colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
    min_child_weight = xgb_train$bestTune$min_child_weight,
    subsample = c(.5, .7, .9, 1)
)

xgb_train = train(
    x = input_x,
    y = input_y,
    trControl = xgb_trcontrol,
    tuneGrid = xgb_grid,
    method = 'xgbTree'
)

plot_fit(xgb_train)

xgb_grid = expand.grid(
    nrounds = seq(from = xgb_train$bestTune$nrounds - 25, to = xgb_train$bestTune$nrounds + 25, by = 25),
    max_depth = xgb_train$bestTune$max_depth,
    eta = xgb_train$bestTune$eta,
    gamma = c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 1),
    colsample_bytree = xgb_train$bestTune$colsample_bytree,
    min_child_weight = xgb_train$bestTune$min_child_weight,
    subsample = xgb_train$bestTune$subsample
)

xgb_train = train(
    x = input_x,
    y = input_y,
    trControl = xgb_trcontrol,
    tuneGrid = xgb_grid,
    method = 'xgbTree'
)

plot_fit(xgb_train)

xgb_grid = expand.grid(
    nrounds = seq(from = 100, to=5000, by = 100),
    max_depth = xgb_train$bestTune$max_depth,
    eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
    gamma = xgb_train$bestTune$gamma,
    colsample_bytree = xgb_train$bestTune$colsample_bytree,
    min_child_weight = xgb_train$bestTune$min_child_weight,
    subsample = xgb_train$bestTune$subsample
)

xgb_train = train(
    x = input_x,
    y = input_y,
    trControl = xgb_trcontrol,
    tuneGrid = xgb_grid,
    method = 'xgbTree'
)

plot_fit(xgb_train)

final_grid <- expand.grid(
  nrounds = xgb_train$bestTune$nrounds,
  eta = xgb_train$bestTune$eta,
  max_depth = xgb_train$bestTune$max_depth,
  gamma = xgb_train$bestTune$gamma,
  colsample_bytree = xgb_train$bestTune$colsample_bytree,
  min_child_weight = xgb_train$bestTune$min_child_weight,
  subsample = xgb_train$bestTune$subsample
)

xgb_model <- caret::train(
  x = input_x,
  y = input_y,
  trControl = xgb_trcontrol,
  tuneGrid = final_grid,
  method = "xgbTree",
  verbose = TRUE
)

predictions <- data.frame(test_data$PassengerId, as.numeric(as.character(predict(xgb_model, test_x))))
names(predictions) <- c('PassengerId', 'Survived')

head(predictions)

write.csv(predictions, file = 'xgb_Solution.csv', row.names = F)
