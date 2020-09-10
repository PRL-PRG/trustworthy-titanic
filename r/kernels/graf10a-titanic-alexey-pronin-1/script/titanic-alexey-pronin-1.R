# Reading the train and test data.

train <- read.csv('../input/train.csv')
test <- read.csv('../input/test.csv')

# Adding the 'Survived' column to the test data 
# to make it compatable with the training data
# for rbind-ing.

test$Survived <- rep(NA, nrow(test))

# Combining the data

data <- rbind(train, test)

# Inspecting the data

summary(data)

# We see that there are missing values: 418 in the 'Survided' column
# (expected -- we put them there), 263 in the 'Age' column, and 1 in 
# the Fare column. Let's replace them with median values.

library(Hmisc)
data$Age <- impute(data$Age, fun = median)
data$Fare <- impute(data$Fare, fun = median)

# How many unique entries do we have in each columns?

sapply(apply(data, 2, unique), length)

# We will not be using the 'PassengerId', 'Name', 'Ticket', 
# and 'Cabin' columns in our modeling. Creating the formula
# for future modeling.

fmla <- as.formula('Survived ~ Pclass + Sex + Age + SibSp + 
        Parch + Fare + Embarked')

# Separating the training and test data from 'data'

n <- nrow(train)
train <- data[1:n, ]

# Spliting the training data into a training and
set.seed(123)
split <- sample(1:n, round(0.8*n))
train_train <- train[split, ]
train_test <- train[-split, ]

model_short <- glm(fmla, train_train, 
                   family = 'binomial')
summary(model_short)

prob_short <- predict(model_short, train_test, type = 'response')
pred_short <- ifelse(prob_short > 0.5, 1, 0)
table(train_test$Survived, pred_short)
mean(train_test$Survived == pred_short)


# Random forest fit
# randomForest package

library(randomForest)
model_rf <- randomForest(fmla, train_train, nrees = 500, mtry = 2)
summary(model_rf)

prob_rf <- predict(model_rf, train_test, type = 'response')
pred_rf <- ifelse(prob_rf > 0.5, 1, 0)
table(train_test$Survived, pred_rf)
mean(train_test$Survived == pred_rf)

# ranger package

library(ranger)
model_ranger <- ranger(fmla, train_train, num.trees = 500,
                       respect.unordered.factors = 'order')
prob_ranger <- predict(model_ranger, train_test)$predictions
pred_ranger <- ifelse(prob_ranger > 0.5, 1, 0)
table(train_test$Survived, pred_ranger)
mean(train_test$Survived == pred_ranger)

# Now let's try the 'caret' package:

library(caret)
set.seed(42)
myFolds <- createFolds(train$Survived, k = 5)

myControl <- trainControl(
    method = 'cv',
    number = 5,
    summaryFunction = twoClassSummary,
    classProbs = TRUE,
    verboseIter = TRUE,
    savePredictions = TRUE,
    index = myFolds
    )

train$Survived <- as.factor(train$Survived)
levels(train$Survived) <- c('No', 'Yes')

model_rf_1 <- train(
    fmla, train,
    metric = "ROC",
    method = "ranger",
    trControl = myControl
)

pred_rf_1 <- predict(model_rf_1)
confusionMatrix(pred_rf_1, train$Survived)

model_glmnet <- train(
    fmla, train,
    metric = "ROC",
    method = "glmnet",
    tuneGrid = expand.grid(
        alpha = 0:1,
        lambda = 0:10/10
    ),
    trControl = myControl
)

pred_glmnet <- predict(model_glmnet)
confusionMatrix(pred_glmnet, train$Survived)

# We will use model_rf_1 for our final submission.
# Separating the test data (corrected).

test <- data[(n + 1):nrow(data), ]

# Making predictions.

pred <- predict(model_rf_1, test, type = 'raw')

test$Survived <- ifelse(pred == 'No', 0, 1)

write.csv(test[c('PassengerId', 'Survived')], 
          'Titanic_submission.csv', 
          row.names = FALSE)