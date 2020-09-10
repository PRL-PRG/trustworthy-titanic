
library(xgboost)

train = read.csv("../input/train.csv")
test = read.csv("../input/test.csv")


head(train)
head(test)

full  <- rbind(train[, -2], test)
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

train2 <- full[1:891,]
test <- full[892:1309,]
train2$Survived <- train$Survived

head(train2)
head(test)

train_prep <- train2[, -c(1, 3, 8, 10)]
test_prep <- test[, -c(1,3,8,10)]

head(train_prep)
head(test_prep)

str(train_prep$Embarked)

train_prep$Sex <- as.numeric(train_prep$Sex)
train_prep$Embarked <- as.numeric(train_prep$Embarked)
train_prep$Title <- as.numeric(as.factor(train_prep$Title))

trains <- as.matrix(train_prep[, -9])
head(trains)

Survived <- as.matrix(train_prep$Survived)
head(Survived)



dtrain <- xgb.DMatrix(data = trains, label = Survived)

test_prep$Sex <- as.numeric(test_prep$Sex)
test_prep$Embarked <- as.numeric(test_prep$Embarked)
test_prep$Title <- as.numeric(as.factor(test_prep$Title))

tests <- as.matrix(test_prep)

fit_xg <- xgboost(data = dtrain, nround = 5)
test_xg <- predict(fit_xg, tests)

head(test_xg)

imp <- xgb.importance (model = fit_xg)
xgb.plot.importance (importance_matrix = imp[1:20])

test_xg <- ifelse(test_xg >.5, 1, 0)

submission_xg <- cbind(test$PassengerId, test_xg)
colnames(submission_xg) <- c('PassengerId', 'Survived')
write.csv(submission_xg, "xgboost1.csv", row.names = FALSE)

dim(submission_xg)


