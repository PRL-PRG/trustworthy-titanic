library(class)
library(readr)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
train$Embarked[c(62, 830)] <- "S"
train$Embarked <- factor(train$Embarked)
test$Embarked <- factor(test$Embarked)
test$Fare[153] <- median(c(train$Fare, test$Fare), na.rm = TRUE)
train_labels <- train$Survived
train$Survived <- NULL
findTitle <- function(x) {
    y <- rep(NA, length(x))
    y[grep("Mr.", x, fixed = TRUE)] <- "Mr"
    y[grep("Mrs.", x, fixed = TRUE)] <- "Mrs"
    y[grep("Master.", x, fixed = TRUE)] <- "Master"
    y[grep("Miss.", x, fixed = TRUE)] <- "Miss"
    y[grep("Col.", x, fixed = TRUE)] <- "Col"
    y[grep("Dr.", x, fixed = TRUE)] <- "Dr"
    y[grep("Rev.", x, fixed = TRUE)] <- "Rev"
    y[c(grep("Mlle.", x, fixed = TRUE), grep("Mme.", x, fixed = TRUE))] <- "Mlle"
    y[c(grep("Sir.", x, fixed = TRUE), grep("Don.", x, fixed = TRUE), grep("Major.", x, fixed = TRUE), grep("Capt.", x, fixed = TRUE))] <- "Sir"
    y[grep("Ms.", x, fixed = TRUE)] <- "Ms"
    y[c(grep("Lady.", x, fixed = TRUE), grep("Countess.", x, fixed = TRUE), grep("Jonkheer.", x, fixed = TRUE), grep("Dona.", x, fixed = TRUE))] <- "Lady"
    return(y)
}
train$Title <- findTitle(train$Name)
test$Title <- findTitle(test$Name)
all_data <- merge(train, test, all = TRUE)
library(rpart)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title, data = all_data[!is.na(all_data$Age), ], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age), ])
train <- all_data[1:891, ]
test <- all_data[892:1309, ]
knn_train <- train
knn_test <- test
knn_train$TitleNum[knn_train$Title == "Mr"] <- 1
knn_train$TitleNum[knn_train$Title == "Mrs"] <- 2
knn_train$TitleNum[knn_train$Title == "Miss"] <- 3
knn_train$TitleNum[knn_train$Title == "Master"] <- 4
knn_train$TitleNum[knn_train$Title == "Sir"] <- 5
knn_train$TitleNum[knn_train$Title == "Rev"] <- 6
knn_train$TitleNum[knn_train$Title == "Dr"] <- 7
knn_train$TitleNum[knn_train$Title == "Mlle"] <- 8
knn_train$TitleNum[knn_train$Title == "Ms"] <- 9
knn_train$TitleNum[knn_train$Title == "Lady"] <- 10
knn_train$TitleNum[knn_train$Title == "Col"] <- 11
knn_test$TitleNum[knn_test$Title == "Mr"] <- 1
knn_test$TitleNum[knn_test$Title == "Mrs"] <- 2
knn_test$TitleNum[knn_test$Title == "Miss"] <- 3
knn_test$TitleNum[knn_test$Title == "Master"] <- 4
knn_test$TitleNum[knn_test$Title == "Sir"] <- 5
knn_test$TitleNum[knn_test$Title == "Rev"] <- 6
knn_test$TitleNum[knn_test$Title == "Dr"] <- 7
knn_test$TitleNum[knn_test$Title == "Mlle"] <- 8
knn_test$TitleNum[knn_test$Title == "Ms"] <- 9
knn_test$TitleNum[knn_test$Title == "Lady"] <- 10
knn_test$TitleNum[knn_test$Title == "Col"] <- 11
min_tnum <- min(knn_train$TitleNum)
max_tnum <- max(knn_train$TitleNum)
knn_train$TitleNum <- (knn_train$TitleNum - min_tnum)/(max_tnum - min_tnum)
knn_test$TitleNum <- (knn_test$TitleNum - min_tnum)/(max_tnum - min_tnum)
knn_train$Sex <- as.numeric(knn_train$Sex)
knn_test$Sex <- as.numeric(knn_test$Sex)
knn_train$Embarked <- as.numeric(knn_train$Embarked)
knn_test$Embarked <- as.numeric(knn_test$Embarked)
knn_train$Name <- NULL
knn_test$Name <- NULL
knn_train$Ticket <- NULL
knn_test$Ticket <- NULL
knn_train$Cabin <- NULL
knn_test$Cabin <- NULL
knn_train$Title <- NULL
knn_test$Title <- NULL
knn_all <- merge(knn_train, knn_test, all = TRUE)
to_normalize_list <- c("TitleNum", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")
normalize_row <- function(x) {
    min_x <- min(x)
    max_x <- max(x)
    normed_x <- (x - min_x)/(max_x - min_x)
    return(normed_x)
}
for (col in to_normalize_list) {
    knn_all[, col] <- normalize_row(knn_all[, col])
}
knn_train <- knn_all[1:891, ]
knn_test <- knn_all[892:1309, ]
knn_train$PassengerId <- NULL
knn_test$PassengerId <- NULL
set.seed(123)
pred <- knn(knn_train, knn_test, train_labels, 20)
my_results <- data.frame(PassengerId = test$PassengerId, Survived = pred)
write.csv(my_results, file = "my_solution_knn.csv", row.names = FALSE)
