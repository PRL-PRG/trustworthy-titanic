TrainTitanic = read.csv("../input/train.csv", header = TRUE, na.strings = c("", " ", "NA"))
str(TrainTitanic)
TrainTitanic$Survived = as.factor(TrainTitanic$Survived)
TrainTitanic$Pclass = as.factor(TrainTitanic$Pclass)
summary(TrainTitanic)
colSums(is.na(TrainTitanic))
library(car)
library(ggplot2)
library(data.table)
TrainTitanic$Age[is.na(TrainTitanic$Age)] = mean(TrainTitanic$Age, na.rm = TRUE)
ggplot(TrainTitanic, aes(x = Age, fill = factor(Survived))) + geom_histogram(breaks = seq(0, 80, by = 4), colour = "black") + xlab("Age") + ylab("Passengers")
ggplot(TrainTitanic, aes(x = Survived, fill = Sex)) + geom_bar()
ggplot(TrainTitanic, aes(x = Fare, fill = factor(Survived))) + geom_histogram(breaks = seq(0, 550, by = 10), colour = "black") + xlab("Fare") + ylab("Passengers")
Fare_mean_Pclass = aggregate(TrainTitanic$Fare, list(TrainTitanic$Pclass), mean)
colnames(Fare_mean_Pclass) <- c("pclass", "mean")
ggplot(Fare_mean_Pclass, aes(x = pclass, y = mean)) + geom_bar(stat = "identity")
plot(TrainTitanic$Age, TrainTitanic$Fare, pch = 9, col = c("red", "green")[TrainTitanic$Survived])
ggplot(TrainTitanic, aes(x = Survived, fill = Embarked)) + geom_bar()
Train = read.csv("../input/train.csv", header = TRUE, na.strings = c("", " ", "NA"))
Train$Name = as.character(Train$Name)
Train$Survived = as.factor(Train$Survived)
Train$Pclass = as.factor(Train$Pclass)
str(Train)
for (i in 1:nrow(Train)) {
    Train$Title[i] <- as.character(trimws(strsplit(strsplit(Train$Name[i], ",")[[1]][2], "[.]")[[1]][1]))
}
table(Train$Title)
Train$Title = as.factor(Train$Title)
Train$Title = recode(Train$Title, "c('Don','Jonkheer','Lady','Sir','the Countess','Don','Capt','Col','Major','Dr','Rev')='Less';\n       c('Miss','Mlle','Ms')='Miss';c('Mrs','Mme')='Mrs'")
colSums(is.na(Train))
aggregate(x = Train$Age, by = list(Train$Sex, Train$Pclass, Train$Title), FUN = mean, na.rm = TRUE)
Age_Na_Treatment_Train <- function(df) {
    for (i in 1:nrow(df)) {
        if (is.na(df$Age[i])) {
            if (df$Sex[i] == "female" && df$Pclass[i] == 1) {
                if (df$Title[i] == "Less") {
                  df$Age[i] = 43.33
                }
                else if (df$Title[i] == "Miss") {
                  df$Age[i] = 29.74
                }
                else if (df$Title[i] == "Mrs") {
                  df$Age[i] = 40.4
                }
            }
            else if (df$Sex[i] == "female" && df$Pclass[i] == 2) {
                if (df$Title[i] == "Miss") {
                  df$Age[i] = 22.56
                }
                else if (df$Title[i] == "Mrs") {
                  df$Age[i] = 33.68
                }
            }
            else if (df$Sex[i] == "female" && df$Pclass[i] == 3) {
                if (df$Title[i] == "Miss") {
                  df$Age[i] = 16.12
                }
                else if (df$Title[i] == "Mrs") {
                  df$Age[i] = 33.51
                }
            }
            else if (df$Sex[i] == "male" && df$Pclass[i] == 1) {
                if (df$Title[i] == "Less") {
                  df$Age[i] = 48.7272
                }
                else if (df$Title[i] == "Master") {
                  df$Age[i] = 5.3
                }
                else if (df$Title[i] == "Mr") {
                  df$Age[i] = 41.58
                }
            }
            else if (df$Sex[i] == "male" && df$Pclass[i] == 2) {
                if (df$Title[i] == "Less") {
                  df$Age[i] = 42
                }
                else if (df$Title[i] == "Master") {
                  df$Age[i] = 2.25
                }
                else if (df$Title[i] == "Mr") {
                  df$Age[i] = 32.76
                }
            }
            else if (df$Sex[i] == "male" && df$Pclass[i] == 3) {
                if (df$Title[i] == "Master") {
                  df$Age[i] = 5.35
                }
                else if (df$Title[i] == "Mr") {
                  df$Age[i] = 28.72
                }
            }
        }
    }
    df
}
Train_Age_Imputed = Age_Na_Treatment_Train(Train)
colSums(is.na(Train_Age_Imputed))
str(Train_Age_Imputed)
Train_Name_PassId_removed = Train_Age_Imputed[, c(-1, -4)]
colSums(is.na(Train_Name_PassId_removed))
mode_embarked <- function(columnName) {
    max_embarked = table(columnName)
    names(max_embarked[which.max(max_embarked)])
}
mode_embarked(Train_Name_PassId_removed$Embarked)
Train_Name_PassId_removed$Embarked[is.na(Train_Name_PassId_removed$Embarked)] <- mode_embarked(Train_Name_PassId_removed$Embarked)
colSums(is.na(Train_Name_PassId_removed))
Train_Name_PassId_removed$Cabin = factor(Train_Name_PassId_removed$Cabin, levels = c(levels(Train_Name_PassId_removed$Cabin), "U"))
Train_Name_PassId_removed$Cabin[is.na(Train_Name_PassId_removed$Cabin)] = "U"
colSums(is.na(Train_Name_PassId_removed))
str(Train_Name_PassId_removed)
for (i in 1:nrow(Train_Name_PassId_removed)) {
    Train_Name_PassId_removed$Cabin_Encoded[i] <- as.character(trimws(substring(Train_Name_PassId_removed$Cabin[i], 1, 1)))
}
Train_Name_PassId_removed$Cabin_Encoded = as.factor(Train_Name_PassId_removed$Cabin_Encoded)
Train_Name_PassId_removed = Train_Name_PassId_removed[, -9]
str(Train_Name_PassId_removed)
for (i in 1:nrow(Train_Name_PassId_removed)) {
    Train_Name_PassId_removed$Family_Size[i] <- Train_Name_PassId_removed$SibSp[i] + Train_Name_PassId_removed$Parch[i] + 1
}
str(Train_Name_PassId_removed)
for (i in 1:nrow(Train_Name_PassId_removed)) {
    if (Train_Name_PassId_removed$Family_Size[i] == 1) {
        Train_Name_PassId_removed$Family_Type[i] = "Single"
    }
    else if (Train_Name_PassId_removed$Family_Size[i] >= 2 && Train_Name_PassId_removed$Family_Size[i] <= 4) {
        Train_Name_PassId_removed$Family_Type[i] = "Small"
    }
    else {
        Train_Name_PassId_removed$Family_Type[i] = "Large"
    }
}
Train_Name_PassId_removed$Family_Type = as.factor(Train_Name_PassId_removed$Family_Type)
str(Train_Name_PassId_removed)
str(Train_Name_PassId_removed)
Train_Name_PassId_removed = Train_Name_PassId_removed[, -7]
Train_Name_PassId_removed = Train_Name_PassId_removed[, c(-11)]
str(Train_Name_PassId_removed)
library(earth)
library(caret)
dummies <- caret::dummyVars(Survived ~ ., data = Train_Name_PassId_removed)
Train_Encoded = predict(dummies, newdata = Train_Name_PassId_removed)
Train_Encoded = as.data.frame(Train_Encoded)
Train_Encoded = cbind(Train_Name_PassId_removed[, 1], Train_Encoded)
setnames(Train_Encoded, "Train_Name_PassId_removed[, 1]", "Survived")
Test = read.csv("../input/test.csv", header = TRUE, na.strings = c("", " ", "NA"))
str(Test)
Test$Pclass = as.factor(Test$Pclass)
Test$Name = as.character(Test$Name)
for (i in 1:nrow(Test)) {
    Test$Title[i] <- as.character(trimws(strsplit(strsplit(Test$Name[i], ",")[[1]][2], "[.]")[[1]][1]))
}
table(Test$Title)
library(car)
Test$Title = recode(Test$Title, "c('Don','Jonkheer','Lady','Sir','the Countess','Don','Dona','Capt','Col','Major','Dr','Rev')='Less';\n                  c('Miss','Mlle','Ms')='Miss';c('Mrs','Mme')='Mrs'")
Test$Title = as.factor(Test$Title)
colSums(is.na(Test))
aggregate(x = Test$Age, by = list(Test$Sex, Test$Pclass, Test$Title), FUN = mean, na.rm = TRUE)
Age_Na_Treatment_Test <- function(df) {
    for (i in 1:nrow(df)) {
        if (is.na(df$Age[i])) {
            if (df$Sex[i] == "female" && df$Pclass[i] == 1) {
                if (df$Title[i] == "Miss") {
                  df$Age[i] = 31.42
                }
                else if (df$Title[i] == "Mrs") {
                  df$Age[i] = 45.6
                }
                else if (df$Title[i] == "Less") {
                  df$Age[i] = 39
                }
            }
            else if (df$Sex[i] == "female" && df$Pclass[i] == 2) {
                if (df$Title[i] == "Miss") {
                  df$Age[i] = 17.37
                }
                else if (df$Title[i] == "Mrs") {
                  df$Age[i] = 33
                }
            }
            else if (df$Sex[i] == "female" && df$Pclass[i] == 3) {
                if (df$Title[i] == "Miss") {
                  df$Age[i] = 19.87
                }
                else if (df$Title[i] == "Mrs") {
                  df$Age[i] = 29.87
                }
            }
            else if (df$Sex[i] == "male" && df$Pclass[i] == 1) {
                if (df$Title[i] == "Less") {
                  df$Age[i] = 51
                }
                else if (df$Title[i] == "Master") {
                  df$Age[i] = 9.5
                }
                else if (df$Title[i] == "Mr") {
                  df$Age[i] = 41.2
                }
            }
            else if (df$Sex[i] == "male" && df$Pclass[i] == 2) {
                if (df$Title[i] == "Less") {
                  df$Age[i] = 35.5
                }
                else if (df$Title[i] == "Master") {
                  df$Age[i] = 5
                }
                else if (df$Title[i] == "Mr") {
                  df$Age[i] = 31.71
                }
            }
            else if (df$Sex[i] == "male" && df$Pclass[i] == 3) {
                if (df$Title[i] == "Master") {
                  df$Age[i] = 7.45
                }
                else if (df$Title[i] == "Mr") {
                  df$Age[i] = 27.2
                }
            }
        }
    }
    df
}
Test_Age_Imputed = Age_Na_Treatment_Test(Test)
colSums(is.na(Test_Age_Imputed))
str(Test_Age_Imputed)
Test_Name_removed = Test_Age_Imputed[, -3]
str(Test_Name_removed)
colSums(is.na(Test_Name_removed))
Test_Name_removed$Cabin = factor(Test_Name_removed$Cabin, levels = c(levels(Test_Name_removed$Cabin), "U"))
Test_Name_removed$Cabin[is.na(Test_Name_removed$Cabin)] = "U"
colSums(is.na(Test_Name_removed))
str(Test_Name_removed)
for (i in 1:nrow(Test_Name_removed)) {
    Test_Name_removed$Cabin_Encoded[i] <- as.character(trimws(substring(Test_Name_removed$Cabin[i], 1, 1)))
}
Test_Name_removed$Cabin_Encoded = as.factor(Test_Name_removed$Cabin_Encoded)
Test_Name_PaTest_Age_Imputed = Test_Name_removed[, -9]
str(Test_Name_PaTest_Age_Imputed)
colSums(is.na(Test_Name_PaTest_Age_Imputed))
Test_Name_PaTest_Age_Imputed$Fare[is.na(Test_Name_PaTest_Age_Imputed$Fare)] = mean(Test_Name_PaTest_Age_Imputed$Fare, na.rm = TRUE)
colSums(is.na(Test_Name_PaTest_Age_Imputed))
for (i in 1:nrow(Test_Name_PaTest_Age_Imputed)) {
    Test_Name_PaTest_Age_Imputed$Family_Size[i] <- Test_Name_PaTest_Age_Imputed$SibSp[i] + Test_Name_PaTest_Age_Imputed$Parch[i] + 1
}
str(Test_Name_PaTest_Age_Imputed)
for (i in 1:nrow(Test_Name_PaTest_Age_Imputed)) {
    if (Test_Name_PaTest_Age_Imputed$Family_Size[i] == 1) {
        Test_Name_PaTest_Age_Imputed$Family_Type[i] = "Single"
    }
    else if (Test_Name_PaTest_Age_Imputed$Family_Size[i] >= 2 && Test_Name_PaTest_Age_Imputed$Family_Size[i] <= 4) {
        Test_Name_PaTest_Age_Imputed$Family_Type[i] = "Small"
    }
    else {
        Test_Name_PaTest_Age_Imputed$Family_Type[i] = "Large"
    }
}
Test_Name_PaTest_Age_Imputed$Family_Type = as.factor(Test_Name_PaTest_Age_Imputed$Family_Type)
str(Test_Name_PaTest_Age_Imputed)
colSums(is.na(Test_Name_PaTest_Age_Imputed))
Test_Name_PaTest_Age_Imputed = Test_Name_PaTest_Age_Imputed[, -7]
str(Test_Name_PaTest_Age_Imputed)
Test_Selected = Test_Name_PaTest_Age_Imputed[, c(-11)]
str(Test_Selected)
dummies_Test <- caret::dummyVars(PassengerId ~ ., data = Test_Selected)
Test_Encoded = predict(dummies_Test, newdata = Test_Selected)
Test_Encoded = as.data.frame(Test_Encoded)
Test_Encoded = cbind(Test_Selected[, 1], Test_Encoded)
setnames(Test_Encoded, "Test_Selected[, 1]", "PassengerId")
str(Test_Encoded)
str(Train_Encoded)
library(randomForest)
library(rpart.plot)
library(caret)
library(e1071)
Train_Encoded_Selected = Train_Encoded[, which(names(Train_Encoded) %in% c("Survived", "Sex.female", "Title.Mr", "Fare", "Sex.male", "Pclass.3", "Age", "Title.Miss", "Title.Master", "Title.Mrs", "Cabin_Encoded.U", "Embarked.S", "Family_Type.Small", "Family_Type.Large", "Embarked.C", "Pclass.2", "Pclass.1", "Cabin_Encoded.E", "Embarked.Q", "Family_Type.Small"))]
tRF <- tuneRF(x = Train_Encoded_Selected[, -1], y = Train_Encoded_Selected$Survived, mtryStart = 4, ntreeTry = 151, stepFactor = 1.5, improve = 1e-04, trace = TRUE, plot = TRUE, doBest = TRUE, nodesize = 10, importance = TRUE)
dim(Train_Encoded_Selected)
randomForestModel <- randomForest(Survived ~ ., data = Train_Encoded_Selected, ntree = 151, mtry = 3, nodesize = 10, importance = TRUE)
library(data.table)
library(gbm)
library(ranger)
library(caTools)
Survived = predict(randomForestModel, newdata = Test_Encoded, type = "class")
submitfile = as.data.table(Survived)
submitfile = cbind(Test_Encoded$PassengerId, submitfile)
submitfile
setnames(submitfile, "V1", "PassengerId")
submitfile
fwrite(submitfile, "submitFile.csv")
getwd()
