library(tidyverse)
library(class)
list.files(path = "../input")
rm(list = ls())
t1 <- read.csv("../input/test.csv", as.is = T)
t2 <- read.csv("../input/train.csv", as.is = T)
all <- bind_rows(t2, t1)
colSums(is.na(all))
all$Sex <- gsub("female", "1", all$Sex)
all$Sex <- gsub("male", "0", all$Sex)
all$Sex <- as.numeric(all$Sex)
missing.a <- all[which(is.na(all$Age)), ]
train <- all[which(!is.na(all$Age)), ]
model <- lm(Age ~ Pclass + Sex, data = train)
pred <- predict(model, missing.a)
pred <- as.data.frame(pred)
count <- 0
for (i in which(is.na(all$Age))) {
    count <- count + 1
    all$Age[i] <- pred$pred[count]
}
colSums(is.na(all))
all$Fare[all$Fare == 0] <- NA
all$Embarked[which(all$Embarked == "")] <- "S"
all$Fare <- as.numeric(all$Fare)
colSums(is.na(all))
all$Fare[which(is.na(all$Fare))] <- median(all$Fare, na.rm = T)
colSums(is.na(all))
data.frame(num = 1:length(names(all)), names = names(all))
df <- all[-c(1, 4, 9, 11)]
head(df)
df$Survived <- as.factor(df$Survived)
df$Embarked[which(df$Embarked == "C")] <- 0
df$Embarked[which(df$Embarked == "Q")] <- 1
df$Embarked[which(df$Embarked == "S")] <- 2
df$Embarked <- as.numeric(df$Embarked)
norm <- function(x) {
    return((x - min(x))/(max(x) - min(x)))
}
train_target <- df[1:579, 1]
test_target <- df[580:891, 1]
df_z <- df
df <- as.data.frame(lapply(df[, c(2:8)], norm))
x <- df
df <- df[1:891, ]
train <- df[1:579, ]
test <- df[580:891, ]
k <- as.integer(sqrt(579))
model <- knn(train = train, test = test, cl = train_target, k = k)
t <- table(test_target, model)
t
accuracy <- 100 - ((t[2] + t[3])/length(model)) * 100
cat("\n")
cat("accuracy = ", accuracy, "%")
df_z1 <- as.data.frame(scale(df_z[-c(1)]))
df_z1_train <- df_z1[1:579, ]
df_z1_test <- df_z1[580:891, ]
df_z1_train_target <- df_z[1:579, 1]
df_z1_test_target <- df_z[580:891, 1]
k <- 5
model <- knn(train = df_z1_train, test = df_z1_test, cl = df_z1_train_target, k = k)
t <- table(df_z1_test_target, model)
t
accuracy <- 100 - ((t[2] + t[3])/length(model)) * 100
cat("\n")
cat("accuracy = ", accuracy, "%")
k <- 5
model <- knn(train = df_z1_train, test = x[892:1309, ], cl = df_z1_train_target, k = k)
submit <- data.frame(PassengerId = 892:1309, Survived = model)
write.csv(submit, file = "score.csv", row.names = F)
