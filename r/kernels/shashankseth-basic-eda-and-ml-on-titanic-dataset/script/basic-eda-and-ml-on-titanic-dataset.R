library(ggplot2)
library(readr)
library(dplyr)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
str(train)
str(test)
full <- bind_rows(train, test)
str(full)
lapply(full, function(x) length(unique(x)))
boxplot(full$Age)
remove_outliers <- function(x, na.rm = TRUE) {
    qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
    H <- 1.5 * IQR(x, na.rm = T)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}
a <- remove_outliers(full$Age)
sum(is.na(a))
full$Age <- a
mean_full <- mean(full$Age, na.rm = T)
full[is.na(full$Age), "Age"] <- mean_full
full[c(62, 830), "Embarked"]
full[c(62, 830), "Fare"]
fare_emb <- full %>% filter(PassengerId != 62 & PassengerId != 830)
ggplot(fare_emb, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot()
full$Embarked[c(62, 830)] <- "C"
length(unique(full$Embarked))
full$FamSize <- full$SibSp + full$Parch + 1
ggplot(full, aes(x = FamSize, fill = factor(Survived))) + geom_bar()
full$FamSizeD[full$FamSize == 1] <- 1
full$FamSizeD[full$FamSize > 1 & full$FamSize <= 4] <- 2
full$FamSizeD[full$FamSize > 4] <- 3
full$Pclass <- as.factor(full$Pclass)
full$Survived <- as.factor(full$Survived)
full$Embarked <- as.factor(full$Embarked)
full$FamSizeD <- as.factor(full$FamSizeD)
train_set <- full[1:891, ]
str(train_set)
test_set <- full[-c(1:891), ]
str(test_set)
set.seed(123)
div <- sample(nrow(train_set), nrow(train_set) * 0.8)
train_set1 <- train_set[div, ]
str(train_set1)
valid_set <- train_set[-div, ]
str(valid_set)
prop.table(table(train_set1$Survived))
prop.table(table(valid_set$Survived))
lr_model1 <- glm(Survived ~ Pclass + Sex + Age + FamSizeD + Embarked, data = train_set1, family = binomial)
summary(lr_model1)
train_pred <- predict(lr_model1, newdata = train_set1, type = "response")
table(train_set1$Survived, train_pred > 0.5)
train_acc <- ((371 + 198)/(371 + 62 + 81 + 198)) * 100
train_acc
valid_pred <- predict(lr_model1, newdata = valid_set, type = "response")
table(valid_set$Survived, valid_pred > 0.5)
valid_acc <- ((106 + 43)/(106 + 10 + 20 + 43)) * 100
valid_acc
test_pred <- predict(lr_model1, newdata = test_set, type = "response")
test_set$Survived <- as.factor(ifelse(test_pred > 0.5, 1, 0))
table(test_set$Survived)
output <- data.frame(PassengerId = test_set$PassengerId, Survived = test_set$Survived)
write.csv(output, file = "lr_logistic_reg.csv", row.names = F)
