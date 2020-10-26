library(ggplot2)
library(dplyr)
library(e1071)
library(mice)
library(randomForest)
library(vcd)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
full <- bind_rows(train, test)
head(full)
full <- full[, -c(1, 9)]
full$family <- full$SibSp + full$Parch + 1
full$Familyd[full$family == 1] <- "single"
full$Familyd[full$family > 1 & full$family < 5] <- "small"
full$Familyd[full$family >= 5] <- "large"
full$Familyd <- as.factor(full$Familyd)
apply(full, 2, function(x) length(which(is.na(x))))
apply(full, 2, function(x) length(which(x == "")))
head(full$Name)
full$Name <- gsub("(.*, )|(\\. .*)", "", full$Name)
table(full$Name)
a_name <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
full$Name[full$Name == "Mlle"] <- "Miss"
full$Name[full$Name == "Ms"] <- "Miss"
full$Name[full$Name == "Mme"] <- "Mrs"
full$Name[full$Name %in% a_name] <- "Rare"
table(full$Name)
age_plot <- ggplot(full, aes(Age)) + geom_histogram(col = "black", fill = "white") + labs(title = "Have NA's")
set.seed(123)
age <- full[!is.na(full$Age), ]
age_pre <- mice(full[, -1], method = "rf")
mice_output <- complete(age_pre)
full$Age <- mice_output$Age
age_plot1 <- ggplot(full, aes(Age)) + geom_histogram(col = "black", fill = "white") + labs(title = "Remove NA's")
length(which(is.na(full$Age)))
gridExtra::grid.arrange(age_plot, age_plot1)
full[which(is.na(full$Fare)), ]$Fare <- mice_output[which(is.na(full$Fare)), ]$Fare
length(which(is.na(full$Fare)))
full$Embarked <- as.factor(full$Embarked)
embarked <- naiveBayes(Embarked ~ . - Embarked, full[, -c(1, 9)])
full[which(full$Embarked == ""), ]$Embarked <- predict(embarked, full$Embarked)[which(full$Embarked == "")]
full$Embarked <- as.factor(as.character(full$Embarked))
length(which(full$Embarked == ""))
head(full$Cabin)
length(which(full$Cabin == ""))
full$Cabin <- as.character(full$Cabin)
full$Cabin <- as.factor(sapply(full$Cabin, function(x) strsplit(x, "")[[1]][1]))
head(full$Cabin)
cabin <- full[!is.na(full$Cabin), ]
cabin0 <- full[is.na(full$Cabin), ]
for (i in 1:ncol(cabin)) {
    if (is.integer(cabin[, i])) {
        cabin[, i] <- as.factor(cabin[, i])
    }
    else if (is.character(cabin[, i])) {
        cabin[, i] <- as.factor(cabin[, i])
    }
}
for (i in 1:ncol(cabin0)) {
    if (is.integer(cabin0[, i])) {
        cabin0[, i] <- as.factor(cabin0[, i])
    }
    else if (is.character(cabin0[, i])) {
        cabin0[, i] <- as.factor(cabin0[, i])
    }
}
for (i in 1:ncol(cabin0)) {
    if (is.factor(cabin0[, i])) {
        levels(cabin0[, i]) <- union(levels(cabin0[, i]), levels(cabin[, i]))
    }
}
for (i in 1:ncol(cabin)) {
    if (is.factor(cabin[, i])) {
        levels(cabin[, i]) <- union(levels(cabin0[, i]), levels(cabin[, i]))
    }
}
set.seed(1)
ranavi0 <- randomForest::randomForest(Cabin ~ . - Cabin, na.omit(cabin[, -1]))
full[is.na(full$Cabin), ]$Cabin <- predict(ranavi0, cabin0[, -c(1, 9)])
length(which(is.na(full$Cabin)))
apply(full, 2, function(x) length(which(is.na(x))))
apply(full, 2, function(x) length(which(x == "")))
train <- full[!is.na(full$Survived), ]
test <- full[is.na(full$Survived), ]
dim(train)
head(train)
str(train)
dim(test)
head(test)
str(test)
my_mosaic <- function(file, xcolname, ycolname) {
    file <- file[, c(xcolname, ycolname)]
    cname <- c(xcolname, ycolname)
    a <- file[colnames(file) %in% cname]
    xname <- as.name(xcolname)
    yname <- as.name(ycolname)
    prob <- signif(prop.table(table(a), 1), digits = 2)
    mosaic(prob, pop = F, shade = F, legend = T, rot_lables = c(0, 90, 0, 0), labeling_args = list(set_varnames = c(xname = xcolname, yname = ycolname)), main = "Survived Rate")
    labeling_cells(text = prob, margin = 0)(prob)
}
bar_function <- function(x) {
    a <- as.data.frame(table(train[, x], factor(train[, "Survived"])))
    ggplot(a, aes(Var1, Freq, fill = Var2)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(x = Var1, y = Freq, label = Freq), position = position_dodge(width = 0.8), vjust = -0.2) + labs(x = x, y = "Freq", fill = "Survived")
}
bar_function("Pclass")
my_mosaic(train, "Pclass", "Survived")
bar_function("Sex")
my_mosaic(train, "Sex", "Survived")
bar_function("Name")
my_mosaic(train, "Name", "Survived")
ggplot(train, aes(as.factor(Survived), Age)) + geom_violin(aes(fill = Survived)) + labs(x = "Survived") + geom_hline(aes(yintercept = 10), lty = 2, lwd = 1, col = "red") + scale_y_continuous(breaks = seq(0, 80, 10)) + theme(legend.position = "none")
bar_function("family")
my_mosaic(train, "Familyd", "Survived")
ggplot(train, aes(as.factor(Survived), Fare)) + geom_violin(aes(fill = Survived)) + labs(x = "Survived") + geom_hline(aes(yintercept = max(train[train$Survived == 0, ]$Fare)), lty = 2, lwd = 1, col = "red") + scale_y_continuous(breaks = c(seq(0, 200, 100), max(train[train$Survived == 0, ]$Fare), seq(300, 500, 100))) + theme(legend.position = "none")
ggplot(train, aes(Fare)) + geom_histogram(data = train[train$Survived == 0, ], aes(fill = "red"), colour = "red", binwidth = 20, alpha = 0.3) + geom_histogram(data = train[train$Survived == 1, ], aes(fill = "blue"), colour = "blue", binwidth = 20, alpha = 0.3) + geom_vline(aes(xintercept = 50), lty = 2, lwd = 0.5) + scale_colour_manual(name = "Survived", values = c(red = "red", blue = "blue"), labels = c(red = 0, blue = 1)) + scale_fill_manual(name = "Survived", values = c(red = "red", blue = "blue"), 
    labels = c(red = 0, blue = 1)) + scale_x_continuous(breaks = c(0, 50, seq(100, 500, 100))) + labs(title = "Fare by Embarked & Survived") + theme(plot.title = element_text(hjust = 0.5)) + facet_grid(. ~ Embarked)
full$Survived <- as.factor(full$Survived)
for (i in c(1, 2, 3, 6, 7, 10)) {
    if (is.integer(full[, i])) {
        full[, i] <- as.factor(full[, i])
    }
    else if (is.character(full[, i])) {
        full[, i] <- as.factor(full[, i])
    }
}
levels(full$Pclass) <- seq(-1, 1, length.out = length(levels(full$Pclass)))
levels(full$Sex) <- seq(-1, 1, length.out = length(levels(full$Sex)))
levels(full$SibSp) <- seq(-1, 1, length.out = length(levels(full$SibSp)))
levels(full$Parch) <- seq(-1, 1, length.out = length(levels(full$Parch)))
levels(full$Cabin) <- seq(-1, 1, length.out = length(levels(full$Cabin)))
levels(full$Embarked) <- seq(-1, 1, length.out = length(levels(full$Embarked)))
levels(full$Name) <- seq(-1, 1, length.out = length(levels(full$Name)))
levels(full$Familyd) <- seq(-1, 1, length.out = length(levels(full$Familyd)))
train <- full[!is.na(full$Survived), ]
test <- full[is.na(full$Survived), ]
dim(train)
dim(test)
head(train)
set.seed(123)
train_train <- train[sample(seq_len(nrow(train)), floor(0.8 * nrow(train))), ]
set.seed(123)
train_test <- train[-sample(seq_len(nrow(train)), floor(0.8 * nrow(train))), ]
svm_test <- tune.svm(Survived ~ . - Survived, data = train_train, gamma = 2^c(-5:5), cost = 2^c(-5:5))
plot(svm_test)
d <- double(nrow(svm_test$performances))
for (i in 1:nrow(svm_test$performances)) {
    b_svm <- svm(Survived ~ . - Survived, train_train, gamma = svm_test$performances[i, 1], cost = svm_test$performances[i, 2], type = "C-classification")
    b <- table(train_test[, 1], predict(b_svm, train_test[, -1]))
    d[i] <- sum(diag(b))/sum(b)
}
e <- data.frame(gamma = svm_test$performances[1], cost = svm_test$performances[2], error = svm_test$performances[3], dispersion = svm_test$performances[4], accrancy = d)
e <- e[order(e$acc, decreasing = T), ]
head(e, 10)
svm_test1 <- tune.svm(Survived ~ . - Survived, data = train_train, gamma = seq(0, 1, 0.05), cost = seq(0.05, 4, 0.25))
plot(svm_test1)
d1 <- double(nrow(svm_test1$performances))
for (i in 1:nrow(svm_test1$performances)) {
    b_svm1 <- svm(Survived ~ . - Survived, train_train, gamma = svm_test1$performances[i, 1], cost = svm_test1$performances[i, 2], type = "C-classification")
    b1 <- table(train_test[, 1], predict(b_svm1, train_test[, -1]))
    d1[i] <- sum(diag(b1))/sum(b1)
}
e1 <- data.frame(gamma = svm_test1$performances[1], cost = svm_test1$performances[2], error = svm_test1$performances[3], dispersion = svm_test1$performances[4], accrancy = d1)
e1 <- e1[order(e1$acc, decreasing = T), ]
head(e1, 10)
final <- svm(Survived ~ . - Survived, train, gamma = 0.05, cost = 1.8, type = "C-classification")
Survived <- predict(final, test[, -1])
solution <- data.frame(PassengerId = 892:1309, Survived = Survived)
write.csv(solution, file = "svm_predicton.csv", row.names = F)
