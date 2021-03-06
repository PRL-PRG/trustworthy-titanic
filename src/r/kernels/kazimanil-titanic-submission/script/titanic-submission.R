library(class)
library(rpart)
library(randomForest)
library(partykit)
library(data.table)
library(stringr)
rm(list = ls())
gc()
train <- fread("../input/train.csv")
gender_submission <- fread("../input/gender_submission.csv")
test <- fread("../input/test.csv")
NBCalc <- function(train, test, rows, objective) {
    stopifnot((is.character(rows) & is.character(objective)) | (is.numeric(rows) & is.numeric(objective)))
    rts <- if (is.numeric(rows)) {
        as.numeric(cbind(as.data.table(t(rows)), as.data.table(objective)))
    }
    else {
        as.character(cbind(as.data.table(t(rows)), as.data.table(objective)))
    }
    dat <- train[, rts, with = FALSE]
    det <- test[, rows, with = FALSE]
    det$p1 <- train[, mean(get(objective))]
    p1 <- train[, mean(get(objective))]
    nrowdet <- nrow(det)
    rff <- ncol(dat) - 1
    for (i in 1:rff) {
        name <- colnames(dat)[i]
        res <- dat[, .(value = unique(get(name)), lift = mean(get(objective))/p1), .(get(name))][, 2:3]
        res <- `colnames<-`(res, c("value", paste0("lift_", name)))
        det <- merge(det, res, by.x = paste0(name), by.y = "value")
        stopifnot(nrowdet == nrow(det))
    }
    scol <- rff + 2
    ecol <- (2 * rff) + 1
    det[, `:=`(NaiveBayesProb, apply(det[, scol:ecol], 1, prod) * p1)]
    return(det)
}
train[, `:=`(Title, trimws(str_split_fixed(gsub(",", ".", Name), "[.]", n = 3)[, 2]))]
train[Title == "Master", `:=`(Title, "Mr")]
train[Title == "Mlle", `:=`(Title, "Miss")]
train[Title %in% c("Ms", "Mme"), `:=`(Title, "Mrs")]
train[Title %in% c("the Countess", "Sir", "Jonkheer", "Lady", "Don", "Dona"), `:=`(Title, "Noble")]
train[Title %in% c("Capt", "Col", "Major"), `:=`(Title, "Military")]
train[, `:=`(Title, as.factor(Title))]
train <- merge(train, train[, mean(Age, na.rm = T), .(Title)], by = "Title")
train[, `:=`(Age, ceiling(Age))]
train[, `:=`(glmAge, ifelse(is.na(Age), V1, Age))]
train[, `:=`(V1, NULL)]
train[, `:=`(Cl_Age, as.factor(pmin(ceiling(glmAge/10), 8)))]
train[, `:=`(Cl_Relative, as.factor(pmin(SibSp + Parch, 5)))]
train[, `:=`(Pclass, as.factor(Pclass))]
train[, `:=`(Embarked, as.factor(Embarked))]
train[, `:=`(Sex, as.factor(Sex))]
train[, `:=`(log_Fare, log10(Fare + 1))]
train[log_Fare < 1, `:=`(Cl_Fare, "Cheapest")]
train[log_Fare >= 1 & log_Fare < 1.5, `:=`(Cl_Fare, "Cheap")]
train[log_Fare >= 1.5 & log_Fare < 2, `:=`(Cl_Fare, "Modest")]
train[log_Fare > 2, `:=`(Cl_Fare, "Expensive")]
train[, `:=`(CabinDpt, substr(Cabin, 1, 1))]
train[CabinDpt == " ", `:=`(CabinDpt, as.character(NA))]
train <- merge(train, train[, .(nTicket = .N), Ticket], all.x = T, by = "Ticket")
train[, `:=`(fac_nTicket, as.factor(nTicket))]
test[, `:=`(Title, trimws(str_split_fixed(gsub(",", ".", Name), "[.]", n = 3)[, 2]))]
test[Title == "Master", `:=`(Title, "Mr")]
test[Title == "Mlle", `:=`(Title, "Miss")]
test[Title %in% c("Ms", "Mme"), `:=`(Title, "Mrs")]
test[Title %in% c("the Countess", "Sir", "Jonkheer", "Lady", "Don", "Dona"), `:=`(Title, "Noble")]
test[Title %in% c("Capt", "Col", "Major"), `:=`(Title, "Military")]
test[, `:=`(Title, as.factor(Title))]
test[is.na(Fare), `:=`(Fare, 0)]
test <- merge(test, test[, mean(Age, na.rm = T), .(Title)], by = "Title")
test[, `:=`(Age, ceiling(Age))]
test[, `:=`(glmAge, ifelse(is.na(Age), V1, Age))]
test[, `:=`(V1, NULL)]
test[, `:=`(Cl_Age, as.factor(pmin(ceiling(glmAge/10), 8)))]
test[, `:=`(Cl_Relative, as.factor(pmin(SibSp + Parch, 5)))]
test[, `:=`(Pclass, as.factor(Pclass))]
test[, `:=`(Embarked, as.factor(Embarked))]
test[, `:=`(Sex, as.factor(Sex))]
test[, `:=`(log_Fare, log10(Fare + 1))]
test[log_Fare < 1, `:=`(Cl_Fare, "Cheapest")]
test[log_Fare >= 1 & log_Fare < 1.5, `:=`(Cl_Fare, "Cheap")]
test[log_Fare >= 1.5 & log_Fare < 2, `:=`(Cl_Fare, "Modest")]
test[log_Fare > 2, `:=`(Cl_Fare, "Expensive")]
test[, `:=`(CabinDpt, substr(Cabin, 1, 1))]
test[CabinDpt == " ", `:=`(CabinDpt, as.character(NA))]
test <- merge(test, test[, .(nTicket = .N), Ticket], all.x = T, by = "Ticket")
test[, `:=`(fac_nTicket, as.factor(nTicket))]
acc <- data.table(set = numeric(), k_nn = numeric(), Accuracy = integer())
s <- 1
while (s < 101) {
    knn_train <- train[, .(Survived, Title = as.numeric(Title), Pclass = as.numeric(Pclass), Sex = as.numeric(Sex), Embarked = as.numeric(Embarked), glmAge, Relative = as.numeric(Cl_Relative), log_Fare, nTicket)]
    knn_train <- knn_train[sample(nrow(train)), ]
    knn_r <- knn_train[1:700]
    knn_e <- knn_train[701:891]
    knn_s <- knn_r$Survived
    knn_r[, `:=`(Survived, NULL)]
    knn_a <- knn_e$Survived
    knn_e[, `:=`(Survived, NULL)]
    for (k in 1:100) {
        knn_o <- cbind(knn_e, pred = knn(knn_r, knn_e, knn_s, k), res = knn_a)
        acc <- rbind(acc, as.data.table(t(c(set = s, k_nn = k, Accuracy = knn_o[pred == res, .N/nrow(knn_o)]))))
    }
    s <- s + 1
}
acc[, .(Ort = mean(Accuracy), SD = sd(Accuracy)), .(k_nn)][order(-Ort)]
knn_test <- test[, .(PassengerId, Title = as.numeric(Title), Pclass = as.numeric(Pclass), Sex = as.numeric(Sex), Embarked = as.numeric(Embarked), glmAge, Relative = as.numeric(Cl_Relative), log_Fare, nTicket)]
knn_test <- cbind(knn_test, Survived = knn(knn_train[, 2:9], knn_test[, 2:9], cl = train$Survived, k = 7))
fwrite(knn_test[, c(1, 10)], "submission_knn_2.csv")
train2 <- train[sample(nrow(train)), ]
folds <- cut(seq(1, nrow(train2)), breaks = 10, labels = FALSE)
acc <- data.table(Model = character(), Accuracy = integer())
for (i in 1:10) {
    testIndexes <- which(folds == i, arr.ind = TRUE)
    testData <- train2[testIndexes, ]
    trainData <- train2[-testIndexes, ]
    assign(x = paste0("model_", i), step(direction = "backward", object = glm(formula = Survived ~ glmAge + log_Fare + SibSp + Parch + Embarked + Pclass + Title + Cl_Relative + nTicket, data = trainData, family = binomial(link = "logit"))))
    testData[, `:=`(phat, predict.glm(get(paste0("model_", i)), newdata = testData, type = "response"))]
    testData[, `:=`(pred, ifelse(phat > 0.5, 1, 0))]
    pasted <- cbind(Model = paste0("model_", i), Accuracy = testData[pred == Survived, .N/nrow(testData)])
    acc <- rbind(acc, pasted, deparse.level = 0)
}
acc[, `:=`(Accuracy, as.numeric(Accuracy))]
selectedcols <- colnames(as.data.table(get(acc[Accuracy == max(Accuracy)]$Model)$model))[2:ncol(get(acc[Accuracy == max(Accuracy)]$Model)$model)]
model <- glm(formula = Survived ~ glmAge + log_Fare + Pclass + Title + Cl_Relative, data = train, family = binomial(link = "logit"))
test[, `:=`(phat, predict.glm(model, newdata = test, type = "response"))]
test[, `:=`(Survived, ifelse(phat > 0.5, 1, 0))]
gender_submission <- merge(gender_submission[, 1], test[, c("PassengerId", "Survived")], by = "PassengerId")
fwrite(gender_submission, "submission_20171228_4.csv")
train[, .(mean(Survived), .N), .(Sex)]
summary(aov(formula = Survived ~ Sex, data = train))
train[, .(mean(Survived), .N), .(Embarked)]
summary(aov(formula = Survived ~ Embarked, data = train))
train[, .(mean(Survived), .N), .(Pclass)]
summary(aov(formula = Survived ~ Pclass, data = train))
train[, .(mean(Survived), .N), .(Cl_Fare)]
summary(aov(formula = Survived ~ Cl_Fare, data = train))
train[, .(mean(Survived), .N), .(Cl_Age)]
summary(aov(formula = Survived ~ Cl_Age, data = train))
train[, .(mean(Survived), .N), .(Cl_Relative)]
summary(aov(formula = Survived ~ Cl_Relative, data = train))
summary(aov(formula = Survived ~ as.factor(nTicket), data = train))
rows <- c("Pclass", "Title", "Embarked", "Cl_Fare", "Cl_Age", "Cl_Relative", "fac_nTicket")
objective <- "Survived"
fwrite(file = "submission_naivebayes_2.csv", x = merge(test[, 1:20], NBCalc(train, test, rows, objective)[, .(Survived = ifelse(mean(NaiveBayesProb) > 0.5, 1, 0)), .(fac_nTicket, Cl_Relative, Cl_Age, Cl_Fare, Embarked, Title, Pclass)], by = c("Pclass", "Title", "Embarked", "Cl_Fare", "Cl_Age", "Cl_Relative", "fac_nTicket"), all.x = TRUE)[, .(PassengerId, Survived)])
library("rpart")
library("randomForest")
library("partykit")
dt <- rpart(data = train, formula = Survived ~ Title + Pclass + Sex + Embarked + glmAge + Cl_Relative + log_Fare + nTicket + CabinDpt)
printcp(dt)
plotcp(dt)
plot(dt)
text(dt, use.n = TRUE, all = TRUE, cex = 0.8)
model_ct <- ctree(formula = Survived ~ Title + Pclass + Sex + Embarked + glmAge + Cl_Relative + log_Fare + nTicket, data = train)
test[, `:=`(prob, predict(object = model_ct, newdata = test))]
test[, `:=`(Survived, ifelse(prob > 0.5, 1, 0))]
fwrite(test[, .(PassengerId, Survived)], "submission_Ctree_1.csv")
model_ct2 <- ctree(formula = Survived ~ Title + Pclass + Sex + Embarked + glmAge + Cl_Relative + log_Fare + nTicket, data = train, control = ctree_control(minsplit = 31))
test[, `:=`(prob2, predict(object = model_ct2, newdata = test))]
test[, `:=`(Survived2, ifelse(prob2 > 0.5, 1, 0))]
fwrite(test[, .(PassengerId, Survived = Survived2)], "submission_Ctree_2.csv")
model_ct3 <- ctree(formula = Survived ~ Title + Pclass + Embarked + glmAge + Cl_Relative + log_Fare + nTicket, data = train)
test[, `:=`(prob3, predict(object = model_ct2, newdata = test))]
test[, `:=`(Survived3, ifelse(prob2 > 0.5, 1, 0))]
fwrite(test[, .(PassengerId, Survived = Survived3)], "submission_Ctree_3.csv")
