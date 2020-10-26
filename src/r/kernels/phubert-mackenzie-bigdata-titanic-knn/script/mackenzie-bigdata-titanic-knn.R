library(tidyverse)
library(class)
dados = read.csv("../input/train.csv", sep = ",", header = T)
colnames(dados)
head(dados)
dados = dados %>% subset(select = -c(PassengerId, Ticket, Name, Cabin, Embarked))
colnames(dados)
colSums(is.na(dados))
dados[which(is.na(dados$Age)), "Age"] = mean(dados$Age, na.rm = T)
dados$Sex = as.character(dados$Sex)
dados[which(dados$Sex == "male"), "Sex"] = 1
dados[which(dados$Sex == "female"), "Sex"] = 0
dados$Sex = as.numeric(dados$Sex)
for (i in 1:ncol(dados)) {
    dados[, i] = as.numeric(dados[, i])
    dados[, i] = (dados[, i] - min(dados[, i]))/(max(dados[, i]) - min(dados[, i]))
}
X = dados %>% subset(select = -Survived)
y = dados %>% subset(select = Survived)
maxk = 15
loo.res = matrix(NA, nrow = maxk)
N = nrow(dados)
for (k in 1:maxk) {
    tmp = 0
    for (i in 1:N) {
        Xtest = X[i, ]
        ytest = y[i, ]
        Xtrain = X[-i, ]
        ytrain = y[-i, ]
        mod.knn = knn(train = Xtrain, test = Xtest, cl = ytrain, k = k, prob = FALSE)
        tmp = tmp + ((ytest - as.numeric(levels(mod.knn))[mod.knn])^2)/N
    }
    print(paste0("k ", k, " - accuracy = ", tmp))
    loo.res[k] = tmp
}
plot(loo.res, type = "b")
print(paste0("Melhor k = ", which.max(loo.res)))
test = read.csv("../input/test.csv", stringsAsFactors = F)
colnames(test)
test = test %>% subset(select = -c(Name, Ticket, Cabin, Embarked))
colSums(is.na(test))
test[which(is.na(test$Age)), "Age"] = mean(test$Age, na.rm = T)
test[which(is.na(test$Fare)), "Fare"] = mean(test$Fare, na.rm = T)
test[which(test$Sex == "male"), "Sex"] = 1
test[which(test$Sex == "female"), "Sex"] = 0
test$Sex = as.numeric(test$Sex)
summary(test)
for (i in ncol(test)) {
    test[, i] = (test[, i] - min(test[, i]))/(max(test[, i]) - min(test[, i]))
}
k = which.max(loo.res)
pred = knn(train = X, test = test[, 2:ncol(test)], cl = y[, ], k, prob = F)
df = data.frame(PassengerID = test[, 1], Survived = pred)
write.csv(df, file = "knn_submit.csv", row.names = F)
