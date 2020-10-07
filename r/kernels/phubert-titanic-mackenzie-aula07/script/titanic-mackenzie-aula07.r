library(tidyverse)
dados = read.csv("../input/train.csv", sep = ",", header = T)
library(rpart)
library(rpart.plot)
dados$Survived = as.factor(dados$Survived)
dadosBoot = sample_n(dados, size = nrow(dados), replace = T)
mod1 = rpart(Survived ~ Sex + Age + Pclass + SibSp + Parch + Fare, data = dadosBoot)
rpart.plot(mod1)
dados = dados %>% select(-c("Name", "Ticket", "Cabin", "Embarked"))
dados$Survived = factor(dados$Survived)
colnames(dados)
passageiro_teste = sample_n(dados, 1)
dados_train = dados %>% filter(PassengerId != passageiro_teste$PassengerId)
nboot = 100
nbag = 30
nvar = 3
i = 1
prev_arvore = matrix(nrow = nboot, ncol = 1)
prev_bag = matrix(nrow = nboot, ncol = 1)
prev_rf = matrix(nrow = nboot, ncol = 1)
dados_boot = sample_n(dados_train, nrow(dados_train), replace = T)
arvore = rpart(data = dados_boot, Survived ~ Sex + Age + Pclass + SibSp + Parch + Fare)
prev_arvore[i] = predict(arvore, passageiro_teste)[1]
passageiro_teste = sample_n(dados, 1)
dados_train = dados %>% filter(PassengerId != passageiro_teste$PassengerId)
nboot = 100
nbag = 30
nvar = 3
prev_arvore = matrix(nrow = nboot, ncol = 1)
prev_bag = matrix(nrow = nboot, ncol = 1)
prev_rf = matrix(nrow = nboot, ncol = 1)
for (i in 1:nboot) {
    dados_boot = sample_n(dados_train, nrow(dados_train), replace = T)
    arvore = rpart(data = dados_boot, Survived ~ Sex + Age + Pclass + SibSp + Parch + Fare)
    prev_arvore[i] = predict(arvore, passageiro_teste)[1]
    pbag = 0
    prf = 0
    for (j in 1:nbag) {
        dados_bag = sample_n(dados_boot, nrow(dados_boot), replace = T)
        arvore_tmp = rpart(data = dados_bag, Survived ~ Sex + Age + Pclass + SibSp + Parch + Fare)
        pbag = pbag + predict(arvore_tmp, passageiro_teste)[1]/nbag
        dados_rf = cbind(Survived = dados_bag$Survived, dados_bag %>% select(-c(PassengerId, Survived)) %>% sample(nvar))
        arvore_tmp = rpart(data = dados_rf, eval(paste0("Survived~", colnames(dados_rf %>% select(-Survived)))))
    }
    prf = prf + predict(arvore_tmp, passageiro_teste)[1]/nbag
    prev_bag[i] = pbag
    prev_rf[i] = prf
}
sd(prev_arvore)
sd(prev_bag)
sd(prev_rf)
dfplot = rbind(data.frame(prev = prev_arvore, Modelo = rep("Árvore", length(prev_arvore))), data.frame(prev = prev_bag, Modelo = rep("Bagging", length(prev_bag))), data.frame(prev = prev_rf, Modelo = rep("RF", length(prev_rf))))
g = ggplot(data = dfplot %>% subset(Modelo %in% c("Árvore", "Bagging")), aes(x = prev, fill = Modelo))
g + geom_histogram(aes(y = ..count../sum(..count..)), bins = 10, alpha = 0.3, position = "identity") + xlab("Previsão") + ylab("Frequência") + ggtitle("Previsões- árvore x bagging") + theme(plot.title = element_text(size = 30, face = "bold"))
g = ggplot(data = dfplot, aes(x = prev, fill = Modelo))
g + geom_histogram(aes(y = ..count../sum(..count..)), bins = 20, alpha = 0.3, position = "identity") + xlab("Previsão") + ylab("Frequência") + ggtitle("Previsões- árvore x bagging x rf") + theme(plot.title = element_text(size = 25, face = "bold"))
library(randomForest)
dados[which(is.na(dados$Age)), "Age"] = mean(dados$Age, na.rm = T)
g_ntree = c(10, 100, 200)
g_mtry = c(2, 3, 4, 5, 6)
g_maxnodes = c(3, 5, 10, 20, 40)
n = length(g_ntree) * length(g_mtry) * length(g_maxnodes)
n_fold = 10
n_ind = floor(nrow(dados)/n_fold)
i_ntree = 2
i_mtry = 3
i_maxnodes = 4
inicio = Sys.time()
indices = sample(1:nrow(dados))
ntree = g_ntree[i_ntree]
mtry = g_mtry[i_mtry]
maxnodes = g_maxnodes[i_maxnodes]
acc = 0
for (i in 1:n_fold) {
    test = dados[(indices[((i - 1) * n_ind):(i * n_ind)]), ]
    train = dados[-(indices[((i - 1) * n_ind):(i * n_ind)]), ]
    modrf = randomForest(Survived ~ ., data = train, ntree = ntree, mtry = mtry, maxnodes = maxnodes, na.action = na.roughfix)
    prev = predict(modrf, test %>% select(-Survived))
    acc = acc + (sum(prev == test$Survived)/length(prev))/n_fold
}
fim = Sys.time()
t = (fim - inicio)
print(paste0("Tempo estimado para rodar todas as ", n, " combinações:"))
print(n * t)
inicio = Sys.time()
dfres = data.frame(ntree = integer(), mtry = integer(), maxnodes = integer(), acc = numeric(), stringsAsFactors = F)
indices = sample(1:nrow(dados))
for (i_ntree in 1:length(g_ntree)) {
    ntree = g_ntree[i_ntree]
    for (i_mtry in 1:length(g_mtry)) {
        mtry = g_mtry[i_mtry]
        for (i_maxnodes in 1:length(g_maxnodes)) {
            maxnodes = g_maxnodes[i_maxnodes]
            acc = 0
            for (i in 1:n_fold) {
                test = dados[(indices[((i - 1) * n_ind):(i * n_ind)]), ]
                train = dados[-(indices[((i - 1) * n_ind):(i * n_ind)]), ]
                modrf = randomForest(Survived ~ ., data = train, ntree = ntree, mtry = mtry, maxnodes = maxnodes, na.action = na.roughfix)
                prev = predict(modrf, test %>% select(-Survived))
                acc = acc + (sum(prev == test$Survived)/length(prev))/n_fold
            }
            linha = data.frame(ntree = ntree, mtry = mtry, maxnodes = maxnodes, acc = acc)
            dfres = rbind(dfres, linha)
            print(paste0("ntree = ", ntree, ", mtry = ", mtry, ", maxnodes = ", maxnodes, ", acc = ", acc))
        }
    }
}
fim = Sys.time()
t = (fim - inicio)
dfres = dfres[order(dfres$acc, decreasing = T), ]
ntree = dfres[1, "ntree"]
mtry = dfres[1, "mtry"]
maxnodes = dfres[1, "maxnodes"]
print(paste0("Melhores parâmetros: ntree = ", ntree, ", mtry = ", mtry, ", maxnodes = ", maxnodes))
modrf_final = randomForest(Survived ~ ., data = dados, ntree = ntree, mtry = mtry, maxnodes = maxnodes)
test = read.csv("../input/test.csv")
test[which(is.na(test$Age)), "Age"] = mean(test$Age, na.rm = T)
test[which(is.na(test$Fare)), "Fare"] = mean(test$Fare, na.rm = T)
res = predict(modrf_final, test)
submit = data.frame(PassengerId = test$PassengerId, Survived = res)
write.csv(submit, "submit_RF.csv", row.names = F)
submit
