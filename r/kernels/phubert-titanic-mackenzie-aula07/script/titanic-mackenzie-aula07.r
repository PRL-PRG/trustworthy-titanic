
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

dados = read.csv('../input/train.csv', sep = ',', header = T)

# Árvore
library(rpart)
library(rpart.plot)

dados$Survived = as.factor(dados$Survived)

# Treinando a árvore para diferentes subamostras 
dadosBoot = sample_n(dados, size = nrow(dados), replace = T)
mod1 = rpart(Survived ~ Sex + Age + Pclass + SibSp + Parch + Fare, data = dadosBoot)
rpart.plot(mod1)

# Eliminando do data set as variáveis que não queremos usar para construir os algoritmos de previsão
dados = dados %>% select(-c('Name', 'Ticket', 'Cabin', 'Embarked'))
dados$Survived = factor(dados$Survived)
colnames(dados)


passageiro_teste = sample_n(dados, 1)

dados_train = dados %>% filter(PassengerId != passageiro_teste$PassengerId)

# Número de bootstraps para estimar a variância da previsão
nboot = 100

# Número de boostraps para o bagging
nbag = 30

# Número de variáveis para sortear a cada árvore
nvar = 3

i = 1

prev_arvore = matrix(nrow = nboot, ncol = 1)
prev_bag = matrix(nrow = nboot, ncol= 1)
prev_rf = matrix(nrow = nboot, ncol= 1)

dados_boot = sample_n(dados_train, nrow(dados_train), replace = T)
# Obtenho a árvore
arvore = rpart(data = dados_boot, Survived ~ Sex + Age + Pclass + SibSp + Parch + Fare)
# Obtenho a previsão
prev_arvore[i] = predict(arvore, passageiro_teste)[1]

# Verificando a variância das previsões

# Vamos fazer o seguinte: vou sortear um passageiro e retirá-lo do banco de dados
# Em seguida, aplico um bootstrapping nos passageiros restantes.
# Para cada amostra sorteada no bootstrapping, ajusto um modelo de árvore e calculo a previsão para o passageiro que foi separado.
# Também faço um bootstrapping em cima dessa nova amostra, e calculo a previsão feita usando o bagging.

passageiro_teste = sample_n(dados, 1)

dados_train = dados %>% filter(PassengerId != passageiro_teste$PassengerId)

# Número de bootstraps para estimar a variância da previsão
nboot = 100

# Número de boostraps para o bagging
nbag = 30

# Número de variáveis para sortear a cada árvore
nvar = 3

prev_arvore = matrix(nrow = nboot, ncol = 1)
prev_bag = matrix(nrow = nboot, ncol= 1)
prev_rf = matrix(nrow = nboot, ncol= 1)
for(i in 1:nboot) {
    # Obtenho a amostra
    dados_boot = sample_n(dados_train, nrow(dados_train), replace = T)
    # Obtenho a árvore
    arvore = rpart(data = dados_boot, Survived ~ Sex + Age + Pclass + SibSp + Parch + Fare)
    # Obtenho a previsão
    prev_arvore[i] = predict(arvore, passageiro_teste)[1]
    
    # Fazendo o bagging
    pbag = 0
    prf = 0
    for(j in 1:nbag) {
        dados_bag = sample_n(dados_boot, nrow(dados_boot), replace = T)
        arvore_tmp = rpart(data = dados_bag, Survived ~ Sex + Age + Pclass + SibSp + Parch + Fare)
        
        # Atualizando a previsão do bagging
        pbag = pbag + predict(arvore_tmp, passageiro_teste)[1]  / nbag
        
        # Incluindo a seleção aleatória de variáveis
        dados_rf = cbind(Survived = dados_bag$Survived, dados_bag %>% select(-c(PassengerId, Survived)) %>% sample(nvar))
        arvore_tmp = rpart(data = dados_rf, eval(paste0("Survived~", colnames(dados_rf %>% select(-Survived)))))    }
    
        # Atualizando a previsão do bagging + sav
        prf = prf + predict(arvore_tmp, passageiro_teste)[1] / nbag
    
    # Previsão final do bagging
    prev_bag[i] = pbag
    prev_rf[i] = prf
}

# Calculando o desvio-padrão das previsões em cada caso
sd(prev_arvore)
sd(prev_bag)
sd(prev_rf)

# Histogramas
dfplot = rbind(data.frame(prev = prev_arvore, Modelo = rep('Árvore', length(prev_arvore))), data.frame(prev = prev_bag, Modelo = rep('Bagging', length(prev_bag))), data.frame(prev = prev_rf, Modelo = rep('RF', length(prev_rf))))
g = ggplot(data = dfplot %>% subset(Modelo %in% c('Árvore', 'Bagging')), aes(x = prev, fill = Modelo))
g + geom_histogram(aes(y=..count../sum(..count..)), bins = 10, alpha = 0.3, position = 'identity') +
xlab("Previsão") + ylab("Frequência") + ggtitle("Previsões- árvore x bagging") +
theme(plot.title = element_text(size = 30, face = "bold"))

g = ggplot(data = dfplot, aes(x = prev, fill = Modelo))
g + geom_histogram(aes(y=..count../sum(..count..)), bins = 20, alpha = 0.3, position = 'identity') +
xlab("Previsão") + ylab("Frequência") + ggtitle("Previsões- árvore x bagging x rf") +
theme(plot.title = element_text(size = 25, face = "bold"))


# Pacote para random forest
library(randomForest)


# Preenchendo missing values de idade com a média. Dá pra melhorar...
dados[which(is.na(dados$Age)), 'Age'] = mean(dados$Age, na.rm = T)

# Grid para busca
g_ntree = c(10, 100, 200)
g_mtry = c(2, 3, 4, 5, 6)
g_maxnodes = c(3, 5, 10, 20, 40)

# Número de combinações
n = length(g_ntree) * length(g_mtry) * length(g_maxnodes)

# Parãmetro para cross-validation
n_fold = 10

# Número de indivíduos em cada fatia
n_ind = floor(nrow(dados) / n_fold)

# Primeiro vamos calcular quanto tempo demora para fazer o cross-validation com uma combinação
i_ntree = 2
i_mtry = 3
i_maxnodes = 4

inicio = Sys.time()

# Sorteio uma permutação das linhas do dataset
indices = sample(1:nrow(dados))

ntree = g_ntree[i_ntree]
mtry = g_mtry[i_mtry]
maxnodes = g_maxnodes[i_maxnodes]
acc = 0
# Para cada fatia
for(i in 1:n_fold) {
    test = dados[(indices[((i-1)*n_ind):(i*n_ind)]),] # Teste na fatia
    train = dados[-(indices[((i-1)*n_ind):(i*n_ind)]),] # Treino no restante
    
    modrf = randomForest(Survived~., data = train, ntree = ntree, mtry = mtry, maxnodes = maxnodes, na.action = na.roughfix)
    
    # Calculando as previsões
    prev = predict(modrf, test %>% select(-Survived))
    
    # Atualizando a acurácia média
    acc = acc + (sum(prev == test$Survived) / length(prev)) / n_fold   
}

fim = Sys.time()
t = (fim - inicio)

print(paste0("Tempo estimado para rodar todas as ", n, " combinações:"))
print(n*t)

# SE o tempo for razoável, vou rodar a busca exaustiva
inicio = Sys.time()

dfres = data.frame(ntree = integer(), mtry = integer(), maxnodes = integer(), acc = numeric(), stringsAsFactors = F)
# Sorteio uma permutação das linhas do dataset
indices = sample(1:nrow(dados))
for(i_ntree in 1:length(g_ntree)) {
    ntree = g_ntree[i_ntree]
    for(i_mtry in 1:length(g_mtry)){
         mtry = g_mtry[i_mtry]
        for(i_maxnodes in 1:length(g_maxnodes)) {
            maxnodes = g_maxnodes[i_maxnodes]
            
            acc = 0
            # Percorro as fatias do dataset
            for(i in 1:n_fold) {
                test = dados[(indices[((i-1)*n_ind):(i*n_ind)]),] # Teste na fatia
                train = dados[-(indices[((i-1)*n_ind):(i*n_ind)]),] # Treino no restante

                modrf = randomForest(Survived~., data = train, ntree = ntree, mtry = mtry, maxnodes = maxnodes, na.action = na.roughfix)

                # Calculando as previsões
                prev = predict(modrf, test %>% select(-Survived))

                # Atualizando a acurácia média
                acc = acc + (sum(prev == test$Survived) / length(prev)) / n_fold   
            }
            
            # Guardando os resultados
            linha = data.frame(ntree = ntree, mtry = mtry, maxnodes = maxnodes, acc = acc)
            dfres = rbind(dfres, linha)
            print(paste0("ntree = ", ntree, ", mtry = ", mtry, ", maxnodes = ", maxnodes, ", acc = ", acc))
        }
    }
}

fim = Sys.time()
t = (fim - inicio)

# Ordenando da maior acurácia para a menor
dfres = dfres[order(dfres$acc, decreasing = T), ]

# PEgando os melhores parametros
ntree = dfres[1, 'ntree']
mtry = dfres[1, 'mtry']
maxnodes = dfres[1, 'maxnodes']

print(paste0("Melhores parâmetros: ntree = ", ntree, ", mtry = ", mtry, ", maxnodes = ", maxnodes))

# Treinando o modelo com os melhores parâmetros e todos os dados
modrf_final = randomForest(Survived~., data = dados, ntree = ntree, mtry = mtry, maxnodes = maxnodes)

# Amostra de teste
test = read.csv('../input/test.csv')

# Preenchendo missing de Age e Fare
test[which(is.na(test$Age)), 'Age'] = mean(test$Age, na.rm = T)
test[which(is.na(test$Fare)), 'Fare'] = mean(test$Fare, na.rm = T)

# Calculando previsões para submeter à competição
res = predict(modrf_final, test)

submit = data.frame(PassengerId = test$PassengerId, Survived = res)
write.csv(submit, "submit_RF.csv", row.names = F)

submit


