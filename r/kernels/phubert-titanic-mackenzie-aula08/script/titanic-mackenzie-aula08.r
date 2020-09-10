
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

dados = read.csv('../input/train.csv', sep = ',', header = T)

# Convertendo sexo para numérica (1 = male)
dados$Sexo = 0
dados[which(dados$Sex == 'Male'), 'Sexo'] = 1

dados = dados %>% select(c('Survived', 'Age', 'Sexo', 'Fare', 'SibSp', 'Parch', 'Pclass'))

# XGBoost
library(xgboost)

# Caret para fazer o split da amostra (treino e teste)
library(caret)

# Número de linhas no data frame
nrow(dados)

# Dividindo a amostra em treino e teste

p_treino = .9 # proporção de casos para amostra de treino

# Usando o caret, podemos dividir a amostra preservando a proporção entre sobreviventes e não-sobreviventes
indice_treino = createDataPartition(dados$Survived, times = 1, p = p_treino)

# Testando
table(dados[indice_treino$Resample1, 'Survived']) / sum(table(dados[indice_treino$Resample1, 'Survived']))
table(dados[-indice_treino$Resample1, 'Survived']) / sum(table(dados[-indice_treino$Resample1, 'Survived']))

# Criando amostra de treino e teste
train = dados[indice_treino$Resample1,]
test = dados[-indice_treino$Resample1, ]

X_train = as.matrix(train %>% select(c(Sexo, Fare, Age, Parch, SibSp, Pclass)))
y_train = as.matrix(train$Survived)

X_test = as.matrix(test %>% select(c(Sexo, Fare, Age, Parch, SibSp, Pclass)))
y_test = as.matrix(test$Survived)

# Treinando um modelo na amostra de treino
lista_n = c(3, 10, 100, 200, 500, 1000)
res = data.frame(nrounds = NA, erro_treino = NA, erro_teste = NA)
for(i in 1:length(lista_n)) {
    print(paste0("Rodando xgboost com ", lista_n[i], " rounds."))
    mod1 = xgboost(data = X_train, label = y_train, nrounds = lista_n[i], verbose = 0, objective = 'binary:logistic')
    erro_treino = mod1$evaluation_log$train_error[length(mod1$evaluation_log$train_error)]
    y_pred = predict(mod1, X_test)    
    y_pred = ifelse(y_pred > 0, 1, 0)
    tmp = data.frame(previsto = y_pred, real = y_test)
    erro_teste = 1 - sum(tmp$previsto == tmp$real) / nrow(tmp)
    res = rbind(res, data.frame(nrounds = lista_n[i], erro_treino = erro_treino, erro_teste = erro_teste))
}


dt.plot = data.frame(x = mod1$evaluation_log$iter, y = mod1$evaluation_log$train_error)
g = ggplot(data = dt.plot, aes(x = x, y = y))
g + geom_line() + geom_point() + annotate('text', label = 'Note como o erro de treino não para de cair', x = 500, y = 0.15, color = 'black')

# Fazendo cross-validation 

# Construindo as matrizes para o xgboost
X = as.matrix(dados %>% select(c(Sexo, Fare, Age, Parch, SibSp, Pclass)))
y = as.matrix(dados$Survived)

# Teste: executando o cross-validation uma vez
# nfold = 5 significa que a função vai dividir a base de dados em 5 fatias e vai treinar o modelo 5 vezes (cada vez deixando uma fatia de fora
# para fazer o papel de amostra de teste)
cv.res = xgb.cv(data = X, label = y, nfold = 5, nrounds = 100, max_depth = 6, objective = 'binary:logistic')

# Fazendo o gráfico do erro de treino x erro de teste
dt.plot = data.frame(iter = cv.res$evaluation_log$iter, test = cv.res$evaluation_log$test_error_mean, train = cv.res$evaluation_log$train_error_mean)

# Preciso dessa biblioteca para usar a função melt
library(reshape)

# Essa função tem o efeito de transformar duas colunas (test e train) em uma coluna "valor" e uma coluna "variável"
# Por exemplo, em vez de ter uma linha
#
#   id  |  test  |  train
#   1   |   0.5  |   0.3
#
# Passamos a ter duas linhas
#
#   id  |  variable  |  value
#    1  |   test     | 0.5
#    1  |   train    | 0.3
#
# Essa operação é útil para fazermos o gráfico das duas linhas com cores diferentes, conforme ggplot abaixo
dt.plot = melt(data = dt.plot, measure.vars = c('test', 'train'), id.vars = 'iter')

g = ggplot(data = dt.plot, aes(x = iter, y = value, color = variable))
g + geom_line() + geom_point() + labs(color = 'Erro') + xlab("Número de modelos") + ylab("Erro") + 
annotate("text", label = "Note como o erro de teste para de cair, \nenquanto o erro de treino continua caindo...", x = 25, y = 0.25, color = "black") 

# Fazendo cross-validation para um parâmetro (max_depth)

# Lista de valores possíveis
lista_max_depth = c(1, 2, 3, 6, 10, 20)

df.erro = data.frame(max_depth = NA, erro_medio = NA)
for(l in lista_max_depth) {
    print(paste0("Testando max_depth = ", l))
    # Rodo o cross-validation com o valor atual de lista_max_depth
    cv.res = xgb.cv(data = X, label = y, nfold = 5, nrounds = 50, max_depth = l, verbose = F, objective='binary:logistic')
    
    # Guardo o erro de teste médio
    erro = mean(cv.res$evaluation_log$test_error_mean)
    df.erro = rbind(df.erro, data.frame(max_depth = l, erro_medio = erro))
}
df.erro = df.erro[2:nrow(df.erro),]

# Visualizando resultados
# Valor ótimo foi max_depth = 3
df.erro

# Rodando cross-validation para dois parâmetros
# Existem vários outros parâmetros que podem ser otimizados
# Ver a lista em https://xgboost.readthedocs.io/en/latest/parameter.html#parameters-for-tree-booster
lista_max_depth = c(2, 3, 10)
lista_colsample_bytree = c(2/6,3/6,4/6,5/6)

df.res = data.frame(max_depth = NA, colsample_bytree = NA, erro_medio = NA)
for(max_depth in lista_max_depth) {
    for(colsample_bytree in lista_colsample_bytree){
        print(paste0("Testando max_depth = ", max_depth, " e colsample_bytree = ", colsample_bytree))
        # Rodo o cross-validation com o valor atual dos parâmetros
        cv.res = xgb.cv(data = X, label = y, nfold = 5, nrounds = 50, max_depth = max_depth, colsample_bytree = colsample_bytree, verbose = F, objective='binary:logistic')

        # Guardo o erro de teste médio
        erro = mean(cv.res$evaluation_log$test_error_mean)
        df.res = rbind(df.res, data.frame(max_depth = max_depth, colsample_bytree = colsample_bytree, erro_medio = erro))
    }
}

df.res = df.res[2:nrow(df.res), ]

# Avaliando resultados
# Ordeno o data frame em ordem crescente do erro_medio
df.res[order(df.res$erro_medio),]
# O melhor resultado foi com max_depth = 3 e colsample_bytree = 0.333 

# Treinando o modelo com os melhores parâmetros
max_depth = 3
colsample_bytree = 1 / 3
mod.opt = xgboost(data = X, label = y, nrounds = 50, verbose = 0, max_depth = max_depth, colsample_bytree = colsample_bytree, objective = 'binary:logistic')


# Amostra de teste Kaggle
test = read.csv('../input/test.csv')

# Preenchendo missing de Age e Fare
test[which(is.na(test$Age)), 'Age'] = mean(test$Age, na.rm = T)
test[which(is.na(test$Fare)), 'Fare'] = mean(test$Fare, na.rm = T)
test$Sexo = 0
test[which(dados$Sex == 'Male'), 'Sexo'] = 1

X_test = as.matrix(test %>% select(c(Sexo, Fare, Age, Parch, SibSp, Pclass)))

# Calculando previsões para submeter à competição
res = predict(mod.opt, X_test)

# Aplicando o threshold: se a probabilidade prevista for maior que t, considero que a previsão é "sobrevivente"
t = 0.5
pres = ifelse(res > t, 1, 0)

submit = data.frame(PassengerId = test$PassengerId, Survived = pres)
write.csv(submit, "submit_XG.csv", row.names = F)


