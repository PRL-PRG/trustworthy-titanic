
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

dados = read.csv('../input/train.csv', sep = ',', header = T)

# Survived x Fare
library(ggplot2)

g = ggplot(data = dados, aes(group = Survived, y = Fare, x = Survived))
g + geom_boxplot(fill = 'lightblue') + xlab("Sobrevivente (1 = sobrevivente)") + ylab("Preço da passagem (limitado a $200)") + ylim(c(0,200))


# Média de Fare 
dados %>% group_by(Survived) %>% summarise('media' = mean(Fare))

# Usando o classificador simples: se Fare > 35, survived = 1
ypred = ifelse(dados$Fare > 35, 1, 0)

# Obtendo a matriz de confusão
library(caret)
confusionMatrix(factor(ypred), factor(dados$Survived), positive = '1')

# Testando a acurácia de vários pontos de corte
tmin = min(dados$Fare)
tmax = max(dados$Fare)

# Número de pontos de corte para testar (entre tmin e tmax)
npontos = 1000
pontos = seq(from = tmin, to = tmax, length.out = npontos)

res = as.data.frame(matrix(ncol=4, nrow=0))
for(i in 1:npontos) {
    t = pontos[i]
    ypred = ifelse(dados$Fare < t, 0, 1)
    m = confusionMatrix(factor(ypred), factor(dados$Survived))
    a = m$overall['Accuracy']
    tb = m$table
    p = tb[2,2] / sum(tb[,2])
    r = tb[2, 1] / sum(tb[,1])
    res[i, 1] = t
    res[i, 2] = a
    res[i, 3] = p
    res[i, 4] = r
}
colnames(res) = c("corte", "acuracia", "tpr", "fpr")

resFare = res
# Ponto ótimo
topt = res[which.max(res$acuracia),1]
print(paste0("Corte ótimo = ", topt))

# ROC curve
g = ggplot(data = res, aes(y = tpr, x = fpr))
g + geom_line() + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "blue", linetype = 'dashed') +
xlab('Taxa de falsos positivos (frequência de alarme falso)') + ylab('Taxa de verdadeiros positivos (recall)') +
ggtitle("ROC - Fare")

# Plotando os resultados
g = ggplot(data = res, aes(x = corte, y = acuracia))
g + geom_line() + xlab('Ponto de corte') + ylab('Acurácia')

g = ggplot(data = res[which(res$corte < 100),], aes(x = corte, y = acuracia))
g + geom_line() + geom_point() + xlab('Ponto de corte') + ylab('Acurácia') +
geom_vline(xintercept = topt, colour = 'red', linetype = 'dashed')

# Repetindo para Parch
g = ggplot(data = dados, aes(group = Survived, y = Parch, x = Survived))
g + geom_boxplot(fill = 'lightblue') + xlab("Sobrevivente (1 = sobrevivente)") + ylab("Parentes a bordo") 

# Testando a acurácia de vários pontos de corte
tmin = min(dados$Parch)
tmax = max(dados$Parch)

# Número de pontos de corte para testar (entre tmin e tmax)
npontos = 1000
pontos = seq(from = tmin, to = tmax, length.out = npontos)

res = as.data.frame(matrix(ncol=2, nrow=0))
for(i in 1:npontos) {
    t = pontos[i]
    ypred = ifelse(dados$Parch < t, 0, 1)
    m = confusionMatrix(factor(ypred), factor(dados$Survived))
    a = m$overall['Accuracy']
    tb = m$table
    p = tb[2,2] / sum(tb[,2])
    r = tb[2, 1] / sum(tb[,1])
    res[i, 1] = t
    res[i, 2] = a
    res[i, 3] = p
    res[i, 4] = r
}
colnames(res) = c("corte", "acuracia", "tpr", "fpr")

resParch = res
# Ponto ótimo
topt = res[which.max(res$acuracia),1]
print(paste0("Corte ótimo = ", topt))

# ROC curve
g = ggplot(data = res, aes(y = tpr, x = fpr))
g + geom_line() + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "blue", linetype = 'dashed') +
xlab('Taxa de falsos positivos (frequência de alarme falso)') + ylab('Taxa de verdadeiros positivos (recall)') +
ggtitle("ROC - Parch")

# Plotando os resultados
g = ggplot(data = res, aes(x = corte, y = acuracia))
g + geom_line() + xlab('Ponto de corte') + ylab('Acurácia') + geom_point()



# ROC Parch x Fare
dfRoc = rbind(cbind.data.frame(variavel = rep('Fare', nrow(resFare)), tpr = resFare$tpr, fpr = resFare$fpr), cbind.data.frame(variavel = rep('Parch', nrow(resParch)), tpr = resParch$tpr, fpr = resParch$fpr))

g = ggplot(data = dfRoc, aes(y = tpr, x = fpr, color = variavel))
g + geom_line() + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "blue", linetype = 'dashed') +
xlab('Taxa de falsos positivos (frequência de alarme falso)') + ylab('Taxa de verdadeiros positivos (recall)') +
ggtitle("ROC")

# Gráfico Parch x Fare
g = ggplot(data =dados, aes(x = Parch, y = Fare, color = factor(Survived)))
g + geom_point() + guides(color = F)

g = ggplot(data =dados, aes(x = Parch, y = Fare, color = factor(Survived)))
g + geom_point() + guides(color = F) + geom_hline(yintercept = 50.77, color = 'red', linetype = 'dashed')

g = ggplot(data =dados, aes(x = Parch, y = Fare, color = factor(Survived)))
g + geom_point() + guides(color = F) + geom_hline(yintercept = 50.77, color = 'red', linetype = 'dashed') +
geom_vline(xintercept = 0.5, color = 'blue', linetype = 'dashed' )

# Calculando a proporção de sobreviventes em cada região
dados %>% subset(Fare < 50.77 & Parch == 0) %>% summarise(p = sum(Survived) / n())
dados %>% subset(Fare < 50.77 & Parch > 0) %>% summarise(p = sum(Survived) / n())
dados %>% subset(Fare >= 50.77 & Parch == 0) %>% summarise(p = sum(Survived) / n())
dados %>% subset(Fare >= 50.77 & Parch > 0) %>% summarise(p = sum(Survived) / n())


# Ponto de corte ótimo para cada região
tmin = min(dados$Parch)
tmax = max(dados$Parch)

# Número de pontos de corte para testar (entre tmin e tmax)
npontos = 1000
pontos = seq(from = tmin, to = tmax, length.out = npontos)

tmp = dados %>% subset(Fare < 50.77)
res = as.data.frame(matrix(ncol=2, nrow=0))
for(i in 1:npontos) {
    t = pontos[i]
    ypred = ifelse(tmp$Parch < t, 0, 1)
    m = confusionMatrix(factor(ypred), factor(tmp$Survived))
    a = m$overall['Accuracy']
    res[i, 1] = t
    res[i, 2] = a
}
colnames(res) = c("corte", "acuracia")

# Ponto ótimo
topt1 = res[which.max(res$acuracia),1]
print(paste0("Corte ótimo = ", topt1))

tmp = dados %>% subset(Fare >= 50.77)
res = as.data.frame(matrix(ncol=2, nrow=0))
for(i in 1:npontos) {
    t = pontos[i]
    ypred = ifelse(tmp$Parch < t, 0, 1)
    m = confusionMatrix(factor(ypred), factor(tmp$Survived))
    a = m$overall['Accuracy']
    res[i, 1] = t
    res[i, 2] = a
}
colnames(res) = c("corte", "acuracia")

# Ponto ótimo
topt2 = res[which.max(res$acuracia),1]
print(paste0("Corte ótimo = ", topt2))

# Gráfico da região de decisão
g = ggplot(data =dados, aes(x = Parch, y = Fare, color = factor(Survived)))
g + geom_point() + guides(color = F) + geom_hline(yintercept = 50.77, color = 'red', linetype = 'dashed') +
 geom_segment(aes(x = 5.5, y = -1, xend = 5.5, yend = 50.77), color = "blue", linetype = 'dashed') +
geom_segment(aes(x = 0.5, y = 50.77, xend = 0.5, yend = 550), color = "blue", linetype = 'dashed') +
scale_y_continuous(expand = c(0, 0))

# Calculando a proporção de sobreviventes em cada região
dados %>% subset(Fare < 50.77 & Parch <= 5) %>% summarise(p = sum(Survived) / n())
dados %>% subset(Fare < 50.77 & Parch > 5) %>% summarise(p = sum(Survived) / n())
dados %>% subset(Fare >= 50.77 & Parch == 0) %>% summarise(p = sum(Survived) / n())
dados %>% subset(Fare >= 50.77 & Parch > 0) %>% summarise(p = sum(Survived) / n())

# Árvore
library(rpart)
library(rpart.plot)

dados$Survived = as.factor(dados$Survived)

# Treinando uma árvore com apenas três variáveis
mod1 = rpart(Survived ~ Sex + Age + Pclass, data = dados)

# Visualizando a árvore resultante
rpart.plot(mod1)

# Resumo do processo de treinamento
summary(mod1)

# Vamos avaliar o ganho de previsão a cada novo split da árvore
# Esse ganho é chamado "complexity parameter" (CP)
printcp(mod1)

# Visualizando a relação entre complexidade do modelo e erro de previsão
# Eixo horizontal: complexidade
# Eixo vertical: média e desvio padrão do erro no cross-validation
plotcp(mod1)

# Podando a árvore: escolhemos um valor c de CP, e eliminamos todos os splits que não tenham ganho de qualidade de no mínimo c
# Um método usual é escolher como corte para o CP o valor que forneceu o mínimo erro de cross-validation (coluna xerror da cptable)
# Neste caso, o menor erro de cross validation foi no mínimo valor de cp; portanto a poda não terá efeito
pmod = prune(mod1, mod1$cptable[which.min(mod1$cptable[,"xerror"]),"CP"])
rpart.plot(pmod)

# Para efeito de teste, vamos podar a árvore com um valor maior de complexidade
pmod = prune(mod1, 0.02)
rpart.plot(pmod)

# Vamos observar agora as previsões da árvore
# O método predict vai fornecer a previsão do modelo
# No caso da classificação binária, a previsão é uma probabilidade (P(y = 0) ou P(y=1))
prob = predict(mod1, dados %>% select(c(Age, Sex, Pclass)))

# Modelo com mais variáveis
mod2 = rpart(Survived ~ Sex + Age + Pclass + SibSp + Parch + Fare, data = dados)
prob2 = predict(mod2, dados %>% select(c(Age, Sex, Pclass, Parch, SibSp, Fare)))

mod3 = rpart(Survived ~ Sex + Age + Pclass + SibSp + Parch + Fare + Embarked, data = dados)
prob3 = predict(mod3, dados)

# Pergunta: a parrtir de qual valor da probabilidade de y = 1 devemos classificar o indivíduo como sobrevivente?
# Vamos observar o que acontece na curva ROC
# Utilizaremos o pacote AUC
library(AUC)

roc1 = roc(prob[,2], dados$Survived)
roc2 = roc(prob2[,2], dados$Survived)
roc3 = roc(prob3[,2], dados$Survived)
plot(roc1)
plot(roc2, add = T, col = 'blue')
plot(roc3, add = T, col = 'red')

# Calculando a área sob a curva
auc(roc1)
auc(roc2)
auc(roc3)

plotcp(mod2)

pmod2 = prune(mod2, 0.027)
rpart.plot(pmod2)

# Amostra de teste
test = read.csv('../input/test.csv')

# Calculando previsões para submeter à competição
res = predict(mod2, test)

submit = data.frame(PassengerId = test$PassengerId, Survived = ifelse(res[,2] > 0.5, 1, 0))
write.csv(submit, "submit_CART.csv", row.names = F)


