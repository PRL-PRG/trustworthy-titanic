
dados = read.csv("../input/train.csv",header = T)
head(dados);dim(dados)

str(dados)


dados = dados[,-c(4,9,11)]

# ha valores NA EM Age e Embarked
summary(dados)

by(dados$Embarked,dados$Pclass,table)
by(dados$Embarked,dados$Sex,table)
by(dados$Embarked,dados$SibSp,table)

which(dados$Embarked=="")
dados$Embarked[c(62,830)]="S"

by(dados$Age,dados$Pclass,summary)
by(dados$Age,dados$Sex,summary)
by(dados$Age,dados$SibSp,summary)
by(dados$Age,dados$Parch,summary)
by(dados$Age,dados$Embarked,summary)


dados$Age[c(which(is.na(dados$Age)&dados$Parch==2))]=16.5

dados$Age[c(which(is.na(dados$Age)&dados$Pclass==1))]=37
dados$Age[c(which(is.na(dados$Age)&dados$Pclass==2))]=29
dados$Age[c(which(is.na(dados$Age)&dados$Pclass==3))]=24

sum(apply(dados,2,is.na)) # nao ha mais valores ausentes, podendo portanto passar aos modelos

library(dplyr)
set.seed(70815)
treino = sample_n(dados,800,replace = F)
teste = dados[-as.integer(row.names(treino)),]


modelo_rg = glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,
                family = "binomial",data = treino)
summary(modelo_rg)

# Fare nao significativo
modelo_rg = update(modelo_rg,.~.-Fare)
summary(modelo_rg)

# Parch nao significativo
modelo_rg = update(modelo_rg,.~.-Parch)
summary(modelo_rg)

pchisq(modelo_rg$deviance,df=modelo_rg$df.residual,lower.tail = F)
# deviance nao significativa, portanto, modelo adequado

library(hnp)
set.seed(5047774)
hnp(modelo_rg,halfnormal = F,paint.out = T,pch=16,sim=500,print.on = T)
# somente 3.88% dos pontos ficaram fora do envelope

library(ROCR)

# Encontra os valores preditos para as probabilidades

predictionTreino=predict(modelo_rg,treino,type="response")

pred = prediction(predictionTreino, treino$Survived)

# Calcula verdadeiros positivos e falsos positivos
perf = performance(pred,"tpr", "fpr")
plot(perf,colorize = TRUE,main="Curva ROC - Conj. de Treino")
abline(0, 1, lty = 2)

# Plota sensibilidade e especificidade
perf1 = performance(pred, "sens", "spec")
plot(perf1,main="Sensibilidade e Especificidade - Conj. de Treino")


## Calcula area abaixo da curva
auc=(performance(pred,"auc")@y.values)[[1]]
auc # 0.8562058

# utilizando o conjunto de teste 

# Encontra os valores preditos para as probabilidades
predictionTeste=predict(modelo_rg,teste,type="response")

pred_GLM = prediction(predictionTeste, teste$Survived)

# Calcula verdadeiros positivos e falsos positivos
perf = performance(pred_GLM,"tpr", "fpr")
plot(perf,colorize = TRUE,main="Curva ROC - Conj. de Teste")
abline(0, 1, lty = 2)

# Plota sensibilidade e especificidade
perf1 = performance(pred_GLM, "sens", "spec")
plot(perf1,main="Sensibilidade e Especificidade - Conj. de Teste")

## Calcula area abaixo da curva
auc=(performance(pred_GLM,"auc")@y.values)[[1]]
auc # 0.8598485

pred_teste=ifelse(predictionTeste>0.50,1,0) # definindo ponto de corte igual a 0.5
# acuracia
1-ks::compare(pred_teste, teste$Survived)$error # 0.7912088

# definindo um ponto de corte ideal
corte=seq(0.35,0.85,0.01)
acuracia = c()
for(i in 1:length(corte)){
  pred_teste=ifelse(predictionTeste>corte[i],1,0)
  acuracia[i]=1-ks::compare(pred_teste, teste$Survived)$error
}
plot(corte,acuracia,type="l")
max(acuracia) # 0.8461538
corte[which(acuracia==max(acuracia))]
# 0.56 0.57 0.58

# verificando o corte otimo com os dados de treino
corte=seq(0.35,0.85,0.01)
acuracia = c()
for(i in 1:length(corte)){
  pred_treino=ifelse(predictionTreino>corte[i],1,0)
  acuracia[i]=1-ks::compare(pred_treino, treino$Survived)$error
}
plot(corte,acuracia,type="l")
max(acuracia) # 0.8175
corte[which(acuracia==max(acuracia))]
# 0.58 0.62
# o ponto de corte 0.58 aparece como o melhor com os 
# dados de treino e tbm com os dados de teste 

dados_nb = treino
dados_nb$Survived = as.factor(dados_nb$Survived)

modelo_nb = naivebayes::naive_bayes(Survived~Pclass+Sex+Age+SibSp+
                                      Parch+Fare+Embarked, data = dados_nb)
y_estimado_nb = predict(modelo_nb, newdata = teste[,-c(1,2)], type = "class")

# acuracia 
1-ks::compare(teste$Survived,y_estimado_nb)$error
# 0.8021978

x_treino = treino[,-c(1,2)] # coluna PassengerId tbm deve ser desconsiderada
head(x_treino)

# passando as variaveis para numericas 
summary(x_treino$Age)
x_treino$Age = cut(x_treino$Age,breaks = c(0,22,26,37,80),labels = c(1,2,3,4))
summary(x_treino$Age)

summary(x_treino$Fare)
x_treino$Fare = cut(x_treino$Fare,breaks = c(-1,7.896,14.456,31.069,513),
                    labels = c(1,2,3,4))

x_treino$Sex = ifelse(x_treino$Sex=="male",1,2)
x_treino$Embarked = ifelse(x_treino$Embarked=="C",1,
                           ifelse(x_treino$Embarked=="Q",2,3))
y_treino = treino$Survived

x_teste = teste[,-c(1,2)]

x_teste$Age = cut(x_teste$Age,breaks = c(0,22,26,37,80),labels = c(1,2,3,4))

x_teste$Fare = cut(x_teste$Fare,breaks = c(-1,7.896,14.456,31.069,513),
                    labels = c(1,2,3,4))

x_teste$Sex = ifelse(x_teste$Sex=="male",1,2)
x_teste$Embarked = ifelse(x_teste$Embarked=="C",1,
                           ifelse(x_teste$Embarked=="Q",2,3))
y_teste = teste$Survived

x_treino = data.frame(apply(x_treino,2,as.numeric))
x_teste = data.frame(apply(x_teste,2,as.numeric))


k = 5
set.seed(2900741)
y_estimado = class::knn(x_treino, x_teste, y_treino,
                        k = k ,prob = F)
# acuracia
1-ks::compare(y_teste,y_estimado)$error # 0.9010989

acuracia=c()
for(i in 1:35){
  y_estimado = class::knn(x_treino, x_teste, y_treino,
                          k = i ,prob = F)
  acuracia[i]=1-ks::compare(y_teste,y_estimado)$error
}
k=1:length(acuracia)
plot(k,acuracia,type = "l",main="")

max(acuracia);k[which(acuracia==max(acuracia))]
# 0.9010989 ; 5

# mlp {RSNNS}
set.seed(12345)
modelo_mlp = RSNNS::mlp(x_treino, y_treino, size = c(4,4),maxit = 30000, 
                 inputsTest = x_teste, targetsTest = y_teste)
# size defini a arquitetuta da rede, numero de camadas intermediarias,
# por exemplo: size=c(4,4) gera uma rede com 4 camadas intermediarias cada uma com
# 4 neuronios

# Não normalizamos os dados, no entanto, redes neurais é sempre melhor trabalhar com dados normalizados

y_estimado_rn = round(modelo_mlp$fittedTestValues,0)

## acuracia
1-ks::compare(y_teste,y_estimado_rn)$error
# 0.8681319

# plot do erro quadratico medio
plot(modelo_mlp$IterativeFitError,type="n",main="Curva de Aprendizagem",xlab="Iteraçao",
     ylab="Erro medio quadrado")
lines(modelo_mlp$IterativeFitError,col="1",lwd=3,cex=2)
# deseja-se que a curva de aprendizagem do erro medio va para 0 a medida que aumenta o 
# numero de iterações, como nao foi o caso

########################## realizando predicoes com os dados de test ####################################

################################### regressao logistica ################################################

test = read.csv("../input/test.csv",header = T)
head(test)
summary(test)

# retirando variaveis irrelevantes para analises subsequentes:
# Name, Ticket e Cabin
dados = test[,-c(3,8,10)]

# ha valores NA em Age e Fare 
# como Fare nao foi significativa no modelo de regresao 
# so imputaremos a variavel Age da mesma forma que fizemos anteriormente
# individuos com Parch 2 sera imputada a mediana da idade desta
# categoria, no caso, 16.5
dados$Age[c(which(is.na(dados$Age)&dados$Parch==2))]=16.5
# para os demais valores ausentes sera imputada a mediana 
# levando em consideracao a variavel Pclass

dados$Age[c(which(is.na(dados$Age)&dados$Pclass==1))]=37
dados$Age[c(which(is.na(dados$Age)&dados$Pclass==2))]=29
dados$Age[c(which(is.na(dados$Age)&dados$Pclass==3))]=24

summary(dados)
# nao ha mais valores ausentes, exceto em Fare

summary(modelo_rg)

# Encontra os valores preditos para as probabilidades
predictionTeste=predict(modelo_rg,dados,type="response")

pred_teste=ifelse(predictionTeste>0.58,1,0) # definindo ponto de corte igual a 0.58
submission_rg = data.frame(PassengerId=dados$PassengerId,Survived=pred_teste)
head(submission_rg)

# write.csv(submission_rg,file = "submission_rg.csv",row.names = F)

################################### naive bayes ##########################################################

y_estimado_nb = predict(modelo_nb, newdata = dados, type = "class")
submission_nb = data.frame(PassengerId=dados$PassengerId,Survived=y_estimado_nb)
head(submission_nb)
# write.csv(submission_nb,file = "submission_nb.csv",row.names = F)


##################################### knn ###############################################################


x_teste = dados[,-c(1)]

x_teste$Age = cut(x_teste$Age,breaks = c(0,22,26,37,80),labels = c(1,2,3,4))
summary(x_teste$Age)


# ha um valor NA em Fare, atribuiremos a este o mediana da variavel
x_teste$Fare[which(is.na(x_teste$Fare))]=median(x_teste$Fare,na.rm = T)
x_teste$Fare = cut(x_teste$Fare,breaks = c(-1,7.896,14.456,31.069,513),
                   labels = c(1,2,3,4))
summary(x_teste$Fare)


x_teste$Sex = ifelse(x_teste$Sex=="male",1,2)
x_teste$Embarked = ifelse(x_teste$Embarked=="C",1,
                          ifelse(x_teste$Embarked=="Q",2,3))


x_teste = data.frame(apply(x_teste,2,as.numeric))

k = 5
set.seed(2900741)
y_estimado_knn = class::knn(x_treino, x_teste, y_treino,
                        k = k ,prob = F)
submission_knn = data.frame(PassengerId=dados$PassengerId,Survived=y_estimado_knn)
head(submission_knn)
# write.csv(submission_knn,file = "submission_knn.csv",row.names = F)

####################################### rede neural ###################################################
set.seed(12345)
predictions = predict(modelo_mlp,x_teste)
y_estimado_rn = round(predictions,0)
submission_rn = data.frame(PassengerId=dados$PassengerId,Survived=as.vector(y_estimado_rn))
head(submission_rn)
# write.csv(submission_rn,file = "submission_rn.csv",row.names = F)
