
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

# Gerando dados linearmente separáveis
library(MASS)
# Gero as features da classe 1 da normal bivariada com média (-3, 3) e variâncias (1, 1)
# Serão 20 indivíduos da classe 1
xy_classe1 = mvrnorm(n = 20, mu = matrix(c(-3, 3), 2, 1), Sigma = matrix(c(1,0,0,1), 2, 2))

# Gero as features da classe -1 da normal bivariada com média (3, -3) e variância (1,1)
# 20 indivíduos da classe -1
xy_classe2 = mvrnorm(n = 20, m = matrix(c(0, 0), 2, 1), Sigma = matrix(c(1,0,0,1), 2, 2))

# Crio o dataframe
dfsim = rbind(data.frame(x = xy_classe1[,1], y = xy_classe1[,2], classe = rep(x = 1, times = nrow(xy_classe1))), data.frame(x = xy_classe2[,1], y = xy_classe2[,2], classe = rep(x = -1, times = nrow(xy_classe1))))
dfsim$classe = factor(dfsim$classe)

# Gráfico das classes
g = ggplot(data = dfsim, aes(x = x, y = y, color = classe))
g + geom_point() + labs(color = 'Classe')

# Incluindo algumas retas separadoras
g = ggplot(data = dfsim, aes(x = x, y = y, color = classe))
g + geom_point() + labs(color = 'Classe') +
geom_abline(slope = 0.9, intercept = 2.5) +
geom_abline(slope = 0.95, intercept = 2.5, linetype = 'dashed') +
geom_abline(slope = 0.95, intercept = 2.8, linetype = 'dotted') +
geom_abline(slope = 0.8, intercept = 3, linetype = 'dotdash') 

# Incluindo algumas retas separadoras
g = ggplot(data = dfsim, aes(x = x, y = y, color = classe))
g + geom_point() + labs(color = 'Classe') +
geom_abline(slope = 2.1, intercept = 5.2) +
geom_abline(slope = 0.95, intercept = 3, linetype = 'dashed') 

dfsim[mod.svm$index,]

# Rodando SVM
library(e1071)

mod.svm = svm(classe ~ ., data = dfsim, kernel = "linear", cost = 10, scale = FALSE)

# Coeficientes
beta = drop(t(mod.svm$coefs)%*%as.matrix(dfsim[mod.svm$index,1:2]))
beta0 = mod.svm$rho

# Reta encontrada pelo SVM
g = ggplot(data = dfsim, aes(x = x, y = y, color = classe))
g + geom_point() + labs(color = 'Classe') +
geom_abline(intercept = beta0 / beta[2], slope = -beta[1] / beta[2])

# Exemplo não linear

x_classe11 = runif(n = 100, min = -3, max = 3)
y_classe11 = sqrt(9-x_classe11^2) + rnorm(n = 100, mean = 0, sd = 0.1)

x_classe12 = runif(n = 100, min = -3, max = 3)
y_classe12 = -sqrt(9-x_classe12^2) + rnorm(n = 100, mean = 0, sd = 0.1)

dfsim21 = data.frame(x = rbind(as.matrix(x_classe11), as.matrix(x_classe12)), y = rbind(as.matrix(y_classe11), as.matrix(y_classe12)), classe = rep(1, 2*length(x_classe11)))

x_classe21 = runif(n = 100, min = -2, max = 2)
y_classe21 = sqrt(4-x_classe21^2) + rnorm(n = 10, mean = 0, sd = 0.1)

x_classe22 = runif(n = 100, min = -2, max = 2)
y_classe22 = -sqrt(4-x_classe22^2) + rnorm(n = 10, mean = 0, sd = 0.1)

dfsim22 = data.frame(x = rbind(as.matrix(x_classe21), as.matrix(x_classe22)), y = rbind(as.matrix(y_classe21), as.matrix(y_classe22)), classe = rep(-1, 2*length(x_classe21)))

dfsim2 = rbind(dfsim21, dfsim22)
dfsim2$classe = factor(dfsim2$classe)

g = ggplot(data = dfsim2, aes(x = x, y = y, color = classe))
g + geom_point() + labs(color = 'Classe')

# Criando a feature extra
dfsim2$z = sqrt(dfsim2$x^2 + dfsim2$y^2)

# Plotando o scatter 3d
library(scatterplot3d)

colors <- c("red", "blue")
colors <- colors[as.numeric(dfsim2$classe)]

scatterplot3d(x=dfsim2$x, y=dfsim2$y, z=dfsim2$z, color=colors, xlab = 'X', ylab = 'Y', zlab = 'X^2 + Y^2')


library('e1071')
mod.svm = svm(data = dfsim2, classe ~ ., kernel = 'polynomial', degree = 2)
print(mod.svm)
plot(mod.svm, data = dfsim2,classe ~ .)

#SVM no Titanic

dados = read.csv('../input/train.csv', sep = ',', header = T)
dados$Survived = factor(dados$Survived)

# Convertendo sexo para numérica (1 = male)
dados$Sexo = 0
dados[which(dados$Sex == 'male'), 'Sexo'] = 1

# Preenchendo idade missing
dados[which(is.na(dados$Age)),'Age'] = mean(dados$Age, na.rm = T)

dados = dados[,c('Survived', 'Age', 'Sexo', 'Fare', 'SibSp', 'Parch', 'Pclass')]

# O pacote e1071 faz o cross-validation automaticamente

# Exemplo: usando o separador linear, com cross-validation 10-fold na base do Titanic
mod.svm = svm(data = dados, Survived~., cross = 10, kernel = 'linear')

# Resultado
print(paste0("Acurácia do modelo com kernel linear = ", mod.svm$tot.accuracy))

# Exemplo: usando o kernel polinomial de grau 2, com cross-validation 10-fold na base do Titanic
mod.svm = svm(data = dados, Survived~., cross = 10, kernel = 'polynomial', degree= 2)

# Resultado
print(paste0("Acurácia do modelo com kernel quadrático = ", mod.svm$tot.accuracy))

# Usando a função tune.svm para encontrar o melhor grau no kernel polinomial
tune.svm(data = dados, Survived~., kernel = 'polynomial', degree = c(2, 3, 4, 5))

# Usando a função tune.svm para encontrar o melhor valor do custo no kernel linear
# O parâmetro de custo dá o tamanho da penalidade quando um indivíduo está no lado errado da fronteira
tune.svm(data = dados, Survived~., kernel = 'linear', cost = c(0.1, 1, 10, 100))

# Usando tune.svm para encontrar a melhor combinação gamma e coef0 para o kernel polinomial
# Para entender melhor os parâmetros gamma e coef0, veja a documentação https://cran.r-project.org/web/packages/e1071/e1071.pdf
tune.svm(data = dados, Survived~., kernel = 'polynomial', cost = 0.1, degree = 2, coef0 = c(-1, 0, 1), gamma = c(0.1, 1, 10) )

# Fazendo o cross-validation no modelo utilizando os parâmetros selecionados
mod.svm = svm(data = dados, Survived~., cross = 10, kernel = 'polynomial', degree= 2, cost = 0.1, coef0 = 1, gamma = 10)

# Resultado
print(paste0("Acurácia do modelo com kernel quadrático = ", mod.svm$tot.accuracy))

# Usando tune.svm para encontrar o melhor gamma do kernel RBF
tune.svm(data = dados, Survived~., kernel = 'radial', cost = 0.1, gamma = c(0.001, 0.01, 0.1, 1, 10))

# Fazendo o cross-validation no modelo utilizando os parâmetros selecionados
mod.svm = svm(data = dados, Survived~., cross = 10, kernel = 'radial',cost = 0.1, gamma = 1)

# Resultado
print(paste0("Acurácia do modelo com kernel radial = ", mod.svm$tot.accuracy))

# Dentre os modelos testados acima, o melhor foi o kernel polinomial
# Treino o modelo final
mod.svm = svm(data = dados, Survived ~., kernel = 'polynomial', degree= 2, cost = 0.1, coef0 = 1, gamma = 10)
mod.svm = svm(data = dados, Survived ~., kernel = 'linear', cost = 0.1)

# Amostra de teste Kaggle
test = read.csv('../input/test.csv')

# Preenchendo missing de Age e Fare
test[which(is.na(test$Age)), 'Age'] = mean(test$Age, na.rm = T)
test[which(is.na(test$Fare)), 'Fare'] = mean(test$Fare, na.rm = T)
test$Sexo = 0
test[which(dados$Sex == 'male'), 'Sexo'] = 1

X_test = test[,c('Sexo', 'Fare', 'Age', 'Parch', 'SibSp', 'Pclass')]

# Calculando previsões para submeter à competição
res = predict(mod.svm, newdata = X_test)

submit = data.frame(PassengerId = test$PassengerId, Survived = res)
write.csv(submit, "submit_SVM.csv", row.names = F)


