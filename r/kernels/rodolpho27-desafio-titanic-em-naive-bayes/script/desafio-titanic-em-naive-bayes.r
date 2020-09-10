
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(e1071)#Naive Bayes

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory


# Any results you write to the current directory are saved as output.

#Leitura dos dados 
test<- read.csv("../input/test.csv")
train <- read.csv( "../input/train.csv")



#Transformando o tipo da variavel Survived de INT para Factor
train1<-as.factor(train$Survived)
train$Survived<-train1


#Pegando a media para colocar no lugar dos NAS
age.median<- mean(train$Age, na.rm=TRUE)  
train[is.na(train$Age), "Age"]<- age.median

age.media_test<- mean(test$Age, na.rm=TRUE)
test[is.na(test$Age), "Age"]<-age.media_test

fare.media<- mean(test$Fare, na.rm= TRUE)
test[is.na(test$Fare), "Fare"]<- fare.media


#Sexo por sobrevivente
ggplot(train, aes(Sex))+geom_bar(aes(fill=factor(Survived)))


#Classe por sobrevivente
ggplot(train, aes(Pclass))+geom_bar(aes(fill=factor(Survived)))


#Retirada de variaveis desnecessárias 
train$PassengerId<-NULL
train$Name<-NULL
train$Cabin<-NULL
train$Ticket<-NULL

#Construção do modelo através do Naive Bayes
modelo<- naiveBayes(Survived~., data = train)


#Criação da coluna Survived no dataset test com o valor das predicoes
test$Survived <- predict(modelo, newdata=test)


#Criação de um dataframe com a coluna PassengerId e Survived
resposta<-data.frame(PassengerId=test$PassengerId, Survived=test$Survived)


#As 10 primeiras linhas do dataframe com sobrevivente ou não 
head(resposta, 10)
