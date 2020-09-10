
# Name: Matheus Brizolla
# Date: 04/22/18
# Dataset: Titanic

# Loading our library / Carregando nossa biblioteca
library(MASS)

# Loading our train data and test data  / Carregando nossas bases de treino e teste
test <- read.csv("../input/test.csv")
train <- read.csv("../input/train.csv")

# Checking if our data is ok / Verifica se os dados estão de acordo com o esperado
head (train)

# Checking the type of each column / Verifica o tipo das colunas
str(train)

# Summarize our data / Traz as principais informações para a nossa primeira análise
summary (train)

# Telling to our model what variables are factors / Avisa ao nosso modelo quais são as variáveis categóricas
train$Pclass <- factor(train$Pclass)
train$Survived <- factor(train$Survived)

test$Pclass <- factor(test$Pclass)

# Checking if our train data have missing values / Verificando se há algum campo em vazio
NAvalue_train <- sapply(train, function(x) sum(is.na(x)))
MissingNA_train <- data.frame(index = names(train), Missing_Values = NAvalue_train)
MissingNA_train[MissingNA_train$Missing_Values > 0,]
                  
# Checking if our test data have missing values / Verificando se há algum campo em vazio
NAvalue_test <- sapply(test, function(x) sum(is.na(x)))
MissingNA_test <- data.frame(index = names(test), Missing_Values = NAvalue_test)
MissingNA_test[MissingNA_test$Missing_Values > 0,]

# Handling our missing values / Cuidando dos valores NA
train$Age [is.na(train$Age)] <- median(train$Age, na.rm=TRUE)

test$Fare [is.na(test$Fare)] <- median(test$Fare, na.rm=TRUE)
test$Age [is.na(test$Age)] <- median(train$Age, na.rm=TRUE)

# Logistic Regression / Regressão logística
rl <- glm(Survived ~ Sex + Pclass + Age + SibSp, data = train, family = "binomial")

# Summarizing our model / Verificando nosso modelo
summary (rl)

# Predicting / Predição
previsto <- ifelse(predict(rl, newdata = train, type = "response")>0.5,1,0)

# Setting our result in a new column / Colocando o nosso resultado em uma nova coluna
train$Previsao <- previsto

# Visualizing our prediction / Vizualizando nossa predição
tb <- table(train$Previsao >= 0.5, train$Survived)
tb
cat("Accuracy: ", (tb[1,1] + tb[2,2])/ nrow(train)*100, "/ Taxa de acerto:", (tb[1,1] + tb[2,2])/ nrow(train)*100)

# Now we are doing the same to test data / Repetindo as ações mas na base de treino
predictTest <- predict(rl, newdata = test, type = "response")
test$Survived <- as.numeric(predictTest >= 0.5)
table(test$Survived)

# Creating the file to submit / Criando o arquivo para envio
subm<-data.frame(test$PassengerId)
names(subm)[1]<-"PassengerId"

subm$Survived<-test$Survived

write.csv(subm, file = "titanic_logr_submission.csv", row.names=FALSE)
