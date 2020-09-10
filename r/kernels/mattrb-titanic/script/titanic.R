
# Set working directory and import datafiles
library(tidyverse)
library(rpart)
library(randomForest)
library(modelr)
library(party)
library(xlsx)
#setwd("...")

train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")

summary(train)
str(train)
head(train)
tail(train)
#vemos que el 61% de los pasajeron en el conjunto de entrenamiento murieron.
prop.table(table(train$Survived))


#se sabe que se trató de salvar primero a las mujeres y niños, veamos las proporciones.
prop.table(table(train$Sex))
#la mayoría son hombres

prop_sexo <- prop.table(table(train$Sex, train$Survived),1)
prop_sexo
#efectivamente la mayoría de las mujeres sobrevivireron y la mayoría de los hombres no.

summary(train$Age)
#debido a que la edad no es variable categórica como la edad, la tabla de proporcion es casi inútil. 
#asignemos un anueva variable "child"
train <- train %>% mutate(Child = ifelse(train$Age<18, 1,0))
test <- test %>% mutate(Child = ifelse(test$Age<18, 1,0))
#cuántos sobrevivientes hay en cada subgrupo?
aggregate(data=train, Survived~Child+Sex, FUN=sum) #no nos sirve mucho pues sólo es la cuenta de sobrevivientes

#lo mismo pero proporcionalmente:
prop_sexo_edad <- aggregate(data=train, Survived~Child+Sex, FUN=function(x){
                                            sum(x)/length(x)}
                            )
                            #aquí parece que es más probale sobrevivir siendo mujer que hombre pero sin importar si se es 
                            # niño o no. Continuemos explorando para ver si la clase es un factor importante.
prop_sexo_edad





#implementamos un árbol de decisión agregando las demás variables.
#las columnas de PassengerId, Name, Ticket y Cabin son identificadores únicos,
#por lo que no nos ayudan. 
#La variable Age ya fue resumida en Child.
fit <- rpart(Survived~Pclass+Sex+SibSp+Parch+Fare+Embarked+Child, data=train)
plot(fit, uniform=TRUE, main="primer intento")
text(fit,all=TRUE,use.n=TRUE,cex=.6)
#no tenemos idea de qué tan bien lo está haciendo este modelo pues no hay con qué comparar.

#dividiremos nuestra train data en 70% y 30% y así poder medir performance:
split_data <- resample_partition(data=train, c(split_test=0.3, split_train=0.7))#lista de 2 entradas
fit2 <- rpart(Survived~Pclass+Sex+SibSp+Parch+Fare+Embarked+Child, data=split_data$split_train)
#calcular el mean average error para este modelo:
mae(model=fit2, data=split_data$split_test)


#por un momento, supongamos que no sabemos que la función rpart optimiza la profundidad del árbol
#vamos a hacer varios árboles con diferentes profundidades para averiguar cuál es la ideal:
get_mae <- function(maxdepth, target, predictors, train_data, test_data){
  predictors <- paste(predictors, collapse="+")
  formula <- as.formula(paste(target, "~", predictors, sep=""))
  
  model <- rpart(formula, data=train_data, control=rpart.control(maxdepth=maxdepth))
  mae <- mae(model,test_data)
  return(mae)
  
}

target <- "Survived"
predictors <- c("Pclass","Sex","SibSp","Parch","Fare","Embarked","Child")
#ans <- c(1:10)
for(i in 1:10){
  mae <- get_mae(maxdepth=i, target=target, predictors=predictors, train_data=split_data$split_train,
                 test_data=split_data$split_test)
  print(glue::glue("Maxdepth: ",i,"\t MAE: ",mae))
  #ans[i]=mae
}
#al ver los resultados nos damos cuenta que la función rpart detiene el crecimiento del árbol entonces 
#no es neceario preocuparnos por esto de ahora en adelante

########### el bueno #################

set.seed(450)
fit3 <- rpart(Survived~Pclass+Sex+SibSp+Parch+Fare+Embarked+Child, data=train)
prediction <- predict(fit3, test)
pred <- as.numeric(prediction)
col <- ifelse(pred>0.5, 1,0)
submit <- data.frame(PassengerId=test$PassengerId, Survived=col)
write_csv(submit, "submission.csv")

# al someter este archivo a la competencia del titanic se logró una precisión del 79%







