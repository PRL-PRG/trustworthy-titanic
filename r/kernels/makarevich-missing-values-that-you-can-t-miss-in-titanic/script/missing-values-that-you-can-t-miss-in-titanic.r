
library(doBy)
library(mice)
library(ggplot2)
x_train <- read.csv('../input/train.csv', header = T, sep = ',')
x_train$Fare[x_train$Fare == 0] <- NA
x_train[c(62, 830), 'Embarked'] <- NA
data.frame('missing_values' = sapply(x_train, function(x) sum(is.na(x))))


Fare_mean <- median(x_train$Fare[x_train$Pclass == 1], na.rm = T)
ggplot(x_train[x_train$Pclass == 1 & x_train$Fare >= Fare_mean,], aes(x = Embarked, fill = 'coral')) +
  geom_bar(show.legend = F) +
  theme_classic()
x_train[c(62, 830), 'Embarked'] <- 'C'

tab <- summaryBy(Fare ~ Embarked + Pclass, data = x_train, FUN = median, na.rm = T)
tab
x_train$Fare[x_train$Embarked =='S' & x_train$Pclass == '1' & is.na(x_train$Fare)] <- tab$Fare.median[7]
x_train$Fare[x_train$Embarked =='S' & x_train$Pclass == '2' & is.na(x_train$Fare)] <- tab$Fare.median[8]
x_train$Fare[x_train$Embarked =='S' & x_train$Pclass == '3' & is.na(x_train$Fare)] <- tab$Fare.median[9]

nam <- names(x_train)[c(1, 4, 9, 11)]
imp <- mice(data = x_train[,!names(x_train) %in% nam], seed = 8, printFlag = F)
data.frame(count = 1:5, median = apply(imp$imp$Age, 2, median))

imp_titanic <- complete(imp, 3)
x_train$Age <- imp_titanic$Age
data.frame('missing_values' = sapply(x_train, function(x) sum(is.na(x))))
