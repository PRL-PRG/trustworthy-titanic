Titanic.train = read.csv("../input/train.csv",stringsAsFactors = FALSE)
Titanic.test = read.csv("../input/test.csv",stringsAsFactors = FALSE)


Titanic.train$Istrain <- 'TRUE'
Titanic.test$Istrain <- 'FALSE'

Titanic.test$Survived<-'NA' 

Titanic.full <- rbind(Titanic.train, Titanic.test)

#Filling in the missing values for Embarked  - See Onenote or Tableau
Titanic.full[Titanic.full$Embarked == '', "Embarked"] <- 'C'


#Filling in the missing age parameters using Linear Regression

#filtering out the outliers
upper_whisker_age <- boxplot(Titanic.full$Age)$stats[5]
age_filter <-Titanic.full$Age < upper_whisker_age

age_equation = "Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked"

age_regressor <- lm(formula = age_equation, data = Titanic.full[age_filter,] )


age.predictions <-predict(age_regressor,newdata =Titanic.full[is.na(Titanic.full$Age),
          c("Pclass","Sex","SibSp","Parch","Fare","Embarked")])
Titanic.full[is.na(Titanic.full$Age),"Age"] <- age.predictions

#Filling in the missing fare parameters using Linear Regression

#filtering out the outliers
upper_whisker_fare <- boxplot(Titanic.full$Fare)$stats[5]
fare_filter <-Titanic.full$Fare < upper_whisker_fare

fare_equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"

fare_regressor <- lm(formula = fare_equation, data = Titanic.full[fare_filter,] )


fare.predictions <-predict(fare_regressor,newdata =Titanic.full[is.na(Titanic.full$Fare),
                                                              c("Pclass","Sex","SibSp","Parch","Age","Embarked")])
Titanic.full[is.na(Titanic.full$Fare),"Fare"] <- fare.predictions

#categorical casting [as.factor didnt work ]
Titanic.full$Pclass <- factor(x= Titanic.full$Pclass,
                              levels = c('1','2','3'),
                              labels = c('1','2','3'))
Titanic.full$Sex <- factor(x= Titanic.full$Sex,
                           levels = c('male','female'),
                           labels = c('0','1'))
Titanic.full$Embarked <- factor(x= Titanic.full$Embarked,
                                levels = c('C','Q','S'),
                                labels = c('1','2','3'))


#Training randomForest on the training set

rf.equation = "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked "
rf.formula = as.formula(rf.equation)
library(randomForest)
rf.training.indep <- Titanic.full[Titanic.full$Istrain == TRUE,
                            c("Pclass","Age","Sex","SibSp","Parch","Fare","Embarked")]

rf.training.dep <-Titanic.full[Titanic.full$Istrain == TRUE, "Survived"]
                                
classifier = randomForest(x = rf.training.indep, y =as.factor(rf.training.dep) , ntree = 500)

rf.test.indep <- Titanic.full[Titanic.full$Istrain == FALSE,
                        c("Pclass","Age","Sex","SibSp","Parch","Fare","Embarked")]

survivor <- predict(classifier, newdata = rf.test.indep)


PassengerId <- as.data.frame(Titanic.test$PassengerId)
output.df <-PassengerId
output.df$Survivor <- survivor

write.csv(output.df, 'Survivor.csv')

