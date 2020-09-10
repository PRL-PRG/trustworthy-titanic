# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(dplyr)
library(data.table)
library(mice)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(VIM)
library(dmm)
library(randomForest)
library(kknn)
library(forcats)


train<-fread(file = "../input/train.csv", header = TRUE, data.table = FALSE, sep = ",")
test<-fread(file = "../input/test.csv", header = TRUE, data.table = FALSE, sep = ",")

df<-bind_rows(train,test) # Binding train & test data

str(df)

df<-df%>%
        mutate(Sex=factor(Sex), Embarked=factor(Embarked),
                Survived=factor(Survived), Pclass=factor(Pclass))
                
                
#Missing Values

aggr(df, prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE)

imputation<-mice(df, method = "rf")%>%mice::complete()

df$Age<-imputation$Age


#Exploratory Data Analisis:

train<-df[1:891,]

train%>%ggplot(aes(Age, fill=Sex))+
        geom_density(na.rm = TRUE, alpha=0.4, stat = "count")+
        facet_wrap(~Survived, ncol = 1)

train %>%
        ggplot(aes(Fare, fill=Pclass)) +
        geom_density(alpha = 0.5) +
        scale_x_log10() +
        facet_wrap(~ Survived, ncol = 1)


df$Fare[which(is.na(df$Fare))]<-mean(df$Fare, na.rm=TRUE)

table(df$Embarked)

train[which(df$Embarked==""),]

train[which(df$Embarked==""),12]<-"S"

train%>%ggplot(aes(x = Embarked, fill = Survived)) +
        geom_bar(position = "fill") +
        ylab("Survival Rate")

train%>%ggplot(aes(x = SibSp, fill = Survived)) +
        geom_bar(position = "fill") +
        ylab("Survival Rate")

train%>%ggplot(aes(x = Parch, fill = Survived)) +
        geom_bar(position = "fill") +
        ylab("Survival Rate")


df<-df%>%mutate(Family=SibSp+Parch, Title=gsub('(.*, )|(\\..*)', '', Name))
        

Others<- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

df$Title[df$Title=="Mlle"]<-"Mrs"
df$Title[df$Title=="Ms"]<-"Miss"
df$Title[df$Title=="Mme"]<-"Miss"
df$Title[df$Title %in% Others]  <- 'Others'

df<-df%>%mutate(Family=factor(Family), Title=factor(Title))

train<-df[1:891,]

train%>%ggplot(aes(x = Family, fill = Survived)) +
        geom_bar(position = "fill") +
        ylab("Survival Rate")

train%>%ggplot(aes(x = Title, fill = Survived)) +
        geom_bar(position = "fill") +
        ylab("Survival Rate")
        
#-----------------------------------------------------PREDICTION---------------------------------------------------------------------



train<-df[1:891,]; test<-df[892:1309,]

levels(train$Survived) <- make.names(levels(factor(train$Survived)))

control<-trainControl(method = "repeatedcv", number = 10, repeats = 2, savePredictions = 'final', classProbs = TRUE, index =createResample(train$Survived, times = 2))

algorithms<-c("rf", "treebag","adaboost", "cforest", "knn")

formula<-as.formula("Survived ~ Pclass + Age + Sex + SibSp + Parch + Fare + Embarked + Family + Title")

models <- caretList(formula, data=train, 
                    trControl=control, methodList=algorithms, preProcess="scale")

results <- resamples(models); 

summary(results)
splom(results)

control.stack<-trainControl(method = "repeatedcv", number = 10, repeats = 2, savePredictions = 'final', classProbs = TRUE)
stack<-caretStack(models, method="rf",metric="Accuracy", trControl=control.stack, data=train, preProcess="scale")
print(stack)

fit<-predict(stack, test)

levels(fit)<-c("0","1"); fit<-unfactor(fit)

output<-data.frame(PassengerID=test$PassengerId, Survived=fit)

write.csv(output, file = "output.csv", row.names = FALSE)