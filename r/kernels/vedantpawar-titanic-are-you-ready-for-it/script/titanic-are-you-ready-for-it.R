# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # missing values
library('e1071')
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # missing values
library('e1071')
train = read.csv("../input/train.csv")
test = read.csv("../input/test.csv")
data = bind_rows(train,test) 
str(data)
summary(data)
data1 = data[,-2]
summary(data1)
str(data1)
colnames(data1)[colSums(is.na(data1))>0]
age <- data1 %>%
  filter(!is.na(data$Age) )
str(age)  
summary(age$Age)
age3 = age
for(i in 1:1046){
  if(age$Age[i] <= 21.00 ){
    age$Age[i] <- 0
  } 
  else if(age$Age[i] <= 39){
    age$Age[i] <- 1
  }
  else if(age$Age[i] > 39.00){
    age$Age[i] <- 2
  }
}
table(age$Age)
theme_update(plot.title = element_text(hjust = 0.5))
ggplot(age , aes(SibSp, fill=as.factor(Age))) +
  geom_bar() +
  labs(title="Genre distribution with ranking ", x="SibSp", y="Count") +
  scale_fill_discrete(name="Age")
#missing age
age1 <- data1 %>%
  filter(is.na(data$Age) )
#without missing age
age2 <- data1 %>%
  filter(!is.na(data1$Age) )
a = mean(age2$Age < 21)
b = mean(age2$Age < 39)
c = mean(age2$Age >= 39)
#sib>3 age = c
for(i in 1:263){
  
  if(is.na(age1$Age[i])){
    
    if(age1$SibSp[i] > 3 ){
      age1$Age[i] <- c
    } 
  }
}
#male average
for(i in 1:263){
  if(is.na(age1$Age[i])){
    
    if(age1$Sex[i] == "male" ){
      age1$Age[i] <- mean(age2$Age[age2$Sex == "male"])
      
    }
  } 
}
#female avreage
for(i in 1:263){
  if(is.na(age1$Age[i])){
    
    if(age1$Sex[i] == "female" ){
      age1$Age[i] <- mean(age2$Age[age2$Sex == "female"])
      
    }
  } 
}
#bind age1 and age2
data2 = bind_rows(age1,age2)
summary(data2)
colnames(data2)[colSums(is.na(data2))>0]

#non missig  fare 
fare <- data2 %>%
  filter(!is.na(data2$Fare) )
#pclass 3 dikha dena data2$Pclass[1099] se -  3 ayega

data2$Fare[1099] = mean(fare$Fare[fare$Pclass == "3"])

#no missing value :)
sum(is.na(data2))
data2$Survived = data$Survived

#Grab surnames from name variable 
data2$Surname <- sapply(data2$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])
# Create a family size variable including the passenger themselves
data2$Fsize <- data2$SibSp + data2$Parch + 1

# Create a family variable 
data2$Family <- paste(data2$Surname, data2$Fsize, sep='_')

#Let's see 1st element of Family Variable 
data2$Family[1]

ggplot(data2[1:891,], aes(x = Fsize, fill = factor(Survived))) +
geom_bar(stat='count', position='dodge') +
scale_x_continuous(breaks=c(1:11)) +
labs(x = 'Family Size') +
theme_few()
sum(is.na(data2))
data2$FsizeD = 0
data2$FsizeD[data2$Fsize == 1] <- 'singlF'
data2$FsizeD[data2$Fsize < 5 && data$Fsize > 1] <- 'smallF'
data2$FsizeD[data2$Fsize > 4] <- 'largeF'
sum(is.na(data2))
train1 = data2[1:891,]
test1 = data2[892:1309,]
set.seed(2)
model = svm(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch +
                    Fare + Embarked + FsizeD,
                  data = train1 , cost = 16 , gamma = 10)
predicttrain = predict(model , train1 )
a = table(train1$Survived , predicttrain)
sum(diag(a)/sum(a))