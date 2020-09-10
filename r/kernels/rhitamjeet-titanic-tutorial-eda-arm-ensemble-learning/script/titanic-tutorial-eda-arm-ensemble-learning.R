## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = F,message = F)

library(ggplot2)
library(dplyr)
library(knitr)
library(caret)
library(randomForest)
library(arules)
library(gridExtra)
library(ggpubr)
library(class)
library(gbm)
library(tree)
library(e1071)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_train = read.csv("../input/train.csv",na.strings = '')
titanic_test = read.csv("../input/test.csv",na.strings = '')

Survived = titanic_train$Survived
titanic_full = bind_rows(titanic_train[-2],titanic_test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
colSums(is.na(titanic_full))/nrow(titanic_full)*100
colSums(is.na(titanic_full))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pclass = titanic_train %>% group_by(Pclass,Survived) %>% summarise(Count = n())

ggplot(pclass) + geom_bar(aes(x = Pclass, y = Count, fill = as.factor(Survived)), stat = 'identity') + labs(title = "Pclass wise Survivors") 



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sex = titanic_train %>% group_by(Sex,Survived) %>% summarise(Count = n())
ggplot(sex) + geom_bar(aes(x = Sex, y = Count, fill = as.factor(Survived)), stat = 'identity')+ labs(title = "Sex v/s Survivors")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train) + geom_histogram(aes(x = Age, fill = as.factor(Survived))) + facet_wrap(~Sex)
ggplot(titanic_train) + geom_boxplot(aes(y = Age, x = Sex, fill = Sex), alpha = 0.5)

ggplot(titanic_train) + geom_boxplot(aes(y = Age, x = Sex, fill = Sex), alpha = 0.5) + facet_wrap(~Pclass) + labs(title = "Pclass wise Age plots") 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full$Age_category = ifelse(titanic_full$Age<14,"Child","Adult")
titanic_train$Age_category = ifelse(titanic_train$Age<14,"Child","Adult")
age_category = titanic_train %>% group_by(Age_category,Survived,Pclass) %>% summarise(Count = n()) %>% filter(!is.na(Age_category))

ggplot(age_category) + geom_bar(aes(x = Pclass, y = Count, fill = Age_category),stat = 'identity') + labs(title = "No. of children per Pclass")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_apriori = titanic_train[,c(2,3,5,13)]
for(i in 1:4)
{
  titanic_apriori[,i] = as.factor(titanic_apriori[,i])
}
titanic_rules = apriori(titanic_apriori,parameter = list(minlen = 3, supp = 0.002, conf = 0.8),appearance = list(default = "none",rhs = c("Survived=1","Survived=0"),
                                  lhs = c("Pclass=1","Pclass=2","Pclass=3"    ,"Age_category=Child","Age_category=Adult","Sex=male","Sex=female")))
rules.sorted = sort(titanic_rules, by = 'confidence')


#removing redundant rules
subset.matrix = is.subset(rules.sorted, rules.sorted, sparse = F)
subset.matrix[lower.tri(subset.matrix, diag= T)] = NA

redundant = colSums(subset.matrix,na.rm = T) >=1

which(redundant)

#removing redundant rules

rules.pruned = rules.sorted[!redundant]

inspect(rules.pruned)




## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
children = age_category %>% filter(Age_category=='Child')
p1 = ggplot(children) + geom_bar(aes(x = Pclass, y = Count, fill = as.factor(Survived)),stat = 'identity') + labs(title = "Survival of Children across Pclass")

sex_pclass_female = titanic_train %>% group_by(Sex, Pclass,Survived) %>% summarise(Count = n()) %>% filter(Sex =="female")
p2 = ggplot(sex_pclass_female) + geom_bar(aes(x = Pclass, y = Count, fill = as.factor(Survived)),stat = 'identity')  + labs(title = "Survival of Females across Pclass")

ggarrange(p1,p2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full$Family_size = titanic_full$SibSp + titanic_full$Parch + 1
titanic_train$Family_size = titanic_train$SibSp + titanic_train$Parch + 1

ggplot(titanic_train) + geom_histogram(aes(x = Family_size, fill = as.factor(Survived))) + labs(title = "Family Size v/s Survival")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train) + geom_histogram(aes(x = Fare,fill = as.factor(Survived))) + labs(title = "Does Higher fare ensure Survival?")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cabin = gsub(pattern = '[^[:alpha:]]', replacement = '', titanic_full$Cabin)
cabin = substr(cabin,1,1)
titanic_full$cabin_deck = cabin
titanic_full$cabin_deck = as.factor(titanic_full$cabin_deck)

cabin1 = gsub(pattern = '[^[:alpha:]]', replacement = '', titanic_train$Cabin)
cabin1 = substr(cabin1,1,1)
titanic_train$cabin_deck = cabin1
titanic_train$cabin_deck = as.factor(titanic_train$cabin_deck)

ggplot(titanic_train) + geom_bar(aes(x = cabin_deck, fill = as.factor(Pclass),stat = "count")) + labs(title = "Distribution of Pclass passengers across decks")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p3 = ggplot(titanic_train) + geom_boxplot(aes(x = cabin_deck, y = Fare, fill = cabin_deck), alpha = 0.5) + labs(title = "cabin_deck wise Fare")
p4 = ggplot(titanic_train) + geom_boxplot(aes(x = as.factor(titanic_train$Pclass), y = Fare, fill = as.factor(Pclass)), alpha = 0.5) + labs(title = "Pclass wise Fare")

ggarrange(p3,p4)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train) + geom_bar(aes(x = cabin_deck, fill = as.factor(Survived)), stat = 'count') + labs(title = "cabin_deck wise Survival")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full$Family_group = cut(titanic_full$Family_size,breaks = c(1,2,5,12), include.lowest = T, right = F, labels = c('single','small family','large family'))

titanic_train$Family_group = cut(titanic_train$Family_size,breaks = c(1,2,5,12), include.lowest = T, right = F, labels = c('single','small family','large family'))

ggplot(titanic_train) + geom_bar(aes(fill = Family_group,x = cabin_deck), stat = 'count')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(titanic_full[titanic_full$Family_group=='large family',]$Pclass)
titanic_full[titanic_full$Family_group=='large family' & titanic_full$Pclass==1,]$cabin_deck



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full[is.na(titanic_full$Fare),]$Fare=median(titanic_full[titanic_full$Embarked =='S' & titanic_full$Pclass == 3 & titanic_full$Family_size == 1,]$Fare, na.rm = T)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train) + geom_histogram(aes(x = Age, fill = as.factor(Survived )),binwidth = 3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full$Age_category = cut(titanic_full$Age,breaks = seq(from = 0, to = 85,by = 5), include.lowest = F, right = T)

titanic_train$Age_category = cut(titanic_train$Age,breaks = seq(from = 0, to = 85,by = 5), include.lowest = F, right = T)

age_test = titanic_full[is.na(titanic_full$Age),c(2,4,5,6,7,9)]
age_train = titanic_full[!is.na(titanic_full$Age),c(2,4,5,6,7,9)]

#Random Forest
age_rf = randomForest(x = age_train[-3],
                      y = age_train$Age,
                      ntree = 500)
age_impute = predict(age_rf, newdata = age_test[-3])

age_test$Age = age_impute
age_test$PassengerId = rownames(age_test)

titanic_full[is.na(titanic_full$Age),]$Age = age_test$Age

titanic_full$Age_category = cut(titanic_full$Age,breaks = seq(from = 0, to = 85,by = 5), include.lowest = F, right = T)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cabin_train = titanic_full[!is.na(titanic_full$cabin_deck),c(2,4,5,9,15,14)]

cabin_test = titanic_full[is.na(titanic_full$cabin_deck),c(2,4,5,9,15,14)]

cabin_rf = randomForest(x = cabin_train[-6],
                        y = cabin_train$cabin_deck,
                        ntree = 500)
cabin_impute = predict(cabin_rf, newdata = cabin_test[-6])
cabin_test$cabin_deck = cabin_impute

titanic_full[is.na(titanic_full$cabin_deck),]$cabin_deck = cabin_test$cabin_deck


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full$Title = gsub('(.*,)|(\\..*)','',titanic_full$Name)

table(titanic_full$Title)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

titanic_full$Title[titanic_full$Title == 'Mlle']        = 'Miss' 
titanic_full$Title[titanic_full$Title == 'Ms']          = 'Miss'
titanic_full$Title[titanic_full$Title == 'Mme']         = 'Mrs' 
titanic_full$Title[titanic_full$Title %in% rare_title]  = 'Rare Title'
titanic_full$Title = as.factor(titanic_full$Title)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
final_train = titanic_full[1:891,c(2,4,5,6,7,9,12,13,14,15,16)]
final_test = titanic_full[892:1309,c(2,4,5,6,7,9,12,13,14,15,16)]
final_train$Survived = as.factor(Survived)

#Random Forest

final_rf = randomForest(x = final_train[-12], y = final_train$Survived,
                        ntree = 1000)

pred = predict(final_rf, newdata = final_test)

# GBM
final_gbm = gbm.fit(x = final_train[-12],
                    y = final_train$Survived,
                    distribution = 'gaussian',
                    n.trees = 1000,
                    interaction.depth = 3,
                    shrinkage = 0.01,
                    nTrain = 0.8*nrow(final_train))
gbm.perf(final_gbm)
summary(final_gbm)
pred1 = predict(final_gbm, newdata = final_test, n.trees = 836)
pred1 = ifelse(pred1<1.5,0,1)

###decision tree
final_tree = tree(data = final_train,
                  formula = Survived~.)
pred2 = predict(final_tree, newdata = final_test, type = 'class')

##NaiveBayes
final_NaiveBayes = naiveBayes(x = final_train[-c(12)],
                y = final_train$Survived)
pred3 = predict(final_NaiveBayes, newdata = final_test)
##ensemble learning

final = as.data.frame(cbind(as.numeric(as.character(pred)),as.numeric(as.character(pred2)),as.numeric(as.character(pred3))))

final_yes = apply(final,1,function(x) x[which.max(table(x))] )
final_yes


