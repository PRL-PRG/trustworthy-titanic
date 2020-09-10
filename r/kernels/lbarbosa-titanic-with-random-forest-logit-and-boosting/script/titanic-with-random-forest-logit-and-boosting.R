## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list = ls())

library(data.table)

titanic_train = fread("../input/train.csv")

titanic_test = fread("../input/test.csv")

head(titanic_train,200)

## Summary statistics
library(stargazer)

stargazer(titanic_train, type="text", median=TRUE, iqr=TRUE,digits=1, title="Descriptive Statistics")

summary(titanic_train)  # Age => 177 NA's.  Need values for those as age likely to be important for determining survival


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# I will use the title to estimate the age of those individuals with NA's

library(stringr)

titanic_train$title = str_extract(titanic_train$Name, pattern = "[A-Za-z]+\\.") # extracting title from names and creating "title" column
titanic_train_temp <- titanic_train[,3:13]

titanic_test$title = str_extract(titanic_test$Name, pattern = "[A-Za-z]+\\.") # extracting title from names and creating "title" column
titanic_test_temp <- titanic_test[,2:12]

colnames(titanic_test_temp)

titanic_both = rbind(titanic_train_temp, titanic_test_temp)

library(ggplot2)
at <- ggplot(data = titanic_both, aes(x=title, y=Age))
at + geom_boxplot() #boxplot showing agy by title

library(plyr)
library(dplyr)
library(dtplyr)

list = aggregate(Age ~ title, titanic_both, mean)
list

# This "for" loop checks for Age NAs and assigns mean Age to those.  Basically mapping the data set to the list of title/meanAge
for (i in (1:length(titanic_train$Age)))
{if (is.na(titanic_train$Age[i])) 
    {titanic_train$Age[i] <- list$Age[match(titanic_train$title[i],list$title)]} 
    else {}
}

head(titanic_train,250)

z = titanic_train[title=="Mme."|title=="Mlle."]

head(z)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

bar <- aggregate(Survived ~ Pclass, titanic_train, mean)
bar

yo <- ggplot(data = bar, aes(x=Pclass, y=Survived))
yo + geom_bar(stat="identity")

# As we can see, about 60% of the people travelling in the first class survived. In contrast, only approximately 25% of the people travelling in the third class survived. Accordingly, this plot suggests that the class in which people travel affects the chances of survival.



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# title count...
ggplot(titanic_train, aes(x=title)) + geom_histogram(colour="green", stat="count")

# From the plot we can see that titles like 'Master', 'Miss', 'Mr', and 'Mrs', appear several times. Accordingly, I will not group them. I will also keep 'Dr.' and 'Rev.'.

# Regarding Mme and Mlle, from https://www.frenchtoday.com/blog/french-culture/madame-or-mademoiselle-a-delicate-question they correspond to the categories Mrs and Miss, respectively. As a consequence, we will assign them to those titles.

for (i in (1:length(titanic_train$Age)))
{if (titanic_train$title[i]=="Mme.")
    {titanic_train$title[i] <- "Mrs."} 
    else if (titanic_train$title[i]=="Mlle.")
    {titanic_train$title[i] <- "Miss."}
    else if (titanic_train$title[i]!="Mr." & titanic_train$title[i]!="Master." & titanic_train$title[i]!="Mrs." 
             & titanic_train$title[i]!="Miss." & titanic_train$title[i]!="Dr." & titanic_train$title[i]!="Rev.")
    {titanic_train$title[i] <- "Other."}
    else {}
}

head(titanic_train,250)

titanic_train[640:649]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bar <- aggregate(Survived ~ title, titanic_train, mean)
bar

yo <- ggplot(data = bar, aes(x=title, y=Survived))
yo + geom_bar(stat="identity", width = 0.5, color="blue", fill="white")

# People with the title 'Mr' survived less than people with any other title.
# Titles with a survival rate higher than 50% are those that correspond to female (Miss or Mrs) or children (Master) titles



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bar <- aggregate(Survived ~ Sex, titanic_train, mean)
bar

yo <- ggplot(data = bar, aes(x=Sex, y=Survived))
yo + geom_bar(stat="identity", width = 0.5, color="red", fill="white")

# This plot shows that women were a lot more likely to survive than men



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
linea <- aggregate(Survived ~ Age, titanic_train, mean)
linea

yo <- ggplot(data = linea, aes(x=Age, y=Survived))
yo + geom_line(stat="identity", color="blue")

# There seems to be three regions: 0 to 15, 15 to 48, and 48 to 80



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# creating "Age_n" column, 0 to 15 => "Child", 15 to 48 => Adult, 48 to 80 => Senior.
titanic_train$Age_n <- ifelse(titanic_train$Age<=15,"Child", ifelse(titanic_train$Age<=49,"Adult", "Senior"))

titanic_train[0:100]

bar <- aggregate(Survived ~ Age_n, titanic_train, mean)
bar

yo <- ggplot(data = bar, aes(x=Age_n, y=Survived))
yo + geom_bar(stat="identity", width = 0.5, color="red", fill="white")

# From this plot, Children (0 to 15 yrs old) were more likely to survive



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# creating family size column from SibSp (# of siblings/spouses aboard the Titanic) & parch (# of parents/children aboard the Titanic)
titanic_train$fam_size <- titanic_train$SibSp + titanic_train$Parch

bar <- aggregate(Survived ~ fam_size, titanic_train, mean)
bar

yo <- ggplot(data = bar, aes(x=as.factor(fam_size), y=Survived))
yo + geom_bar(stat="identity", width = 0.5, color="blue", fill="white")

# Members of large families (fam_size>3) are less likely to survive


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# I would expect the same as Pclass.  The higher the fare, the higher the survival rate.

ggplot(titanic_train, aes(x=as.factor(Survived), y=Fare, fill=as.factor(Survived))) + geom_boxplot() + xlab("Survived") + ylab("Fare") 

# From this plot implies that people who survived paid a higher fare


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bar <- aggregate(Fare ~ Pclass + Survived, titanic_train, mean)
bar

yo <- ggplot(data = bar, aes(fill=as.factor(Pclass), y=Fare, x=as.factor(Survived)))
yo + geom_bar(stat="identity", position="dodge")

# From plot below, Fare is more helpful to distinguish between Pclass 1 passengers, and not to relevant for other Pclass. An interaction term would help.


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

titanic_train[titanic_train$Embarked==""]
# Only two people, Passenger ID 62 & 830, do not have an Embarkment port, both of which Survived
# Per encyclopedia Titanica https://www.encyclopedia-titanica.org/titanic-survivor/martha-evelyn-stone.html, Mrs Stone [Passenger ID 830] boarded the Titanic in Southampton (S) on 10 April 1912 and was travelling in first class with her maid Amelie Icard [Passenger ID 62]. She occupied cabin B-28.
titanic_train$Embarked[62]="S"
titanic_train$Embarked[830]="S"

bar <- aggregate(Survived~Embarked, titanic_train, mean)
bar

yo <- ggplot(data = bar, aes(y=Survived, x=as.factor(Embarked)))
yo + geom_bar(stat="identity")

# People who embarked on "C" are more likely to survive

titanic_train



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# The first letter of the cabin is the Deck assigned.

titanic_train[titanic_train$Cabin!=""] # There are 687 wihtout a Cabin assigned

titanic_train$Deck = ifelse(titanic_train$Cabin=="","Unk",substring(titanic_train$Cabin, 1, 1))

titanic_train

bar <- aggregate(Survived~Deck, titanic_train, mean)
bar

yo <- ggplot(data = bar, aes(y=Survived, x=as.factor(Deck)))
yo + geom_bar(stat="identity", width = 0.5, color="blue", fill="white")

# People who do not have a Cabin number assigned (Unknown) were likely to survive.
# Passenger 340 only one with Deck=T, he did not survive. Reassigning as "Unk" (category with less probabily of survival)
titanic_train$Deck[340]="Unk" 



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df = titanic_train

# dropping: 'PassengerId', 'Name', 'Ticket', 'Cabin'
df = titanic_train[,-c(1,4,9,11)]

colnames(df) #  "Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "title", "Age_n", "fam_size", "Deck" 

# canging to factor type categoritcal variables
df$Survived = as.factor(df$Survived)
df$Pclass = as.factor(df$Pclass)
df$Sex = as.factor(df$Sex)
df$Embarked = as.factor(df$Embarked)
df$title = as.factor(df$title)
df$Age_n = as.factor(df$Age_n)
df$Deck = as.factor(df$Deck)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(randomForest)

# Running without Age, SibSp, Parch
rf0 = randomForest(Survived~ Pclass+Sex+Fare+Embarked+title+Age_n+fam_size+Deck, data=df, mtry=1, ntree=500)  
print(rf0) # error = ~17%
print(importance(rf0))
varImpPlot(rf0)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# In rf0, replacing Age_n with Age
rf1 = randomForest(Survived~ Pclass+Sex+Fare+Embarked+title+Age+fam_size+Deck, data=df, mtry=3, ntree=500)  
print(rf1) # error = ~17%
print(importance(rf1))
varImpPlot(rf1)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# In rf1, replacing fam_size with SibSp and Parch
rf2 = randomForest(Survived~ Pclass+Sex+Fare+Embarked+title+Age+SibSp+Parch+Deck, data=df, mtry=3, ntree=500)  
print(rf2) # error = ~17%
print(importance(rf2))
varImpPlot(rf2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Removing Embarked from rf1
rf3 = randomForest(Survived~ Pclass+Sex+Fare+title+Age+fam_size+Deck, data=df, mtry=3, ntree=500)  
print(rf3) # error = ~16%
print(importance(rf3))
varImpPlot(rf3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# In rf3, replacing fam_size with SibSp and Parch
rf4 = randomForest(Survived~ Pclass+Sex+Fare+title+Age+SibSp+Parch+Deck, data=df, mtry=3, ntree=500)  
print(rf4) # error = ~16%
print(importance(rf4))
varImpPlot(rf4)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# removing Parch from rf4
rf5 = randomForest(Survived~ Pclass+Sex+Fare+title+Age+SibSp+Deck, data=df, mtry=3, ntree=500)  
print(rf5) # error = ~16%
print(importance(rf5))
varImpPlot(rf5)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Running without Age_n, SibSp, Parch
lg0 <- glm(Survived~Pclass+Fare+Sex+Embarked+title+Age+fam_size+Deck, data=df, family=binomial(link="logit"))  

library(QuantPsyc) # Has the MASS package, which has stepAIC

step <- stepAIC(lg0, direction="both")
step$anova # Deck and Embarked seem not signification per AIC.


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Running with interaction term between Age & fam_size
lg1 <- glm(Survived~Pclass+Fare+Sex+Embarked+title+Age*fam_size+Deck, data=df, family=binomial(link="logit"))  

stargazer(lg0,lg1,
          title="Regression Results", type="text", 
          column.labels=c("Logit-0","Logit-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) # lg1 => Interaction Age:fam_size not significant

# Running with SibSp & Parch instead of fam_size
lg2 <- glm(Survived~Pclass+Fare+Sex+Embarked+title+Age+SibSp+Parch+Deck, data=df, family=binomial(link="logit"))  

stargazer(lg0,lg1,lg2,
          title="Regression Results", type="text", 
          column.labels=c("Logit-0","Logit-1","Logit-2"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) # lg2 => SibSp & Parch significant

# Running with interaction term between Age & (SibSp & Parch)
lg3 <- glm(Survived~Pclass+Fare+Sex+Embarked+title+Age*SibSp+Age*Parch+Deck, data=df, family=binomial(link="logit"))  

stargazer(lg0,lg1,lg2,lg3,
          title="Regression Results", type="text", 
          column.labels=c("Logit-0","Logit-1","Logit-2","Logit-3"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) # lg1 => SibSp & Parch significant BUT make Age insignificant.


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(lmtest)

## Model fit assessment 
lg_null <- glm(Survived~1, data=df, family=binomial(link="logit")) # This is the command to run a logit on null model 

print("Logit-0")
lrtest(lg_null, lg0) # We compare the null model to our model to determine the model fit. The p-value for the chi-square tet is  less than 0.001. Thus, our model fits significantly better than the null model.

## Measuring the predictive power of the logit
pred = predict(lg0, data=df, type="response") 
head(pred)
return_prediction <- ifelse(pred >= 0.5,1,0)  
misClasificError <- mean(return_prediction != df$Survived) 
print(paste('Accuracy',1-misClasificError)) # the correct classification rate is 84.06%

print("Logit-1")
lrtest(lg_null, lg1) # We compare the null model to our model to determine the model fit. The p-value for the chi-square tet is  less than 0.001. Thus, our model fits significantly better than the null model.

## Measuring the predictive power of the logit
pred = predict(lg1, data=df, type="response") 
head(pred)
return_prediction <- ifelse(pred >= 0.5,1,0)  
misClasificError <- mean(return_prediction != df$Survived) 
print(paste('Accuracy',1-misClasificError)) # the correct classification rate is 84.06%



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print("Logit-2")
lrtest(lg_null, lg2) # We compare the null model to our model to determine the model fit. The p-value for the chi-square tet is  less than 0.001. Thus, our model fits significantly better than the null model.

## Measuring the predictive power of the logit
pred = predict(lg2, data=df, type="response") 
head(pred)
return_prediction <- ifelse(pred >= 0.5,1,0)  
misClasificError <- mean(return_prediction != df$Survived) 
print(paste('Accuracy',1-misClasificError)) # the correct classification rate is 83.84%

print("Logit-3")
lrtest(lg_null, lg3) # We compare the null model to our model to determine the model fit. The p-value for the chi-square tet is  less than 0.001. Thus, our model fits significantly better than the null model.

## Measuring the predictive power of the logit
pred = predict(lg3, data=df, type="response") 
head(pred)
return_prediction <- ifelse(pred >= 0.5,1,0)  
misClasificError <- mean(return_prediction != df$Survived) 
print(paste('Accuracy',1-misClasificError)) # the correct classification rate is 83.73%



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# removing Embarked and Deck from lg0
lg4 <- glm(Survived~Pclass+Fare+Sex+title+Age+fam_size, data=df, family=binomial(link="logit"))  
# removing Embarked and Deck from lg3
lg5 <- glm(Survived~Pclass+Fare+Sex+title+Age*SibSp+Age*Parch, data=df, family=binomial(link="logit"))  

stargazer(lg0,lg1,lg2,lg3,lg4,lg5,
          title="Regression Results", type="text", 
          column.labels=c("Logit-0","Logit-1","Logit-2","Logit-3","Logit-4","Logit-5"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) # 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print("Logit-4")
lrtest(lg_null, lg4) # We compare the null model to our model to determine the model fit. The p-value for the chi-square tet is  less than 0.001. Thus, our model fits significantly better than the null model.

## Measuring the predictive power of the logit
pred = predict(lg4, data=df, type="response") 
head(pred)
return_prediction <- ifelse(pred >= 0.5,1,0)  
misClasificError <- mean(return_prediction != df$Survived) 
print(paste('Accuracy',1-misClasificError)) # the correct classification rate is 82.83%

print("Logit-5")
lrtest(lg_null, lg5) # We compare the null model to our model to determine the model fit. The p-value for the chi-square tet is  less than 0.001. Thus, our model fits significantly better than the null model.

## Measuring the predictive power of the logit
pred = predict(lg5, data=df, type="response") 
head(pred)
return_prediction <- ifelse(pred >= 0.5,1,0)  
misClasificError <- mean(return_prediction != df$Survived) 
print(paste('Accuracy',1-misClasificError)) # the correct classification rate is 82.94%


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(xgboost)
library(Matrix)

print(names(df))


# df is a dataframe with many categorical features. Need df and target as matrix 
# lg0 => Survived~Pclass+Fare+Sex+Embarked+title+Age+fam_size+Deck
x0 <- sparse.model.matrix(lg0, data=df)
y <- as.matrix(df$Survived)
print("bst0")
bst = xgboost(data=x0, label=y, objective="binary:logistic", nrounds=500, print_every_n=10)

# lg1 => Survived~Pclass+Fare+Sex+Embarked+title+Age*fam_size+Deck
x1 <- sparse.model.matrix(lg1, data=df)
print("bst1")
bst = xgboost(data=x1, label=y, objective="binary:logistic", nrounds=500, print_every_n=10) #seems best with error 1.23%

# lg2 => Survived~Pclass+Fare+Sex+Embarked+title+Age+SibSp+Parch+Deck
x2 <- sparse.model.matrix(lg2, data=df)
print("bst2")
bst = xgboost(data=x2, label=y, objective="binary:logistic", nrounds=500, print_every_n=10)

# lg3 => Survived~Pclass+Fare+Sex+Embarked+title+Age*SibSp+Age*Parch+Deck
x3 <- sparse.model.matrix(lg3, data=df)
print("bst3")
bst = xgboost(data=x3, label=y, objective="binary:logistic", nrounds=500, print_every_n=10)

# lg4 => Survived~Pclass+Fare+Sex+title+Age+fam_size
x4 <- sparse.model.matrix(lg4, data=df)
print("bst4")
bst = xgboost(data=x4, label=y, objective="binary:logistic", nrounds=500, print_every_n=10)

# lg5 => Survived~Pclass+Fare+Sex+title+Age*SibSp+Age*Parch
x5 <- sparse.model.matrix(lg5, data=df)
print("bst5")
bst = xgboost(data=x5, label=y, objective="binary:logistic", nrounds=500, print_every_n=10)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(titanic_test,200)

stargazer(titanic_test, type="text", median=TRUE, iqr=TRUE,digits=1, title="Descriptive Statistics")

summary(titanic_test) # Age => 86 NA's. Fare => 1 NA.  Need those for prediction


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Using the title to estimate the age of those individuals with NA's

at <- ggplot(data = titanic_test, aes(x=title, y=Age))
at + geom_boxplot() #boxplot showing agy by title

# This "for" loop checks for Age NAs and assigns mean Age to those.  Basically mapping the data set to the list of title/meanAge
for (i in (1:length(titanic_test$Age)))
{if (is.na(titanic_test$Age[i])) 
    {titanic_test$Age[i] <- list$Age[match(titanic_test$title[i],list$title)]} 
    else {}
}

head(titanic_test,250)
summary(titanic_test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Using the Pclass to estimate the Fare of those individuals with NA's

titanic_test[is.na(titanic_test$Fare)]

list = aggregate(Fare ~ Pclass, titanic_both, mean)
list

# This "for" loop checks for Fare NAs and assigns mean Fare.  Basically mapping the data set to the list of Pclass/meanFare
for (i in (1:length(titanic_test$Fare)))
{if (is.na(titanic_test$Fare[i])) 
    {titanic_test$Fare[i] <- list$Fare[match(titanic_test$Pclass[i],list$Pclass)]} 
    else {}
}

titanic_test[titanic_test$PassengerId==1044]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# grouping titles as was done for the training set

# Regarding Mme and Mlle, from https://www.frenchtoday.com/blog/french-culture/madame-or-mademoiselle-a-delicate-question they correspond to the categories Mrs and Miss, respectively. As a consequence, we will assign them to those titles.

for (i in (1:length(titanic_test$Age)))
{if (titanic_test$title[i]=="Mme.")
    {titanic_test$title[i] <- "Mrs."} 
    else if (titanic_test$title[i]=="Mlle.")
    {titanic_test$title[i] <- "Miss."}
    else if (titanic_test$title[i]!="Mr." & titanic_test$title[i]!="Master." & titanic_test$title[i]!="Mrs." 
             & titanic_test$title[i]!="Miss." & titanic_test$title[i]!="Dr." & titanic_test$title[i]!="Rev.")
    {titanic_test$title[i] <- "Other."}
    else {}
}

head(titanic_test,250)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# creating family size column from SibSp (# of siblings/spouses aboard the Titanic) & parch (# of parents/children aboard the Titanic)
titanic_test$fam_size <- titanic_test$SibSp + titanic_test$Parch

# extracting deck from cabin as was done for training set
titanic_test$Deck = ifelse(titanic_test$Cabin=="","Unk",substring(titanic_test$Cabin, 1, 1))
titanic_test

colnames(titanic_test)
colnames(df)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dft = titanic_test # "PassengerId", "Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Ticket", "Fare", "Cabin"       "Embarked", "title", "fam_size", "Deck"   

# dropping: 'Name', 'Ticket', 'Cabin'
dft = titanic_test[,-c(3,8,10)]

colnames(dft) #  "PassengerId", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "title", "fam_size", "Deck" 
colnames(df)
dft[dft$Embarked=="C"]
# need dummy for survived so that can be transformed to matrix
colnames(dft)[1] <- "Survived"

# canging to factor type categoritcal variables
dft$Pclass = as.factor(dft$Pclass)
dft$Sex = as.factor(dft$Sex)
dft$Embarked = as.factor(dft$Embarked)
dft$title = as.factor(dft$title)
dft$Deck = as.factor(dft$Deck)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Based on the several models run, best seems to be bst with lg1.  lg3 seems like the best conceptual model though
# lg3 => Survived~Pclass+Fare+Sex+Embarked+title+Age*SibSp+Age*Parch+Deck
bst = xgboost(data=x3, label=y, objective="binary:logistic", nrounds=50, print_every_n=10, eta=0.1, colsample_bytree=1/3, max_depth=6, sub_sample=3, min_child_weight=3, gamma=0.001) # Included a lot of params most at default value.  Can easily changed them for tunning.

xt <- sparse.model.matrix(Survived~Pclass+Fare+Sex+Embarked+title+Age*SibSp+Age*Parch+Deck, data=dft)

colnames(xt)
colnames(x3)
help(logit)
pred <- predict(bst,xt)
head(pred)

# Transforming probabilities to 1 if >= 0.5, else to 0
return_prediction <- ifelse(pred >= 0.5,1,0)  
return_prediction

submission <- as.data.frame(cbind(titanic_test$PassengerId,return_prediction))
colnames(submission) <- c("PassengerId", "Survived")
submission

write.csv(submission,file="Submission.csv")
# Need to delete index before submission


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# lg1 => Survived~Pclass+Fare+Sex+Embarked+title+Age*fam_size+Deck
# lg3 => Survived~Pclass+Fare+Sex+Embarked+title+Age*SibSp+Age*Parch+Deck
pred = predict(lg1, newdata=dft, type="response") 
head(pred)
return_prediction <- ifelse(pred >= 0.5,1,0) 
return_prediction

submission <- as.data.frame(cbind(titanic_test$PassengerId,return_prediction))
colnames(submission) <- c("PassengerId", "Survived")
submission

write.csv(submission,file="Submission.csv")
# Need to delete index before submission


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# rf3 => Survived~ Pclass+Sex+Fare+title+Age+fam_size+Deck
# rf4 => Survived~ Pclass+Sex+Fare+title+Age+SibSp+Parch+Deck

pred = predict(rf4, newdata=dft, type="response") 
head(pred)

return_prediction <- ifelse(pred==1,1,0) 

submission <- as.data.frame(cbind(titanic_test$PassengerId,return_prediction))
colnames(submission) <- c("PassengerId", "Survived")
submission

write.csv(submission,file="Submission.csv")
# Need to delete index before submission


