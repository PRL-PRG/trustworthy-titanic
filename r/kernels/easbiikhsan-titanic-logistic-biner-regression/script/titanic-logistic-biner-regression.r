
# This is my first kernel using R.
# Titanic Disaster is something so serious in sea transportation. 
# First of all, I begin the analysis this data set using EDA (Exploratory data Analysis) 
# I consist of Visualization and Descriptive analysis, and also data preprocessing like missing values handle.
# I am going to use Logistic Biner Regression, with respons : Survival ==> 0 = No, 1 = Yes
# And many predictors like : 

# 1) PClass
# 2) embarked
# 3) sibsp
# 4) parch


library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
train <- read_csv("../input/train.csv")
head(train,10)

#I try to see the missing value in each column. Here, i am using sapply funcntion :
attach(train)
# we see how many missing value in each attributes, here:
sapply(train, function(x) sum(is.na(x)))


#And I try to plot the missing values VS observation with missmap from Amelia's package. 
library(Amelia)
missmap(train)
#from the summary we can see that cabin, age, and embarked variables have the missing data 
#but the cabin variable has much more missing value, so i drop it from the dataset. 
#and for the age variable, i use mean imputation. And for Embarket variable, i use mode statistics.

d_train = train[,-1]

#This is below the process of imputation Age's attribute using mean
d_train$Age[is.na(d_train$Age)] = mean(d_train$Age, na.rm = T) 
# This is below the process of imputation Embarked's attribute using mode, using getmode function
# We use mode beacuse the data is discrete
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}
d_train$Embarked[is.na(d_train$Embarked)] = getmode(d_train$Embarked)

## Modelling Data
#After the EDA step, i try to modelling the data to model the survived variable depends on other variable. 
#here i am using logistic biner regression to classified survived = 1, or not survive = 0.
model1 <- glm(Survived ~ Pclass + Age + Sex +SibSp + Parch + Fare + Embarked, family = binomial(link='logit'), data=d_train)
summary(model1)


#After modelling, we see that Pclass, Age, Sex, SibSp have siginificant in model. 
#And for the goodness of the model we see AIC 802,78.
#next i try to use the 2 other model like below : 
model2 <- glm(Survived ~ Pclass + Age + Sex +SibSp, family = binomial(link='logit'), data=d_train)
summary(model2)

#and the model3 like below : 
model3 <- glm(Survived ~ Pclass + Age + Sex, family = binomial(link='logit'), data=d_train)
summary(model3)

# In this cases, i will use logistic regression with step function to find best subset wit AIC lower
# Model 1 as full model, and use find AIC backward as below
model.aic.backward <- step(model1, direction = "backward", trace = 1)

#we see that the AIC is lower at 799.91 using logistic biner regression, with variable X -> Pclass, Age, Sex SibSp, and Embarked

model_final <- glm(Survived ~ Pclass + Age + Sex +SibSp + Embarked, family = binomial(link='logit'), data=d_train)
summary(model_final)
##Conclusion
#that's all about modelling the data. 
#Thanks.

# Thats's 
