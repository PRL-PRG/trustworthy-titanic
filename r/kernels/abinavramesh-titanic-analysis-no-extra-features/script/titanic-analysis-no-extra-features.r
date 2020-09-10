
library(tidyverse) 
library(readr)
library(dplyr) # For Data-frame manipulation
library(ggplot2) # For visualizations
library(stringr)
library(lubridate)
library(tidyr)
library(readxl)
library(pROC) #for obtaining ROC and AUC
library(plotly) # For interactive visualizations
library(class) # For K-NN algorithm
library(dummies) # Package used for dummifying variables
library(randomForest) # Package used for Random Forest classification
library(mice) # For imputing variables
library(purrr) # For functional programming in R

train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")
gender_submission <- read_csv("../input/gender_submission.csv")

summary(train)

summary(test)

full<-bind_rows(train %>% select(-Survived),test)
summary(full)

#Fare is dependent on pclass and embarked
#What value does the missing value for Fare have for columns pclass and embarked
full[is.na(full$Fare),]

# Since the missing record had PClass = 3 and Embarked="S", we can see how Fare is distributed for these conditions

 ggplotly(full %>% select(Pclass,Fare,Embarked) %>% filter(Pclass==3,Embarked=="S") %>% ggplot(aes(Fare))+geom_density() )
 full %>% select(Pclass,Fare,Embarked) %>% filter(Pclass==3,Embarked=="S") %>%summarize(median(Fare,na.rm=TRUE))

#Hence we can populate the missing Fare wih the median
full[is.na(full$Fare),][["Fare"]]<-as.data.frame(full %>% select(Pclass,Fare,Embarked) %>% filter(Pclass==3,Embarked=="S"))[["Fare"]]%>%median(na.rm = TRUE)
sum(is.na(full$Fare))

# Checking the columns that have NA in Embarked
full[is.na(full$Embarked),]

 ggplotly(full %>% select(Pclass,Fare,Embarked) %>% filter(Pclass==1) %>% ggplot(aes(y=Fare,x=Embarked))+geom_boxplot() )

full[is.na(full$Embarked),]$Embarked<-"C"

full$Pclass <- as.factor(full$Pclass)
full$Sex <- as.factor(full$Sex)
  
# Random Forest classifier made using variables Pclass, Sex, SibSp, Parch
rf<-randomForest(formula= Age ~Pclass+Sex+SibSp+Parch, data = full%>% filter(!is.na(Age)))
full$Age_rf_imputation <- full$Age  
full[is.na(full$Age_rf_imputation),][["Age_rf_imputation"]] <-predict(rf,newdata =full %>% filter(is.na(Age)) )

head(full)

ggplotly(ggplot(full,aes(Age))+geom_histogram())


ggplotly(ggplot(full,aes(Age_rf_imputation))+geom_histogram())

sum(is.na(full$Age))

summary(full)

# Convert the following columns into factors
convert_factors <- c("PassengerId","Pclass","Sex","Embarked")
full[convert_factors]<- map(.x=convert_factors, .f= function(x){as.factor(full[[x]])})

# Set a random seed
  set.seed(129)

# Perform mice imputation, including only useful variables:
  mice_mod <- mice(full[, names(full) %in% c("Pclass","Sex","Embarked","Age","SibSp","Parch","Fare")], method='rf') 
  
  mice_output <- complete(mice_mod)

# Distribution of original Age distributions
ggplotly(ggplot(full,aes(Age))+geom_histogram())



# Distribution of imputed Age distribution using "mice"
ggplotly(ggplot(mice_output,aes(Age))+geom_histogram())

full[c("Pclass","Sex","Embarked","Age","SibSp","Parch","Fare")] <-mice_output[c("Pclass","Sex","Embarked","Age","SibSp","Parch","Fare")]

head(full)

# Remove the column Age_rf_imputation
full$Age_rf_imputation <- NULL

#Split it back into train and test sets
train<-bind_cols(full[1:dim(train)[1],],Survived=train$Survived)
test <-full[-(1:dim(train)[1]),]

plot_ly(train %>% group_by(Survived) %>% count(), labels = ~Survived, values = ~n, type = 'pie') %>%
  layout(title = 'Total Survived vs Not Survived',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  ggplotly(train %>% group_by(Survived,Pclass) %>% count() %>% ggplot(aes(x=factor(Pclass),y=n,fill=factor(Survived))) + geom_col(position = "fill") + labs(x="Passenger Class",y="Proportion of People", title = "Proportion survived vs Not-Survived with respect to Passenger Class")+scale_fill_discrete(name = "Survived"))

  ggplotly(train %>% group_by(Survived,Pclass) %>% count() %>% ggplot(aes(x=factor(Survived),y=n,fill=factor(Pclass))) + geom_col(position = "fill") + labs(x="Survived",y="Proportion of People", title = "Proportion Survived vs Not-Survived")+scale_fill_discrete(name = "Passenger Class"))

  ggplotly(train %>% group_by(Survived,Sex) %>% count() %>% ggplot(aes(x=factor(Sex),y=n,fill=factor(Survived))) + geom_col(position = "fill") + labs(x="Sex",y="Proportion of People", title = "Proportion survived vs Not Survived")+scale_fill_discrete(name = "Survived"))

 ggplotly(train %>% group_by(Survived,Sex) %>% count() %>% ggplot(aes(x=factor(Survived),y=n,fill=factor(Sex))) + geom_col(position = "fill") + labs(x="Survived",y="Proportion of People", title = "Proportion survived vs Not Survived")+scale_fill_discrete(name = "Gender"))

#Number of people with different number of siblings
train %>% group_by(SibSp) %>% count()  

#Percentage of people Not-Survived  
ggplotly(train %>% group_by(SibSp,Survived) %>%summarise(n=n())%>% ungroup()%>% group_by(SibSp) %>%mutate(Total_per_sibling=sum(n),perc_dead=(n/Total_per_sibling)*100)%>%filter(Survived==0)%>%ggplot(aes(x=factor(SibSp),y=perc_dead)) + geom_col(position = "dodge",fill="skyblue") + labs(x="Number of siblings or spouses",y="Percentage of People Not-Survived (%)", title = "Percentage of people Not-Survived"))

# Count of people with different number of Parents/children
train %>% group_by(Parch) %>% count()  

# Percentage of people dead with respect to the number of children/parents
ggplotly(train %>% group_by(Parch,Survived) %>%summarise(n=n())%>% ungroup()%>% group_by(Parch) %>%mutate(Total_per_sibling=sum(n),perc_dead=(n/Total_per_sibling)*100)%>%filter(Survived==0)%>%ggplot(aes(x=factor(Parch),y=perc_dead)) + geom_col(position = "dodge",fill="skyblue") + labs(x="Number of Parents or children",y="Percentage of People Not-Survived (%)", title = "Percentage of people Not-Survived"))

#Number of people at different embarking stations
train %>% group_by(Embarked) %>% count() 

ggplotly(train %>% filter(!is.na(Embarked))%>%group_by(Embarked,Survived) %>%summarise(n=n())%>% ungroup()%>% group_by(Embarked) %>%mutate(Total_per_sibling=sum(n),perc_dead=(n/Total_per_sibling)*100)%>%filter(Survived==0)%>%ggplot(aes(x=factor(Embarked),y=perc_dead)) + geom_col(position = "dodge",fill="skyblue") + labs(x="Station embarked",y="Percentage of People Not-Survived (%)", title = "Percentage of people Not-Survived"))

  # By Cabin
 train %>% mutate(Cabin_corrected= substring(Cabin,1,1)) %>% filter(!is.na(Cabin_corrected)) %>% count(Cabin_corrected)%>% mutate(Total_count=sum(n))

ggplotly(train %>% select(Survived,Age,Sex)%>% ggplot(aes(x=factor(Survived),y=Age))+geom_boxplot()+facet_wrap(~factor(Sex)) + labs(x="Survived"))

# Converting variables into factors in training and test set

  train$Survived <- factor(train$Survived)
  train$Pclass <- factor(train$Pclass)
  train$Sex <- factor(train$Sex)
  train$Embarked <- factor(train$Embarked)

  
  test$Pclass <- factor(test$Pclass)
  test$Sex <- factor(test$Sex)
  test$Embarked <- factor(test$Embarked)
  

# Logistic regression model built on important variables

lin_reg<- glm(formula = Survived ~., data = train %>% select(-c(Name,Cabin,Ticket,PassengerId)),family = "binomial")
summary(lin_reg)

prediction_linreg<-predict(lin_reg,newdata = train %>% select(-c(Name,Cabin,Ticket,Survived,PassengerId)) ,type = "response") 

table(train$Survived,as.numeric(prediction_linreg>0.5))

# Import the library pROC to plot ROC values
 library(pROC)

# Create an ROC object 
 ROC <- roc(train$Survived, as.numeric(prediction_linreg>0.5))

#Plot the ROC curve
 plot(ROC, col = "blue")

# Find the Area under the curve for prediction of training set
paste("The Area under the curve is : ", auc(ROC)) 

paste("As we can see, the accuracy is",100*((477+237)/(105+72+477+237)),"%"," on the training set") 

# Table of predictions of test set
(cm<-as.matrix(table(Actual = gender_submission$Survived,Predicted = as.numeric(predict(lin_reg,newdata = test ,type = "response")>0.5))))

# Create an ROC object 
 ROC_test <- roc(gender_submission$Survived, as.numeric(predict(lin_reg,newdata = test ,type = "response")>0.5))

#Plot the ROC curve
 plot(ROC_test, col = "blue")

# Find the Area under the curve for prediction of training set
paste("The Area under the curve is : ", auc(ROC_test)) 
paste("As we can see, the accuracy is",100*(sum(diag(cm))/sum(cm)),"%"," on the test set")

#Dummy training set only with the required columns 
x<-train %>% select(-c(Name,Cabin,Ticket,PassengerId))%>% filter(!is.na(Age) , !is.na(Embarked))

 knn_model<-knn(train = x %>% as.data.frame()%>% select(-Survived)%>% dummy.data.frame(dummy.class = "factor"),test =cbind(test,test_actual=gender_submission$Survived)[names(x %>% select(-Survived))]%>% filter(!is.na(Age),!is.na(Fare))%>% as.data.frame() %>% dummy.data.frame(),cl=x$Survived)


cm<-as.matrix(table( Actual= as.factor(as.vector(cbind(test,test_actual=gender_submission$Survived)[c(names(x %>% select(-Survived)),"test_actual")] %>% filter(!is.na(Age),!is.na(Fare)) %>% select(test_actual))[["test_actual"]]),Predicted = knn_model))
paste("As we can see, the accuracy is",100*(sum(diag(cm))/sum(cm)),"%"," on the training set")

train1<-bind_cols(full[1:dim(train)[1],],Survived=train$Survived)
test1 <-full[-(1:dim(train)[1]),]
train1$Survived <- as.factor(train1$Survived)
rf<-randomForest(formula = Survived ~ Pclass+Sex+Age+SibSp + Parch + Fare + Embarked, data= train1)
(cm <- as.matrix(table(Actual = gender_submission[["Survived"]],Predicted = predict(rf,test1))))
paste("As we can see, the accuracy is",100*(sum(diag(cm))/sum(cm)),"%"," on the test set")
