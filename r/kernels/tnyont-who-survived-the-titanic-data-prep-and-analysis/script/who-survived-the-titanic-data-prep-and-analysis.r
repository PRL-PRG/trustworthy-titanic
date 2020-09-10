
library(tidyverse)
library(plyr)
library(dplyr)
library(gridExtra) #for plotting
library(Amelia) #for missing values
library(corrplot) #for correlation
library(mice) #for missing values
library(naniar) #For missing values plot
library(boot) #For diognastic plots
library(car) # for avplots
library(caret)

set.seed(10)
train=read_csv("../input/train.csv")
test=read_csv("../input/test.csv")

str(train)

str(test)

test$Survived=NA
data=rbind(train, test)
str(data)

ggplot(data [!is.na(data$Age),], aes(x=Age, fill= Sex)) +
    geom_histogram(position="dodge", bins=25) 

pt1= ggplot(data, aes(x=Embarked, fill= Sex)) +
    geom_bar(position="dodge")+
    geom_text(stat='count', aes(label=..count..), vjust=-0, color="black", size=2)
pt2= ggplot(data, aes(x=Pclass, fill= Sex)) +
    geom_bar(position="dodge")+
    geom_text(stat='count', aes(label=..count..), vjust=0.5, color="black", size=2)
pt3= ggplot(data, aes(x=SibSp, fill= Sex)) +
    geom_bar(position="dodge")+
    geom_text(stat='count', aes(label=..count..), vjust=0.5, color="black", size=2)
pt4= ggplot(data[!is.na(data$Survived),], aes(x=Survived, fill= Sex)) +
    geom_bar(position="dodge")+
    geom_text(stat='count', aes(label=..count..), vjust=0.5, color="black", size=2)
pt5=ggplot(data, aes(x=Parch, fill= Sex)) +
    geom_bar(position="dodge")+
    geom_text(stat='count', aes(label=..count..), vjust=0.5, color="black", size=2)
pt6=ggplot(data, aes(x=Sex)) +
    geom_bar(color="skyblue", fill="skyblue")+
    geom_text(stat='count', aes(label=..count..), vjust=1.5, color="black", size=3)
grid.arrange(pt1,pt2,pt3,pt4,pt5,pt6, nrow=3)

gg_miss_var(data)

#to check where these missing values are located in the dataset
missmap(data[-1], col=c('grey', 'Maroon'), y.cex=0.5, x.cex=1) 

sort(sapply(data, function (x) sum(is.na(x))), decreasing=T)

data %>%
    select_if(is.numeric) %>% # get only the numeric columns
    replace(is.na(.), 0) %>% # replace all na values with 0
    cor() %>% # calculate the correlations
    corrplot() # plot the correlations

#Check if we have duplicated rows
duplication <- data_frame(duplicated = duplicated(data), row = 1:nrow(data)) %>%
    filter(duplicated == T)

print("The number of duplicated rows in this data set is: ")
duplicated(data) %>%
    sum()

which(is.na(data$Fare)) #to find the row number

data[1044,]

d=subset(data, !is.na(data$Fare))
mean(d$Fare[d$Pclass=="3" & d$Embarked=="S"])

data$Fare[1044]=14.44

table(data$Embarked)
which(is.na(data$Embarked))

data[62,]
data[830,]

data$Embarked[c(62,830)]= "S"

empty <- mice(data, maxit=0) #create an empty model to take the parameters from 
method <- empty$method
predictorMatrix <- empty$predictorMatrix

imdata <- mice(data, method, predictorMatrix, m=5) # create multiple imputations
imdata <- complete(imdata) #select one of them

newdata=data #create a new data set to keep the old one in case I need it later
newdata$Age=imdata$Age 
summary(newdata$Age)

AgeOld=ggplot(data [!is.na(data$Age),], aes(x=Age)) +
    geom_histogram(position="dodge", bins=25, color="Maroon", fill="LightGrey") +
    labs(title="Age before Imputation")
AgeNew=ggplot(newdata, aes(x=Age)) +
    geom_histogram(position="dodge",bins=25, color="Maroon", fill="LightGrey") +
    labs(title="Age after Imputation")
grid.arrange(AgeOld,AgeNew, nrow=1)

pnames=newdata$Name
title <-  gsub("^.*, (.*?)\\..*$", "\\1", pnames)
newdata$Title=title
table(newdata$Sex, newdata$Title)

newdata$Title[newdata$Title %in% c("Lady","Dona","the Countess", "Mlle","Ms", "Dr") & 
           newdata$Sex=="female"]="Miss" #we have one female Dr so had to add the condition
newdata$Title[newdata$Title=="Mme"]="Mrs"
newdata$Title[newdata$Title %in% c("Capt","Col","Don", "Dr","Jonkheer", "Major","Rev")]="Sir"
table(newdata$Sex, newdata$Title)

newdata$FamilyName=gsub(",", "",word(newdata$Name)) #extract first word in Name and remove the comma
head(newdata[,c("Name","FamilyName", "SibSp", "Parch")], 10)

newdata$SoloPass=0
newdata$SoloPass[newdata$SibSp+newdata$Parch==0]=1 #Not a solo passenger
newdata$SoloPass[newdata$SibSp+newdata$Parch!=0]=0 #A solo passenger

newdata$FamilyCount=0
newdata$FamilyCount=newdata$SibSp+newdata$Parch + 1 
#+1 is to count the passenger with the family member

#Let's check if we got it right in a sample
head(newdata[,c("Name","FamilyName", "SibSp", "Parch", "SoloPass", "FamilyCount", "Survived")], 15)

ggplot(newdata, aes(x=FamilyCount, fill =Sex)) +
    geom_bar(position='dodge')

fam=newdata %>%
    filter (newdata$FamilyCount!=1) #to exclude passenger travelling solo

ggplot(fam, aes(x=Survived, fill = Sex)) +
    geom_bar(position="dodge") +
    labs(title="Passengers Survival by Family Size [Survived = 1]") +
    facet_wrap(~FamilyCount)+
    theme(legend.position="top") +
    geom_text(stat='count', aes(label=..count..), vjust=0.3, color="black", size=2.5)   


ggplot(fam, aes(x=Pclass)) +
    geom_bar(position="dodge", color="Maroon", fill="Maroon") +
    labs(title="Passengers Class by Family Size") +
    facet_wrap(~FamilyCount)+
    geom_text(stat='count', aes(label=..count..), vjust=-0.1, color="black", size=2.5)   

fam7=subset(fam, fam$FamilyCount==7 & fam$Pclass==3)
head(fam7)

#Let's take the Andersson family and check out their members.
#fam7Ander=subset(fam, fam$FamilyCount==7 & fam$Pclass==3 &FamilyName=="Andersson")
fam7Ander=subset(fam,  FamilyName=="Andersson")
dim(fam7Ander)
head(fam7Ander[,c("Name","FamilyName", "Age", "SibSp", "Parch", "SoloPass", 
                  "FamilyCount", "Survived")],9)
famandersson=subset(newdata,  FamilyName=="Andersson")
head(famandersson[,c("Name","FamilyName", "Age", "SibSp", "Parch", "SoloPass", 
                  "FamilyCount", "Survived")],11)

lcount=as.data.frame(table(newdata$FamilyName))
colnames(lcount) [colnames(lcount)=="Var1"]="Lname"
nrow(lcount) # Number of unique family names
lcount[lcount$Lname=="Andersson",] # checking our values

children=subset(newdata, newdata$Age<=17)
adults=subset(newdata, newdata$Age>17)
dim(children)
dim(adults)

child=ggplot(children, aes(x=Survived, fill=Sex)) +
    geom_bar(position='dodge')+
    theme(legend.position="top") +
    labs(title="Children's Survival")
adult=ggplot(adults, aes(x=Survived, fill=Sex)) +
    geom_bar(position='dodge')+
    theme(legend.position="top") +
    labs(title=" Adult's Survival")
grid.arrange(child,adult, nrow=1)

sum(is.na(newdata$Cabin))
head(newdata$Cabin,10)

sapply(newdata, class)

newdata$Sex[newdata$Sex=="female"]="1"
newdata$Sex[newdata$Sex=="male"]="0"
newdata$Sex=as.numeric(newdata$Sex)

newdata$Embarked[newdata$Embarked=="C"]="1"
newdata$Embarked[newdata$Embarked=="Q"]="2"
newdata$Embarked[newdata$Embarked=="S"]="3"
newdata$Embarked=as.numeric(newdata$Embarked)

sapply(newdata, class)

newdata %>%
    select_if(is.numeric) %>% # get only the numeric columns
    replace(is.na(.), 0) %>% # replace all na values in Survived with 0
    cor() %>% # calculate the correlations
    corrplot() # plot the correlations

training=subset(newdata,!is.na(newdata$Survived))
dim(training)

testing=subset(newdata,is.na(newdata$Survived))
dim(testing)

modelone=glm(Survived~ Pclass + Sex + Age+ SibSp + Parch + Fare + Embarked + 
            SoloPass, #include the variables that could be important for survival
            data=training, 
            family="binomial") #logical regression

glm.diag.plots(modelone) #plot to see if our model is a good fit for the data and problem

table (training$Survived)

summary(modelone)

# output plots
par(mfrow = c(2,2)) 
# diagnostic plots
plot(modelone)

avPlots(modelone)

predone= predict(modelone, training, type="response") #we use response to find probabilities
table(training$Survived, predone>0.5) #training set (baseline) predictions

predonematrix=ifelse(predone> 0.5,1,0)
confusionMatrix(training$Survived,predonematrix)

testpred=predict(modelone,testing, type="response") 
tests=as.data.frame(testpred)
testing$Survival=as.numeric(unlist(tests))
testing$Survival=ifelse(testing$Survival>=0.5,1,0)

table(testing$Survival)

table(testing$Survival,testing$Sex)
