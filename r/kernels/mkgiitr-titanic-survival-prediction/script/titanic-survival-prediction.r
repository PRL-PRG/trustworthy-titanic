
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.

#Loading library
library(tidyverse)
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

#Checking the files loaded into kernel
list.files(path = "../input")

#Checking the working directory
getwd()
#loading the data files
train <- read.csv("../input/train.csv",stringsAsFactors=F )
test <- read.csv("../input/test.csv",stringsAsFactors=F)

full <- bind_rows(train,test)
#Checking the structure of data
str(full)


#Create the user define function for descriptive analysis
var_summ <- function(x){
    if(class(x)=="numeric"){
        var_type=class(x)
        n <- length(x)
        n_miss <- sum(is.na(x))
        mean <- mean(x,na.rm=T)
        std <- sd(x,na.rm=T)
        min <- min(x,na.rm=T)
        max <- max(x,na.rm=T)
        return(c(var_type=var_type,n=n,n_miss=n_miss,mean=mean,std=std,min=min,max=max))
    }
    else{
        var_type=class(x)
        n = length(x)
        n_miss = sum(is.na(x))
         return(c(n=n,n_miss=n_miss))
    }
}

#checking the variable is numeric or not.
num_var <- sapply(full,is.numeric)

#apply above defined function on the data
num_data <- t(data.frame(apply(full[num_var],2,var_summ)))
num_data
cat_data <- data.frame(apply(full[!num_var],2,var_summ))
cat_data



#playing with name variable-extracting title of name
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

#Checking title with gender
table(full$Sex,full$Title)
#combined the rare title into group
rare_title <- c("Capt","Col","Don","Dona","Dr","Jonkheer","Lady","Major","Mlle","Mme","Ms","Rev","Sir","the Countess")

full$Title[full$Title %in% rare_title] <- "rare_title"

#Show title count by sex.
table(full$Sex,full$Title)

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                      function(x) strsplit(x, split = '[,.]')[[1]][1])

#family size calculation
full$Fsize <- full$Parch+full$SibSp+1

#create a family variable
full$Family <- paste(full$Surname, full$Fsize, sep='_')

ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

#grouping family by its size
full$FsizeD[full$Fsize==1] <- "Singleton"
full$FsizeD[full$Fsize<5 & full$Fsize>1]  <- "Small"
full$FsizeD[full$Fsize>4] <- "Large"

#Checking survival by familysize
table(full$Survive,full$FsizeD)

# Create a Deck Variable
full$Cabin[1:28]
strsplit(full$Cabin[2],NULL)[[1]][1]
full$Deck <- factor(sapply(full$Cabin,function(x) strsplit(x,NULL)[[1]][1]))
table(full$Deck)                           


#Missing Value Treatment
unique(full$Embarked)
table(full$Embarked)
full[c(62,830),]

#removing missing value
embarked_value <- full %>% filter(PassengerId !=62 & PassengerId !=830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embarked_value,aes(x=Embarked,y=Fare, fill=factor(Pclass)))+
geom_boxplot()+
geom_hline(aes(yintercept=80), colour='red', linetype='dashed', lwd=2)+
scale_y_continuous(labels=dollar_format())+
theme_few()

# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62,830)] <- 'C'


#Fare missing Value Treatmet
full[is.na(full$Fare),]
full[1044,]

#cheking fare distribution for class=3 & embarked=S
ggplot(full[full$Pclass==3 & full$Embarked=='S',],aes(x=Fare))+
geom_density(fill='#99d6ff',alpha=0.4)+
geom_vline(aes(xintercept=median(Fare,na.rm=T)),colour='red',linetype='dashed',lwd=1)+
scale_x_continuous(labels=dollar_format())+
theme_few()

# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass==3 & full$Embarked=='S',]$Fare,na.rm=T)

#Age missing value treatment with predictive imputation
#no of missing age value
sum(is.na(full$Age))

#we are going to use MICE imputation
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Title','Surname','Family','FsizeD')
full[factor_vars] <- lapply(full[factor_vars],function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf')

#save the complete output
mice_output <- complete(mice_mod)                            

# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
  col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
  col='lightgreen', ylim=c(0,0.04))

#now replace the age to mice age
full$Age <- mice_output$Age
sum(is.na(full$Age))


# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Show counts
table(full$Child, full$Survived)

# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

# Show counts
table(full$Mother, full$Survived)

# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

#Now all the variable is treated & going for the prediction using randomforest
# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]


#Building the Model
#set seed
set.seed(101)

#build the model using random forest
rf_model <- randomForest(factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+FsizeD+Child+Mother,data=train)
rf_model
#Giving 16.61% Error which we can accept right now

#Applyin the model to test data set to predict
prediction <- predict(rf_model,test)

#Save the prediction into data frame
solution <- data.frame(PassengerID=test$PassengerId,Survived=prediction)
table(solution$Survived)

#Write solution to a csv file
write.csv(solution,'rf_model_solution.csv',row.names=F)




