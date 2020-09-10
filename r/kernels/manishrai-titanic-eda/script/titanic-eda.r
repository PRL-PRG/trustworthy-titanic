
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

train = read_csv("../input/train.csv")
test = read_csv("../input/test.csv")

train$NamePrefix = gsub(".*,(.*)\\..*", "\\1", train$Name)
head(train, n = 10)

test$NamePrefix = gsub(".*,(.*)\\..*", "\\1", test$Name)
head(test, n = 10)

by(data = train[, c('Survived', 'Age', 'SibSp', 'Parch', 'Fare', 'Cabin', 'Embarked')], INDICES = train[, c('Pclass')], summary)

by(data = train[, c('Survived', 'Age', 'SibSp', 'Parch', 'Fare', 'Cabin', 'Embarked')], INDICES = train[, c('Sex')], summary)

by(data = train[, c('Survived', 'Age', 'SibSp', 'Parch', 'Fare', 'Cabin', 'Embarked')], INDICES = train[, c('Pclass', 'Sex')], summary)

unique(train$Embarked)

library(gridExtra)
# Survived vs Pclass
plot1 = ggplot(data = train, aes(x = Pclass)) + geom_bar(aes(fill=factor(Survived)), stat = "count") #+ theme(aspect.ratio = 0.5)
# plot1
# Survived vs Sex
plot2 = ggplot(data = train, aes(x = Sex)) + geom_bar(aes(fill=factor(Survived))) #+ theme(aspect.ratio = 0.5)
# plot2
# class vs sex
plot3 = ggplot(data = train, aes(x = Pclass)) + geom_bar(aes(fill=Sex)) #+ theme(aspect.ratio = 0.5)
# plot3
# Survived vs Age
plot4 = ggplot(data = train, aes(x = Age)) + geom_density(aes(fill=factor(Survived)), alpha=0.5, na.rm = TRUE) #+ theme(aspect.ratio = 0.5)
# plot4
# Survived vs SibSP
plot5 = ggplot(data = train, aes(x = SibSp)) + geom_bar(aes(fill=factor(Survived)), alpha=0.5) #+ theme(aspect.ratio = 0.5)
# plot5
# Survived vs parch
plot6 = ggplot(data = train, aes(x = Parch)) + geom_bar(aes(fill=factor(Survived)), alpha=1) # theme(aspect.ratio = 0.5)
# plot6
# Fair vs Survived
plot7 = ggplot(data = train, aes(x = Fare)) + geom_density(aes(fill=factor(Survived)), alpha=0.5) #+ theme(aspect.ratio = 0.5)
# plot7
# # Embarked vs Survived
plot8 = ggplot(data = train, aes(x = Embarked)) + geom_bar(aes(fill=factor(Survived))) #+ theme(aspect.ratio = 0.5)
# plot8
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, nrow=4, ncol=2)

train = transform(train, Pclass = as.character(Pclass),
                  SibSp = as.character(SibSp),
                  Parch = as.character(Parch),
                  Sex_encoded = as.character(ifelse(Sex=='male', 1, 0)), Embarked_encoded = as.character(ifelse(Embarked=='C', 1, ifelse(Embarked=='Q', 2, ifelse(Embarked=='S', 3, '')))))
head(train)

my_logit = glm(factor(Survived) ~ (Pclass)*(Sex_encoded)*Age*(SibSp)*(Parch)*Fare*(Embarked_encoded),
               family = "binomial", data = train)
summary(my_logit)

my_logit2 = glm(factor(Survived) ~ (Pclass) + (Sex_encoded) + Age,
               family = "binomial", data = train)
summary(my_logit2)

by(data =train[, c('Survived', 'Age', 'SibSp', 'Parch', 'Fare', 'Cabin', 'Embarked')], INDICES = train[, c('NamePrefix')], summary)

test = transform(test, Pclass = as.character(Pclass),
                  SibSp = as.character(SibSp),
                  Parch = as.character(Parch),
                  Sex_encoded = as.character(ifelse(Sex=='male', 1, 0)), 
                  Embarked_encoded = as.character(ifelse(Embarked=='C', 1, ifelse(Embarked=='Q', 2, ifelse(Embarked=='S', 3, '')))))

# for Master
test$Age <- ifelse(is.na(test$Age) == TRUE & test$NamePrefix=='Master', 4.574, test$Age) 
test$Age <- ifelse(is.na(test$Age) == TRUE & test$NamePrefix=='Miss', 21.77, test$Age)
test$Age <- ifelse(is.na(test$Age) == TRUE & test$NamePrefix=='Mr', 32.37, test$Age)
test$Age <- ifelse(is.na(test$Age) == TRUE & test$NamePrefix=='Mrs', 35.73, test$Age)
test$Age <- ifelse(is.na(test$Age) == TRUE & test$Sex=='male', 30.73, test$Age)
test$Age <- ifelse(is.na(test$Age) == TRUE & test$Sex=='female', 27.92, test$Age)

# # for male
# test$Age <- ifelse(is.na(test$Age) == TRUE & test$Sex=='male' & test$Pclass=='1', 40, test$Age) # Pclass=1
# test$Age <- ifelse(is.na(test$Age) == TRUE & test$Sex=='male' & test$Pclass=='2', 30, test$Age) # Pclass=2
# test$Age <- ifelse(is.na(test$Age) == TRUE & test$Sex=='male' & test$Pclass=='3', 25, test$Age) # Pclass=3

# # for female
# test$Age <- ifelse(is.na(test$Age) == TRUE & test$Sex=='female' & test$Pclass=='1', 35, test$Age) #Pclass=1
# test$Age <- ifelse(is.na(test$Age) == TRUE & test$Sex=='female' & test$Pclass=='2', 28, test$Age) #Pclass=2
# test$Age <- ifelse(is.na(test$Age) == TRUE & test$Sex=='female' & test$Pclass=='3', 21.5, test$Age) #Pclass=3
# head(test, n = 10)
sapply(test, function(x){sum(is.na(x))})

# test[, 1:13][is.na(test[,1:13])] <- 0
pred = data.frame(pred = predict(my_logit2, newdata = test, type = "response"))
pred = transform(pred, Survived = ifelse(pred>0.5, 1, 0), PassengerId = seq(from = 892, length.out = 418))
gender_submission = pred[, c('PassengerId', 'Survived')]
# gender_submission[, 2][is.na(gender_submission[, 2])] <-0
head(gender_submission)

install.packages("xgboost")
require(xgboost)

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
