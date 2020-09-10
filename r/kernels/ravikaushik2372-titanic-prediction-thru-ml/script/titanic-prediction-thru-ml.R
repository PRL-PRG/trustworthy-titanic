# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
```{r, message=FALSE, warning=FALSE}
system("../input/")
```{r, message=FALSE, warning=FALSE}
# Any results you write to the current directory are saved as output.
##
##
# setting working directory


#importing files
train <- read.csv("C:/Users/ravik/Documents/R/kaggle-titanic-master/data/train.csv")
test <- read.csv("C:/Users/ravik/Documents/R/kaggle-titanic-master/data/test.csv")

## now we need to clean the data, but before that make a new combined dataset.

# creating equal variables in both datasets
names(train)
names(test)

test$Survived <- NA
# creating another var. for further data split 1= data from train, 0= from test.
train$input <- 1
test$input <- 0

finaldata <- rbind(train, test)  #new combined dataset
View(finaldata)

table(finaldata$input) ##just to cross check.

## data cleaning
str(finaldata)

finaldata$SibSp <- as.factor(finaldata$SibSp)
finaldata$Parch <- as.factor(finaldata$Parch)

mystats <- function(x){
nmiss<-sum(is.na(x))
return(c(nmiss=nmiss))}

names(finaldata)
str(finaldata)

vars <- c( "PassengerId","Pclass","Name","Sex","Age","SibSp",
           "Parch","Ticket","Fare","Cabin","Embarked")

dstats <- t(data.frame(apply(finaldata[vars], 2, mystats)))


##missing values immputation with median
table(is.na(finaldata$Fare))
misfare <- median(finaldata$Fare, na.rm = TRUE)
finaldata$Fare[is.na(finaldata$Fare)] <- misfare


table(finaldata$Embarked)
finaldata[finaldata$Embarked=="", "Embarked"] <- "S"


# finaldata$Age <- as.numeric(finaldata$Fare)

##predicting missing age because it has 263 missing obs....
#first test it on a dummy dataset <- rawdata

library(caret)
library(randomForest)

rawdata <- finaldata

ntmisage <- finaldata[!is.na(rawdata$Age),] #new data set where age is not missing.
misage <- finaldata[is.na(rawdata$Age),] ##new data set where age is missing.

table(is.na(ntmisage$Age))
table(is.na(misage$Age))

predicted.age <- randomForest(Age~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                       data = ntmisage, ntree =500, mtry = 3, nodesize = 0.01 * nrow(ntmisage))

misage$Age <- predict(predicted.age, newdata = misage)

#combining these two data sets

finaldata <- rbind(ntmisage, misage)  #new finaldata with zero NA's in age
table(is.na(finaldata$Age))
#-----------------------------------------------------------------------
### OR we can simply use mode of age .....
# mean(pred.age==abcd$Age)
# 
# misage <- median(finaldata$Age, na.rm = TRUE)
# 
# finaldata$Age[is.na(finaldata$Age)] <- misage
# table(is.na(finaldata$Age))
#-----------------------------------------------------------------------
###adding features in data

# Create the column child, and indicate whether child or adult
finaldata$Child[finaldata$Age < 18] <- 'Child'
finaldata$Child[finaldata$Age >= 18] <- 'Adult'

# Show counts
table(finaldata$Child, finaldata$Survived)

# Adding Mother variable
finaldata$Mother <- 'Not Mother'
finaldata$Mother[finaldata$Sex == 'female' & finaldata$Parch > 0 & finaldata$Age > 18 & finaldata$Title != 'Miss'] <- 'Mother'

table(finaldata$Mother, finaldata$Survived)
finaldata$Child <- as.factor(finaldata$Child)
finaldata$Mother <- as.factor(finaldata$Mother)

# abc <- data.frame(table(finaldata$Cabin))
# length(is.na(finaldata$Cabin))
# finaldata[finaldata$Cabin=="", "Cabin"] <- "C23 C25 C27"

## lets split the data again for modeling by 1= data from train, 0= from test.

trainingdata <- finaldata[finaldata$input== 1,]
testingdata <- finaldata[finaldata$input== 0,]

### modeling 1

################################### Random forest################################### 
#library(randomForest)

#Build Model
set.seed(754)

# Build the model (note: not all possible variables are used)
model12 <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Child + Mother,
                         data = trainingdata, ntree = 500)

# Show model error
plot(model12, ylim=c(0,0.36))
legend('topright', colnames(model12$err.rate), col=3:1, fill=1:3)

acc <- 1-model12$err.rate[nrow(model12$err.rate),1]
plot(acc)

model12$confusion

##     conf.ac <- (503+247)/(503+247+46+95) 

# The black line shows the overall error rate which falls below 20%. 
# The red and green lines show the error rate for 'died' and 'survived' respectively.

#Variable importance

importance    <- importance(model12)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

library(dplyr)
# Create a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = paste0(dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 5, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip()

##predict on test

pred.survived <- predict(model12, newdata = testingdata)

solution <- data.frame(PassengerID = testingdata$PassengerId, Survived = pred.survived)


##-------## accuracy test ##-------------##
library(caret)
mean(pred.survived == solution$Survived)

write.csv(solution,"Titanic_solution.csv",row.names = F)

library(e1071)

postResample(pred = pred.survived, obs = solution$Survived)
print(model12)
#-----------------------------------------------------
################################### Completed ################################### 
