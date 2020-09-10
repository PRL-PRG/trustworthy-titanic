# The Titanic analysis is progressing in 3 steps:

 #1) Reading Data : This step shows " train & test " data , and types of variables in data. From there, I explore what should I do with specific variable in data.

 # 2) Data pre-processing

# 2a. Missing value : all features need to be imputed before feature engineering

# 2b. Feature engineering with Name and Ticket variables

# 2c. Checking the relationship of all features with "Survived"

# 2d. Checking continuous variables
 
# 3) Modeling

## 1 Reading Data

library(party)
library(arules)
library(arulesViz)
training <- read.csv('../input/train.csv', stringsAsFactors = F)
testing  <- read.csv('../input/test.csv', stringsAsFactors = F)
dim(training)
names(training)
dim(testing)
names(testing) # There is no "Survived" in testing data
testing$Survived <- NA
combi <- rbind(training, testing)
str(combi) # The structure is not good - there are not any factor variables.
combi[sapply(combi, is.character)] <- lapply(combi[sapply(combi, is.character)], as.factor) # convert character to factor
combi[sapply(combi, is.integer)] <- lapply(combi[sapply(combi, is.integer)], as.factor) # convert integer to factor
str(combi) # Now the type of all variable look better


## 2 Data Exploring
#a. Missing value

summary(combi)# There are 3 variables need to be imputed : Age, Embarked, and Fare. The feature "Cabin" is dropped, because the amount of missing value is bigger than 50% of the total.

## Imputing Age & Fare by using mean & median
Age <- mean(combi$Age[!is.na(combi$Age)])
combi$Age[is.na(combi$Age)] <- Age

which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

## "Embarked" is a factor variable, so it will be imputed by the most frequency factor
summary(combi$Embarked)
which(combi$Embarked == '') 
combi$Embarked[c(62,830)] = "S" # replace those two with "S"
summary(combi)


## 2b.Feature engineering with Name and Ticket variables.
#The "Name" variable has strings, which are not good for my model, as well as, the "Ticket" also has both of number and character.
   
Name <- as.character(combi$Name)
combi$Title<-sapply(Name,FUN=function(x) {strsplit(x,split='[,.]')[[1]][2]}) # just take a gender of "Name"

table(combi$Title)# there are a few titles, that could not make the model to work well, so let's combine a few of the most unusual ones.

combi$Title[combi$Title %in%c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir' 
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)# Finally, change the variable to factor
combi<-combi[,!(colnames(combi) %in% c("Name","Cabin"))]# drop "Name" & "Cabin"
## "Ticket"
summary(combi$Ticket) # This feature is so complicated for me, because the most frequency factor is "Other": 947, and the frequency others look not too much. So, before dropping this feature, I decide to use Market Basket Analysis to emphasis How important of "Ticket".

## 2c. Checking the relationship of all featuers with Survived

trans <- combi[1:891,-1]
trans$Age <-discretize(trans$Age, method = "frequency", 3) # Continuous variables need to be binned / discretized
trans$Fare <- discretize(trans$Fare, method = "frequency", 3)

str(trans)
transf<- as(trans,"transactions")
itemFrequencyPlot(transf, topN=25,  cex.names=.5)

rules <- apriori(transf,parameter = list(minlen=2, supp=0.005, conf=0.8),appearance = list(rhs=c("Survived=0", "Survived=1"),default="lhs"),control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")

# find redundant rules

subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned) 

# In top 20 rules of stage 2c, the "Ticket" can be dropped

## 2d.Checking continuous variables
library(e1071)
hist(combi$Age) # it seems like there is a small skew here
summary(combi$Age)
skewness(combi$Age)# the skew is acceptable

hist(combi$Fare) # very skew
summary(combi$Fare)
skewness(combi$Fare)# a clear positive skew
kurtosis(combi$Fare)

combi$Fare=log10(combi$Fare+1) # solving skewness
summary(combi$Fare)
hist(combi$Fare)
skewness(combi$Fare)
kurtosis(combi$Fare) 

## 3 Modeling
combi<-combi[,!(colnames(combi) %in% c("Ticket"))]
str(combi)
train <- combi[1:891,]
test <- combi[892:1309,]
set.seed(123)
fit <- cforest(Survived~ .- PassengerId,data = train,controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "mytitanic.csv", row.names = FALSE)
