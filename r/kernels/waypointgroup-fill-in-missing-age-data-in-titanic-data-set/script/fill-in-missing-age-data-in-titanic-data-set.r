
# Load the libraries
library(readr) # CSV file I/O, e.g. the read_csv function
library(tidyverse) # ggplot and others

# Read the files
train.data <- read_csv("../input/train.csv")
test.data <- read_csv("../input/test.csv")

# Combine the files
train.data$file <- "Train"
test.data$file <- "Test"
test.data$Survived <- "NA"
data <- rbind(train.data,test.data)
rm(test.data,train.data)

#
# Find columns with missing data
#
attach(data)
missing <- list(Pclass=nrow(data[is.na(Pclass), ]))
missing$Name <- nrow(data[is.na(Name), ])
missing$Sex <- nrow(data[is.na(Sex), ])
missing$Age <- nrow(data[is.na(Age), ])
missing$Fare <- nrow(data[is.na(Fare), ])
missing$Cabin <- nrow(data[is.na(Cabin), ])
missing$Embarked <- nrow(data[is.na(Embarked), ])
detach(data)

# Print out the columns with missing data

for (name in names(missing)){
  if (missing[[name]][1] > 0){
    print(paste('',name,' miss ',missing[[name]][1],' values',sep=''))
  }
}

rm(name)

# Seperate titles from name (Code from Huijun Zhao)
data$Title <- gsub('(.*, )|(\\..*)','',data$Name)

# Combine Rare Titles. 

rareTitle <- c('Dona','Lady','the Countess','Capt','Col','Don','Dr','Major','Rev','Sir','Jonkheer')
data$Title[data$Title %in% rareTitle] <- 'Rare Title'
rm(rareTitle)

#We count titles like Mlle, Ms all as Miss.
data$Title[data$Title=='Mlle'] <- 'Miss'
data$Title[data$Title=='Ms'] <- 'Miss'
data$Title[data$Title=='Mme'] <- 'Mrs'

table(as.character(data$Sex), data$Title)

data <- data %>%mutate(
  Title = factor(Title))


# Introduce a value for Family size (based on SibSp and Parch)
data$FamilySize <- data$SibSp + data$Parch +1

# Format relevent parameters as factors 
data <- data %>%mutate(
  Survived = factor(Survived),
  Pclass = factor(Pclass),
  FamilySize = factor(FamilySize),
  Embarked = factor(Embarked),
  Sex = factor(Sex),
  file = factor(file)  
)

# Function to find the rows with missing age value.  
# Note that these records don't have other missing data.

getMissingRows <- function(total_data,column.with.gaps){
  missingrows <- integer(0)
  count <- 0
  for (i in 1:nrow(total_data)){
    if(is.na(total_data[i,column.with.gaps])){
      count <- count+1
      missingrows[count] <- i
      
    }
  }
  print(paste(column.with.gaps,"has",count,"record(s) with a missing value"))
  return(missingrows)
}
MissingAge <- getMissingRows(data,"Age")
age.gaps <- data[MissingAge,]
summary(age.gaps)

# Find all the variables that might predict age
# Deliberately ignore survived as a feature
features <- c("Age",
              "PassengerId",
              "Pclass",
              "Sex",
              "SibSp",
              "Parch",
              "FamilySize",
              "Fare",
              "Embarked",
              "Title")
age.data <- subset(data,select=features)
# Store the missing age records in a seperate df
missing.age.data <- age.data[is.na(age.data$Age),]
#
# Establish a clean dataset for an age model
#
# Remove NA records
age.data <- age.data[!is.na(age.data$Age),]
age.data <- age.data[!is.na(age.data$Embarked),]
age.data <- age.data[!is.na(age.data$Fare),]

# split this model into 33% test data and 66%train
set.seed(2)
train=sample(c(TRUE,TRUE,FALSE),nrow(age.data),replace = TRUE)
test=(!train)
# Have a look at the three sets to make sure that they are broadly similar
summary(missing.age.data)
summary(age.data[train,])
summary(age.data[test,])
attach(age.data)

# Scatterplot 
pairs(Age~.,data=age.data)

# Age versus Title
age.data %>%
  ggplot(aes(x=Age, color = Title)) +
  ggtitle("Age vs. Title\n") +
  geom_density(size = 1.5)

# Age versus Pclass
age.data %>%
  ggplot(aes(x=Age, color = Pclass)) +
  ggtitle("Age vs. Pclass\n") +
  geom_density(size = 1.5)

# Age versus Fare
ggplot(age.data) +
  ggtitle("Age vs. Fare\n") +
  geom_point(mapping = aes(x=Age, y=Fare)) 

# Age versus Parch
ggplot(age.data) +
  ggtitle("Age vs. Parch\n") +
  geom_point(mapping = aes(x=Age, y=Parch)) 

# Age versus SibSp
ggplot(age.data) +
  ggtitle("Age vs. SibSp\n") +
  geom_point(mapping = aes(x=Age, y=SibSp)) 

# Age versus Family Size
age.data %>%
  ggplot(aes(x=Age, color = FamilySize)) +
  ggtitle("Age vs. Family Size\n") +
  geom_density(size = 1.5)

# Age versus Sex
age.data %>%
  ggplot(aes(x=Age, color = Sex)) +
  ggtitle("Age vs. Sex\n") +
  geom_density(size = 1.5)

# Age versus Embarked
age.data %>%
  ggplot(aes(x=Age, color = Embarked)) +
  ggtitle("Age vs. Embarked\n") +
  geom_density(size = 1.5)




# set dfs to hold the mse for each model alternative
model.MSE <- data.frame(matrix(vector(), 0, 2)) 
colnames(model.MSE) <- c("Model","MSE")
# set up functions to update the MSE df and missing.age.data df with the output of each model
add.MSE <- function(model.name,MSE.value,model.MSE){
 i <- nrow(model.MSE)+1
 model.MSE[i,"Model"] <- model.name
 model.MSE[i,"MSE"] <- MSE.value
 return(model.MSE)
}  
#
add.predictions <- function(model.name,predictions,age.gaps){
 predictions <- as.data.frame(predictions)
    colnames(predictions)[1] <- model.name
 age.gaps <- cbind(age.gaps,predictions)   
 return(age.gaps)
}  

model.name <- "Linear Model" 
# Model the training set 
model <- lm(Age~.,data = age.data[train,])
summary(model)
plot(model)
# Predict the ages of the test set 
predictions <- predict.lm(model,age.data[test,])
# Calculate the MSE 
MSE <- mean((age.data$Age[test]-predictions)^2)
# Predict the ages for the missing rows 
predictions <- predict.lm(model,missing.age.data)
# Update our summaries
model.MSE <- add.MSE(model.name,MSE,model.MSE)
age.gaps <- add.predictions(model.name,predictions,age.gaps)

### Tree Model

model.name <- "Tree" 
library(tree)
# Model the training set 
model <- tree(Age~.,age.data[train,])
summary(model)
plot(model)
text(model,pretty = 0)
# Predict the ages of the test set 
predictions <- predict(model,age.data[test,])
# Calculate the MSE 
MSE <- mean((age.data$Age[test]-predictions)^2)
# Predict the ages for the missing rows 
predictions <- predict(model,missing.age.data)
# Update our summaries
model.MSE <- add.MSE(model.name,MSE,model.MSE)
age.gaps <- add.predictions(model.name,predictions,age.gaps)

model.name <- "RPart" 
# From: https://www.kaggle.com/huijunzhao/titanic-r
#________________________________________________________________________
#  
# Model the training set 
library(rpart)
model <- rpart(Age ~., data=age.data[train,],method='anova')
summary(model)
plot(model)
text(model,pretty = 0)
# Predict the ages of the test set 
predictions <- predict(model,age.data[test,])
# Calculate the MSE 
MSE <- mean((age.data$Age[test]-predictions)^2)
# Predict the ages for the missing rows 
predictions <- predict(model,missing.age.data)
# Update our summaries
model.MSE <- add.MSE(model.name,MSE,model.MSE)
age.gaps <- add.predictions(model.name,predictions,age.gaps)


model.name <- "Random Forest" 
# Model the training set 
library(randomForest)
model <- randomForest(Age ~., data=age.data[train,],ntree=5000,mtry=3)
summary(model)
varImpPlot(model)
# Predict the ages of the test set 
predictions <- predict(model,age.data[test,])
# Calculate the MSE 
MSE <- mean((age.data$Age[test]-predictions)^2)
# Predict the ages for the missing rows 
predictions <- predict(model,missing.age.data)
# Update our summaries
model.MSE <- add.MSE(model.name,MSE,model.MSE)
age.gaps <- add.predictions(model.name,predictions,age.gaps)

model.name <- "Boosted Model" 
# Model the training set 
library(gbm)
model <- gbm(Age ~., data=age.data[train,],distribution="gaussian",n.trees=1000,interaction.depth=2)
summary(model)
# Predict the ages of the test set 
predictions <- predict(model,age.data[test,],n.trees=1000)
# Calculate the MSE 
MSE <- mean((age.data$Age[test]-predictions)^2)
# Predict the ages for the missing rows 
predictions <- predict(model,missing.age.data,n.trees=1000)
# Update our summaries
model.MSE <- add.MSE(model.name,MSE,model.MSE)
age.gaps <- add.predictions(model.name,predictions,age.gaps)



age.gaps

model.MSE
