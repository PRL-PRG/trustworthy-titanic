

library(tidyverse) # metapackage with lots of helpful functions
library(class)
list.files(path = "../input")
# write.csv(evl,"test_scored.csv")

rm(list=ls())
t1 <- read.csv('../input/test.csv',  as.is = T)
t2 <- read.csv('../input/train.csv', as.is = T)
all <-  bind_rows(t2,t1)

# Predict missing ages.
colSums(is.na(all))
# female=1 male=0
all$Sex <- gsub('female','1',all$Sex)
all$Sex <- gsub('male','0',all$Sex)
# make all$Sex numeric
all$Sex <- as.numeric(all$Sex)
missing.a <- all[which(is.na(all$Age)),] # I want to predict these  missing values
train <- all[which(!is.na(all$Age)),] # For training the model I want to use data w/no missing values

# Train the model
model <- lm(Age ~ Pclass+Sex, data = train)
# predict
pred <- predict(model, missing.a)
pred <- as.data.frame(pred)
# Fill in the missing values
# There maybe an easier way to do this.
count <- 0
for (i in which(is.na(all$Age))) {
count <- count + 1
  all$Age[i] <- pred$pred[count]
}
colSums(is.na(all))

# Some of the fares are zero so I'll turn those cells to NA
all$Fare[all$Fare==0] <- NA
all$Embarked[which(all$Embarked=='')] <- 'S'
# Change to numeric type.
all$Fare <- as.numeric(all$Fare)
colSums(is.na(all))
# str(all)

all$Fare[which(is.na(all$Fare))] <- median(all$Fare, na.rm = T)
colSums(is.na(all))

data.frame(num=1:length(names(all)),names=names(all) ) 

# Remove some columns
df <- all[-c(1,4,9,11)]
head(df)

# Make df$Survived a factor.
df$Survived <- as.factor(df$Survived)
# Change Embarked to mumbers.
df$Embarked[which(df$Embarked=='C')] <- 0
df$Embarked[which(df$Embarked=='Q')] <- 1
df$Embarked[which(df$Embarked=='S')] <- 2
# make embarek numeric
df$Embarked <- as.numeric(df$Embarked)

###### Normalize data
norm <- function(x){return( (x-min(x))/(max(x)-min(x)) )}

train_target <- df[1:579,1]
test_target <- df[580:891,1]
df_z <- df # I'll use df_z later for z-score standardization
# # Leave the target comumn out.
df <- as.data.frame(lapply(df[,c(2:8)],norm))
x <- df # x will be used to predict survival.

df<- df[1:891,]
train <- df[1:579,]
test <- df[580:891,]
# I'll start with k to be a square root of train observations.
k <- as.integer(sqrt(579))

# Train model
model <- knn(train = train,
             test = test,    
             cl = train_target,
             k=k )

t <- table(test_target,model)
t
accuracy <- 100-((t[2]+t[3])/length(model))*100
cat('\n')
cat('accuracy = ',accuracy,'%')

df_z1 <- as.data.frame(scale(df_z[-c(1)]))

df_z1_train <- df_z1[1:579,]
df_z1_test <- df_z1[580:891,]
df_z1_train_target <- df_z[1:579,1]
df_z1_test_target <- df_z[580:891,1]

k <- 5
model <- knn(train = df_z1_train,
             test = df_z1_test,
             cl = df_z1_train_target,
             k=k )

t <- table(df_z1_test_target,model)
t
accuracy <- 100-((t[2]+t[3])/length(model))*100
cat('\n')
cat('accuracy = ',accuracy,'%')

k <- 5
model <- knn(train = df_z1_train,
             test = x[892:1309,], #df_z1_test, 
             cl = df_z1_train_target,
             k=k )
# write.csv(evl,"test_scored.csv")

submit <- data.frame(PassengerId=892:1309,Survived=model)
write.csv(submit,file = 'score.csv', row.names = F)
