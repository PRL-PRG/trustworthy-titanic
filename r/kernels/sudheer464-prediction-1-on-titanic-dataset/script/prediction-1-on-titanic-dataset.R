
# This R script will run on our backend. You can write arbitrary code here!


# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")




str(train)  # 891 obs of 12 variables

str(test)   # 418 obs of 11 variables



table(train$Survived) 
#  0   1    0--- not survived  and 1 --> survived  
# 549 342 

prop.table(table(train$Survived))

# 0         1                 38% of passengers survived the disaster in the training set.   
# 0.6161616 0.3838384 


# since there was no "Survived" column in the dataframe, we will create on for us and repeat our "0" prediction 418 times, the number of rows we have.

test$Survived<- rep(0,418)


submit<- data.frame(PassengerId=test$PassengerId, Survived= test$Survived)

write.csv(submit, "1stpred.csv", row.names = FALSE)
