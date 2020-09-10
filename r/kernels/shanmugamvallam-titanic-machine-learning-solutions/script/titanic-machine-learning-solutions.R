
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(class)
#library(randomForest)
#library(party)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")
train <- train[-1]
test <- test[-1]


###################Binomial regression
  model <- glm(formula=Survived~Pclass+Sex+SibSp,data=train, family = binomial)
  test_pred <- predict(model,test, type="response")
   test_pred <- ifelse(test_pred>0.5,1,0)
   table(test_pred)


#   Died                Survived
#   0                   1 
#   269 (Total:266)     149 (Total:152) 
# Accuracy: 149/152 = ~98%

##############################

###################Random Forest

#  model <- randomForest(Survived~Pclass+Sex+SibSp,data=train,ntree=500, importance=TRUE)
#  test_pred <- predict(model,test, type="response")
#   test_pred <- ifelse(test_pred>0.5,1,0)
#   table(test_pred)


#   Died                Survived
#   0                   1 
#   269 (Total:266)     149 (Total:152) 
# Accuracy: 149/152 = ~98%

##############################

###################Conditional Tree

#  model <- ctree(Survived~Pclass+Sex+SibSp,data=train)
 #test_pred <- predict(model,test, type="response")
  # test_pred <- ifelse(test_pred>0.5,1,0)
  # table(test_pred)


#   Died                Survived
#   0                   1 
#   269 (Total:266)     149 (Total:152) 
# Accuracy: 149/152 = ~98%

##############################


png("1_survival_by_Sex.png", width=800, height=600)
mosaicplot(test$Sex ~ test_pred, main="Passenger Survival by Sex",
          color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)
dev.off()