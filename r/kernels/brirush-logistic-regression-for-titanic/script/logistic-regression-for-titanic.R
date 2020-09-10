
#This just implements the logistic regression with no prior data manipulation. 

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")


# We use the glm package with 'binomial' for logistic. We use some of the classes with few factors.
logistic <- glm(Survived ~ SibSp+Parch+Sex+Pclass, data = train,family='binomial')

#This summary lists the variables used, and assigns them z-scores and probabilities to describe what effect they have on the decision.
summary(logistic)

#Predict Output. Predict takes in the output of glm.
predicted<- predict(logistic,test,type="response")

#This gives us probabilities. We need to make a decision based on them, so we round (over .5 survives, under .5 does not).

predicted<-round(predicted)

output<-data.frame(test$PassengerId,predicted)

colnames(output)<-cbind("PassengerId","Survived")

write.csv(output, file = 'Rushton_Solution.csv', row.names = F)
