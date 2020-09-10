test = read.csv("../input/test.csv")
train = read.csv("../input/train.csv")

#fitting logistic regression
titanic.fit = glm(Survived~as.factor(Pclass)+Sex,data=train,family="binomial")
#predicting new data
titanic.prob = predict.glm(titanic.fit,newdata = test,type="response")
titanic.pred = rep(0,length(test))
titanic.pred[titanic.prob>=0.5] = 1 

output = data.frame(PassengerId=test$PassengerId,Survived=titanic.pred)
output
#write.table(output, file="submission.csv")
