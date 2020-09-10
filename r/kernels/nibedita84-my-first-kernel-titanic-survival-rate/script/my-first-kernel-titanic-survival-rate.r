
#Loading Packages

library('ggplot2')  # for data visualization
library('rpart')    # algorithm for decision trees
library('rattle')   # data mining
library('dplyr')  #data manipulation
library('rpart.plot')  # for visualization
library('RColorBrewer')  # for visualization
library('randomForest')  # algorithm for random forest
library('gbm') # algorithm for boosting



traindata <- read.csv('../input/train.csv')
testdata <- read.csv('../input/test.csv')

totaldata <- bind_rows(traindata, testdata)

str(totaldata)


totaldata$Family_size <- totaldata$SibSp + totaldata$Parch + 1

totaldata



# checking the structure of totaldata now

str(totaldata)

# fill the missing values in embarkment for 62 & 830

embark1 <- subset(totaldata, !( PassengerId==62 | PassengerId==830)) 

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark1, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot()


totaldata$Embarked[c(62, 830)] <- "C"

#factorizing the embarked column because categorical data

totaldata$Embarked <- factor(totaldata$Embarked)


age_predicted <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Family_size,
                       data = totaldata[!is.na(totaldata$Age),], method = "anova")
totaldata$Age[is.na(totaldata$Age)] <- predict(age_predicted, totaldata[is.na(totaldata$Age),])



totaldata$Fare[1044] <- median(totaldata[totaldata$Pclass == "3" & totaldata$Embarked == "S", ]$Fare, na.rm = TRUE)

totaldata$Fare[1044]


totaldata$Child <- NA
totaldata$Child[totaldata$Age < 18] <- 1
totaldata$Child[totaldata$Age >= 18] <- 0

str(totaldata)

# Let's see the proportions in  table

prop.table(table(totaldata$Child, totaldata$Survived),1)


# not much of a difference being a child. now let's see if family size can have better predictivity

prop.table(table(totaldata$Family_size, totaldata$Survived), 1)



# final check to see if our data is fine for prediction..str(totaldata)
str(totaldata)

#Splitting the data back to Train & Test datasets
train <- totaldata[1:891,]
test <- totaldata[892:1309,]

# Now the prediction!!

model1 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_size + Child,
                      data = train, method = "class")

#visualize the tree 

fancyRpartPlot(model1)

prediction1 <- predict(model1, train, type = "class")

summary(prediction1)

#I can see that decision trees is showing death to survival as 624:267 as opposed to 549:342.  Now, I will use the random forest to see if my result changes and by how much...

set.seed(111)

model2 <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Child + Family_size,
                          data = train, importance = TRUE, ntree = 1000)

prediction2 <- predict(model2, train, type = "class")

summary(prediction2)


# Random forest seems to get better prediction results. So, i will go with the randomforest model for my prediction. 

prediction <- predict(model2, test, type = "class")

solution <- data.frame(PassengerId = test$PassengerId, Survived = prediction)

write.csv(solution, file = "rf_solution.csv", row.names = FALSE)

# I am also going to do try out Gadient Boosting to see if the prediction can improve further.. 

totaldata = select(totaldata, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked, Child, Family_size)
end_trn = nrow(train)
end = nrow(totaldata)
model3 = gbm.fit(x = totaldata[1:end_trn, ], y = train$Survived, 
             shrinkage=0.01, distribution = 'bernoulli',
             interaction.depth=3, nTrain = round(end_trn * 0.8), 
             n.minobsinnode=10, n.trees=3000, verbose=T)

summary(model3)
gbm.perf(model3)



# now checking prediction on train data

predictiontest <- predict(object = model3, newdata = totaldata[1:end_trn,], n.trees = gbm.perf(model3, plot.it = F), type = "response")
predictiontest <- round(predictiontest)
head(predictiontest, n=20)
head(train$Survived, n=20)



prediction3 <- predict(object = model3, newdata = totaldata[(end_trn+1):end, ], n.trees = gbm.perf(model3, plot.it = F), type = "response")
prediction3 <- round(prediction3)
solution2 <- data.frame(PassengerId = test$PassengerId, Survived = prediction3)

write.csv(solution2, file = "gbm_solution.csv", row.names = FALSE)
