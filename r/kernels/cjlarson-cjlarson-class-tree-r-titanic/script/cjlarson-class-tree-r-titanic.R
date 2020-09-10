library(tree)
library(mice)

#read in data
train = read.csv("../input/train.csv")
test = read.csv("../input/test.csv")
full = rbind(train[,-2], test)

#define factors for imputation
myfactors = c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare")

#compute missing values for imputation, and put those values into the original test and training frames
mice_mod = mice(full[myfactors],method="rf")
mice_output = complete(mice_mod)
full$Age <- mice_output$Age
full$Fare <- mice_output$Fare

survivors = train$Survived
train <- full[1:891,]
test <- full[892:1309,]

train$Survived = survivors

#Create a classification tree using variables that may be helpful to prediction
class_tree = tree(as.factor(Survived)~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, na.action="na.omit")
summary(class_tree)

#predict data using classification tree and write to output file
test$Survived_Pred = predict(class_tree, test, type="class")
solution <- data.frame(PassengerID = test$PassengerId, Survived = test$Survived_Pred)
write.csv(solution, file="class_tree_predictions.csv",row.names=F)