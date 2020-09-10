## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
loaddata <- function(file) {
 data <- read.csv(file, header = TRUE, stringsAsFactors=F)
 # compute family size on dataset (including self)
 data$FamilySize <- data$SibSp + data$Parch + 1

 data
}

data <- loaddata("../input/train.csv")
# load real test data
titanic_test <- loaddata("../input/test.csv")

# change survived from integer to boolean

data$Survived <- as.logical(data$Survived)
levels(data$Survived) <- c("Not survived", "Survived")

# make explicit factor levels for specific variables: 3=Pclass, 5=Sex, 12=Embarked
for(i in c(3,5,12)) {
  data[,i] <- as.factor(data[,i])
}



## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
ggplot(data, aes(x=Age, y=Pclass, color=Survived)) + 
  geom_jitter(position = position_jitter(height = .1)) +
  scale_color_manual(values=c("red", "blue")) + facet_grid(Sex ~ .) +
  ggtitle("Age, Sex, and Class as Survival Factors") + ylab("Pclass")


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# e.g., if two people assigned to cabin A13 and familysize == 1 then bump up familysize to 2
# combine set of cabins from both test and training data
cabins <- data$Cabin # 1309 rows
n_occur <- data.frame(table(Var1=cabins))	# 187 rows
# remove missing cabin and/or juse the cabin letter code (e.g. D)
n_occur <- subset(n_occur, nchar(as.character(Var1)) > 1) # 183 rows

sharedCabins <- n_occur$Var1[n_occur$Freq > 1]
data$FamilySizeAdj <- data$FamilySize
print(table(data$FamilySize))

sharedInd <- data$FamilySizeAdj == 1 & data$Cabin %in% sharedCabins
data$FamilySizeAdj[sharedInd] <- 2
rowCount <- sum(sharedInd)
print(c("adjusted rows", rowCount)) # 27 rows
print(table(data$FamilySizeAdj))


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(caret)
set.seed(820)
inTrainingSet <- createDataPartition(data$Survived, p = 0.5, list=FALSE)
train <- data[inTrainingSet,]
test <- data[-inTrainingSet,]


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
modelaccuracy <- function(test, rpred) {
  result_1 <- test$Survived == rpred
  sum(result_1) / length(rpred)
}

checkaccuracy <- function(accuracy) {
	if (accuracy > bestaccuracy) {
	 bestaccuracy <- accuracy
	 assign("bestaccuracy", accuracy, envir = .GlobalEnv)
	 label <- 'better'
	} else if (accuracy < bestaccuracy) {
	 label <- 'worse'
	} else {
	 label <- 'no change'
	}
	label
}

library(rpart)
# starting with Age and Sex as indicators
fol <- formula(Survived ~ Age + Sex)						# 0.845
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
accuracy <- modelaccuracy(test, rpred)
bestaccuracy <- accuracy # init base accuracy
print(c("accuracy1", accuracy))								# baseline


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fol <- formula(Survived ~ Age + Sex + Pclass)				# 0.838
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
accuracy <- modelaccuracy(test, rpred)
accuracyLabel <- checkaccuracy(accuracy)
# almost as good but little worse
print(c("accuracy2", accuracy, accuracyLabel))				# worse


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fol <- formula(Survived ~ Age + Sex + Fare)					# 0.807
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
accuracy <- modelaccuracy(test, rpred)
accuracyLabel <- checkaccuracy(accuracy)
print(c("accuracy3", accuracy, accuracyLabel))				# worse


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fol <- formula(Survived ~ Age + Sex + Pclass + Fare)		# 0.820
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
#print(rmodel)
accuracy <- modelaccuracy(test, rpred)
accuracyLabel <- checkaccuracy(accuracy)
print(c("accuracy4", accuracy, accuracyLabel))				# worse


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fol <- formula(Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch) # 0.838
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
print(rmodel)
accuracy <- modelaccuracy(test, rpred)
accuracyLabel <- checkaccuracy(accuracy)
print(c("accuracy5", accuracy, accuracyLabel))				# worse


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# strip off cabin numbers
# Extract the deck number
# first letter : Deck (e.g. A31 -> A)

# make sure Deck in both sets has same levels
# if Test set has T but not Train set different levels causes error in model
train$Deck <- substr(train$Cabin,1,1)
train$Deck[train$Deck==''] = NA
test$Deck <- substr(test$Cabin,1,1)
test$Deck[test$Deck==''] = NA

train$Deck <- as.factor(train$Deck)
test$Deck <- as.factor(test$Deck)

# make Deck have same levels
c <- union(levels(train$Deck), levels(test$Deck))
levels(test$Deck) <- c
levels(train$Deck) <- c

# test if deck letter improves the prediction

fol <- formula(Survived ~ Age + Sex + Pclass + SibSp + Parch + Fare + Deck) # 0.807
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
#print(rmodel)
accuracy <- modelaccuracy(test, rpred)
accuracyLabel <- checkaccuracy(accuracy)
print(c("accuracy6", accuracy, accuracyLabel)) 							# 0.807 worse


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fol <- formula(Survived ~ Age + Sex + Pclass + FamilySize)				# 0.872
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
print(rmodel)
accuracy <- modelaccuracy(test, rpred)
accuracyLabel <- checkaccuracy(accuracy)
print(c("accuracy7", accuracy, accuracyLabel)) 						# best so far

p <- ggplot(aes(x=Pclass,y=factor(FamilySize),color=Survived),data=data) + 
 geom_jitter() + facet_grid(Sex ~ .)
p + ggtitle("Large Family Size >= 5 more likely to not survive") + theme_bw() + 
 geom_hline(yintercept=5) + ylab("Family Size")


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mosaicplot(table(FamilySize=data$FamilySize, Survived=data$Survived),
 main="Passenger Survival by Family Size",
 color=c("#fb8072", "#8dd3c7"), cex.axis=1.2)


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# make explicit factor levels for specific variables: Sex + Pclass
titanic_test$Sex <- as.factor(titanic_test$Sex)
titanic_test$Pclass <- as.factor(titanic_test$Pclass)
# now train on entire training set (714 rows)
fol <- formula(Survived ~ Age + Sex + Pclass + FamilySize)
model <- rpart(fol, method="class", data=data)
library(rpart.plot)	
rpart.plot(model,branch=0,branch.type=2,type=1,extra=102,shadow.col="pink",box.col="gray",split.col="magenta",
  main="Decision tree for model")


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fol <- formula(Survived ~ Sex + Age + FamilySize)				    # 0.854
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
accuracy <- modelaccuracy(test, rpred)
accuracyLabel <- checkaccuracy(accuracy)
print(c("accuracy8", accuracy, accuracyLabel)) 	                    # worse


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# does traveling alone contribute to outcome
fol <- formula(Survived ~ Age + Sex + Pclass + FamilySizeAdj)			# 0.872
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
# print(rmodel)
accuracy <- modelaccuracy(test, rpred)
accuracyLabel <- checkaccuracy(accuracy)
print(c("accuracy9", accuracy, accuracyLabel)) 							# no change


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fol <- formula(Survived ~ Age + Sex + Pclass + TravelAlone)			# 0.843
train$TravelAlone <- train$FamilySize == 1
test$TravelAlone <- test$FamilySize == 1
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
accuracy <- modelaccuracy(test, rpred)
accuracyLabel <- checkaccuracy(accuracy)
print(c("accuracy10", accuracy, accuracyLabel)) 			        # worse / no better


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fol <- formula(Survived ~ Age + Sex + Pclass + FamilySize + Embarked)	# 0.858 (worse)
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
accuracy <- modelaccuracy(test, rpred)
accuracyLabel <- checkaccuracy(accuracy)
print(c("accuracy11", accuracy, accuracyLabel)) 					    # little worse


## ----echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print (c("best accuracy", bestaccuracy))

