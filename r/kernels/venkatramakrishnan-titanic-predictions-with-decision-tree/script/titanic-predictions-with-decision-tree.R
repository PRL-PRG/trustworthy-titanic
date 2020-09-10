
# This R script will run on our backend. You can write arbitrary code here!

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

library(rpart)
asc <- function(x) { strtoi(charToRaw(x),16L) }

rpart.training<-read.csv("../input/train.csv")
rpart.testing<-read.csv("../input/test.csv")

#Take care of empty values in Age, Fare and Cabin in the training set
rpart.training$Age[is.na(rpart.training$Age)]<-median(rpart.training$Age,na.rm=TRUE)
rpart.training$Fare[is.na(rpart.training$Fare)]<-median(rpart.training$Fare,na.rm=TRUE)
rpart.training$Cabin<-as.factor(ifelse(rpart.training$Cabin=='',"Z0",as.character(rpart.training$Cabin)))

#Normalize training data's Cabin and Fare into CabinLevel and FareLevel
for(i in 1:nrow(rpart.training)) {
    rpart.training$Cabinlevel[i]<-asc(substr(rpart.training$Cabin[i],1,1))
    
    if ((rpart.training$Fare[i] >= 0) & (rpart.training$Fare[i] < 100)) {
        	rpart.training$Farelevel[i] = 1
        } else if ((rpart.training$Fare[i] >= 100) & (rpart.training$Fare[i] < 200)) {
        	rpart.training$Farelevel[i] = 2
    } else if ((rpart.training$Fare[i] >= 200) & (rpart.training$Fare[i] < 300)) {
    	rpart.training$Farelevel[i] = 3
    } else if ((rpart.training$Fare[i] >= 300) & (rpart.training$Fare[i] < 400)) {
    	rpart.training$Farelevel[i] = 4
    } else if (rpart.training$Fare[i] >= 400) {
     	rpart.training$Farelevel[i] = 5
    } else {
    	rpart.training$Farelevel[i] = 6
    }
}

#Take care of empty values in Age, Fare and Cabin in the testing set
rpart.testing$Age[is.na(rpart.testing$Age)]<-median(rpart.testing$Age,na.rm=TRUE)
rpart.testing$Fare[is.na(rpart.testing$Fare)]<-median(rpart.testing$Fare,na.rm=TRUE)
rpart.testing$Cabin<-as.factor(ifelse(rpart.testing$Cabin=='',"Z0",as.character(rpart.testing$Cabin)))

#Normalize test data's Cabin and Fare into CabinLevel and FareLevel
for(i in 1:nrow(rpart.testing)) {
	rpart.testing$Cabinlevel[i]<-asc(substr(rpart.testing$Cabin[i],1,1))

	if ((rpart.testing$Fare[i] >= 0) & (rpart.testing$Fare[i] < 100)) {
		rpart.testing$Farelevel[i] = 1
	} else if ((rpart.testing$Fare[i] >= 100) & (rpart.testing$Fare[i] < 200)) {
		rpart.testing$Farelevel[i] = 2
	} else if ((rpart.testing$Fare[i] >= 200) & (rpart.testing$Fare[i] < 300)) {
		rpart.testing$Farelevel[i] = 3
	} else if ((rpart.testing$Fare[i] >= 300) & (rpart.testing$Fare[i] < 400)) {
		rpart.testing$Farelevel[i] = 4
	} else if (rpart.testing$Fare[i] >= 400) {
		rpart.testing$Farelevel[i] = 5
	} else {
		rpart.testing$Farelevel[i] = 6
	}
}

#Construct the model
rpart.model<-rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked+Cabinlevel+Farelevel,rpart.training)

print("Here is the constructed model: ")
summary(rpart.model)

#If the prediction value is above 0.49, the person is considered survived, otherwise they are not
rpart.predict<-ifelse(predict(rpart.model,rpart.testing)>0.49,1,0)

print("Here are the predictions for the test set: ")
rpart.predict

print("No. of people Survived: ")
print(sum(rpart.predict))
print("No. of people who didn't survive: ")
print(sum(rpart.predict==0))
