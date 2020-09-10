system("ls ../input")

#load the datafiles

trainfile =read.csv('../input/train.csv', header=TRUE)
testfile = read.csv('../input/test.csv',header=TRUE)
#check the structure

str(trainfile)

set.seed(5221)
library(caret)

intrain <- createDataPartition(trainfile$Survived,p = 0.75,list = FALSE)
trained <-trainfile[intrain,]
localtest <- trainfile[-intrain,]
library(nnet)


trained$Survived <- class.ind(trained$Survived)
localtest$Survived <- class.ind(localtest$Survived)

neurafit <- nnet(Survived~Sex+Age+Pclass,trained,size = 1,softmax = TRUE)
summary(neurafit)

table(data.frame(predicted = predict(neurafit,localtest)[,2] > 0.5,actual = localtest[,2]>0.5))


predicted <- predict(neurafit,testfile)[,2]
predicted[is.na(predicted)] <- 0
head(predicted)
predicted[predicted > 0.5] <- 1
predicted[predicted < 0.5] <- 0
testfile$Survived <- predicted
write.csv(testfile[,1:12],"predicted_file",row.names=FALSE)

