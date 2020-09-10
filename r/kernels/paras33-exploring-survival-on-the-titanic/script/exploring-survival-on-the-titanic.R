test<- read.csv("../input/test.csv",header=TRUE);
str(test);
train <- read.csv("../input/train.csv",header=TRUE);
str(train)
#fix("train")

## Adding survived column in  the test data set to allow combination of test and train 
#test data set.
test.survived <- data.frame(survived=rep("None",nrow(test)),test[,])

test.surviver<-test.survived[,c(2,1,3,4,5,6,7,8,9,10,11,12)]
#str(test.surviver)

identical(train,test.surviver)
#data.combined <- rbind(train,test.survived);

