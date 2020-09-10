#load the training and the testing data
train <-  read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")

#examine the structure of the datas
str(train)
str(test)
#as both the data are same with a difference of the survived column in the test data thus lets make a new dataset woth both the  data set with both datas
full<-rbind(train[,-2],test)




