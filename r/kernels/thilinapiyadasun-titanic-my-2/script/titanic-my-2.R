
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(rpart)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")
head(train);
pclass<- train$Pclass;
sex <-train$Sex;


freqPclass=table(pclass)
freqSex=table(sex)

relaPclass=table(pclass)/nrow(train)
cbind(freqPclass,relaPclass);