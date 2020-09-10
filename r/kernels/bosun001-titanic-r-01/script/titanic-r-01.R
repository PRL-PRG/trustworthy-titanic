library(randomForest)
library(e1071)

#Print you can execute arbitrary python code
train = read.table(file = "../input/train.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
test  = read.table(file = "../input/test.csv", header = TRUE, sep = ",", stringsAsFactor = FALSE)
nrow(train)
#Print to standard output, 
print("Top of the training data:")
train[1:10, ]

#print("\n\nSummary statistics of training data")
summary(train)






#Any files you save will be available in the output tab below
#train.to_csv('copy_of_the_training_data.csv', index=False)
