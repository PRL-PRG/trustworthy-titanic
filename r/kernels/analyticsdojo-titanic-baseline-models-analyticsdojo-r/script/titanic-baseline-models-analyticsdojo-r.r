
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

head(train)

head(test)

test["Survived"] = 0

submission = test[,c("PassengerId", "Survived")]

head(submission)

# Write the solution to file
write.csv(submission, file = 'nosurvivors.csv', row.names = F)

#Here we can code it as Survived, but if we do so we will overwrite our other prediction. 
#Instead, let's code it as PredGender

test[test$Sex == "male", "PredGender"] = 0
test[test$Sex == "female", "PredGender"] = 1

submission = test[,c("PassengerId", "PredGender")]
#This will Rename the survived column
names(submission)[2] <- "Survived"
head(submission)

write.csv(submission, file = 'womensurvive.csv', row.names = F)
