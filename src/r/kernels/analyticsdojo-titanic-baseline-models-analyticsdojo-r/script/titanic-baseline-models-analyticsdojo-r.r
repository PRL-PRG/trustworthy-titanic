train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
head(train)
head(test)
test["Survived"] = 0
submission = test[, c("PassengerId", "Survived")]
head(submission)
write.csv(submission, file = "nosurvivors.csv", row.names = F)
test[test$Sex == "male", "PredGender"] = 0
test[test$Sex == "female", "PredGender"] = 1
submission = test[, c("PassengerId", "PredGender")]
names(submission)[2] <- "Survived"
head(submission)
write.csv(submission, file = "womensurvive.csv", row.names = F)
