# i love pacman, but i dont think im even using it here
pacman::p_load(randomForest, ggplot2)

# read in the data
train <- read.csv("../input/train.csv", na.strings = c("", "NA", NA))
test <- read.csv("../input/test.csv")

# change all to lower because that's how i roll
colnames(train) <- tolower(colnames(train))

# check out structure
str(train)

# change passenger class to factor
train$pclass <- factor(train$pclass)

# # do some housekeeping - change variables to factor
# train$survived[train$survived == '1'] <- 'yes'
# train$survived[train$survived == '0'] <- 'no'

# survival
table(train$survived)

# embarked
table(train$embarked)
levels(train$embarked) <- gsub('C', 'Cherbourg', levels(train$embarked))
levels(train$embarked) <- gsub('Q', 'Queenstown', levels(train$embarked))
levels(train$embarked) <- gsub('S', 'Southampton', levels(train$embarked))

# really curious about fare

summary(train$fare)

ggplot(train, aes(x = train$fare)) +
  geom_histogram()

# # run a correlation between passenger class and fare
# cor.test(train$pclass, train$fare)

ggplot(train, aes(pclass, fare)) +
  geom_boxplot()

# i want to find the summary of fare prices as a function of passenger class - ddply the descriptives?
## would be a fun exercise


# run logistic regression
logistic <- glm(survived ~ sex*age*fare, family = binomial(link='logit'), data = train)


# something is happening such that it's only taking 714 passengers
predictions <- as.data.frame(predict(logistic, type = 'response'))

summary(logistic)

# let's just cut out people with no age just cause i wanna see what happens - OK 714
train.age <- train[!is.na(train$age), ]

# combine predictions with train.age
train.age$prediction <- predict(logistic, type = 'response')

cor.test(train.age$survived, train.age$prediction)

# change predictions from 0 - 50
train.age$pred.binary <- train.age$prediction

train.age$pred.binary[train.age$pred.binary < .5] <- 0
train.age$pred.binary[train.age$pred.binary > .5] <- 1

# run correlation and see how good it is
cor.test(train.age$survived, train.age$pred.binary)

# OK let's do the same on the test set
colnames(test) <- tolower(colnames(test))

test$prediction <- predict(logistic, newdata = test, type = 'response')

test$prediction[test$prediction < .5] <- 0
test$prediction[test$prediction > .5] <- 1

## but eventually we will have to say that the people without age are dead or something
test$prediction[is.na(test$prediction)] <- 0

# cut columns
colnames(test)
submit.1 <- test[, c(1, 12)]

colnames(submit.1) <- c("PassengerId", "Survived")
  
# write out to submit; need to delete first column manually
write.csv(submit.1, file = "submit.1.csv")