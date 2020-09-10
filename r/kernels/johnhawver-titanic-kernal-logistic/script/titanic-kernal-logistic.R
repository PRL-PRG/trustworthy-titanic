# Setup
library(pscl)

##### Training Data #####

# Get and clean data
tData <- read.csv("~/Dropbox/KaggleData/train.csv", header = T, na.strings = "")

# Check features for missing data
colSums(is.na(tData))

# Remove the 2 rows where the passengers survived AND did not embark anywhere
cData <- tData[!is.na(tData$Embarked), ]
nrow(cData); nrow(tData)
tData <- cData; nrow(tData)

# Average ages 
avg_female_age <- mean(tData$Age[tData$Sex == 'female'], na.rm = T)
avg_male_age <- mean(tData$Age[tData$Sex == 'male'], na.rm = T)

# Correct age NA's by sex
tData$Age[(is.na(tData$Age) & tData$Sex == 'female')] <- avg_female_age
tData$Age[is.na(tData$Age) & tData$Sex == 'male'] <- avg_male_age

# Let's modify the cabin to true or false; reloading data so can handle the text better
cabinData <- read.csv("~/Dropbox/KaggleData/train.csv", header = T, stringsAsFactors = F)
cabinData$Cabin[nchar(cabinData$Cabin) > 0] <- 1
cabinData$Cabin[nchar(cabinData$Cabin) < 1] <- 0
tData$Cabin <- cabinData$Cabin


# Take a look at the data, make sure clean
head(tData)
summary(tData)
colSums(is.na(tData))
str(tData)

# Functions; from https://en.wikipedia.org/wiki/Matthews_correlation_coefficient
fMCC <- function(confusion_matrix) {
  TP <- confusion_matrix[2,2]; TN <- confusion_matrix[1,1]
  FP <- confusion_matrix[1,2]; FN <- confusion_matrix[2,1]
  N = TN + TP + FN + FP
  S = (TP + FN) / N
  P = (TP + FP) / N
  MCC = (TP / N - S * P) / sqrt(P*S*(1-S)*(1-P))
  return(MCC)
}
fmR2 <- function(confusion_matrix) { (fMCC(confusion_matrix))^2 } # R2 = cor^2
fAccuracy <- function(confusion_matrix) { sum(diag(confusion_matrix))/sum(confusion_matrix) }

# Create our features
fitData <- subset(tData, select = c('Survived', "Pclass", "Fare", "Sex", "Age", "SibSp", "Parch", "Embarked", "Cabin"))

# The Model
mylogit <- glm(Survived ~ . , data = fitData, family = 'binomial')
summary(mylogit)
anova(mylogit, test = "Chisq")

# fit accuracy comparison
fit <- predict(mylogit, type = 'response')
fit_Y <- ifelse(fit > 0.5, 1, 0)

# plot the fit
plot(fit); abline(h = .5); grid()
points(tData$Survived, col = 'blue')
points(fit_Y, col = 'green')

# assess results
confusion_matrix <- ftable(Y, fit_Y)
accuracy <- fAccuracy(confusion_matrix)
MCC <- fMCC(confusion_matrix)
mR2 <- fmR2(confusion_matrix)



# Let's split the data into a new training set and see how well it predicts the test set
splitRow <- round(nrow(tData)/2)
trainData <- tData[1:splitRow, ]; testData <- tData[(splitRow + 1):nrow(tData), ]
nrow(trainData)
nrow(testData)
Y <- trainData$Survived
X1 <- trainData$Pclass
X2 <- trainData$Fare
X3 <- trainData$Sex
X4 <- trainData$Age
X5 <- trainData$SibSp
X6 <- trainData$Parch
X7 <- trainData$Embarked
trainlogit <- glm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7, family = binomial(link='logit'))
summary(trainlogit)
fit <- predict(trainlogit, type = 'response')
fit_Y <- ifelse(fit > 0.5, 1, 0)
confusion_matrix <- ftable(Y, fit_Y)
accuracy <- fAccuracy(confusion_matrix)
MCC <- fMCC(confusion_matrix)
mR2 <- fmR2(confusion_matrix)
# So in and out of sample, roughly the same mac-R2 and accuracy, not bad
testFit <- predict(trainlogit, newdata = testData)
fit_Y <- ifelse(testFit > 0.5, 1, 0)
confusion_matrix <- ftable(Y, fit_Y)
accuracy <- fAccuracy(confusion_matrix)
MCC <- fMCC(confusion_matrix)
mR2 <- fmR2(confusion_matrix)



# what would be my results had I only used an average age, non-sex adjusted; very slight downward move in assesment
tData <- read.csv("~/Dropbox/KaggleData/train.csv", header = T, na.strings = "")
tData <- tData[!is.na(tData$Embarked), ]
tData$Age[is.na(tData$Age)] <- mean(tData$Age, na.rm = T)
Y <- tData$Survived
X1 <- tData$Pclass
X2 <- tData$Fare
X3 <- tData$Sex
X4 <- tData$Age
X5 <- tData$SibSp
X6 <- tData$Parch
X7 <- tData$Embarked
agelogit <- glm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7, family = binomial(link='logit'))
summary(agelogit)
fit <- predict(agelogit, type = 'response')
fit_Y <- ifelse(fit > 0.5, 1, 0)
confusion_matrix <- ftable(Y, fit_Y)
accuracy_age <- fAccuracy(confusion_matrix)
MCC_age <- fMCC(confusion_matrix)
mR2 <- fmR2(confusion_matrix)



##### Test Data #####

# Get and clean data
testData <- read.csv("~/Dropbox/KaggleData/test.csv", header = T, na.strings = "")
colSums(is.na(testData))

# Average ages 
avg_female_age <- mean(testData$Age[testData$Sex == 'female'], na.rm = T)
avg_male_age <- mean(testData$Age[testData$Sex == 'male'], na.rm = T)

# Correct age NA's by sex
testData$Age[(is.na(testData$Age) & testData$Sex == 'female')] <- avg_female_age
testData$Age[is.na(testData$Age) & testData$Sex == 'male'] <- avg_male_age

# fix the 1 fare data point
testData$Fare[is.na(testData$Fare)] <- mean(testData$Fare, na.rm = T)

# Let's modify the cabin to true or false
cabinData <- read.csv("~/Dropbox/KaggleData/test.csv", header = T, stringsAsFactors = F)
cabinData$Cabin[nchar(cabinData$Cabin) > 0] <- 1
cabinData$Cabin[nchar(cabinData$Cabin) < 1] <- 0
testData$Cabin <- cabinData$Cabin

# Take a look at the data, make sure clean
head(testData)
summary(testData)
colSums(is.na(testData))
str(testData)

# Apply the "mylogit" model to the test data
test_fitData <- subset(testData, select = c("Pclass", "Fare", "Sex", "Age", "SibSp", "Parch", "Embarked", "Cabin"))
test_fit <- predict(mylogit, newdata = test_fitData, type = 'response')
testSurvived <- ifelse(test_fit > 0.5, 1, 0)
submissionDF <- data.frame(PassengerID = testData$PassengerId, Survived = testSurvived)
write.csv(submissionDF, file ="~/Dropbox/KaggleData/titanic_submission.csv" , row.names = F)
