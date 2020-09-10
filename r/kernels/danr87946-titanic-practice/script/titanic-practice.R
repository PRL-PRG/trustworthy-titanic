# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(rpart)
library('dplyr')
# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test  <- read.csv("../input/test.csv", stringsAsFactors = F)

full <- bind_rows(train, test) # Combine the sets

# Predict ages for missing values
#missing_age <- full[is.na(full$Age),]
#notmissing_age <- full[!(is.na(full$Age)),]
#age_fit <- lm(Age ~ Pclass + Embarked + Sex + SibSp + Parch + Fare, data = notmissing_age)
#age_pred <- round(predict(age_fit, missing_age))
#full[is.na(full$Age),]$Age <- age_pred

# Split the data 
#train <- full[1:891,]
#test <- full[892:1309,]

tree <- rpart(Survived ~ Pclass + Age + Sex + Embarked + SibSp + Parch + Fare, data = train, method = "class")

pred <- predict(tree, test, type="class")


PassengerId <- test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- pred

write.csv(output.df, file="out.csv", row.names = FALSE)