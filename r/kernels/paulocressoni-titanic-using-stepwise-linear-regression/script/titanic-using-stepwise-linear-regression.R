# Stepwise Regression
library('MASS')
library('ggplot2')
library('ggthemes') # visualization

# input dataset
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
#str(train)
#str(test)

### data cleaning and preparation ###
# converting the types
train$Pclass <- as.factor(train$Pclass)
train$Name <- as.character(train$Name)
train$SibSp <- as.factor(train$SibSp)
train$Parch <- as.factor(train$Parch)
train$Cabin <- as.character(train$Cabin)

test$Pclass <- as.factor(test$Pclass)
test$Name <- as.character(test$Name)
test$SibSp <- as.factor(test$SibSp)
test$Parch <- as.factor(test$Parch)
test$Cabin <- as.character(test$Cabin)

# adding family name
train$FamilyName <- sub("^(.*),.*", "\\1", train$Name)
test$FamilyName <- sub("^(.*),.*", "\\1", test$Name)

# adding title
# problem when the name has multiple period (dots, points)
#train$Title <- as.factor(sub("^(.*),\\s(.*)\\.\\s.*","\\2", train$Name))
#test$Title <- as.factor(sub("^(.*),\\s(.*)\\.\\s.*","\\2", test$Name))
train$Title <- as.character(gsub('(.*, )|(\\..*)', '', train$Name))
test$Title <- as.character(gsub('(.*, )|(\\..*)', '', test$Name))

# fixing titles
rare_titles <- c('Capt', 'Col', 'Don', 'Dona', 'Dr', 'Jonkheer', 'Lady', 'Major', 'Rev', 'the Countess', 'Sir')

train$Title[train$Title == 'Mlle'] <- 'Miss'
train$Title[train$Title == 'Ms'] <- 'Miss'
train$Title[train$Title == 'Mme'] <- 'Miss'
train$Title[train$Title %in% rare_titles] <- 'Rare Title'
train$Title <- as.factor(train$Title)

test$Title[test$Title == 'Mlle'] <- 'Miss'
test$Title[test$Title == 'Ms'] <- 'Miss'
test$Title[test$Title == 'Mme'] <- 'Miss'
test$Title[test$Title %in% rare_titles] <- 'Rare Title'
test$Title <- as.factor(test$Title)

# plot the Survivors per Title
#ggplot(train, aes(x = Title, fill = factor(Survived))) + 
#  geom_bar(stat = 'count', position = 'dodge') + 
#  labs(x = 'Title') + 
#  theme_few()

# function to infer the missing Ages according to the mean age from each Title
fill_age_by_title <- function(title, dataset) {
  mean(dataset$Age[dataset$Title == title & !is.na(dataset$Age)])
}

train$Age[is.na(train$Age) & train$Title == 'Master'] <- round(fill_age_by_title('Master', train))
train$Age[is.na(train$Age) & train$Title == 'Miss'] <- round(fill_age_by_title('Miss', train))
train$Age[is.na(train$Age) & train$Title == 'Mr'] <- round(fill_age_by_title('Mr', train))
train$Age[is.na(train$Age) & train$Title == 'Mrs'] <- round(fill_age_by_title('Mrs', train))
train$Age[is.na(train$Age) & train$Title == 'Rare Title'] <- round(fill_age_by_title('Rare Title', train))

test$Age[is.na(test$Age) & test$Title == 'Master'] <- round(fill_age_by_title('Master', test))
test$Age[is.na(test$Age) & test$Title == 'Miss'] <- round(fill_age_by_title('Miss', test))
test$Age[is.na(test$Age) & test$Title == 'Mr'] <- round(fill_age_by_title('Mr', test))
test$Age[is.na(test$Age) & test$Title == 'Mrs'] <- round(fill_age_by_title('Mrs', test))
test$Age[is.na(test$Age) & test$Title == 'Rare Title'] <- round(fill_age_by_title('Rare Title', test))

# adding the family size
train$FamilySize <- as.factor(as.numeric(as.character(train$Parch)) + as.numeric(as.character(train$SibSp)))
test$FamilySize <- as.factor(as.numeric(as.character(test$Parch)) + as.numeric(as.character(test$SibSp)))

# treating Age as factor
train$Age <- as.factor(train$Age)
test$Age <- as.factor(test$Age)

# assume zero for missing Fares
train$Fare[is.na(train$Fare)] <- 0
test$Fare[is.na(test$Fare)] <- 0

# using Stepwise to identify the variables to be used
fit <- lm(Survived ~ Sex + Pclass + Age + FamilySize + Fare + Embarked + Title, data = train)
step <- stepAIC(fit, direction="both")
step$anova # display results 

# linear regression
model <- lm(Survived ~ Sex + Pclass + FamilySize + Fare + Title, data = train)
summary(model)

test$Survived <- round(predict(model, test))
test$Survived[test$Survived < 0] <- 0
test$Survived[test$Survived > 1] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gender_submission.csv", row.names = F)