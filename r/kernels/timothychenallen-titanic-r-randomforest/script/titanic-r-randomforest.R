require(dplyr)
require(caret)
require(mice)
require(ggplot2)

# Read the data and put it in a single dataframe
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
full <- bind_rows(train,test)

# Clean up missing data
# What's missing?
sapply(names(full), function(x) sum(is.na(full[[x]]))) # Age, Fare
sapply(names(full), function(x) sum(full[[x]] == "")) # Cabin, Embarked

# Age
full[is.na(full$Age),]$Age <- -1

# Embarked
full %>% filter(Embarked == "") %>% select(Embarked, Fare)
ggplot(full %>% filter(Pclass==1, Embarked != "")) + 
  geom_boxplot(aes(Embarked, Fare, fill=Embarked)) + 
  geom_hline(yintercept=80, color="red")
# It looks most likely that an 80 fare indicates embarking in Cherbourg
full[full$Embarked=="",]$Embarked <- "C"

# Fare
full %>% filter(is.na(Fare)) %>% select(Embarked, Pclass, Sex)
full %>% filter(Embarked=="S", Pclass==3, Sex=="male") %>% select(Fare) %>% 
  summarize(usual_fare=median(Fare, na.rm = TRUE))
full[is.na(full$Fare),]$Fare <- 8.05

# Get titles and last names
full <- full %>% 
  mutate(Title=gsub(x=Name, pattern="^[^,]+, ([^ .]+)[\\. ].*$", replacement="\\1")) %>%
  mutate(Surname=gsub(x=full$Name, pattern="^([^,]+),.*$", replacement="\\1")) %>%
  mutate(Family_size=SibSp+Parch)

# Fix unusual titles
full[grep(x=full$Title, pattern="Mr|Miss|Mrs|Master", invert=TRUE),]$Title <- "Unusual"


full$Sex <- factor(full$Sex)
full$Embarked <- factor(full$Embarked)
full$Title <- factor(full$Title)

# Split it back up
train.clean <- full[1:891,]
test.clean <- full[892:1309,]

# Now we can do some random forest stuff
set.seed(42)
require(randomForest)
rf <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch +
                     Fare + Embarked + Title + 
                     Family_size, data=train.clean)

plot(rf)

# What's important?
imp <- importance(rf)
imp

test.clean$Survived <- predict(rf, test.clean)
# View(test.clean)
submission <- test.clean %>% select(PassengerId, Survived)
write.csv(submission, "submission.csv", row.names = FALSE)
