
library(ggplot2)
library(caret)
library(dplyr)
library(randomForest)
library(rpart)
library(rpart.plot)

train <- read.csv("../input/train.csv", header = TRUE, stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", header = TRUE, stringsAsFactors = FALSE)
test$Survived <- NA
all <- rbind(train, test)
all$Survived <- as.factor(all$Survived)

#Checking for missing data, a missing data point can be an indication that
#someone died
all$MissingData <- apply(all, 1, function(x) any(is.na(x)))

all %>% group_by(MissingData) %>%
  summarise(n = n()) %>%
  mutate(share = n / sum(n))

#Name variable contains a last name and title
all$LastName <- gsub(", .*", "", all$Name)

#Taking out the title from the name variable
all$Title = gsub(".*, |\\. .*", "", all$Name)
table(all$Title)

#Some weird titles
men <- c("Capt", "Col", "Don", "Dr", "Jonkheer", "Major", "Rev", "Sir")
women <- c("Dona", "Lady", "Mme", "the Countess")
girls <- c("Mlle", "Ms")
all$Title[all$Title %in% men] <- "Mr"
all$Title[all$Title %in% women] <- "Mrs"
all$Title[all$Title %in% girls] <- "Miss"
all$Title <- as.factor(all$Title)

#Pclass is complete and can turned into factor, I don't think it makes sense to 
#view it as numeric
table(all$Pclass)
all$Pclass <- as.factor(all$Pclass)

#Making a ggplot comparing survival by ticket class
all %>% filter(!is.na(Survived)) %>% 
  group_by(Pclass, Survived = ifelse(Survived == 1, "Yes", "No")) %>%
  summarise(n = n()) %>%
  mutate(share = n / sum(n)) %>%
  ggplot(aes(Pclass, share, fill = Survived, label = paste(round(share * 100), "%", sep =""))) +
  geom_bar(position = "stack", stat = "identity", color = "black") +
  geom_text(position = position_stack(vjust = 0.5)) +
  labs(x = "Ticket class", y = "Survival", title = "Survival by ticket class") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

#Sex is complete and can turned into factor
table(all$Sex)
all$Sex <- as.factor(all$Sex)

#Making a ggplot comparing survival by gender and ticket class
all %>% filter(!is.na(Survived)) %>% 
  group_by(Sex, Pclass, Survived = ifelse(Survived == 1, "Yes", "No")) %>%
  summarise(n = n()) %>%
  mutate(share = n / sum(n)) %>%
  ggplot(aes(Sex, share, fill = Survived, label = paste(round(share * 100), "%", sep =""))) +
  geom_bar(position = "stack", stat = "identity", color = "black") +
  geom_text(position = position_stack(vjust = 0.5)) +
  labs(x = "Ticket class", y = "Survival", title = "Survival by ticket class and class") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(. ~ Pclass) +
  theme_bw()

#Have to deal with age as there are so many unknown
length(which(is.na(all$Age)))

#SibSp and Parch are both without unknown (although I suspect the data is not 100%
#correct anyway). Obviously kept as a numeric variable.
length(which(is.na(all$SibSp)))
length(which(is.na(all$Parch)))

#Adding family size
all$FamilySize <- all$SibSp + all$Parch + 1

#Ticket is complete but the char values make little sense to me, kept as char
length(which(is.na(all$Ticket)))

#One with unknown fare. Shouldn't waste too much time there, replace with median
#class fare of same family size (fare doesn't seem to be by person but by ticket)
length(which(is.na(all$Fare)))
noFareRow <- which(is.na(all$Fare))
all$Fare[noFareRow] <- median(all$Fare[all$Pclass == all$Pclass[noFareRow] & 
                                         all$FamilySize == all$FamilySize[noFareRow]], na.rm = TRUE)

all$FarePerson <- apply(all, 1, function(x) as.numeric(x["Fare"]) / nrow(all[all$Ticket == x["Ticket"], ]))

#Plenty of blanks in Cabin variable but no NA, mostly Pclass 1 have cabin numbers
length(which(is.na(all$Cabin)))
table(all$Cabin == "", all$Pclass)

#No missing embarked, turned to factor
length(which(is.na(all$Embarked)))
all$Embarked <- as.factor(all$Embarked)

#Trying to classify passengers into family/company groups
all$Family <- NA
workData <- all[, c("LastName", "Ticket", "FamilySize")]
repeat {
  row <- workData[1, ]
  ticket <- row$Ticket
  lastName <- row$LastName
  famSize <- row$FamilySize
  famData <- workData[workData$LastName == lastName & workData$FamilySize == famSize, ]
  if (!nrow(famData) == row$FamilySize) {
    if (nrow(famData[famData$Ticket == ticket, ]) == famSize) {
      famData <- famData[famData$Ticket == ticket, ]
    } else {
      famData <- workData[1, ]
    }
  }
  tickets <- unique(famData$Ticket)
  famData <- workData[workData$Ticket %in% tickets, ]
  famCount <- nrow(famData)
  lastNames <- paste(unique(famData$LastName), collapse = "/")
  indices <- row.names(famData)
  if (famCount > 1) all[row.names(all) %in% indices, "Family"] <- paste(lastNames, famCount, ticket)
  workData <- workData[!row.names(workData) %in% indices, ]
  if (nrow(workData) == 0) {
    break
  }
}

#Now that we have families, let's create two variables indicating whether a family member
#survived or died
all$FamilyLive <- NA
all$FamilyDied <- NA
for (i in 1:nrow(all)) {
  famData <- all[all$Family == all$Family[i] & all$PassengerId != all$PassengerId[i], ]
  all$FamilyLive[i] <- ifelse(length(which(famData$Survived == 1)) > 0, 1, 0)
  all$FamilyDied[i] <- ifelse(length(which(famData$Survived == 0)) > 0, 1, 0)
}
all$FamilyLive <- as.factor(all$FamilyLive)
all$FamilyDied <- as.factor(all$FamilyDied)

#Making a bar chart comparing survival between people with and without family survivors
all %>% filter(!is.na(Survived)) %>%
  group_by(FamilyLive = ifelse(FamilyLive == 1, "Yes", "No"), Survived) %>%
  summarise(n = n()) %>%
  mutate(share = n / sum(n)) %>%
  ggplot(aes(x = FamilyLive, y = share, fill = Survived, label = paste(round(share * 100), "%", sep = ""))) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent)

#Making a bar chart comparing survival between people with and without family causulties
all %>% filter(!is.na(Survived)) %>%
  group_by(FamilyDied = ifelse(FamilyDied == 1, "Yes", "No"), Survived) %>%
  summarise(n = n()) %>%
  mutate(share = n / sum(n)) %>%
  ggplot(aes(x = FamilyDied, y = share, fill = Survived, label = paste(round(share * 100), "%", sep = ""))) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

#Now dealing with the age problem, making a dummy matrix from possibly relevant 
imputeData <- all[, c("Pclass", "Sex", "Age", "Fare", "Embarked", "FamilySize", "Title")]
dummies <- predict(dummyVars(" ~ .", data = imputeData), imputeData)

#The predict function is a genric function producing something from a model.
#Both dummyVars and preprocess are treated like models in that way here.
preprocess <- preProcess(dummies, method = "bagImpute")
imputed <- as.data.frame(predict(preprocess, dummies))

#Transfering the age variable with the predicted values
all$Age = imputed$Age

#Making a graph comparing age and survival
all %>% filter(!is.na(Survived)) %>%
  mutate(bin = paste(floor(Age / 10) * 10, "-", ceiling((Age + 0.1) / 10) * 10)) %>%
  group_by(bin, Survived) %>%
  summarise(n = n()) %>%
  mutate(share = n / sum(n)) %>%
  ggplot(aes(x = bin, fill = Survived, y = n, label = paste(round(share * 100), "%", sep = ""))) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(position = position_stack(vjust = 0.5)) +
  labs(x = "Age group", y = "Count") +
  theme_bw()

#Some combination of age, family size and title seem a bit exotic to a modern
#person but trying to capture who is a child and not
all$Child <- ifelse(all$Age < 17 & all$Title != "Mrs", 1, 0)

#Making a chart comparing survival per family size, facet by child and gender
ggplot(all[!is.na(all$Survived), ], aes(x = FamilySize, y = as.numeric(ifelse(Survived == 0, 0, 1)))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_line(stat = "summary", fun.y = "mean", color = "blue") +
  labs(y = "Survival") +
  ylim(NA, 1) +
  facet_wrap(Sex ~ ifelse(Child == 1, "child", "adult")) +
  theme_bw()

features <- c("Sex", "Child", "FamilyDied", "FamilySize", "FarePerson", "Pclass")
trainData <- all[!is.na(all$Survived), features]
outcome <- all$Survived[!is.na(all$Survived)]

set.seed(111)
folds <- createMultiFolds(outcome, k = 3, times = 10)
control <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = folds)

rpartModel <- train(x = trainData, y = outcome, method = "rpart", tuneLength = 30, trControl = control)
prp(rpartModel$finalModel, type = 0, extra = 1, under = TRUE)

rfModel <- randomForest(x = trainData, y = outcome, importance = TRUE)

testData <- all[is.na(all$Survived), features]
predictions <- as.data.frame(predict(rfModel, testData))
submission <- cbind(all$PassengerId[is.na(all$Survived)], predictions)
names(submission) <- c("PassengerId", "Survived")
write.csv(submission, "titanic_20180921_1.csv", row.names = FALSE)
