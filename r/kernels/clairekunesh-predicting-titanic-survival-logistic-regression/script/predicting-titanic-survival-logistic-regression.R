# load packages
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(mice)
library(randomForest)
library(caret)
library(caTools)

# load data

train_titanic <- read_csv("../input/train.csv")
test_titanic <- read_csv("../input/test.csv")
test_titanic$Survived <- NA

full_titanic <- bind_rows(train_titanic, test_titanic)

# summarize things

summary(full_titanic)
str(full_titanic)

# clean up data; survived, sex, pclass, and embarked should be factors

full_titanic$Sex <- as.factor(full_titanic$Sex)
full_titanic$Embarked <- as.factor(full_titanic$Embarked)
full_titanic$Survived <- as.factor(full_titanic$Survived)
full_titanic$Pclass <- as.factor(full_titanic$Pclass) # this won't be a linear relationship

# get last names

full_titanic$Lastname <- sapply(full_titanic$Name,
                                function(x) strsplit(x, split = ',')[[1]][1])

# create family size (+1 to include individual him-/herself)

full_titanic$Famsize <- full_titanic$SibSp + full_titanic$Parch + 1
hist(full_titanic$Famsize)
  
# identify families based on a combination of name and size ... 
# not going to use it here though ...

full_titanic$Fam <- with(full_titanic, paste(Lastname, Famsize, sep = ""))
full_titanic$Fam <- as.factor(full_titanic$Fam)
summary(full_titanic$Fam)

# create a variable for title

full_titanic$Title <- gsub("^.*, (.*?)\\..*$", "\\1", full_titanic$Name)
table(full_titanic$Title)

# make flag for foreign name (maybe they had difficulty communicating): Jonkheer, Mlle, Mmme, etc.
# I cannot identify Monsieur vs Mister; there are also lots of non-Anglo-Saxon names with "normal"
# titles, but maybe it signifies something it they put a foreign title down
# also reassign some other titles to have more meaningful categories
# Jonkheer could go with foreign or noble

foreign <- c("Jonkheer", "Don", "Dona", "Mlle", "Mme")
full_titanic$Title[full_titanic$Title %in% foreign] <- "foreign"

military <- c("Capt", "Col", "Major")
full_titanic$Title[full_titanic$Title %in% military] <- "military"

noble <- c("Lady", "Sir", "the Countess")
full_titanic$Title[full_titanic$Title %in% noble] <- "noble"

# unclear whether Ms's were married or not; going with Miss 
full_titanic[which(full_titanic$Title == "Ms"),]
full_titanic$Title[full_titanic$Title == "Ms"] <- "Miss"
  
full_titanic$Title <- as.factor(full_titanic$Title)
table(full_titanic$Title)

# Fare is per ticket based on patterns, e.g., full_titanic[which(full_titanic$Fare == 512.3292),]

# calculate TicketN = how many people per ticket

full_titanic <- full_titanic %>%
  group_by(Ticket) %>%
  mutate(TicketN = n()) %>%
  ungroup()

# calculate Farepp = fare per person
full_titanic <- mutate(full_titanic, Farepp = Fare / TicketN)
class(full_titanic$Farepp)
summary(full_titanic$Farepp)

# impute missing data for fare using median class and port of embarkation
# use Farepp since a singleton

full_titanic[which(is.na(full_titanic$Fare)),]

median(full_titanic[full_titanic$Pclass == 3 & full_titanic$Embarked == "S",]$Farepp, na.rm = TRUE) # 7.7958
full_titanic$Fare[which(is.na(full_titanic$Fare))] <- median(full_titanic[full_titanic$Pclass == 3 & full_titanic$Embarked == "S",]$Farepp, na.rm = TRUE)
full_titanic$Farepp[full_titanic$PassengerId == 1044] <- 7.7958

# impute missing data for embarked; they paid 40 pp
# one has English-sounding name; other has French name; they have the same ticket no.
# I am going with Southampton in the end because of the older woman's name
# although the fare is much higher than the median of S, it's also higher than that of C

full_titanic[which(is.na(full_titanic$Embarked)),]

full_titanic %>%
  group_by(Embarked) %>%
  summarize(mean = mean(Fare, na.rm = TRUE), median = median(Fare, na.rm = TRUE))

full_titanic %>%
  group_by(Embarked) %>%
  summarize(mean = mean(Farepp, na.rm = TRUE), median = median(Farepp, na.rm = TRUE))

ggplot(full_titanic, aes(x = Farepp)) +
  geom_histogram() +
  facet_grid(~ Embarked)

full_titanic$Embarked[which(is.na(full_titanic$Embarked))] <- "S"
table(full_titanic$Embarked)

# impute missing data for age using mice package excluding variables likely not predictive of age

mice_age <- mice(full_titanic[, !names(full_titanic) %in% c("PassengerId", "Survived", "Name", "Ticket", "Cabin", "Lastname", "Fam")], method = "rf")
mice_age_output <- complete(mice_age)

ggplot(mice_age_output, aes(x = Age)) +
  geom_histogram()

full_titanic[6:15, "Age"]
mice_age_output[6:15, "Age"]

# thise only overwrites missing values as the others are the same
full_titanic$Age <- mice_age_output$Age

# create a variable for child; cut-off is debatable
# this is based on the analysis of split training data below

full_titanic$Child <- ifelse(full_titanic$Age <= 12, 1, 0)
full_titanic$Smallchild <- ifelse(full_titanic$Age <= 5, 1, 0)

# split into training and testing sets
# shuffle row indices, randomly order data, and split

set.seed(227537)

train_titanic <- filter(full_titanic, !is.na(Survived))

rows <- sample(nrow(train_titanic))
train_titanic <- train_titanic[rows, ]

# find row to split on (80% train, 20% validate)
split <- round(nrow(train_titanic) * .8)

train <- train_titanic[1:split, ]
valid <- train_titanic[(split + 1):nrow(train_titanic), ]

# see relationship between survival and age; were the youngest children
# more likely to survive?

ggplot(train, aes(x = Age, fill = as.factor(Survived))) +
  geom_density(alpha = 0.5, position = "identity")

# fit logistic regression
# including an interaction between pch and sibsp as the effect of these variables
# likely depends on the other

log_model <- glm(Survived ~ Pclass + Sex*Age + SibSp*Parch + Fare + Farepp + TicketN + Embarked + 
                   Title + Child + Smallchild, family = "binomial", data = train)
summary(log_model) # most variables not statistically sig but some cells are small and don't have power

# use cross-validation to see how the model does on new data
valid_logit_p <- predict(log_model, valid, type = "response")
summary(valid_logit_p)
hist(valid_logit_p)

logit_class_1 <- ifelse(valid_logit_p >= .5, 1, 0)
confusionMatrix(logit_class_1, valid[["Survived"]]) # above 80% accuracy, sensitivity, and specificity 

logit_class_2 <- ifelse(valid_logit_p >= .4, 1, 0)
confusionMatrix(logit_class_2, valid[["Survived"]]) # accuracy drops when we use a lower cutoff 

logit_class_3 <- ifelse(valid_logit_p >= .6, 1, 0)
confusionMatrix(logit_class_3, valid[["Survived"]]) # improves to 0.8933 w my data but not here; does set seed not work wi Kaggle?

# plot ROC curve to see accuracy of model across different cutoff levels
colAUC(valid_logit_p, valid[["Survived"]], plotROC = TRUE)

# we could further train and validate the model, but I will stop here for now

# predict using the test set
test_titanic <- filter(full_titanic, is.na(Survived))
log_prediction <- predict(log_model, test_titanic, type = "response")
hist(log_prediction)
solution <- data.frame(PassengerID = test_titanic$PassengerId, Survived = ifelse(log_prediction >= 0.6, 1, 0))

# write logistic regression solution to a csv
write.csv(solution, file = "log_model_solution_1.csv", row.names = FALSE)
