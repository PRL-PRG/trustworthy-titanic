library(ggplot2);
library(gridExtra);
library(caret);

set.seed(42);

setwd("../input");
titanic <- read.csv("train.csv", stringsAsFactors = FALSE, na.strings = "");

head(titanic);

test <- read.csv("test.csv", stringsAsFactors = FALSE, na.strings = "");
test$Survived <- NA;

head(test);

titanic <- rbind(titanic, test);

rm(test); # Unload the test set.

# Convert some of the features into known factors.
titanic$Survived <- factor(titanic$Survived);
titanic$Pclass   <- factor(titanic$Pclass);
titanic$Sex      <- factor(titanic$Sex);

# Fill in the blank fare records.
for (i in 1:3) { # Passenger class
   for (j in c("S", "Q", "C")) { # Embarked
      titanic$Fare[is.na(titanic$Fare) != FALSE & titanic$Pclass == i & titanic$Embarked == j] <- median(titanic$Fare[is.na(titanic$Fare) == FALSE & titanic$Pclass == i & titanic$Embarked == j]);
   }
}

g0 <- ggplot(titanic, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
      geom_boxplot() +
      scale_fill_discrete(name = "Class", breaks = 1:3, labels = c("Class 1", "Class 2", "Class 3"));

# Manually assign the most common embarkment ignoring the above plot (hinting at embarkment C) to the missing ports.
titanic$Embarked[is.na(titanic$Embarked)] <- "S";
titanic$Embarked <- factor(titanic$Embarked);

# titanic$Age[titanic$Age < 1] <- 0;
# Round down all of the less than 1 ages (and the estimate ages).
# All the less than 1 ages survived in the training dataset.
titanic$Age <- as.integer(titanic$Age);

#Engineer a new feature.
titanic$Deck <- factor(ifelse(is.na(titanic$Cabin), "U", substr(titanic$Cabin, 1, 1)));

# New feature regarding the number persons using the ticket.
titanic$TicketSize <- 1;

for (i in 1:nrow(titanic)) {
   titanic$TicketSize[i] <- length(titanic$Ticket[titanic$Ticket == titanic$Ticket[i]]);
}

# Individual fare feature.
titanic$IndividualFare <- titanic$Fare / titanic$TicketSize;

# Find the salutation of the person.
titanic$Name  <- gsub(" [A-Z]\\. ", " ", titanic$Name);
titanic$Title <- gsub("^.+, (.+)\\..*$", "\\1", titanic$Name);
titanic$Title[titanic$Title %in% c("Jonkheer", "Don", "Dona", "the Countess", "Sir", "Lady")] <- "Royalty";
titanic$Title[titanic$Title %in% c("Capt", "Col", "Major", "Rev", "Dr")] <- "Rank";
titanic$Title[titanic$Title == "Ms"]   <- "Mrs"; # There is only one "Ms".
titanic$Title[titanic$Title == "Mlle"] <- "Miss";
titanic$Title[titanic$Title == "Mme"]  <- "Mrs";

titanic$Title <- factor(titanic$Title);

# Fill in the blank age records.
for (i in 1:3) { # Passenger class
   for (j in levels(titanic$Title)) { # Titles
      titanic$Age[is.na(titanic$Age) != FALSE & titanic$Pclass == i & titanic$Title == j] <- median(titanic$Age[is.na(titanic$Age) == FALSE & titanic$Pclass == i & titanic$Title == j]);
   }
}

# Engineer more new featrures.
titanic$FamilySize <- titanic$Parch + titanic$SibSp + 1;

titanic$Mother <- 0;
titanic$Mother[titanic$Sex == "female" & titanic$Parch > 0 & titanic$Age > 18] <- 1;
titanic$Mother <- factor(titanic$Mother);

titanic$Wife <- 0;
titanic$Wife[titanic$Sex == "female" & titanic$SibSp > 0 & titanic$Age > 18 & titanic$Title != "Miss"] <- 1;
titanic$Wife <- factor(titanic$Wife);

titanic$FemaleAlone <- 0;
titanic$FemaleAlone[titanic$Sex == "female" & titanic$SibSp == 0 & titanic$Parch == 0] <- 1;
titanic$FemaleAlone <- factor(titanic$FemaleAlone);

titanic$ChildWithSiblings <- 0;
titanic$ChildWithSiblings[titanic$Age < 18 & titanic$SibSp > 0] <- 1;
titanic$ChildWithSiblings <- factor(titanic$ChildWithSiblings);

sapply(titanic, function(x) sum(is.na(x)));

titanic_train <- titanic[is.na(titanic$Survived) == FALSE, ];
titanic_test  <- titanic[is.na(titanic$Survived) != FALSE, ];

g1 <- ggplot(titanic_train, aes(x = Survived)) +
      geom_bar(stat = "count", fill = "#56B4E9", color = "#56B4E9") + 
      labs(title = "Overall survivual", x = "Survived", y = "Count");

g2 <- ggplot(titanic_train, aes(x = Sex)) + 
      facet_grid(. ~ Survived, labeller = as_labeller(setNames(c("Not survived", "Survived"), sort(levels(titanic$Survived))))) + 
      geom_bar(stat = "count", fill = "#56B4E9", color = "#56B4E9") +
      labs(title = "Gender survivual", x = "Gender", y = "Count");

g3 <- ggplot(titanic_train, aes(x = Age)) + 
      facet_grid(. ~ Survived, labeller = as_labeller(setNames(c("Not survived", "Survived"), sort(levels(titanic$Survived))))) +
      geom_bar(stat = "count", fill = "#56B4E9", color = "#56B4E9") + 
      labs(title = "Age survivual", x = "Age", y = "Count");

g4 <- ggplot(titanic_train, aes(x = Age)) + 
      facet_grid(. ~ Survived + Sex, labeller = as_labeller(setNames(c("Not survived", "Survived", "Female", "Male"), sort(c(levels(titanic$Survived), levels(titanic$Sex)))))) + 
      geom_bar(stat = "count", fill = "#56B4E9", color = "#56B4E9") + 
      labs(title = "Age and gender survivual", x = "Age", y = "Count");

grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2);

g5 <- ggplot(titanic_train, aes(x = Survived)) + 
      facet_grid(. ~ Pclass, labeller = label_context) + 
      geom_bar(stat = "count", fill = "#56B4E9", color = "#56B4E9") + 
      labs(title = "Class survivual", x = "Survived", y = "Count");

g6 <- ggplot(titanic_train, aes(x = Sex)) + 
      facet_grid(. ~ Pclass + Survived, labeller = label_context) + 
      geom_bar(stat = "count", fill = "#56B4E9", color = "#56B4E9") + 
      labs(title = "Class and gender survivual", x = "Gender", y = "Count");

grid.arrange(g5, g6, ncol = 1, nrow = 2);

# Split our training data into a sub training (80%) and test (20%) set.
training_split <- createDataPartition(y = titanic_train$Survived, p = 0.80, list = FALSE);
training_set <- titanic_train[training_split, ];
testing_set  <- titanic_train[-training_split, ];

model_fit <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + TicketSize + FemaleAlone + Wife + Mother, method = "rf", data = training_set, tuneGrid = expand.grid(.mtry = c(1:15)), trControl = trainControl(method = "repeatedcv", repeats = 3, number = 10, search = "grid"));

testing_set_predict <- predict(model_fit, newdata = testing_set);
testing_set$Correct <- (testing_set_predict == testing_set$Survived);
accuracy <- length(testing_set$Correct[testing_set$Correct == TRUE]) / length(testing_set$Correct);
paste("Training accuracy (rf):", accuracy);

model_fit <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + TicketSize + FemaleAlone + Wife + Mother, method = "cforest", data = training_set, tuneGrid = expand.grid(.mtry = c(1:15)), trControl = trainControl(method = "repeatedcv", repeats = 3, number = 10, search = "grid"));

testing_set_predict <- predict(model_fit, newdata = testing_set);
testing_set$Correct <- (testing_set_predict == testing_set$Survived);
accuracy <- length(testing_set$Correct[testing_set$Correct == TRUE]) / length(testing_set$Correct);
paste("Training accuracy (cf):", accuracy);

# Train using all of our training dataset.
model_fit <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + TicketSize + FemaleAlone + Wife + Mother, method = "rf", data = titanic_train, tuneGrid = expand.grid(.mtry = c(1:15)), trControl = trainControl(method = "repeatedcv", repeats = 3, number = 10, search = "grid"));

# Predict results.
titanic_test$Survived <- predict(model_fit, newdata = titanic_test);

# Output results.
write.csv(titanic_test[ , c(1, 2)], "results.csv", row.names = FALSE);
