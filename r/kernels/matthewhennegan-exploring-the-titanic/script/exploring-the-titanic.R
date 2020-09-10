## ----Library Imports, message=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# For data manipulation
library('dplyr')          # data manipulation
library('zoo')            # Use of na.aggregate

# For data visualization
library('ggplot2')        # visualization
library('scales')         # visualization

# For machine learning
library('randomForest')   # For Random Forest
library('caret')          # For CTree
library('party')          # For CTree
library('class')          # For KNN
library('e1071')          # For SVM



## ----Import Data-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Individual data tables of training and testing data
titanic_train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
titanic_test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)

# Have a look
glimpse(titanic_train)



## ----Check Names Format----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Check the format of names
head(titanic_train$Name, 20)



## ----Create Titles Column--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Extract just the titles
titles <- gsub(".*, (.*?)\\..*", "\\1", titanic_train$Name)

# Add this column to dataset
titanic_train$Title <- titles

# See all unique titles
unique(titanic_train$Title)



## ----Frequency of All Titles-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Use the same colour for consistency
myBarCol <- "#f46b42"

# Create dataset to plot
freq_data <- titanic_train %>%
  group_by(Title) %>%
  summarize(Freq = n())

# Display plot
ggplot(freq_data, aes(x = Title, y = Freq)) +
  geom_bar(fill = myBarCol, stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) +
  labs(title = "Frequency of Titles", y = "Number of Passengers") +
  scale_y_continuous(expand=c(0, 0)) +
  expand_limits(y = c(0, 560)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## ----Frequency Of Titles---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Set a list of the common titles
common_titles <- c("Mr", "Mrs", "Miss", "Master")

# Replace all the rare titles with 'special'
titanic_train <- titanic_train %>%
  mutate(Title = replace(Title, !(Title %in% common_titles), "Special"))

# Reset data
freq_data <- titanic_train %>%
  group_by(Title) %>%
  summarize(Freq = n())

# Replot the previous graph to see what the distribution looks like now
ggplot(freq_data, aes(x = Title, y = Freq)) +
  geom_bar(fill = myBarCol, stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) +
  labs(title = "Frequency of Titles", y = "Number of Passengers") +
  scale_y_continuous(expand=c(0, 0)) +
  expand_limits(y = c(0, 560))



## ----Survival Rates of Titles----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Manipulate our data so we can graph the survival of titles
title_survival <- titanic_train %>%
  group_by(Title) %>%
  summarize(Percent_Survived = mean(Survived))

# Pre-set graph layers to be re-used
geom_bar_titanic <- geom_bar(stat = "identity", fill = myBarCol)
scale_y_continuous_titanic <- scale_y_continuous(labels = percent,
                                                 limits = c(0, 1),
                                                 expand = c(0, 0))

# Graph passenger title survival rates
ggplot(title_survival, aes(x = Title, y = Percent_Survived)) +
  geom_bar_titanic +
  scale_y_continuous_titanic +
  geom_text(aes(label = paste0(round(Percent_Survived * 100, 2), "%")), vjust = -0.5) +
  labs(title = "Survival Rates of Titles", y = "Survival Percentage")



## ----Survival Rates of Special Titles by Sex-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Have a look at the survival rate of men vs women with special titles
special_bysex <- titanic_train %>%
  filter(Title == "Special") %>%
  group_by(Sex) %>%
  summarise(Percent_Survived = mean(Survived))

# Graph this result
ggplot(special_bysex, aes(x = Sex, y = Percent_Survived)) +
  geom_bar_titanic +
  scale_y_continuous_titanic +
  geom_text(aes(label = paste0(round(Percent_Survived * 100, 2),"%")), vjust = 2) +
  labs(title = "Survival Rates of Special Titles by Sex", y = "Survival Rate")



## ----Survival Percentage By Sex--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Have a look at the survival rates by sex
by_sex <- titanic_train %>%
  group_by(Sex) %>%
  summarise(Percent_Survived = mean(Survived))

# Graph results
ggplot(by_sex, aes(x = Sex, y = Percent_Survived)) +
  geom_bar_titanic +
  geom_text(aes(label = paste0(round(Percent_Survived * 100, 2),"%")), vjust = -1) +
  scale_y_continuous_titanic +
  labs(title = "Survivale Rates by Gender", y = "Survival Rate")



## ----Survival Rates of Men Vs Women in Different Classes-------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Sort out the data we'll need for the plot
by_Pclass_Sex <- titanic_train %>%
  group_by(Pclass, Sex) %>%
  summarize(Percent_Survived = mean(Survived)) %>%
  arrange(Pclass, Sex)

# Plot the graph
ggplot(by_Pclass_Sex, aes(x = factor(Pclass), y = Percent_Survived, fill = Sex)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Survival Rates of Men vs Women in Different Classes",
       y = "Percent Survived",
       x = "Social Status") +
  scale_x_discrete(labels=c("1" = "Upper Class",
                            "2" = "Middle Class",
                            "3" = "Lower Class")) +
  coord_flip()



## ----Survival Percentages of Men and Women Across Classes------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Plot the graph
ggplot(by_Pclass_Sex, aes(x = factor(Pclass), y = Percent_Survived, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Survival Rates of Men vs Women in Different Classes",
       y = "Percent Survived",
       x = "Social Status") +
  scale_x_discrete(labels=c("1" = "Upper Class",
                            "2" = "Middle Class",
                            "3" = "Lower Class")) +
  scale_y_continuous_titanic +
  geom_text(aes(label = paste0(round(Percent_Survived * 100, 2), "%")),
            vjust = 2,
            position = position_dodge(width = 0.9))



## ----Distribution of Survival----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Adjust jitter amount
titanic_jitter <- position_jitter(width = 0.4, height = 0.3)

# Plot survival
ggplot(titanic_train, aes(x = factor(Pclass), y = factor(Survived), colour = Sex)) +
  geom_point(alpha = 0.6, position = titanic_jitter) +
  scale_x_discrete(labels=c("1" = "Upper Class",
                            "2" = "Middle Class",
                            "3" = "Lower Class")) +
  scale_y_discrete(labels=c("0" = "Died", "1" = "Survived")) +
  labs(title = "Distribution of Survival",
       x = "Social Status",
       y = "Survival")



## ----Explore NA Values-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Are any values missing in the columns?
summary(titanic_train)



## ----Aggregate NAS---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Change all NA's to the median of the ages of their Title
clean_titanic_train <- titanic_train %>%
      mutate(Age = round(na.aggregate(Age, by = Title, FUN = median)))

# Now explore frequency of ages
ggplot(clean_titanic_train, aes(Age)) +
  geom_histogram(breaks = seq(0, 80, 5), fill = myBarCol) +
  labs(title = "Distribution of Ages", x = "Age", y = "Frequency") +
  scale_x_continuous(breaks = seq(0, 80, 10))




## ----Categorize Age--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Break the ages up into manually binned categories
age_breaks <- c(0, 8, 13, 17, 24, 35, 50, 120)
age_labels <- c("Infant", "Child", "Teen", "Young Adult",
                "Adult", "Middle Aged", "Elderly")

clean_titanic_train$age_bins <- cut(clean_titanic_train$Age,
                                    breaks = age_breaks,
                                    labels = age_labels,
                                    ordered_result = TRUE,
                                    include.lowest = TRUE)

# Get summary data of the groups
age_data <- clean_titanic_train %>%
  group_by(age_bins, Sex) %>%
  summarize(Percent_Survived = mean(Survived))

# Plot the data
ggplot(age_data, aes(x = age_bins, y = Percent_Survived, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous_titanic +
  labs(title = "Survival Rates of Age Groups",
       x = "Age Groups",
       y = "Survival Rate") +
  geom_text(aes(label = paste0(round(Percent_Survived * 100, 1), "%")),
            vjust = -1, position = position_dodge(width = 1), size = 3)



## ----Embark Locations------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

port_data <- titanic_train %>%
  filter(Embarked != "")

port_data$Pclass <- factor(port_data$Pclass, levels = c("1", "2", "3"),
                           labels = c("Upper Class", "Middle Class", "Lower Class"))

port_jitter <- position_jitter(width = 0.3, height = 0.3)

ggplot(port_data, aes(x = factor(Embarked), y = Fare)) +
  geom_point(alpha = 0.4, position = port_jitter, shape = 21, colour = myBarCol) +
  scale_y_continuous(labels = dollar) +
  scale_x_discrete(labels = c("C" = "Cherbourg",
                              "Q" = "Queenstown",
                              "S" = "Southampton")) +
  labs(title = "SES of Embark locations",
       x = "Embark Location", y = "Ticket Price") +
  facet_wrap(~ Pclass, scale = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## ----CTree Prediction------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Take a fresh copy of the data to mutate
cTree_train_data <- clean_titanic_train

# Convert 'Survived' to labelled factor
cTree_train_data$Survived <- factor(cTree_train_data$Survived,
                                    levels = c(0, 1),
                                    labels = c("Died", "Survived"))

# Get a list of all character columns
chars <- sapply(cTree_train_data, is.character)

# Convert all character columns to factors
cTree_train_data[chars] <- lapply(cTree_train_data[chars], as.factor)

# Pick what variables contribute to classifying survival
formula <- Survived ~ Pclass + Sex + Fare + Embarked + Title + age_bins

# Prepare a vector to populate with all percentage results
cTree_accuracy <- c()

# Loop over this process changing the seed each time
for(i in 1:10)
{
  #Use a seed
  set.seed(1234 * i)
  
  # Randomize a 70 - 30 split of the data
  ind <- sample(2, nrow(clean_titanic_train), replace=TRUE, prob=c(0.7, 0.3))

  # Set aside the train and validation data
  cTree_training <- cTree_train_data[ind==1,]
  cTree_validate <- cTree_train_data[ind==2,]

  # Create ctree
  titanic_ctree <- ctree(formula, data = cTree_training)

  # Confusion Matrix of predictions
  iterationResult <- confusionMatrix(predict(titanic_ctree, newdata = cTree_validate),
                                     cTree_validate$Survived)
  
  # Add to list of accuracies obtained
  cTree_accuracy <- append(cTree_accuracy, iterationResult$overall["Accuracy"]) 
  
}

  # Print out the mean accuracy of the cTree algorithm
  mean(cTree_accuracy)



## ----Ctree Plot, fig.width = 12, fig.height = 6----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

plot(titanic_ctree)



## ----Random Forest---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Specify forumla for random forest
rf_formula <- Survived ~ Pclass + Sex + age_bins + Fare + Title

# Make a list of all variables to be used
rf_prediction_vars <- c('Survived', 'Pclass', 'Sex', 'Fare', 'Title', 'age_bins')

# Use just these varaibles to train with
rf_train_data <- clean_titanic_train[rf_prediction_vars]

# Convert 'Survived' to labelled factor
rf_train_data$Survived <- factor(rf_train_data$Survived,
                                 levels = c(0, 1), labels = c("Died", "Survived"))

# Get a list of all character columns
chars <- sapply(rf_train_data, is.character)

# Convert all character columns to factors
rf_train_data[chars] <- lapply(rf_train_data[chars], as.factor)

# Prepare a vector of accuracy to populate
rf_accuracy <- c()

# Loop with different splits 10 times to get an average accuracy
for(i in 1:10)
{
  # Use new seed each iteration
  set.seed(1234 * i)
  
  # Randomize a 70 - 30 split of the data
  ind <- sample(2, nrow(rf_train_data), replace=TRUE, prob=c(0.7, 0.3))

  # Set aside the train and validation data
  rf_training <- rf_train_data[ind==1,]
  rf_validate <- rf_train_data[ind==2,]
  
  # Train the model
  titanic_rf <- randomForest(rf_formula, data = rf_training)

  # Create a confusion matrix of predictions
  iterationResult <- confusionMatrix(predict(titanic_rf, newdata = rf_validate),
                                     rf_validate$Survived)

  # Add this iterations accuracy to the vector
  rf_accuracy <- append(rf_accuracy, iterationResult$overall["Accuracy"])
}

# Print out the mean accuracy of the random forest model
mean(rf_accuracy)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Find out what variables are most important
importance <- importance(titanic_rf)

# Create a data frame from the importance data
importance_clean <- data.frame(row.names(importance),
                               round(importance, 2))

# Clear the row names now that we've shifted them to a proper column
row.names(importance_clean) <- NULL

# Give the columns proper names
colnames(importance_clean) <- c("Variable", "Importance")

# Order the values
importance_clean$Variable <- factor(importance_clean$Variable, 
                                    levels = arrange(importance_clean,
                                                     Importance)$Variable)
  
# Graph importance of variables to the random forest
ggplot(importance_clean, aes(x = Variable, y = Importance)) +
  geom_bar_titanic +
  labs(title = "Importance of Variables") +
  coord_flip()



## ----KNN Prediction--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Varaibles to feed into the KNN algorithm
KNN_vars <- c('Survived', 'Fare', 'Pclass', 'Sex', 'Embarked', 'Title', 'age_bins')

# Assign variables to use
knn_train <- clean_titanic_train[KNN_vars]

# Get a list of all character columns
chars <- sapply(knn_train, is.character)

# Convert all character columns to factors
knn_train[chars] <- lapply(knn_train[chars], as.factor)

# Get a list of all factor columns
factors <- sapply(knn_train, is.factor)

# Now convert all those to integers (for KNN)
knn_train[factors] <- lapply(knn_train[factors], as.integer)

# Prepare vector to populate with accuracies
knn_accuracy <- c()

for(i in 1:10)
{
  # New seed every iteration
  set.seed(1234 * i)
  
  # Randomize a 70 - 30 split of the data
  ind <- sample(2, nrow(rf_train_data), replace=TRUE, prob=c(0.7, 0.3))

  # Split into train and validate
  knn_training <- knn_train[ind==1,]
  knn_validate <- knn_train[ind==2,]

  # Take away result (Store for later)
  knn_train_result <- knn_training$Survived
  knn_valid_result <- knn_validate$Survived
  knn_training$Survived = NULL
  knn_validate$Survived = NULL

  # Run K Nearest Neighbour Algorithm
  knn_titanic <- knn(train = knn_training, test = knn_validate,
                     cl = knn_train_result, k = 15)

  # Add accuracy to iteration vector
  knn_accuracy <- append(knn_accuracy,
                         (sum(knn_valid_result == knn_titanic)/length(knn_valid_result)))
  
}

# Print out mean accuracy
mean(knn_accuracy)



## ----Support Vector Machine Prediction-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Varaibles to feed into the KNN algorithm
svm_vars <- c('Survived', 'Fare', 'Pclass', 'Sex', 'Embarked', 'Title', 'age_bins')

# Assign variables to use
svm_train <- clean_titanic_train[svm_vars]

# Get a list of all character columns
chars <- sapply(svm_train, is.character)

# Convert all character columns to factors
svm_train[chars] <- lapply(svm_train[chars], as.factor)

# Get a list of all factor columns
factors <- sapply(svm_train, is.factor)

# Now convert all those to integers (for KNN)
svm_train[factors] <- lapply(svm_train[factors], as.integer)

# Create vector to be populated with accuracies
svm_accuracy <- c()

# Repeat process 10 times with different splits to get average accuracy
for(i in 1:10)
{
  # New seed every iteration
  set.seed(1234 * i)
  
  # Randomize a 70 - 30 split of the data
  ind <- sample(2, nrow(rf_train_data), replace=TRUE, prob=c(0.7, 0.3))
  
  # Split data
  svm_training <- svm_train[ind==1,]
  svm_validate <- svm_train[ind==2,]

  # Get vector of training results
  svm_train_results <- svm_training['Survived']
  svm_train_results <- factor(svm_train_results$Survived)

  # Get vector of validation results
  svm_valid_results <- svm_validate['Survived']
  svm_valid_results <- factor(svm_valid_results$Survived)

  # Remove classifier from training data sets
  svm_training$Survived <- NULL
  svm_validate$Survived <- NULL

  # Create the svm model
  svm_model <- svm(svm_training, svm_train_results)

  # Predict on validate data
  pred <- predict(svm_model, svm_validate)

  # Confusion Matrix of results
  iterationResult <- confusionMatrix(pred, factor(svm_valid_results))
  
  # Add accuracy to vector
  svm_accuracy <- append(svm_accuracy, iterationResult$overall["Accuracy"])

}

# Print out average accuracy
mean(svm_accuracy)



## ----Add Titles------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Extract just the titles
titles <- gsub(".*, (.*?)\\..*", "\\1", titanic_test$Name)

# Add this column to dataset
titanic_test$Title <- titles

# Replace any titles which aren't common with 'special'
titanic_test <- titanic_test %>%
  mutate(Title = replace(Title, !(Title %in% common_titles), "Special"))



## ----Aggregate and Bin Ages------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Aggregate missing age values to the median of their title
titanic_test <- titanic_test %>%
      mutate(Age = round(na.aggregate(Age, by = Title, FUN = median)))

# Cut ages into groups
titanic_test$age_bins <- cut(titanic_test$Age,
                             breaks = age_breaks,
                             labels = age_labels,
                             ordered_result = TRUE,
                             include.lowest = TRUE)



## ----Strip All Unused Variables--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Create list of vars to test on
test_prediction_vars <- c('Pclass', 'Sex', 'Fare', 'Title', 'age_bins')

# Remove from test data for the predictions
minimal_titanic_test <- titanic_test[test_prediction_vars]



## ----Convert Character Vectors to Factors----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Get a list of all character columns
chars <- sapply(minimal_titanic_test, is.character)

# Convert all character columns to factors
minimal_titanic_test[chars] <- lapply(minimal_titanic_test[chars], as.factor)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Train the model
titanic_rf <- randomForest(rf_formula, data = rf_train_data)

# Make predictions on test data
predictions <- predict(titanic_rf, newdata = minimal_titanic_test)

# Discard row names
pred_arr <- as.character(predictions)
pred_arr <- as.factor(pred_arr)

# Create new data frame of passenger ID, and prediction only
solution <- data.frame(PassengerId = titanic_test$PassengerId, Survived = pred_arr)

# Write output
write.csv(solution, file = 'rf_solution.csv', row.names = FALSE)


