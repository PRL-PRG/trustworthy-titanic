## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# For data manipulation and tidying
library(dplyr)

# For data visualizations
library(ggplot2)

# For modeling and predictions
library(caret)
library(glmnet)
library(ranger)
library(e1071)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('../input/train.csv', header = TRUE, stringsAsFactors = FALSE)
train$Dataset <- "train"

test <- read.csv('../input/test.csv', header = TRUE, stringsAsFactors = FALSE)
test$Dataset <- "test"

full <- bind_rows(train, test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
factor_variables <- c('PassengerId', 'Survived', 'Pclass', 'Sex', 'Embarked', 'Dataset')
full[factor_variables] <- lapply(full[factor_variables], function(x) as.factor(x))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
names <- full$Name

titles <-  gsub("^.*, (.*?)\\..*$", "\\1", names)

full$Titles <- titles

unique(full$Titles)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(full$Sex, full$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(full$Pclass, full$Titles)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Titles <- gsub("Dona|Lady|Madame|the Countess", "Lady", full$Titles)
full$Titles <- gsub("Don|Jonkheer|Sir", "Sir", full$Titles)

unique(full$Titles)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Titles <- as.factor(full$Titles)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full <- mutate(full, FamilySize = SibSp + Parch + 1)


## ----echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
hist(full$FamilySize, 
     main = "Family Group Size", 
     xlab = "People per Family Group", 
     col = "#56B4E9",
     xlim = c(1,11),
     breaks = 11)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$TravelGroup <- NA


## ----results = 'hide'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full2 <- arrange(full, Ticket)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(full2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full2 <- (transform(full2, TravelGroup = match(Ticket, unique(Ticket))))

# Can't forget to make those Travel Groups into factors!
full2$TravelGroup <- as.factor(full2$TravelGroup)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full3 <- full2 %>% 
            group_by(TravelGroup) %>% 
            mutate(GroupSize = n()) %>%
            ungroup()


## ----echo = FALSE, fig.show = 'hold', fig.width = 4, fig.height = 3.5------------------------------------------------------------------------------------------------------------------------------------------------------------

hist(full3$FamilySize, 
     main = "Family Group Size", 
     xlab = "People per Family Group", 
     col = "#56B4E9",
     xlim = c(1,11),
     breaks = 11)
hist(full3$GroupSize, 
     main = "Travel Group Size", 
     xlab = "People per Travel Group", 
     col = "#D55E00",
     xlim = c(1,11),
     breaks = 11)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
filtered <- filter(full3, GroupSize == 1)

# How many were listed as being onboard with siblings or spouses?
fSibSp <- filtered[filtered$SibSp > 0, ]
nrow(fSibSp)

# How many were listed as being onboard with parents or children?
fParch <- filtered[filtered$Parch > 0, ]
nrow(fParch)

# How many of those people overlapped both groups?
sum(fSibSp$PassengerId %in% fParch$PassengerId)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(full3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full3[(which(is.na(full3$Fare))) , 1]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Resort the dataset by Passenger Number
full4 <- arrange(full3, PassengerId)

# Where did this passenger leave from? What was their class?
full4[1044, c(3, 12)]


## ----echo = FALSE, warning = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(full4[full4$Pclass == '3' & full$Embarked == 'S', ], 
  aes(x = Fare)) +
  geom_density(fill = '#56B4E9', alpha = 0.4) + 
  geom_vline(aes(xintercept = median(Fare, na.rm = T)),
    colour = '#D55E00', linetype = 'dotted', lwd = 1) +
  scale_x_continuous() +
  theme(panel.grid.major = element_blank())



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full4 %>%
  filter(Pclass == '3' & Embarked == 'S') %>%
  summarise(missing_fare = median(Fare, na.rm = TRUE))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full4$Fare[1044] <- 8.05

summary(full4$Fare)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full4$Embarked[full4$Embarked == ""] <- NA

full4[(which(is.na(full4$Embarked))), 1]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full4[c(62, 830), c(1,3,10)]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full4 %>%
  group_by(Embarked, Pclass) %>%
  filter(Pclass == "1") %>%
  summarise(mfare = median(Fare),
            n = n())


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Assign empty embark ports to 'C'
full4$Embarked[c(62,830)] <- 'C'

# Drop unused levels (since there should be no more blanks)
full4$Embarked <- droplevels(full4$Embarked)

# Check to make sure there are no NA's or blanks
levels(full4$Embarked)


## ----results = 'hide'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predicted_age <- train(
  Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Titles + FamilySize + GroupSize,
  tuneGrid = data.frame(mtry = c(2, 3, 7)),
  data = full4[!is.na(full4$Age), ],
  method = "ranger",
  trControl = trainControl(
      method = "cv", number = 10,
      repeats = 10, verboseIter = TRUE),
  importance = 'impurity'
  )



## ----echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Creating a Variable Importance variable
 vimp <- varImp(predicted_age)

# Plotting "vimp"
 ggplot(vimp, 
        top = dim(vimp$importance)[1]
        )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full4$Age[is.na(full4$Age)] <- predict(predicted_age, full4[is.na(full4$Age),])

# Check the summary to make sure there are no more NA values
summary(full4$Age)


## ----echo = FALSE, fig.show = 'hold', fig.width = 4, fig.height = 3.5------------------------------------------------------------------------------------------------------------------------------------------------------------
hist(full3$Age, 
     main = "Known Age Distribution", 
     xlab = "Known Age", 
     col = "#56B4E9",
     breaks = 20)
hist(full4$Age,
     main = "Known + Predicted Age Distribution",
     xlab = "Known Age",
     col = "#D55E00",
     breaks = 20)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_complete <- full4[full4$Dataset == 'train', ]
test_complete <- full4[full4$Dataset == 'test', ]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myControl <- trainControl(
	  method = "cv", 
	  number = 10,
	  repeats = 10, 
	  verboseIter = TRUE
  )


## ----results = 'hide'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rf_model <- train(
    Survived ~ Age + Pclass + Sex + SibSp + Parch + Fare + Embarked + Titles + FamilySize + 
      TravelGroup + GroupSize,
    tuneGrid = data.frame(mtry = c(2, 5, 8, 10, 15)),
    data = train_complete, 
    method = "ranger", 
    trControl = myControl,
    importance = 'impurity'
)


## ----results = 'hide'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glm_model <- train(
    Survived ~ Age + Pclass + Sex + SibSp + Parch + Fare + Embarked + Titles + FamilySize + 
      TravelGroup + GroupSize, 
    method = "glmnet",
    tuneGrid = expand.grid(alpha = 0:1,
      lambda = seq(0.0001, 1, length = 20)),
    data = train_complete,
    trControl = myControl
)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a list of models
models <- list(rf = rf_model, glmnet = glm_model)

# Resample the models
resampled <- resamples(models)

# Generate a summary
summary(resampled)

# Plot the differences between model fits
dotplot(resampled, metric = "Accuracy")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Reorder the data by Passenger ID number
test_complete <- test_complete %>%
                  arrange(PassengerId)

# Make predicted survival values
my_prediction <- predict(glm_model, test_complete)


train <- full[1:891,]
test <- full[892:1309,]

# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
library(randomForest)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           FsizeD + Child + Mother,
                         data = train)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
#```

#The black line shows the overall error rate which falls below 20%. The red and green lines show the error rate for 'died' and 'survived' respectively. We can see that right now we're much more successful predicting death than we are survival. What does that say about me, I wonder?

## Variable importance

#Let's look at relative variable importance by plotting the mean decrease in Gini calculated across all trees.

#```{r, message=FALSE, warning=FALSE}
# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()
#```

#Whoa, glad we made our title variable! It has the highest relative importance out of all of our predictor variables. I think I'm most surprised to see that passenger class fell to `r rankImportance[rankImportance$Variable == 'Pclass', ]$Rank`, but maybe that's just bias coming from watching the movie Titanic too many times as a kid.

## Prediction!

#We're ready for the final step --- making our prediction! When we finish here, we could iterate through the preceding steps making tweaks as we go or fit the data using different models or use different combinations of variables to achieve better predictions. But this is a good starting (and stopping) point for me now.

#```{r}
# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a data frame with two columns: PassengerId & Survived where Survived contains my predictions.
my_solution_5 <- data.frame(PassengerID = test$PassengerId, Survived = my_prediction)

# Write the solution to a csv file 
write.csv(my_solution_5, file = "my_solution_5.csv", row.names = FALSE)

