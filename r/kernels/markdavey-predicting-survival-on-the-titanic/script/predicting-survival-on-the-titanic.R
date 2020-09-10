## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# For data manipulation and tidying
library(dplyr)
library(data.table)

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



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Titles <- gsub("Dona|Lady|Madame|the Countess", "Lady", full$Titles)
full$Titles <- gsub("Don|Jonkheer|Sir", "Sir", full$Titles)

full$Titles <- as.factor(full$Titles)

full <- mutate(full, FamilySize = SibSp + Parch + 1)



full$TravelGroup <- NA

full2 <- arrange(full, Ticket)

full2 <- (transform(full2, TravelGroup = match(Ticket, unique(Ticket))))

# Can't forget to make those Travel Groups into factors!
full2$TravelGroup <- as.factor(full2$TravelGroup)

full3 <- full2 %>% 
            group_by(TravelGroup) %>% 
            mutate(GroupSize = n()) %>%
            ungroup()

filtered <- filter(full3, GroupSize == 1)

# How many were listed as being onboard with siblings or spouses?
fSibSp <- filtered[filtered$SibSp > 0, ]
nrow(fSibSp)

# How many were listed as being onboard with parents or children?
fParch <- filtered[filtered$Parch > 0, ]
nrow(fParch)

# How many of those people overlapped both groups?
sum(fSibSp$PassengerId %in% fParch$PassengerId)


# Resort the dataset by Passenger Number
full4 <- arrange(full3, PassengerId)

# Where did this passenger leave from? What was their class?
full4[1044, c(3, 12)]


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

setnames(train_complete,"Survived","SurvivedFact")
reviewFit <- predict(glm_model, train_complete)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Reorder the data by Passenger ID number
test_complete <- test_complete %>%
                  arrange(PassengerId)

# Make predicted survival values
my_prediction <- predict(glm_model, test_complete)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a data frame with two columns: PassengerId & Survived where Survived contains my predictions.
#my_solution_5 <- data.frame(PassengerID = test$PassengerId, Survived = my_prediction)
head(reviewFit)
# Write the solution to a csv file 
#write.csv(my_solution_5, file = "my_solution_5.csv", row.names = FALSE)

