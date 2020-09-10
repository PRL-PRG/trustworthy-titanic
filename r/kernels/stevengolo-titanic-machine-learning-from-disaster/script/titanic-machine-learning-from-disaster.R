## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----packages, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------
## Importing packages
library(glmnet)
library(gridExtra)
library(kableExtra)
library(knitr)
library(randomForest)
library(reshape2)
library(tidyverse)

# Create custom theme for ggplot2
theme_custom <- function(base_family = "Times"){
  theme_minimal(base_family = base_family) %+replace%
    theme(
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 16, vjust = -1),
      
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), angle = 90),
      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0)),
      
      strip.text.x = element_text(size = 16)
    )
}


## ----load, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read_csv('../input/train.csv')
test <- read_csv('../input/test.csv')

titanic <- train %>% 
            bind_rows(test) %>%
            select(-PassengerId) %>%
            mutate_at(vars(Pclass, Sex, Embarked), funs(factor(.)))


## ----class, echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
titanic %>%
  ggplot(aes(x = Pclass)) +
    geom_bar(width = 0.5, fill = "#56B4E9") +
    coord_flip() +
    labs(title = 'Count of the passengers by class') + 
    scale_x_discrete(name = "Passenger's class") +
    scale_y_continuous(name = "Count", breaks = seq(0, 900, 100)) +
    theme_custom()


## ----sex, echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------
titanic %>%
  ggplot(aes(x = Sex)) +
    geom_bar(width = 0.5, fill = "#56B4E9") +
    coord_flip() +
    labs(title = 'Count of the passengers by sex') + 
    scale_x_discrete(name = "Passenger's sex") +
    scale_y_continuous(name = "Count", breaks = seq(0, 900, 100)) +
    theme_custom()


## ----title, echo=TRUE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Extract the title from the Passenger's name.
Title <- "^.*, (.*?)\\..*$" %>% 
          gsub("\\1", titanic$Name)
# Create another factors for low represented title.
title_high <- c('Mr', 'Miss', 'Mrs', 'Master')
Title <- Title %in% title_high %>%
          if_else(Title, 'Other')
# Add titlecolumn to the dataframe
titanic <- titanic %>% 
            add_column(Title) %>%
            mutate_at(vars(Title), funs(factor(.)))


## ----name, echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
titanic %>%
  ggplot(aes(x = Title)) +
    geom_bar(width = 0.5, fill = "#56B4E9") +
    coord_flip() +
    labs(title = 'Count of the passengers by title') + 
    scale_x_discrete(name = "Passenger's title") +
    scale_y_continuous(name = "Count", breaks = seq(0, 900, 100)) +
    theme_custom()


## ----missing_port, echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic %>% 
  filter(is.na(Embarked)) %>%
  kable(format = 'html') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


## ----plot_missing_embarked, echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------
titanic %>% 
  filter(Pclass == 1) %>%
  ggplot(aes(x = Embarked, y = Fare)) +
    geom_boxplot(colour = "black", fill = "#56B4E9", outlier.colour = 'red') +
    ggtitle("Fare depending on the port of embarkation for the first class") +
    scale_x_discrete(name = "Port of embarkation") +
    scale_y_continuous(name = "Fare") +
    theme_custom()


## ----inputation_port, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic[62, "Embarked"] <- "C"
titanic[830, "Embarked"] <- "C"


## ----port, echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
titanic %>%
  ggplot(aes(x = Embarked)) +
    geom_bar(width = 0.5, fill = "#56B4E9") +
    coord_flip() +
    labs(title = 'Count of the passengers by port of embarkation') + 
    scale_x_discrete(name = "Passenger's port of embarkation") +
    scale_y_continuous(name = "Count", breaks = seq(0, 900, 100)) +
    theme_custom()


## ----missing_fare, echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic %>% 
  filter(is.na(Fare)) %>%
  kable(format = 'html') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


## ----plot_fare, echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------
titanic %>%
  filter(Embarked == 'S', Pclass == 3) %>%
  ggplot(aes(x = Fare)) +
    geom_density(kernel = 'gaussian', colour = "#FFFFFF", fill = "#56B4E9") +
    ggtitle("Density of Fare", subtitle = "Embarked in Southampton in third class") +
    xlab("Fare ($)") + ylab("Density") +
    theme_custom()


## ----fare_inputation, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic[1044, "Fare"] <- titanic %>% filter(Embarked == 'S', Pclass == 3) %>% pull(Fare) %>% median(na.rm = TRUE)


## ----plot_age, echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------
grid.arrange(
titanic %>% 
  ggplot(aes(y = Age)) +
    geom_boxplot(fill = "#56B4E9", outlier.colour = 'red') +
    coord_flip() +
    ggtitle("Boxplot of Age") +
    scale_x_continuous(breaks = NULL) +
    theme_custom(),

titanic %>%
  ggplot(aes(x = Age)) +
    geom_density(kernel = 'gaussian', colour = "#FFFFFF", fill = "#56B4E9") +
    ggtitle("Density of Age") +
    xlab("Age") + ylab("Density") +
    theme_custom(),
ncol = 2, nrow = 1)


## ----ridge_regression_age, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Split the dataset into the ones with Age and the ones without Age.
titanic.with.age <- titanic %>% 
  filter(!is.na(Age)) %>%
  select(-c(Survived, Name, Ticket, Cabin))
titanic.without.age <- titanic %>%
  filter(is.na(Age)) %>%
  select(-c(Survived, Name, Ticket, Cabin)) %>%
  mutate(Age = 0)

# Build a model matrix of the data
titanic.lm <- lm(Age ~ ., data = titanic.with.age)
titanic.with.age.model.matrix <- model.matrix(titanic.lm, data = titanic.with.age)[,-1]
# Perform the Ridge Regression (alpha = 0)
titanic.age.model <- glmnet(titanic.with.age.model.matrix, titanic.with.age$Age, alpha = 0)

# Prediction of the Age 
titanic.without.age$Age <- predict(titanic.age.model, 
  newx = model.matrix(titanic.lm, data = titanic.without.age)[, -1],
  s = cv.glmnet(titanic.with.age.model.matrix, titanic.with.age$Age, alpha = 0)$lambda.min,
  type = 'link')

# Replace the missing Age into the all dataset
titanic[is.na(titanic$Age), "Age"] <- titanic.without.age$Age


## ----plot_age2, echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------
grid.arrange(
titanic %>% 
  ggplot(aes(y = Age)) +
    geom_boxplot(fill = "#56B4E9", outlier.colour = 'red') +
    coord_flip() +
    ggtitle("Boxplot of Age") +
    scale_x_continuous(breaks = NULL) +
    theme_custom(),

titanic %>%
  ggplot(aes(x = Age)) +
    geom_density(kernel = 'gaussian', colour = "#FFFFFF", fill = "#56B4E9") +
    ggtitle("Density of Age") +
    xlab("Age") + ylab("Density") +
    theme_custom(),
ncol = 2, nrow = 1)


## ----plot_sipsp, echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------
titanic %>%
  ggplot(aes(x = as.factor(SibSp))) +
    geom_bar(width = 0.5, fill = "#56B4E9") +
    coord_flip() +
    labs(title = 'Count of the passengers number of siblings/spouses') + 
    scale_x_discrete(name = "Number of Siblings/Spouses") +
    scale_y_continuous(name = "Count", breaks = seq(0, 900, 100)) +
    theme_custom()


## ----plot_parch, echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------
titanic %>%
  ggplot(aes(x = as.factor(Parch))) +
    geom_bar(width = 0.5, fill = "#56B4E9") +
    coord_flip() +
    labs(title = 'Count of the passengers number of parents/children') + 
    scale_x_discrete(name = "Number of Parents/Children") +
    scale_y_continuous(name = "Count", breaks = seq(0, 1000, 100)) +
    theme_custom()


## ----del_cabin, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic <- titanic %>% select(-Cabin)


## ----del_ticket, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic <- titanic %>% select(-Ticket)


## ----model, echo=TRUE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- titanic %>% select(-Name) %>% filter(!is.na(Survived))
test <- titanic %>% select(-Name) %>% filter(is.na(Survived))

# Split the train set into two dataset (for validation)
set.seed(42)
sample <- sample(c(TRUE, FALSE), nrow(train), replace = TRUE, prob = c(2/3, 1/3))
train.val <- train[sample, ]
test.val <- train[!sample, ]

# Perform Ridge regression
train.lm <- lm(Survived ~ ., data = train.val)
X <- model.matrix(train.lm, data = train.val)[ , -1]
Y <- train.val$Survived
train.ridge.model <- glmnet(X, Y, alpha = 0, family = 'binomial')

# Prediction on the test.val set
test.val.predict <- predict(train.ridge.model, 
                            s = cv.glmnet(X, Y, alpha = 0)$lambda.min,
                            newx = model.matrix(train.lm, data = test.val)[ , -1],
                            type = 'class')


## ----submission, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Prediction of the test set
test$Survived <- 0
test.predict <- predict(train.ridge.model, 
                        s = cv.glmnet(X, Y, alpha = 0)$lambda.min,
                        newx = model.matrix(train.lm, data = test)[ , -1],
                        type = 'class') 

# Construt the dataframe
result <- data.frame(PassengerID = row.names(test.predict),
                     Survived = test.predict[ , 1])

# Export as CSV
write.csv(result, 'results.csv', row.names = FALSE)

