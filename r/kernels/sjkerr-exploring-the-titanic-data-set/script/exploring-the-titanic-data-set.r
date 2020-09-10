
# Load the packages

library(tidyverse)
library(caret)
library(class)
library(naivebayes)
library(pROC)
library(randomForest)
library(rpart)
library(rpart.plot)
library(broom)
library(mgcv)
library(ranger)
library(vtreat)
library(xgboost)
library(class)
library(WVPlots)
library(magrittr)

# Read in the data
titanic <- read.csv('../input/train.csv', stringsAsFactors = F)

# Turn data into a tbl
titanic <- as_tibble(titanic)

glimpse(titanic)


na_cols <- sapply(titanic, is.na) 
colSums(na_cols)

ggplot(titanic, aes(x = Pclass, y = Sex, color = Survived)) +
        geom_jitter() +
        geom_point(alpha = 0.4) +
        ggtitle("Passengers by Class and Sex")

titanic %>%
        group_by(Pclass, Sex, Survived) %>%
        summarise(n = n()) %>%
        group_by(Pclass, Sex) %>%
        summarise(Total = sum(n),
                  Died_percent =  round(sum(n[Survived == 0]) / Total * 100 ),
                  Died_Percent_PTotal = round(sum(n[Survived == 0]) / 891 * 100))

titanic %>%
        group_by(Pclass, Sex, Survived) %>%
        summarise(n = n()) %>%
        group_by(Sex) %>%
        summarise(Total = sum(n),
                  Died = round(sum(n[Survived == 0]) / Total * 100),
                  Died_Percent_PTotal = round(sum(n[Survived == 0]) / 891 * 100))

ggplot(titanic, aes(x = SibSp, y = Parch, color = as.factor(Survived))) +
        geom_jitter() +
        geom_point(alpha = 0.4) +
        ggtitle("Passengers Siblings, Parents, Children")

titanic %>%
        filter(SibSp > 2) %>%
        group_by(Pclass, Survived) %>%
        summarise(n = n()) %>%
        summarise(Total = sum(n),
                  Died_Percent = round(sum(n[Survived == 0]) / Total * 100),
                  Died_Percent_PTotal = round(sum(n[Survived == 0]) / 891 * 100))

titanic %>%
        group_by(SibSp, Parch, Survived) %>%
        summarise(n = n()) %>%
        summarise(Total = sum(n),
                  Died_Percent = round(sum(n[Survived == 0]) / Total * 100),
                  Died_Percent_PTotal = round(sum(n[Survived == 0]) / 891 * 100))

titanic %>%
        filter(Parch > 0, SibSp > 2) %>%
        group_by(Pclass, Survived) %>%
        summarise(n = n()) %>%
        summarise(Total = sum(n),
                  Died_Percent = round(sum(n[Survived == 0]) / Total * 100),
                  Died_Percent_PTotal = round(sum(n[Survived == 0]) / 891 * 100))

# I'm sure there is an easier way of doing this!
titanic$Age_Group <- ifelse(is.na(titanic$Age),0, titanic$Age)
titanic$Age_Group <- ifelse(titanic$Age_Group > 0 & titanic$Age_Group < 16,
                            1, titanic$Age_Group)
titanic$Age_Group <- ifelse(titanic$Age_Group >= 16 & titanic$Age_Group < 30, 
                            21, titanic$Age_Group)
titanic$Age_Group <- ifelse(titanic$Age_Group >= 30 & titanic$Age_Group < 45, 
                            22, titanic$Age_Group)
titanic$Age_Group <- ifelse(titanic$Age_Group >= 45 & titanic$Age_Group < 60, 
                            23, titanic$Age_Group)
titanic$Age_Group <- ifelse(titanic$Age_Group >= 60, 3, titanic$Age_Group)

# Factorise groupings

titanic$Age_Group <- factor(titanic$Age_Group, levels = c(0, 1, 21, 22, 23, 3), 
                            labels = c("Unknown", "Child", "Adult1", "Adult2",
                                       "Adult3", "Senior"))

ggplot(titanic, aes(Age_Group, fill = as.factor(Survived))) +
        geom_bar(position = position_dodge()) +
        ggtitle("Age Group")

titanic %>%
        group_by(Age_Group, Survived) %>%
        summarise(n = n()) %>%
        summarise(Total = sum(n),
                  Died_Percent = round(sum(n[Survived == 0]) / Total * 100),
                  Died_Percent_PTotal = round(sum(n[Survived == 0]) / 891 * 100))

titanic %>%
        group_by(Age_Group, Pclass, Survived) %>%
        summarise(n = n()) %>%
        summarise(Total = sum(n),
                  Died_Percent = round(sum(n[Survived == 0]) / Total * 100),
                  Died_Percent_PTotal = round(sum(n[Survived == 0]) / 891 * 100))



titanic <- read.csv('../input/train.csv', stringsAsFactors = F)

# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(titanic))

# Randomly order data
titanic <- titanic[rows, ]

# Determine row to split on: split 60:40
split <- round(nrow(titanic) * .60)

# Create train
titanic_train <- titanic[1:split, ]

# Create test
titanic_test <- titanic[(split + 1): nrow(titanic), ]

str(titanic_train)

outcome <- titanic_train$Survived
vars <- c("Pclass", "Sex", "Age", "SibSp", "Parch")

# Create the treatment plan
treatplan <- designTreatmentsZ(titanic_train, vars)

scoreFrame <- treatplan %>%
                use_series(scoreFrame) %>%
                select(varName, origName, code)

# Select the rows with codes "clean" or "lev"
newvars <- scoreFrame %>%
                filter(code %in% c("clean", "lev")) %>%
                use_series(varName)

# Create the treated training data
titanic_train.treat <- prepare(treatplan, titanic_train, varRestriction = newvars)

newvars


# Create formula
fmla.1 <- Survived ~ Pclass + Sex + SibSp + Parch
fmla.2 <- Survived ~ Pclass:Sex + SibSp + Parch

# Fit the logistic regression model
t_model.1 <- glm(fmla.1, data = titanic_train, family = binomial)

# Call summary
summary(t_model.1)

# Call glance
perf.1 <- glance(t_model.1)

# Calculate pseudo-R-squared
pseudoR2.1 <- 1 - perf$deviance / perf$null.deviance

# Fit the logistic regression model
t_model.2 <- glm(fmla.2, data = titanic_train, family = binomial)

# Call summary
summary(t_model.2)

# Call glance
perf.2 <- glance(t_model.2)

# Calculate pseudo-R-squared
pseudoR2.2 <- 1 - perf$deviance / perf$null.deviance
