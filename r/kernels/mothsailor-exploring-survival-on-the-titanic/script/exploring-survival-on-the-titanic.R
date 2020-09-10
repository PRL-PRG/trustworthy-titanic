## ----setup, echo = TRUE, message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_knit$set(width = 100)
knitr::opts_chunk$set(comment =  '', echo=TRUE)
knitr::opts_chunk$set(fig.align = 'center', fig.width = 6, fig.asp = 0.618, out.width = '80%')


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load packages
library(tidyverse)
library(stringr)
library(forcats)
library(randomForest)
library(missForest)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read_csv('../input/train.csv')
test  <- read_csv('../input/test.csv')

full  <- bind_rows(list(train=train, test=test), .id = 'id') 


glimpse(full)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Grab title from passenger names
Title <- str_extract(full$Name, '(, .+\\.)')
Title <- str_sub(Title, 3, -2)
Title[514] <- str_sub(Title[514], 1, 3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Show title counts by sex
xtabs(~ Sex + Title, full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Titles with very low cell counts to be combined to "Other" level

# Also reassign mlle, ms, and mme accordingly
Title[Title == 'Mlle']        <- 'Miss' 
Title[Title == 'Ms']          <- 'Miss'
Title[Title == 'Mme']         <- 'Mrs' 

Title <- fct_lump(Title, n = 4)

# Show title counts by sex again
xtabs(~Sex + Title, full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a family size variable including the passenger themselves
full <- full %>% mutate(Fsize = SibSp + Parch + 1)

full <- data.frame(full, Title)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use ggplot2 to visualize the relationship between family size & survival
full %>% filter(id == 'train') %>%
  ggplot(aes(x = Fsize, fill = factor(Survived))) +
    geom_bar(stat = 'count', position = 'dodge') +
    scale_x_continuous(breaks = c(1:11)) +
    scale_fill_discrete('Survived') +
    labs(x = 'Family Size') +
    theme_bw()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

full$FsizeD <- case_when(full$Fsize == 1 ~ 'singleton',
                    full$Fsize < 5 ~ 'small',
                    full$Fsize > 4 ~ 'large')


# Show family size by survival using a mosaic plot
mosaicplot(xtabs(~ FsizeD + Survived, full), shade = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(is.na(full$Cabin))

full$Deck <- factor(str_sub(full$Cabin, 1, 1))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mice::md.pattern(full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ix_embark <- which(is.na(full$Embarked))
# Passengers 62 and 830 are missing Embarkment
full[ix_embark, ]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full %>% filter(Pclass == 1) %>% 
  group_by(Embarked) %>%
  summarise(median(Fare))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Embarked[c(62, 830)] <- 'C'


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full[1044, ]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
imp_val <- full %>% filter(Embarked == 'S' & Pclass == 3) %>%
  summarise(median(Fare, na.rm = TRUE))

full[1044, 'Fare'] <- as.double(imp_val)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Show number of missing Age values
sum(is.na(full$Age))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

ix <- is.na(full$Age)

mfAge <- full %>%
  select(Age, Pclass, SibSp, Parch, Fare, Title, Fsize) %>%
  missForest(.)

Age_imputed <- ifelse(ix, mfAge$ximp$Age, full$Age)



## ---- out.width='45%'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot age distributions
plot_dta <- data.frame(Age_original = full$Age, Age_imputed = Age_imputed)
plot_dta <- plot_dta %>% tidyr::gather(source, Age) %>%
  mutate(source = str_sub(source, 5))

ggplot(full, aes(x = Age, y = ..density..)) + geom_histogram(binwidth = 1) 
ggplot(full[!ix, ], aes(x = Age, y = ..density..)) + geom_histogram(binwidth = 1)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Age <- Age_imputed



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mice::md.pattern(full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full <- full %>%
  mutate(SexNum = if_else(Sex == 'male', 0, 1),
         PclassSex = as.integer(Pclass) * SexNum,
         AgeSex = Age * SexNum,
         SibSpSex = SibSp * SexNum,
         FareSex = Fare * SexNum,
    #     FsizeDSex = as.integer(as.character(FsizeD)) * SexNum,
    #     ParchEmb = as.integer(as.character(Embarked)) * Parch,
      #   AgeEmb = as.integer(as.character(Embarked)) * Age,
         AgeFare = Age * Fare,
         PclassFare = as.integer(Pclass) * Fare)
     #    PclassFsizeD = as.integer(Pclass) * as.integer(as.character(FsizeD)))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Split the data back into a train set and a test set
train <- filter(full, id == 'train')
test <- filter(full, id == 'test')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set a random seed
set.seed(999)

rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + PclassSex + Age + AgeSex + SibSp +
                           Parch + Fare + Embarked + Title + FsizeD,
                           data = train, ntree = 2000)


as_data_frame(rf_model$err.rate) %>% 
  mutate(Tree = row_number()) %>%
  tidyr::gather(Type, Error, -Tree) %>%
  ggplot(aes(x = Tree, y = Error, colour = Type)) + geom_line() + 
  theme_bw()



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'], 2))

# Create a rank variable based on importance
varImportance %>%
  mutate(Rank = paste0('#', dense_rank(desc(Importance)))) %>%
  ggplot(aes(x = reorder(Variables, Importance), 
      y = Importance, fill = Importance)) +
    geom_bar(stat='identity') + 
    geom_text(aes(x = Variables, y = 0.5, label = Rank),
      hjust=0, vjust=0.55, size = 4, colour = 'red') +
    labs(x = 'Variables') +
    coord_flip() + guides(fill = FALSE) +
    theme_bw()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(caret)
ctrl <- trainControl(method = "repeatedcv",
                      repeats = 3,
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary)

car1 <- train(factor(Survived, labels = c('died', 'survived')) ~ Pclass + Sex + PclassSex + Age + AgeSex + SibSp +
  Parch + Fare + Embarked + Title + FsizeD + SibSpSex +
  FareSex + AgeFare + PclassFare, data = train,
  method = 'pls', preProc = c('center', 'scale'),
  tuneLength = 20,
  trControl = ctrl,
  metric = 'ROC')

print(car1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

prediction <- predict(car1, test) 
prediction <- ifelse(prediction == 'died', 0L, 1L)


# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = as.integer(test$PassengerId), Survived = prediction)

#write_csv(solution, path = 'Solution4.csv')

