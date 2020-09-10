## ---- include = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(MASS) # lda / qda
library(class) # knn
library(grid)
library(gridExtra)
library(stringr)

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  fig.width = 6,
  fig.asp = 0.618,
  out.width = "80%",
  fig.align = "center"
)


## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainFile <- '../input/train.csv'
testFile <- '../input/test.csv'
titanicTrainingDataset <- read_csv(trainFile)
knitr::kable(titanicTrainingDataset[1:10, ], caption = 'Titanic training dataset overview')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
map_dbl(titanicTrainingDataset, function(x) mean(is.na(x)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicTrainingDataset <- read_csv(
  trainFile, 
  col_types = cols(
    Survived = col_factor(0:1), 
    Pclass = col_factor(1:3), 
    Sex = col_factor(c("male", "female")), 
    Embarked = col_factor(c("C", "Q", "S"))
  )
) %>%
  filter(!is.na(Embarked)) %>%
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm = T), Age)) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicTrainingDataset <- titanicTrainingDataset %>%
  mutate(Title = as.factor(str_extract(Name, regex("([a-z]+\\.)", ignore_case = T))))

levels(titanicTrainingDataset$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(titanicTrainingDataset)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicTrainingDataset %>%
  ggplot(aes(x = Survived)) +
  geom_bar()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
analysePredictorResponse <- function(data, predictor, response) {
  if (is.factor(data[[predictor]])) {
      ggplot(mapping = aes(data[[predictor]], fill = data[[response]])) +
        geom_bar() +
        labs(title = paste(predictor, "vs", response), x = predictor, fill = response) 
  } else {
    chart1 <- ggplot(mapping = aes(data[[response]], data[[predictor]])) +
      geom_boxplot() +
      labs(title = paste(predictor, "vs", response), x = response, y = predictor)
    
    chart2 <- ggplot(mapping = aes(x = data[[predictor]], , y = ..density.., colour = data[[response]])) +
      geom_freqpoly(position = "dodge") +
      labs(title = paste(predictor, "vs", response), colour = response, x = predictor)
    
    grid.arrange(chart1, chart2)
  }
}


## ---- fig.asp = 1.2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
analysePredictorResponse(titanicTrainingDataset, 'Age', 'Survived')


## ---- fig.asp = 1.2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
analysePredictorResponse(titanicTrainingDataset, 'Sex', 'Survived')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicTrainingDataset %>% 
  group_by(Sex) %>%
  summarize(SurvivedRatio = mean(Survived == 1))


## ---- fig.asp = 1.2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
analysePredictorResponse(titanicTrainingDataset, 'Pclass', 'Survived')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicTrainingDataset %>% 
  group_by(Pclass) %>%
  summarize(SurvivedRatio = mean(Survived == 1))


## ---- fig.asp = 1.2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
analysePredictorResponse(titanicTrainingDataset, 'SibSp', 'Survived')


## ---- fig.asp = 1.2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
analysePredictorResponse(titanicTrainingDataset, 'Parch', 'Survived')


## ---- fig.asp = 1.2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
analysePredictorResponse(titanicTrainingDataset, 'Fare', 'Survived')


## ---- fig.asp = 1.2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
analysePredictorResponse(titanicTrainingDataset, 'Embarked', 'Survived')

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicTrainingDataset %>% 
  group_by(Embarked) %>%
  summarize(SurvivedRatio = mean(Survived == 1))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicTrainingDataset %>%
  ggplot(mapping = aes(Pclass, Fare)) +
  geom_boxplot()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicTrainingDataset %>%  
  ggplot(mapping = aes(Embarked, fill = Pclass)) +
  geom_bar()


## ---- fig.asp = 1.2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicTrainingDataset %>%
  group_by(Title) %>%
  summarize(SurvivedRatio = mean(Survived == 1)) %>%
  arrange(SurvivedRatio) %>%
  mutate(Title = factor(Title, levels = Title)) %>%
  ggplot(aes(x = Title, y = SurvivedRatio)) +
  geom_col() +
  coord_flip()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicTrainingDataset <- titanicTrainingDataset %>% mutate(
  RefinedTitle = factor(ifelse(Title %in% c('Capt.', 'Don', 'Jonkheer.', 'Rev.', 'Mr.'), 1, 
    ifelse(Title %in% c('Col.', 'Dr.', 'Major.', 'Master.'), 2, 3
    )
  ))
)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model <- glm(Survived ~ RefinedTitle + Pclass + SibSp + Embarked + Age + Sex + Parch, data = titanicTrainingDataset %>% filter(!is.na(Age)), family = binomial)
summary(model)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model <- glm(Survived ~ RefinedTitle + Pclass + SibSp + Age, data = titanicTrainingDataset, family = binomial)
summary(model)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for (j in seq(.1, .9, .1)) {
  predictions <- rep(0, nrow(titanicTrainingDataset))
  for (i in 1:nrow(titanicTrainingDataset)) {
    model <- glm(Survived ~ RefinedTitle + Pclass + SibSp + Age, data = titanicTrainingDataset, family = binomial, subset = -i)
    predictions[i] <- predict(model, titanicTrainingDataset[i,], type = "response") > j
  }
  
  print(paste("Decision boundary value :", j))
  print(table(predictions, titanicTrainingDataset$Survived))
  print(mean(predictions != titanicTrainingDataset$Survived))
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model <- glm(Survived ~ RefinedTitle + Pclass + SibSp + Age, data = titanicTrainingDataset, family = binomial)

titanicTestDataset <- read_csv(
  testFile,
  col_types = cols(
    Pclass = col_factor(1:3), 
    Sex = col_factor(c("male", "female")), 
    Embarked = col_factor(c("C", "Q", "S"))
  )
) %>%
  mutate(Title = as.factor(str_extract(Name, regex("([a-z]+\\.)", ignore_case = T)))) %>% 
  mutate(
    RefinedTitle = factor(ifelse(Title %in% c('Capt.', 'Don', 'Jonkheer.', 'Rev.', 'Mr.'), 1, 
      ifelse(Title %in% c('Col.', 'Dr.', 'Major.', 'Master.'), 2, 3
      )
    ))
  ) %>%
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm = T), Age))

predictions <- predict(model, titanicTestDataset, type = "response") > .6

tibble(PassengerId = titanicTestDataset$PassengerId, Survived = as.integer(predictions)) %>%
  write_csv('predictions-logistic-regression.csv')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model <- qda(Survived ~ RefinedTitle + Pclass + SibSp + Age, data = titanicTrainingDataset, family = binomial)
model


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predictions <- factor(rep(0, nrow(titanicTrainingDataset)), levels = 0:1)
for (i in 1:nrow(titanicTrainingDataset)) {
  model <- qda(Survived ~ RefinedTitle + Pclass + SibSp + Age, data = titanicTrainingDataset, subset = -i)
  predictions[i] <- predict(model, titanicTrainingDataset[i,])$class
}

table(predictions, titanicTrainingDataset$Survived)
mean(predictions != titanicTrainingDataset$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model <- qda(Survived ~ RefinedTitle + Pclass + SibSp + Age, data = titanicTrainingDataset)
predictions <- predict(model, titanicTestDataset)$class

tibble(PassengerId = titanicTestDataset$PassengerId, Survived = predictions) %>%
  write_csv('predictions-qda.csv')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1)
titanicTrainingDatasetKnn <- titanicTrainingDataset
titanicTrainingDatasetKnn$RefinedTitle = as.integer(titanicTrainingDatasetKnn$RefinedTitle)
titanicTrainingDatasetKnn$Pclass = as.integer(titanicTrainingDatasetKnn$Pclass)

testSampleSize <- 150
isTest <- sample(nrow(titanicTrainingDataset), testSampleSize)

knnTrainDataset <- titanicTrainingDatasetKnn[-isTest,] %>%
  subset(select = c(RefinedTitle, Pclass, SibSp, Age)) %>%
  scale()

knnTestDataset <- titanicTrainingDatasetKnn[isTest,] %>%
  subset(select = c(RefinedTitle, Pclass, SibSp, Age)) %>%
  scale()

cl <- titanicTrainingDatasetKnn[-isTest,]$Survived

errorsRate <- rep(0, testSampleSize)
for (k in 1:testSampleSize) {
  predictions <- knn(knnTrainDataset, knnTestDataset, cl, k)
  errorsRate[k] = mean(predictions != titanicTrainingDatasetKnn[isTest,]$Survived)
}

tibble(k = 1:testSampleSize, errorsRate = errorsRate) %>%
  ggplot(aes(x = k, y = errorsRate)) +
  geom_path()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicTestDatasetKnn <- titanicTestDataset %>%
  mutate(RefinedTitle = as.integer(RefinedTitle)) %>%
  mutate(Pclass = as.integer(Pclass))

knnTrainDataset <- titanicTrainingDatasetKnn %>%
  subset(select = c(RefinedTitle, Pclass, SibSp, Age)) %>%
  scale()

knnTestDataset <- titanicTestDatasetKnn %>%
  subset(select = c(RefinedTitle, Pclass, SibSp, Age)) %>%
  scale()

cl <- titanicTrainingDatasetKnn$Survived

predictions <- knn(knnTrainDataset, knnTestDataset, cl, 35)

tibble(PassengerId = titanicTestDataset$PassengerId, Survived = predictions) %>%
  write_csv('predictions-knn.csv')

