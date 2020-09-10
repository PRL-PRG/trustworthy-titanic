
# 01 IMPORT LIBRARY
library(dplyr)         ## Data manipulation
library(ggplot2)       ## Data vistualization

library(class)         ## KNN model
library(rpart)         ## Decision tree
library(rpart.plot)
library(ramdomForest)  ## Random forest


# 02 IMPORT DATASET
train_data <- read.csv('../input/train.csv') # Import train dataset
test_data  <- read.csv('../input/test.csv')  # Import test dataset


# 03 EXPLORATORY DATASET
## Look at all variables in dataset
glimpse(train_data)  

## Create new data frame called "train_data2" including 2 new variables
train_data2 <- train_data %>% 
  mutate(
## Create new variable "Fam" - Number of person in Family
      Fam = Parch + SibSp, 
## Create new variables "Age_grp" - Age group band
      Age_grp = case_when(
                    is.na(.$Age) ~ "00 Error",
                    .$Age > 0  & .$Age <= 12 ~ "01 Child",
                    .$Age > 12 & .$Age <= 20  ~ "02 Teenager",
                    .$Age > 20 & .$Age <= 50  ~ "03 Adult",
                    .$Age > 50 ~ "04 Elder"))


## Create function to do summary table
sumtabl_fun  <- function(data, var){
    temp_table  <- data  %>% 
        group_by_(as.character(var))  %>%
        summarise(Survival_rate = sum(Survived)/n())
 
    return(temp_table)
}

## Start summarising function
sumtabl_Sex       <- sumtabl_fun(train_data, 'Sex')
sumtabl_Pclass    <- sumtabl_fun(train_data, 'Pclass') 
sumtabl_SibSp     <- sumtabl_fun(train_data, 'SibSp')
sumtabl_Parch     <- sumtabl_fun(train_data, 'Parch') 
sumtabl_Embarked  <- sumtabl_fun(train_data, 'Embarked')
sumtabl_Agegrp    <- sumtabl_fun(train_data2, 'Age_grp')

## Plot bar chart
# Plot Sex bar chart
ggplot(sumtabl_Sex, aes(Sex, Survival_rate)) +  geom_bar(aes(fill = Sex), position = "dodge", stat="identity")
print(sumtabl_Sex)

# Plot PClass bar chart
ggplot(sumtabl_Pclass, aes(Pclass, Survival_rate)) +  geom_bar(aes(fill = Pclass), position = "dodge", stat="identity")
print(sumtabl_Pclass)

# Plot SibSp bar chart
ggplot(sumtabl_SibSp, aes(SibSp, Survival_rate)) +  geom_bar(aes(fill = SibSp), position = "dodge", stat="identity")
print(sumtabl_SibSp)

# Plot Parch bar chart
ggplot(sumtabl_Parch, aes(Parch, Survival_rate)) +  geom_bar(aes(fill = Parch), position = "dodge", stat="identity")
print(sumtabl_Parch)

# Plot Embarked bar chart
ggplot(sumtabl_Embarked, aes(Embarked, Survival_rate)) +  geom_bar(aes(fill = Embarked), position = "dodge", stat="identity")

# Plot Age group chart
ggplot(sumtabl_Agegrp, aes(Age_grp, Survival_rate)) + geom_bar(aes(fill = Survival_rate), position = "dodge", stat="identity")

# Plot Age distribution compare between death and survived
ggplot(train_data, aes(Age, fill = as.factor(Survived))) + geom_histogram(alpha = 0.5, position = 'identity')

# Plot Sibling distribution compared between death and survived
ggplot(train_data, aes(SibSp, fill = as.factor(Survived))) + geom_histogram(alpha = 0.5, position = 'identity')

# Plot Parch distribution compared between death or survived
ggplot(train_data, aes(Parch, fill = as.factor(Survived))) + geom_histogram(alpha = 0.5, position = 'identity')

# Plot Family size distribution compared between death and survivied
ggplot(train_data2, aes(Fam, fill = as.factor(Survived))) + geom_histogram(alpha = 0.5, position = 'identity')

# 04 Data manipulation for data preparation
## Create new data frame called "train_db" for modelling part
train_db <- train_data2 %>% 
  mutate(trn_Sex   = ifelse(Sex == 'male', 1, 0),
         trn_Alone = ifelse((SibSp + Parch) == 0, 1, 0),
         trn_Embar = case_when(
                        .$Embarked == "C" ~ 1,
                        .$Embarked == "S" ~ 2,
                        .$Embarked == "Q" ~ 3,
                        TRUE ~ 0 ),
         trn_Pclass = Pclass,
         trn_child  = ifelse(Age_grp == "01 Child", 1, 0)) %>% 
  select(starts_with("trn"), Survived)

## Create new data frame called "test_db" for validation part 
test_db <- test_data %>% 
  mutate(Fam = Parch + SibSp, 
         Age_grp = case_when( is.na(.$Age) ~ "00 Error",
                              .$Age > 0  & .$Age <= 12 ~ "01 Child",
                              .$Age > 12 & .$Age <= 20  ~ "02 Teenager",
                              .$Age > 20 & .$Age <= 50  ~ "03 Adult",
                              .$Age > 50 ~ "04 Elder")) %>% 

  mutate(trn_Sex   = ifelse(Sex == 'male', 1, 0),
         trn_Alone = ifelse((SibSp + Parch) == 0, 1, 0),
         trn_Embar = case_when(
                        .$Embarked == "C" ~ 1,
                        .$Embarked == "S" ~ 2,
                        .$Embarked == "Q" ~ 3,
                        TRUE ~ 0 ),
         trn_Pclass = Pclass,
         trn_child  = ifelse(Age_grp == "01 Child", 1, 0)) %>% 
  select(starts_with("trn"))

# 05 MODELLING PART
survived  <- train_db$Survived
passenger <- test_data$PassengerId

## Using GLM logit regression
model_glm1 <- glm(Survived ~.,family = binomial(link='logit'), data=train_db)
model_glm2 <- glm(Survived ~ trn_Sex + trn_Pclass + trn_child, family = binomial(link='logit'), data=train_db)

predict_glm <- predict(model_glm2, test_db)

# Score 0.77033

## Using kkn
train_db2 <- train_db %>% 
  select(-Survived)

result_knn <- knn(train_db2, test_db, survived, k = 5, l = 0, prob = TRUE, use.all = TRUE)
submit.knn <- data.frame(PassengerId = passenger, Survived = result_knn) 

# Score 0.78468

## Using Decision tree
model_tree <- rpart(Survived ~., data = train_db)

fitted.results_tree <- data.frame(prop  = predict(model_tree, test_db))
result_tree <-  fitted.results_tree %>% 
  mutate(Survived = ifelse(prop > 0.5, 1, 0)) %>% 
  select(Survived)

rpart.plot(model_tree)

submit.tree <- data.frame(PassengerId = passenger, Survived = result_tree)

## Using Random Forest
model_ranforest <- randomForest(Survived ~., data = train_db, importance = TRUE )

fitted.results_ranforest <- data.frame(prop  =  predict(model_ranforest, test_db, type = "class"))

result_ranforest <- fitted.results_ranforest %>% 
  mutate(Survived = ifelse(prop > 0.5, 1, 0)) %>% 
  select(Survived)

submit.tree <- data.frame(PassengerId = passenger, Survived = result_ranforest)

#Score 0.78947
