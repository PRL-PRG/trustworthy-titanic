
---
title: "Titanic: Machine Learning from Disaster"
author: Matthias Westhues
date: "`r format(Sys.time(), '%d %B, %Y')`"
output:
  html_notebook:
    number_sections: true
    toc: true
    fig_width: 7
    fig_height: 4.5
    theme: readable
    highlight: tango
---

# Competition Description
> The sinking of the RMS Titanic is one of the most infamous shipwrecks in 
> history.
> On April 15, 1912, during her maiden voyage, the Titanic sank after colliding 
> with an iceberg, killing 1502 out of 2224 passengers and crew.
> This sensational tragedy shocked the international community and led to better 
> safety regulations for ships.

> One of the reasons that the shipwreck led to such loss of life was that there 
> were not enough lifeboats for the passengers and crew.
> Although there was some element of luck involved in surviving the sinking,
> some groups of people were more likely to survive than others, such as women,
> children, and the upper-class.

> In this challenge, we ask you to complete the analysis of what sorts of people 
> were likely to survive. In particular, we ask you to apply the tools of machine 
> learning to predict which passengers survived the tragedy.

[Source](https://www.kaggle.com/c/titanic)


## Data Dictionary
| Variable | Description                                |
| -------- | -----------                                |
| survival | Survival  (0 = No, 1 = Yes)                |
| pclass   | Ticket class (1 = 1st, 2 = 2nd, 3 = 3rd)   |
| sex      | Sex                                        |
| Age      | Age in years                               |
| sibsp    | # of siblings / spouses aboard the titanic | 
| parch    | # of parents / children aboard the titanic |
| ticket   | Ticket number                              |
| fare     | Passenger fare                             |
| embarked | Port of Embarkation                        |





## Load and check data
```{r Load_Data, message = FALSE}
# Load packages
if (!require("pacman")) install.packages("pacman")
## Package overview
# dplyr: data manipulation
# purrr: functional programming
# readr: read data
# stringr: string manipulation
# ggplot2: visualizations
# tibble: alternative data frames
# forcats: factor manipulation
# mice: chained missing value imputation
# tidyr: data reshaping/tidying
# caret: machine learning wrapper
# DMwR: SMOTE subsampling during resampling
# kernlab: Support Vector Machines
# pamr: Nearest Shrunken Centroids
# nnet: Neural Networks
# xgboost: Extreme Gradient Boosting

# Install a 'dplyr' version that is more recent than 0.50 because the function
# case_when() did not comply with mutate() in this and earlier versions.
pacman::p_load_gh("tidyverse/dplyr")
pacman::p_load(
  "purrr", "readr", "stringr", "ggplot2", "tibble", "forcats", "mice", "tidyr", 
  "caret", "DMwR", "kernlab", "pamr", "nnet", "xgboost"
)

# Training data
train_dat <- "../input/train.csv" %>% 
  read_csv()
# Test data
test_dat <- "../input/test.csv" %>% 
  read_csv()
# For feature pre-processing, merge the training and the test data sets.
full <- list(
  Training = train_dat,
  Test = test_dat
  ) %>% 
  bind_rows(.id = "Data_Set")
```


First, we'll have a look at the structure of the data.
```{r}
full %>% 
  summary()
```







# Names and titles
The 'Name' variable contains not only the names of passengers, but also their
respective titles.
This might provide useful information on top of knowing the passengers' gender
and their ticket price.

```{r}
full %>% 
  select(Name) %>% 
  flatten_chr() %>% 
  head(n = 10L)
```

Let's extract these titles.
Here, it seems as if the last name is separated from the title by a `,`, which
is in turn succeded by a `.`.

```{r}
# Function for extracting the title from the 'Name' variable.
extract_title <- function(x) {
  passenger_pos <- regexpr(
    "(?<=,\\s)[[:word:]]+(?=\\.)", text = x, perl = TRUE
  )
  titles <- substring(
    x, 
    first = passenger_pos,
    last = passenger_pos + attr(passenger_pos, "match.length") - 1
  )
  titles
}

# Extract the title.
full <- full %>% 
  mutate(Title = extract_title(Name))

# Look at the unique titles.
full %>% 
  select(Title) %>% 
  unique() %>% 
  flatten_chr()
```

At least one person doesn't have a title.
Let's see what's going on there.

```{r}
full %>% 
  filter(nchar(Title) == 0) %>% 
  select(Name) %>% 
  flatten_chr()
```

Ok, so this passenger actually has a title, which was not captured by the 
regular expression.
Let's just record her as "Countess" manually, here.

```{r}
full <- full %>% 
  mutate(Title = if_else(
    nchar(Title) == 0,
    true = "Countess", 
    false = Title
  ))

full %>% 
  select(Title) %>% 
  flatten_chr() %>% 
  unique()
```


Let's see, if the titles also have some information on the age of the passengers.
```{r}
full %>% 
  ggplot(aes(x = Title, y = Age)) +
  geom_boxplot() +
  facet_wrap(~ Sex, scales = "free") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
```

It looks like all male children were titled 'Master'. 

```{r}
full %>% 
  filter(Title == "Master") %>% 
  select(Age) %>% 
  flatten_dbl() %>% 
  summary()
```

I'll impute the age of the `r full %>% filter(Title == "Master") %>% select(Age) %>% flatten_dbl() %>% is.na() %>% sum()` NAs of passengers with the title 'Master' as 
the arithmetic mean of this group.
```{r}
impute_age_as_average <- function(x) {
  x %>% 
   mutate(Age = if_else(
     is.na(Age),
     true = mean(Age, na.rm = TRUE),
     false = Age
   ))
}

full <- full %>% 
  split(.$Title) %>% 
  map_at("Master", .f = ~impute_age_as_average(.)) %>% 
  bind_rows()
```


Some of the titles have the same meaning, such as "Miss" and "Ms", however, 
titles such as "Lady" might hint at their social status.
Here, we'll aggregate some titles.
```{r}
full <- full %>% 
  mutate(Title = fct_collapse(Title,
    Mrs = c("Mrs", "Mme"),
    Miss = c("Miss", "Ms"),
    Upperclass = c(
      "Don", "Dona", "Rev", "Dr", "Major", "Lady", "Sir", "Mlle", "Col", 
      "Capt", "Countess", "Jonkheer"
      ),
    Clerical = "Rev"
  ))
```


Some of the passengers had the title "Rev", hence I've caterogized them as 'Clerical'.
Were members of the clerus more likely to help other's and refrain from taking a spot in any of the life boats?
```{r}
full %>% 
  filter(Title == "Clerical") %>% 
  select(Survived) %>% 
  flatten_int() %>% 
  summary()
```

Indeed, none of the reverends survived.



# Fare
Is the fare different for children and for adults?
```{r}
categorize_age <- Vectorize(
  function(x) {
    if (x <= 6) {
      "Young_Child"
    } else if (6 < x && x <= 16) {
      "Kid"
    }
  }
)
full %>% 
  mutate(Age_Category = case_when(
    .$Age <= 6 ~ "Young_Child",
    .$Age <= 16 ~ "Kid",
    .$Age > 16 ~ "Adult",
    is.na(.$Age) ~ "Missing"
  )) %>% 
  ggplot(aes(x = Age_Category, y = Fare)) +
  geom_boxplot() +
  facet_wrap(~ Pclass)
```

Surprisingly, the ticket fares for children were not lower; actually they were
rather higher than for adults.
Hence, we should not use information on the ticket price for imputing the age
of other passengers.



# Families
We would like to know whether single passengers have a higher chance of 
surviving than families.
```{r}
# Assign passengers to either "Single" (i.e., travelling by themselves),
# "Small" families or "Large" families.
full <- full %>% 
  mutate(
    Surname = gsub(",.*$", replacement = "", x = Name),
    Family_Size = SibSp + Parch + 1
  ) %>% 
  mutate(FamSizeD = case_when(
    .$Family_Size > 4 ~ "Large",
    .$Family_Size == 1 ~ "Single",
    .$Family_Size <= 4 ~ "Small"
  ))

full %>% 
  filter(Data_Set == "Training") %>% 
  mutate(Survived = Survived %>% as.character()) %>% 
  rename(`Discrete Family Size` = FamSizeD) %>% 
  ggplot(aes(x = `Discrete Family Size`, fill = Survived)) +
  geom_bar(position = "fill")
```

Members of large families had the lowest survival rate, followed by singles
and members of small families, which had the highest chance of surviving the 
disaster.




# Embarkation
Did the location of embarkation have an influence on the chance for survival?
```{r}
full %>% 
  filter(Data_Set == "Training") %>% 
  mutate(Survived = Survived %>% as.character()) %>% 
  ggplot(aes(x = Embarked, fill = Survived)) +
  geom_bar(position = "fill")
```

It looks as if passengers from Cherbourg had a higher chance of survival.
But why should the point of their embarkation in itself have any influence on 
survival?
Perhaps, the fraction of 1st class passengers among people who embarked in
Cherbourg is higher compared to passengers from the other two cities.

```{r}
full %>% 
  mutate(Pclass = Pclass %>% as.character()) %>% 
  ggplot(aes(x = Embarked, fill = Pclass)) +
  geom_bar(position = "fill")
```

Indeed, the fraction of first class passengers is highest among people who 
embarked the Titanic in Cherbourg.
Our null hypothesis is that the location of embarkation and the passenger class
are independent variables.
We'll test this assumption with a $\chi^{2}$-test.

```{r}
full %>% 
  select(Embarked, Pclass) %>% 
  table() %>% 
  chisq.test()
```

The $\chi^{2}$-test confirms what was indicated by the bar plot: The location
of embarkation and the passenger class do not seem to be independent variables.
We can probably omit the `Embarked` variable from the prediction models.



# Cabins/Decks
Further, we would like to know whether there is any pattern in the cabin IDs.
After all, a lower-deck cabin should be more likely to have people in a sinking 
ship trapped.

```{r}
# Get a glimpse of the general pattern of cabin names.
full %>% 
  select(Cabin) %>% 
  flatten_chr() %>% 
  discard(is.na(.)) %>% 
  head(n = 10L)
```

So every cabin is initiated by a single letter, which might be indicative for
the level of the cabin inside the ship.
Let's extract this letter.
```{r}
extract_letter <- function(x) {
  x %>% 
    stringr::str_match(., pattern = "[A-Z]") %>% 
    c()
}

full <- full %>% 
  mutate(CabinD = if_else(
    !is.na(Cabin),
    true = extract_letter(Cabin),
    false = Cabin
  ))

full %>% 
  ggplot(aes(x = Pclass, fill = CabinD)) +
  geom_bar(position = "stack")
```

Interesting; the majority of passengers from the first class has been assigned
to cabins with valid numbers, whereas most passengers from the second and the
third class do not have a record of their cabin.

Let's further summarize by only looking at the distribution of existing
cabins among passenger classes.
```{r}
full %>% 
  filter(!is.na(Cabin)) %>% 
  ggplot(aes(x = Pclass, fill = CabinD)) +
  geom_bar(position = "stack")
```

Cabins with the letters A to C seem to have belonged almost exclusively to
first class passengers.
At this point, my hypothesis would be that these classes comprised the three 
uppermost levels of the Titanic, which should boost the passengers' chance of 
survival.
So we should definitely model this variable!
We should also have a look at the number of passengers in any cabin category
and ensure that any category does not comprise a very low number of passengers,
which would be useless for our predictions.

```{r}
full %>% 
  select(CabinD, Title) %>% 
  table() %>% 
  addmargins()
```

Clearly, categories "G" and "T", with 5 and 1 passenger, respectively, contain
far too little information to be useful later on.
Based on the previous plot and frequency table, we could see that the majority
of identified cabins were taken by first class passengers.
This can be further corroborated by looking at the following frequency table
for the variables `CabinD` and `Pclass`:

```{r}
full %>% 
  select(CabinD, Pclass) %>% 
  table() %>% 
  addmargins()
```

Obviously, `CabinD` is only useful for assisting the prediction of survival for
passengers of the first class.
Therefore, I will assign all passengers from the second and the third class, as 
well as the single member of cabin category "T" to a new cabin class "unknown".

```{r}
full <- full %>% 
  mutate(CabinD = case_when(
    CabinD == "T" ~ "unknown",
    Pclass %in% c(2, 3) ~ "unknown",
    is.na(CabinD) ~ "unknown",
    Pclass == 1 ~ CabinD
  ))

# Check out the new levels.
full %>% 
  select(CabinD, Pclass) %>% 
  table(useNA = "ifany") %>% 
  addmargins()
```


We could further have a look at the range of ticket prices for each cabin 
category:

```{r}
full %>% 
  ggplot(aes(x = CabinD, y = Fare, fill = CabinD)) +
  geom_boxplot()
```

The ticket prices for cabin category "A" somewhat defy my hypothesis.
This might be a special case, though.
However, the prices in categories "B" and "C", which are made up largely by 
first class passengers, are in agreement with my hunch and we also see 
that prices decrease from category "C" onwards.
The ticket prices for unclassified cabins are also considerably lower than 
most other categories.
However, the fares for some unclassified cabins were fairly high, which 
suggests that something simply went wrong when recording the passengers.


## Tickets
Now, let's have a look at the tickets, which might add information on the 
cabin.

```{r}
full %>% 
  select(Ticket) %>% 
  flatten_chr() %>% 
  head(n = 10L)
```

Some tickets are only comprised of digits whereas some tickets are a combination
of digits and letters.
Maybe this is informative of something more.

```{r}
not_na <- compose(`!`, is.na)
full <- full %>% 
  mutate(Ticket = if_else(
    Ticket %>% extract_letter() %>% not_na(),
    true = "Alphanumeric",
    false = "Numeric"
  ))
```


```{r}
full %>% 
  ggplot(aes(x = CabinD, fill = Ticket)) +
  geom_bar(position = "fill")
```

```{r}
full %>% 
  ggplot(aes(x = Pclass, fill = Ticket)) +
  geom_bar(position = "fill")
```

There does not seem to be an obvious association between what's printed on 
the ticket and in which cabin category or passenger class a person traveled.
Perhaps though, there is an underlying pattern so let's keep the recoded
'Ticket' variable.









# Fare adjustment
```{r}
# Look at Tukey's five nubmer summary of the ticket prices.
full %>% 
  select(Fare) %>% 
  flatten_dbl() %>% 
  fivenum()
```

This looks as if some people were free-riding the Titanic.
Perhaps, some guests were invited to ride along with other passengers.
The largest ticket price is `r train_dat %>% select(Fare) %>% flatten_dbl() %>% max()`.
This looks plausible.
The most expensive cabins, given that there were probably very few extremely
luxurious suites.
Next, let's check the distribution of ticket fares:

```{r}
full %>% 
  ggplot(aes(x = Fare)) +
  geom_histogram()
```

This looks like a power distribution, indicating that the majority of passengers 
paid a (relatively) small amount of money for tickets, whereas a few passengers 
paid a rather large sum.
Apply a BoxCox transformation to the `Fare` variable to normalize the 
distribution.

```{r}
transform_boxcox <- function(x) {
  x %>% 
    BoxCoxTrans() %>% 
    predict(., x)
}

# Prior to the BoxCox-transformation, ensure that all values are striclty 
# positive.
full <- full %>% 
  mutate(Pos_Fare = if_else(
    Fare <= 0 | is.na(Fare),
    true = 1,
    false = Fare
  )) %>% 
  mutate(BoxCoxFare = transform_boxcox(Pos_Fare))

full %>% 
  ggplot(aes(x = BoxCoxFare)) +
  geom_density() +
  ylab("Density")
```

This looks a bit better and will (hopefully) suffice for modeling.





# Develop a treatment plan
For our predictions, we need to ensure that our predictors do not contain 
missing values.
Hence, we need to impute them in a meaningful way.

```{r}
# Compute the average level of missing data for each predictor.
compute_na_fraction <- compose(mean, is.na)
full %>% 
  select(one_of(c(
    "Pclass", "BoxCoxFare", "Title", "Age", "FamSizeD", "CabinD", "Ticket"
  ))) %>% 
  summarize_all(.funs = funs(compute_na_fraction))
```

We have a very large number of missing values in the predictor `Age`, which 
might be informative for survival because i) children might have had priority
for lifeboat spots and ii) older people could be more frail/less agile.
escaping their rooms and to search for a lifeboat.
Hence, we should try to impute this variable, but also add a new variable, which
indicates which values were imputed so that this information is also available
to the prediction model.

```{r}
# Add a new variable `AgeIsBad`, which indicates whether, for a particular 
# passenger, the `Age` variable is uninformative ("yes") or informative "no").
full <- full %>% 
  mutate(AgeIsBad = if_else(
    is.na(Age),
    true = "yes",
    false = "no"
  )) %>% 
  mutate(AgeIsBad = AgeIsBad %>% as.factor())

var_names <- c(
  "Pclass", "BoxCoxFare", "Title", "Age", "Sex", "FamSizeD", "CabinD", 
  "Ticket", "AgeIsBad"
)

# Impute the missing age values using Bayesian linear regression 
# (method = "norm") and 100 rounds of multiple imputations.
mice_age <- full %>% 
  select(one_of(var_names)) %>% 
  mice(., method = "norm", m = 100, print = FALSE) %>% 
  mice::complete() %>% 
  select(Age) %>% 
  flatten_dbl()

# Convert these variables to factors.
factor_vars <- c(
  "Survived", "Pclass", "Ticket", "FamSizeD", "CabinD"
)

# Chained function for save conversion from any format to factors.
convert_to_factor <- compose(as.factor, as.character)

# Add the imputed ages to the data frame.
full <- full %>% 
  mutate(Imputed_Age = mice_age) %>% 
  mutate_at(.vars = factor_vars, .funs = convert_to_factor)

# Check whether the imputation of the `Age` variable produced sensible results.
full %>%
  select(Age, Imputed_Age, PassengerId) %>% 
  gather(key = State, value = Age, -PassengerId) %>% 
  mutate(State = State %>% as.factor()) %>% 
  mutate(State = fct_recode(State,
    Original = "Age",
    Imputed = "Imputed_Age"
    )) %>% 
  ggplot(aes(x = Age)) +
  geom_histogram() +
  facet_wrap(~ State)
```

It looks as if the "mice" function did a good job on the imputation of missing 
"Age" values when comparing the distribution of the "Imputed" values with the 
distribution of the "Original" values.





# Final fixes
Prior to modeling the data, we need to convert any factors, that are encoded
by digits, to strings.
Moreover, scale and center the `Imputed_Age` variable.
```{r}
full <- full %>% 
  mutate(
    Survived = fct_recode(
      Survived,
      yes = "1",
      no = "0"
    ),
    Pclass = fct_recode(
      Pclass,
      First = "1",
      Second = "2",
      Third = "3"
    ),
    Imputed_Age = scale(Imputed_Age, center = TRUE, scale = TRUE),
    Survived = Survived %>% factor(., levels = c("yes", "no"))
  )
```




# Cross-validation
Next in line is cross-validation on the training set to minimize differences in
model performance between training and test sets.
First, we'll recover the training set data:
```{r}
# Use only the following variables in the prediction models.
pred_vars <- c(
  "Survived", "Pclass", "Imputed_Age", "Ticket", "Title", "FamSizeD", "CabinD",
  "BoxCoxFare", "AgeIsBad"
)
# Extract the training data after the previous feature engineering.
eng_train <- full %>%
  filter(Data_Set == "Training") %>% 
  select(one_of(pred_vars))
```


## Logistic regression
```{r Logistic_Regression, cache = TRUE}
# Next, we'll create balanced splits of the data based on the outcome.
train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  classProbs = TRUE,
  sampling = "smote",
  summaryFunction = twoClassSummary
)

set.seed(340934)
log_res <- train(
  Survived ~ .,
  data = eng_train,
  method = "glm",
  metric = "ROC",
  trControl = train_control,
  trace = FALSE
)
```


## Support Vector Machines
```{r SVM, cache = TRUE}
# Hyper parameter estimation for the Gaussian Radial Basis Kernel
# Note that 'frac' denotes the fraction of data to be used for estimation.
# Here, we set it to a value of '1' because were are working on the training
set.seed(340934)
sig_dist <- sigest(
  Survived ~ .,
  data = eng_train,
  frac = 1
)
svm_tune_grid <- data.frame(
  sigma = as.vector(sig_dist)[1],
  C = 2^(-2:7)
)

set.seed(340934)
svm_res <- train(
  Survived ~ .,
  data = eng_train,
  method = "svmRadial",
  tuneGrid = svm_tune_grid,
  metric = "ROC",
  trControl = train_control,
  trace = FALSE
)
```


## Nearest Shrunken Centroids
```{r NSC, cache=TRUE}
nsc_grid <- data.frame(.threshold = seq(from = 0, to = 25, by = 1))
set.seed(340934)
nsc_res <- train(
  Survived ~ .,
  data = eng_train,
  method = "pam",
  tuneGrid = nsc_grid,
  metric = "ROC",
  trControl = train_control
)
```


## Neural Networks
```{r NN, cache = TRUE}
nnet_grid <- expand.grid(
  .size = seq_len(10),
  .decay = c(0, 0.1, 1, 2)
)
max_size <- max(nnet_grid$.size)
num_wts <- 1 * (max_size * (nrow(eng_train) + 1) + max_size + 1)
set.seed(340934)
nnet_res <- train(
  Survived ~ .,
  data = eng_train,
  method = "nnet",
  metric = "ROC",
  tuneGrid = nnet_grid,
  maxit = 2000,
  MaxNWts = num_wts,
  trControl = train_control,
  trace = FALSE
)
```


## K-Nearest Neighbors
```{r KNN, cache = TRUE}
knn_grid <- data.frame(
  .k = c(4 * (0:5) + 1,
         20 * (1:5) + 1,
         50 * (2:9) + 1)
)
set.seed(340934)
knn_res <- train(
  Survived ~ .,
  data = eng_train,
  method = "knn",
  metric = "ROC",
  tuneGrid = knn_grid,
  trControl = train_control
)
```


## Extreme Gradient Boosting
```{r XGBoost, cache = TRUE}
# Set up the cross-validated hyper-parameter search
# grid parameters taken from http://stats.stackexchange.com/a/263649
xgb_grid <- expand.grid(
  nrounds = 1e3,
  eta = 0.05,
  max_depth = c(2L, 10L),
  gamma = c(0, 10),
  colsample_bytree = c(0.1, 0.4),
  min_child_weight = c(1L, 10L),
  subsample = c(0.5, 1)
)
set.seed(340934)
xgb_res <- train(
  Survived ~ .,
  data = eng_train,
  method = "xgbTree",
  metric = "ROC",
  tuneGrid = xgb_grid,
  trControl = train_control,
  trace = FALSE
)
```


# Model comparisons
```{r Model_Comparison, cache = TRUE}
resampled_res <- list(
  Logistic = log_res,
  SVM = svm_res,
  NSC = nsc_res,
  NeuralNet = nnet_res,
  KNN = knn_res,
  XGBoost = xgb_res
) %>% 
  resamples() %>% 
  .[["values"]] %>% 
  as_data_frame()

# Function for transforming untidy model output from caret's train() and
# resamples() functions into the tidy long format.
tidy_predictions <- function(x) {
  # Extract model names
  algos <- x %>% 
    select(-matches("Resample")) %>% 
    colnames() %>% 
    str_extract(pattern = "[^~]+") %>% 
    unique()
  
  # Function to transform results for a single algorithm from wide to long
  # format.
  tidy_single_model <- function(algo, x) {
    algo_resample <- paste(algo, "Resample", sep = "|")
    x %>% 
      select(matches(algo_resample)) %>% 
      setNames(gsub(
        pattern = "[[:word:]]+~",
        replacement = "",
        perl = TRUE,
        x = names(.)
      )) %>% 
      gather(key = Statistic, value = Value, -Resample) %>% 
      mutate(Algorithm = algo)
  }
  
  # Transform results from each algorithm from wide to long format and 
  # aggregate all model outputs in a single data frame.
  algos %>% 
    as.list() %>% 
    map(., .f = tidy_single_model, x = x) %>% 
    bind_rows()
}


resampled_res %>% 
  tidy_predictions() %>% 
  mutate(
    Algorithm = Algorithm %>% as.factor()
  ) %>% 
  ggplot(aes(
    x = fct_reorder(f = Algorithm, x = Value, fun = median),
    fill = Algorithm, y = Value
  )) +
  geom_boxplot() +
  facet_wrap(~ Statistic, scales = "free_y") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = "top"
  )
```

We can see that extreme gradient boosting "XGBoost" is the clear winner in 
terms of predicting the chance of survival.

What is the importance of the variables that we provided to the extreme 
gradient boosting algorithm?
```{r}
xgb_res %>% 
  varImp(scale = TRUE) %>% 
  .[["importance"]]
```

If your title was "Mr" you had a particular high chance **not** to survive the 
sinking of the Titanic.
The same is true for passengers without a known cabin.
Females and passengers from the upper levels (cabins E, D and C) had a high chance
of survival and most children made it.



# Prediction
Now, we will apply this algorithm to the test data.
```{r}
# Extract the test set after featuer engineering.
eng_test <- full %>% 
  filter(Data_Set == "Test") %>% 
  select(one_of(c(pred_vars, "PassengerId")))

# Predict using the test set.
# Convert factors back to integer for compliance with the original data.
#prediction <- 
prediction <- predict(object = xgb_res, eng_test) %>% 
  fct_recode(., "0" = "no", "1" = "yes") %>% 
  as.character() %>% 
  as.integer()

# Save the solution to a data frame with two columns: PassengerId and Survived.
solution <- data.frame(
  PassengerId = eng_test %>% select(PassengerId) %>% flatten_int(),
  Survived = prediction
)

# Write the solution to a file.
write_csv(solution, path = "titanic_xgb_mod_solution.csv")
```


# Acknowledgments
Many thanks to [Megan L. Risdal](https://www.kaggle.com/mrisdal) for inspiring 
a couple of aspects in this analysis, particularly with respect to missing 
value imputation using the `mice` package.
You should definitely check out her very popular [kernel](https://www.kaggle.com/mrisdal/titanic/exploring-survival-on-the-titanic/notebook).
