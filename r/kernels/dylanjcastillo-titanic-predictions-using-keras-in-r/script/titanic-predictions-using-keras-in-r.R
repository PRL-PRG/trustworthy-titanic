### LOAD LIBRARIES & SET INITIAL SETTINGS --------------------------------------
library(tidyverse)
library(keras)
set.seed(123)

### READ & SPLIT DATA ----------------------------------------------------------
train_init_df <- read_csv("../input/train.csv")
test_df <- read_csv("../input/test.csv")

# Split training into train/dev sets
sample_size <- floor(0.80 * nrow(train_init_df))
train_df <- sample_n(train_init_df, sample_size) 
dev_df <- anti_join(train_init_df, train_df) 

### MISSING VALUES & FEATURE ENGINEERING ---------------------------------------
glimpse(train_df)
summary(train_df)

# Check missing values
colMeans(is.na(train_df))
colMeans(is.na(dev_df))
colMeans(is.na(test_df))

# To see if there's rows with very few values
max(rowMeans(is.na(train_df)))
max(rowMeans(is.na(dev_df)))
max(rowMeans(is.na(test_df)))


# Choices for missing values (using only training set, to avoid data leakage)
missing_age <- mean(train_df$Age, na.rm = TRUE)
missing_fare <- mean(train_df$Fare, na.rm = TRUE)
missing_embk <- "S"

# Join datasets for feature engineering and treatment of missing values
full_df <- bind_rows(
  train_df %>% mutate(isTraining = 1, isDev = 0),
  dev_df %>% mutate(isTraining = 1, isDev = 1),
  test_df %>% mutate(isTraining = 0, isDev = 0))

# Treat missing values
full_1_df <- full_df %>% 
  mutate(MissingAge = is.na(Age) * 1,
         Age = coalesce(Age, missing_age),
         Cabin = coalesce(Cabin, "UNK"),
         Embarked = coalesce(Embarked, missing_embk),
         Fare = coalesce(Fare, missing_fare))

colMeans(is.na(full_1_df)) # Check all columns were treated

# Obtain titles from passengers' names
obtainTitles <- function(col) {
  res <- map(str_split(col, pattern = ","), 2)
  res <- unlist(map(str_split(res, pattern = " "), 2))
}

titles <- obtainTitles(train_df$Name)
table(titles) # Some titles have very low counts, group those
low_count_titles <- names(which(table(titles) < 5))

# Replace those low-counts titles with Other
titles_proc <- ifelse(titles %in% low_count_titles, "Other", titles)

# Create dictionary to do replacements on full dataset
titles_dict <- tibble(Title = titles, TitleProc = titles_proc) %>% unique()

# Add processed titles
full_2_df <- full_1_df %>% 
  mutate(Title = obtainTitles(Name)) %>% 
  left_join(titles_dict) %>% 
  mutate(TitleProc = coalesce(TitleProc, "Other"))

# Other features
full_3_df <- full_2_df %>% 
  mutate(CabinFirstLetter = ifelse(Cabin == "UNK", "UNK", substr(Cabin, 1, 1)),
         LettersInTicket = grepl("[A-z]", Ticket) * 1,
         HasParenthesis = grepl("[(|)]", Name) * 1)

### DATA WRANGLING FOR NN INPUT ------------------------------------------------
# Select categorical & binary variables
categorical_binary_df <- full_3_df %>% 
  select(Pclass, Sex, SibSp, Parch, Embarked, MissingAge, TitleProc, 
         HasParenthesis, LettersInTicket, CabinFirstLetter, isTraining, isDev)

categorical_factors <- data.frame(apply(categorical_binary_df, 2, as.factor))
numerical_categorical <- data.frame(data.matrix(categorical_factors)) - 1

# Select categorical variables
categorical_df <- numerical_categorical %>% 
  select(-Sex, -MissingAge, -HasParenthesis, 
         -LettersInTicket, -isTraining, -isDev)

# Select binary variables
binary_df <- numerical_categorical %>% 
  select(Sex, MissingAge, HasParenthesis, LettersInTicket, isTraining, isDev)

# Select & normalize numerical variables
normalizeSeries <- function(series, min_val, max_val){
  (series - min_val) / (max_val - min_val)
}

# Normalize Age & Fare
min_age <- min(full_3_df %>% filter(isTraining == 1 & isDev == 0) %>% pull(Age))
max_age <- max(full_3_df %>% filter(isTraining == 1 & isDev == 0) %>% pull(Age))
min_fare <- min(
  full_3_df %>% filter(isTraining == 1 & isDev == 0) %>% pull(Fare))
max_fare <- max(
  full_3_df %>% filter(isTraining == 1 & isDev == 0) %>% pull(Fare))

numeric_df <- full_3_df %>%
  select(Age, Fare) %>%
  mutate(Age = normalizeSeries(Age, min_age, max_age), 
         Fare = normalizeSeries(Fare, min_fare, max_fare))

# One-hot encoding of categorical variables
oneHotEncodeCategoricals <- function(df){
  output_df <- matrix(numeric(0), nrow = nrow(df), ncol = 1)
  
  for (col in colnames(df)) {
    to_bind <- to_categorical(df %>% pull(col))
    colnames(to_bind) <- paste0(col, 1:ncol(to_bind))
    output_df <- cbind(output_df, to_bind)
  }
  output_df[,-1]
}

cat_one_hot_enc <- oneHotEncodeCategoricals(categorical_df)

# Merge categorical, binary & numerical variables

full_proc_df <- cbind(binary_df, cat_one_hot_enc, numeric_df)

# X matrixes
x_train <- data.matrix(
  full_proc_df %>% 
    filter(isTraining == 1 & isDev == 0) %>% 
    select(-isTraining, -isDev)
  )
x_val <- data.matrix(
  full_proc_df %>% 
    filter(isTraining == 1 & isDev == 1) %>% 
    select(-isTraining, -isDev)
)
x_test <- data.matrix(
  full_proc_df %>% 
    filter(isTraining == 0) %>% 
    select(-isTraining, -isDev)
)

# Y vector
y_train <- full_3_df %>% filter(isTraining == 1 & isDev == 0) %>% pull(Survived)
y_val <- full_3_df %>% filter(isTraining == 1 & isDev == 1) %>% pull(Survived)

### NN MODEL -------------------------------------------------------------------
model <- keras_model_sequential() %>% 
  layer_dense(units = 32, activation = "relu", input_shape = c(43)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = optimizer_adam(),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train,
  y_train,
  epochs = 50,
  validation_data = list(x_val, y_val)
)

# Generate predictions 
y_test <- model %>% predict_classes(x_test)
predictions <- data.frame(PassengerId = as.integer(test_df$PassengerId),
                          Survived = as.integer(y_test))

# Write predictions to file
write_csv(predictions, "predictions_DC.csv")