## ----libraries, message=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load all the project packages
library("tidyverse")
library("forcats")
library("lubridate")
library("stringr")
library("ggpubr")
library("ggmosaic")
library("broom")
library("caret")
library("ranger")
library("parallel")
library("GGally")
library("doParallel")
library("mice")
library("randomForest")
library("e1071")


## ----read_data, message=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dataset <- 
  read_csv("../input/train.csv") %>%
  mutate(set = "train")

dataset <- 
  read_csv("../input/test.csv") %>%
  mutate(set = "test",
         Survived = NA) %>%
  bind_rows(dataset)


## ----check-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glimpse(dataset)


## ----rename_columns--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dataset <- dataset %>%
  select(passenger_id    = PassengerId, # passenger identifier in the dataset
         name            = Name,        # name of the passenger
         sex             = Sex,         # sexe
         age             = Age,         # age
         family_horiz    = SibSp,       # number of family members on the same level (siblings or spouse)
         family_vert     = Parch,       # number of family members a level away (children or parents)
         cabin_number    = Cabin,       # cabin number
         ticket_number   = Ticket,      # ticket number 
         ticket_class    = Pclass,      # is the ticket 1st, 2nd or 3rd class
         ticket_fare     = Fare,        # price of the ticket  
         embark_port     = Embarked,    # embarkment port
         survived        = Survived,    # survival - output targer
         set)                         # is the data from training or test

glimpse(dataset) # glimpse again to see the changes


## ----factoring-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dataset <- dataset %>%
  mutate(sex = factor(sex),
         ticket_class = factor(ticket_class),
         survived = factor(survived),
         set = factor(set))


## ----missings--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
map_df(dataset, function(x) {sum(is.na(x))}) %>% 
  gather(variable, missing) %>%
  arrange(desc(missing))


## ----ticket_fare-missing---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  filter(is.na(ticket_fare))


## ----ticket_fare-impute----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dataset$ticket_fare[is.na(dataset$ticket_fare)] <- 
  dataset %>%
  filter(ticket_class == 3,
         embark_port == "S") %>% 
  pull(ticket_fare) %>%
  median(na.rm = TRUE)


## ----embark_port-missing---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  filter(is.na(embark_port)) %>% 
  select(-family_horiz, -family_vert, -survived)


## ----embarkment_port-impute------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggboxplot(dataset, x = "embark_port", y = "ticket_fare", 
          fill = "ticket_class") +
  geom_hline(aes(yintercept = 80), colour = "red")


## ----embark_port-replace---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# replace with S
dataset <- dataset %>%
  replace_na(list(embark_port = 'C')) %>%
  mutate(embark_port = factor(embark_port))


## ----deck------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dataset <- dataset %>%
  mutate(deck = str_sub(cabin_number, 1, 1))

dataset %>%
  filter(set == "train") %>%
  ggplot(aes(x = deck, fill = survived, label = ..count..)) +
    geom_bar(stat = "count", position = "fill") +
    geom_text(stat = "count", position = position_fill(vjust = 0.5))


## ----cabin-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dataset <- dataset %>%
  mutate(cabin_known = !is.na(cabin_number))

dataset %>%
  filter(set == "train") %>%
  ggplot(aes(x = cabin_known, fill = survived, label = ..count..)) +
    geom_bar(stat = "count", position = "fill") +
    geom_text(stat = "count", position = position_fill(vjust = 0.5))


## ----names-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dataset <- dataset %>%
  mutate(last_name    = str_extract(name, "^.+,") %>%
                        str_extract("[A-Z][A-Z 'a-z]*"),
         title        = str_extract(name, ", .+[.]") %>%
                        str_extract("[A-Z][A-Z 'a-z]*"),
         surname      = ifelse(!is.na(str_extract(name, "\\(\".*\"\\)")),
                               "formal",
                               ifelse(!is.na(str_extract(name, "\".*\"")),
                                      "informal",
                                      NA)))

dataset %>%
  filter(set == "train") %>%
  ggplot(aes(x = title, fill = survived, label = ..count..)) +
    geom_bar(stat = "count", position = "fill") +
    geom_text(stat = "count", position = position_fill(vjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ----category, message=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggviolin(data = dataset, x = "title", y = "age", fill = "title",
         add = "boxplot", add.params = list(fill = "white")) +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))

dataset <- dataset %>% 
  mutate(category = as_factor(title) %>%
         # we add the foreign and variant to there group
           fct_collapse(mrs = c("Mrs", "Mme", "Ms"),
                        miss = c("Miss", "Mlle")) %>%
         # we keep only the four more frequent group and lump the rest together
           fct_lump(n = 4)) 

# we take a look again at the ages
ggviolin(data = dataset, x = "category", y = "age", fill = "category",
         add = "boxplot", add.params = list(fill = "white"))

# and another look at the survival by category
dataset %>%
  filter(set == "train") %>%
  ggplot(aes(x = category, fill = survived, label = ..count..)) +
    geom_bar(stat = "count", position = "fill") +
    geom_text(stat = "count", position = position_fill(vjust = 0.5))


## ----age-missing-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# the mice package is used to impute the missng ages with a random forest
mice_model <- dataset %>% 
  select(age, 
         sex,
         family_horiz,
         family_vert,
         ticket_class,
         ticket_fare, 
         embark_port,
         set,
         category) %>%
  mice(method = 'rf')

mice_output <- complete(mice_model)


## ----eval_mice, message=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#bind dataset and mice_output and plot the density of age
dataset %>% 
  mutate(set = "original") %>% 
  bind_rows(mice_output %>% 
    mutate(set = "imputed")) %>% 
  mutate(set = factor(set)) %>%
  ggdensity(x = 'age', fill = "set", alpha = 0.5) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dataset$age <- mice_output$age


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
durations <- data.frame(model = as.character(),
                        duration)


## ----split-rf--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- dataset %>%
  filter(set == "train") %>%
  select(-set)

test <- dataset %>%
  filter(set == "test") %>%
  select(-set)


## ----random-forest---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
start_time <- Sys.time()

# initiate the multicore
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

rf_model <- train(survived ~ sex +
                          age +
                          family_horiz +
                          family_vert +
                          ticket_class +
                          ticket_fare +
                          embark_port +
                          cabin_known +
                          category,
               data = train, 
               method = "ranger",
               tuneLength = 10,
	       #tuneGrid = data.frame(mtry = c(2, 4, 8, 12)),
               importance = "impurity",
               trControl = trainControl(
                 method = "repeatedcv", number = 10,
                 allowParallel = TRUE,
                 repeats = 10, verboseIter = TRUE))

# stop multicore
stopCluster(cluster)
registerDoSEQ()

end_time <- Sys.time()

durations <- durations %>%
  rbind(list(model = "random forest", 
          duration = end_time - start_time))

rf_model 


## ---- message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
submission <- read_csv("../input/genderclassmodel.csv")
submission$Survived <- predict(rf_model, test, type = "raw")

write_csv(submission, "submission.csv")

