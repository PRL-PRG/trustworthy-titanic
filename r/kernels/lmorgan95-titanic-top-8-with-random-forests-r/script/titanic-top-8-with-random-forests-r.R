## ----message = FALSE, warning = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(readr) # import
library(dplyr) # data manipulation
library(ggplot2) # visualizations
library(gridExtra) # visualizations
library(tictoc) # timing models
library(caret) # tuning & cross-validation

theme_set(theme_light())

train_titanic <- read_csv("../input/train.csv") %>%
  mutate(Test_Data = 0)

test_titanic <- read_csv("../input/test.csv") %>%
  mutate(Test_Data = 1)

titanic_full <- bind_rows(train_titanic, test_titanic) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glimpse(titanic_full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missing_vars <- function(x) {
  var <- 0
  missing <- 0
  missing_prop <- 0
  for (i in 1:length(names(x))) {
    var[i] <- names(x)[i]
    missing[i] <- sum(is.na(x[, i]))
    missing_prop[i] <- missing[i] / nrow(x)
  }
  (missing_data <- data.frame(var = var, missing = missing, missing_prop = missing_prop) %>% 
      arrange(desc(missing_prop)))
}

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missing_vars(titanic_full)


## ----echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full %>%
  select(Survived, Age, Fare, Pclass, SibSp, Parch) %>%
  cor(use="pairwise.complete.obs") %>%
  corrplot::corrplot.mixed(upper = "circle", tl.col = "black")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(titanic_full$PassengerId)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(train_titanic$Survived))


## ----echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train_titanic, aes(x = factor(Survived), fill = factor(Survived))) + 
  geom_bar() + 
  scale_y_continuous(breaks = seq(0, 600, 100)) + 
  labs(x = "Survived", y = "Count") + 
  theme(legend.position = "none") + 
  ggtitle("Train Dataset - Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(titanic_full$Pclass))


## ----echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p1 <- ggplot(titanic_full, aes(x = factor(Pclass))) + 
  geom_bar(fill = "deepskyblue4") + 
  scale_y_continuous(breaks = seq(0, 1000, 100)) +
  labs(x = "Class", y = "Count") + 
  ggtitle("Full Dataset - Class") +
  theme(plot.title = element_text(size = 10, face = "bold"))

p2 <- ggplot(train_titanic, aes(x = factor(Pclass), fill = factor(Survived))) + 
  geom_bar(position = "dodge") + 
  scale_y_continuous(breaks = seq(0, 400, 50)) + 
  scale_fill_discrete(name = "Survived") +
  labs(x = "Class", y = "Count") + 
  ggtitle("Train Dataset - Class (Proportion Survived)") +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        legend.position = "bottom")

grid.arrange(p1, p2, ncol = 2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
length(unique(titanic_full$Name)) < nrow(titanic_full)

titanic_full %>% 
  filter(Name == "Connolly, Miss. Kate" | Name == "Kelly, Mr. James") %>%
  select(PassengerId, Survived, Name, Age, Ticket)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(titanic_full$Sex))


## ----echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g3 <- ggplot(titanic_full, aes(x = factor(Sex))) + 
  geom_bar(fill = "deepskyblue4") + 
  scale_y_continuous(breaks = seq(0, 900, 100)) +
  labs(x = "Sex", y = "Count") + 
  ggtitle("Full Dataset - Sex") +
  theme(plot.title = element_text(size = 10, face = "bold"))

g4 <- ggplot(train_titanic, aes(x = factor(Sex), fill = factor(Survived))) + 
  geom_bar(position = "dodge") + 
  scale_y_continuous(breaks = seq(0, 500, 50)) + 
  scale_fill_discrete(name = "Survived") +
  labs(x = "Sex", y = "Count") + 
  ggtitle("Train Dataset - Sex (Proportion Survived)") +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        legend.position = "bottom")

grid.arrange(g3, g4, ncol = 2)


## ----message = FALSE, warning = FALSE, echo = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g5 <- ggplot(titanic_full, aes(x = Age)) + 
  geom_histogram(fill = "deepskyblue4") + 
  scale_y_continuous(breaks = seq(0, 150, 10)) + 
  labs(x = "Age", y = "Count") + 
  ggtitle("Full Dataset - Age") +
  theme(plot.title = element_text(size = 10, face = "bold"))

g6 <- ggplot(train_titanic, aes(x = Age, fill = factor(Survived))) + 
  geom_histogram() + 
  scale_y_continuous(breaks = seq(0, 150, 10)) + 
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  scale_fill_discrete(name = "Survived") + 
  labs(x = "Age", y = "Count") + 
  ggtitle("Train Dataset - Age (Proportion Survived)") +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        legend.position = "bottom")

grid.arrange(g5, g6, ncol = 2)


## ----message = FALSE, warning = FALSE, echo = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g7 <- ggplot(titanic_full, aes(x = SibSp)) + 
  geom_histogram(fill = "deepskyblue4") + 
  scale_y_continuous(breaks = seq(0, 1000, 100)) + 
  scale_x_continuous(breaks = seq(0, 10)) +
  labs(x = "SibSp", y = "Count") + 
  ggtitle("Full Dataset - SibSp") +
  theme(plot.title = element_text(size = 10, face = "bold"))

g8 <- ggplot(train_titanic, aes(x = SibSp, fill = factor(Survived))) + 
  geom_histogram() + 
  scale_y_continuous(breaks = seq(0, 700, 100)) + 
  scale_x_continuous(breaks = seq(0, 10)) +
  scale_fill_discrete(name = "Survived") + 
  labs(x = "SibSp", y = "Count") + 
  ggtitle("Train Dataset - SibSp (Proportion Survived)") +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        legend.position = "bottom")

grid.arrange(g7, g8, ncol = 2)


## ----message = FALSE, warning = FALSE, echo = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g9 <- ggplot(titanic_full, aes(x = Parch)) + 
  geom_histogram(fill = "deepskyblue4") + 
  scale_y_continuous(breaks = seq(0, 1000, 100)) + 
  scale_x_continuous(breaks = seq(0, 10)) +
  labs(x = "Parch", y = "Count") + 
  ggtitle("Full Dataset - Parch") +
  theme(plot.title = element_text(size = 10, face = "bold"))

g10 <- ggplot(train_titanic, aes(x = Parch, fill = factor(Survived))) + 
  geom_histogram() + 
  scale_y_continuous(breaks = seq(0, 700, 100)) + 
  scale_x_continuous(breaks = seq(0, 10)) +
  scale_fill_discrete(name = "Survived") + 
  labs(x = "Parch", y = "Count") + 
  ggtitle("Train Dataset - Parch (Proportion Survived)") +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        legend.position = "bottom")

grid.arrange(g9, g10, ncol = 2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
length(unique(titanic_full$Ticket))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full %>%
  group_by(Ticket) %>%
  count() %>%
  arrange(desc(n))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full %>%
  group_by(Ticket, Fare) %>%
  count() %>%
  arrange(desc(n))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full %>%
  filter(Ticket == '7534')


## ----warning = FALSE, echo = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_full, aes(x = 1, y = Fare)) + 
  geom_boxplot() + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y=element_blank(), 
        axis.title.y=element_blank()) + 
  scale_y_continuous(breaks = seq(0, 600, 50)) + 
  coord_flip() + 
  ggtitle("Full Dataset - Fare")


## ----warning = FALSE, echo = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train_titanic, aes(x = 1, y = Fare)) + 
  geom_boxplot() + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y=element_blank(), 
        axis.title.y=element_blank()) + 
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50)) + 
  facet_grid(Survived ~ ., labeller = label_both) + 
  coord_flip() + 
  ggtitle("Train Dataset - Fare (Distributions, Survived vs Died)")


## ----warning = FALSE, echo = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train_titanic, aes(x = 1, y = Fare)) + 
  geom_boxplot() + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y=element_blank(), 
        axis.title.y=element_blank()) + 
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 300, 50)) + 
  facet_grid(Survived ~ Pclass, labeller = label_both) + 
  coord_flip() + 
  ggtitle("Train Dataset - Fare, Controlling for Class (Survived vs Died)")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(is.na(titanic_full$Cabin))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(titanic_full$Cabin[!is.na(titanic_full$Cabin)])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(titanic_full$Embarked))

## ----echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g11 <- titanic_full %>%
  filter(!is.na(Embarked)) %>%
ggplot(aes(x = factor(Embarked))) + 
  geom_bar(fill = "deepskyblue4") + 
  scale_y_continuous(breaks = seq(0, 1000, 100)) + 
  labs(x = "Embarked", y = "Count") + 
  ggtitle("Full Dataset - Embarked") +
  theme(plot.title = element_text(size = 10, face = "bold"))


g12 <- train_titanic %>%
  filter(!is.na(Embarked)) %>%
ggplot(aes(x = factor(Embarked), fill = factor(Survived))) + 
  geom_bar(position = "dodge") + 
  scale_y_continuous(breaks = seq(0, 500, 50)) + 
  scale_fill_discrete(name = "Survived") +
  labs(x = "Embarked", y = "Count") + 
  ggtitle("Train Dataset - Embarked (Proportion Survived)") +
  theme(plot.title = element_text(size = 10, face = "bold"), 
        legend.position = "bottom")

grid.arrange(g11, g12, ncol = 2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full$Title_FE <- gsub('(.*, )|(\\..*)', '', titanic_full$Name)

titanic_full %>%
  group_by(Title_FE) %>%
  count() %>%
  arrange(desc(n))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full$Title_FE <- ifelse(titanic_full$Title_FE %in% c("Mr", "Miss", "Mrs", "Master"), 
                                titanic_full$Title_FE, 
                                "Other")

table(titanic_full$Title_FE, titanic_full$Sex)


## ----echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full %>%
  filter(!is.na(Survived)) %>%
ggplot(aes(x = factor(Title_FE), fill = factor(Survived))) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Survived") +
  labs(x = "Title", y = "") + 
  ggtitle("Title_FE - Engineered Variable (Proportion Survived)")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full$Cabin_data_FE <- ifelse(!is.na(titanic_full$Cabin), 1, 0)

prop.table(table(titanic_full$Cabin_data_FE))


## ----echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full %>%
  filter(!is.na(Survived)) %>%
ggplot(aes(x = factor(Cabin_data_FE), fill = factor(Survived))) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Survived") +
  labs(x = "Cabin Data Presence (0 = Missing, 1 = Available)", y = "") + 
  ggtitle("Cabin_data_FE - Engineered Variable (Proportion Survived)")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(titanic_fare_pp <- titanic_full %>%
  group_by(Ticket, Fare) %>%
  summarize(Group_size_FE = n()) %>%
  mutate(Fare_pp_FE = Fare / Group_size_FE))

titanic_full <- left_join(titanic_full, titanic_fare_pp, by = c("Ticket", "Fare"))


## ----message = FALSE, warning = FALSE, echo = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g13 <- ggplot(titanic_full, aes(x = Group_size_FE, y = Fare)) + 
  geom_jitter(alpha = 0.2) + 
  geom_smooth(method = "lm", se = F) + 
  scale_y_continuous(limits = c(0, 300)) + 
  labs(x = "Group Size", y = "Fare") + 
  theme(legend.position = "none")

g14 <- ggplot(titanic_full, aes(x = Group_size_FE, y = Fare, col = factor(Pclass))) + 
  geom_jitter(alpha = 0.2) + 
  geom_smooth(method = "lm", se = F) + 
  facet_grid(Pclass ~ ., labeller = label_both) + 
  scale_y_continuous(limits = c(0, 300)) + 
  labs(x = "Group Size", y = "Fare") + 
  theme(legend.position = "none")

grid.arrange(g13, g14, ncol = 2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(lm(Fare ~ Group_size_FE, titanic_full))$r.square


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(lm(Fare ~ Group_size_FE + factor(Pclass), titanic_full))$r.square


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(lm(Fare ~ Group_size_FE + factor(Pclass) + factor(Pclass)*Group_size_FE, titanic_full))$r.square


## ----echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full %>%
  filter(!is.na(Survived)) %>%
ggplot(aes(x = Fare_pp_FE, fill = factor(Survived))) + 
  geom_histogram(binwidth = 2) + 
  scale_y_continuous(breaks = seq(0, 500, 50)) + 
  scale_x_continuous(breaks = seq(0, 200, 20)) +
  scale_fill_discrete(name = "Survived") + 
  labs(x = "Fare (per person)", y = "Count") + 
  ggtitle("Fare_pp_FE - Engineered Variable (Proportion Survived)")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full$Family_size_FE <- titanic_full$SibSp + titanic_full$Parch + 1

titanic_full$Total_group_size_FE <- pmax(titanic_full$Family_size_FE, titanic_full$Group_size_FE)


## ----message = FALSE, warning = FALSE, echo = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full %>%
  filter(!is.na(Survived)) %>%
ggplot(aes(x = Total_group_size_FE, fill = factor(Survived))) + 
  geom_histogram() + 
  scale_y_continuous(breaks = seq(0, 700, 100)) + 
  scale_x_continuous(breaks = seq(0, 10)) +
  scale_fill_discrete(name = "Survived") + 
  labs(x = "Group Size: max(Family Size, Group Size)", y = "Count") + 
  ggtitle("Total_group_size_FE - Engineered Variable (Proportion Survived)")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full <- titanic_full %>%
  select(-c(Name, Ticket, Cabin, Group_size_FE, Family_size_FE, Fare))

missing_vars(titanic_full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full[is.na(titanic_full$Fare_pp_FE), ]
titanic_full$Fare_pp_FE[is.na(titanic_full$Fare_pp_FE)] <- median(titanic_full$Fare_pp_FE, na.rm = T)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full[is.na(titanic_full$Embarked), c("PassengerId", "Embarked", "Fare_pp_FE", "Total_group_size_FE")]


## ----echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full %>%
  filter(!is.na(Embarked)) %>%
ggplot(aes(x = Embarked, y = Fare_pp_FE)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 40, col = "deepskyblue4")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full$Embarked[is.na(titanic_full$Embarked)] <- "C"


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Nominal factors
titanic_full_nominal <- c('Survived', 'Sex', 'Embarked', 'Title_FE', 'Cabin_data_FE')
titanic_full[titanic_full_nominal] <- lapply(titanic_full[titanic_full_nominal], function(x){factor(x)})

# Ordinal factors
titanic_full$Pclass <- factor(titanic_full$Pclass, 
                              ordered = TRUE, 
                              levels = c(3, 2, 1), 
                              labels = c("Third", "Second", "First"))

# Tidying
titanic_full$Total_group_size_FE <- as.integer(titanic_full$Total_group_size_FE)
titanic_full$Test_Data <- as.integer(titanic_full$Test_Data)
titanic_full$Fare_pp_FE <- round(titanic_full$Fare_pp_FE, 2)

glimpse(titanic_full)


## ----message = FALSE, warning = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
age_train <- titanic_full %>%
  filter(!is.na(Age))

set.seed(2307)

repeatedCV <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)

rf_age_grid <- expand.grid(mtry = c(2, 3, 4))

tic()
rf_age <- train(x = age_train %>% select(c(Pclass, Sex, SibSp, Parch, Embarked, Title_FE, Cabin_data_FE, Fare_pp_FE, Total_group_size_FE)),
                     y = age_train$Age,
                     method = "rf", 
                     trControl = repeatedCV, 
                     importance = TRUE, 
                     tuneGrid = rf_age_grid)
toc()

rf_age
varImp(rf_age)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
age_predictions <- predict(rf_age, titanic_full)

# joining back onto dataframe

titanic_full <- as_tibble(cbind(titanic_full, age_predictions))

titanic_full$Age_IMP <- ifelse(!is.na(titanic_full$Age), 
                               titanic_full$Age, 
                               titanic_full$age_predictions)

titanic_full <- titanic_full %>%
  select(-c(Age, SibSp, Parch, age_predictions))

titanic_full$Age_IMP <- ifelse(titanic_full$Title_FE == 'Master' & titanic_full$Age_IMP > 13,
                               13, 
                               titanic_full$Age_IMP) 
                               # winsorizing those above the datasets 'Master' upper bound for age


## ----message = FALSE, warning = FALSE, echo = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full %>%
  filter(!is.na(Survived)) %>%
ggplot(aes(x = Age_IMP, fill = factor(Survived))) + 
  geom_histogram() + 
  scale_y_continuous(breaks = seq(0, 150, 10)) + 
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  scale_fill_discrete(name = "Survived") + 
  labs(x = "Age (with imputations)", y = "Count") + 
  ggtitle("Age including imputations - (Proportion Survived)")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full$IsChild_FE <- factor(ifelse(titanic_full$Age_IMP < 15, 1, 0))


## ----echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_full %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(x = IsChild_FE, fill = Survived)) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Survived") +
  labs(x = "Child Flag (Age < 15)", y = "") + 
  ggtitle("IsChild_FE - Engineered Variable (Proportion Survived)")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glimpse(titanic_full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rf_test <- as_tibble(titanic_full) %>%
  filter(Test_Data == 1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rf_train_1 <- titanic_full %>%
  filter(Test_Data == 0) %>%
  select(c(Survived, Pclass, Sex, Embarked, Title_FE, Cabin_data_FE, Fare_pp_FE, Total_group_size_FE, Age_IMP, IsChild_FE))


## ----message = FALSE, warning = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2307)

repeatedCV <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)

rf_grid <- expand.grid(mtry = seq(from = 2, to = ncol(rf_train_1) - 1, by = 1))

tic()
rf_model_1 <- train(x = rf_train_1[ ,-1],
                  y = rf_train_1$Survived,
                  method = "rf", 
                  trControl = repeatedCV, 
                  importance = TRUE, 
                  tuneGrid = rf_grid)
toc()


rf_model_1 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
varImp(rf_model_1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
paste("The maximum accuracy was", round(max(rf_model_1$results$Accuracy), 5))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rf_train_2 <- titanic_full %>%
  filter(Test_Data == 0) %>%
  select(c(Survived, Pclass, Sex, Embarked, Title_FE, Fare_pp_FE, Total_group_size_FE, Age_IMP))


## ----message = FALSE, warning = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2307)

repeatedCV <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)

rf_grid <- expand.grid(mtry = seq(from = 2, to = ncol(rf_train_2) - 1, by = 1))

tic()
rf_model_2 <- train(x = rf_train_2[ ,-1],
                    y = rf_train_2$Survived,
                    method = "rf", 
                    trControl = repeatedCV, 
                    importance = TRUE, 
                    tuneGrid = rf_grid)
toc()


rf_model_2


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
varImp(rf_model_2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
paste("The maximum accuracy was", round(max(rf_model_2$results$Accuracy), 5))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rf_train_3 <- titanic_full %>%
  filter(Test_Data == 0) %>%
  select(c(Survived, Pclass, Sex, Embarked, Title_FE, Fare_pp_FE, Total_group_size_FE, IsChild_FE))


## ----message = FALSE, warning = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2307)

repeatedCV <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)

rf_grid <- expand.grid(mtry = seq(from = 2, to = ncol(rf_train_3) - 1, by = 1))

tic()
rf_model_3 <- train(x = rf_train_3[ ,-1],
                    y = rf_train_3$Survived,
                    method = "rf", 
                    trControl = repeatedCV, 
                    importance = TRUE, 
                    tuneGrid = rf_grid)
toc()


rf_model_3 


## ----echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(rf_model_3) + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.002)) + 
  geom_vline(xintercept = rf_model_3$bestTune$mtry, col = "green")


## ----echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(varImp(rf_model_3))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
paste("The maximum accuracy was", round(max(rf_model_3$results$Accuracy), 5))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rf_pred_3 <- predict(rf_model_3, rf_test)

test_predictions_3 <- cbind(rf_test, rf_pred_3)

test_predictions_3 <- test_predictions_3 %>%
  select(PassengerId, rf_pred_3) %>%
  rename(Survived = rf_pred_3)

write.csv(test_predictions_3, 
          file = "RF_Titanic_3.csv", 
          row.names = FALSE, 
          quote = FALSE)


## ----echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Final_model_error <- as.data.frame(cbind(ntree = 1:500, rf_model_3$finalModel$err.rate)) %>%
  tidyr::gather(key = "Error_Type", value = "Error", -ntree)

ggplot(Final_model_error, aes(x = ntree, y = Error, col = factor(Error_Type))) + 
  geom_line() + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.05)) +
  labs(x = "Number of Trees", y = "Error Rate", colour = "Error Type") + 
  ggtitle("Final Model Error Rates - Died, Survived & Out-Of-Bag")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rf_model_3$finalModel$confusion

