## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE
)


## ---- message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(mice)
library(randomForest)
library(mice)
library(tree)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_train <- read.csv('../input/train.csv',
                          stringsAsFactors = FALSE)
titanic_test <- read.csv('../input/test.csv',
                         stringsAsFactors = FALSE)

str(titanic_train)
str(titanic_test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic <- bind_rows(titanic_train, titanic_test)

str(titanic)

head(titanic)

tail(titanic)

summary(titanic)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sapply(titanic, function(x) sum(is.na(x)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic$fam_size <- titanic$SibSp + titanic$Parch + 1


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891, ],
       aes(fam_size,
           fill = factor(Survived))) +
  geom_bar(stat = "count",
           position = "dodge") +
  scale_x_continuous(breaks = 0:11) +
  ggtitle("Family size vs No of survivors") +
  xlab("Family Size") +
  ylab("No of survivors") +
  labs(fill = "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

unique(titanic$fam_size)

titanic$fam_type[titanic$fam_size == 1] <- "single"
titanic$fam_type[titanic$fam_size > 1 & titanic$fam_size < 4] <- "few"
titanic$fam_type[titanic$fam_size > 3] <- "many"

ggplot(titanic[1:891, ],
       aes(fam_type,
           fill = factor(Survived))) +
  geom_bar(stat = "count",
           position = "dodge") +
  ggtitle("Family type vs No of survivors") +
  xlab("Family Type") +
  ylab("No of survivors") +
  labs(fill = "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(titanic$Cabin)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic$Deck <- factor(sapply(titanic$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
unique(titanic$Deck)

ggplot(na.omit(titanic[1:891, ]),
       aes(Survived,
           fill = factor(Survived))) +
  geom_bar(stat = "count",
           position = "dodge") +
  facet_wrap(~Deck) +
  scale_x_continuous(breaks = 0:1) +
  ggtitle("Deck level vs No of survivors") +
  ylab("No of survivors") +
  labs(fill = "Survived") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891, ],
       aes(Survived,
           fill = factor(Survived))) +
  geom_bar(stat = "count",
           position = "dodge") +
  facet_wrap(~Pclass) +
  scale_x_continuous(breaks = 0:1) +
  ggtitle("Survivors in each class") +
  xlab("Class") +
  ylab("No of survivors") +
  labs(fill = "Survived") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
  


## ----warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891, ],
       aes(Fare, Age)) +
  geom_point(aes(colour = factor(Survived))) +
  facet_wrap(~Sex) +
  ggtitle("Survivors by gender, age vs fare") +
  labs(colour = "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891, ],
       aes(factor(Embarked),
           fill = factor(Survived))) +
  geom_bar(stat = "count",
           position = "dodge") +
  ggtitle("Survivors according to boarding point") +
  xlab("Boarding point") +
  ylab("No of survivors") +
  labs(fill = "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891, ] %>% filter(Survived == 1),
       aes(Sex)) +
  geom_bar(stat = "count",
           position = "dodge") +
  facet_grid(~Pclass) +
  ggtitle("Survivors according to gender for each class") +
  ylab("No of people")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(titanic$Name)

titanic$title <- gsub('(.*, )|(\\..*)', '', titanic$Name)

unique(titanic$title)

table(titanic$title, titanic$Sex)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')


titanic$title[titanic$title == 'Mlle']        <- 'Miss' 
titanic$title[titanic$title == 'Ms']          <- 'Miss'
titanic$title[titanic$title == 'Mme']         <- 'Mrs' 
titanic$title[titanic$title %in% rare_title]  <- 'Rare Title'

table(titanic$title, titanic$Sex)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic$surname <- sapply(titanic$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])

head(titanic$surname)
length(unique(titanic$surname))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

unique(titanic$Embarked)

which(titanic$Embarked == "")

titanic[which(titanic$Embarked == ""), ]

titanic[c(62,830), ]

titanic %>% filter(PassengerId != 62 & PassengerId != 830 & Pclass == 1) %>% group_by(Embarked) %>% summarise(n = median(Fare))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic$Embarked[c(62,830)] <- "C"


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic$Embarked[c(62,830)] <- "S"


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891, ],
       aes(factor(Embarked),
           fill = factor(Survived))) +
  geom_bar(stat = "count",
           position = "dodge") +
  ggtitle("Survivors according to boarding point") +
  xlab("Boarding point") +
  ylab("No of survivors") +
  labs(fill = "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(is.na(titanic$Fare))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
which(is.na(titanic$Fare))

titanic[which(is.na(titanic$Fare)), ]

titanic %>% filter(Pclass == 3 & Embarked == "S") %>% summarise(n = median(Fare, na.rm = TRUE))

ggplot(titanic %>% filter(Pclass == 3 & Embarked == "S"),
       aes(x = Fare)) +
  geom_density() +
  geom_vline(xintercept = 8)

titanic$Fare[which(is.na(titanic$Fare))] <- median(titanic[titanic$Pclass == 3 & titanic$Embarked == "S", ]$Fare, na.rm = TRUE)

titanic[1044, "Fare"]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(titanic)

titanic %>% group_by(Pclass, Deck, Embarked) %>% summarise(mean = mean(Fare, na.rm = TRUE))
titanic %>% group_by(Pclass, Deck, Embarked) %>% summarise(med = median(Fare, na.rm = TRUE))
titanic %>% group_by(Pclass, Deck, Embarked) %>% summarise(min = min(Fare, na.rm = TRUE))
titanic %>% group_by(Pclass, Deck, Embarked) %>% summarise(max = max(Fare, na.rm = TRUE))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
quantile(titanic[titanic$title == "Mr", ]$Age, na.rm = TRUE)
quantile(titanic[titanic$title == "Mrs", ]$Age, na.rm = TRUE)

quantile(titanic[titanic$title == "Mr", ]$Age, na.rm = TRUE, .05)
quantile(titanic[titanic$title == "Mrs", ]$Age, na.rm = TRUE, .05)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic <- titanic %>% mutate(age_type = ifelse(Age < 16, "Child", ifelse(Age < 60, "Adult", "Senior")))

titanic %>% group_by(Pclass, age_type, Sex) %>% summarise(age = median(Age, na.rm = TRUE))

titanic %>% filter(Age < 16) %>% group_by(Pclass, Sex) %>% summarise(age = median(Age, na.rm = TRUE), age_mean = mean(Age, na.rm = TRUE), age_sd = sd(Age, na.rm = TRUE))

titanic %>% filter(Age >= 16 & Age < 60) %>% group_by(Pclass, Sex) %>% summarise(age = median(Age, na.rm = TRUE), age_mean = mean(Age, na.rm = TRUE), age_sd = sd(Age, na.rm = TRUE))

titanic %>% filter(Age >= 60) %>% group_by(Pclass, Sex) %>% summarise(age = median(Age, na.rm = TRUE), age_mean = mean(Age, na.rm = TRUE), age_sd = sd(Age, na.rm = TRUE))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(is.na(titanic$Age))

mice_titanic <- mice(data = titanic[ , !names(titanic) %in% c("PassengerId", "Survived", "Name", "Ticket", "Cabin", "surname", "age_type")], m = 3, method = "rf", maxit = 20, seed = 765)

mice_titanic_op <- complete(mice_titanic)

ggplot() +
  geom_density(aes(x = titanic$Age, col = "blue")) +
  geom_density(aes(x = mice_titanic_op$Age, col = "red")) +
  ggtitle("Distribution of age in original and imputed data set") +
  xlab("Age") +
  ylab("Density") +
  labs(col = "Colour") +
  scale_color_manual(labels = c("titanic", "mice"), values = c("blue", "red"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic$Age <- mice_titanic_op$Age


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic <- titanic %>% mutate(age_type = ifelse(Age < 16, "Child", ifelse(Age < 60, "Adult", "Senior")))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_factors <- titanic

str(titanic_factors)
titanic_factors$Survived <- as.factor(titanic_factors$Survived)
titanic_factors$Pclass <- as.factor(titanic_factors$Pclass)
titanic_factors$Sex <- as.factor(titanic_factors$Sex)
titanic_factors$Embarked <- as.factor(titanic_factors$Embarked)
titanic_factors$fam_type <- as.factor(titanic_factors$fam_type)
titanic_factors$title <- as.factor(titanic_factors$title)
titanic_factors$age_type <- as.factor(titanic_factors$age_type)

str(titanic_factors)

titanic_train <- titanic_factors[1:891, ]
titanic_test <- titanic_factors[892:1309, ]

titanic_model_4 <- tree(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + fam_type + title + age_type + Fare, data = titanic_train )

titanic_tree_pred <- predict(titanic_model_4, titanic_test, type = "class")

submission <- data.frame(PassengerId = titanic_test$PassengerId, Survived = titanic_tree_pred)

write.csv(submission, file = "submission_tree.csv", row.names = FALSE)


## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# titanic_model_3 <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + fam_type + title + age_type + Fare, data = titanic_train )
# 
# titanic_rf_pred <- predict(titanic_model_3, titanic_test, type = "class")
# 
# submission <- data.frame(PassengerId = titanic_test$PassengerId, Survived = titanic_rf_pred)
# 
# write.csv(submission, file = "submission_rf.csv", row.names = FALSE)

# titanic_model_1 <- tree(Survived~ Pclass+Sex+Age+SibSp+Fare, data = titanic_train)
# titanic_tree_pred_1 <- predict(titanic_model_1, titanic_test, type = "class")
# 
# submission <- titanic_test
# 
# submission$Survived <- titanic_tree_pred_1
# 
# submission2 <- submission[,c("PassengerId", "Survived")]
# 
# write.csv(x = submission2,file = "submission_2.csv", row.names = FALSE)


