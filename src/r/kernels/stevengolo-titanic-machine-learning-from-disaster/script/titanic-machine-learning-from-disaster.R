knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(glmnet)
library(gridExtra)
library(kableExtra)
library(knitr)
library(randomForest)
library(reshape2)
library(tidyverse)
theme_custom <- function(base_family = "Times") {
    theme_minimal(base_family = base_family) %+replace% theme(plot.title = element_text(size = 20), plot.subtitle = element_text(size = 16, vjust = -1), axis.title = element_text(size = 18), axis.text = element_text(size = 16), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), angle = 90), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0)), strip.text.x = element_text(size = 16))
}
train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")
titanic <- train %>% bind_rows(test) %>% select(-PassengerId) %>% mutate_at(vars(Pclass, Sex, Embarked), funs(factor(.)))
titanic %>% ggplot(aes(x = Pclass)) + geom_bar(width = 0.5, fill = "#56B4E9") + coord_flip() + labs(title = "Count of the passengers by class") + scale_x_discrete(name = "Passenger's class") + scale_y_continuous(name = "Count", breaks = seq(0, 900, 100)) + theme_custom()
titanic %>% ggplot(aes(x = Sex)) + geom_bar(width = 0.5, fill = "#56B4E9") + coord_flip() + labs(title = "Count of the passengers by sex") + scale_x_discrete(name = "Passenger's sex") + scale_y_continuous(name = "Count", breaks = seq(0, 900, 100)) + theme_custom()
Title <- "^.*, (.*?)\\..*$" %>% gsub("\\1", titanic$Name)
title_high <- c("Mr", "Miss", "Mrs", "Master")
Title <- Title %in% title_high %>% if_else(Title, "Other")
titanic <- titanic %>% add_column(Title) %>% mutate_at(vars(Title), funs(factor(.)))
titanic %>% ggplot(aes(x = Title)) + geom_bar(width = 0.5, fill = "#56B4E9") + coord_flip() + labs(title = "Count of the passengers by title") + scale_x_discrete(name = "Passenger's title") + scale_y_continuous(name = "Count", breaks = seq(0, 900, 100)) + theme_custom()
titanic %>% filter(is.na(Embarked)) %>% kable(format = "html") %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
titanic %>% filter(Pclass == 1) %>% ggplot(aes(x = Embarked, y = Fare)) + geom_boxplot(colour = "black", fill = "#56B4E9", outlier.colour = "red") + ggtitle("Fare depending on the port of embarkation for the first class") + scale_x_discrete(name = "Port of embarkation") + scale_y_continuous(name = "Fare") + theme_custom()
titanic[62, "Embarked"] <- "C"
titanic[830, "Embarked"] <- "C"
titanic %>% ggplot(aes(x = Embarked)) + geom_bar(width = 0.5, fill = "#56B4E9") + coord_flip() + labs(title = "Count of the passengers by port of embarkation") + scale_x_discrete(name = "Passenger's port of embarkation") + scale_y_continuous(name = "Count", breaks = seq(0, 900, 100)) + theme_custom()
titanic %>% filter(is.na(Fare)) %>% kable(format = "html") %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
titanic %>% filter(Embarked == "S", Pclass == 3) %>% ggplot(aes(x = Fare)) + geom_density(kernel = "gaussian", colour = "#FFFFFF", fill = "#56B4E9") + ggtitle("Density of Fare", subtitle = "Embarked in Southampton in third class") + xlab("Fare ($)") + ylab("Density") + theme_custom()
titanic[1044, "Fare"] <- titanic %>% filter(Embarked == "S", Pclass == 3) %>% pull(Fare) %>% median(na.rm = TRUE)
grid.arrange(titanic %>% ggplot(aes(y = Age)) + geom_boxplot(fill = "#56B4E9", outlier.colour = "red") + coord_flip() + ggtitle("Boxplot of Age") + scale_x_continuous(breaks = NULL) + theme_custom(), titanic %>% ggplot(aes(x = Age)) + geom_density(kernel = "gaussian", colour = "#FFFFFF", fill = "#56B4E9") + ggtitle("Density of Age") + xlab("Age") + ylab("Density") + theme_custom(), ncol = 2, nrow = 1)
titanic.with.age <- titanic %>% filter(!is.na(Age)) %>% select(-c(Survived, Name, Ticket, Cabin))
titanic.without.age <- titanic %>% filter(is.na(Age)) %>% select(-c(Survived, Name, Ticket, Cabin)) %>% mutate(Age = 0)
titanic.lm <- lm(Age ~ ., data = titanic.with.age)
titanic.with.age.model.matrix <- model.matrix(titanic.lm, data = titanic.with.age)[, -1]
titanic.age.model <- glmnet(titanic.with.age.model.matrix, titanic.with.age$Age, alpha = 0)
titanic.without.age$Age <- predict(titanic.age.model, newx = model.matrix(titanic.lm, data = titanic.without.age)[, -1], s = cv.glmnet(titanic.with.age.model.matrix, titanic.with.age$Age, alpha = 0)$lambda.min, type = "link")
titanic[is.na(titanic$Age), "Age"] <- titanic.without.age$Age
grid.arrange(titanic %>% ggplot(aes(y = Age)) + geom_boxplot(fill = "#56B4E9", outlier.colour = "red") + coord_flip() + ggtitle("Boxplot of Age") + scale_x_continuous(breaks = NULL) + theme_custom(), titanic %>% ggplot(aes(x = Age)) + geom_density(kernel = "gaussian", colour = "#FFFFFF", fill = "#56B4E9") + ggtitle("Density of Age") + xlab("Age") + ylab("Density") + theme_custom(), ncol = 2, nrow = 1)
titanic %>% ggplot(aes(x = as.factor(SibSp))) + geom_bar(width = 0.5, fill = "#56B4E9") + coord_flip() + labs(title = "Count of the passengers number of siblings/spouses") + scale_x_discrete(name = "Number of Siblings/Spouses") + scale_y_continuous(name = "Count", breaks = seq(0, 900, 100)) + theme_custom()
titanic %>% ggplot(aes(x = as.factor(Parch))) + geom_bar(width = 0.5, fill = "#56B4E9") + coord_flip() + labs(title = "Count of the passengers number of parents/children") + scale_x_discrete(name = "Number of Parents/Children") + scale_y_continuous(name = "Count", breaks = seq(0, 1000, 100)) + theme_custom()
titanic <- titanic %>% select(-Cabin)
titanic <- titanic %>% select(-Ticket)
train <- titanic %>% select(-Name) %>% filter(!is.na(Survived))
test <- titanic %>% select(-Name) %>% filter(is.na(Survived))
set.seed(42)
sample <- sample(c(TRUE, FALSE), nrow(train), replace = TRUE, prob = c(2/3, 1/3))
train.val <- train[sample, ]
test.val <- train[!sample, ]
train.lm <- lm(Survived ~ ., data = train.val)
X <- model.matrix(train.lm, data = train.val)[, -1]
Y <- train.val$Survived
train.ridge.model <- glmnet(X, Y, alpha = 0, family = "binomial")
test.val.predict <- predict(train.ridge.model, s = cv.glmnet(X, Y, alpha = 0)$lambda.min, newx = model.matrix(train.lm, data = test.val)[, -1], type = "class")
test$Survived <- 0
test.predict <- predict(train.ridge.model, s = cv.glmnet(X, Y, alpha = 0)$lambda.min, newx = model.matrix(train.lm, data = test)[, -1], type = "class")
result <- data.frame(PassengerID = row.names(test.predict), Survived = test.predict[, 1])
write.csv(result, "results.csv", row.names = FALSE)
