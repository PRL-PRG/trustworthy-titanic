
library(tidyverse) # metapackage with lots of helpful functions
library(ggplot2) # package that makes nice looking graphics
library(sqldf) # data processing and data analysis using SQL on dataframes
library(dplyr)
library(gridExtra)

list.files(path = "../input") # shows all of the files that are available in the directory

train_data = read.csv("../input/train.csv", stringsAsFactors = FALSE, header = TRUE) # importing in the training data set
test_data = read.csv("../input/test.csv", stringsAsFactors = FALSE, header = TRUE) # importing in the testing data set

head(train_data)

train_data$Survived <- as.factor(train_data$Survived)
train_data$Pclass <- as.factor(train_data$Pclass)
train_data$Sex <- as.factor(train_data$Sex)
train_data$SibSp <- as.factor(train_data$SibSp)
train_data$Parch <- as.factor(train_data$Parch)

summary(train_data)

g <- ggplot(data = train_data, aes(x = Sex))

p1 <- g + geom_bar(aes(fill = Survived)) +
    labs(title = "Survival Rates By Sex", subtitle = "Colored by Survival", y = "Counts", x = "Sex")

p2 <- g + geom_bar(aes(fill = Survived), position = "fill") +
    labs(title = "Survival Rates By Sex", subtitle = "Colored by Survival; Normalized using Percents", y = "Counts", x = "Sex")

grid.arrange(p1, p2)

sex_survived1 = sqldf("SELECT Survived, Sex, COUNT(Survived) AS Counts
                      FROM train_data 
                      GROUP BY Sex, Survived")

sex_survived2 = sqldf("SELECT Sex, COUNT(Sex) AS TotalCounts
                      FROM train_data 
                      GROUP BY Sex")

sex_survived = sqldf("SELECT sex_survived1.Survived, sex_survived1.Sex, sex_survived1.Counts, sex_survived2.TotalCounts
                       FROM sex_survived1
                       INNER JOIN sex_survived2 ON sex_survived1.Sex=sex_survived2.Sex")

sex_survived$Percents = round((sex_survived$Counts / sex_survived$TotalCounts)*100, 2)

sex_survived

g <- ggplot(data = train_data, aes(x = Pclass))

p1 <- g + geom_bar(aes(fill = Survived)) +
    labs(title = "Survival Rates By Class", subtitle = "Colored by Survival", y = "Counts", x = "Pclass")

p2 <- g + geom_bar(aes(fill = Survived), position = "fill") +
    labs(title = "Survival Rates By Class", subtitle = "Colored by Survival; Normalized using Percents", y = "Counts", x = "Pclass")

grid.arrange(p1, p2)

pclass_survived1 = sqldf("SELECT Survived, Pclass, COUNT(Survived) AS Counts
                      FROM train_data 
                      GROUP BY Pclass, Survived")

pclass_survived2 = sqldf("SELECT Pclass, COUNT(Pclass) AS TotalCounts
                      FROM train_data 
                      GROUP BY Pclass")

pclass_survived = sqldf("SELECT pclass_survived1.Survived, pclass_survived1.Pclass, pclass_survived1.Counts, pclass_survived2.TotalCounts
                       FROM pclass_survived1
                       INNER JOIN pclass_survived2 ON pclass_survived1.Pclass = pclass_survived2.Pclass
                       ORDER BY pclass_survived1.Pclass, Survived")

pclass_survived$Percents = round((pclass_survived$Counts / pclass_survived$TotalCounts)*100, 2)

pclass_survived

g <- ggplot(data = train_data, aes(x = Sex))

p1 <- g + geom_bar(aes(fill = Survived)) +
    labs(title = "Survival Rates By Sex", subtitle = "Colored by Survival; Split by Pclass", y = "Counts", x = "Sex")+
    facet_wrap(~ Pclass)

p2 <- g + geom_bar(aes(fill = Survived), position = "fill") +
    labs(title = "Survival Rates By Sex", subtitle = "Colored by Survival; Split by Pclass; Normalized Using Percents", y = "Counts", x = "Sex")+
    facet_wrap(~ Pclass)

grid.arrange(p1, p2)

sex_pclass_survived1 = sqldf("SELECT Survived, Sex, Pclass, COUNT(Survived) AS Counts
                      FROM train_data 
                      GROUP BY Sex, Pclass, Survived")

sex_pclass_survived2 = sqldf("SELECT Sex, Pclass, COUNT(Pclass) AS TotalCounts
                      FROM train_data 
                      GROUP BY Sex, Pclass")

sex_pclass_survived = sqldf("SELECT sex_pclass_survived1.Survived, sex_pclass_survived1.Pclass,
                                    sex_pclass_survived1.Sex, sex_pclass_survived1.Counts, sex_pclass_survived2.TotalCounts
                       FROM sex_pclass_survived1
                       INNER JOIN sex_pclass_survived2 
                           ON (sex_pclass_survived1.Pclass = sex_pclass_survived2.Pclass) AND (sex_pclass_survived1.Sex = sex_pclass_survived2.Sex)
                       ORDER BY sex_pclass_survived1.Pclass, sex_pclass_survived1.Sex, Survived")

sex_pclass_survived$Percents = round((sex_pclass_survived$Counts / sex_pclass_survived$TotalCounts) * 100, 2)


sex_pclass_survived
