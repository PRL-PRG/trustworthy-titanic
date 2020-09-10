library(readr)
library(titanic)
library(magrittr)
library(dplyr)
library(stringr)
data("titanic_train")
head(titanic_train)

titanic_train <- titanic_train %>%
  mutate(Survived = factor(Survived))
levels(titanic_train$Survived)<- c("Não","Sim")


titanic_train<-titanic_train %>%
  mutate(title = str_extract(tolower(Name),'[a-z]{1,}\\.'))

head(titanic_train)


titanic_train %>% group_by(title) %>%
  summarize(n=n()) %>%
  arrange(-n)

head(titanic_train)

titanic_train <- titanic_train %>%
  group_by(Sex, title) %>%
  mutate(Age=ifelse(is.na(Age), median(Age, na.rm=TRUE),Age))
summary(titanic_train$Age)

head(titanic_train)

titanic_train<-titanic_train %>%
  mutate(young = ifelse(Age < 36,"Sim","Não"))

head(titanic_train)



