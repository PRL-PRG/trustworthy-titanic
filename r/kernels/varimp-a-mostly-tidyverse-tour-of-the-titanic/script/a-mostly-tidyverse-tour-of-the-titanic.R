## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)

train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")
class(train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glimpse(train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glimpse(test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setdiff(names(train), names(test)) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summarise(train, SurvivalRate = sum(Survived)/nrow(train)*100)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
baseline_solution <- data.frame(PassengerID = test$PassengerId, Survived = 0)
# To submit this as an entry, just un-comment the next line and submit the .csv file 
# write.csv(baseline_solution, file = 'baseline_model.csv', row.names = F) 


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic <- full_join(train, test)
glimpse(titanic)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(forcats)
titanic <- titanic %>%
                mutate(Survived = factor(Survived)) %>%
                mutate(Survived = fct_recode(Survived, "No" = "0", "Yes" = "1"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic <- titanic %>%
        mutate(Sex = factor(Sex)) %>%
        mutate(Sex = fct_recode(Sex, "Female" = "female", "Male" = "male"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891,], aes(Sex, fill=Survived)) +
           geom_bar(position = "fill") +
           ylab("Survival Rate") +
           geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "white", lty = 2) +
           ggtitle("Survival Rate by Gender")



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(stringr)
titanic <- mutate(titanic, Title = str_sub(Name, str_locate(Name, ",")[ , 1] + 2, str_locate(Name, "\\.")[ , 1] - 1))

titanic %>% group_by(Title) %>%
              summarise(count = n()) %>%
              arrange(desc(count))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic <- titanic %>%
          mutate(Mother = factor(ifelse(c(titanic$Title == "Mrs" | titanic$Title == "Mme" | titanic$Title == "the Countess" | titanic$Title == "Dona" | titanic$Title == "Lady") & titanic$Parch > 0, "Yes", "No"))) 

ggplot(titanic[1:891,], aes(x = Mother, fill = Survived)) +
          geom_bar(position = "fill") +
          ylab("Survival Rate") +
          geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "white", lty = 2) +
          ggtitle("Survival Rate by Motherhood Status")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic <- titanic %>%
          mutate(Title = factor(Title)) %>%
          mutate(Title = fct_collapse(Title, "Miss" = c("Mlle", "Ms"), "Mrs" = "Mme", 
                                      "Ranked" = c( "Major", "Dr", "Capt", "Col", "Rev"),
                                      "Royalty" = c("Lady", "Dona", "the Countess", "Don", "Sir", "Jonkheer"))) 

ggplot(titanic[1:891,], aes(x = Title, fill = Survived)) +
          geom_bar(position = "fill") +
          ylab("Survival Rate") +
          geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "white", lty = 2) +
          ggtitle("Survival Rate by Title")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(qdap)
titanic <- titanic %>%
              mutate(Surname = factor(beg2char(Name, ","))) %>% 
              glimpse()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic <- mutate(titanic, Solo = factor(ifelse(SibSp + Parch + 1 == 1, "Yes", "No")))

ggplot(titanic[1:891,], aes(x = Solo, fill=Survived)) +
          geom_bar(position = "fill") +
          ylab("Survival Rate") +
          geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "white", lty = 2) +
          ggtitle("Survival Rate by Solo Passenger Status")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic <- titanic %>% 
              mutate(FamilySize = SibSp + Parch + 1) %>% 
              mutate(FamilyType = factor(ifelse(FamilySize > 4, "Large", ifelse(FamilySize == 1, "Single", "Medium"))))

ggplot(titanic[1:891,], aes(x = FamilyType, fill = Survived)) +
          geom_bar(position = "fill") +
          ylab("Survival Rate") +
          geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "white", lty = 2) + 
          ggtitle ("Survival Rate by Family Group Size")



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(VIM)

titanic %>% map_dbl(~sum(is.na(.)))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
aggr(titanic, prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
filter(titanic, is.na(Embarked)) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
filter(titanic, is.na(Fare)) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic %>% group_by(Pclass, Embarked) %>%
                summarise(count = n(), median_fare = median(Fare, na.rm=TRUE))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic <- titanic %>%
              mutate(Embarked = factor(ifelse(is.na(Embarked), names(which.max(table(titanic$Embarked))), Embarked))) %>%
              group_by(Pclass, Embarked) %>%
              mutate(Fare = ifelse(is.na(Fare), round(median(Fare, na.rm = TRUE), 4), Fare))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891,], aes(x = Embarked, fill = Survived)) +
      geom_bar(position = "fill") +
      ylab("Survival Rate") +
      geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "white", lty = 2) +
      ggtitle("Survival Rates by Embarkation Port")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891,], aes(x = Pclass, fill = Survived)) +
      geom_bar(position = "fill") +
      ylab("Survival Rate") +
      geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "white", lty = 2) +
      ggtitle("Survival Rates by Passenger Class")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891,], aes(x = log(Fare), fill = Survived)) +
      geom_density(alpha = 0.4)  + 
      ggtitle("Density Plot of Fare related to Survival") 



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic %>% group_by(Title) %>%
             summarise(median = median(Age, na.rm = TRUE))
 
titanic <- titanic %>%
              group_by(Title) %>%
              mutate(Age = ifelse(is.na(Age), round(median(Age, na.rm = TRUE), 1), Age)) 



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891,], aes(x = Age, fill = Survived)) +
      geom_density(alpha = 0.4)  + 
      ggtitle("Density Plot of Age related to Survival") 



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic <- mutate(titanic, LifeStage = factor(ifelse(Age < 18, "Child", ifelse(Age <= 60, "Adult", "OAP"))))

ggplot(titanic[1:891,], aes(x = LifeStage, fill = Survived)) +
      geom_bar(position = "fill") +
      ylab("Survival Rate") +
      geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "white", lty = 2) +
      ggtitle("Survival Rates by Life Stage")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic <- select(titanic, -c(Name, Ticket, Cabin, Surname)) %>%
              glimpse()


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train1 <- titanic[1:891,]
test1 <- titanic[892:1309,]

library(party)
set.seed(144)
cf_model <- cforest(Survived ~ Sex + Age + SibSp + Parch + 
                 log(Fare) + Embarked + Pclass + Title + Mother + 
                 Solo + FamilySize + FamilyType + LifeStage,
                 data = train1, 
                 controls = cforest_unbiased(ntree = 2000, mtry = 3)) 


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
xtab <- table(predict(cf_model), train1$Survived)
library(caret) 
confusionMatrix(xtab)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
varimp(cf_model)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cforestImpPlot <- function(x) {
  cforest_importance <<- v <- varimp(x)
  dotchart(v[order(v)])
}

cforestImpPlot(cf_model)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cf_prediction <- predict(cf_model, test1, OOB=TRUE, type = "response")
cf_prediction <- ifelse(cf_prediction == "No", 0, 1)
cf_solution <- data.frame(PassengerID = test1$PassengerId, Survived = cf_prediction)
# To submit this as an entry, just un-comment the next line and submit the .csv file 
# write.csv(cf_solution, file = 'cf_model.csv', row.names = F)

