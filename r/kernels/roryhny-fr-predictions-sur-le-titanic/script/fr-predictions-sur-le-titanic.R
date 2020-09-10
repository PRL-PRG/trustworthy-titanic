## ----packages, warning=F, message=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)           # Manipulation df
library(ggplot2)         # Visualisation
library(stringr)         # Manipulation carcts
library(magrittr)        # Outil %>%
library(randomForest)    # Algorithme


## ---- message=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test <- read.csv('../input/test.csv', stringsAsFactors = F)


## ---- message=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full <- bind_rows(train,test)


## ---- message=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
NA_finder <- function(df){
  
  NA_sum_t <- data.frame(VarPos = c(1:ncol(df)),
                         VarName = rep(0),
                         SumNA = rep(0),
                         SumEmpty = rep(0),
                         SumSpaces = rep(0))
  
  for(i in 1:ncol(df)){
    if(sum(is.na(df[,i])) > 0){
      NA_sum_t[i,'SumNA'] <- sum(is.na(df[,i]))
      NA_sum_t[i, 'VarName'] <- names(df)[i]
    } else if(length(which(df[,i] == '')) > 0){
      NA_sum_t[i,'SumEmpty'] <- length(which(df[,i] == ''))
      NA_sum_t[i, 'VarName'] <- names(df)[i]
    } else if(length(which(df[,i] == ' ')) > 0){
      NA_sum_t[i,'SumSpaces'] <- length(which(df[,i] == ' '))
      NA_sum_t[i, 'VarName'] <- names(df)[i]
    }
  }
  
  NA_sum_res <- NA_sum_t %>%
    filter(SumNA > 0 | SumEmpty > 0 | SumSpaces > 0) %>%
    arrange(VarPos)
  
  return(NA_sum_res)
  
}



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
NA_finder(full)


## ---- message = F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
split_name <- str_split(full$Name,'[.*,]') # Decomposition Name en 3 parties

full['Surname'] <- rep(0) # Creation variable Nom de famille
full['Fstname'] <- rep(0) # Creaation variable Prenom
full['Title']   <- rep(0) # Creaation variable Titre

 # Creation variables

for(i in 1:1309){
  full[i,'Surname'] <- split_name[[i]][1]
}
for(i in 1:1309){
  full[i,'Fstname'] <- split_name[[i]][3]
}
for(i in 1:1309){
  full[i,'Title'] <- split_name[[i]][2]
}

full$Title <- gsub('(.*\\s)',"",full$Title) # Suppression des espaces dans les titres



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(full$Sex,full$Title) # Repartition selon le titre

table(full$Pclass, full$Title) # Repartition selon la classe


## ---- message=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
special_titls <- c( "Don","Mme","Major","Lady","Sir","Mlle",
                    "Col","Capt","Countess","Jonkheer","Dona") 

for(i in 1:nrow(full)){
  if(full[i,'Title'] == 'Ms'){
    full[i, 'Title'] <- 'Miss'
  } else if(full[i,'Title'] == 'Rev'){
    full[i, 'Title'] <- 'Mr'
  } else if(full[i,'Title'] == 'Dr' & full[i,'Sex'] == 'male'){
    full[i, 'Title'] <- 'Mr'
  } else if(full[i,'Title'] == 'Dr' & full[i,'Sex'] == 'female'){
    full[i, 'Title'] <- 'Mrs'
  } else if(full[i,'Title'] %in% special_titls){
    full[i, 'Title'] <- 'Special'
  }
}



## ---- message=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full <- full %>%
        mutate(FamNr = SibSp + Parch )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full[which(is.na(full$Fare)),c('PassengerId','Pclass','Embarked','FamNr')]


## ---- message = F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p3Sou <- full %>% filter(Pclass == 3 & Embarked == 'S' & FamNr == 1)
full[1044, 'Fare'] <- median(p3Sou$Fare, na.rm = T)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Recherche des tickets identiques

for(i in 1:nrow(full)){
  full$Perptick[i] <- 0
  for(j in 1:nrow(full)){
    if(full[j, 'Ticket'] == full[i, 'Ticket']){
      full[i, 'Perptick'] <- full[i, 'Perptick'] + 1
    } 
  }
}



## ---- message=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Variable du prix a l'unite

full$Fare_unit <- round(full$Fare / full$Perptick, 2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Deck <- sapply(X = full$Cabin, FUN = function(x){str_split(x, '[[:digit:]]')[[1]][1]}) 
full[which(full$Deck == 'F G'),'Deck'] <- 'F'
full[which(full$Deck == 'F E'),'Deck'] <- 'F'
full[which(full$Deck == 'T'),'Deck'] <- 'A'

for(i in 1:1309){
  if(full[i,'Deck'] == ''){
    full[i,'Deck'] <- 'Unk'
  }
}



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Prix des tickets des embarcations inconnues

full[which(full$Embarked == ''), c('PassengerId','Pclass','Ticket','Fare_unit')]



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(filter(full, Pclass == 1 & Embarked != ''), aes(x = factor(Embarked), y = Fare_unit)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 40), col = 'red', linetype = 'dashed') +
  labs(x = 'Port', y = 'Prix (en £)', title = 'Prix du ticket selon les ports') +
  coord_cartesian(ylim = c(5,60))




## ---- message = F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

full[which(full$Embarked == ''), 'Embarked'] <- 'C'



## ---- message=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

full$Age <- round(full$Age, 0) # Arrondi pour les valeurs estimees
age_unk <- full[which(is.na(full$Age)),] # Df age inconnu
age_k <- full[-which(is.na(full$Age)),] # Df age connu

age_k_surv <- age_k[which(age_k$PassengerId %in% c(1:891)),] # age connu et donnees survie



## ---- message=F,echo = T---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

youngs <- filter(age_k_surv, Age < 20) # Passagers de moins de vingt ans

ggplot(youngs, aes(x = Age, fill = factor(Survived))) +
  geom_histogram(position = 'dodge') +
  geom_hline(aes(yintercept = 5), col = 'red', linetype = 'dashed') +
  scale_y_continuous(breaks = c(0:15)) +
  scale_fill_discrete(name = 'A survecu?', labels = c('Non','Oui')) +
  facet_wrap( ~ Sex) +
  labs(title = 'Survie des jeunes passagers') 
  



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(youngs, aes(x = Age, fill = factor(Survived))) +
  geom_histogram(position = 'dodge', binwidth = 5) + # intervalle de 5
  geom_hline(aes(yintercept = 5), col = 'red', linetype = 'dashed') +
  scale_fill_discrete(name = 'A survecu?', labels = c('Non','Oui')) +
  facet_wrap( ~ Sex) +
  labs(title = 'Survie des jeunes passagers') 
  



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(age_k, aes(x = Title, y = Age, group = Title)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 14), col = 'red', linetype = 'dashed')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(filter(age_k, Title == 'Miss'), aes(x = factor(FamNr), y = Age, group = FamNr)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 14), col = 'red', linetype = 'dashed')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(filter(age_k, Title == 'Miss' & FamNr == 0), aes(x = factor(Perptick), y = Age, group = Perptick)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 14), col = 'red', linetype = 'dashed')



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(filter(age_k, Title == 'Miss' & FamNr == 0), aes(x = factor(Perptick))) +
  geom_bar(fill = 'blue', alpha = 0.5) 



## ---- results=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

age_TitFam_Med <- age_k %>% 
  group_by(Title, FamNr) %>%
  summarise(Age_median = median(Age)) %>%
  ungroup()



## ---- results=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

age_unk <- left_join(age_unk, age_TitFam_Med, by = c("Title","FamNr"))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

filter(NA_finder(age_unk), VarName == 'Age_median')



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

age_unk[which(is.na(age_unk$Age_median)),c("PassengerId","Name","SibSp",
                                           "Parch","Title","Perptick","Age_median")]



## ---- results=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

for(i in 1 : nrow(age_unk)){
  
  if(is.na(age_unk$Age_median)[i] & age_unk$Parch[i] == 0){
    age_unk[i,"Age_median"] <- 14
  } else if(is.na(age_unk$Age_median)[i] & age_unk$Parch[i] == 2){
    age_unk[i,"Age_median"] <- 16
  } else if(is.na(age_unk$Age_median)[i] & age_unk$Parch[i] == 9){
    age_unk[i,"Age_median"] <- 40
  }
  
}

for(i in 1 : nrow(age_unk)){
  age_unk[i, "Age"] <- round(age_unk[i,"Age_median"],0)
}

full_bind <- bind_rows(age_k,age_unk[,c(1:19)])

full <- full_bind[order(full_bind$PassengerId),]




## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for(i in 1:nrow(full)){
  if(full[i,"Age"] < 3){
    full$Aclass[i] <- "Baby"
  } else if(full[i,"Age"] >= 3 & full[i,"Age"] < 15){
    full$Aclass[i] <- "Child"
  } else if(full[i,"Age"] >= 15 & full[i,"Age"] < 60){
    full$Aclass[i] <- "Adult"
  } else {
    full$Aclass[i] <- "Old"
  }
} 



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

NA_finder(full)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

cat_vars <- c('Aclass','Pclass','Sex','Embarked','Title','FamNr','Deck')


full[cat_vars] <- lapply(full[cat_vars], function(x) as.factor(x))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

train_cl <- full[1:891,]
test_cl <- full[892:1309,]



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

set.seed(459) # graine pour creation aleatoire

random_for <- randomForest(factor(Survived) ~ Aclass + Pclass + Sex + Embarked + 
                                              Title + FamNr + Deck + Perptick,  
                                              data = train_cl)  # Modele

prev_age <- predict(random_for, test_cl) # Previsions


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

solution <- data.frame(PassengerID = test_cl$PassengerId, Survived = prev_age)

# Laisser en com - write.csv(solution, file = 'sub_titanic.csv', row.names = F)


