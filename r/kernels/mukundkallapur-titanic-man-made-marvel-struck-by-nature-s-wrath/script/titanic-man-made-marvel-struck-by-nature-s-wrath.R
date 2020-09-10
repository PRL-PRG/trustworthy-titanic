## ----setup, include=FALSE, message=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE,warning = FALSE, error = TRUE)


## ----Setup, message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Load the library and read the file. 
library(tidyverse)
library(DMwR)

titanic_vis <- read.csv("../input/train.csv")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(titanic_vis)
dim(titanic_vis)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Conversion of the attributes 'Survived' and 'PClass' to 'Categorical'

titanic_vis$Survived <- as.factor(titanic_vis$Survived)
titanic_vis$Pclass <- as.factor(titanic_vis$Pclass)

### Recode the attributes Survived, PClass, Embarked and Gender for better Visualization. 
titanic_vis$Survived <-  ifelse(titanic_vis$Survived == "1","Yes", "No")
titanic_vis$Pclass <- ifelse(titanic_vis$Pclass == "1", "First",
                          ifelse(titanic_vis$Pclass == "2", "Second", "Third"))
titanic_vis$Sex <- ifelse(titanic_vis$Sex == "male", "Male", "Female")

titanic_vis$Embarked <- ifelse(titanic_vis$Embarked == "C", "Cherbourg",
                             ifelse(titanic_vis$Embarked == "Q", "Queenstown", 
                             ifelse(titanic_vis$Embarked == "S","Southampton","Southampton")))
                             
## Impute the Age

#summary(titanic_vis)
#sum(is.na(titanic_vis))
titanic_vis <- DMwR :: centralImputation(titanic_vis)
                             
## Binning the Age manually and splitting into groups using recode method. 

titanic_vis$Age <- ifelse(titanic_vis$Age <=1, "Baby",
                      ifelse((titanic_vis$Age > 1 & titanic_vis$Age <= 12), "Kid",
                             ifelse((titanic_vis$Age > 12 & titanic_vis$Age <= 19), "Teen",
                                    ifelse((titanic_vis$Age > 19 & titanic_vis$Age <= 40), "Young",
                                           ifelse((titanic_vis$Age > 40 & titanic_vis$Age <= 60), 
                                                  "Middle","Senior")))))
                                                  
titanic_vis$Age <- as.factor(titanic_vis$Age)                             
                             


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_vis %>%
  group_by(Survived) %>% 
      summarise(n=n()) %>% 
        ggplot() +
          geom_col(aes(reorder(Survived, -n),y=n, fill = Survived), width = 0.4) +
            xlab("Survival of Passengers") + ylab("Count") + 
              ggtitle("Titanic Survival Count") +
                  theme_minimal() + 
                    theme(plot.title = element_text(hjust = 0.5, face = "bold"),axis.title.x.bottom = element_text(face = "bold"), 
                        axis.title.y.left = element_text(face = "bold")) +
                            geom_text(aes(x = Survived, y = n, label = n), vjust = -0.5, stat = "sum", show.legend = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_vis %>%
  group_by(Pclass, Survived) %>%
    dplyr :: summarise(n = n()) %>% 
      ggplot(aes(x = reorder(Pclass, -n), y = n, fill = Survived)) + 
          geom_bar(position = "dodge",stat = "identity", color = "black")  +
                geom_text(aes(label = n),position=position_dodge(width=0.9), vjust=-0.25) +
                    xlab("Passenger Class") + ylab("Count") + 
                        ggtitle("Titanic Passenger Class vs Survival") +
                            theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),axis.title.x.bottom = element_text(face = "bold",
  vjust = 3,size = 15), axis.title.y.left = element_text(face = "bold", size = 15),
  axis.text.x.bottom = element_text(face = "bold",size = 12,vjust = 3))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_vis %>%
  group_by(Sex, Survived) %>%
    summarise(n = n()) %>% 
      ggplot(aes(x = reorder(Sex,-n), y = n, fill = Survived)) + 
          geom_bar(position = position_stack(), stat = "identity", width = 0.7, color = "black")  +
            geom_text(aes(label = n),position=position_stack(vjust = 0.5), size = 5) +
                             xlab("Passenger Gender") + ylab("Count") + 
                    ggtitle("Titanic Passenger Gender vs Survival") +
                      theme_minimal() +
  scale_fill_brewer(palette = 15) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),axis.title.x.bottom = element_text(face = "bold",vjust = 3,size = 15), axis.title.y.left = element_text(face = "bold", size = 15),axis.text.x.bottom = element_text(face = "bold",size = 12,vjust = 3))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_vis %>%
  group_by(Embarked, Survived) %>%
  
  summarise(n = n()) %>%
  
  ggplot(aes(x = reorder(Embarked, -n), y = n, fill = Survived)) + 
  geom_bar(position = position_stack(), stat = "identity", width = 0.7, color = "black")  +

  geom_text(aes(label = n),position=position_stack(vjust = 0.5), size = 5) +
  xlab("Embarkation Point") + ylab("Count") + 
  ggtitle("Embarked against Survival") +
  theme_minimal() +
  scale_fill_brewer(palette = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x.bottom = element_text(face = "bold",vjust = 3,size = 15), 
        axis.title.y.left = element_text(face = "bold", size = 15),
        axis.text.x.bottom = element_text(face = "bold",size = 12,vjust = 3))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_vis %>%
  select(Age,Survived) %>%
    group_by(Age, Survived) %>%
        dplyr :: summarise(n = n()) %>%
            arrange(desc(Age)) %>%
  ggplot() +
  #coord_flip()+
        geom_col(aes(x=reorder(Age, -n), y = n,fill = Age)) +
          xlab("Age") + ylab("Count") + 
            facet_grid(~ fct_rev(Survived),scales = "free") +
                            guides(fill = guide_legend(title = "Age Categories"))  +
                    ggtitle("Survival vs Age")  +
                        theme_minimal() + 
                            theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                                axis.title.x.bottom = element_text(face = "bold"), 
                                    axis.title.y.left = element_text(face = "bold"), 
                                        axis.text.x.bottom = element_text(face = "bold"),
                                            legend.title = element_text(face = "bold")) +
                                                geom_text(aes(x = Age, y = n, label = n, vjust = -0.5))

