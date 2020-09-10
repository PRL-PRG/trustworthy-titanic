
#Load the libraries
library(ggplot2)
library(dplyr)


#Load the Data
Traindataset<-read.csv("../input/train.csv", stringsAsFactors = F)
Testdataset<-read.csv("../input/test.csv", stringsAsFactors = F)

#Research Question: 
#According to the provided data of Titanic Disaster we are asked to 0find out the information of people who survived . 
#Observing the given data sets , I found these variables more impactable:
#Survived , Sex and Age
#I am going to calculate here :

#(1) who survived more? : Males or Females, i.e. survival of people based on their sex

#(2) who survived more? ,i.e. Survival based on their Age.

#Data Exploration
names(Traindataset)
str(Traindataset)

#Data Preprocessing for Research question 1
# Number of persons survived based on their Sex : Male or Female

Traindataset %>% 
  filter(Sex != "NA") %>%
  group_by(Sex) %>% 
  summarise(count = n())

Survived_MF<- Traindataset[ which(!is.na(Traindataset$Sex) & !is.na(Traindataset$Survived)), ]
Survived_MF <- Survived_MF %>% select(Sex, Survived)

ggplot(aes(x= Sex, y= Survived), data=Survived_MF) + geom_boxplot() + ggtitle('Survived People')
#Thus, we can say that Females survived more than Males in Titanic Tragedy.

#Data Preprocessing for Research question 2
#Survival based on Age of people

Traindataset$Age <- as.integer(Traindataset$Age)
Traindataset %>% 
  filter(Age != "NA") %>%
 mutate(Age = ifelse(Age > 0, "Positive_Sur", "Negative_Sur"))
Traindataset %>%  
  group_by(Age) %>% 
  summarise(count = n())

Traindataset %>% 
  filter(!(is.na(Age))) %>%
  summarise(Agemean = mean(Age), Agemedian = median(Age), Agesd = sd(Age), 
            Agemin = min(Age), Agemax = max(Age))

Traindataset <- Traindataset %>% 
    filter(!(is.na(Age))) %>%
   mutate(Age = ifelse(Age >= 28, "HighAge", "LowAge"))
Traindataset %>% 
  group_by(Age) %>% 
  summarise(count = n())
  
  ggplot(aes(x= Age, y= Survived), data=Traindataset) + geom_boxplot() + ggtitle('Survived People')

#Number of Persons positive survived and negative survived
Traindataset <- Traindataset %>% 
    filter(!(is.na(Survived))) %>%
  mutate(Survived = ifelse(Survived > 0, "Positive_Sur", "Negative_Sur"))
Traindataset %>% 
  group_by(Survived) %>% 
  summarise(count = n())
  