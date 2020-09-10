
library(readr) # Reading in data
library(dplyr) # Data manipulation
library(tibble) # Data manipulation
library(ggplot2) # Data visualization
library(ggthemes) # Data visualization
library(RColorBrewer) # Data visualization

titanic <- read_csv("../input/train.csv")
head(titanic)

titanic <- titanic %>%
  mutate(Pclass = factor(Pclass), 
         Survived = factor(Survived), 
         Sex = factor(Sex)) 

survival <- table(titanic$Survived) %>%
  as_tibble() %>%
  rename(Survived = Var1, Count = n)

survival

survival_ratio <- prop.table(table(titanic$Survived)) %>%
  as_tibble() %>%
  rename(Survived = Var1, Percentage = n) %>%
  mutate(Percentage = round(Percentage, 2)*100)

survival_ratio

titanic %>%
  ggplot() +
  geom_bar(aes(x = Survived, fill = Survived)) +
  geom_text(data = survival, 
            aes(x = Survived, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25,
            fontface = "bold") +
  geom_label(data = survival_ratio, 
             aes(x = Survived, y = Percentage, label = paste0(Percentage, "%"), group = Survived), 
             position = position_stack(vjust = 5)) +
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Titanic Total Survival Rate") +
  scale_x_discrete(name= "Survival Rate", labels = c("Did Not Survive", "Survived")) +
  scale_y_continuous(name = "Passenger Count") +
  scale_fill_discrete(name = "Outcome", labels = c("Did Not Survive", "Survived"))

gender <- titanic %>%
  group_by(Sex) %>%
  summarise(Count = n())

gender

gender_ratio <- titanic %>%
  group_by(Sex, Survived) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))

gender_ratio

titanic %>%
  ggplot() +
  geom_bar(aes(x = Sex, fill = Survived)) +
  geom_text(data = gender, 
            aes(x = Sex, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = gender_ratio, 
             aes(x = Sex, y = Count, label = paste0(Percentage, "%"), group = Survived), 
             position = position_stack(vjust = 0.5)) +
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Titanic Gender Survival Rate") +
  scale_x_discrete(name= "Gender") +
  scale_y_continuous(name = "Passenger Count") +
  scale_fill_discrete(name = "Outcome", labels = c("Did Not Survive", "Survived"))  

pclass <- titanic %>%
  group_by(Pclass) %>%
  summarise(Count = n())

pclass

pclass_ratio <- titanic %>%
  group_by(Pclass, Survived) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))

pclass_ratio

titanic %>%
  ggplot() +
  geom_bar(aes(x = Pclass, fill = Survived)) +
  geom_text(data = pclass, 
            aes(x = Pclass, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = pclass_ratio, 
             aes(x = Pclass, y = Count, label = paste0(Percentage, "%"), group = Survived), 
             position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Titanic Pclass Survival Rate") +
  scale_x_discrete(name= "Pclass") +
  scale_y_continuous(name = "Passenger Count") +
  scale_fill_discrete(name = "Outcome", labels = c("Did Not Survive", "Survived"))  

pclass_gender <- titanic %>%
  group_by(Pclass) %>%
  summarise(Count = n())

pclass_gender

pclass_gender_ratio <- titanic %>%
  group_by(Pclass, Sex) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))

pclass_gender_ratio

titanic %>%
  ggplot() +
  geom_bar(aes(x = Pclass, fill = Sex)) +
  geom_text(data = pclass_gender, 
            aes(x = Pclass, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = pclass_gender_ratio, 
             aes(x = Pclass, y = Count, label = paste0(Percentage, "%"), group = Sex), 
             position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Titanic Gender Proportion by Ticket Class") +
  scale_x_discrete(name= "Pclass") +
  scale_y_continuous(name = "Passenger Count") +
  scale_fill_brewer(name = "Gender", labels = c("Female", "Male"), palette = "Paired")

pclass_gender_ratio <- titanic %>%
  group_by(Pclass, Sex) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))

pclass_gender_ratio

pclass_gender_survived_ratio <- titanic %>%
  group_by(Pclass, Sex, Survived) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))

pclass_gender_survived_ratio

# Using facet_wrap(~ Pclass)
titanic %>%
  ggplot() +
  geom_bar(aes(x = Sex, fill = Survived)) +
  facet_wrap(~ Pclass) +
  geom_text(data = pclass_gender_ratio, 
            aes(x = Sex, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust= -1.5, 
            fontface = "bold") +
  geom_label(data = pclass_gender_survived_ratio, 
             aes(x = Sex, y = Count, label = paste0(Percentage, "%"), group = Survived), 
             position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Titanic Gender Survival Rate by Pclass") +
  scale_x_discrete(name= "Gender by Pclass ") +
  scale_y_continuous(name = "Passenger Count", limits = c(0,360)) +
  scale_fill_discrete(name = "Outcome", labels = c("Did Not Survive", "Survived"))  

# Using facet_grid(Sex ~ Pclass) to separate Gender and Pclass
titanic %>%
  ggplot() +
  geom_bar(aes(x = Survived, fill = Survived)) +
  facet_grid(Sex ~ Pclass) +
  geom_text(data = pclass_gender_survived_ratio, 
            aes(x = Survived, y = Count, label = paste0(Percentage, "%")), 
            position = position_dodge(width=0.9), 
            vjust= -0.5, 
            fontface = "bold") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Titanic Gender Survival Rate by Pclass") +
  scale_x_discrete(name= "Survival Rate", labels = c("No", "Yes")) +
  scale_y_continuous(name = "Passenger Count", limits = c(0,360)) +
  scale_fill_discrete(name = "Outcome", labels = c("Did Not Survive", "Survived"))  

# Remove Missing Values (177 NA Values)
median(titanic$Age, na.rm = TRUE)

titanic %>%
  ggplot() +
  geom_histogram(aes(x = Age), binwidth = 5, color = "#355a63", fill = "#96e4f7") +
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Titanic Age Distribution") +
  scale_x_continuous(name= "Passenger Age", breaks = 5*c(0:18)) +
  scale_y_continuous(name = "Passenger Count")

titanic %>%
  ggplot() +
  geom_histogram(aes(x = Age, fill = Survived), binwidth = 5, color = "#355a63") +
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Titanic Survival Rate by Age") +
  scale_x_continuous(name= "Passenger Age", breaks = 5*c(0:18)) +
  scale_y_continuous(name = "Passenger Count") +
  scale_fill_discrete(name = "Outcome", labels = c("Did Not Survive", "Survived"))  

titanic %>%
  ggplot() +
  geom_histogram(aes(x = Age, fill = Survived), binwidth = 5, color = "#355a63") +
  facet_grid(Sex ~ Pclass) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Titanic Survival Rate by Age, Gender and Class") +
  scale_x_continuous(name= "Passenger Age", breaks = 10*c(0:8)) +
  scale_y_continuous(name = "Passenger Count") +
  scale_fill_discrete(name = "Outcome", labels = c("Did Not Survive", "Survived"))  

titanic <- titanic %>%
  mutate(FamilySize = 1 + SibSp + Parch) 

titanic %>%
  group_by(FamilySize, Survived) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))

titanic %>%
  ggplot() +
  geom_histogram(aes(x = FamilySize, fill = Survived), binwidth = 1) +
  facet_grid(Sex ~ Pclass) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Titanic Survival Rate by Family Size") +
  scale_x_continuous(name = "Family Size") +
  scale_y_continuous(name = "Passenger Count") +
  scale_fill_discrete(name = "Outcome", labels = c("Did Not Survive", "Survived"))  

titanic %>%
  ggplot() +
  geom_point(aes(x = Age, y = FamilySize, color = Survived), alpha = 0.7) +
  facet_grid(Sex ~ Pclass) +
  theme_bw() +
  theme(plot.title = element_text(size=18, color = "#054354")) +
  ggtitle("Survival Rate by Gender, Class, Age, and Family Size") +
  scale_x_continuous(name= "Passenger Age", breaks = 10*c(0:8)) +
  scale_y_continuous(name = "Family Size") +
  scale_color_discrete(name = "Outcome", labels = c("Did Not Survive", "Survived"))  
