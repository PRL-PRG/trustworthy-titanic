
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(ggthemes)
library(scales)
library(plyr)
library(stringr)
library(InformationValue) #to compute WOE and IV
library(MLmetrics)
library(rpart)
library(randomForest)
library(dplyr)
library(e1071)
library(party)
library(gbm)
library(class)
library(ggmap)
library(caret)
library(rpart.plot)
library(RColorBrewer)

## Upload data into console
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)



## Pull titles ("Mr", "Mrs, etc) from names to use as separate variable
titles <- gsub(".*, (.*?)\\..*", "\\1", train$Name)
train$Title<-titles


## Create data to plot frequency of titles
freq_data<- train %>%
  group_by(Title) %>%
  summarize(Freq=n())

## Plot frequency of titles
ggplot(freq_data, aes(x=Title, y=Freq, fill=Title))+geom_bar(stat = "identity")+
  geom_text(aes(label=Freq), data = freq_data, vjust=-0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")

## Since the frequency of passengers with titles such as "Mr", "Mrs", etc, is
## significantly higher than that of the other titles, we can separate the "other"
## less frequent titles in one group called "Special"
common<-c("Mr", "Mrs", "Miss", "Master")
train<-train %>%
  mutate(Title=replace(Title, !(Title %in% common), "Special"))

survivalbytitle<-train %>%
  group_by(Title) %>%
  summarize(Percent_Survived = mean(Survived))

  
ggplot(survivalbytitle, aes(x=Title, y=Percent_Survived*100, fill=Title))+
  geom_text(aes(label=paste0(round(Percent_Survived*100,2), "%")),vjust=-0.5)+
  geom_bar(stat = "identity")+
  labs(title="Survival Rate by Title", y="Percent that Survived")+
  theme(legend.position = "none")

survivalbysex<-train %>%
  group_by(Sex) %>%
  summarize(Percent_Survived = mean(Survived))

ggplot(survivalbysex, aes(x=Sex, y=Percent_Survived, fill=Sex))+
  geom_text(aes(label=paste0(round(Percent_Survived*100, 2), "%")), vjust=-0.5)+
  geom_bar(stat = "identity")+
  labs(title="Survival Rate by Sex", y="Percent that Survived")+
  theme(legend.position = "none")

## Survival Prediction Using CTree

# List character columns
chars<-sapply(train, is.character)

# Convert to factors

train[chars]<-lapply(train[chars], as.factor)



# The model

fit<-rpart(Survived~Pclass+Sex+Age+Parch+Fare+Title,
            data = train,
            method = "class")
plot(fit)
text(fit)

rpart.plot(fit)


