## ----results="hide", message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(readr)
library(tidyverse)
library(caret)
library(corrplot)
library(randomForest)
library(RANN) #needed for imputing
library(stringr)
library(rpart)
library(party)
library(gridExtra)


## ----results="hide", message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")
test$Survived <- NA
titanicCombo<- rbind(train,test)


## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
colSums(is.na(titanicCombo[1:891,]))
round(colSums(100*(is.na(titanicCombo[1:891,])/nrow(titanicCombo[1:891,]))),3)


## ----results="hide", message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo <- titanicCombo %>% 
  mutate(Survived = as.factor(Survived), Pclass = as.factor(Pclass), Sex = as.factor(Sex), Embarked = as.factor(Embarked), Cabin = as.factor(ifelse(is.na(Cabin),0,1))) 


## ----message=FALSE, warning=FALSE, fig.width=10----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g1 <- titanicCombo[1:891,] %>% 
  ggplot() + 
  geom_bar(aes(x = Pclass, fill = Survived))
g2 <- titanicCombo[1:891,] %>% 
  ggplot() + 
  geom_bar(aes(x = Pclass, fill = Survived), position = "fill")
  
grid.arrange(g1,g2)


## ----message=FALSE, warning=FALSE, fig.width=10----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo[1:891,] %>% 
  ggplot() + 
  geom_bar(aes(x = Pclass, fill = str_c(Survived,Sex))) + 
  scale_fill_discrete("Survived | Sex")
titanicCombo[1:891,] %>% 
  ggplot() + 
  geom_bar(aes(x = Pclass, fill = Survived), position = "fill") + 
  facet_wrap(~Sex) 


## ----message=FALSE, warning=FALSE, fig.width=10, fig.height=7--------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo[1:891,] %>% 
  ggplot(aes(x = Age, y = Survived)) + 
  geom_jitter(aes(color = Sex)) + 
  facet_wrap(~Pclass) 


## ----message=FALSE, warning=FALSE, fig.width=10, fig.height=7--------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo[1:891,] %>% 
  ggplot(aes(x = Age, y = Survived)) +
  geom_jitter(aes(color = (as.factor(SibSp + Parch)), size = (as.factor(SibSp + Parch)))) + 
  facet_grid(Pclass~Sex) + scale_color_discrete("FamilySize") + 
  scale_size_discrete("FamilySize")


## ----message=FALSE, warning=FALSE, fig.width=8.6---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g1 <- titanicCombo[1:891,] %>% 
  ggplot(aes(x = as.factor(SibSp + Parch))) + 
  geom_bar(aes(fill = as.factor(SibSp + Parch))) + 
  labs(x="FamilySize") + 
  scale_fill_discrete(guide=FALSE) 

g2 <-titanicCombo[1:891,] %>% 
  ggplot(aes(x = as.factor(SibSp + Parch))) + 
  geom_bar(aes(fill = Survived),position = "fill")+ 
  labs(x="FamilySize") 
  
grid.arrange(g1,g2)


## ----message=FALSE, warning=FALSE, fig.width=10, fig.height=7--------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo[1:891,] %>% 
  ggplot(aes(x = Cabin)) + 
  geom_bar(aes(fill = Survived),position = "fill") +
  facet_grid(Sex~Pclass) 

titanicCombo[1:891,] %>% 
  ggplot(aes(x = Fare, y = Survived)) +
  geom_jitter(aes(color = (as.factor(SibSp + Parch)), size = (as.factor(SibSp + Parch)))) + 
  facet_grid(Sex~Pclass, scales = "free") + 
  scale_color_discrete("FamilySize") + 
  scale_size_discrete("FamilySize") 


## ----message=FALSE, warning=FALSE, fig.width=10, fig.height=7--------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo[1:891,] %>% filter(Pclass==3, Sex=="male", Fare > 55, (SibSp + Parch) < 1)


## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo[1:5,]


## ----message=FALSE, warning=FALSE, fig.width=10, fig.height=7--------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo <- titanicCombo %>% 
  mutate(Title = str_extract(Name,".[a-z]+\\."), Title = as.factor(Title))

titanicCombo[1:891,] %>% 
  group_by(Title) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  ggplot() + 
  geom_bar(aes(x = reorder(Title, desc(n), FUN = median), y = n, fill = Title), stat = "identity") + 
  labs(x = "Title", y = "Count") +
  scale_fill_discrete(guide=FALSE) 


## ----message=FALSE, warning=FALSE, fig.width=10, fig.height=7--------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo <- titanicCombo %>% 
  mutate(Title3 = ifelse(str_detect(Title,"Master."),as.character(Title),ifelse(str_detect(Title,"Mr."),as.character(Title),ifelse(str_detect(Title,"Mrs."),as.character(Title),ifelse(str_detect(Title,"Ms."),as.character("Mrs."),ifelse(str_detect(Title,"Miss."),as.character(Title),"OTHER")))))) %>% 
  mutate(Title3 = as.factor(Title3))



## ----message=FALSE, warning=FALSE, fig.width=10, fig.height=7--------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo %>% group_by(Title3) %>% summarise(n = n(), Age.NA = sum(is.na(Age))) %>% arrange(desc(n))


## ----message=FALSE, warning=FALSE, fig.width=10, fig.height=7--------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo[1:891,] %>% filter(!is.na(Age)) %>% group_by(Title3) %>% summarise(n = n(), MinAge = min(Age), MedianAge = median(Age), MaxAge = max(Age)) %>% arrange(desc(n))


## ----Feature6, message=FALSE, warning=FALSE, fig.width=10, fig.height=7----------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo <- titanicCombo %>% 
  mutate(Age = ifelse(is.na(Age) & as.character(Title3) == "Master.",3.5,Age))


## ----message=FALSE, warning=FALSE, fig.width=10, fig.height=7--------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo$LastName = sapply(strsplit(titanicCombo$Name, ", "), `[`, 1)
sum(is.na(titanicCombo$LastName))



## ----message=FALSE, warning=FALSE, fig.width=10, fig.height=7--------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo <- titanicCombo %>% mutate(Ticket2 = str_extract(Ticket,"[[:digit:]]+$")) %>% mutate(Ticket2 = ifelse(is.na(Ticket2),Ticket,Ticket2)) 
sum(is.na(titanicCombo$Ticket2))


## ----message=FALSE, warning=FALSE, fig.width=10, fig.height=7--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Bring in ONLY the training data
titanicSQL <- titanicCombo[1:891,]

#Finding Men
titanicMen <- titanicSQL %>% mutate(Uniq = Ticket2) %>%
  group_by(Uniq,Sex) %>% 
  summarise(Men = n()) %>% #Men on same Ticket
  filter(str_to_lower(Sex) == "male") %>%
  arrange(desc(Men)) %>% ungroup(Uniq)

#Finding Women
titanicWomen <- titanicSQL %>% mutate(Uniq = Ticket2) %>% 
  group_by(Uniq,Sex) %>% 
  summarise(Women = n()) %>% #Women on same Ticket 
  filter(str_to_lower(Sex) == "female") %>%
  arrange(desc(Women))

#Finding Fares
titanicFare <- titanicSQL %>% mutate(Uniq2 = Fare) %>% 
  group_by(Uniq2) %>% 
  summarise(Fare = n()) %>% #Women on same Ticket 
  arrange(desc(Fare))

#Join Ticket information
titanicCombo  <- titanicCombo %>% mutate(Uniq = Ticket2) %>% 
  left_join(titanicMen ,by = "Uniq") %>% 
  left_join(titanicWomen ,by = "Uniq") %>%
  select(1:16, 19, 21) %>% 
  mutate(Men = ifelse(is.na(Men) & Sex.x == "male",1,ifelse(is.na(Men),0,Men))) %>% 
  mutate(Women = ifelse(is.na(Women) & Sex.x == "female",1,ifelse(is.na(Women),0,Women))) %>% 
  mutate(TicketSize = Men + Women) 

#Join Fare information
titanicCombo <- titanicCombo %>% mutate(Uniq2 = Fare) %>% 
  left_join(titanicFare ,by = "Uniq2") %>%
  mutate(Fare.y = ifelse(is.na(Fare.y),1,Fare.y)) %>%
  select(1:19,21) %>% 
  rename(FareSize = Fare.y) %>%
  mutate(FamilySize = SibSp + Parch)



## ----message=FALSE, warning=FALSE, fig.width=10, fig.height=7--------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo[1:891,] %>% filter(Ticket==1601) %>% select(14:21)

## ----message=FALSE, warning=FALSE, fig.width=10, fig.height=7--------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanicCombo[1:891,] %>% 
  ggplot(aes(x = Fare.x, y = Survived)) +
  geom_jitter(aes(color = (as.factor(TicketSize)), size = (as.factor(TicketSize)))) + 
  facet_grid(Sex.x~Pclass, scales = "free")  + 
  scale_color_discrete("TicketSize") + 
  scale_size_discrete("TicketSize") 


## ----eval=FALSE, message=FALSE, warning=FALSE, fig.width=10, fig.height=7--------------------------------------------------------------------------------------------------------------------------------------------------------
## #Remove unneeded columns
## titanicCombo <- titanicCombo %>%
##   select(-PassengerId,-Name,-Ticket,-LastName,-Parch, -SibSp, -Title, -Ticket2)
## 
## #Create the Dummy Model
## DummyRF_mod <- dummyVars(~Pclass + Sex.x + Title3 + Embarked,
##                          data = titanicCombo,
##                          levelsOnly = FALSE)
## #This actually creates the Dummy Variables
## titanicCombo_Dummy <- as.tibble(predict(DummyRF_mod,newdata = titanicCombo))
## 
## #Now we bind those two data sets and remove the old columns we created the new ones from.
## titanicCombo_Dummy <- cbind(titanicCombo,titanicCombo_Dummy)
## titanicCombo_Dummy <- as.tibble(titanicCombo_Dummy)
## titanicCombo_Dummy <- titanicCombo_Dummy %>%
##   select(-Pclass, -Sex.x,-Title3, -Embarked)
## 


## ----eval=FALSE, message=FALSE, warning=FALSE, fig.width=10, fig.height=7--------------------------------------------------------------------------------------------------------------------------------------------------------
## titanicCrfTune_Dummy <- train(Survived ~.,
##                               data = titanicCombo_Dummy[1:891,],
##                               method = "cforest",
##                               preProc = c("knnImpute"),
##                               trControl = trainControl(method = "cv"),
##                               na.action = na.pass)

