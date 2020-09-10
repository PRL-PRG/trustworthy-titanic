
library(tidyverse)
library(tree)
library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(ggpubr)
library(plotly)
library(gridExtra)

test <- read.csv("../input/test.csv", na.strings = "")
train <- read.csv('../input/train.csv', na.strings = "")
fulldata <- rbind(select(train,-Survived), test) 


test_ini <- test
train_ini <- train
full_ini <- fulldata

summary(train)

train$Survived <- factor(train$Survived, levels = c(0,1))
train$Pclass <- factor(train$Pclass, levels = c(1,2,3))

p1 <- train %>% ggplot(aes(Pclass, fill = Survived))+ geom_bar() + theme_classic() + scale_fill_discrete(guide = FALSE) 
p2 <- train %>% ggplot(aes(Sex, fill = Survived)) + geom_bar() + theme_classic() + scale_fill_discrete(guide = FALSE) 
p3 <- train %>% filter(is.na(Age) == FALSE) %>% ggplot(aes(Age, fill = Survived)) + geom_histogram(binwidth=5, alpha=.3, position="identity") + theme_classic() + scale_fill_discrete(guide = FALSE) 
p4 <- train %>% ggplot(aes(SibSp, fill = Survived)) + geom_bar(alpha=.3, position="identity") + theme_classic() + guides(fill = NULL)
p5 <- train %>% ggplot(aes(Parch, fill = Survived)) + geom_bar(alpha=.3, position="identity") + theme_classic() 
p6 <- train %>% ggplot(aes(Fare, fill = Survived)) + geom_histogram(binwidth = 10) + theme_classic() 
p7 <- train %>% ggplot(aes(Embarked, fill = Survived)) + geom_bar() + theme_classic()
ggarrange( ggarrange(p1,p2,p7, nrow = 1, ncol = 3), p6, nrow = 2,common.legend = TRUE)
ggarrange( p4,p5,p3, nrow = 1,ncol = 3, common.legend = TRUE)

train %>% ggplot() + 
  geom_jitter(aes(x = Age, y = '', colour = Survived)) + 
  facet_grid(factor(Pclass,levels = c(1,2,3), labels = c('Pclass = 1','Pclass = 2', 'Pclass = 3')) ~ Sex, scale = 'free') + 
  theme_bw() +
  labs(y = '') +
  theme(strip.text.y = element_text(size = 10), 
        strip.background = element_rect(fill = 'pink'),
        axis.ticks.y = element_blank()
       )


train %>% ggplot(aes(size = factor(Parch + SibSp),x = Age, y =Sex, colour = Survived)) + geom_jitter() + theme_classic() 

fulldata$Pclass <- as.factor(fulldata$Pclass)
fulldata$Name <- as.character(fulldata$Name)
fulldata$Age <- as.integer(fulldata$Age)
fulldata$Ticket <- as.character(fulldata$Ticket)
summary(fulldata)


fulldata <- select(fulldata,-Cabin)

filter(fulldata,is.na(Embarked))
table(fulldata$Embarked)

fulldata$Embarked[c(62,830)]<- 'S'

filter(fulldata, is.na(Fare))

fulldata$Fare[1044] <- fulldata %>% 
  filter(Pclass == 3 & Embarked == 'S') %>% 
  .$Fare %>%
  mean(., na.rm = TRUE)


namesplited <- strsplit(fulldata$Name, split = '[,.]')
fulldata$Title <- sapply(namesplited, FUN = function(x){sub(" ","",x[2])})

fulldata %>% 
  filter(is.na(Age)) %>% 
  select(Title) %>% 
  table()

fulldata %>% 
  filter(Title %in% c('Dr','Master','Miss','Mr','Mrs','Ms')) %>%
  select(Title) %>%
  table()

fulldata %>% 
  filter(Title == 'Ms')

fulldata$Title[c(444,980)] <- 'Miss'

fulldata %>% 
  filter(Title == 'Miss') %>%
  ggplot() + 
  geom_histogram(aes(Age,fill = factor(Parch)),binwidth = 2) +  
  scale_fill_brewer(palette = 'Pastel2') + 
  labs(fill = 'Number of Parch')

med1 <- fulldata %>% 
  filter(Title == 'Miss' & Parch > 0 & is.na(Age) == FALSE) %>% .$Age %>% median()
med2 <- fulldata %>% 
  filter(Title == 'Miss' & Parch == 0 & is.na(Age) == FALSE) %>% .$Age %>% median()
m <- c(med1,med2)
fulldata %>% 
  mutate(tempt = Parch/Parch) %>%
  filter(Title == 'Miss' & is.na(Age) == FALSE)  %>%  
  ggplot(aes(x = factor(tempt), y = Age)) + 
  geom_boxplot(aes(colour =factor(tempt, levels = c(1,'NaN'), labels = c('Parch > 0','Parch = 0')) ),width = 0.2) + 
  geom_text(data = data.frame(m,c(1,2)), aes(x = c(1,2)+0.15,y = m, label = m),color = 'purple') + 
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        legend.title = element_blank())


missage1 <- fulldata %>%
  filter(Title == 'Miss' & is.na(Age) & Parch > 0) %>% 
  pull(PassengerId)

fulldata$Age[missage1] <- 9.5

missage2 <- fulldata %>%
  filter(Title == 'Miss' & is.na(Age) & Parch == 0) %>% 
  pull(PassengerId)
fulldata$Age[missage2] <- 26 

fulldata %>% 
  filter(Title == 'Master') %>%
  select(Age) %>%
  summary() 

masterage <- fulldata %>%
  filter(Title == 'Master' & is.na(Age)) %>% 
  pull(PassengerId)
fulldata$Age[masterage] <- 5

fulldata %>%
  filter(Title == 'Dr')

fulldata$Age[767] <- fulldata %>% 
  filter(Pclass == 1 & Sex == 'male' & Title == 'Dr' & is.na(Age) == FALSE) %>% 
  .$Age %>%
  mean() 

# 44.75

fulldata %>% 
  filter(is.na(Age) == FALSE & Title %in% c('Mrs','Mr')) %>% 
  ggplot() + 
  geom_boxplot(aes(x = '', y = Age, fill = factor(Title))) + 
  theme_bw()  + 
  facet_grid(.~factor(Pclass, 
                      levels = c(1,2,3), 
                      labels = c('Pclass = 1','Pclass = 2', 'Pclass = 3')
                     )
            ) +
  labs(x = '') + 
  theme(legend.title = element_blank())

fulldata %>% 
  filter(is.na(Age) == FALSE & Title %in% c('Mrs','Mr')) %>% 
  group_by(Pclass, Title) %>% summarise(Med_Age = median(Age))

age1 <- fulldata %>% filter(Title == 'Mr' & Pclass == 1 & is.na(Age)) %>% pull(PassengerId)
age2 <- fulldata %>% filter(Title == 'Mrs' & Pclass == 1 & is.na(Age)) %>% pull(PassengerId)
age3 <- fulldata %>% filter(Title == 'Mr' & Pclass == 2 & is.na(Age)) %>% pull(PassengerId)
age4 <- fulldata %>% filter(Title == 'Mrs' & Pclass == 2 & is.na(Age)) %>% pull(PassengerId)
age5 <- fulldata %>% filter(Title == 'Mr' & Pclass == 3 & is.na(Age)) %>% pull(PassengerId)
age6 <- fulldata %>% filter(Title == 'Mrs' & Pclass == 3 & is.na(Age)) %>% pull(PassengerId)
fulldata$Age[age1] <- 41.5
fulldata$Age[age2] <- 45.0
fulldata$Age[age3] <- 30
fulldata$Age[age4] <- 30.5
fulldata$Age[age5] <- 26
fulldata$Age[age6] <- 31

summary(fulldata)

fulldata $ TravalSize <- fulldata $ SibSp + fulldata $ Parch

train_temp <- fulldata[1:891,] 
train_temp$Survived <- train$Survived

train_temp %>% ggplot() + 
  geom_jitter(aes(x = Age, y = '', colour = Survived)) + 
  facet_grid(factor(Pclass,levels = c(1,2,3), labels = c('Pclass = 1','Pclass = 2', 'Pclass = 3')) ~ Sex, scale = 'free') + 
  theme_bw() +
  labs(y = '') +
  theme(strip.text.y = element_text(size = 10), 
        strip.background = element_rect(fill = 'pink'),
        axis.ticks.y = element_blank()
       )

library(tree)

fulldata2 <- select(fulldata, -c(Name,SibSp,Parch,Ticket,Title))

train2 <- fulldata2[1:891,]

str(train2)

samp <- sample (1:nrow(train2), 700)


survival <- train$Survived
survival <- as.factor(survival)

survival_test <- survival[-samp]
testdf <- train2[-samp,]

treedf <- tree(survival ~ . -PassengerId, train2, subset = samp)
treePred <- predict(treedf, testdf, type = "class")

table(treePred, survival_test)

treemodel <- tree(survival ~ . - PassengerId, train2)

predictdf <- fulldata2[892:nrow(fulldata2), ]

prediction <- predict(treemodel,predictdf,type = 'class')

sub <- data.frame(test$PassengerId,prediction)

colnames(sub) <- c('PassengerId','Survived')

write.csv(sub,'MySubmission')


