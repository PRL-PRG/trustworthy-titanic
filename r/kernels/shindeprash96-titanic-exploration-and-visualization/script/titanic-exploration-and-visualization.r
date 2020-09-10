
## Importing packages

suppressMessages(library('ggplot2'))
suppressMessages(library('ggthemes')) 
suppressMessages(library('scales')) 
suppressMessages(library('dplyr'))
suppressMessages(library('randomForest'))
suppressMessages(library('corrplot'))
suppressMessages(library('plyr'))
#loading data 
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

full  <- bind_rows(train, test) # test + train

options( warn = -1)
str(full)
summary(full)
#exploratary analysis 
# Age vs Survived
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins=30) + 
  theme_few() +
  xlab("Age") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Age vs Survived")
# Sex vs Survived
ggplot(full[1:891,], aes(Sex, fill = factor(Survived))) + 
  geom_bar(stat = "count", position = 'dodge')+
  theme_few() +
  xlab("Sex") +
  ylab("Count") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Sex vs Survived")
#pclass vs sex vs survived
ggplot(full[1:891,], aes(x = Age, y = Sex)) + 
  geom_jitter(aes(colour = factor(Survived))) + 
  theme_few()+
  theme(legend.title = element_blank())+
  facet_wrap(~Pclass) + 
  labs(x = "Age", y = "Sex", title = "Pclass vs Sex vs Age vs Survived")+
  scale_fill_discrete(name = "Survived") + 
  scale_x_continuous(name="Age",limits=c(0, 81)) 

