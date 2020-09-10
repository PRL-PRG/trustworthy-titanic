
library(tidyverse)
list.files=(path="../input")
train<-read_csv('../input/train.csv')
test<-read_csv('../input/test.csv')
test$Survived<-NA
full<-rbind(train,test)
str(full)
dim(full)
head(full)
summary(full)
