## ----setup, include=TRUE, warning=FALSE, message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(flexdashboard)
library(ggplot2)
library(readr)
library(highcharter)
library(dplyr)
library(bindrcpp)
library(gridExtra)
library(rpart)
library(corrplot)
library(randomForest)

train <- read.csv('../input/train.csv')
test <- read.csv('../input/test.csv')
total <- rbind(train[,-2],test)

# Keeping a version of the original dataset, will be used later
total2 <- total


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
total$Title <- gsub('(.*, )|(\\..*)', '', total$Name)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Also reassign mlle, ms, and mme accordingly
total$Title[total$Title == 'Mlle']        <- 'Miss' 
total$Title[total$Title == 'Ms']          <- 'Miss'
total$Title[total$Title == 'Mme']         <- 'Mrs' 
total$Title[total$Title %in% rare_title]  <- 'Rare Title'
total$Title <- factor(total$Title)
# Show title counts by sex again
# table(train$Sex, train$Title)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Finally, grab surname from passenger name
total$Surname <- sapply(as.character(total$Name),  
                        function(x) strsplit(x, split = '[,.]')[[1]][1])

total$Surname <- factor(total$Surname)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
total$Mother <- 'Not Mother'
total$Mother[total$Sex == 'female' & total$Parch > 0 & total$Age > 18 & total$Title != 'Miss'] <- 'Mother'
total$Mother <- factor(total$Mother)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- train[train$Embarked!='',]
train$Embarked <- factor(train$Embarked, labels = c("Cherbourg", "Queenstown", "Southhampton"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Num_NAtrain<-sapply(train[,-2],function(y)length(which(is.na(y)==T)))
Train_Empty<-sapply(train[,-2],function(y)length(which(y=="")))
Num_NAtest<-sapply(test,function(y)length(which(is.na(y)==T)))
Test_Empty<-sapply(test,function(y)length(which(y=="")))
NA_Count<- data.frame(NATrain=Num_NAtrain,NATest=Num_NAtest,Empty_Train = Train_Empty, Test_Empty = Test_Empty,
                      Sum_NA=Num_NAtrain+Num_NAtest,Sum_Empty=Train_Empty+Test_Empty)
NA_Count


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Is there any information we can obtain from the variable "Ticket"?
total$ticketclass <- substr(total$Ticket, 1, 4)
total$ticketclass <- as.numeric(as.integer(factor(total$ticketclass)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data1 <- total[!is.na(total$Age),-c(1,3,8,10,13)]
data2 <- total[is.na(total$Age),]
data1train <- data1[1:800,]
data1test <- data1[-c(1:800),]
model <- rpart(Age~., data1train,control = rpart.control(cp = 0.01, minsplit = 30, xval = 700,maxdepth = 20, minbucket = 1))
modelrf <- randomForest(Age~., data1train, ntree = 300, nodesize=1)
data2$Age <- NULL
data3 <- data2
data2$Age <- predict(model,data2[,-c(1,3,7,9)])
data3$Age <- predict(modelrf,data2[,-c(1,3,7,9)])
data1 <- total[!is.na(total$Age),]
total_merged <- rbind(data1,data2)
total_merged2 <- rbind(data1,data3)


## ---- echo=F---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,3))
hist(total$Age, freq=F, main='Age: Original Data', 
     col='yellow', ylim=c(0,0.05))
hist(total_merged$Age, freq=F, main='Age: Rpart output', 
     col='darkgreen', ylim=c(0,0.05))
hist(total_merged2$Age, freq=F, main='Age: Random Forest output', 
     col='lightgreen', ylim=c(0,0.05))


## ---- include=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
total <- total_merged2

total <- total[order(total$PassengerId),]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_new <- total[1:889,]
train <- cbind(train_new,Survived = train$Survived)


## ---- echo = F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tmp_male <- total2 %>%  filter(Sex=="male", !is.na(Age)) %>% select(Age) %>% .[[1]]
b <- hist(tmp_male, 20, plot=FALSE)
tmp_female <- total2 %>%  filter(Sex=="female", !is.na(Age)) %>% select(Age) %>% .[[1]]
a <- hist(tmp_female, breaks = b$breaks, plot=FALSE)

df <- data.frame(Age=c(a$mids,b$mids),Density=c(a$density,b$density),Sex=c(rep("female",length(a$mids)),rep("male",length(b$mids))))

highchart() %>% 
  hc_add_series(name="female", select(filter(df,Sex=="female"),Density)[[1]], type="column", color='rgba(255, 192, 203, 0.30)', showInLegend=FALSE) %>% 
  hc_add_series(name="male", select(filter(df,Sex=="male"),Density)[[1]], type="column", color='rgba(68, 170, 255, 0.30)', showInLegend=FALSE) %>% 
  hc_add_series(name="male", select(filter(df,Sex=="male"),Density)[[1]], type="spline", color="#44AAFF") %>% 
  hc_add_series(name="female", select(filter(df,Sex=="female"),Density)[[1]], type="spline", color="#FFC0Cb") %>% 
  hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:.3f}<br/>",
             shared = FALSE) %>% 
  hc_yAxis(title=list(text='Density')) %>% 
  hc_xAxis(title=list(text='Age'))   


## ---- echo = F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tmp_male <- total %>%  filter(Sex=="male", !is.na(Age)) %>% select(Age) %>% .[[1]]
b <- hist(tmp_male, 20, plot=FALSE)
tmp_female <- total %>%  filter(Sex=="female", !is.na(Age)) %>% select(Age) %>% .[[1]]
a <- hist(tmp_female, breaks = b$breaks, plot=FALSE)

df <- data.frame(Age=c(a$mids,b$mids),Density=c(a$density,b$density),Sex=c(rep("female",length(a$mids)),rep("male",length(b$mids))))

highchart() %>% 
  hc_add_series(name="female", select(filter(df,Sex=="female"),Density)[[1]], type="column", color='rgba(255, 192, 203, 0.30)', showInLegend=FALSE) %>% 
  hc_add_series(name="male", select(filter(df,Sex=="male"),Density)[[1]], type="column", color='rgba(68, 170, 255, 0.30)', showInLegend=FALSE) %>% 
  hc_add_series(name="male", select(filter(df,Sex=="male"),Density)[[1]], type="spline", color="#44AAFF") %>% 
  hc_add_series(name="female", select(filter(df,Sex=="female"),Density)[[1]], type="spline", color="#FFC0Cb") %>% 
  hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:.3f}<br/>",
             shared = FALSE) %>% 
  hc_yAxis(title=list(text='Density')) %>% 
  hc_xAxis(title=list(text='Age'))   


## ---- tidy=T, echo=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tmp <- train %>%  group_by(Title) %>% summarize(Survived = mean(Survived))
tmp$colors <- c("#d35400", "#2980b9", "#2ecc71", '#e4d354', '#2b908f')
hchart(tmp, "column", hcaes(x = Title, y = Survived, color=colors)) %>% 
  hc_tooltip(pointFormat = "{point.y:.2f}</br>",shared = FALSE)



## ---- echo=F---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tmp <- train %>% filter(!(Embarked=="")) %>% group_by(Embarked) %>% tally() %>% mutate(Percent = n/sum(n))
tmp$colors <- c("#d35400", "#2980b9", "#2ecc71")
tmp <- arrange(tmp,desc(Percent))
highchart() %>% 
  hc_xAxis(categories = c("Southhampton", "Cherbourg", "Queenstown")) %>%
  hc_yAxis(title=list(text='Percentage')) %>%
  hc_add_series(tmp, "bar", hcaes(x = Embarked, y = Percent, color=colors)) %>% 
  hc_tooltip(pointFormat = "{point.y:.2f}</br>",shared = FALSE) %>% 
  hc_legend(enabled=FALSE)



## ---- echo=F---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tmp <- train %>%  group_by(Pclass) %>% summarize(Survived = mean(Survived))
tmp$colors <- c("#d35400", "#2980b9", "#2ecc71")
hchart(tmp, "column", hcaes(x = Pclass, y = Survived, color=colors)) %>%  
  hc_tooltip(pointFormat = "{point.y:.2f}</br>",shared = FALSE)



## ---- echo=F---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tmp <- train %>% filter(!(Embarked=="")) %>% group_by(Embarked) %>% summarize(Survived = mean(Survived))
tmp$colors <- c("#d35400", "#2980b9", "#2ecc71")
hchart(tmp, "column", hcaes(x = Embarked, y = Survived, color=colors))


## ---- echo=F---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tmp <- train %>% group_by(Sex) %>% tally() %>% mutate(pct = n/sum(n))
tmp$colors <- c('#7cb5ec', '#434348')

hchart(tmp, "pie", hcaes(x = Sex, y = pct, color=colors))


## ---- echo=F---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

train$Parch = factor(train$Parch)

tmp <- train %>% group_by(Parch) %>% tally() %>% mutate(pct = n/sum(n))
tmp$colors <- c('#7cb5ec', '#434348', '#90ed7d', '#f7a35c', '#8085e9', 
   '#f15c80', '#e4d354')

hchart(tmp, "pie", hcaes(x = Parch, y = pct, color=colors))


## ---- echo=F---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


par(mfrow=c(1,2))
hist(train$Age, freq=T, main='Income: Train Data', 
     col='darkgreen', ylim=c(0,55),breaks = 50)
hist(test$Age, freq=T, main='Income: Test Data', 
     col='lightgreen', ylim=c(0,55),breaks = 50)


## ---- echo=F, warning=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(total, aes(x=Pclass,y=Fare,fill=Fare,colour=Fare)) + 
  geom_bar(stat='identity') + facet_grid(Sex ~ .)


## ---- echo=F---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tmp <- train %>%  group_by(Mother) %>% summarize(Survived = mean(Survived))
tmp$colors <- c("#d35400", "#2980b9")
hchart(tmp, "column", hcaes(x = Mother, y = Survived, color=colors)) %>% 
  hc_tooltip(pointFormat = "{point.y:.2f}</br>",shared = FALSE)


## ---- echo = F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#total1 <- total
par(mfrow=c(1,1))
for(i in 1:14){
  if(is.factor(total[,i])){
    total[,i]<-(as.integer(total[,i]))
  }
}

total[,-1] %>%
  cor(use="complete.obs", method="spearman") %>%
  corrplot(type="lower", method="number", diag=FALSE)

