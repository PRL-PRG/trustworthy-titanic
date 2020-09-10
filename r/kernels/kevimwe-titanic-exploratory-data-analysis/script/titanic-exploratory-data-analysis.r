
#Import  packages
pacman::p_load(pacman, ggplot2, dplyr,stringr,gridExtra,knitr,tidyverse,reshape2, readr, plyr)
detach("package:plyr")

#import dataset 
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

Full <- bind_rows(train, test) # Merged test and training sets for preprocessing

head(Full, 2)
str(Full)

Full$Name<-as.character(Full$Name)
Full$Survived<-as.factor(Full$Survived)
Full$Pclass<-as.factor(Full$Pclass)
Full$Sex<-as.factor(Full$Sex)
Full$Embarked<-as.factor(Full$Embarked)
str(Full)

head(Full, 2)

Full$Title <- as.factor(gsub("(^.*,)|(\\..*)", "", Full$Name)) #use gsub to grab title from the 
summary(Full$Title)

temp<-Full[which(as.character(Full$Title) %in% c(' Major',' Capt', ' Rev', ' Col', ' Sir', ' Don', ' Dr', ' Jonkheer')),] 
temp

for(i in 1:nrow(temp)){
    Full<-Full%>%
     mutate(Title=replace(Title, Title==temp$Title[i], ' Mr')) %>%
     as.data.frame()
    }
# rerun factor level to delete empy factors levels
Full$Title<-factor(Full$Title)
summary(Full$Title)

# reassigning passanger 797 title to Miss
Full[which((Full$Title ==' Mr') & (Full$Sex=='female')),]$Title<-' Mrs'

# Replace all ' Lady', ' Mlle', ' Ms' with Miss and  'Mme', ' the Countess' with Mrs
temp<-Full[which(as.character(Full$Title) %in% c(' Lady', ' Mlle', ' Ms', ' Dona')),] 
temp

for(i in 1:nrow(temp)){
    Full<-Full%>%
     mutate(Title=replace(Title, Title==temp$Title[i], ' Miss')) %>%
     as.data.frame()
    }


temp<-Full[which(as.character(Full$Title) %in% c(' Mme', ' the Countess')),] 
temp
for(i in 1:nrow(temp)){
    Full<-Full%>%
     mutate(Title=replace(Title, Title==temp$Title[i], ' Mrs')) %>%
     as.data.frame()
    }
# rerun factor level to delete empy factors levels
Full$Title<-factor(Full$Title)
summary(Full$Title)


#Facet Age Histogram by title
facet =ggplot(Full, aes(x=Age, color=Title)) +
  geom_histogram(alpha=0.5, position="identity")

facet + facet_grid(. ~ Title)

Full$Family_Size <- Full$SibSp + Full$Parch + 1
head(Full, 3)
# Family Size Histogram
facet= ggplot(Full, aes(x=Family_Size)) +
  geom_histogram(alpha=0.5, position="identity")
facet + ggtitle('Family Size')

unique(Full$Cabin)

Full$Cabin_Class <- as.character(substr(Full$Cabin, 1, 1))
#Full[which((is.na(Full$Cabin_Class))),]$Cabin_Class<-'X'# Missing Cabin 
#Full$Cabin_Class <-Full$Cabin_Class)
Full[which(Full$Cabin_Class==''),]$Cabin_Class<-'Z'
Full$Cabin_Class <-as.factor(Full$Cabin_Class)
summary(Full$Cabin_Class)

print(paste0('There are Cabins ', length(summary(Full$Cabin_Class)), ' in the Titanic. Does your survival depend on which cabin you are in? Keep reading to find out'))

Age_Temp<-Full%>%
select(Title,Sex, Age)%>%
group_by(Title,Sex)%>%
summarize(Youngest = min(Age, na.rm=TRUE),
          Avg.Age = mean(Age, na.rm=TRUE),
          Median.Age = median(Age, na.rm=TRUE),
          Oldest = max(Age, na.rm=TRUE)
         
         
         )
Age_Temp

set.seed(10000)
X1 = round(runif(1, Age_Temp[which((Age_Temp$Title ==' Mr')),]$Median.Age, Age_Temp[which((Age_Temp$Title ==' Mr')),]$Avg.Age), 2)
print(paste0("Mr: ", X1))
X2 = round(runif(1, Age_Temp[which((Age_Temp$Title ==' Mrs')),]$Median.Age, Age_Temp[which((Age_Temp$Title ==' Mrs')),]$Avg.Age), 2)
print(paste0("Mrs: ", X2))
X3 = round(runif(1, Age_Temp[which((Age_Temp$Title ==' Miss')),]$Median.Age, Age_Temp[which((Age_Temp$Title ==' Miss')),]$Avg.Age), 2)
print(paste0("Miss: ", X3))
X4 = round(runif(1, Age_Temp[which((Age_Temp$Title ==' Master')),]$Median.Age, Age_Temp[which((Age_Temp$Title ==' Master')),]$Avg.Age), 2)
print(paste0("Master: ", X4))

Full[which((Full$Title ==' Mr') & (is.na(Full$Age))),]$Age<-X1#Mr
Full[which((Full$Title ==' Mrs') & (is.na(Full$Age))),]$Age<-X2 #Mrs
Full[which((Full$Title ==' Miss') & (is.na(Full$Age))),]$Age<-X3 #Miss
Full[which((Full$Title ==' Master') & (is.na(Full$Age))),]$Age<-X4 #Master

#Facet Age Histogram by title
facet =ggplot(Full, aes(x=Age, color=Title)) +
  geom_histogram(alpha=0.5, position="identity")

facet + facet_grid(. ~ Title)

pacman::p_load(plyr)


lower_bound <- c(0,14,26,40,65)
Full$Age_group <- findInterval(Full$Age, lower_bound)

# Create Definition Table
Age_group <- c(1,2,3,4,5)
Age_Brackets <- c("Below_14", "14_to_25", "26_to_39", "40_to_64", "Above_64")
Age_Table <- data.frame(Age_group, Age_Brackets)


Full <- join(Full, Age_Table, by = "Age_group")
Full$Age_Brackets <- as.factor(Full$Age_Brackets)



head(Full)

# check if there are any more missing values
Full %>%
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))
str(Full)

#replace missing Fare with median as mean looks skewed
Full[which( (is.na(Full$Fare))),]$Fare<-14.454
#median(Full$Fare)

summary(Full)

# check if there are any more missing values
Full %>%
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))
str(Full)

#replace missing Missing Embarked based on Fare
Full$Embarked <-as.character(Full$Embarked)
Full[which(Full$Embarked==''),]$Embarked<-'C' #mode
Full$Embarked <- as.factor(Full$Embarked)
summary(Full)

# Split the data back into a train set and a test set
test<-Full[which(Full$PassengerId %in% c(test$PassengerId)),]
train<-Full[which(Full$PassengerId %in% c(train$PassengerId)),]

p <- ggplot(train, aes(Survived))
# Number of cars in each class:
p + geom_bar(aes(fill = Survived))+ ggtitle('Survived?')+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = .9)# adjust title location


p1 <- ggplot(train, aes(Pclass))+ geom_bar(aes(fill = Survived), position = "Fill")+ ggtitle('% Survival by Passanger Class')+
    theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 2)# adjust title location

p2 <- ggplot(train, aes(Pclass))+ geom_bar(aes(fill = Survived), position = "dodge")+ ggtitle('Survival by Passanger Class')+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 2)# adjust title location

grid.arrange(p1,p2, ncol=2)#, heights=c(2.5/4, 1.5/4), ncol=1)

p1 <- ggplot(train, aes(Sex))+ geom_bar(aes(fill = Survived), position = "Fill")+ ggtitle('% Survival by Gender')+
    theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 2)# adjust title location

p2 <- ggplot(train, aes(Sex))+ geom_bar(aes(fill = Survived), position = "dodge")+ ggtitle('Survival by Gender')+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 2)# adjust title location

grid.arrange(p1,p2, ncol=2, heights=c(2.5/4, 1.5/4))

p1 <- ggplot(train, aes(as.factor(Family_Size)))+ geom_bar(aes(fill = Survived), position = "Fill")+  ggtitle('% Survival by Family Size')+
    theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 2)# adjust title location

p2 <- ggplot(train, aes(as.factor(Family_Size)))+ geom_bar(aes(fill = Survived), position = "dodge")+ ggtitle('Survival by Family Size')+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 2)# adjust title location

grid.arrange(p1,p2, ncol=2, heights=c(2.5/4, 1.5/4))

p1 <- ggplot(train, aes(Age_Brackets))+ geom_bar(aes(fill = Survived), position = "Fill")+  ggtitle('% Survival by Age Bracket')+
    theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 0.7)# adjust title location

p2 <- ggplot(train, aes(Age_Brackets))+ geom_bar(aes(fill = Survived), position = "dodge")+ ggtitle('Survival by Age Bracket')+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 0.7)# adjust title location

grid.arrange(p1,p2, ncol=1)

p <- ggplot(data = train, aes(Sex)) + geom_bar(aes(fill = Survived), position = "Fill")
p + facet_wrap(~Age_Brackets)

# Relationship between age & survival
ggplot(train, aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  facet_grid(.~Sex)


# Relationship between Fare & survival
ggplot(train, aes(Fare, fill = factor(Survived))) + 
  geom_histogram() 
  


# Relationship between Fare & survival  by class
ggplot(train, aes(Fare, fill = factor(Survived))) + 
  geom_histogram() + 
  facet_grid(.~Pclass)


pacman::p_load(mlr)

head(Full, 1)

#columns to keep except Name, Ticket, Cabin, Age_group
Full_Final<-Full

Full_Final$Name<-NULL
Full_Final$Ticket<-NULL # will revisit this feature
Full_Final$Cabin<-NULL
Full_Final$Age_group<-NULL



head(Full_Final)
str(Full_Final)

#create dummies
pacman::p_load(mlr)
head(Full_Final)

df1 =Full_Final
df1 = createDummyFeatures(df1, cols = c('Sex','Pclass', 'Embarked', 'Cabin_Class', 'Title', 'Age_Brackets'))
head(df1)
names(df1)

# Split the data back into a train set and a test set
test<-df1[which(df1$PassengerId %in% c(test$PassengerId)),]
train<-df1[which(df1$PassengerId %in% c(train$PassengerId)),]

write.csv(train, 'modeling_train.csv', row.names=FALSE)
write.csv(test, 'modeling_test.csv', row.names=FALSE)

sessionInfo()


