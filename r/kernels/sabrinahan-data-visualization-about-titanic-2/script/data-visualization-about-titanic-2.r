
## 1. Train Data Processing &Description

###-Import and Overview

library(data.table)
train <- read.csv("../input/train.csv", sep=",", header=TRUE, stringsAsFactors=TRUE)
summary(train)

###-Deal with Age's NAs
train = na.omit(train)
#Age has 177 missing values. Cabin has 687 NAs and Embarked has 2. We'll can put Cabin and Embarked aside. 
#Age is an important demographic variable, so we can try to fill with predictions.

train$Embarked=as.character(train$Embarked) #change data type that can add NA
#train[Embarked=='', Embarked:=NA] #set '' in Embarked as missing value

#Tried regression model, R-square is not ideal. So take the tree method.
library(rpart)
library(ggplot2)
library(gridExtra)
#age_fit2 = rpart(Age ~ Pclass + Sex + SibSp + Parch
#                +Fare+Embarked, 
#                data = train[!is.na(Age), ],
#                na.action = na.omit, method = 'anova',cp=0.01)
#page1=qplot(train$Age,geom='histogram',fill=I('turquoise3'),xlab='Age-raw', 
#           ylab='Numbers of People')
#train[is.na(Age), Age := predict(age_fit2, newdata=train[is.na(Age)]) ]
page2=qplot(train$Age,geom='histogram',fill=I('turquoise3'),xlab='Age-adjusted', 
            ylab='Numbers of People')
page2

#The two distribution of age seem alike. That's what we want.

###-List single variable distribution except Name, Ticket and Cabin

#Now, most of variables are factors and numbers. 

psur=qplot(train$Survived,geom='bar', ylab='Numbers of People',fill=I('turquoise3'), 
           xlab="0=Not survived   1=Survived" )
ppclass=qplot(train$Pclass,geom='bar',fill=I('turquoise3'),xlab='Social-Economic Status', 
      ylab='Numbers of People')
psex=qplot(train$Sex,geom='bar',fill=I('turquoise3'), xlab='Gender',
      ylab='Numbers of People')
pss=qplot(train$SibSp,geom='bar',xlab='People with x Siblings and Spouse', 
      ylab='Numbers of People',fill=I('turquoise3'))
ppc=qplot(train$Parch,geom='bar',xlab='People with x Parents and Children', 
      ylab='Numbers of People',fill=I('turquoise3'))
pf=qplot(train$Fare, geom='histogram',fill=I('turquoise3'),xlab='Price of Ticket', 
      ylab='Numbers of People')
pe=qplot(train$Embarked, xlab='Embarked Port', fill=I('turquoise3'),
      ylab='Numbers of People',geom='bar')
grid.arrange(psur, ppclass,psex,page2,pss,ppc,pf,pe, ncol=3)

#Most people are in their golden ages(20~40). Most people are men. 
#Most people are travel alone. Most people from 3rd class. 
#Most people from Southampton. And most of them lost their lives in that adventure.

###-Interesting Thing about Name: New Name &New Life

##Name has many interesting title. Let's grasp these info.
train$Title=gsub('(.*,)|(\\..*)','',train$Name)
train$Title=as.factor(train$Title)
train$Survived=as.character(train$Survived)
ggplot(train,aes(x=Title, fill=Survived))+geom_bar()+coord_flip()+scale_fill_brewer(palette = "BuGn")+ylab('')

#The survived "Miss"" is **no** more than married women "Mrs"" in terms of ration. 

#Another phenomenon attracts me is that many people have two names, 
#like "Cumings, Mrs. John Bradley (Florence Briggs Thayer)". 
#Did they change name before this tragedy or after? We don't know. Just have a look.

train$NewName=grepl('(', train$Name,fixed=TRUE)
train$Survived=as.character(train$Survived)
ggplot(train,aes(x=NewName, fill=Survived))+geom_bar()+scale_fill_brewer(palette = "BuGn")+ylab('')

#The ration looks high for people survived to have a second name. 

###-A Quick Look of Correlation and Importance: Women, Young, High Class
library(GGally) #Only shows numeric variables
train <- as.data.table(train) 

train[Sex=='male',nSex:=1]
train[Sex=='female',nSex:=2]
train[Embarked=='C',Port:=1]
train[Embarked=='Q',Port:=2]
train[Embarked=='S',Port:=3]
train$Survived=as.numeric(train$Survived)
ggcorr(train,label=TRUE, label_alpha=TRUE, name='Correlation', low = "turquoise3", mid = "white", high = "orangered3")
library(randomForest)
rf = randomForest(Survived ~ Pclass+Age+nSex+Parch+SibSp+Fare+Port, data=train,
                   na.action = na.omit, ntree=101, importance=TRUE)
imp = importance(rf, type=1)
featureImportance = data.frame(Feature=row.names(imp), Importance=imp[,1])
ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill=I("turquoise3")) +
  coord_flip() +  xlab('')+ylab("Importance on Survive")


#Women, people of young age and people of higher class are more likely to survive.


###-X with Survived

#Take a close look of every independent variable influence on survive.
#This time add SibSp and Parch as family size

train$Survived=as.character(train$Survived)
SurPc=ggplot(train,aes(x=Pclass, fill=Survived))+geom_bar()+ scale_fill_brewer(palette = "BuGn")+ylab('')
Surage=ggplot(train,aes(x=Age, fill=Survived))+geom_histogram()+ scale_fill_brewer(palette = "BuGn")
SurFam=ggplot(train,aes(x=SibSp+Parch, fill=Survived))+geom_histogram()+scale_fill_brewer(palette = "BuGn")+ylab('')
SurFare=ggplot(train,aes(x=Fare, fill=Survived))+geom_histogram()+ scale_fill_brewer(palette = "BuGn")+ylab('')
SurSex=ggplot(train,aes(x=Sex, fill=Survived))+geom_bar()+ scale_fill_brewer(palette = "BuGn")+ylab('')
SurEmbarked=ggplot(train,aes(x=Embarked, fill=Survived))+geom_bar()+ scale_fill_brewer(palette = "BuGn")+ylab('')
grid.arrange(SurPc,Surage,SurFam,SurFare,SurSex,SurEmbarked, ncol=3) 

#From the diagram, we have a guess: people who are women, young but not too young, 
#have 1-2 companies have more chance to survive.


##2. Calculate Single Factor Importance in Female Surviving

### -Pclass: Not Be 3rd

table(train$Sex,train$Pclass,train$Survived)

#The female survived in the Titanic most from first and second class, 
#the number of female in the first and second class died in Titanic is less than 10. 
#This is a very important factor.


### -Parch: Good If You're Alone

table(train$Sex,train$Parch,train$Survived)

#Women without any children or parents are more likely to survive 
#While the more parch they have, the less probability they can survive. 


### -SibSp

table(train$Sex,train$SibSp,train$Survived)

#Women without any siblings or spouses are more likely to survive.
#While the more siblings they have, the less probability they can survive. 


### -Title

table(train$Sex,train$Title,train$Survived)

#The survived unmarried women is more than married women, 
#which is accordance with the result about SibSp. Generally, 
#the number of women survived is more than the women died.

### -Data visualization

####**Fare and Sex Whisker Plot**

ggplot(train)+geom_boxplot(aes(x=Sex,y=Fare,fill=Survived))

#The lower whisker and lower quartile for all the four box are the same.
#the survived female have higher median and upper quartier than the died female.
#That shows female with higher fare or higher pclass have more chance for surviving.
#it's the same with male's surviving, but the amount of surviving men is smaller than women.
#The distribution of survived female is very sparse while the distribution of died male is very intensive.

####**Fare and Pclass Whisker Plot**

ggplot(train)+geom_boxplot(aes(x=Pclass,y=Fare,fill=Survived))

#The charts shows that the fare in the first class has larger range than the second and third class.
#The median of survived people in first class is equal to the upper quartier of dead people.
#Generally, people in the first class with higher fare have more chance to survive.
#The upper whisker of second and third class are almost equal to the lower quartier of first class.
#The fare distribution of second and third class are more intensive than the first class.

####**Age and Pclass Violin Plot**

p<-ggplot(train,aes(factor(Pclass),Age))
p+geom_violin(aes(fill=Survived))

#In the first class, most of survived people are around 35 years old and its range is very wide. 
#The distribution of the first class is even, no obvious concentrated trend.
#In the second class, there is obvious concentrated trend at around 30 year old. 
#That shows most people are aronud 30 years old. The distribution of dead people's age is a little higher than survived people.
#The concentrated trend at 20 year old in the third class is also very obvious. 
#The survived people in third class are younger than the dead people.

####**Age and Sex Violin Plot**

p<-ggplot(train,aes(factor(Sex),Age))
p+geom_violin(aes(fill=Survived))

#The distribution of male is wider than female generally. 
#Most survived women are around 20 to 30 years old.
#The distribution of other women at other age is tend to be uniform.
#The survived men's age is concentrated in 30 years old. 
#The distribution of other age is not uniform but extremely few.

####**Pclass,Age and Fare Scatter plot**

train$Pclass=as.character(train$Pclass) #To make sure change safety
train$Pclass=as.factor(train$Pclass)
p<-ggplot(data=train,mapping = aes(x=Age,y=Fare,shape=Pclass,color=Survived))
p+geom_point()

#This scatter plot shows there is no linear relation between age and fare. 
#From the color scatter we can see, blue points are mostly on the top 
#wihle the red points are maily on the bottom. 
#That means the higher fare is, the more chances for surviving. 
#It also can be shown in the shape of different classes. 
#The first and second class get more chances for surviving compared to the third class.

####**Sex,Age and Fare Scatter plot**

p<-ggplot(data=train,mapping = aes(x=Age,y=Fare,shape=Sex,color=Survived))
p+geom_point()

#The blue round points is more than blue squre points. 
#That shows females have more chances of surviving compared to males. 
#In addition, the higher fare of females bought, the higher chances for surviving.


##3.Analytics of Factors

#Let's do some investigation at the influence of age, pclass, and sex to the survival. 
#in order to show the result vividly, we use ggplot2 to draw the facet_grid diagram.

result1 = ggplot(data = train, aes(x = Age, y = Sex, color = factor(Survived)))+geom_point()
result = result1+facet_grid(.~Pclass)+ggtitle("Social Status")
result

#In this diagram, we can see that age, pclas and sex are all important factors. 
#For those who were in the first class and who were female, 
#there was a bigger chance to survive. We can imagine the scene back then: 
#Titanic was falling, few lifeboats were prepared and young, well-classed ladies and madams 
#were rescued first.For those who landed lifeboats and left, the chance of survival was the biggest.
#But for those females who were in the third class and the males, things were not so happy. 
#Lifeboats were limited, they must find their own way to make lives. 
#The possiblities of their living can be very small.

#Also, we can discuss the relationship between survival and family size. 
#There are no factors called "family size", but we can conclude from siblings and parch. According to our experience, those who were with babies and  old people might have a bigger chance to survive. We can know if we are right from the following diagram.

family = train$SibSp+train$Parch
result2 = ggplot(data = train, aes(x = Age, y = family, color = factor(Survived)))+geom_point()
result = result2+facet_grid(.~Sex)+ggtitle("Gender")
result

#Then we can see that our assumption is true. 
#First,females with 1 or 5,6 members in a family got the biggest chance of survival, 
#while those who had only 1 or 2 family members did not. 
#We can imagine that they might honorly give the chance of living to those who need most, 
#and choose to die with their husbands or fathers. 
#Males does not have a significant feature from the diagram. 
