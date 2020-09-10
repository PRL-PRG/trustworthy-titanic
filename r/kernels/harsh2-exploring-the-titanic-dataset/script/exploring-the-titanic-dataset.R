library(Hmisc)
library(knitr)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(cowplot)
library(ROCR)
library(e1071)

train <- read.csv("../input/train.csv", stringsAsFactors = F, na.strings = c("NA", ""))
test <- read.csv("../input/test.csv", stringsAsFactors = F, na.strings = c("NA", ""))


##Data size and structure

#The training set consist of 891 observations and 12 variables. I have read all the "factors" (for instance male/female) into R as character strings, as some of them #require processing first.

str(train)
str(test)
test$Survived <- NA
all <- rbind(train,test)

##Completeness of the data

#First of all, I would like to see which variables contain missing values (blanks are also imported as NA (=Not Available)).

sapply(all, function(x) {sum(is.na(x))})

#Of course, the 418 NAs in Survived is the total number is observations in the test set. So this variable is what it should be (No NAs in train). Of the other #variables, Cabin is sparsely populated. Age is also missing a substantial number of values. In addition, Embarked is missing two values and one Fare value is missing.

##Exploring some of the most important variables

#Our response variable in the training set is complete, as well as Sex and Pclass, which seem two of the most important predictors. Since they are complete and tidy, I #am converting them into factors.

all$Sex <- as.factor(all$Sex)
all$Survived <- as.factor(all$Survived)
all$Pclass <- as.ordered(all$Pclass) #because Pclass is ordinal

###The response variable; Survived
#Of course, the very first thing that I want to do is explore the response variable. How many people survived, and how many died? You can see this below. Altogether, #of the people in the training set (891 observations) 61.6% died. For the remaining 418 observations (test set), this is what we have to predict.

ggplot(all[!is.na(all$Survived),], aes(x = Survived, fill = Survived)) +
  geom_bar(stat='count') +
  labs(x = 'How many people died and survived on the Titanic?') +
  geom_label(stat='count',aes(label=..count..))


###Sex/gender

#Of the 1309 people on the Titanic, a majority of 64.4% was male. This percentage was almost the same in the training data (64.7%). Within the training data 81.1% of #the men died, and 25.8% of the women died. Due to this large difference, Sex/gender seems a very important predictor.

p1 <- ggplot(all, aes(x = Sex, fill = Sex)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'All data') +
  geom_label(stat='count', aes(label=..count..))
p2 <- ggplot(all[!is.na(all$Survived),], aes(x = Sex, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Training data only') +
  geom_label(stat='count', aes(label=..count..))
plot_grid(p1, p2)


###Passenger Class

#As you can see below, most people traveled in 3rd class. Also, as expected, survival is strongly correlated with the passenger class. A majority of first class #passengers survived, and most people in 3rd class died. It is also noticable that almost all women in 1st and 2nd class survived. For men, 2nd class was almost as bad #as 3rd class.

p3 <- ggplot(all, aes(x = Pclass, fill = Pclass)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Pclass, All data') + geom_label(stat='count', aes(label=..count..)) +
  theme(legend.position="none")     
p4 <- ggplot(all[!is.na(all$Survived),], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='dodge') + labs(x = 'Training data only') +
  theme(legend.position="none")
p5 <- ggplot(all[!is.na(all$Survived),], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'Training data only', y= "Count") + facet_grid(.~Sex) +
  theme(legend.position="none")
p6 <- ggplot(all[!is.na(all$Survived),], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Training data only', y= "Percent") + facet_grid(.~Sex) +
  theme(legend.position="none")
plot_grid(p3, p4, p5, p6, ncol=2)


#In previous version, I have been working with Pclass and Sex as separate predictors. However, I realized that the 'headline' of the model should actually be a #combination of the two (also thanks to Oscar Takeshita, who also uses this idea in his excellent analysis. See: [Divide and Conquer [0.82296]]#(https://www.kaggle.com/pliptor/divide-and-conquer-0-82296)). Details that are lost when using the predictors separately were actually already mentioned in the #beginning of this section (Pclass 1 and 2 are almost guaranteed survival for women, and Pclass 2 is almost as bad as Pclass 3 for men).

all$PclassSex[all$Pclass=='1' & all$Sex=='male'] <- 'P1Male'
all$PclassSex[all$Pclass=='2' & all$Sex=='male'] <- 'P2Male'
all$PclassSex[all$Pclass=='3' & all$Sex=='male'] <- 'P3Male'
all$PclassSex[all$Pclass=='1' & all$Sex=='female'] <- 'P1Female'
all$PclassSex[all$Pclass=='2' & all$Sex=='female'] <- 'P2Female'
all$PclassSex[all$Pclass=='3' & all$Sex=='female'] <- 'P3Female'
all$PclassSex <- as.factor(all$PclassSex)

#Feature engineering

#I am not using the Title variable anymore since version 15. Although most kernals use it, I believe it double counts variance to much (overfitting). Hopefully, I will #manage to 'compensate' the slight drop in public score in upcoming version by improving other predictors. For the sake of completeness, I am keeping the title section #for the time being.

##Creating the Title variable

#The name variable is complete (no NAs), but actually contains more than just a first name and surname. It also contains a Title for each person, which must be #separated from the name to obtain tidy data. I am also saving the Surname (first part of the name variable, before the title), as I want to investigate the effects of #families traveling together later on (matching with #siblings/spouses and #parents/children).

#Extracting Title and Surname from Name
all$Surname <- sapply(all$Name, function(x) {strsplit(x, split='[,.]')[[1]][1]})
#correcting some surnames that also include a maiden name
all$Surname <- sapply(all$Surname, function(x) {strsplit(x, split='[-]')[[1]][1]})
all$Title <- sapply(all$Name, function(x) {strsplit(x, split='[,.]')[[1]][2]})
all$Title <- sub(' ', '', all$Title) #removing spaces before title
kable(table(all$Sex, all$Title))

#After seeing this, I want to reducing the number of titles to create better and more substantial Titles that can be used for prediction. Ms. is usually used for #younger married women. I will therefore join this one with Miss. I assume that Mlle stands for Mademoiselle in French. I will also join this category with Miss. I #also assume that Mme stands for Madame, and I will join Madame with Mrs. For the titles with low frequecies, I will create one new category.

#A problem of the model is that it produces many False Negatives for men (men predicted to die while actually surviving). Because there is such a big difference in #survival chances of especially the Misters in 1st class and 2nd and 3rd class, I split the Mr title in version 4. Since this split did not work anymore in version 5, #I abandoned this split. However in version 9 I finally introduced a  Child variable (using clean Age data only, see section 5.1), and this split works again regarding #the public score. The idea is to give Misters in 1st class some more weight.

all$Title[all$Title %in% c("Mlle", "Ms")] <- "Miss"
all$Title[all$Title== "Mme"] <- "Mrs"
all$Title[!(all$Title %in% c('Master', 'Miss', 'Mr', 'Mrs'))] <- "Rare Title"
all$Title <- as.factor(all$Title)
kable(table(all$Sex, all$Title))


ggplot(all[!is.na(all$Survived),], aes(x = Title, fill = Survived)) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'Title')

##Finding groups of people traveling together

###Families; siblings, spouses, parents and children

#In order to create the family size for each person on the boat, I will add up his/her number of parents/children, his/her number of siblings/spouses, and of course #add one (the person himself).

#creating family size variable (Fsize)
all$Fsize <- all$SibSp+all$Parch +1


#Below, you can easily see that solo travelers had a much higher chance to die than to survive. In addition, people traveling in families of 2-4 people actually had a #relatively high chance to survive. This chance is significantly lower among 5+ families.

ggplot(all[!is.na(all$Survived),], aes(x = Fsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size')

#What I now could do is convert these family sizes into categories (solo, small family, large family), but I first want to check if there are any inconsistencies in #the data.

###Families; what about uncles, aunts, cousins, nieces, grandparents, brothers/sisters-in law?

#To check the size data for inconsistencies, I am creating a variable that combines the surname and the Fsize. After that, I am going to check where these combinations #lead to strange numbers of families.

#composing variable that combines total Fsize and Surname
all$FsizeName <- paste(as.character(all$Fsize), all$Surname, sep="")

SizeCheck <- all %>%
  group_by(FsizeName, Fsize) %>%
  summarise(NumObs=n())
SizeCheck$NumFam <- SizeCheck$NumObs/SizeCheck$Fsize
SizeCheck$modulo <- SizeCheck$NumObs %% SizeCheck$Fsize
SizeCheck <- SizeCheck[SizeCheck$modulo !=0,]
sum(SizeCheck$NumObs)
kable(tail(SizeCheck))


#As you can see, this check does not always add up to a round number of families (for a total of 93 passengers). Part of these are no problem. For instance, there is #one large Andersson family of 7 and 2 additional women with the Surname Andersson and family size of 7. If the SibSp and Parch numbers are reliable, this just means #that there must be 5 more family members in the remaining data that we don't have (Note: this was my first assumption. It will be checked in a later version as the #passenger list seems complete. The other people on the boat all seem to be crew (2224-1309= 915 crew).

#However, I found out that there is something 'hidden' in this information that does matter. For instance, it turns out that the Hockings and the Richards' are #related. The connection here is that passenger 438 travels with 2 children, 1 parent, a brother and a sister. For her, all these people count are direct family. #However, other people are linked indirectly. For the 2 children for instance, only the brother and mother count as direct family. This leads to Fsizes that cannot be #compared to most families as 'apples to apples'. Their Fsizes are generally too high, as it is likely that those people have split up into smaller groups. The mother #may have stayed with her children, while the brother and sister probably have stayed with the grandmother.

#Note: this family is actually even more complex, as the grandmother also travels with a sister with the same maiden name. However, as this really seems an exception #and the other Mrs Needs already has the very reasonable Fsize of 2 (it's  Mrs Wilkes Needs), I am not taking her into consideration.

kable(all[all$Ticket %in% c('29104', '29105', '29106'),c(2,3,4,5,6,7,8,9,15)])

#In order to fix this, I first have to 'glue' those families together using maiden names.

NC <- all[all$FsizeName %in% SizeCheck$FsizeName,] #create data frame with only relevant Fsizenames

#extracting maiden names
NC$Name <- sub("\\s$", "", NC$Name) #removing spaces at end Name
NC$Maiden <- sub(".*[^\\)]$", "", NC$Name) #remove when not ending with ')'
NC$Maiden <- sub(".*\\s(.*)\\)$", "\\1", NC$Maiden)
NC$Maiden[NC$Title!='Mrs'] <- "" #cleaning up other stuff between brackets (including Nickname of a Mr)
NC$Maiden <- sub("^\\(", '', NC$Maiden) #removing opening brackets (sometimes single name, no spaces between brackets)
#making an exceptions match
NC$Maiden[NC$Name=='Andersen-Jensen, Miss. Carla Christine Nielsine'] <- 'Jensen'

#take only Maiden names that also exist as surname in other Observations
NC$Maiden2[NC$Maiden %in% NC$Surname] <- NC$Maiden[NC$Maiden %in% NC$Surname] 
#create surname+maiden name combinations
NC$Combi[!is.na(NC$Maiden2)] <- paste(NC$Surname[!is.na(NC$Maiden2)], NC$Maiden[!is.na(NC$Maiden2)])

#create labels dataframe with surname and maiden merged into one column
labels1 <- NC[!is.na(NC$Combi), c('Surname','Combi')]
labels2 <- NC[!is.na(NC$Combi), c('Maiden','Combi')]
colnames(labels2) <- c('Surname', 'Combi')
labels1 <- rbind(labels1, labels2)

NC$Combi <- NULL
NC <- left_join(NC, labels1, by='Surname')

#Find the maximum Fsize within each newly found 'second degree' family
CombiMaxF <- NC[!is.na(NC$Combi),] %>%
  group_by(Combi) %>%
  summarise(MaxF=max(Fsize)) #summarise(MaxF=n())
NC <- left_join(NC, CombiMaxF, by = "Combi")

#create family names for those larger families
NC$FsizeCombi[!is.na(NC$Combi)] <- paste(as.character(NC$Fsize[!is.na(NC$Combi)]), NC$Combi[!is.na(NC$Combi)], sep="")

#find the ones in which not all Fsizes are the same
FamMaid <- NC[!is.na(NC$FsizeCombi),] %>%
  group_by(FsizeCombi, MaxF, Fsize) %>%
  summarise(NumObs=n())
FamMaidWrong <- FamMaid[FamMaid$MaxF!=FamMaid$NumObs,]

kable(unique(NC[!is.na(NC$Combi) & NC$FsizeCombi %in% FamMaidWrong$FsizeCombi, c('Combi', 'MaxF')]))

#As you can see, 7 combinations (total of 28 passengers) are found as families with not all members having the same Fsize, which means that they are broader families #with non-direct family links included. Before I decided what to do with these, I first have to find the families who are similarly linked on the 'male' side.

NC$MaxF <- NULL #erasing MaxF column maiden combi's

#Find the maximum Fsize within remaining families (no maiden combi's)
FamMale <- NC[is.na(NC$Combi),] %>%
  group_by(Surname) %>%
  summarise(MaxF=max(Fsize))
NC <- left_join(NC, FamMale, by = "Surname")

NCMale <- NC[is.na(NC$Combi),] %>%
  group_by(Surname, FsizeName, MaxF) %>%
  summarise(count=n()) %>%
  group_by(Surname, MaxF) %>%
  filter(n()>1) %>%
  summarise(NumFsizes=n())

NC$Combi[NC$Surname %in% NCMale$Surname] <- NC$Surname[NC$Surname %in% NCMale$Surname]

kable(NCMale[, c(1,2)])

#Example. Mr Julius Vander Planke is traveling with a spouse and 2 siblings. His spouse and siblings (brothers/sisters-in-law) are 'indirectly' related to each other.

kable(all[all$Surname=='Vander Planke', c(2,3,4,5,6,7,8,9,15)])

#This means that altogether, there are 9 families (37 passengers) that include 'second degree' family members. What I want to do is give each member in such family the #same Fsize (which gives everybody in these families the same survival chances with regards to the group variable). I have chosen to make this the average of the Fsize #(which are based on siblings/spouse/parents/children only).

#selecting those 37 passengers In Not Correct dataframe
NC <- NC[(NC$FsizeCombi %in% FamMaidWrong$FsizeCombi)|(NC$Surname %in% NCMale$Surname),]

#calculating the average Fsize for those 9 families
NC1 <- NC %>%
  group_by(Combi) %>%
  summarise(Favg=mean(Fsize))
kable(NC1)

#A result is that for instance the Fsize is 4 for all 6 people in the Richards-Hockings family. This exactly what I wanted, as I wanted to combine those people into a #group with all members having the same Fsize (to give equal survival chances to all members within the group) but also not the maximum size as they are less likely to #stay together than first degree families.

NC <- left_join(NC, NC1, by = "Combi") #adding Favg to NC dataframe 
NC$Favg <- round(NC$Favg) #rounding those averages to integers
NC <- NC[, c('PassengerId', 'Favg')]
all <- left_join(all, NC, by='PassengerId')

#replacing Fsize by Favg
all$Fsize[!is.na(all$Favg)] <- all$Favg[!is.na(all$Favg)]


###Did people book together?

#Besides families, groups of friends can off course also travel together. A nice example of this is the ticket below.

kable(all[all$Ticket=='1601', c('Survived', 'Pclass', 'Title', 'Surname', 'Age', 'Ticket', 'SibSp', 'Parch', 'Fsize')])


#Below, I am adding the number of people on each ticket as variable.

#composing data frame with group size for each Ticket
TicketGroup <- all %>%
  select(Ticket) %>%
  group_by(Ticket) %>%
  summarise(Tsize=n())
all <- left_join(all, TicketGroup, by = "Ticket")

#Very similarly to the family group sizes, small groups of 2-4 people traveling together on the same ticket have a higher chance of survival.

ggplot(all[!is.na(all$Survived),], aes(x = Tsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Ticket Size')


#As there is so much overlap between family size and ticket size, I am consolidating these two variables into one group variable.

all$Group <- all$Fsize
for (i in 1:nrow(all)){
  all$Group[i] <- max(all$Group[i], all$Tsize[i])
}

###Can we still find more groups?

#Am I still missing groups? Yes, at it appears that some people traveling solo with the same surname have tickets with almost the same number!

#creating a variable with almost the same ticket numbers (only last 2 digits varying)
all$Ticket2 <- sub("..$", "xx", all$Ticket)


#As they have no sibling/spouses and no parents/children, these people are likely cousins/uncles. If you look deeper into the data, you will see that these groups of #cousins/uncles sometimes also travel with (first degree) families. However, I think the key to this exercise is not to find the absolute largest groups that people #may have stayed together with. I think it should be to detect smaller groups that actually stayed together. It sounds reasonable to assume that first degree families #stayed together, and that uncles/cousins also took care of each other (this is consistent with the averaging of the Fsizes in section 4.2.2). Altogether, I have found #another 44 passengers that I can assign a group size to.

rest <- all %>%
  select(PassengerId, Title, Age, Ticket, Ticket2, Surname, Group) %>%
  filter(Group=='1') %>%
  group_by(Ticket2, Surname) %>%
  summarise(count=n())
rest <- rest[rest$count>1,]
rest1 <- all[(all$Ticket2 %in% rest$Ticket2 & all$Surname %in% rest$Surname & all$Group=='1'), c('PassengerId', 'Surname', 'Title', 'Age', 'Ticket', 'Ticket2', 'Group', 'SibSp', 'Parch')]
rest1 <- left_join(rest1, rest, by = c("Surname", "Ticket2"))
rest1 <- rest1 %>%
  arrange(Ticket2, Surname)
kable(rest1[1:12,])

#replacing Group size in my overall dataframe with the count numbers in the table above
all <- left_join(all, rest1)
for (i in 1:nrow(all)){
  if (!is.na(all$count[i])){
    all$Group[i] <- all$count[i]
  }
}

ggplot(all[!is.na(all$Survived),], aes(x = Group, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Final Groups')

#Now I can finally created my factorized variable for the group sizes. As '1' and '2' are large groups with their own typical survival rates, I am keeping them as #separate groups. Sizes '3' and '4' clearly have the best survival chances, and the groups of 5 and more clearly have worse chances.

#Creating final group categories
all$GroupSize[all$Group==1] <- 'solo'
all$GroupSize[all$Group==2] <- 'duo'
all$GroupSize[all$Group>=3 & all$Group<=4] <- 'group'
all$GroupSize[all$Group>=5] <- 'large group'
all$GroupSize <- as.factor(all$GroupSize)

ggplot(all[!is.na(all$Survived),], aes(x = GroupSize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Final Group Sizes')

#clean up
all$count <- NULL
all$Name <- NULL
rm(CombiMaxF)
rm(FamMaid)
rm(FamMaidWrong)
rm(FamMale)
rm(labels1)
rm(labels2)
rm(NC)
rm(NC1)
rm(NCMale)
rm(rest)
rm(rest1)
rm(SizeCheck)
rm(TicketGroup)
rm(p1); rm(p2); rm(p3); rm(p4); rm(p5); rm(p6)



##Dealing with the Fare variable

###Which data relevant to fare are missing?
#There are two missing values in Embarked, and one in Fare. Embarked could be important to Fare, as different Embarkement cities mean longer or shorter journeys.

#display passengers with missing Embarked
kable(all[which(is.na(all$Embarked)),c('Surname', 'Title', 'Survived', 'Pclass', 'Age', 'SibSp', 'Parch', 'Ticket', 'Fare', 'Cabin', 'Embarked', 'Group') ])

#Both women are traveling solo from a family perspective (Fsize=1), but must be friends as they are both are traveling on ticket 113572 (nobody else was traveling on #this ticket, so Group=2). The both have the same fare, but this fare might still have been per person. I came to the conclusion that prices are indeed per ticket. As #the explanation was getting lengthy, I will now just continue under the assumption that fares are per person. 

#I want to impute the missing embarkement city with the median Fare Per Person for each Embarkement city, and per Pclass.

all$FarePP <- all$Fare/all$Tsize #creating the Fare Per Person variable

tab2 <- all[(!is.na(all$Embarked) & !is.na(all$Fare)),] %>%
  group_by(Embarked, Pclass) %>%
  summarise(FarePP=median(FarePP))
kable(tab2)

#As the FarePP of those two women is 40, they most likely embarked at Cherbourgh.

#imputing missing Embarked values
all$Embarked[all$Ticket=='113572'] <- 'C'
#converting Embarked into a factor
all$Embarked <- as.factor(all$Embarked)

#I can actually use the same table to find a sensible fare for Mr Story. As you can see below, he traveled 3rd class and embarked at Southampton. 

#display passengers with missing Fare
#kable(all[which(is.na(all$Fare)), c('Surname', 'Title', 'Survived', 'Pclass', 'Age', 'SibSp', 'Parch', 'Ticket', 'Fare', 'Cabin', 'Embarked', 'Group')])

#imputing FarePP (as the Fare will be dropped later on anyway)
all$FarePP[1044] <- 7.8

###The Fare Per Person Variable

#Although there now are no missing FarePP's anymore, I also noticed that 17 Fares actually have the value 0. These people are not children that might travel for free. #I think the information might actually be correct (have people won free tickets?), but I think that the zero-Fares might confuse the algorithm. For instance, there #are zero-Fares within the 1st class passengers. To avoid this possible confusion, I am replacing these values by the median FarePP's for each Pclass.

tab3 <- all[(!is.na(all$FarePP)),] %>%
  group_by(Pclass) %>%
  summarise(MedianFarePP=median(FarePP))
all <- left_join(all, tab3, by = "Pclass")
all$FarePP[which(all$FarePP==0)] <- all$MedianFarePP[which(all$FarePP==0)]

#Below you can see that the FarePP is very skewed. I know that this is not desirable for some algorithms, and can be solved by taking the logarithm or normalisation #(preprocessing with centering and scaling). 

hist(all$FarePP, main='Histogram of Fare Per Person', col='blue', xlab='Fare Per Person')

#Another option is to use Fare bins instead of keeping the FarePP as a numeric variable. In version 18, I am using Fare Bin in the GBM model.

#Note Hmisc needs to be loaded before dplyr, as the other ways around errors occured due to the kernel using the Hmisc summarize function instead of the dplyr #summarize function
all$FareBins <- cut2(all$FarePP, g=5)
table(all$FareBins)

##Predicting missing Age values

#The density plot below shows that survival chances of children are relatively high. Survival chances of ages 20-30 are below average, and I see less significant #differences in the 30+ region. I think there may be a lot of solo travelers in the 20-30 category, which could explain the below averages survival chances. A possible #use case of Age could be to use it to identify children. Therefore, I will focus on good looking Age imputations in the region 0-18 years old.

ggplot(all[(!is.na(all$Survived) & !is.na(all$Age)),], aes(x = Age, fill = Survived)) +
  geom_density(alpha=0.5, aes(fill=factor(Survived))) + labs(title="Survival density and Age") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

#I first want to visualize the relation between the Age. Title and Pclass seem the most important predictors for Age to me. As you can see below, there are significant #differences in Age across the Titles (By the way, this graph tells me that "Masters" are all very young. I did not know what a master was, but googling it tells me #that a master was used as a title for the eldest son only.). Similarly, there differences in Age when looking at the Title/Passenger Class combinations.

ggplot(all[!is.na(all$Age),], aes(x = Title, y = Age, fill=Pclass )) +
  geom_boxplot() + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

#The title Master seems to be a good predictor for male children. However, female children are included in the Miss title, and of the 263 missing age values, 51 are #Misses. If I would just take the median Age of the Titles (possibly also by Pclass), I would at least not predict the missing ages of female children well. I tried #both Mice imputation and Linear Regression, and focused on how good the imputations for children looked. The Mice imputations looked reasonable, but I preferred #Linear Regression.

#predicting Age with Linear Regression
set.seed(12000)
AgeLM <- lm(Age ~ Pclass + Sex + SibSp + Parch + Embarked + Title + GroupSize, data=all[!is.na(all$Age),])
summary(AgeLM)
all$AgeLM <- predict(AgeLM, all)

#As expected, the most significant predictors according to Linear Regression were Passenger Class and Title. Below you can see that the histogram of the predicted #values versus the shape of the known ages. The Mice histogram actually looked nicer, but I was wondering how it could predict high ages well given the sparseness of #these ages in the original data?

par(mfrow=c(1,2))
hist(all$Age[!is.na(all$Age)], main='Original data, non-missing', xlab='Age', col='green')
hist(all$AgeLM[is.na(all$Age)], main= 'LM NA predictions', xlab='Age', col='orange', xlim=range(0:80))

#As mentioned before, I especially looked at young predicted ages. Both mice and Linear Regression predicted all Masters with missing ages to be children indeed (the #one in Linear Regression with a negative age did not bother me that much, as it is categorized as a child anyway). Mice predicted some Mr.'s of 14 years old, which is #too young. As Linear Regression also predicted a reasonable number of Misses to be children, I eventually chose Linear Regression.

#display which passengers are predicted to be children (age<18) with Linear Regression.
all[(is.na(all$Age) & all$AgeLM <18), c('Sex', 'SibSp', 'Parch', 'Title', 'Pclass', 'Survived', 'AgeLM')]

#imputing Linear Regression predictions for missing Ages
indexMissingAge <- which(is.na(all$Age))
indexAgeSurvivedNotNA<- which(!is.na(all$Age) & (!is.na(all$Survived))) #needed in sections 4.6 and 4.7
all$Age[indexMissingAge] <- all$AgeLM[indexMissingAge]

#So now all missing data have been imputed. Am I going to use Age as a predictor in my model? I am not sure yet, as the substantial number of imputations will also add #noise. I wil look at using it to create a Child predictor later on.

##What to do with Cabin?

#Cabin is very sparsely populated. So I either have to ignore it, or use it somehow without making it too specific. On the internet, you can find that that the first #letter corresponds to the Deck. Decks A-E are the topdecks and cabins on those decks are mostly first class.

#replacing NAs with imaginary Deck U, and keeping only the first letter of ech Cabin (=Deck)
all$Cabin[is.na(all$Cabin)] <- "U"
all$Cabin <- substring(all$Cabin, 1, 1)
all$Cabin <- as.factor(all$Cabin)

ggplot(all[(!is.na(all$Survived)& all$Cabin!='U'),], aes(x=Cabin, fill=Survived)) +
  geom_bar(stat='count') +facet_grid(.~Pclass) + labs(title="Survivor split by class and Cabin")

#Below, you can see that there are interesting difference among Decks. For instance, the top Deck (A) was not best place to be. Even Deck F had better survival rates.

c1 <- round(prop.table(table(all$Survived[(!is.na(all$Survived)&all$Cabin!='U')], all$Cabin[(!is.na(all$Survived)&all$Cabin!='U')]),2)*100)
kable(c1)

#Although I feel that Deck and Deck sections (front/back of boat, sections close to stairs et cetera) would be great predictors, I am not using Cabin due to the the #sparseness of the data.

##How to deal with Children in the model?

#The survival density plot in the Age section shows that Children below roughly 14.5 (which is also the maximum Age of Masters in the data) have a better survival rate #than then other Ages. However, if you look at the imputed Ages below 14.5, you will also see that all these age imputation are for Pclass 3 and most of these children #actually died (10 out of 13).

#This makes me wonder if I should add a survival 'bonus' for all Pclasses. Below you can see that most children in P3 actually die. As these children in P3 also 3include age imputations which may add noise, I decided to exclude P3 from the Child predictor.

ggplot(all[all$Age<14.5 & !is.na(all$Survived),], aes(x=Pclass, fill=Survived))+
  geom_bar(stat='count')

all$IsChildP12 <- 'No'
all$IsChildP12[all$Age<=14.5 & all$Pclass %in% c('1', '2')] <- 'Yes'
all$IsChildP12 <- as.factor(all$IsChildP12)

##What does Embarked tell us?

#Although I feel that the city of Embarked should not be related to survival rates, I still wanted to check it. As you can see below, there somehow are significant #differences between the three ports of embarkment.
d1 <- ggplot(all[!is.na(all$Survived),], aes(x = Embarked, fill = Survived)) +
  geom_bar(stat='count') +
  labs(x = 'Embarked', y= 'Count')
d2 <- ggplot(all[!is.na(all$Survived),], aes(x = Embarked, fill = Survived)) +
  geom_bar(stat='count', position= 'fill') +
  labs(x = 'Embarked', y= 'Percent')
plot_grid(d1, d2)

#To get a feel for where this differences may come from, I plotted them against Sex and Pclass. Roughly, differences were:

#* Southampton survival rates are worse than Cherbourg in all Pclass/Sex combinations
#* Cherbourg survival rates are better than Queenstown as many 1st class passengers boarded at Cherbourgh, while almost all Queenstown passengers boarded 3rd class #(but within 3rd class, female survival rate is better than Cherbourg and male survival rate is worse than Cherbourgh).

#My conclusion is that at least the lower survival rate of Southampton compared to Cherbourg cannot be explained by Pclass or Sex. One thing that I want to look at is #the relation between Embarked, Age and Survived, because Linear Regression surprisingly enough also labeled Embarked at Queenstown as a significant predictor for Age. #Below I am only using the known Ages of the training data (714 observation = training set - 177 observations with missing Age).

ggplot(all[indexAgeSurvivedNotNA,], aes(x = Age, fill = Survived)) +
  geom_histogram(aes(fill=factor(Survived))) + labs(title="Survival density, known-ages, and Embarked") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + facet_grid(.~Embarked)

#This shows that is very little data for especially Queenstown when looking at known Ages. Below you can see that the total number of people who embarked at Queenstown #is low indeed, but especially the high percentage of missing ages in Queenstown is really high. Using imputed ages will therefore add too much noise, and combining #Age and Embarked as a predictor is a bad idea.

tab1 <- rbind(table(all$Embarked[!is.na(all$Survived)]),table(all$Embarked[indexAgeSurvivedNotNA]))
tab1 <- cbind(tab1, (rowSums(tab1)))
tab1 <- rbind(tab1, tab1[1,]-tab1[2,])
tab1 <- rbind(tab1, round((tab1[3,]/tab1[1,])*100))
rownames(tab1) <- c("All", "With Age", "Missing Age", "Percent Missing")
colnames(tab1) <- c("C", "Q", "S", "Total")
kable(tab1)


#The only other thing that I can think of that might explain the differences is that probably people from the different embarkement cities are somehow grouped on #certain sections of the decks. 

#I kept Embarked in my model until version 16. However, after cleaning up the model, it gradually became clear that Embarked does not add anything. I am nit using it #anymore in version 17.

##Ticket survivors

#In version 17, I added a variable that checks if any people in a group did survive. The idea is that anyone in a certain group survived, chances of others also #surviving are higher. I did this using the Ticket information, and it improved the scores. In the next version, I will try to extend this variable to match the group #info, as it now does not take families traveling on different tickets into account. It should be seen as a promising start; it does make a positive difference, but I #will need group IDs for groups such as the Richard Hockings family. However, it is harder than that, as there for instance also is a group consisting of 3 married #sisters (3 different surnames, 1 maiden name).

TicketSurvivors <- all %>%
  group_by(Ticket) %>%
  summarize(Tsize = length(Survived),
            NumNA = sum(is.na(Survived)),
            SumSurvived = sum(as.numeric(Survived)-1, na.rm=T))

all <- left_join(all, TicketSurvivors)
all$AnySurvivors[all$Tsize==1] <- 'other'
all$AnySurvivors[all$Tsize>=2] <- ifelse(all$SumSurvived[all$Tsize>=2]>=1, 'survivors in group', 'other')
all$AnySurvivors <- as.factor(all$AnySurvivors)

table(all$AnySurvivors)

#Predictions (with caret cross validation)

#cleaning up
all$PassengerId <- NULL
all$SibSp <- NULL
all$Parch <- NULL
all$Ticket <- NULL
all$Fare <- NULL
all$Cabin <- NULL
all$Surname <- NULL
all$Fsize <- NULL
all$FsizeName <- NULL
all$Favg <- NULL
all$Tsize <- NULL
all$Group <- NULL
all$Ticket2 <- NULL
all$AgeLM <- NULL
all$Child <- NULL
all$HasParch <- NULL
all$MedianFarePP <- NULL
rm(tab1); rm(tab2); rm(tab3); rm(AgeLM); rm(c1); rm(d1); rm(d2);

#splitting data into train and test set again
trainClean <- all[!is.na(all$Survived),]
testClean <- all[is.na(all$Survived),]

#Initially, I had some difficulties with the Caret cross validation, as the cross validated accuracies did not seem a good prodictor for the public score on Kaggle (as #I have seen in many kernels). However, after studying how caret and the R formula function work together I managed to get it working in version 11. Although the #formula method must be used with many algorithms, it is better to not use it with Random Forest as this causes issues with weights of predictors.

##Random Forest model
#I started this analysis with just a Random Forest model, as it is known for high accuracy and limiting overfitting. Since version 17, I am just using 5 predictors.

set.seed(2017)
caret_matrix <- train(x=trainClean[,c('PclassSex', 'GroupSize', 'FarePP', 'AnySurvivors', 'IsChildP12')], y=trainClean$Survived, data=trainClean, method='rf', trControl=trainControl(method="cv", number=7))
caret_matrix
caret_matrix$resample
caret_matrix$results

varImpPlot(caret_matrix$finalModel, main=" Variable importance")

#using the model to make Survival predictions on the test set
solution_rf <- predict(caret_matrix, testClean)
#creating csv file for submission on Kaggle
submission_rf <- data.frame(PassengerId = test$PassengerId, Survived = solution_rf)
write.csv(submission_rf, file = 'Titanic_rf.csv', row.names = F)
