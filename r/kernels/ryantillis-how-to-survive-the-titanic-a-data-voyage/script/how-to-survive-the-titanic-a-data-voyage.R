## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.height = 2.5)
library(dplyr)
library(data.table)
library(ggplot2)
library(DT)
library(plotly)
library(mice)
library(stringi)


## ----loading---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##Include length of name metric
##Include Pictures for class selection
##Make Fare input dependent on class
#Picture of deck
train <- read.csv("../input/train.csv")
test <- read.csv("../input/train.csv")
train <- as.data.table(train)
test. <- as.data.table(test)
names(train)


## ----Pclass----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$Pclass <- as.factor(train$Pclass)
summary(train$Pclass)


## ----chart1, echo = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge')# + scale_col_manual(colors()[78], colors()[16], colors()[55])


## ----Name, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(sapply(as.character(unique(train$Name)),nchar))#Summary of lengths of name


## ----Tsplit, echo = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)

# Show titles by sex
table(train$Sex, train$Title)


## ----sexs, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(train$Sex)#Almost twice as many men


## ----sexc, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sb <- ggplot(train, aes(x=Sex)) + geom_bar(fill=c(colors()[542], colors()[121]), col=c(colors()[543], colors()[123]), lwd = 2)+labs(x="Class")
sb


## ----age, echo = FALSE, warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(train$Age) #177 NA's, Median age is 28 aka born in 1884 (start of input), 

ap <- ggplot(train, aes(x=Age))+geom_density(adjust=.5)
ap


## ----SibSp, include = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
unique(train$SibSp)


## ----sib2------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$SibSp <- as.integer(train$SibSp) #possibly alone vs with family variable 
summary(train$SibSp) #median is 0


## ----sib3, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(train[SibSp > 0,])


## ----sib4, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(train[SibSp == 0,])


## ----sibplots, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = SibSp, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:7)) +
  labs(x = 'SibSp')


## ----sibp, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = SibSp, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:7)) +
  labs(x = 'SibSp')+ ylim(0,115)


## ----Parch, echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(train$Parch)


## ----p1, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(train[Parch > 0,])


## ----p2,echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(train[Parch == 0,])


## ----p3, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = Parch, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:7)) +
  labs(x = 'Parch')


## ----p4, echo = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = Parch, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:7)) +
  labs(x = 'Parch')+ ylim(0,65)


## ----Ticket, echo = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
length(unique(train$Ticket)) #Create Ticket pre and ticket num and LINE value all as non-integers
tt <- data.table(names(summary(train$Ticket)),summary(train$Ticket))
names(tt) <- c("Ticket", "Count")
setorder(tt, -Count)


## ----tplot, echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
datatable(tt)
#Use ticket to predict cabin

train$FL <- stri_extract_first_regex(train$Ticket, "[A-Z]+") #Grabs first Text occurence
train$FT <- stri_extract_first_regex(train$Ticket, "[0-9][0-9]+") #Grabs ticket number
train[is.na(train$FL),]$FL <- "NA"


## ----plot, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tpref <- train[!train$FL=="NA",]
gg <- ggplot(aes(y = Fare, x = FL), data = tpref) + geom_boxplot() + labs(title="Ticket Prefix")
gg


## ----ticketplot, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p2 <- ggplot(aes(x = FL, fill = factor(Survived)), data = tpref) + geom_bar(stat='count', position='dodge') + labs(title="Ticket Prefix")
p2



## ----dig-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tfix <- tpref[tpref$FL %in% c("A", "CA", "Soton", "W"),]
summary(sapply(as.character(train$FT),nchar))
summary(sapply(as.character(tfix$FT), nchar))


## ----numlength-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$ticketlength <- sapply(as.character(train$FT),nchar)


## ----plot23, echo = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
s1 <- ggplot(aes(x = factor(ticketlength), fill = factor(Survived)), data = train) + geom_bar(stat='count', position='dodge') + labs(title="Ticket Number Length")
s1

f1 <- ggplot(aes(x = factor(ticketlength), y=Fare, fill = factor(Survived)), data = train) + geom_boxplot() + labs(title="Ticket Number Length")
f1


## ----plot2, echo = FALSE, fig.height=20------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(gridExtra)
train$one <- sapply(train$FT, function(x){substr(x,1,1)})
train$two <- sapply(train$FT, function(x){substr(x,2,2)})
train$three <- sapply(train$FT, function(x){substr(x,3,3)})
train$four <- sapply(train$FT, function(x){substr(x,4,4)})
train$five <- sapply(train$FT, function(x){substr(x,5,5)})
train$six <- sapply(train$FT, function(x){substr(x,6,6)})
train$seven <- sapply(train$FT, function(x){substr(x,7,7)})

train$one <- as.factor(train$one)
train$two <- as.factor(train$two)
train$three <- as.factor(train$three)
train$four <- as.factor(train$four)
train$five <- as.factor(train$five)
train$six <- as.factor(train$six)
train$seven <- as.factor(train$seven)

onep <- ggplot(aes(y = Fare, x = factor(one)), data = train) + geom_boxplot() + labs(title="First Digit Price")
ones <- ggplot(aes(x = factor(one), fill = factor(Survived)), data = train) + geom_bar(stat='count') + labs(title="Second Digit Survival")


twop <- ggplot(aes(y = Fare, x = factor(two)), data = train) + geom_boxplot() + labs(title="Second Digit Price")
twos <- ggplot(aes(x = factor(two), fill = factor(Survived)), data = train) + geom_bar(stat='count') + labs(title="Second Digit Survival")

threep <- ggplot(aes(y = Fare, x = factor(three)), data = train) + geom_boxplot() + labs(title="Third Digit Price")
threes <- ggplot(aes(x = factor(three), fill = factor(Survived)), data = train) + geom_bar(stat='count') + labs(title="Third Digit Survival")


fourp <- ggplot(aes(y = Fare, x = factor(four)), data = train) + geom_boxplot() + labs(title="Fourth Digit Price")
fours <- ggplot(aes(x = factor(four), fill = factor(Survived)), data = train) + geom_bar(stat='count') + labs(title="Fourth Digit Survival")

fivep <- ggplot(aes(y = Fare, x = factor(five)), data = train) + geom_boxplot() + labs(title="Fifth Digit Price")
fives <- ggplot(aes(x = factor(five), fill = factor(Survived)), data = train) + geom_bar(stat='count') + labs(title="Fifth Digit Survival")


sixp <- ggplot(aes(y = Fare, x = factor(six)), data = train) + geom_boxplot() + labs(title="Sixth Digit Price")
sixs <- ggplot(aes(x = factor(six), fill = factor(Survived)), data = train) + geom_bar(stat='count') + labs(title="Sixth Digit Survival")

sevenp <- ggplot(aes(y = Fare, x = factor(seven)), data = train) + geom_boxplot() + labs(title="Seventh Digit Price")
sevens <- ggplot(aes(x = factor(seven), fill = factor(Survived)), data = train) + geom_bar(stat='count') + labs(title="Seventh Digit Survival")

grid.arrange(onep,ones,twop,twos, threep, threes, fourp, fours, fivep, fives, sixp, sixs, sevenp, sevens, ncol=2, nrow =7)


## ----unFare----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train[Fare==0]$Sex


## ----fthis, echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
length(unique(train$Fare)) #248 unique ticket prices, suggests 248 cabin assignment, use 


## ----fw, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train %>% group_by(Pclass) %>% summarise_each(funs(min, max, mean, median),Fare) 


## ----fthat, echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train %>% group_by(Sex) %>% summarise_each(funs(min, max, mean, median),Fare) #Women pay way more for same (start of input)
#train %>% group_by(Sex, Pclass) %>% summarise_each(funs(min, max, mean, median),Fare) #Women pay way more for same (start of input)


## ----cabin, echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(!substring(train$Cabin, 1, 1) == "")#Fill in cabin data
datatable(data.frame(summary(train$Cabin))) # is it safe to impute cabin data?


## ----CabinLetter-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$CL <- substring(train$Cabin, 1, 1)
train$CL <- as.factor(train$CL)
unique(train$CL)


## ----Csum------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(train[!substring(train$Cabin, 1, 1) == "",]$Embarked)
summary(train[!substring(train$Cabin, 1, 1) == "",]$Pclass)



## ----cabinsplit, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

cabins <- train[!substring(train$Cabin, 1, 1) == "",]
cabins$PCL <- interaction(cabins$Pclass, cabins$CL)
gg <- ggplot(aes(y = Fare, x = CL, fill = factor(Survived)), data = cabins) + geom_boxplot()
gg


## ----cabinsplit2, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g2 <- ggplot(aes(x = CL, fill = factor(Survived)), data = cabins) + geom_bar(stat='count')
g2


## ----cabinbin--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#train$Assigned <- 0
#train[!substring(train$Cabin, 1, 1) == "",]$Assigned <- 1


## ----Embarked, echo = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
unique(train$Embarked)
summary(train$Embarked)


## ----miss, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Missing Values
datatable(train[Embarked=="",])


## ----pressure, echo = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
apply(train=="",2, sum)
apply(is.na(train),2, sum)


## ----pr, results = 'hide'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- as.data.frame(train)
#Imputing the missing age values with the MICE package
impute <- mice(train[, !names(train) %in% c('PassengerId','Name','Ticket','Cabin','Survived', 'Assigned','FL','FT','ticketlength','one','two','three','four','five','six','seven')], method='rf')

trained_mouse <- complete(impute)


## ----pplot, echo = FALSE, warning = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Plotting Histograms
ap <- ggplot(train, aes(x=Age))+geom_density(adjust=.5)+labs(title="Original Data")
ap
mp <- ggplot(trained_mouse, aes(x=Age))+geom_density(adjust=.5)+labs(title="Imputed Data")
mp


## ----replace1--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$Age <- trained_mouse$Age


## ----Missing Cabin---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
unique(train[grep("*^B", train$Cabin),]$Embarked)


## ----cab, echo = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train[grep("*^B", train$Cabin),] %>% group_by(Embarked) %>% summarize_each(funs(mean),Fare)


## ----mcab, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train[grep("*^B", train$Cabin),] %>% group_by(Embarked) %>% summarize_each(funs(median),Fare)


## ----bark, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bark <- train[grep("*^B", train$Cabin),] %>% group_by(Embarked)
bark <- bark[!bark$Embarked=="",]
#Also, there a 72.4% change of any passenger embarking from Southampton
ggplot(bark, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
    colour='red', linetype='dashed', lwd=2)


## ----split, echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(train$Embarked)
bark2 <- train[!train$Embarked=="",]
ggplot(bark2, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
    colour='red', linetype='dashed', lwd=2)
summary(train[grep("*^B", train$Cabin),]$Embarked)


## ----replace---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$Embarked[c(62, 830)] <- 'S'

