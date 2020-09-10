
---
title: "Titanic sinking - Survival Analysis"
author: by Rogerio Rezende

output:
  html_document:
    toc: true
    theme: united
    toc_float:
      collapsed: true
      smooth_scroll: false
number_sections: true
---
I have great satisfaction in presenting this first data analysis.
I hope this analysis will be a source of inspiration to Kagglers beginners like me, and that everyone will have great enthusiasm to learn about this fascinating world of data science.

![](/opt/pentaho/arquivos/LanguageR/Titanic/titanic.jpg)

##Step 1 : Load the packages 
```{r, message=FALSE, warning=FALSE}
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library(VIM) # missing data
library('mice') # imputation
library('randomForest') # classification algorithm

```
##Step 2 : Load the files

```{r, message=FALSE, warning=FALSE}
train <- read.csv('dataset/train.csv', stringsAsFactors = F)
test <- read.csv('dataset/test.csv', stringsAsFactors = F)
```
#### Create "full" dataframe 
```{r}
full  <- bind_rows(train, test)
str(full)
```
#### Detailed Variables 

PassengerId   => Ordinal Passanger Id number 

Survived      => Survived (0) and Died (1) 

Pclass        => Passenger's class

Name          => Passenger's name 

Sex           => Passenger's gender 

Age           => Passenger's age 

Sibsp         => Number of siblings/spouses aboard

Parch         => Number of parents/children aboard

Ticket        => Passenger's ticket number 

Fare          => Fare paid 

Cabin         => Cabin

Embarked      => Port of embarkation (Q)ueenstown, (S)outhampton and (C)hebourg


##Step 3: Missing Data (NA)

To identify missing data from full dataset we will use a smart function with "is.na" test .
```{r}
MData <- sapply(full, function(x) sum(is.na(x))); MData[MData>0]
MData
```
Now let's show a interesting plot about missing data with VIM package. We will check that :

#### => 30% of Survived information is NA 

#### => 20% of Age information is NA 

#### => 0,07% of Fare information is NA  

Our task will be completing missing data of Age and Fare. Survived information will be part of predictive analysis.

```{r}
aggr_plot <- aggr(full, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(full), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
```
Let's input data to Age column with mice package .
Create TMPfull dataset and complete mice imputation .

```{r}
TMPfull <- mice(full,m=5,maxit=50,meth='pmm',seed=500)
TMPComplete <- complete(TMPfull)
```
Below we can see an example of the first 30 rows of Age column.  
Attention to rows 06, 18, 20, 27, 29, 30. 
```{r}
full$Age[1:30]
TMPComplete$Age[1:30]
```
Update Age column with new Age result .  

```{r}
full$Age <- TMPComplete$Age
```
Check missing data again. Notice that the column Age is not listed anymore .  

```{r}
NData <- sapply(full, function(x) sum(is.na(x))); NData[NData>0]
```
#### Missing Fare Information 
First of all we have to discover which passenger has Fare = NA 
```{r}
FareM <- full[is.na(full$Fare),]
FareM
```
The passenger is #1044 . We will use sample median to generate a compatible Fare value .

```{r}
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

```

##Step 4: Titles and Surname

Thought the surname we can identify some families : 

```{r}
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex, full$Title)
```
Titles with less frequency : 

```{r}
Rare <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
```
Organizing titles :
```{r}
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% Rare]  <- 'Rare'
table(full$Sex, full$Title)
```
Show titles by Gender : 
```{r}
table(full$Sex, full$Title)
```
Grab surname from passenger's name . We will discovered 875 probable families .

```{r}
full$Surname <- sapply(full$Name,  
                      function(x) strsplit(x, split = '[,.]')[[1]][1])
nr_surname <- nlevels(factor(full$Surname));
nr_surname
```
##Step 5: Family Sizes 

```{r}
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep='_')
```
Improving graph presentation on Survived variable
```{r}
full$Survived [full$Survived == '0'] <- 'Died'
full$Survived [full$Survived == '1'] <- 'Survived'
```

```{r}
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

```
```{r}
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)
```

##Step 6 : Port of Embarked

We will check if any passenger has incorrect information about port abord.
```{r}

full[full$Embarked != 'C' &  full$Embarked != 'Q' & full$Embarked != 'S',]
```
As we could check, passengers 62 and 830 don't have valid information about embarked .
Let's identify the apropriate port based in the column Fare . As Megan Risdal https://www.kaggle.com/mrisdal/titanic/exploring-survival-on-the-titanic said, the median fare for a first class passenger departing from Charbourg (âCâ) coincides nicely with the $80 paid by our embarkment-deficient passengers

```{r}
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
    colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()
full$Embarked[c(62, 830)] <- 'C'
```

##Step 7 : Age and Gender   

```{r}
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()
```

##Step 8 : Class Survival

```{r}
ggplot(full[1:891,], aes(x = Pclass, fill = factor(Survived), label = Pclass)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Survival by Class') +
  theme_few()

```

##Step 9 : train and test again

```{r}
md.pattern(full)

train <- full[1:891,]
test <- full[892:1309,]

```

#### Back Survived to 0 and 1 

```{r}
train$Survived [train$Survived == 'Died'] <- 0
train$Survived [train$Survived == 'Survived'] <- 1

```

#### Create factors to prediction

```{r}
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Age','SibSp','Parch','Fare',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

set.seed(754)

```

##Step 10 : Random Forest

```{r}

rf_model <- randomForest(factor(Survived) ~ Pclass + Age + SibSp + Parch + 
                                           Fare + Fsize , data = train,
                                           importance=TRUE)

```

#### Show model error

```{r}
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

```

##Step 11: Predict (test)

```{r}
prediction <- predict(rf_model, test)
```

#### Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)

```{r}
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

```

#### Write the solution to file

```{r}
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

```
##Acknowledgements

Thanks to Kaggle by the opportunity and learning [https://www.kaggle.com/]

Thanks to Megan Risdal [https://www.kaggle.com/mrisdal/titanic/exploring-survival-on-the-titanic] by the inspiration and code. 

