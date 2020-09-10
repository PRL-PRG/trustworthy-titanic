#Load row data
train <- read.csv("../input/train.csv", header = TRUE)
test <- read.csv("../input/test.csv", header = TRUE)

#Add a "survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

#Reagranges the first and second cols
col_idx <- grep("PassengerId", names(test.survived))
test.survived <- test.survived[, c(col_idx, (1:ncol(test.survived))[-col_idx])]

#Combine data sets
data.combined <- rbind(train, test.survived)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

#Shows the data STRucture of data.combined
str(data.combined)

#Looks at gross survival rates in a table format
table(data.combined$Survived)

#Distribution across classes
table(data.combined$Pclass)

#Load up ggplot2 package for visualisation
library(ggplot2)

#Hypothesis: 1st class survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Examine the first few names in the training data set
head(as.character(train$Name))

#How many unique names are there across both train & test?
length(unique(as.character(data.combined$Name)))

#Returns two duplicate names, lets look closer
#First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

#Next, take a look at the records in the combined data set; are these four people different
data.combined[which(data.combined$Name %in% dup.names), ]
#So they are different people!

#Lets see if there is anything up with people's titles
library(stringr)

#Any correlation with other variables?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss. ")), ]
#Denotes a range of rows 1-5, R indexes from 1 not 0
misses[1:5, ]

mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs. ")), ]
mrses[1:5, ]

males <- data.combined[which(train$Sex == "male"), ]
males[1:5, ]
#Notice the master title!
#So title is intresting!

#Exapand upon the relationship between "Survived" and "Pclass" by adding the new "Title" variable
#Data set and then explore a potential 3-dimentional relationship.

#Create a utilty function to help with  title extraction
extractTitle <- function(name) 
{
  name <- as.character(name)
  
  if(length(grep("Miss.", name)) > 0) 
  {
    return ("Miss.")
  }
  else if (length(grep("Master.", name)) > 0) 
  {
    return ("Master.")
  }
  else if (length(grep("Mrs.", name)) > 0) 
  {
    return ("Mrs.")
  }
  else if (length(grep("Mr.", name)) > 0) 
  {
    return ("Mr.")
  }
  else 
  {
    return ("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined))
{
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}
data.combined$Title <- as.factor(titles)

#Since we only have survived lables for the train set, only use the first 891 rows
ggplot(data.combined[1:891, ], aes(x = Title, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Class") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

#What's the distribution of males to females across train and test?
table(data.combined$Sex)

#Visualise the 3-way realtionship of sex, pclass and survival
ggplot(data.combined[1:891, ], aes(x = Sex, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Class") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Look at the distibutions of age over the entire data set
#Gives stats on the col.
summary(data.combined$Age)

#Just to be thorogh, take a look at survival rates broken ou tby sex, class and age
ggplot(data.combined[1:891, ], aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~Sex + ~Pclass) +
  xlab("Age") +
  ylab("Total Count") +
  labs(fill = "Survived") +
  ggtitle("Gender and Class")

#Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$Title == "Master."), ]
summary(boys$Age)

#We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$Title == "Miss."), ]
summary(misses$Age)
#So, you can not infer that Miss. means a young girl

#Visualise age for Miss. by class
ggplot(misses[misses$Survived != "None", ], aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass) +
  xlab("Age") +
  ylab("Total Count") +
  labs(fill = "Survived") +
  ggtitle("Age of Misses by Class")

#It appears that female children may have different survival rates
#This could be a candidate for feature engineering alter
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0), ]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))
#14.5 has been chosen as with the male data, this was the max age of a boy

#Anaylising the SibSp variable
summary(data.combined$SibSp)

#Can we treat as a factor?
length(unique(data.combined$SibSp))
#As there are 7 unique values, we can turn SibSp into a factor
data.combined$SibSp <- as.factor(data.combined$SibSp)

#Visualising SibSp
ggplot(data.combined[1:891, ], aes(x = SibSp, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Class and Title") +
  xlab("SibSp") +
  ylim(0, 300) +
  labs(fill = "Survived")

#Looking and visualising the parch variable
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891, ], aes(x = Parch, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Class and Title") +
  xlab("ParCh") +
  ylim(0, 300) +
  labs(fill = "Survived")

#Let's try some feature engineering. What about reatig a family size feature?
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
#We take these from the original files, as we want to keep their int structure

data.combined$Family <- as.factor(temp.sibsp + temp.parch + 1)

#Visualise this new variable, to see if it's predictive
ggplot(data.combined[1:891, ], aes(x = Family, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Class and Title") +
  xlab("Family Count") +
  ylab("Total Count") +
  ylim(0, 300) +
  labs(fill = "Survived")

#Looking at the previous graph, it is hard to see the other smaller bars
ggplot(data.combined[1:891, ], aes(x = Family, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Class and Title") +
  xlab("Family Count") +
  ylab("Total Count") +
  ylim(0, 70) +
  labs(fill = "Survived")