## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('../input/train.csv')
test <- read.csv('../input/test.csv')


## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#for decision trees
library(rpart) 

#for cforest
library(party) 


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Adding a column of NA (missing) values in test
test$Survived <- NA     

# row binding train and test
combi  <- rbind(train, test) 

# taking a look at the structure of combi
str(combi) #or View(combi)


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1 is for the passenger himself/herself
combi$FamilySize <- combi$SibSp + combi$Parch + 1          


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#to display the first 6 records in the Name column
head(combi$Name) 


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# first converting to character type
combi$Name <- as.character(combi$Name)

strsplit(combi$Name[1], split = '[,.]')


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
strsplit(combi$Name[1], split = '[,.]')[[1]][2]


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combi$Title <- sapply(combi$Name, FUN = function(x){ strsplit(x, split = '[,.]')[[1]][2]})


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#substitute the first occurrence of a white space with nothing
combi$Title <- sub(' ', '', combi$Title) 


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(combi$Title)


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# The titles 'Mme' and 'Mlle' are merged into one 'Mlle' and similarly, for others. 
# %in% is used to satisfy the logical OR condition, means if the title is one among the titles
# in the given vector, condition is satisfied. Below is the way:
    
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Jonkheer', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'Ms', 'the Countess', 'Mlle')] <- 'Lady'

#To change into factor (datatype that contains categories)
combi$Title <- factor(combi$Title) 
table(combi$Title)


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combi$Surname <- sapply(combi$Name, FUN = function(x){ strsplit(x, split = '[,.]')[[1]][1]})


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# pasting the two columns together
combi$FamilyId <- paste(as.character(combi$FamilySize), combi$Surname, sep = '') 

# as obvious below, those with family size less than or equal to 2 will be designated as Small
combi$FamilyId[combi$FamilySize <= 2] <- 'Small'


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# famId, a temporary variable, will store the FamilyId with their frequencies
famId <- data.frame(table(combi$FamilyId))

# Now, famId will only contain records with frequency or number of passengers in that family 
# less than or equal to 2
famId <- famId[famId$Freq <= 2, ]

# Any FamilyId existing in famId would then be declared 'Small'
combi$FamilyId[combi$FamilyId %in% famId$Var1] <- 'Small'

# finally converting to factor
combi$FamilyId <- factor(combi$FamilyId)

table(combi$FamilyId)


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#creating a column of NA values first
combi$Deck <- NA  

#NULL splits at each position
combi$Deck <- sapply(as.character(combi$Cabin), function(x){ strsplit(x, NULL)[[1]][1]}) 
combi$Deck <- factor(combi$Deck)
summary(combi$Deck)


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(combi)


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# first creating the decision tree on the training data, or only the rows that contain a value for Age column
fitAge <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = combi[!(is.na(combi$Age)), ], method = "anova")

# now putting the predicted Age values in the rows that have NA in their Age column
combi$Age[is.na(combi$Age)] <- predict(fitAge, combi[is.na(combi$Age), ])


## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# which row(s) has NA in the Fare column
which(is.na(combi$Fare))  

# median value assigned to the NA value.
combi$Fare[1044] <- median(combi$Fare, na.rm = TRUE)


## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# which row has blank as Embarked
which(combi$Embarked == '')

combi$Embarked[c(62,830)] = 'S'


## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fitDeck <- rpart(Deck ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize + Age, data = combi[!(is.na(combi$Deck)), ], method = "class")

combi$Deck[is.na(combi$Deck)] <- predict(fitDeck, combi[is.na(combi$Deck), ], type = "class")


## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create a temporary data frame storing Ticket values
tempo <- data.frame(table(combi$Ticket))

# store only those Ticket values that belong to more than one passenger
tempo <- tempo[tempo$Freq > 1, ]

tempo$Fare <- 0

# run a nested loop to extract the Fare values for the Ticket values in tempo
for(i in 1:nrow(combi)) {
  for(j in 1:nrow(tempo)) {
    if(combi$Ticket[i] == tempo$Var1[j]) {
      tempo$Fare[j] = combi$Fare[i] } } } 

# calculate the Fare per passenger
tempo$Fare <- tempo$Fare / tempo$Freq

# put back the Fare per passenger values to the combi data frame
for(i in 1:nrow(combi)) {
  for(j in 1:nrow(tempo)) {
    if(combi$Ticket[i] == tempo$Var1[j]) {
      combi$Fare[i] = tempo$Fare[j] } } }



## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1:891 means 1 to 891(included)
train <- combi[1:891, ]
test <- combi[892:1309, ]


## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# setting the seed so that everytime the model is created, random numbers produced would be the same
set.seed(400)

# the way of building a model is quite similar to how we made a couple of decision trees
# ntree specifies number of trees as base learners and mtry is for the number of variables to be used in each tree 
fit <- cforest(factor(Survived) ~ Pclass + Age + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyId + Deck, data = train, controls = cforest_unbiased(ntree = 2000, mtry = 3))

# making a bar plot of the importance of predictor variables for the model
par(las = 2)
barplot(varimp(fit))


## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# our model operating upon the test set
Prediction <- predict(fit, test, OOB = TRUE, type = "response")

# create the submit data frame
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

# write the data frame as a csv file without the row numbers as a separate column
write.csv(submit, file = "RFcondInfTrees.csv", row.names = FALSE)

