## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library('plyr') # feature correlation
library('corrplot') # feature correlation plotting


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
testDat <- read.csv("../input/titanic/test.csv",stringsAsFactors = F)
trainDat <- read.csv("../input/titanic/train.csv",stringsAsFactors = F)
totalDat <- bind_rows(trainDat, testDat) # bind training & test data
ethnicityDat <- read.csv("../input/ethnicity/ethnicity.csv", stringsAsFactors = F)

str(totalDat) # check full data


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(ethnicityDat) # check ethnicity data


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
totalDat$Title <- gsub('(.*, )|(\\..*)', '', totalDat$Name) # Extract the title from the name
totalDat$Surname <- tolower(sapply(totalDat$Name,function(x) {strsplit(x, split = '[,.]')[[1]][1]})) # Extract the Surname
Name <- sapply(totalDat$Name, function(x) {strsplit(x, split='[,.]')[[1]][3]})
Names <- sapply(Name, function(x) {strsplit(x, split=' ')[[1]][2]}) # Extract the given Name
mat  <- matrix(unlist(Names), ncol=1, byrow=TRUE)
mat <- gsub("[[:punct:]]","",mat)
totalDat$Given<- tolower(mat)
totalDat$Given <- totalDat$Given[,]
totalDat <- merge(totalDat,ethnicityDat,by.x="Given",by.y="Name",all.x=T) # Merge with ethnicityDat
colnames(totalDat)[colnames(totalDat)=='Eth'] <- 'Ethnicity' # Rename Eth to Ethnicity

table(totalDat$Sex, totalDat$Title) # create a table which shows all of the title combinations

officer_title <- c('Capt','Col','Major')
community_title <- c('Dr','Sir')
rare_title <- c('Dona', 'Lady', 'the Countess', 'Don', 'Rev', 'Jonkheer')
totalDat$Title[totalDat$Title == 'Mlle']        <- 'Miss' 
totalDat$Title[totalDat$Title == 'Ms']          <- 'Miss'
totalDat$Title[totalDat$Title == 'Mme']         <- 'Mrs' 
totalDat$Title[totalDat$Title %in% rare_title]  <- 'Rare Title'
totalDat$Title[totalDat$Title %in% officer_title]  <- 'Crew'
totalDat$Title[totalDat$Title %in% community_title]  <- 'Member'
table(totalDat$Sex,totalDat$Title) # List titles again


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
totalDat$Fsize <- totalDat$SibSp + totalDat$Parch + 1 # Passenger + Siblings/Spouses + Parents/Children
totalDat$IsAlone[totalDat$Fsize==1] <- 'Alone' # Is the passenger travelling alone?
totalDat$IsAlone[totalDat$Fsize!=1] <- 'Not Alone'
totalDat$IsAlone <- factor(totalDat$IsAlone)
totalDat$Family <- paste(totalDat$Surname, totalDat$Fsize, sep='_') # families Surname


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
totalDat <- totalDat[order(totalDat$PassengerId),] #Sort the data
ggplot(totalDat[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='bin', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
totalDat$FsizeD[totalDat$Fsize == 1] <- 'singleton'
totalDat$FsizeD[totalDat$Fsize < 5 & totalDat$Fsize > 1] <- 'small'
totalDat$FsizeD[totalDat$Fsize > 4] <- 'large'
mosaicplot(table(totalDat$FsizeD, totalDat$Survived), main='Family Size by Survival', shade=TRUE) # Plotting


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
n_occur <- data.frame(table(totalDat$Ticket))
totalDat <- merge(totalDat,n_occur, by.x="Ticket", by.y="Var1", x.all=T) # Assign the frequency of each ticket appearance
totalDat$Fare <- totalDat$Fare / totalDat$Freq # Recalculate the fares accordingly


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
totalDat$Deck<-factor(sapply(totalDat$Cabin, function(x) {strsplit(x, NULL)[[1]][1]}))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
totalDat <- totalDat[order(totalDat$PassengerId),]
totalDat$Deck[totalDat$Deck=='']<-NA
totalDat$Deck <- factor(totalDat$Deck)
totalDat$Deck <- addNA(totalDat$Deck)
ggplot(totalDat[1:891,],aes(x=Deck,fill=factor(Survived)))+geom_bar()+scale_fill_discrete("Survived?")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
levels(totalDat$Deck)[9] <- "TT"
train <- totalDat[1:891,] # Look only at Data with known survival
ggplot(train[train$Deck!='TT',],aes(x=Deck,fill=factor(Survived)))+geom_bar()+scale_fill_discrete("Survived?")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
totalDat$Fare[1044] <- median(totalDat[totalDat$Pclass == '3' & totalDat$Embarked == 'S', ]$Fare, na.rm = TRUE)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
totalDat$Fare[totalDat$PassengerId==62][1]
totalDat$Fare[totalDat$PassengerId==830][1]


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
embark_fare <- totalDat %>% filter(PassengerId != 62 & PassengerId != 830)
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=40), colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  scale_fill_discrete("Passenger class") +
  labs(title= "Fares by Embarked") +
  theme_few()


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
totalDat$Embarked[c(62, 830)] <- 'C'


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(is.na(totalDat$Age))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
totalDat$PassengerId <- factor(totalDat$PassengerId)
totalDat$Pclass <- factor(totalDat$Pclass)
totalDat$Sex <- factor(totalDat$Sex)
totalDat$Embarked <- factor(totalDat$Embarked)
totalDat$Title <- factor(totalDat$Title)
totalDat$Surname <- factor(totalDat$Surname)
totalDat$Family <- factor(totalDat$Family)
totalDat$FsizeD <- factor(totalDat$FsizeD)
totalDat$Ethnicity <- factor(totalDat$Ethnicity)
set.seed(129)
mice_mod <- mice(totalDat[, !names(totalDat) %in% c('PassengerId','Name','Ticket',
			'Cabin','Family','Surname','Survived','Ethnicity')], method='rf')
mice_output <- complete(mice_mod)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,2))
hist(totalDat$Age, freq=F, main='Age: Original Data', 
  col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
  col='lightgreen', ylim=c(0,0.04))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
totalDat$Age <- mice_output$Age


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(totalDat[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  # "Women and children first!"
  facet_grid(.~Sex) + 
  theme_few()


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
totalDat$Child[totalDat$Age < 18] <- 'Child'
totalDat$Child[totalDat$Age >= 18] <- 'Adult'
totalDat$Mother <- 'Not Mother'
totalDat$Mother[totalDat$Sex == 'female' & totalDat$Parch > 0 & totalDat$Age > 18 & totalDat$Title != 'Miss'] <- 'Mother'
totalDat$Child  <- factor(totalDat$Child)
totalDat$Mother <- factor(totalDat$Mother)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
levels(totalDat$Ethnicity)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Let's replace the values with shorter and more clear versions
totalDat$Ethnicity <- addNA(totalDat$Ethnicity)
levels(totalDat$Ethnicity)[is.na(levels(totalDat$Ethnicity))] <- 'NaN'
levels(totalDat$Ethnicity)[tolower(levels(totalDat$Ethnicity))=='asian and pacific islander'] <- 'Asian'
levels(totalDat$Ethnicity)[tolower(levels(totalDat$Ethnicity))=='black non hispanic'] <- 'Black'
levels(totalDat$Ethnicity)[tolower(levels(totalDat$Ethnicity))=='white non hispanic'] <- 'White'
levels(totalDat$Ethnicity)[tolower(levels(totalDat$Ethnicity))=='hispanic'] <- 'Hispanic'
totalDat <- totalDat[order(totalDat$PassengerId),]
levels(totalDat$Ethnicity) #And the new levels are


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(totalDat[1:891,],aes(x=Ethnicity,fill=factor(Survived))) +
 		geom_bar() +
 		scale_fill_discrete("Survived?") +
 		labs(title= "Survival Rate by Ethnicity")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(totalDat[1:891,],aes(x=Ethnicity,fill=factor(Survived))) +
 		geom_bar() +
 		scale_fill_discrete("Survived?") +
 		labs(title= "Survival Rate by Ethnicity") +
 		facet_grid(.~Sex)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(totalDat[totalDat$Deck!='TT',],aes(x=factor(Deck),fill=factor(Pclass))) +
 		geom_bar() +
 		scale_fill_discrete("Class") +
 		labs(title= "Class separation by decks",x = 'Deck')


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(totalDat[1:891,],aes(x=factor(Pclass),fill=factor(Survived))) +
 		geom_bar() +
 		scale_fill_discrete("Survived?") +
 		labs(title= "Class survival by sex",x = 'Class') +
        facet_grid(.~Sex)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(totalDat[1:891,],aes(x=factor(Pclass),fill=factor(Survived))) +
 		geom_bar() +
 		scale_fill_discrete("Survived?") +
 		labs(title= "Child survival by class",x = 'Class') +
		facet_grid(.~Child)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
totalDat$ChildFrom12 <- 'Not from'
totalDat$ChildFrom12[totalDat$Child=='Child'&totalDat$Pclass==1] <- 'From'
totalDat$ChildFrom12[totalDat$Child=='Child'&totalDat$Pclass==2] <- 'From'
totalDat$ChildFrom12 <- factor(totalDat$ChildFrom12)
totalDat$FemaleFrom12 <- 'Not from'
totalDat$FemaleFrom12[totalDat$Sex=='female'&totalDat$Pclass==1] <- 'From' 
totalDat$FemaleFrom12[totalDat$Sex=='female'&totalDat$Pclass==2] <- 'From' 
totalDat$FemaleFrom12 <- factor(totalDat$FemaleFrom12)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(totalDat[1:891,],aes(x=Mother,fill=factor(Survived))) +
 		geom_bar() +
 		scale_fill_discrete("Survived?") +
 		labs(title= "Survival of Mother by Class",x = 'Mother') +
       	facet_grid(.~Pclass)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
totalDat$MotherFrom12 <- 'Not from'
totalDat$MotherFrom12[totalDat$Mother=='Mother'&totalDat$Pclass==1] <- 'From' 
totalDat$MotherFrom12[totalDat$Mother=='Mother'&totalDat$Pclass==2] <- 'From' 
totalDat$MotherFrom12 <- factor(totalDat$MotherFrom12)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(totalDat[1:891,],aes(x=factor(Child),fill=factor(Survived))) +
 		geom_bar() +
 		scale_fill_discrete("Survived?") +
 		labs(title= "Survival by Child, Discrete family Size and Sex",x = 'Child') +
		facet_grid(.~FsizeD+Sex)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
totalDat$ChildSaved <- 'Not saved'
totalDat$ChildSaved[totalDat$Child=='Child'&totalDat$Sex=='female'&totalDat$FsizeD!='large'] <- 'Saved'
totalDat$ChildSaved[totalDat$Child=='Child'&totalDat$Sex=='male'&totalDat$FsizeD=='small'] <- 'Saved'
totalDat$ChildSaved <- factor(totalDat$ChildSaved)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
corr_data <- totalDat[1:891,]
## transform to numeric type and recodification
corr_data$Embarked <- revalue(corr_data$Embarked, 
			c("S" = 1, "Q" = 2, "C" = 3))
corr_data$Sex <- revalue(corr_data$Sex, 
			c("male" = 1, "female" = 2))
corr_data$Title <- revalue(corr_data$Title, 
			c("Mr" = 1, "Master" = 2,"Crew" = 3, 
			"Mrs" = 4,"Member" = 5,"Miss" = 6, "Rare Title" = 7))
corr_data$FsizeD <- revalue(corr_data$FsizeD, 
			c("small" = 1, "singleton" = 2, "large" = 3))
corr_data$Child <- revalue(corr_data$Child, 
			c("Adult" = 1, "Child" = 2))
corr_data$Mother <- revalue(corr_data$Mother, 
			c("Mother" = 1, "Not Mother" = 2))
corr_data$Mother <- as.numeric(corr_data$Mother)
corr_data$FsizeD <- as.numeric(corr_data$FsizeD)
corr_data$Child <- as.numeric(corr_data$Child)
corr_data$Sex <- as.numeric(corr_data$Sex)
corr_data$Embarked <- as.numeric(corr_data$Embarked)
corr_data$Title <- as.numeric(corr_data$Title)
corr_data$Pclass <- as.numeric(corr_data$Pclass)
corr_data$Survived <- as.numeric(corr_data$Survived)
corr_data$Freq <- as.numeric(corr_data$Freq)
corr_data$Age <- as.numeric(corr_data$Age)
corr_data$ChildFrom12 <- as.numeric(revalue(corr_data$ChildFrom12,
			c("From"=1,"Not from" = 2)))
corr_data$FemaleFrom12 <- as.numeric(revalue(corr_data$FemaleFrom12, 
			c("From"=1,"Not from" = 2)))
corr_data$MotherFrom12 <- as.numeric(revalue(corr_data$MotherFrom12,
			c("From"=1,"Not from" = 2)))
corr_data <-corr_data[,c("Survived", "Pclass", "Sex", 
			"FsizeD", "Age", "Fare", "Mother",
			"Embarked","Title","Child","ChildFrom12",
			"FemaleFrom12","Freq","MotherFrom12")]
mcorr_data <- cor(corr_data)
corrplot(mcorr_data,method="circle")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Split the data back into a train set and a test set
train <- totalDat[1:891,]
test <- totalDat[892:1309,]


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(156)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Fare + Embarked + Title + 
			FsizeD + Freq + ChildSaved + FemaleFrom12 + ChildFrom12, data = train, trees=500)
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
rf.fitted = predict(rf_model)
ans_rf = rep(NA,891)
for(i in 1:891){
  ans_rf[i] = as.integer(rf.fitted[[i]]) - 1
}
# Result
table(ans_rf)

print(rf_model)
mean(ans_rf == train$Survived)
varImpPlot(rf_model, main = "RF_MODEL")
# Var importancies
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# var imp
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Graph importancies
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip()


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

