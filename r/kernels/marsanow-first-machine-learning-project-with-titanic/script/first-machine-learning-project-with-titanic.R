## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----"Get the data"--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Get the training & test datasets and assign them to the appropriate variables
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")


## ----Packages, message=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Data manipulation
library(dplyr)
library(tidyr)
library(plyr)
# Data visualization
library(waffle)
library(ggplot2)
library(lattice)
# Missing value imputation
library(mice)
# Prediction models
library(randomForest)
library(party)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Structure of the train dataset
str(train)
# Structure of the test dataset
str(test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Reload the training & test datasets with the correct setting
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)


## ----Combine both datasets Structure---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
both_sets <- bind_rows(train, test)
# Check the structure of the new dataset to see if the merge was successful
str(both_sets)


## ----Passenger Proportion vs Sex-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# % of passengers grouped by Sex
round((prop.table(table(both_sets$Sex))*100), 1)


## ----Passenger Proportion Survived vs Sex----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# % of passengers grouped by the variables Sex & Survived
round((prop.table(table(both_sets$Sex, both_sets$Survived), 2)*100), 1)


## ----Waffle chart Female vs Male-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Waffle chart of the comparison
iron(
        waffle(c(female = 36, male = 64), rows = 5,  
               colors = c("#C93414", "#144EC9"), title = "All passengers grouped by Sex (%)"),
        waffle(c(female = 68, male = 32), rows = 5, 
               colors = c("#C93414", "#144EC9"), title = "Survivors grouped by Sex (%)")
)


## ----Create Child Variable-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Filter out the children and create a new variable
both_sets$Child <- 0
both_sets$Child[both_sets$Age < 16] <- 1


## ----FTable vs Sex and Child-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a proportions table
round((prop.table(ftable(both_sets[c("Sex", "Child", "Survived")]), 1)*100), 1)


## ----Prepare the dataframe Child vs Adult----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Aggregate the proportions into a data frame
chart <- aggregate(Survived ~ Sex + Child, data=both_sets, FUN=function(x) {round((sum(x)/length(x)*100), 1)})
# Prepare the data frame for ggplot
chart <- unite(chart, "Group", c("Sex", "Child"))
chart[, 1] <- c("Women", "Men", "Girls", "Boys")
chart$Group <- factor(chart$Group, levels = chart$Group[order(chart$Survived)])
chart <- arrange(chart, Group, Survived)


## ----Percentage of Survivors Child Adult Graph-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot the data frame
ggplot(data=chart, aes(x=Group, y=Survived, fill=Group)) +
        ggtitle("Percentage of Survivors for each group")+
        geom_bar(stat="identity", width=0.5)+
        scale_fill_manual(values=c("#144EC9", "#00ABF0", "#F08300", "#C93414"))+
        geom_text(aes(label=Survived), hjust=1.5, color="white", size=5)+
        scale_y_continuous(name ="Survived (%)")+
        expand_limits(y=c(0,100))+
        coord_flip()+
        theme(plot.title = element_text(hjust = 0.5))


## ----Compare Survived vs Pclass Child Sex----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round((prop.table(ftable(both_sets[c("Pclass", "Child", "Sex", "Survived")]), 1)*100), 1)


## ----Prepare the dataframe Survived vsPclass Child Sex---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Aggregate the proportions into a data frame
chart2 <- aggregate(Survived ~ Child + Sex + Pclass, data=both_sets, FUN=function(x) {round((sum(x)/length(x)*100), 1)})
# Prepare the data frame for ggplot
chart2 <- unite(chart2, "Group", c("Sex", "Child"))
chart2$Group <- mapvalues(chart2$Group, from = c("female_0", "female_1", "male_0", "male_1"), to = c("Women", "Girls", "Men", "Boys"))


## ----Plot Survivors by Class-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot the data frame
ggplot(data=chart2, aes(x=Pclass, y=Survived, fill=Group)) +
        ggtitle("Percentage of Survivors Grouped by Pclass, Sex and Child Variable")+
        geom_bar(stat="identity", position=position_dodge(), width=0.9)+
        scale_fill_manual(values=c("#00ABF0", "#F08300", "#144EC9", "#C93414"))+
        geom_text(aes(label=Survived), position = position_dodge(0.9), vjust=1.5, hjust=0.5, color="white", size=3.5)+
        scale_y_continuous(name ="Survived %")+
        theme(plot.title = element_text(hjust = 0.5))



## ----Child that died from 1st class----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
na.omit(both_sets[both_sets$Survived==0 & both_sets$Pclass==1 & both_sets$Child==1, ])


## ----Check travel companions-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
both_sets[both_sets$Ticket==113781, ]


## ----Summary Fare across Pclass--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check the summary of the Fare distribution across class
tapply(both_sets$Fare, both_sets$Pclass, summary)


## ----Boxplot Fare vs Pclass, message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set Pclass as a factor
both_sets$Pclass <- factor(both_sets$Pclass)
# Create a boxplot
ggplot(both_sets, aes(x=Pclass, y=Fare, color=Pclass)) +
        geom_boxplot(fill="white", position="dodge")+
        theme(legend.position="right")+
        labs(y = "Fare (£)")+
        scale_color_brewer(palette="Dark2")+
        stat_summary(fun.y=mean, geom="point", shape=2, size=4)


## ----Boxplot Zoom Fare vs Pclass, message=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(both_sets, aes(x=Pclass, y=Fare, color=Pclass)) +
        geom_boxplot(fill="white", position="dodge")+
        theme(legend.position="right")+
        labs(y = "Fare (£)")+
        scale_color_brewer(palette="Dark2")+
        stat_summary(fun.y=mean, geom="point", shape=2, size=4)+
        coord_cartesian(ylim=c(0, 75))


## ----Fare1 intervals-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Split the Fare variable into intervals
both_sets$Fare1[both_sets$Fare == 0] <- '0'
both_sets$Fare1[both_sets$Fare > 0 & both_sets$Fare < 9] <- '<9'
both_sets$Fare1[both_sets$Fare >= 9 & both_sets$Fare < 16] <- '9-16'
both_sets$Fare1[both_sets$Fare >= 16 & both_sets$Fare < 26] <- '16-26'
both_sets$Fare1[both_sets$Fare >= 26 & both_sets$Fare < 40] <- '26-40'
both_sets$Fare1[both_sets$Fare >= 40] <- '40+'


## ----Fare1 vs Pclass Sex Child---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Aggregate the proportions into a data frame
chart3 <- aggregate(Survived ~ Pclass + Fare1 + Sex + Child, data=both_sets, FUN=function(x) {sum(x)/length(x)*100})
chart3


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Merge Sex and Child Variable to create 4 groups for Men, Women, Girls and Boys
chart3 <- unite(chart3, "Group", c("Sex", "Child"))
chart3$Group <- mapvalues(chart3$Group, from = c("female_0", "female_1", "male_0", "male_1"), to = c("Women", "Girls", "Men", "Boys"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Convert Fare1 to a factor, check the order of the levels
chart3$Fare1 <- factor(chart3$Fare1)
print(levels(chart3$Fare1))
# Rearrange the order
chart3$Fare1 <- factor(chart3$Fare1,levels(chart3$Fare1)[c(2,1,6,3,4,5)])
# Check if correct
print(levels(chart3$Fare1))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot the data frame
levels(chart3$Pclass) <- c("Class 1", "Class 2", "Class 3")
ggplot(chart3, aes(x=Fare1, y=Survived, col=Fare1))+
        geom_point()+
        ggtitle("How survival compares across Groups, Class and Fare")+
        facet_grid(Pclass~Group)+
        labs(x = "Fare (£)", y="Survived %")+
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle=45, hjust=1),
              legend.position = "none")


## ----Check missing Fare----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
which(is.na(both_sets$Fare))
both_sets[1044, ]


## ----Fix Fare--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
both_sets$Fare[1044] <- median(na.omit(both_sets$Fare[both_sets$Pclass==3]))


## ----Output Fixed Fare-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
both_sets[1044, ]


## ----Create Deck variable--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Extract the first letter from the cabin numbers
both_sets$Deck <- sapply(both_sets$Cabin, FUN=function(x) {strsplit(x, split='')[[1]][1]})


## ----Deck variable table---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check the distribution of the deck levels
table(both_sets$Deck)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(both_sets$Deck, both_sets$Pclass)


## ----Deck data manipulation------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Fix the deck letters
both_sets$Deck[both_sets$Deck=="G"] <- "F"
both_sets$Deck[both_sets$Deck=="T"] <- "C"
# Check if the changes were made correctly
table(both_sets$Deck)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Change the NA values to 0, because Random Forest doesn't handle missing values
both_sets <- both_sets %>%
     mutate(Deck=replace(Deck, is.na(Deck), 0))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Convert the Deck variable into a factor
both_sets$Deck <- factor(both_sets$Deck)


## ----Check Embarked variable-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check which "Embarked" values are missing
which(both_sets$Embarked=="")

## ----Print Missing from the Embarked variable------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Print the rows with missing "Embarked" values
both_sets[c(62, 830), ]


## ----Check others with similar ticket--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
otherTickets <- na.omit(both_sets[both_sets$Fare1 =="40+" & both_sets$Pclass==1 & both_sets$Deck=="B", ])
table(otherTickets$Embarked)


## ----Fix the missing values for Embarked-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
both_sets$Embarked[c(62, 830)] <- "S"
# Check if changes were made correctly
both_sets[c(62, 830), ]
# Convert Embarked variable to a factor
both_sets$Embarked <- factor(both_sets$Embarked)


## ----Extract titles--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Extract titles from names
both_sets$Title <- sapply(both_sets$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
both_sets$Title <- sub(" ", "", both_sets$Title)
# Explore titles and age
ftable(both_sets$Child, both_sets$Title)


## ----Title vs Sex----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ftable(both_sets$Sex, both_sets$Title)


## ----Group titles----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Group titles by the same category
both_sets$Title[both_sets$Title %in% c("Miss", "Mlle", "Ms")] <- "Miss"
both_sets$Title[both_sets$Title %in% c("Mrs", "Mme")] <- "Mrs"
both_sets$Title[both_sets$Title %in% c("Capt", "Col", "Don", "Jonkheer", "Major", "Rev", "Sir")] <- "Sir"
both_sets$Title[both_sets$Title %in% c("Dona", "Lady", "the Countess")] <- "Lady"
both_sets$Title[both_sets$Title %in% c("Dr") & both_sets$Sex=="female"] <- "Mrs"
both_sets$Title[both_sets$Title %in% c("Dr") & both_sets$Sex=="male"] <- "Mr"
# Convert Title variable to a factor
both_sets$Title <- as.factor(both_sets$Title)


## ----Create Family Variable------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a variable "FamilySize"
both_sets$FamilySize <- both_sets$SibSp + both_sets$Parch + 1

# Extract last names to form "LastName" variable
both_sets$LastName <- sapply(both_sets$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

# Combine last names and family size to form families
both_sets$Family <- paste(as.character(both_sets$FamilySize), both_sets$LastName, sep=" ")
FamilyTable <- data.frame(table(both_sets$Family))

# Separate passengers by single, small families, and leave the large families as they are
both_sets <- both_sets %>%
        mutate(Family=replace(Family, FamilySize==1, "Single"))

both_sets <- both_sets %>%
        mutate(Family=replace(Family, FamilySize==2 | FamilySize==3, "Small"))

# Convert to a factor
both_sets$Family <- factor(both_sets$Family)
str(both_sets$Family)


## ----Distinct Values of Ticket---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
n_distinct(both_sets$Ticket)


## ----Variable TicketGroups, message=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a new data frame with frequencies of each ticket
TicketFreq <- data.frame(table(both_sets$Ticket))
# Create a new column combining the ticket number and how many times it repeats
TicketFreq$TicketGroups <- paste(as.character(TicketFreq$Var1), TicketFreq$Freq, sep=" ")
# Rename the first column so that this data frame can be combined with both_sets
TicketFreq <- dplyr::rename(TicketFreq, Ticket = Var1)
both_sets <- left_join(x = both_sets, y = TicketFreq, by="Ticket")

# Separate values into different groups
both_sets <- both_sets %>%
     mutate(TicketGroups=replace(TicketGroups, Freq==1, "unique"))

both_sets <- both_sets %>%
     mutate(TicketGroups=replace(TicketGroups, Freq==2, "pair"))

both_sets <- both_sets %>%
     mutate(TicketGroups=replace(TicketGroups, Freq==3, "groupof3"))

# Set the variable as a factor
both_sets$TicketGroups <- factor(both_sets$TicketGroups)
str(both_sets$TicketGroups)


## ----Check Age summary-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(both_sets$Age)

## ----Plot Age--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(both_sets, aes(x=Age)) +
        geom_histogram(binwidth=1, color="white", fill="steelblue2") +
        labs(x="Age (years)")+
        geom_vline(aes(xintercept=mean(Age, na.rm=TRUE)), color="purple", linetype="dashed", size=1, show.legend = TRUE) +
        geom_vline(aes(xintercept=median(Age, na.rm=TRUE)), color="green", linetype="dashed", size=1, show.legend = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# use the MICE package to impute missing Age values using only the desired variables
missingAge <- mice(both_sets[, c("Age", "Pclass", "Sex", "SibSp", "Parch", "Fare", "Embarked", "Title" )], m = 1, method ='cart', seed = 78956, printFlag=FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check the density plot of the imputed values (in red) with the original values (in blue)
densityplot(missingAge, ~Age)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check how the imputed values (in red) compare with the original values (in blue) across the 
# Title variable
xyplot(missingAge, Age ~ Title)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check how the imputed values (in red) compare with the original values (in blue) across the 
# Fare variable
xyplot(missingAge, Age ~ Fare)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Get back the completed dataset
agePrediction <- complete(missingAge)
# Check the distribution of the predicted Age variable and compare it with the original
# Original
summary(both_sets$Age)
# Predicted
summary(agePrediction$Age)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Merge the Prediction with the original dataset
both_sets$Age <- agePrediction$Age


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(both_sets)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Change variables that will be used into factors
library(dplyr) 
both_sets <- both_sets %>%
    mutate(
        Survived = as.factor(Survived),
        Sex = as.factor(Sex),
        SibSp = as.factor(SibSp),
        Parch = as.factor(Parch),
        Fare1 = as.factor(Fare1)
    )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Spit the train and test datasets
train <- both_sets[1:891, ]
test <- both_sets[892:1309, ]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use the Random Forest model for prediciton
set.seed(190340)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Deck + Title + FamilySize + Family + TicketGroups,
                    data=train,
                    importance=TRUE,
                    ntree=2000)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Inspect the Importance of each variable
data <- as.data.frame(cbind(rownames(fit$importance), round(fit$importance[,"MeanDecreaseGini"], 1)))
colnames(data) <- c("Variables","MeanDecreaseGini")
data$MeanDecreaseGini <- as.numeric(as.character(data$MeanDecreaseGini))
data$Variables <- factor(data$Variables, levels = data$Variables[order(data$MeanDecreaseGini)])
# Plot the Importance using ggplot
ImportancePlot <- ggplot(data) + 
        geom_point(aes(MeanDecreaseGini,Variables), colour="blue")
ImportancePlot


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "RF Prediction 1", row.names = FALSE)
# Score: 0.71291


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Remove the 3 least important variables & try different settings
set.seed(897654)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Deck + Title + FamilySize + Family + TicketGroups,
                    data=train,
                    importance=TRUE,
                    mtry=2,
                    nodesize=3,
                    ntree=2001)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Submit the new prediction
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "RF Prediction 2", row.names = FALSE)
# Score: 0.70813


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Try the cforest function the party package
set.seed(6690)
cfit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Deck + Title + FamilySize + Family + TicketGroups,
               data=train,
               controls=cforest_unbiased(ntree=2000, mtry=3))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Submit the Prediction
Prediction2 <- predict(cfit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction2)
write.csv(submit, file = "Final prediction.csv", row.names = FALSE)
#Score: 0.81339

