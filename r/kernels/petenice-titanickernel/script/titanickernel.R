# Titanic Data set
# Exercise
library(caret)
library(randomForest)
library(lava)
library(dplyr)
library(rpart)
library(ggplot2)
library(data.table)
library(xgboost)

# Read file
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")

test$Survived = NA
full = rbind(train, test)

# Separate Names
full$LastName <- gsub("^(.*), .*?\\..*$", "\\1", full$Name)
full$Title <- trim(gsub("^.*,(.*?)\\..*$", "\\1", full$Name))
full$Title <- lava::trim(full$Title)
full$FirstName <- gsub("^.*,.*?\\.[[:space:]]([[:graph:]]*).*$", "\\1", full$Name)
full$FirstName <- gsub("[\\(|\\)]", "", full$FirstName)

full$OtherName <- ifelse(grepl('^.+\\({1}.+[[:space:]](\\w*)\\){1}$', full$Name), 
       gsub('^.+\\({1}.+[[:space:]](\\w*)\\){1}$', '\\1', full$Name),
       "")
      

# Start with the Group Number as the ticket number. 
# We will modify this field to join some groups
# that had separate ticket entries
#full$GroupNumber <- full$Ticket

# Get Number in Group

set.seed(1232313)

full$Ticket <- as.character(full$Ticket)

InGroups <- full %>% group_by_at(vars(LastName, Ticket)) %>%
  dplyr::mutate(GroupNumber=sample(1:1000000,1))

set.seed(1232313)
InGroups <- full %>% group_by(Ticket) %>% 
    dplyr::mutate(GroupNumber=sample(1:1000000,1))
                  

AdjustGroupNumber <- function(GroupNumber, LastName) {
  NumGroupNumbers <- length(unique(GroupNumber))
  if (NumGroupNumbers == 1) {
    GroupNumber
  } else {
    newGroupNumber = sample(1:1000000,1)
    gn <- unique(GroupNumber)
    for (each in gn) {
      InGroups[which(InGroups$GroupNumber==each),]$GroupNumber <- newGroupNumber
    }
    newGroupNumber
  }
 }

InGroups <- InGroups %>% group_by(LastName) %>%
    mutate(GroupNumber=AdjustGroupNumber(GroupNumber, LastName))

### Let's determine age of the missing
## Rather than just average mr, master, mrs, ms, let's focus 
# instead on size of group and gender ?

InGroups <- InGroups %>% group_by(GroupNumber) %>%
    mutate(NumInGroupNumber=n())

#Piecemeal?

## Clean up the other names and the 00 in the singles
## and error table?

# Select only the single males?

# Group by SibSp and Parch and Sex 
# And then impute the age for the known ages
InGroups <- InGroups %>% group_by_at(vars(SibSp, Parch, Sex)) %>%
                                mutate(Age=ifelse(is.na(Age), median(Age, na.rm=TRUE), Age),
                                       ImputedAge=median(Age, na.rm=TRUE))
  
#ggplot(data=as.data.frame(table(full$Title)), aes(Var1, Freq)) +
#       geom_bar(stat="identity", fill="red") + 
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Find other missing features

# Impute missing values with the mean of each bucket
# Do the same for test set?
#for (titlename in unique(full$Title)) {
#    titleAge <- subset(full, Title==titlename & !is.na(Age), select=Age)
#    meanTitleAge <- mean(titleAge$Age)
#    print(paste0("Mean Age for a ", titlename, " is ", as.character(meanTitleAge))) 
#    full$Age[full$Title==titlename & is.na(full$Age)] = meanTitleAge
#}

# Break down ticket price by person
# Group by tickets
ticketNumbers <- plyr::count(InGroups, c('Ticket'))
colnames(ticketNumbers)[2] <- "PeoplePerTicket"
InGroups <- merge(InGroups, ticketNumbers, by="Ticket", all.x=TRUE)
InGroups <- InGroups %>% dplyr::mutate(FarePerPerson = Fare / PeoplePerTicket)

# Impute the missing fare
MedianFare <- median(subset(InGroups, Pclass==3 & FarePerPerson < 19)$FarePerPerson)
InGroups$Fare[is.na(InGroups$Fare)] = MedianFare
InGroups$FarePerPerson[is.na(InGroups$FarePerPerson)] = MedianFare

# Group Siblings-Spouse and Parent-Child? and add self
#full <- full %>% dplyr::mutate(TotalInGroup = SibSp + Parch + 1)

# Convert passenger class to factor
InGroups$Pclass <- as.factor(InGroups$Pclass)
InGroups$Survived <- as.factor(InGroups$Survived)
InGroups$GroupNumber <- as.factor(InGroups$GroupNumber)
#full$TotalInGroup <- as.factor(full$TotalInGroup)

# Seperate into train and test sets again
test <- InGroups[is.na(InGroups$Survived),]
train <- InGroups[!is.na(InGroups$Survived),]

test$Set <- "test"
train$Set <- "train"
    
# Fit
#fit <- randomForest(Survived ~ Pclass + Sex + Age + TotalInGroup + Fare,
                     #     data=train)

fit <- rpart(Survived ~ Pclass + Sex + Age + NumInGroupNumber + FarePerPerson,
                    data=train)


#Try to train a random forest model
predict <- predict(fit, test)

predict <- as.data.frame(predict)
test$Survived <- round(predict$"1")

#output <- rbind(train, test)

output <- test %>% select(PassengerId, Survived)

write.csv(output, "output.csv", row.names=FALSE)