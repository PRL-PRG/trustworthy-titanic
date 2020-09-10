
library(dplyr)
library(gmodels)
train<-read.table('../input/train.csv', sep=',', stringsAsFactors=F, header=T)
test<-read.table('../input/test.csv', sep=',', stringsAsFactors=F, header=T)
# The two datasets, train and test, are binded together to form a 'full' dataset
test$Survived <- NA
full <- rbind(train, test)

full$Title <- gsub('(.*, )|(\\..*)','', full$Name)
group_by(full,Title) %>% summarize(count=n())

full$Title[full$Title %in% c('Col', 'Major')]  <- 'Mil'
full$Title[full$Title %in% c('Don', 'Capt', 'Jonkheer')]  <- 'Mr'
full$Title[full$Title %in% c('Dona', 'Mme', 'Lady')]  <- 'Mrs'
full$Title[full$Title %in% c('Ms', 'Mlle')]  <- 'Miss'
full$Title[full$Title %in% c('Sir', 'the Countess')]  <- 'Noble'

# adjust Mr/Master according to Age
full$Title[full$Title == 'Master' & full$Age > 13] <- 'Mr'
full$Title[full$Title == 'Mr' & full$Age <= 13] <- 'Master'
# there are women married labeled as 'Miss':
full$Name[grepl('Mrs',full$Name) & full$Title != 'Mrs']
# so I adjust them:
full$Title[grepl('Mrs',full$Name) & full$Title != 'Mrs'] <- 'Mrs'

# new variable 'Surname' added:
full$Surname <- gsub('(, .*)','',full$Name)

# some tickets have non-numeric characters before the number; they must be removed:
CastTicketsToNumbers <- function(ticket) {
    resultado <- ticket
    for (i in seq(1,length(ticket))) {
        espacio <- regexpr(' ', ticket[i])[1]
        if (espacio > 0) {
            resultado[i] <- substr(ticket[i],espacio+1,nchar(ticket[i]))
            if (is.na(suppressWarnings(as.numeric(resultado[i])))) {
                espacio <- regexpr(' ', resultado[i])[1]
                if (espacio > 0) resultado[i] <- substr(resultado[i],espacio+1,nchar(resultado[i]))
            }
        }
    }
   resultado
}
full$TicketNumber <- CastTicketsToNumbers(full$Ticket)
    
# now there are only 4 passengers with tickets not numeric:
full[is.na(as.numeric(full$TicketNumber)),]


x <- group_by(full, TicketNumber, Surname) %>% summarize(n=n()) %>% arrange(TicketNumber, Surname)
x$TicketAlt <- x$TicketNumber
previous_ticket <- -1
previous_surname <- ''
for (i in seq(1, nrow(x))) {
    if ((suppressWarnings(as.numeric(x[i,1])) - previous_ticket == 1) & (x[i,2] == previous_surname)) 
        x$TicketAlt[i] <- previous_ticket
    else {
        previous_ticket <- suppressWarnings(as.numeric(x[i,1]))
        previous_surname <- x[i,2]
    }
}
full <- merge(full, x)
full <- merge(full, group_by(full,TicketAlt) %>% summarize(GroupSize=n()))
full$Family <- paste(full$Surname, full$TicketAlt)
full <- merge(full, group_by(full,Family) %>% summarize(FamilySize=n()))

fc <- data.frame(
	FamilySize=c(1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 11, 11, 11, 11, 4),
	SibSp=c(0, 1, 0, 2, 1, 0, 1, 0, 2, 1, 2, 0, 3, 1, 3, 0, 4, 1, 4, 0, 5, 1, 5, 0, 6, 1, 8, 0, 9, 3), 
	Parch=c(0, 0, 1, 0, 1, 2, 2, 3, 1, 3, 2, 4, 1, 4, 2, 5, 1, 5, 2, 6, 1, 6, 2, 7, 1, 9, 2, 10, 1, 0),
	ruleId=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29),
	Type=c('alone', 'unknown', 'unknown', 'brother', 'unknown', 'unknown', 'unknown', 'parent', 'child', 'parent', 'child', 'parent', 
		'child', 'parent', 'child', 'parent', 'child', 'parent', 'child', 'parent', 'child', 'parent', 'child', 'parent', 'child', 'parent',
		'child', 'parent', 'child', 'brother'),
    stringsAsFactors=F
)
fc

# merging both data frames, full and fc, will add a new column 'Type' in full data frame:
full <- merge(x=full, y=fc, all.x=T)

sum(is.na(full$Type))
# there are 117 rows that could not be properly classified.
# The causes are multiple:
# a) Surnames registered not exactly in the same way
#    for all family members (for instance, Frolicher's Family)
# b) Family structures more complex than envisaged (with grandparents, unlces, etc).
#    Although the maiden name is present in the 'Name' column, I have not even tried to use it.
# c) Family members travelling with several tickets that are not correlative.
# d) Mistakes registering 'SibSp' and 'Parch' values ?

# However, some data manipulation can be made in order to classify the passengers:
filter.1 <- (filter(full, is.na(Type)) %>% group_by(Family, FamilySize) %>% summarize(n=n()) %>% 
             filter(n != FamilySize))$Family
full[full$Family %in% filter.1 & full$FamilySize==2 & is.na(full$ruleId), 
     c('SibSp', 'Parch', 'ruleId', 'Type')] <- list(1, 0, 1, 'unknown')
full[full$Family %in% filter.1 & full$FamilySize==3, 
     c('SibSp', 'Parch', 'ruleId', 'Type')] <- list(1, 1, 4, 'unknown')

filter.2 <- (filter(full, is.na(Type)) %>% group_by(Family, FamilySize) %>% summarize(n=n()) %>% 
             filter(n == FamilySize))$Family
full[full$Family %in% filter.2 & full$SibSp==0 & full$Parch==0, 
     c('SibSp', 'Parch', 'ruleId', 'Type')] <- list(1, 0, 1, 'unknown')

filter.3 <- (filter(full, is.na(Type)) %>% group_by(Family, FamilySize) %>% summarize(n=n()) %>% 
             filter(n == FamilySize))$Family
filter.4 <- (filter(full, Family %in% filter.3) %>% group_by(Surname) %>% summarize(n=n()) %>% 
             filter(n == 2))$Surname
full[full$Surname %in% filter.4 & is.na(full$Type), 
     c('SibSp', 'Parch', 'FamilySize', 'ruleId', 'Type')] <- list(1, 0, 2, 1, 'unknown')

filter.5 <- (filter(full, is.na(Type)) %>% group_by(Family) %>% summarize(n=n()) %>% 
             filter(n==1))$Family
full[full$Family %in% filter.5 & is.na(full$Type), 
     c('SibSp', 'Parch', 'ruleId', 'Type')] <- list(0, 0, 0, 'alone')


# There are 5 rules (Id numbers 1, 2, 4, 5 and 6) that have 'Type'=='unknown'
# Now we are going to assign them to the proper Type:

# ruleId ==1 : husband and wife without chidren or brothers.
# If one of them's Title is 'Mrs', then they are husband/wife
full$Type[full$ruleId==1 & 
          full$Title=='Mrs' & 
          full$Family %in% (filter(full, ruleId==1, Sex=='male') %>% select(Family))[,1]] <- 'wife'
full$Type[full$ruleId==1 & 
          full$Sex=='male' & 
          full$Family %in% (filter(full, ruleId==1, Title=='Mrs') %>% select(Family))[,1]] <- 'husband'
full$Type[full$ruleId==1 & full$Type=='unknown'] <- 'brother'

#ruleId ==2 : father or mother + child. The child is the youngest:
minAgeRule2 <- filter(full, ruleId==2, !is.na(Age)) %>% group_by(Family, FamilySize) %>% summarize(total=n(), minimum=min(Age)) %>% filter(FamilySize==total)
FindYoungest <- function(x,y) {
    for (i in seq(nrow(x))) {
        for (j in seq(nrow(y))) {
            if (y$Family[j] == x$Family[i] & y$Type[j] == 'unknown' & y$Age[j] == x$minimum[i])
                y$Type[j] <- 'child'
        }
    }
y
} 
full <- FindYoungest(minAgeRule2, full)
full$Type[full$ruleId==2 & full$Type=='unknown' & full$Title=='Mrs'] <- 'mother'
full$Type[full$ruleId==2 & full$Type=='unknown' & full$Title=='Miss'] <- 'child'
full$Type[full$ruleId==2 & full$Type=='unknown' & full$Title=='Master'] <- 'child'
full$Type[full$ruleId==2 & 
          full$Type=='unknown' & 
          full$Family %in% (filter(full, ruleId==2, Type=='mother') %>% select(Family))[,1]] <- 'child'
full$Type[full$ruleId==2 & 
          full$Type=='unknown'& 
          full$Family %in% (filter(full, ruleId==2, Type=='child') %>% select(Family))[,1]] <- 'father'

# ruleId ==4 and 5: they could be 1 father or mother with 2 children, or father, mother and one child:
# The child is the youngest:
minAgeRule5 <- filter(full, ruleId %in% c(4,5), !is.na(Age)) %>% group_by(Family, FamilySize) %>% summarize(total=n(), minimum=min(Age)) %>% filter(FamilySize==total)
full <- FindYoungest(minAgeRule5, full)
full$Type[full$ruleId %in% c(4,5) & full$Title=='Mrs'] <- 'mother'
full$Type[full$ruleId %in% c(4,5) & full$Title=='Master'] <- 'child'
full$Type[full$ruleId %in% c(4,5) & full$Title=='Miss'] <- 'child'
full$Type[full$ruleId %in% c(4,5) & 
          full$Type=='unknown' & 
          full$Family %in% (filter(full, ruleId %in% c(4,5), Type=='mother', SibSp==0) %>% select(Family))[,1]] <- 'child'
full$Type[full$ruleId %in% c(4,5) & 
          full$Type=='unknown' & 
          full$Family %in% (filter(full, ruleId %in% c(4,5), Type=='mother', SibSp==1) %>% select(Family))[,1] & 
          full$Family %in% (filter(full, ruleId %in% c(4,5), Type=='child', SibSp==0) %>% select(Family))[,1] ] <- 'father'
full$Type[full$ruleId %in% c(4,5) & 
          full$Type=='unknown'  & 
          !full$Family %in% (filter(full, ruleId %in% c(4,5), Type=='mother') %>% select(Family))[,1]] <- 'father'

# ruleId ==6 : father, mother and 2 children
minAgeRule6 <- filter(full, ruleId==6, !is.na(Age)) %>% group_by(Family, FamilySize) %>% summarize(total=n(), minimum=min(Age)) %>% filter(FamilySize==total)
full <- FindYoungest(minAgeRule6, full)
full$Type[full$ruleId==6 & full$Title=='Mrs'] <- 'mother'
full$Type[full$ruleId==6 & full$Title=='Master'] <- 'child'
full$Type[full$ruleId==6 & full$Title=='Miss'] <- 'child'
full$Type[full$ruleId==6 & 
          full$Type=='unknown' & 
          full$Family %in% (filter(full, ruleId==6, Type!='unknown') %>% group_by(Family) %>% summarize(n=n()) %>% filter(n==3))$Family] <- 'father'

# finally, we separate mothers and fathers of the previous rules:
full$Type[full$Type=='parent' & full$Sex=='female'] <- 'mother'
full$Type[full$Type=='parent' & full$Sex=='male'] <- 'father'

# Let's see the results:
with(filter(full,!is.na(Survived)), CrossTable(Type, Survived))
