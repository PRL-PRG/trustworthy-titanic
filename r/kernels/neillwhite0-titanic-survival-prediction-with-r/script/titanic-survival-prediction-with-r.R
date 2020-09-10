## ----getpics, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load images and store locally
library("curl");
imageFile = "titanic.jpg";
if ( ! file.exists( imageFile )){
    curl_download(url = "http://cbsnews1.cbsistatic.com/hub/i/r/2017/01/04/b1b74071-3301-49ee-93bd-82e47c67d3a8/thumbnail/1200x630/0f08f16522eb0723e8d147cc809bc3d1/0103-eve-titanicfire-phillips-1223384-640x360.jpg", destfile = imageFile);
}
imageFile = "TitanicModelSummary.png";
if ( ! file.exists( imageFile )){
    curl_download(url = "http://neillwhite.dynu.net/DataScience/TitanicModelSummary.png", destfile = imageFile);
}


## ----trainsample, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load raw data
train <- read.csv("../input/train.csv", header = TRUE);
edaTrain = train;
knitr::kable( head(train), format="markdown", longtable=TRUE)


## ----testsample, echo=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load libraries
library(stringr);
library(assertthat);
library(dplyr, quietly=TRUE);
library(leaps);
# Load raw data
test <- read.csv("../input/test.csv", header = TRUE);

# Add a "Survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep(NA, nrow(test)), test[,]);

# Combine data sets
data_combined <- rbind(train, test.survived);
#data_combined$survived <- as.factor(data_combined$survived)

knitr::kable( head(test), format="markdown", longtable=TRUE)


## ----eda1------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# go through each variable and if it's an empty factor or numeric NA, sum each column
nlTrainNA = sapply( train, function(x) switch( class(x), factor = sum(x==""), sum( is.na(x) ) ) );
tTrainNA = t(nlTrainNA);                                 # transpose named list
dfTrainNA = data.frame( tTrainNA );                      # convert to dataframe


## ----eda1table, echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::kable( dfTrainNA, format="markdown", longtable=TRUE);


## ----eda2------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
nlTestNA = sapply( test, function(x) switch( class(x), factor = sum(x==""), sum( is.na(x) ) ) );
tTestNA = t(nlTestNA);                                 # transpose named list
dfTestNA = data.frame( tTestNA );                      # convert to dataframe


## ----eda2table, echo=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library( ggplot2 );
knitr::kable( dfTestNA, format="markdown", longtable=TRUE);


## ----eda_pclass1-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = factor(Pclass), fill = factor(Pclass))) +
  geom_bar( show.legend=FALSE) +
  xlab("Pclass") +
  ylab("Total Count")


## ----eda_pclass2-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(width = 0.5, position="dodge") +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")


## ----eda_name--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(as.character(train$Name),n=20);


## ----eda_sex1--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = factor(Sex), fill = factor(Sex))) +
  geom_bar( show.legend=FALSE) +
  xlab("Sex") +
  ylab("Total Count")


## ----eda_sex2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = factor(Sex), fill = factor(Survived))) +
  geom_bar(width = 0.5, position="dodge") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")


## ----eda_age1--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(subset(train, !is.na(Age)), aes(x = Age)) +
  geom_histogram(binwidth=4) +
  xlab("Age") +
  ylab("Total Count");
ggplot(subset(train,!is.na(Age)), aes(y=Age,x="")) + geom_boxplot();


## ----eda_age2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(subset(train,!is.na(Age)), aes(x = Age, fill = factor(Survived))) +
  geom_density( position="stack") +
  xlab("Age") +
  ylab("Total Count") +
  labs(fill = "Survived")


## ----eda_sibsp1------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = SibSp )) +
   geom_histogram(binwidth=0.5) +
   xlab("Number of Siblings/Spouses") +
   ylab("Total Count")


## ----eda_sibsp2------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = SibSp, fill = factor(Survived))) +
   geom_histogram(binwidth=0.5, position="dodge") +
   xlab("Number of Siblings/Spouses") +
   ylab("Total Count") +
   labs(fill = "Survived")


## ----eda_parch1------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = Parch )) +
   geom_histogram(binwidth=0.5) +
   xlab("Number of Parents/Children") +
   ylab("Total Count")


## ----eda_parch2------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = Parch, fill = factor(Survived))) +
   geom_histogram(binwidth=0.5, position="dodge") +
   xlab("Number of Parents/Children") +
   ylab("Total Count") +
   labs(fill = "Survived")


## ----eda_ticket------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(as.character(train$Ticket),n=20);


## ----eda_fare1-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = Fare)) +
  geom_histogram(binwidth=4) +
  xlab("Fare") +
  ylab("Total Count");
ggplot(train, aes(y=Fare,x="")) + geom_boxplot();


## ----eda_fare2-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = Fare, fill = factor(Survived))) +
  geom_density( position="stack") +
  xlab("Fare") +
  ylab("Total Count") +
  labs(fill = "Survived")


## ----eda_cabin1------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
levels(train$Cabin);


## ----eda_cabin2, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cabinLetter = ifelse(train$Cabin == "", NA, substr(train$Cabin,1,1));
cabinREs = gregexpr("\\d+",train$Cabin, perl=TRUE);
cnMatches = regmatches( train$Cabin, cabinREs);
cabinNumber = numeric(length(cnMatches));
for ( i in 1:length(cnMatches) ){
  cabinNumber[i] = mean( as.numeric( unlist( cnMatches[i] )));
}
edaTrain$CabinLetter <- as.factor(cabinLetter);
edaTrain$CabinNumber <- cabinNumber;
ggplot( edaTrain, aes(x=CabinNumber,y=Survived,color=Survived ) ) + 
  geom_point( shape=1,position=position_jitter(height=0.25)) +
  ggtitle("Survivability by Cabin Number") +
  xlab("Cabin Number") +
  ylab("Survived");
ggplot(subset(edaTrain, !is.na(cabinLetter)), aes(x = CabinLetter, fill = as.factor(Survived))) +
  geom_bar() +
  ggtitle("Survivability by Ticket Letter") +
  xlab("Cabin Letter") +
  ylab("Total Count") +
  labs(fill = "Survived");


## ----eda_cabin3, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tbl = table( edaTrain$Survived, edaTrain$CabinLetter);
tbl
chisq.test(tbl);


## ----eda_cabin4------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
edaTrain$CabinAssignment[ edaTrain$Cabin != "" ] <- "Assigned";
edaTrain$CabinAssignment[ edaTrain$Cabin == "" ] <- "Unassigned";
data_combined$CabinAssignment[ data_combined$Cabin != "" ] <- "Assigned";
data_combined$CabinAssignment[ data_combined$Cabin == "" ] <- "Unassigned";
data_combined$CabinAssignment = factor( data_combined$CabinAssignment );
ggplot(edaTrain, aes(x=CabinAssignment, fill=factor(Survived))) +
  geom_bar() + 
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") + 
  xlab("Cabin Assignment") +
  ylab("Total Count") +
  labs(fill="Survived");


## ----eda_embarked1, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::kable( train[train$Embarked == "",], format="markdown", longtable=TRUE);


## ----eda_embarked2---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot( edaTrain, aes(x=factor(Pclass),fill=factor(Survived)))+
  geom_bar() + 
  facet_wrap(~Embarked) +
  ggtitle("Survivability by Port of Embarkation and Passenger Class") +
  xlab("Passenger Class") +
  ylab("Count") +
  labs(fill="Survived");


## ----errors1, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::kable( data_combined[ data_combined$Ticket == "W./C. 6608", ], format="markdown", longtable=TRUE)


## ----errors2, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Mr. William Neal Ford
data_combined[ data_combined$PassengerId ==  87,"SibSp"] = 3;  # From 1
data_combined[ data_combined$PassengerId ==  87,"Parch"] = 1;  # From 3
# Miss. Robina Maggie Ford
data_combined[ data_combined$PassengerId == 148,"SibSp"] = 3;  # From 2
data_combined[ data_combined$PassengerId == 148,"Parch"] = 1;  # From 2
# Miss. Doolina Margaret Ford
data_combined[ data_combined$PassengerId == 437,"SibSp"] = 3;  # From 2
data_combined[ data_combined$PassengerId == 437,"Parch"] = 1;  # From 2
# Mrs. Edward Ford
data_combined[ data_combined$PassengerId == 737,"SibSp"] = 1;  # From 1; Note: sister is Mrs. Eliza Johnston
data_combined[ data_combined$PassengerId == 737,"Parch"] = 4;  # From 3
# Mr. Edward Watson Ford
data_combined[ data_combined$PassengerId == 1059,"SibSp"] = 3;  # From 2
data_combined[ data_combined$PassengerId == 1059,"Parch"] = 1;  # From 2
knitr::kable( data_combined[ data_combined$Ticket == "W./C. 6608", ], format="markdown", longtable=TRUE)


## ----errors3, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::kable( data_combined[ data_combined$Ticket == "C.A. 2673", ], format="markdown", longtable=TRUE)


## ----errors4, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Master. Eugene Joseph Abbott
data_combined[ data_combined$PassengerId == 1284,"SibSp"] = 1;  # From 0
data_combined[ data_combined$PassengerId == 1284,"Parch"] = 1;  # From 2
# Mrs. Stanton (Rosa Hunt)
data_combined[ data_combined$PassengerId == 280,"SibSp"] = 0;  # From 1
data_combined[ data_combined$PassengerId == 280,"Parch"] = 2;  # From 1
knitr::kable( data_combined[ data_combined$Ticket == "C.A. 2673", ], format="markdown", longtable=TRUE)


## ----feature1--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$Survived = as.factor(train$Survived);
train$Pclass = as.ordered(train$Pclass);


## ----impute_embarked-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data_combined[62,"Embarked"] = as.factor("S");
data_combined[830,"Embarked"] = as.factor("S");
data_combined$Embarked = factor( data_combined$Embarked );  # removes empty factor ""


## ----impute_fare1, echo=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::kable( data_combined[1044,], format="markdown", longtable=TRUE)


## ----impute_fare2----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ticketChar = as.character( data_combined$Ticket );
numPassengers = nrow( data_combined );
data_combined$NumPassengersOnTicket = 1;
for (i in 1:numPassengers ){
    thisTicket = as.character( data_combined[i,"Ticket"] );
    idxPeople = which( thisTicket == ticketChar );
    numPeople = length( idxPeople );
    data_combined$NumPassengersOnTicket[i] = numPeople;
}
# now constrain to those that have just 1 person on the ticket, as passenger 1044
singlePassengers = data_combined$NumPassengersOnTicket == 1;
embarkedS = data_combined$Embarked == 'S';
unassignedCabins = data_combined$CabinAssignment == 'Unassigned';
pClass3 = data_combined$Pclass == 3;
similarPassengers = which( singlePassengers & embarkedS & unassignedCabins & pClass3 );
similarPassengersFareData = data_combined[similarPassengers,];  # include all columns but the one we have no Fare info
similarPassengersFareData = similarPassengersFareData[ similarPassengersFareData$PassengerId != 1044,];  # include all columns but the one we have no Fare info
medianFare = median( similarPassengersFareData$Fare );
data_combined[1044,"Fare"] = medianFare;
hist(similarPassengersFareData$Fare, xlab = "Fare", ylab = "Passengers", main = "Fare of Similar Passengers");
summary( similarPassengersFareData$Fare );


## ----impute_age1, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data_combined$FixedAge = data_combined$Age;
passenger_names = as.character(data_combined$Name);
num_commas = unname(sapply( passenger_names, str_count, ","));
all_commas = assert_that( all( num_commas == 1 ) );  # Make sure each row has a comma
# Now, extract titles.  Split on comma and take the tail end of the character string
surnames = sapply( strsplit(as.character(passenger_names), ","), head, 1);
data_combined$Surname = as.factor( surnames );
given_name_string = sapply( strsplit(as.character(passenger_names), ","), tail, 1);
given_name_string = trimws( given_name_string, "left"); # remove leading whitespace
data_combined$NameString = paste( given_name_string, surnames );
# Remove leading whitespace, if any
given_name_string = trimws( given_name_string, "left");
# To extract the title from the Name character string, split each string 
name_tokens = strsplit( given_name_string, "\\s+");
# Exract first token as the title
titles = lapply(name_tokens,"[[",1);
given_names = lapply(name_tokens,"[[",2);
data_combined$GivenName = as.factor( unlist( given_names ) );
title_regs = regexpr( "[\\w+]+\\.", given_name_string, perl=TRUE );
re_titles = regmatches( given_name_string, title_regs );
data_combined$Title = as.factor( unlist( re_titles ));
# The set of titles that have missing age values
ageNA = is.na( data_combined$Age );
titleNA = data_combined$Title[ageNA];
# The set of titles that have missing age values
data_combined$FixedTitle = data_combined$Title;

FIX_TITLES = TRUE;
if ( FIX_TITLES ){
  # change the following titles to "Mr." or "Mrs."
  titlesToChange = c("Capt.", "Col.", "Countess.", "Don.", "Dona.", "Dr.", "Jonkheer.", "Lady.", "Major.", "Mlle.", "Mme.", "Ms.", "Rev.", "Sir.");
  for ( title in titlesToChange ){
    idxChange = which( data_combined$FixedTitle == title );
    idxChangeMale = intersect( idxChange, data_combined$Sex == "male");
    idxChangeFemale = setdiff( idxChange, idxChangeMale );
    data_combined[idxChangeMale,"FixedTitle"] = "Mr.";
    data_combined[idxChangeFemale,"FixedTitle"] = "Mrs.";
  }
  data_combined$FixedTitle = factor( data_combined$FixedTitle );
}

data_combined$AgeTitle = data_combined$Title;
data_combined$AgeTitle[ which( data_combined$Title == "Ms." & ageNA )] = as.factor( "Mrs." );
age_lm = lm( Age~AgeTitle, data=data_combined);
missingAgeIndices = which( is.na( data_combined$Age ) );
missingTitles = data_combined[missingAgeIndices,"AgeTitle"];
missingCount = table( missingTitles );
summary( missingTitles )


## ----impute_age2-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# model is simple: use the passenger's title (i.e., Mr., Master., Mrs.) to determine age
# May want to better this by using age of parents (if travelling with parents), age of siblings, spouse, etc.
age_lm = lm( Age~AgeTitle, data=data_combined);

missingAges = predict( age_lm, data_combined[missingAgeIndices,]);
data_combined[missingAgeIndices,"FixedAge"] = missingAges;
coeffs = coefficients( age_lm );
coeffNames = names(coeffs);
coeffValues = unname(coeffs);
# find (Intercept)
idxIntercept = match( "(Intercept)", coeffNames );
interceptValue = coeffValues[idxIntercept];
allOtherCoeffIndices = setdiff(1:length(coeffNames),idxIntercept);
allOtherCoeffValues = coeffValues[allOtherCoeffIndices];
titleAges = interceptValue + allOtherCoeffValues;
names(titleAges) =  gsub( "AgeTitle", "", coeffNames[allOtherCoeffIndices] );
knitr::kable( bind_rows(titleAges), format="markdown", longtable=TRUE)


## ----feature2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
passenger_names = as.character(data_combined$Name);
head(passenger_names);


## ----feature3--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
num_commas = unname(sapply( passenger_names, str_count, ","));
all_commas = assert_that( all( num_commas == 1 ) );  # Make sure each row has a comma
# Now, extract titles.  Split on comma and take the tail end of the character string
surnames = sapply( strsplit(as.character(passenger_names), ","), head, 1);
data_combined$Surname = as.factor( surnames );
given_name_string = sapply( strsplit(as.character(passenger_names), ","), tail, 1);
# Remove leading whitespace, if any
given_name_string = trimws( given_name_string, "left");
# To extract the title from the Name character string, split each string 
name_tokens = strsplit( given_name_string, "\\s+");
# Exract first token as the title
titles = lapply(name_tokens,"[[",1);
given_names = lapply(name_tokens,"[[",2);
data_combined$GivenName = as.factor( unlist( given_names ) );


## ----feature4--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
passenger_titles = unlist( titles );
table( passenger_titles );


## ----feature5--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data_combined[ which( data_combined$Title == "Countess." ), ]


## ----feature6--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
title_regs = regexpr( "[\\w+]+\\.", given_name_string, perl=TRUE );
re_titles = regmatches( given_name_string, title_regs );
data_combined$Title = as.factor( unlist( re_titles ));
summary( data_combined$Title );


## ----feature7--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# The set of titles that have missing age values
ageNA = is.na( data_combined$Age );
titleNA = data_combined$Title[ageNA];
summary( titleNA );


## ----feature8--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# The set of titles that have missing age values
data_combined$AgeTitle = data_combined$Title;
data_combined$AgeTitle[ which( data_combined$Title == "Ms." & ageNA )] = as.factor( "Mrs." );
summary( data_combined$AgeTitle );


## ----feature9--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
numParents = integer( nrow( data_combined ) );
numChildren = integer( nrow( data_combined ) );

MAX_CHILD_AGE = 14;

# Find all Parch > 0
posParch = data_combined$Parch > 0;
idxParch = which( posParch );
counter = 0;
numCases = length( idxParch );
for (thisRow in idxParch){
  sibsp = data_combined[thisRow,"Sibsp"];
  # if sibsp > 1, then the passenger is travelling with siblings (and therefore, likely parents)
  counter = counter + 1;
  numParch = data_combined[thisRow,"Parch"];
  thisSurname = data_combined[thisRow,"Surname"];
  thisTicket = data_combined[thisRow,"Ticket"];
  thisTitle = as.character( data_combined[thisRow,"Title"] );
  thisAge = data_combined[thisRow,"Age"];
  if ( (thisTitle == "Master.") || (thisTitle == "Miss.") ){
    # if this passenger is a "Master." or Miss. and numParch <= 2, he/she must be someone's child
    numParents[thisRow] = numParch;
    #next;
  }
  if (!is.na( thisAge ) && thisAge <= MAX_CHILD_AGE ){
    # if this passenger is young, declare they cannot be parents, must be a child
    numParents[thisRow] = numParch;
    #next;
  }
  # get passenger rows on same ticket
  sameTickets = data_combined$Ticket == thisTicket;
  sameSurnames = data_combined$Surname == thisSurname;
  sameSurnameSameTicket = sameTickets & sameSurnames & posParch;
  ticketTitles = as.character( data_combined[ sameSurnameSameTicket, "Title"] );
  ticketParches = data_combined[ sameSurnameSameTicket, "Parch"];
  # now, look for passengers with the same surname on the ticket and check their titles and ages
  numSame = length( sameSurnameSameTicket );
  ages = sort( data_combined[sameSurnameSameTicket,"Age"] );
  thisAgePos = which( thisAge == ages )[1];  # find index of thisAge in sorted ages
  gaps = diff( ages );
  numGenerationalGaps = length( which( gaps > MAX_CHILD_AGE) );
  if ( numGenerationalGaps == 0 ){
    if ( !is.na( thisAge )){
      if ( ( thisAge >= 40 ) || (thisTitle == "Mrs." ) ){
        numChildren[thisRow] = numParch;
      }
      else if (numParch > 2 ){
        numChildren[thisRow] = numParch;
      }
      else{
        numParents[thisRow] = numParch;
      }
    }
    else{
      # no age information. If travelling with "kids", and title isn't a kid, then parent
      if ( ! ( thisTitle %in% c("Master.","Miss.") ) ){
        if( any( c("Master.","Miss.") %in% ticketTitles ) ){
          # now, if there are two Mr. in this group, we need to choose the real father
          # If there are three or more children, then Parch will be greater than 2 and
          # will indicate this is the father.  Else, it will be a child
          if( thisTitle == "Mr."){
            maxParches = max( ticketParches );
            if ( ( maxParches > 2 ) && ( maxParches == numParch ) ){
              numChildren[thisRow] = numParch;
            }
            else{
              numParents[thisRow] = numParch;
            }
          }
          else{
            numChildren[thisRow] = numParch;
          }
        }
        else{
          # all we have is a non Mr. or Miss. title.  Make them a child
            numChildren[thisRow] = numParch;
        }
      }
      else{
        numParents[thisRow] = numParch;
      }
    }
  }
  else{
    if ( !is.na(thisAge) ){  # use age in comparison to generation gap to classify kids/parents
      maxGapPos = which.max( gaps ) + 0.5;  # the 0.5 puts it in the middle of the kids/parents
      if ( thisAgePos < maxGapPos ){
        numParents[thisRow] = numParch;
      }
      else{
        numChildren[thisRow] = numParch;
      }
    }
    else{
      # no age info, have to go with titles
    }
  }
  totalParch = numParents[thisRow] + numChildren[thisRow];
  if ( totalParch != numParch ){
    stop( "Number of Parents/Children assigned (", totalParch, ") does not equal the Parch variable (", numParch, ") for passenger ", data_combined[thisRow,"PassengerId"], "\n");
  }
  data_combined$NumParents = numParents;
  data_combined$NumChildren = numChildren;
}


## ----feature10-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
numSiblings = integer( nrow( data_combined ) );
numSpouses = integer( nrow( data_combined ) );

MAX_CHILD_AGE = 14;

posSibSp = data_combined$SibSp > 0;
idxSibSp = which( posSibSp );
counter = 0;
numCases = length( idxSibSp );
for (thisRow in idxSibSp){
  counter = counter + 1;
  numSibSp = data_combined[thisRow,"SibSp"];
  numParch = data_combined[thisRow,"Parch"];
  thisSurname = data_combined[thisRow,"Surname"];
  thisTicket = data_combined[thisRow,"Ticket"];
  thisTitle = as.character( data_combined[thisRow,"Title"] );
  thisAge = data_combined[thisRow,"Age"];
  thisSex = as.character( data_combined[thisRow,"Sex"] );
  thisGivenName = as.character( data_combined[thisRow,"GivenName"] );
  thisPassengerId = data_combined[thisRow,"PassengerId"];
  if ( (thisTitle == "Master.") || (thisTitle == "Miss.") ){
    # if this passenger is a "Master." or Miss., this passenger is not married, so must be spouse
    numSiblings[thisRow] = numSibSp;
    next;
  }
  if (!is.na( thisAge ) && thisAge <= 10 ){
    # if this passenger is young, declare they cannot be parents, must be a child
    numSiblings[thisRow] = numSibSp;
    next;
  }
  # get passenger rows on same ticket
  sameTickets = data_combined$Ticket == thisTicket;
  sameSurnames = data_combined$Surname == thisSurname;
  sameSurnameSameTicket = sameTickets & sameSurnames & posSibSp;
  # if married, look for spouse on the same ticket
  # get given names on same ticket
  same_ticket_rows = data_combined[which(sameSurnameSameTicket),];
  given_names = as.character( same_ticket_rows$GivenName );
  titles = as.character( same_ticket_rows$Title );
  sexes = as.character( same_ticket_rows$Sex );
  passengerIds = same_ticket_rows$PassengerId;
  master_mask = ( titles != "Master." );  
  sex_mask = ( sexes != thisSex );  # opposite sex marriage
  miss_mask = ( titles != "Miss." );
  this_mask = ( passengerIds != thisPassengerId );
  idxMatch = which( ( thisGivenName == given_names ) & master_mask & sex_mask & miss_mask & this_mask);
  if ( length( idxMatch ) == 1 ){
    numSpouses[thisRow] = 1;
    next;
  }
  ticketTitles = as.character( data_combined[ sameSurnameSameTicket, "Title"] );
  ticketParches = data_combined[ sameSurnameSameTicket, "Parch"];
  # now, look for passengers with the same surname on the ticket and check their titles and ages
  numSame = length( sameSurnameSameTicket );
  ages = sort( data_combined[sameSurnameSameTicket,"Age"] );
  thisAgePos = which( thisAge == ages )[1];  # find index of thisAge in sorted ages
  gaps = diff( ages );
  numGenerationalGaps = length( which( gaps > MAX_CHILD_AGE) );
}

# do some manual corrections
numSiblings[168] = 0;  # Mrs. William Skoog
numSpouses[168] = 1;  # Mrs. William Skoog
numSiblings[361] = 0;  # Mr. Wilhelm Skoog
numSpouses[361] = 1;  # Mr. Wilhelm Skoog
numSiblings[679] = 0;  # Mrs. Frederick Goodwin
numSpouses[679] = 1;  # Mrs. Frederick Goodwin
numSiblings[1031] = 0;  # Mr. Charles Frederick Goodwin
numSpouses[1031] = 1;  # Mr. Charles Frederick Goodwin
numSiblings[557] = 0;  # Lady. Duff Gordon
numSpouses[557] = 1;  # Lady. Duff Gordon
numSiblings[600] = 0;  # Sir. Duff Gordon
numSpouses[600] = 1;  # Sir. Duff Gordon
numSiblings[746] = 0;  # Capt. Edward Gifford Crosby
numSpouses[746] = 1;  # Capt. Edward Gifford Crosby
numSiblings[1197] = 0;  # Mrs. Edward Gifford Crosby
numSpouses[1197] = 1;  # Mrs. Edward Gifford Crosby
numSiblings[1059] = 3;  # Mr. Edward Watson Ford
numSpouses[1059] = 0;  # Mr. Edward Watson Ford

counter = 0;
for (thisRow in idxSibSp){
  counter = counter + 1;
  numSibSp = data_combined[thisRow,"SibSp"];
  numParch = data_combined[thisRow,"Parch"];
  thisSurname = data_combined[thisRow,"Surname"];
  thisTicket = data_combined[thisRow,"Ticket"];
  thisTitle = as.character( data_combined[thisRow,"Title"] );
  thisAge = data_combined[thisRow,"Age"];
  thisSex = as.character( data_combined[thisRow,"Sex"] );
  thisGivenName = as.character( data_combined[thisRow,"GivenName"] );
  thisPassengerId = data_combined[thisRow,"PassengerId"];
  numSiblings[thisRow] = numSibSp - numSpouses[thisRow];
}

data_combined$Spouses = numSpouses;
data_combined$SpousesFactor = factor( numSpouses );
data_combined$Siblings = numSiblings;


## ----feature11-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ticketChar = as.character( data_combined$Ticket );
uniqueTickets = unique( ticketChar );
farePerPassenger = data_combined$Fare;
for (i in 1:length(uniqueTickets) ){
    thisTicket = uniqueTickets[i];
    idxPeople = which( ticketChar == thisTicket );
    theseFares = data_combined[idxPeople,"Fare"];
    #if ( !all( theseFares == theseFares[1] ) ){
    #    cat( thisTicket )
    #}
    # should only be a single fare
    
    numPeople = length( idxPeople );
    farePerPassenger[idxPeople] = theseFares[1]/numPeople;
}
data_combined$FarePerPassenger = farePerPassenger;


## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align="center")


## ----include = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MAX_HOURS = 10;
library( assertthat );
data_combined$pclass <- as.factor(data_combined$Pclass);

# A bit about R data types (e.g., factors)
summary(data_combined );



## ----null1, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
numPassengers = length(train$Survived);
died = sum( train$Survived == 0);
survived = sum( train$Survived == 1);
#assert_that( died + survived == numPassengers);
mean_survived = survived/numPassengers;
cat( "Of ", numPassengers, " passengers, ", died, " died and ", survived, " survived.\nMean Survivability = ", survived/numPassengers );


## ----null2, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Time strip
nullModelTrain = data.frame(PassengerId=train$PassengerId, Survived=0);
nullModel = data.frame(PassengerId=test$PassengerId, Survived=0);
write.csv( nullModelTrain, "NullModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( nullModel, "NullModel.csv", row.names=FALSE, quote=FALSE );
nullModelTrainProbs = data.frame(PassengerId=train$PassengerId, Survived=342/891);
write.csv( nullModelTrainProbs, "NullModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
nullModelTestProbs = data.frame(PassengerId=test$PassengerId, Survived=342/891);
write.csv( nullModelTestProbs, "NullModelProbs.csv", row.names=FALSE, quote=FALSE );

#hours = 1;
#par(mfrow=c(2,1));
#barplot( hours, width=0.9, main="Time", xlab="hours", ylim=c(0,1), xlim=c(0,MAX_HOURS), horiz=TRUE );
#bp = ggplot(data=hours,aes(x=hours,y=1)) + geom_bar(stat="identity") + coord_flip();
#bp
### Bias-Variance Strip



## ----genderSubmissionModel, echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
femaleIndices = which( train$Sex == 'female' );
maleIndices = which( train$Sex == 'male' );
numFemales = length( femaleIndices );
numMales = length( maleIndices );
proportionFemaleSurvival = length( intersect( femaleIndices, which( train$Survived == 1)))/numFemales;
proportionMaleSurvival = length( intersect( maleIndices, which( train$Survived == 1)))/numMales;

genderModelTrainProbs = data.frame(PassengerId=train$PassengerId, Survived=0);
genderModelTrainProbs$Survived[ femaleIndices ] = proportionFemaleSurvival;
genderModelTrainProbs$Survived[ maleIndices ] = proportionMaleSurvival;
write.csv( genderModelTrainProbs, "GenderModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
genderModelTestProbs = data.frame(PassengerId=test$PassengerId, Survived=0);
write.csv( genderModelTestProbs, "GenderModelProbs.csv", row.names=FALSE, quote=FALSE );


## ----womenAndChildrenFirstTrain, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(caret, quietly=TRUE);
femaleIndices = which( train$Sex == 'female' );
data_combined[["SurvivedFactor"]] = factor(data_combined[["Survived"]]);
whiten = preProcess(data_combined, c("center","scale","BoxCox") );
whitened_data = data.frame( predict( whiten, data_combined ));
# now "unwhiten" some data like Survived and PassengerId
whitened_data$PassengerId = data_combined$PassengerId;
whitened_data$Survived = data_combined$Survived;
newTrain = subset( data_combined, PassengerId <= nrow(train) );
newTest = subset( data_combined, PassengerId > nrow(train) );
whiteTrain = subset( whitened_data, PassengerId <= nrow(train) );
whiteTest = subset( whitened_data, PassengerId > nrow(train) );
childIndices = which( newTrain$FixedAge < 15 );
womenOrChildren = union( femaleIndices, childIndices );
maleIndices = setdiff( 1:nrow(train), womenOrChildren );
womenOrChildrenSurvived = which( train[ womenOrChildren, "Survived"] == 1 );
menDied = which( train[ maleIndices, "Survived"] == 0);
numFemales = length( femaleIndices );
numMales = length( maleIndices );
trainProportionCorrect = ( length(womenOrChildrenSurvived) + length(menDied))/nrow(train);


## ----womenAndChildrenFirstTest, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# start with all perished model, then set women and children to survived
womenAndChildrenFirstModelTrain = data.frame(PassengerId=train$PassengerId, Survived=0);
womenAndChildrenFirstModel = data.frame(PassengerId=test$PassengerId, Survived=0);
newTest = subset( data_combined, PassengerId > nrow(train) );
femaleIndices = which( newTest$Sex == 'female' );
trainFemaleIndices = which( newTrain$Sex == 'female' );
childIndices = which( newTest$FixedAge < 15 );
trainChildIndices = which( newTrain$FixedAge < 15 );
womenOrChildren = union( femaleIndices, childIndices );
womenOrChildrenTrain = union( trainFemaleIndices, trainChildIndices );
womenAndChildrenFirstModelTrain[womenOrChildrenTrain,"Survived"] = 1;
womenAndChildrenFirstModel[womenOrChildren,"Survived"] = 1;
write.csv( womenAndChildrenFirstModelTrain, "WomenAndChildrenFirstModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( womenAndChildrenFirstModel, "WomenAndChildrenFirstModel.csv", row.names=FALSE, quote=FALSE );

probWCSurvived = mean(newTrain$Survived[ womenOrChildrenTrain ]);
probMenSurvived = mean(newTrain$Survived[ -womenOrChildrenTrain ]);
wcModelTrainProbs = data.frame(PassengerId=train$PassengerId, Survived=newTrain$Survived);
wcModelTrainProbs$Survived[womenOrChildrenTrain] = probWCSurvived;
wcModelTrainProbs$Survived[-womenOrChildrenTrain] = probMenSurvived;
write.csv( wcModelTrainProbs, "WCModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
wcModelTestProbs = data.frame(PassengerId=test$PassengerId, Survived=0);
wcModelTestProbs$Survived[womenOrChildren] = probWCSurvived;
wcModelTestProbs$Survived[-womenOrChildren] = probMenSurvived;
write.csv( wcModelTestProbs, "WCModelProbs.csv", row.names=FALSE, quote=FALSE );


## ----lda1, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(MASS, quietly=TRUE);
xTrain = whiteTrain[,c("SurvivedFactor","pclass","Sex","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","Embarked","NumPassengersOnTicket","NumParents","NumChildren","SpousesFactor","Siblings")];
xTest = whiteTest[,c("SurvivedFactor","pclass","Sex","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","Embarked","NumPassengersOnTicket","NumParents","NumChildren","SpousesFactor","Siblings")];
lda.fit = lda( SurvivedFactor ~., data=xTrain);
lda.fit
plot( lda.fit );
lda.train.pred = predict( lda.fit, xTrain );  
ldaTrainPredictions = as.numeric( lda.train.pred$class ) - 1;
truth = newTrain$Survived;
confusionMatrix( ldaTrainPredictions, truth );
# scored a 0.8373 against the training data
lda.test.pred = predict( lda.fit, xTest );  
# lda.pred$class is a factor; need to convert to 0, 1
ldaPredictions = as.numeric( lda.test.pred$class ) - 1;
ldaModelTrain = data.frame(PassengerId=train$PassengerId, Survived=ldaTrainPredictions);
ldaModel = data.frame(PassengerId=test$PassengerId, Survived=ldaPredictions);

write.csv( ldaModelTrain, "LDAModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( ldaModelTrain, "LDAModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( ldaModel, "LDAModel.csv", row.names=FALSE, quote=FALSE );
write.csv( ldaModel, "LDAModelProbs.csv", row.names=FALSE, quote=FALSE );
# scored a 0.78947


## ----lda2, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(MASS);
xTrain = whiteTrain[,c("SurvivedFactor","pclass","Sex","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","NumChildren","SpousesFactor","Siblings")];
xTest = whiteTest[,c("SurvivedFactor","pclass","Sex","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","NumChildren","SpousesFactor","Siblings")];
lda.fit = lda( SurvivedFactor ~., data=xTrain);
plot( lda.fit );
lda.train.pred = predict( lda.fit, xTrain );  
ldaTrainPredictions = as.numeric( lda.train.pred$class ) - 1;
truth = newTrain$Survived;
confusionMatrix( ldaTrainPredictions, truth );
# scored a 0.8339 against the training data
lda.test.pred = predict( lda.fit, xTest );  
# lda.pred$class is a factor; need to convert to 0, 1
ldaPredictions = as.numeric( lda.test.pred$class ) - 1;
ldaModelTrain = data.frame(PassengerId=train$PassengerId, Survived=ldaTrainPredictions);
ldaModel = data.frame(PassengerId=test$PassengerId, Survived=ldaPredictions);

write.csv( ldaModelTrain, "LDAReducedModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( ldaModelTrain, "LDAReducedModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( ldaModel, "LDAReducedModel.csv", row.names=FALSE, quote=FALSE );
write.csv( ldaModel, "LDAReducedModelProbs.csv", row.names=FALSE, quote=FALSE );
# scored a 0.78947


## ----qda1, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(MASS);
xTrain = whiteTrain[,c("SurvivedFactor","pclass","Sex","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","Embarked","NumPassengersOnTicket","NumParents","NumChildren","SpousesFactor","Siblings")];
xTest = whiteTest[,c("SurvivedFactor","pclass","Sex","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","Embarked","NumPassengersOnTicket","NumParents","NumChildren","SpousesFactor","Siblings")];
qda.fit = qda( SurvivedFactor ~., data=xTrain);
qda.train.pred = predict( qda.fit, xTrain );  
qdaTrainPredictions = as.numeric( qda.train.pred$class ) - 1;
truth = newTrain$Survived;
confusionMatrix( qdaTrainPredictions, truth );
# scored a 0.8159 against the training data
qda.test.pred = predict( qda.fit, xTest );  
# lda.pred$class is a factor; need to convert to 0, 1
qdaPredictions = as.numeric( qda.test.pred$class ) - 1;
qdaModelTrain = data.frame(PassengerId=train$PassengerId, Survived=qdaTrainPredictions);
qdaModel = data.frame(PassengerId=test$PassengerId, Survived=qdaPredictions);

write.csv( qdaModelTrain, "QDAModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( qdaModelTrain, "QDAModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( qdaModel, "QDAModel.csv", row.names=FALSE, quote=FALSE );
write.csv( qdaModel, "QDAModelProbs.csv", row.names=FALSE, quote=FALSE );
# scored a 0.77033


## ----qda2, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(MASS);
xTrain = whiteTrain[,c("SurvivedFactor","pclass","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","NumChildren","Siblings")];
xTest = whiteTest[,c("SurvivedFactor","pclass","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","NumChildren","Siblings")];
qda.fit = qda( SurvivedFactor ~., data=xTrain);
qda.train.pred = predict( qda.fit, xTrain );  
qdaTrainPredictions = as.numeric( qda.train.pred$class ) - 1;
truth = newTrain$Survived;
confusionMatrix( qdaTrainPredictions, truth );
# scored a 0.7845 against the training data
qda.test.pred = predict( qda.fit, xTest );  
# lda.pred$class is a factor; need to convert to 0, 1
qdaPredictions = as.numeric( qda.test.pred$class ) - 1;
qdaModelTrain = data.frame(PassengerId=train$PassengerId, Survived=qdaTrainPredictions);
qdaModel = data.frame(PassengerId=test$PassengerId, Survived=qdaPredictions);

write.csv( qdaModelTrain, "QDASigModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( qdaModelTrain, "QDASigModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( qdaModel, "QDASigModel.csv", row.names=FALSE, quote=FALSE );
write.csv( qdaModel, "QDASigModelProbs.csv", row.names=FALSE, quote=FALSE );
# scored a 0.78947


## ----logisticRegression1, echo=TRUE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all_features = c("pclass", "Sex", "FixedAge", "FarePerPassenger", "FixedTitle", "Siblings", "CabinAssignment", "Embarked", "NumPassengersOnTicket", "NumParents", "NumChildren", "SpousesFactor");
full_df = newTrain[, c("Survived", all_features)];
full.glm.fit = glm( Survived~., data=full_df, family=binomial);
summary( full.glm.fit );
trainSetProbs = predict( full.glm.fit, newTrain, type="response");  # now a vector of probabilities
# probabilities >= 0.5 mean survived, < 0.5 mean perished
# training set performance
threshold = 0.5;
predictions = as.numeric( trainSetProbs >= threshold );
confusionMatrix( predictions, newTrain$Survived );

# now, how'd we do against the test set?
testSetProbs = predict( full.glm.fit, newTest, type="response");
fullLogisticPredictions = as.numeric( testSetProbs >= threshold );
fullLogisticRegressionModelTrain = data.frame(PassengerId=train$PassengerId, Survived=predictions);
fullLogisticRegressionModel = data.frame(PassengerId=test$PassengerId, Survived=fullLogisticPredictions);

fullLogisticRegressionModelTrainProbs = data.frame(PassengerId=train$PassengerId, Survived=trainSetProbs);
fullLogisticRegressionModelProbs = data.frame(PassengerId=test$PassengerId, Survived=testSetProbs);

write.csv( fullLogisticRegressionModelTrainProbs, "LogisticRegressionFullModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( fullLogisticRegressionModelProbs, "LogisticRegressionFullModelProbs.csv", row.names=FALSE, quote=FALSE );

write.csv( fullLogisticRegressionModelTrain, "LogisticRegressionFullModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( fullLogisticRegressionModel, "LogisticRegressionFullModel.csv", row.names=FALSE, quote=FALSE );
# scored a 0.78947 - not expected


## ----logisticRegression2, echo=TRUE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(caret);
library(lattice);
cat( "Training Set performance:\n");
confusionMatrix( predictions, newTrain$Survived);


## ----logisticRegression3, echo=TRUE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
leftoverFeatures = c("FarePerPassenger","Embarked","NumPassengersOnTicket","SpousesFactor","NumParents");
leftover_df = newTrain[, c("Survived", leftoverFeatures)];
leftover.glm.fit = glm( Survived ~., data = leftover_df, family=binomial);
summary(leftover.glm.fit);


## ----logisticRegression4, echo=TRUE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(boot, quietly = TRUE);
significant_features = c("FixedTitle", "Sex", "Siblings", "FixedAge", "CabinAssignment", "pclass", "NumChildren", "FarePerPassenger", "Embarked");
all_combinations_of_significant_features = lapply(1:length(significant_features), function(x) combn(length(significant_features),x));
numFeatureSetClasses = length( all_combinations_of_significant_features );
correct = list();
accuracies = list();
errors = list();
features = list();
numFeatures = list();
counter = 1;
for ( featureClass in 1:numFeatureSetClasses){
  featClassColumns = dim( all_combinations_of_significant_features[[featureClass]] )[2];
  for ( thisCol in 1:featClassColumns ){
    feature_set = significant_features[ all_combinations_of_significant_features[[featureClass]][,thisCol]];
    nFeatures = length( feature_set );
    df = newTrain[, c("Survived", feature_set)]
    glm.fit = glm( Survived~.,data=df,family=binomial);
    cv.glm.fit = cv.glm(df,glm.fit,K=10);
    cv.error = cv.glm.fit$delta[1];
    trainSetProbs = predict( glm.fit, newTrain, type="response");  # now a vector of probabilities
    predictions = as.numeric( trainSetProbs >= threshold );
    numRight = sum( predictions == newTrain$Survived );
    accuracy = mean( predictions == newTrain$Survived );
    features[counter] = list( feature_set );
    numFeatures[counter] = nFeatures;
    accuracies[counter] = accuracy;
    correct[counter] = numRight;
    errors[counter] = cv.error;
    counter = counter + 1;
  }
}
cvResults = data.frame( NumFeatures=unlist(numFeatures), CVError=unlist(errors), TrainingScore=unlist(accuracies), NumCorrect=unlist(correct), Features=I(features) );
cvResults = cvResults[order(cvResults$CVError),];
knitr::kable( head(cvResults,n=20L), format="markdown", longtable=TRUE)


## ----logisticRegression5, echo=TRUE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
correct = list();
accuracies = list();
errors = list();
features = list();
numFeatures = list();
counter = 1;
for ( rowNum in 502:511){
    feature_set = cvResults$Features[[rowNum]];
    nFeatures = cvResults$NumFeatures[rowNum];
    df = newTrain[, c("Survived", feature_set)]
    glm.fit = glm( Survived~.,data=df,family=binomial);
    cv.glm.fit = cv.glm(df,glm.fit);
    cv.error = cv.glm.fit$delta[1];
    trainSetProbs = predict( glm.fit, newTrain, type="response");  # now a vector of probabilities
    predictions = as.numeric( trainSetProbs >= threshold );
    numRight = sum( predictions == newTrain$Survived );
    accuracy = mean( predictions == newTrain$Survived );
    features[counter] = list( feature_set );
    numFeatures[counter] = nFeatures;
    accuracies[counter] = accuracy;
    correct[counter] = numRight;
    errors[counter] = cv.error;
    counter = counter + 1;
}
loocvResults = data.frame( NumFeatures=unlist(numFeatures), CVError=unlist(errors), TrainingScore=unlist(accuracies), NumCorrect=unlist(correct), Features=I(features) );
loocvResults = loocvResults[order(loocvResults$CVError),];
knitr::kable( head(loocvResults,n=10L), format="markdown", longtable=TRUE)


## ----logisticRegression6, echo=TRUE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_features = c("pclass", "Sex", "FixedAge", "FixedTitle", "Siblings", "CabinAssignment", "Embarked", "NumChildren" );
best_df = newTrain[, c("Survived", best_features)];
best.glm.fit = glm( Survived~., data=best_df, family=binomial);
trainSetProbs = predict( best.glm.fit, newTrain, type="response");  # now a vector of probabilities
# probabilities >= 0.5 mean survived, < 0.5 mean perished
threshold = 0.5;
predictions = as.numeric( trainSetProbs >= threshold );
summary( best.glm.fit );

# How'd we do?
# now calculate the training set error
correctRate = mean( predictions == newTrain$Survived );
# scored a 0.8395062

# now, how'd we do against the test set?
testSetProbs = predict( best.glm.fit, newTest, type="response");
bestTestPredictions = as.numeric( testSetProbs >= threshold );
bestLogisticRegressionModelTrain = data.frame(PassengerId=train$PassengerId, Survived=predictions);
bestLogisticRegressionModel = data.frame(PassengerId=test$PassengerId, Survived=bestTestPredictions);

bestLogisticRegressionModelTrainProbs = data.frame(PassengerId=train$PassengerId, Survived=trainSetProbs);
bestLogisticRegressionModelProbs = data.frame(PassengerId=test$PassengerId, Survived=testSetProbs);

write.csv( bestLogisticRegressionModelTrainProbs, "LogisticRegressionSigModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( bestLogisticRegressionModelProbs, "LogisticRegressionSigModelProbs.csv", row.names=FALSE, quote=FALSE );

write.csv( bestLogisticRegressionModelTrain, "LogisticRegressionSigModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( bestLogisticRegressionModel, "LogisticRegressionSigModel.csv", row.names=FALSE, quote=FALSE );
# scored a 0.77990 - not as good as the full model


## ----ridgeRegression1, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library( glmnet, quietly = TRUE );
lambdas = c();
set.seed(1);
# use ridge regression
xTrain = model.matrix( Survived~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumPassengersOnTicket+NumParents+NumChildren+SpousesFactor+Siblings,data=newTrain,family=binomial);
xTest = model.matrix( ~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumPassengersOnTicket+NumParents+NumChildren+SpousesFactor+Siblings,data=newTest,family=binomial);
#xTrain = model.matrix( Survived~.,data=full_df,family=binomial);
full_df_test = newTest[, all_features];
#xTest = model.matrix( ~.,data=full_df_test,family=binomial);
cv.out = cv.glmnet( xTrain, newTrain$Survived, alpha=0, type.measure="class");
plot( cv.out );
lambdaMin = cv.out$lambda.min;
rrmodel = glmnet( xTrain, newTrain$Survived, alpha=0);
train.ridge.pred = predict( rrmodel, s=lambdaMin, newx=xTrain);
trainRidgePredictions = as.numeric( train.ridge.pred >= threshold );
numCorrect = sum(trainRidgePredictions == newTrain$Survived);
accuracy = numCorrect/nrow(newTrain);
ridge.pred = predict( rrmodel, s=lambdaMin, newx=xTest);
fullRidgePredictions = as.numeric( ridge.pred >= threshold );
fullRidgeRegressionModelTrain = data.frame(PassengerId=train$PassengerId, Survived=trainRidgePredictions);
fullRidgeRegressionModel = data.frame(PassengerId=test$PassengerId, Survived=fullRidgePredictions);
fullRidgeRegressionModelTrainProbs = data.frame(PassengerId=train$PassengerId, Survived=train.ridge.pred);
fullRidgeRegressionModelProbs = data.frame(PassengerId=test$PassengerId, Survived=ridge.pred);
write.csv( fullRidgeRegressionModelTrainProbs, "RidgeRegressionFullModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( fullRidgeRegressionModelProbs, "RidgeRegressionFullModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( fullRidgeRegressionModelTrain, "RidgeRegressionFullModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( fullRidgeRegressionModel, "RidgeRegressionFullModel.csv", row.names=FALSE, quote=FALSE );
# score is 0.78947


## ----ridgeRegression1b, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
confusionMatrix(trainRidgePredictions, newTrain$Survived);


## ----ridgeRegression2, echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fullLogisticPredictions = bestTestPredictions;
confusionMatrix( fullRidgePredictions, fullLogisticPredictions );


## ----ridgeRegression3, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library( glmnet );
lambdas = c();
set.seed(1);
# use ridge regression
xTrain = model.matrix( Survived~.,data=best_df,family=binomial);
best_df_test = newTest[, best_features];
xTest = model.matrix( ~.,data=best_df_test,family=binomial);
cv.out = cv.glmnet( xTrain, newTrain$Survived, alpha=0, type.measure="class");
plot( cv.out );
lambdaMin = cv.out$lambda.min;
rrmodel = glmnet( xTrain, newTrain$Survived, alpha=0);
train.ridge.pred = predict( rrmodel, s=lambdaMin, newx=xTrain);
trainRidgePredictions = as.numeric( train.ridge.pred >= threshold );
numCorrect = sum(trainRidgePredictions == newTrain$Survived);
accuracy = numCorrect/nrow(newTrain);
ridge.pred = predict( rrmodel, s=lambdaMin, newx=xTest);
bestRidgePredictions = as.numeric( ridge.pred >= threshold );
bestRidgeRegressionModelTrain = data.frame(PassengerId=train$PassengerId, Survived=trainRidgePredictions);
bestRidgeRegressionModel = data.frame(PassengerId=test$PassengerId, Survived=bestRidgePredictions);

bestRidgeRegressionModelTrainProbs = data.frame(PassengerId=train$PassengerId, Survived=as.numeric(train.ridge.pred));
bestRidgeRegressionModelProbs = data.frame(PassengerId=test$PassengerId, Survived=as.numeric(ridge.pred));
write.csv( bestRidgeRegressionModelTrainProbs, "RidgeRegressionSigModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( bestRidgeRegressionModelProbs, "RidgeRegressionSigModelProbs.csv", row.names=FALSE, quote=FALSE );

write.csv( bestRidgeRegressionModelTrain, "RidgeRegressionSigModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( bestRidgeRegressionModel, "RidgeRegressionSigModel.csv", row.names=FALSE, quote=FALSE );


## ----ridgeRegression4, echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fullLogisticPredictions = bestTestPredictions;
confusionMatrix( bestRidgePredictions, fullLogisticPredictions );


## ----lassoRegression, echo=TRUE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# on to the lasso
xTrain = model.matrix( Survived~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumPassengersOnTicket+NumParents+NumChildren+SpousesFactor+Siblings,data=newTrain,family=binomial);
xTest = model.matrix( ~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumPassengersOnTicket+NumParents+NumChildren+SpousesFactor+Siblings,data=newTest,family=binomial);
cv.out = cv.glmnet( xTrain, newTrain$Survived, alpha=1, type.measure = "class");
plot( cv.out );
lambdaMin = cv.out$lambda.min;
lassomodel = glmnet( xTrain, newTrain$Survived, alpha=1);
train.lasso.pred = predict( lassomodel, s=lambdaMin, newx=xTrain);
trainLassoPredictions = as.numeric( train.lasso.pred >= threshold );
numCorrect = sum(trainLassoPredictions == newTrain$Survived);
accuracy = numCorrect/nrow(newTrain);
lasso.pred = predict( lassomodel, s=lambdaMin, newx=xTest);
lassoPredictions = as.numeric( lasso.pred >= threshold );
fullLassoRegressionModelTrain = data.frame(PassengerId=train$PassengerId, Survived=trainLassoPredictions);
fullLassoRegressionModel = data.frame(PassengerId=test$PassengerId, Survived=lassoPredictions);
fullLassoRegressionModelTrainProbs = data.frame(PassengerId=train$PassengerId, Survived=as.numeric(train.lasso.pred));
fullLassoRegressionModelProbs = data.frame(PassengerId=test$PassengerId, Survived=as.numeric(lasso.pred));
write.csv( fullLassoRegressionModelTrainProbs, "LassoRegressionFullModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( fullLassoRegressionModelProbs, "LassoRegressionFullModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( fullLassoRegressionModelTrain, "LassoRegressionFullModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( fullLassoRegressionModel, "LassoRegressionFullModel.csv", row.names=FALSE, quote=FALSE );


## ----lassoRegression2, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
confusionMatrix( trainLassoPredictions, newTrain$Survived );


## ----lassoRegression3, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
confusionMatrix( lassoPredictions, fullLogisticPredictions );


## ----lassoRegression4, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# on to the lasso
xTrain = model.matrix( Survived~.,data=best_df,family=binomial);
best_df_test = newTest[, best_features];
xTest = model.matrix( ~.,data=best_df_test,family=binomial);
cv.out = cv.glmnet( xTrain, newTrain$Survived, alpha=1, type.measure = "class");
plot( cv.out );
lambdaMin = cv.out$lambda.min;
lassomodel = glmnet( xTrain, newTrain$Survived, alpha=1);
train.lasso.pred = predict( lassomodel, s=lambdaMin, newx=xTrain);
trainLassoPredictions = as.numeric( train.lasso.pred >= threshold );
numCorrect = sum(trainLassoPredictions == newTrain$Survived);
accuracy = numCorrect/nrow(newTrain);
lasso.pred = predict( lassomodel, s=lambdaMin, newx=xTest);
lassoPredictions = as.numeric( lasso.pred >= threshold );
bestLassoRegressionModelTrain = data.frame(PassengerId=train$PassengerId, Survived=trainLassoPredictions);
bestLassoRegressionModel = data.frame(PassengerId=test$PassengerId, Survived=lassoPredictions);

bestLassoRegressionModelTrainProbs = data.frame(PassengerId=train$PassengerId, Survived=as.numeric(train.lasso.pred));
bestLassoRegressionModelProbs = data.frame(PassengerId=test$PassengerId, Survived=as.numeric(lasso.pred));

write.csv( bestLassoRegressionModelTrainProbs, "LassoRegressionSigModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( bestLassoRegressionModelProbs, "LassoRegressionSigModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( bestLassoRegressionModelTrain, "LassoRegressionSigModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( bestLassoRegressionModel, "LassoRegressionSigModel.csv", row.names=FALSE, quote=FALSE );


## ----lassoRegression5, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
confusionMatrix( trainLassoPredictions, newTrain$Survived );


## ----lassoRegression6, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
confusionMatrix( lassoPredictions, fullLogisticPredictions );


## ----lassoRegression7, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
confusionMatrix( lassoPredictions, fullLogisticPredictions );
confusionMatrix( bestRidgePredictions, lassoPredictions );
# score is 0.78947


## ----knn1, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(caret);
trainCtrl = trainControl( method="LOOCV");
knn_fit = train( SurvivedFactor~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumPassengersOnTicket+NumParents+NumChildren+SpousesFactor+Siblings,data=newTrain,method="knn", trControl=trainCtrl, preProcess=c("center","scale"), tuneLength=20 );
knn_fit
plot( knn_fit );
test_pred = predict( knn_fit, newdata=newTrain);
confusionMatrix( test_pred, newTrain$SurvivedFactor)


## ----knn2, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now the whole training set
knnTrain = newTrain;
knnTest = newTest;
trainCtrl = trainControl( method="LOOCV" );
knn_fit = train( SurvivedFactor~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumPassengersOnTicket+NumParents+NumChildren+SpousesFactor+Siblings,data=knnTrain,method="knn", trControl=trainCtrl, preProcess=c("center","scale"), tuneLength=20 );
knn_train_pred = predict( knn_fit, newdata=knnTrain);
knn_pred = predict( knn_fit, newdata=knnTest);
fullKNNModelTrain = data.frame(PassengerId=knnTrain$PassengerId, Survived=as.numeric(knn_train_pred)-1);
fullKNNModel = data.frame(PassengerId=knnTest$PassengerId, Survived=as.numeric(knn_pred)-1);
write.csv( fullKNNModelTrain, "KNNFullModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( fullKNNModel, "KNNFullModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( fullKNNModelTrain, "KNNFullModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( fullKNNModel, "KNNFullModel.csv", row.names=FALSE, quote=FALSE );


## ----knn3, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
significant_features = c("FixedTitle", "Sex", "Siblings", "FixedAge", "CabinAssignment", "pclass", "NumChildren", "FarePerPassenger", "Embarked");
all_combinations_of_significant_features = lapply(1:length(significant_features), function(x) combn(length(significant_features),x));
numFeatureSetClasses = length( all_combinations_of_significant_features );
correct = list();
accuracies = list();
errors = list();
features = list();
numFeatures = list();
k = list();
counter = 1;
numTrain = nrow( newTrain );
trainCtrl = trainControl( method="repeatedcv", number = 8, repeats = 1 );
for ( featureClass in 1:numFeatureSetClasses){
  featClassColumns = dim( all_combinations_of_significant_features[[featureClass]] )[2];
  for ( thisCol in 1:featClassColumns ){
    feature_set = significant_features[ all_combinations_of_significant_features[[featureClass]][,thisCol]];
    nFeatures = length( feature_set );
    df = newTrain[, c("SurvivedFactor", feature_set)]
    knn_fit = train( SurvivedFactor~.,data=df,method="knn", trControl=trainCtrl, preProcess=c("center","scale"), tuneLength=20 );
    knn_pred = predict( knn_fit, newdata=newTrain);
    numRight = sum( knn_pred == newTrain$SurvivedFactor );
    cv.error = ( numTrain - numRight )/numTrain;
    accuracy = mean( knn_pred == newTrain$SurvivedFactor );
    features[counter] = list( feature_set );
    numFeatures[counter] = nFeatures;
    accuracies[counter] = accuracy;
    correct[counter] = numRight;
    errors[counter] = cv.error;
    k[counter] = knn_fit$bestTune$k;
    counter = counter + 1;
  }
}
cvResults = data.frame( NumFeatures=unlist(numFeatures), CVError=unlist(errors), TrainingScore=unlist(accuracies), NumCorrect=unlist(correct), K=unlist(k), Features=I(features) );
cvResults = cvResults[order(cvResults$CVError),];
knitr::kable( head(cvResults,n=20L), format="markdown", longtable=TRUE);


## ----knn4, echo=TRUE, eval=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## correct = list();
## accuracies = list();
## errors = list();
## features = list();
## numFeatures = list();
## k = list();
## counter = 1;
## numTrain = nrow( newTrain );
## trainCtrl = trainControl( method="LOOCV" );
## for ( rowNum in 1:10 ){
##   feature_set = cvResults$Features[[rowNum]];
##   nFeatures = cvResults$NumFeatures[rowNum];
##   df = newTrain[, c("SurvivedFactor", feature_set)]
##   knn_fit = train( SurvivedFactor~.,data=df,method="knn", trControl=trainCtrl, preProcess=c("center","scale"), tuneLength=20 );
##   knn_pred = predict( knn_fit, newdata=newTrain);
##   numRight = sum( knn_pred == newTrain$SurvivedFactor );
##   cv.error = ( numTrain - numRight )/numTrain;
##   accuracy = mean( knn_pred == newTrain$SurvivedFactor );
##   features[counter] = list( feature_set );
##   numFeatures[counter] = nFeatures;
##   accuracies[counter] = accuracy;
##   correct[counter] = numRight;
##   errors[counter] = cv.error;
##   k[counter] = knn_fit$bestTune$k;
##   counter = counter + 1;
##   cat( counter, "\n")
## }
## loocvResults = data.frame( NumFeatures=unlist(numFeatures), CVError=unlist(errors), TrainingScore=unlist(accuracies), NumCorrect=unlist(correct), K=unlist(k), Features=I(features) );
## loocvResults = loocvResults[order(loocvResults$CVError),];
## knitr::kable( head(loocvResults,n=10L), format="markdown", longtable=TRUE);


## ----knn5, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# best 9
knnTrain = newTrain;
knnTest = newTest;
trainCtrl = trainControl( method="LOOCV" );
knn_fit = train( SurvivedFactor~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumChildren+Siblings,data=knnTrain,method="knn", trControl=trainCtrl, preProcess=c("center","scale"), tuneLength=20 );
knn_fit
plot( knn_fit )
# training set performance
knn_train_pred = predict( knn_fit, newdata=knnTrain);
confusionMatrix(knn_train_pred, knnTrain$SurvivedFactor);
# test set performance
knn_pred = predict( knn_fit, newdata=knnTest);
best9KNNModelTrain = data.frame(PassengerId=knnTrain$PassengerId, Survived=as.numeric(knn_train_pred)-1);
best9KNNModel = data.frame(PassengerId=knnTest$PassengerId, Survived=as.numeric(knn_pred)-1);
write.csv( best9KNNModelTrain, "KNNBest9ModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( best9KNNModel, "KNNBest9ModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( best9KNNModelTrain, "KNNBest9ModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( best9KNNModel, "KNNBest9Model.csv", row.names=FALSE, quote=FALSE );


## ----knn6, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# best 8
trainCtrl = trainControl( method="LOOCV" );
knn_fit = train( SurvivedFactor~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+Siblings,data=knnTrain,method="knn", trControl=trainCtrl, preProcess=c("center","scale"), tuneLength=20 );
knn_fit
plot( knn_fit )
# training set performance
knn_train_pred = predict( knn_fit, newdata=knnTrain);
confusionMatrix(knn_train_pred, knnTrain$SurvivedFactor);
# test set performance
knn_pred = predict( knn_fit, newdata=knnTest);
best8KNNModelTrain = data.frame(PassengerId=knnTrain$PassengerId, Survived=as.numeric(knn_train_pred)-1);
best8KNNModel = data.frame(PassengerId=knnTest$PassengerId, Survived=as.numeric(knn_pred)-1);
write.csv( best8KNNModelTrain, "KNNBest8ModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( best8KNNModel, "KNNBest8ModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( best8KNNModelTrain, "KNNBest8ModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( best8KNNModel, "KNNBest8Model.csv", row.names=FALSE, quote=FALSE );


## ----knn7, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# best 7
trainCtrl = trainControl( method="LOOCV" );
knn_fit = train( SurvivedFactor~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Siblings,data=knnTrain,method="knn", trControl=trainCtrl, preProcess=c("center","scale"), tuneLength=20 );
knn_fit
plot( knn_fit )
# training set performance
knn_train_pred = predict( knn_fit, newdata=knnTrain);
confusionMatrix(knn_train_pred, knnTrain$SurvivedFactor);
# test set performance
knn_pred = predict( knn_fit, newdata=knnTest);
best7KNNModelTrain = data.frame(PassengerId=knnTrain$PassengerId, Survived=as.numeric(knn_train_pred)-1);
best7KNNModel = data.frame(PassengerId=knnTest$PassengerId, Survived=as.numeric(knn_pred)-1);
write.csv( best7KNNModelTrain, "KNNBest7ModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( best7KNNModel, "KNNBest7ModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( best7KNNModelTrain, "KNNBest7ModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( best7KNNModel, "KNNBest7Model.csv", row.names=FALSE, quote=FALSE );


## ----knn8, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# best 6
trainCtrl = trainControl( method="LOOCV" );
knn_fit = train( SurvivedFactor~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+Siblings,data=knnTrain,method="knn", trControl=trainCtrl, preProcess=c("center","scale"), tuneLength=20 );
knn_fit
plot( knn_fit )
# training set performance
knn_train_pred = predict( knn_fit, newdata=knnTrain);
confusionMatrix(knn_train_pred, knnTrain$SurvivedFactor);
# test set performance
knn_pred = predict( knn_fit, newdata=knnTest);
best6KNNModelTrain = data.frame(PassengerId=knnTrain$PassengerId, Survived=as.numeric(knn_train_pred)-1);
best6KNNModel = data.frame(PassengerId=knnTest$PassengerId, Survived=as.numeric(knn_pred)-1);
write.csv( best6KNNModelTrain, "KNNBest6ModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( best6KNNModel, "KNNBest6ModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( best6KNNModelTrain, "KNNBest6ModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( best6KNNModel, "KNNBest6Model.csv", row.names=FALSE, quote=FALSE );


## ----treeModel1, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tree);
tree_model = tree( SurvivedFactor~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumPassengersOnTicket+NumParents+NumChildren+SpousesFactor+Siblings,data=newTrain );
summary( tree_model )
cv_tree_model = cv.tree( tree_model, FUN=prune.misclass);
best_terminal_node_idx = which.min(cv_tree_model$dev);
best_terminal_nodes = cv_tree_model$size[best_terminal_node_idx];
pruned_tree = prune.misclass(tree_model,best=best_terminal_nodes);
plot( pruned_tree );
text( pruned_tree, pretty=0);
# training set performance against pruned tree
pruned_tree_train_pred = predict( pruned_tree, newTrain, type="class");
confusionMatrix(pruned_tree_train_pred, newTrain$SurvivedFactor);
# test set performance
tree_pred = predict( pruned_tree, newTest, type="class");
treeModelTrain = data.frame(PassengerId=knnTrain$PassengerId, Survived=as.numeric(pruned_tree_train_pred)-1);
treeModel = data.frame(PassengerId=knnTest$PassengerId, Survived=as.numeric(tree_pred)-1);
write.csv( treeModelTrain, "TreeModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( treeModel, "TreeModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( treeModelTrain, "TreeModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( treeModel, "TreeModel.csv", row.names=FALSE, quote=FALSE );


## ----treeModel2, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tree);
tree_model = tree( SurvivedFactor~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumChildren+Siblings,data=newTrain );
summary( tree_model )
cv_tree_model = cv.tree( tree_model, FUN=prune.misclass);
best_terminal_node_idx = which.min(cv_tree_model$dev);
best_terminal_nodes = cv_tree_model$size[best_terminal_node_idx];
pruned_tree = prune.misclass(tree_model,best=best_terminal_nodes);
plot( pruned_tree );
text( pruned_tree, pretty=0);
# training set performance against pruned tree
pruned_tree_train_pred = predict( pruned_tree, newTrain, type="class");
confusionMatrix(pruned_tree_train_pred, newTrain$SurvivedFactor);
# test set performance
tree_pred = predict( pruned_tree, newTest, type="class");
# scored 0.7608
treeModelTrain = data.frame(PassengerId=knnTrain$PassengerId, Survived=as.numeric(pruned_tree_train_pred)-1);
treeModel = data.frame(PassengerId=knnTest$PassengerId, Survived=as.numeric(tree_pred)-1);
write.csv( treeModelTrain, "TreeSigModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( treeModel, "TreeSigModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( treeModelTrain, "TreeSigModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( treeModel, "TreeSigModel.csv", row.names=FALSE, quote=FALSE );


## ----baggingModel1, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(randomForest);
set.seed(1);
bag_model = randomForest( SurvivedFactor~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumPassengersOnTicket+NumParents+NumChildren+SpousesFactor+Siblings,data=newTrain, mtry=12, importance=TRUE );
bag_model
plot( bag_model )
# Get training set performance
bag_train_pred = predict( bag_model, newdata=newTrain);
confusionMatrix( bag_train_pred, newTrain$SurvivedFactor);
# now test set
bag_pred = predict( bag_model, newdata=newTest);
bagModelTrain = data.frame(PassengerId=newTrain$PassengerId, Survived=as.numeric(bag_train_pred)-1);
bagModel = data.frame(PassengerId=newTest$PassengerId, Survived=as.numeric(bag_pred)-1);
write.csv( bagModelTrain, "BagModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( bagModel, "BagModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( bagModelTrain, "BagModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( bagModel, "BagModel.csv", row.names=FALSE, quote=FALSE );
# scored 0.73205


## ----baggingModel2, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(randomForest);
set.seed(1);
bag_model = randomForest( SurvivedFactor~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumChildren+Siblings,data=newTrain, mtry=9, importance=TRUE );
bag_model
plot( bag_model )
# Get training set performance
bag_train_pred = predict( bag_model, newdata=newTrain);
confusionMatrix( bag_train_pred, newTrain$SurvivedFactor);
# now test set
bag_pred = predict( bag_model, newdata=newTest);
bagModelTrain = data.frame(PassengerId=newTrain$PassengerId, Survived=as.numeric(bag_train_pred)-1);
bagModel = data.frame(PassengerId=newTest$PassengerId, Survived=as.numeric(bag_pred)-1);
write.csv( bagModelTrain, "BagSigModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( bagModel, "BagSigModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( bagModelTrain, "BagSigModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( bagModel, "BagSigModel.csv", row.names=FALSE, quote=FALSE );
# scored 0.7273


## ----rfModel1, echo=TRUE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(randomForest);
set.seed(1);
rf_model = randomForest( SurvivedFactor~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumPassengersOnTicket+NumParents+NumChildren+SpousesFactor+Siblings,data=newTrain, mtry=1, importance=TRUE );
rf_model
importance( rf_model );
varImpPlot( rf_model );
plot( rf_model )
# training set performance
rf_train_pred = predict( rf_model, newdata=newTrain);
confusionMatrix( rf_train_pred, newTrain$SurvivedFactor);
# test set performance
rf_pred = predict( rf_model, newdata=newTest);
rfModelTrain = data.frame(PassengerId=newTrain$PassengerId, Survived=as.numeric(rf_train_pred)-1);
rfModel = data.frame(PassengerId=newTest$PassengerId, Survived=as.numeric(rf_pred)-1);
write.csv( rfModelTrain, "RandomForestModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( rfModel, "RandomForestModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( rfModelTrain, "RandomForestModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( rfModel, "RandomForestModel.csv", row.names=FALSE, quote=FALSE );
# scored 0.7847


## ----rfModel2, echo=TRUE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(randomForest);
set.seed(1);
rf_model = randomForest( SurvivedFactor~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumChildren+Siblings,data=newTrain, importance=TRUE, mtry = 1 );
rf_model
importance( rf_model );
varImpPlot( rf_model );
plot( rf_model )
# training set performance
rf_train_pred = predict( rf_model, newdata=newTrain);
confusionMatrix( rf_train_pred, newTrain$SurvivedFactor);
# test set performance
rf_pred = predict( rf_model, newdata=newTest);
rfModelTrain = data.frame(PassengerId=newTrain$PassengerId, Survived=as.numeric(rf_train_pred)-1);
rfModel = data.frame(PassengerId=newTest$PassengerId, Survived=as.numeric(rf_pred)-1);
write.csv( rfModelTrain, "RandomForestSigModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( rfModel, "RandomForestSigModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( rfModelTrain, "RandomForestSigModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( rfModel, "RandomForestSigModel.csv", row.names=FALSE, quote=FALSE );
# scored 0.7823


## ----rfModel3, echo=TRUE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(randomForest);
set.seed(1);
rf_model = randomForest( SurvivedFactor~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+NumParents+Siblings+NumPassengersOnTicket,data=newTrain, importance=TRUE, mtry=1 );
rf_model
importance( rf_model );
varImpPlot( rf_model );
plot( rf_model )
# training set performance
rf_train_pred = predict( rf_model, newdata=newTrain);
confusionMatrix( rf_train_pred, newTrain$SurvivedFactor);
# test set performance
rf_pred = predict( rf_model, newdata=newTest);
rfModelTrain = data.frame(PassengerId=newTrain$PassengerId, Survived=as.numeric(rf_train_pred)-1);
rfModel = data.frame(PassengerId=newTest$PassengerId, Survived=as.numeric(rf_pred)-1);
write.csv( rfModelTrain, "RandomForestTrimmedModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( rfModel, "RandomForestTrimmedModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( rfModelTrain, "RandomForestTrimmedModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( rfModel, "RandomForestTrimmedModel.csv", row.names=FALSE, quote=FALSE );
# scored 0.7775


## ----boostedModel0, echo=TRUE, warning=FALSE, eval=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## # This is not run; ignore
## library(gbm, quietly = TRUE);
## tree_lengths = 1:4;
## best_error = Inf;
## best_idx = 0;
## min_error_idx = tree_lengths;
## min_errors = tree_lengths;
## xTrain = newTrain[,c("Survived","pclass","Sex","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","Embarked","NumPassengersOnTicket","NumParents","NumChildren","SpousesFactor","Siblings")];
## xTest = newTest[,c("Survived","pclass","Sex","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","Embarked","NumPassengersOnTicket","NumParents","NumChildren","SpousesFactor","Siblings")];
## numSamples = 10;
## trainingProportion = 0.8;
## threshold = 0.5;
## cvFolds = 8;  # n-fold validation
## nTrees = 15000;
## treeDiv = 1000;
## numIntTrees = nTrees/treeDiv - 1;
## nCores = 7; # adjust according to computer
## 
## trainMat = matrix( nrow=numSamples, ncol=numIntTrees);
## testMat = matrix( nrow=numSamples, ncol=numIntTrees);
## for ( depth in tree_lengths ){
##   set.seed(depth);
##   sampleSet = createDataPartition(1:nrow(newTrain),numSamples,p=trainingProportion);
##   for ( samp in 1:numSamples){
##     trainSample = sampleSet[[samp]];
##     testSample = setdiff(1:nrow(newTrain),trainSample);
##     boosted_model = gbm( formula=Survived~.,data=xTrain[trainSample,], distribution="bernoulli", n.trees=nTrees, cv.folds=cvFolds, interaction.depth = depth, n.cores = nCores, verbose=FALSE );
##     best_tree_num = gbm.perf( boosted_model );
##     train_scores = 0;
##     test_scores = 0;
##     for ( ntree in 1:numIntTrees){
##       treelen = (ntree-1)*treeDiv;
##       # now that the model is trained, see how it does on training set for n trees.
##       train_pred = predict( boosted_model, newdata=xTrain[trainSample,], n.trees=treelen, type="response");
##       train_predictions = as.numeric( train_pred >= threshold );
##       train_score = sum( train_predictions == newTrain$Survived[trainSample] )/length(trainSample);
##       train_scores[ ntree ] = train_score;
##       trainMat[samp,ntree] = train_score;
##       cat( "trainMat[", samp, ",", ntree, "] = ", train_score, "\n");
##       # now that the model is trained, see how it does on test set for n trees.
##       test_pred = predict( boosted_model, newdata=xTrain[testSample,], n.trees=treelen, type="response");
##       test_predictions = as.numeric( test_pred >= threshold );
##       test_score = sum( test_predictions == newTrain$Survived[testSample] )/length(testSample);
##       test_scores[ ntree ] = test_score;
##       testMat[samp,ntree] = test_score;
##     }
##   }
##   boosted_model = gbm( formula=Survived~.,data=xTrain, distribution="bernoulli", n.trees=nTrees, cv.folds=cvFolds, interaction.depth = depth, n.cores = nCores, verbose=FALSE );
##   min_error_idx[depth] = which.min( boosted_model$cv.error );
##   min_errors[depth] = min( boosted_model$cv.error );
##   if ( min_errors[depth] < best_error ){
##     best_error = min_errors[depth];
##     best_idx = depth;
##   }
## }
## gbtrees = data.frame( TreeDepth = tree_lengths, BernoulliDeviance = min_errors, NumTrees = min_error_idx );
## knitr::kable( gbtrees, format="markdown", longtable=TRUE);
## #summary( boosted_model );
## #plot( boosted_model );
## set.seed(best_idx);
## best_model = gbm( formula=Survived~.,data=xTrain, distribution="bernoulli", n.trees=nTrees, cv.folds=cvFolds, interaction.depth = best_idx, n.cores = nCores, verbose=FALSE );
## gbm.perf( best_model );
## boosted_train_pred = predict( best_model, newdata=xTrain, n.trees=min_error_idx[best_idx], type="response");
## boosted_pred = predict( best_model, newdata=xTest, n.trees=min_error_idx[best_idx], type="response");
## # probabilities >= 0.5 mean survived, < 0.5 mean perished
## boosted_train_predictions = as.numeric( boosted_train_pred >= threshold );
## boosted_predictions = as.numeric( boosted_pred >= threshold );
## boostedModelTrain = data.frame(PassengerId=newTrain$PassengerId, Survived=boosted_train_predictions);
## boostedModel = data.frame(PassengerId=newTest$PassengerId, Survived=boosted_predictions);
## boostedModelTrainProbs = data.frame(PassengerId=newTrain$PassengerId, Survived=as.numeric(boosted_train_pred));
## boostedModelProbs = data.frame(PassengerId=newTest$PassengerId, Survived=as.numeric(boosted_pred));
## write.csv( boostedModelTrainProbs, "BoostedTreeModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
## write.csv( boostedModelProbs, "BoostedTreeModelProbs.csv", row.names=FALSE, quote=FALSE );
## write.csv( boostedModelTrain, "BoostedTreeModelTrain.csv", row.names=FALSE, quote=FALSE );
## write.csv( boostedModel, "BoostedTreeModel.csv", row.names=FALSE, quote=FALSE );
## # originally scored 0.76076 with interaction depth =4, 0.77990 with interaction depth = 1
## # after 10-fold x-validation, considering interaction depths of 1 to 4, scored a 0.76555
## # same model , stopping at 4800 trees yields 0.77511


## ----boostedModel1, echo=TRUE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(gbm, quietly = TRUE);
tree_lengths = 1:10;
numLengths = length( tree_lengths );
best_error = Inf;
best_idx = 0;
best_shrinkage = 0;
best_steps = 0;
xTrain = newTrain[,c("Survived","pclass","Sex","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","Embarked","NumPassengersOnTicket","NumParents","NumChildren","SpousesFactor","Siblings")];
xTest = newTest[,c("Survived","pclass","Sex","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","Embarked","NumPassengersOnTicket","NumParents","NumChildren","SpousesFactor","Siblings")];
learningRates = c(0.1,0.07,0.05,0.03,0.02,0.01);
numLR = length( learningRates );
threshold = 0.5;
cvFolds = 10;  # n-fold validation
nTrees = 3000; 
nCores = 7; # adjust according to computer
tree_depths = rep(0,numLengths*numLR);
steps = rep(0,numLengths*numLR);
deviances = rep(0,numLengths*numLR);
shrinkages = rep(0,numLengths*numLR);
idx = 1;

for ( depth in tree_lengths ){
  set.seed(depth);
  for ( lrIdx in 1:numLR){
      learningRate = learningRates[lrIdx];
      boosted_model = gbm( formula=Survived~.,data=xTrain, distribution="bernoulli", n.trees=nTrees, cv.folds=cvFolds, interaction.depth = depth, shrinkage = learningRate, n.cores = nCores, verbose=FALSE );
      idxCVErr = gbm.perf( boosted_model, plot.it=FALSE );
      cvError = boosted_model$cv.error[idxCVErr];
      if ( cvError < best_error ){
        best_error = cvError;
        best_idx = depth;
        best_shrinkage = learningRate;
        best_steps = idxCVErr;
      }
      tree_depths[idx] = depth;
      shrinkages[idx] = learningRate;
      steps[idx] = idxCVErr;
      deviances[idx] = cvError;
      idx = idx + 1;
  }
}
gbtrees = data.frame( TreeDepth = tree_depths, Shrinkage = shrinkages, NumTrees = steps, BernoulliDeviance = deviances );
knitr::kable( gbtrees, format="markdown", longtable=TRUE);
#summary( boosted_model );
#plot( boosted_model );
# create a line chart
xrange = range( shrinkages );
yrange = range( deviances );
plot( xrange, yrange, type="n", xlab="Shrinkage", ylab="CV Accuracy (Bernoulli Deviance)");
colors = rainbow( numLengths );
linetype = tree_lengths;
plotchar = seq(0,numLengths,1);
# add lines
for ( i in tree_lengths){
  thisDepthTrees = subset( gbtrees, gbtrees$TreeDepth == i );
  lines(thisDepthTrees$Shrinkage, thisDepthTrees$BernoulliDeviance, type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i]);
}
title( "Bernoulli Deviance by Shrinkage and Tree Depth");
legend( 0.08, yrange[2], 1:numLengths, cex=0.8, col=colors, pch=plotchar, lty=linetype, title="Tree Depth");
set.seed(best_idx);
best_model = gbm( formula=Survived~.,data=xTrain, distribution="bernoulli", n.trees=nTrees, cv.folds=cvFolds, interaction.depth = best_idx, shrinkage = best_shrinkage, n.cores = nCores, verbose=FALSE );
gbm.perf( best_model );
boosted_train_pred = predict( best_model, newdata=xTrain, n.trees=best_steps, type="response");
boosted_train_calls = as.numeric( boosted_train_pred >= threshold );
confusionMatrix(boosted_train_calls,newTrain$Survived);
boosted_pred = predict( best_model, newdata=xTest, n.trees=best_steps, type="response");
# probabilities >= 0.5 mean survived, < 0.5 mean perished
boosted_predictions = as.numeric( boosted_pred >= threshold );
boostedModelTrain = data.frame(PassengerId=newTrain$PassengerId, Survived=boosted_train_calls);
boostedModel = data.frame(PassengerId=newTest$PassengerId, Survived=boosted_predictions);
boostedModelTrainProbs = data.frame(PassengerId=newTrain$PassengerId, Survived=as.numeric(boosted_train_pred));
boostedModelProbs = data.frame(PassengerId=newTest$PassengerId, Survived=as.numeric(boosted_pred));
write.csv( boostedModelTrainProbs, "BoostedTreeModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( boostedModelProbs, "BoostedTreeModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( boostedModelTrain, "BoostedTreeModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( boostedModel, "BoostedTreeModel.csv", row.names=FALSE, quote=FALSE );
# interaction.depth = 8, nTrees = 118, learningRate = 0.05
# scored 0.756


## ----boostedModel2, echo=TRUE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(gbm);
tree_lengths = 1:10;
numLengths = length( tree_lengths );
best_error = Inf;
best_idx = 0;
best_shrinkage = 0;
best_steps = 0;
significant_features = c("FixedTitle", "Sex", "Siblings", "FixedAge", "CabinAssignment", "pclass", "NumChildren", "FarePerPassenger", "Embarked");
xTrain = newTrain[,c("Survived", significant_features) ];
xTest = newTest[,c("Survived", significant_features) ];
learningRates = c(0.1,0.07,0.05,0.03,0.02,0.01);
numLR = length( learningRates );
threshold = 0.5;
cvFolds = 10;  # n-fold validation
nTrees = 3000; 
nCores = 7; # adjust according to computer
tree_depths = rep(0,numLengths*numLR);
steps = rep(0,numLengths*numLR);
deviances = rep(0,numLengths*numLR);
shrinkages = rep(0,numLengths*numLR);
idx = 1;

for ( depth in tree_lengths ){
  set.seed(depth);
  for ( lrIdx in 1:numLR){
      learningRate = learningRates[lrIdx];
      boosted_model = gbm( formula=Survived~.,data=xTrain, distribution="bernoulli", n.trees=nTrees, cv.folds=cvFolds, interaction.depth = depth, shrinkage = learningRate, n.cores = nCores, verbose=FALSE );
      idxCVErr = gbm.perf( boosted_model, plot.it=FALSE );
      cvError = boosted_model$cv.error[idxCVErr];
      if ( cvError < best_error ){
        best_error = cvError;
        best_idx = depth;
        best_shrinkage = learningRate;
        best_steps = idxCVErr;
      }
      tree_depths[idx] = depth;
      shrinkages[idx] = learningRate;
      steps[idx] = idxCVErr;
      deviances[idx] = cvError;
      idx = idx + 1;
  }
}
gbtrees = data.frame( TreeDepth = tree_depths, Shrinkage = shrinkages, NumTrees = steps, BernoulliDeviance = deviances );
knitr::kable( gbtrees, format="markdown", longtable=TRUE);
#summary( boosted_model );
#plot( boosted_model );
# create a line chart
xrange = range( shrinkages );
yrange = range( deviances );
plot( xrange, yrange, type="n", xlab="Shrinkage", ylab="Cross Validation Error (Bernoulli Deviance)");
colors = rainbow( numLengths );
linetype = tree_lengths;
plotchar = seq(0,numLengths,1);
# add lines
for ( i in tree_lengths){
  thisDepthTrees = subset( gbtrees, gbtrees$TreeDepth == i );
  lines(thisDepthTrees$Shrinkage, thisDepthTrees$BernoulliDeviance, type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i]);
}
title( "Bernoulli Deviance by Shrinkage and Tree Depth");
legend( 0.08, yrange[2], 1:numLengths, cex=0.8, col=colors, pch=plotchar, lty=linetype, title="Tree Depth");
set.seed(best_idx);
best_model = gbm( formula=Survived~.,data=xTrain, distribution="bernoulli", n.trees=nTrees, cv.folds=cvFolds, interaction.depth = best_idx, shrinkage = best_shrinkage, n.cores = nCores, verbose=FALSE );
gbm.perf( best_model );
boosted_train_pred = predict( best_model, newdata=xTrain, n.trees=best_steps, type="response");
boosted_train_calls = as.numeric( boosted_train_pred >= threshold );
confusionMatrix(boosted_train_calls,newTrain$Survived);
boosted_pred = predict( best_model, newdata=xTest, n.trees=best_steps, type="response");
# probabilities >= 0.5 mean survived, < 0.5 mean perished
boosted_predictions = as.numeric( boosted_pred >= threshold );
boostedModelTrain = data.frame(PassengerId=newTrain$PassengerId, Survived=boosted_train_calls);
boostedModel = data.frame(PassengerId=newTest$PassengerId, Survived=boosted_predictions);
boostedModelTrainProbs = data.frame(PassengerId=newTrain$PassengerId, Survived=as.numeric(boosted_train_pred));
boostedModelProbs = data.frame(PassengerId=newTest$PassengerId, Survived=as.numeric(boosted_pred));
write.csv( boostedModelTrainProbs, "BoostedTreeSigModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( boostedModelProbs, "BoostedTreeSigModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( boostedModelTrain, "BoostedTreeSigModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( boostedModel, "BoostedTreeSigModel.csv", row.names=FALSE, quote=FALSE );
# interaction.depth = 8, nTrees = 328, learningRate = 0.02
# scored 0.75119


## ----svm1, echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(e1071);
library(caret);
#xTrain = newTrain[,c("pclass","Sex","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","Embarked","NumPassengersOnTicket","NumParents","NumChildren","SpousesFactor","Siblings")];
#yTrain = newTrain[,"SurvivedFactor"];
xTrain = model.matrix( ~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumPassengersOnTicket+NumParents+NumChildren+SpousesFactor+Siblings,data=whiteTrain);
yTrain = newTrain[,"SurvivedFactor"];
#xTest = newTest[,c("pclass","Sex","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","Embarked","NumPassengersOnTicket","NumParents","NumChildren","SpousesFactor","Siblings")];
xTest = model.matrix( ~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumPassengersOnTicket+NumParents+NumChildren+SpousesFactor+Siblings,data=whiteTest);
#xTrainSVM = model.matrix( ~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumPassengersOnTicket+NumParents+NumChildren+SpousesFactor+Siblings,data=newTrain);
#xTestSVM = model.matrix( ~pclass+Sex+FixedAge+FarePerPassenger+FixedTitle+CabinAssignment+Embarked+NumPassengersOnTicket+NumParents+NumChildren+SpousesFactor+Siblings,data=newTest);
costs = c(0.01, 0.1, 0.5, 0.9, 1, 1.1);
gammas = c(0.0001, 0.01, 0.05, 0.1, 0.5);
kernels = c("linear", "polynomial", "radial", "sigmoid");
bestCost = costs[1];
bestGamma = gammas[1];
bestKernel = kernels[1];
bestError = Inf;
errors = rep(Inf,1,length(kernels));
ctr = 1;
for ( kernel in kernels ){
  tune.out = tune( "svm", xTrain, train.y=yTrain, kernel=kernel, ranges=list( cost=costs,gamma=gammas) );
  tune.out$best.performance
  tune.out$best.model
  thisError = tune.out$best.performance;
  errors[ctr] = thisError;
  if ( thisError <  bestError ){
    bestError = thisError;
    bestKernel = kernel;
    bestGamma = tune.out$best.model$gamma;
    bestCost = tune.out$best.model$cost;
  }
  ctr = ctr + 1;
}
svm.fit = svm( x=xTrain, y=yTrain, kernel=bestKernel, cost=bestCost, gamma=bestGamma);
#plot( svm.fit, xTrain );
svm.train.pred = predict( svm.fit, xTrain );
svmTrainingPreds = as.numeric( svm.train.pred ) - 1;
truth = newTrain$Survived;
confusionMatrix( svmTrainingPreds, truth );
# scores a 0.8294 on the training set
svm.test.pred = predict( svm.fit, xTest );  
svmPredictions = as.numeric( svm.test.pred ) - 1;
svmModelTrain = data.frame(PassengerId=newTrain$PassengerId, Survived=svmTrainingPreds);
svmModel = data.frame(PassengerId=newTest$PassengerId, Survived=svmPredictions);

write.csv( svmModelTrain, "SVMModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( svmModel, "SVMModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( svmModelTrain, "SVMModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( svmModel, "SVMModel.csv", row.names=FALSE, quote=FALSE );
# scored a 0.7727


## ----nnModel1, echo=TRUE, eval=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## ### NN Model
## library(nnet);
## #xTrain = newTrain[,c("SurvivedFactor", significant_features) ];
## #xTest = newTest[,c("SurvivedFactor", significant_features) ];
## 
## xTrain = whiteTrain[,c("SurvivedFactor","pclass","Sex","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","Embarked","NumPassengersOnTicket","NumParents","NumChildren","SpousesFactor","Siblings")];
## xTest = whiteTest[,c("SurvivedFactor","pclass","Sex","FixedAge","FarePerPassenger","FixedTitle","CabinAssignment","Embarked","NumPassengersOnTicket","NumParents","NumChildren","SpousesFactor","Siblings")];
## 
## nn = nnet( SurvivedFactor ~ ., data = xTrain, size=12, maxit=500, trace=FALSE);
## # How do we do on the training data?
## 
## nn_train_pred_class = predict( nn, xTrain, type="class" );  # yields "0", "1"
## nn_train_pred = as.numeric( nn_train_pred_class );   # transform to 0, 1
## confusionMatrix(nn_train_pred,xTrain$Survived);
## 
## # try on test data
## nn_test_pred_class = predict( nn, xTest, type="class" );  # yields "0", "1"
## nn_test_pred = as.numeric( nn_test_pred_class );   # transform to 0, 1
## # write to  file
## nnModelTrain = data.frame(PassengerId=newTrain$PassengerId, Survived=nn_train_pred);
## nnModel = data.frame(PassengerId=newTest$PassengerId, Survived=nn_test_pred);
## write.csv( nnModelTrain, "NeuralNetworkFullModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
## write.csv( nnModel, "NeuralNetworkFullModelProbs.csv", row.names=FALSE, quote=FALSE );
## write.csv( nnModelTrain, "NeuralNetworkFullModelTrain.csv", row.names=FALSE, quote=FALSE );
## write.csv( nnModel, "NeuralNetworkFullModel.csv", row.names=FALSE, quote=FALSE );
## # best "over"trained model scores 0.73205 (306/418)
## 
## # Now, we wish to select parameters for neural network by using 10-fold cross-validation with caret
## tuningGrid = expand.grid(size=1:12,decay=c(0,0.0001,0.05,0.1));
## set.seed( 1 );
## trControl = trainControl( method="repeatedcv", number=10, repeats=10 );
## nn_cv = train( SurvivedFactor ~ ., data=xTrain, method="nnet", trControl = trControl, tuneGrid=tuningGrid, verbose=FALSE, trace=FALSE);
## 
## best_size = nn_cv$bestTune[1,"size"];
## best_decay = nn_cv$bestTune[1,"decay"];
## best_nn = nnet( SurvivedFactor ~ ., data = xTrain, size=best_size, decay=best_decay, maxit=500, trace=FALSE);
## best_nn_train_pred_class = predict( best_nn, newdata=xTrain, type="class" );  # yields "0", "1"
## best_nn_train_pred = as.numeric( best_nn_train_pred_class );   # transform to 0, 1
## confusionMatrix( best_nn_train_pred, xTrain$Survived);
## # now do on test data
## best_nn_test_pred_class = predict( best_nn, newdata=xTest, type="class" );  # yields "0", "1"
## best_nn_test_pred = as.numeric( best_nn_test_pred_class );   # transform to 0, 1
## # write to  file
## nnModelTrain = data.frame(PassengerId=newTrain$PassengerId, Survived=best_nn_train_pred);
## nnModel = data.frame(PassengerId=newTest$PassengerId, Survived=best_nn_test_pred);
## write.csv( nnModelTrain, "NeuralNetworkFullModelCVTrainProbs.csv", row.names=FALSE, quote=FALSE );
## write.csv( nnModel, "NeuralNetworkFullModelCVProbs.csv", row.names=FALSE, quote=FALSE );
## write.csv( nnModelTrain, "NeuralNetworkFullModelCVTrain.csv", row.names=FALSE, quote=FALSE );
## write.csv( nnModel, "NeuralNetworkFullModelCV.csv", row.names=FALSE, quote=FALSE );
## # Neural network model scores 0.7847


## ----nnModel2, echo=TRUE, eval=TRUE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### NN Model
library(nnet);
xTrain = whiteTrain[,c("SurvivedFactor", significant_features) ];
xTest = whiteTest[,c("SurvivedFactor", significant_features) ];

nn = nnet( SurvivedFactor ~ ., data = xTrain, size=length(significant_features), maxit=500, trace=FALSE);
# How do we do on the training data?

nn_train_pred_class = predict( nn, xTrain, type="class" );  # yields "0", "1"
nn_train_pred = as.numeric( nn_train_pred_class );   # transform to 0, 1
confusionMatrix(nn_train_pred,xTrain$Survived);
# 0.9068 accuracy on the training set

# try on test data
nn_test_pred_class = predict( nn, xTest, type="class" );  # yields "0", "1"
nn_test_pred = as.numeric( nn_test_pred_class );   # transform to 0, 1
# write to  file
nnModelTrain = data.frame(PassengerId=newTrain$PassengerId, Survived=nn_train_pred);
nnModel = data.frame(PassengerId=newTest$PassengerId, Survived=nn_test_pred);
write.csv( nnModelTrain, "NeuralNetworkSigModelTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( nnModel, "NeuralNetworkSigModelProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( nnModelTrain, "NeuralNetworkSigModelTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( nnModel, "NeuralNetworkSigModel.csv", row.names=FALSE, quote=FALSE );
# best "over"trained model scores 0.72727 (304/418)

# Now, we wish to select parameters for neural network by using 10-fold cross-validation with caret
tuningGrid = expand.grid(size=1:length(significant_features),decay=c(0,0.0001,0.05,0.1));
set.seed( 1 );
trControl = trainControl( method="repeatedcv", number=10, repeats=10 );
nn_cv = train( SurvivedFactor ~ ., data=xTrain, method="nnet", trControl = trControl, tuneGrid=tuningGrid, verbose=FALSE, trace=FALSE);

best_size = nn_cv$bestTune[1,"size"];
best_decay = nn_cv$bestTune[1,"decay"];
best_nn = nnet( SurvivedFactor ~ ., data = xTrain, size=best_size, decay=best_decay, maxit=500, trace=FALSE);
best_nn_train_pred_class = predict( best_nn, newdata=xTrain, type="class" );  # yields "0", "1"
best_nn_train_pred = as.numeric( best_nn_train_pred_class );   # transform to 0, 1
confusionMatrix( best_nn_train_pred, xTrain$Survived);
# now do on test data
best_nn_test_pred_class = predict( best_nn, newdata=xTest, type="class" );  # yields "0", "1"
best_nn_test_pred = as.numeric( best_nn_test_pred_class );   # transform to 0, 1
# write to  file
nnModelTrain = data.frame(PassengerId=newTrain$PassengerId, Survived=best_nn_train_pred);
nnModel = data.frame(PassengerId=newTest$PassengerId, Survived=best_nn_test_pred);
write.csv( nnModelTrain, "NeuralNetworkSigModelCVTrainProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( nnModel, "NeuralNetworkSigModelCVProbs.csv", row.names=FALSE, quote=FALSE );
write.csv( nnModelTrain, "NeuralNetworkSigModelCVTrain.csv", row.names=FALSE, quote=FALSE );
write.csv( nnModel, "NeuralNetworkSigModelCV.csv", row.names=FALSE, quote=FALSE );
# Neural network model scores 0.7847 (334/418) - same as full featured model


## ----ensemble, echo=TRUE, eval=TRUE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(matlab,quietly=TRUE);
library(ROCR,quietly=TRUE);
library(tidyr,quietly=TRUE);
library(dplyr,quietly=TRUE);
library(tibble,quietly=TRUE);

all_train_models = c( "BagModelTrainProbs.csv", "BagSigModelTrainProbs.csv", "BoostedTreeModelTrainProbs.csv", "BoostedTreeSigModelTrainProbs.csv", "GenderModelTrainProbs.csv", "KNNBest6ModelTrainProbs.csv", "KNNBest7ModelTrainProbs.csv", "KNNBest8ModelTrainProbs.csv", "KNNBest9ModelTrainProbs.csv", "KNNFullModelTrainProbs.csv", "LassoRegressionFullModelTrainProbs.csv", "LassoRegressionSigModelTrainProbs.csv", "LDAModelTrainProbs.csv", "LDAReducedModelTrainProbs.csv", "LogisticRegressionFullModelTrainProbs.csv", "LogisticRegressionSigModelTrainProbs.csv", "NeuralNetworkSigModelCVTrainProbs.csv", "NeuralNetworkSigModelTrainProbs.csv", "NullModelTrainProbs.csv", "QDAModelTrainProbs.csv", "QDASigModelTrainProbs.csv", "RandomForestModelTrainProbs.csv", "RandomForestSigModelTrainProbs.csv", "RandomForestTrimmedModelTrainProbs.csv", "RidgeRegressionFullModelTrainProbs.csv", "RidgeRegressionSigModelTrainProbs.csv", "SVMModelTrainProbs.csv", "TreeModelTrainProbs.csv", "TreeSigModelTrainProbs.csv", "WCModelTrainProbs.csv" );

train_models = c( "BoostedTreeModelTrainProbs.csv", "BoostedTreeSigModelTrainProbs.csv", "GenderModelTrainProbs.csv", "LassoRegressionFullModelTrainProbs.csv", "LassoRegressionSigModelTrainProbs.csv", "LDAModelTrainProbs.csv", "LDAReducedModelTrainProbs.csv", "LogisticRegressionFullModelTrainProbs.csv", "LogisticRegressionSigModelTrainProbs.csv", "NeuralNetworkSigModelCVTrainProbs.csv", "NeuralNetworkSigModelTrainProbs.csv", "QDAModelTrainProbs.csv", "QDASigModelTrainProbs.csv", "RandomForestModelTrainProbs.csv", "RandomForestSigModelTrainProbs.csv", "RandomForestTrimmedModelTrainProbs.csv", "RidgeRegressionFullModelTrainProbs.csv", "RidgeRegressionSigModelTrainProbs.csv", "SVMModelTrainProbs.csv", "TreeModelTrainProbs.csv", "TreeSigModelTrainProbs.csv", "WCModelTrainProbs.csv" );

all_test_models = c( "BagModelProbs.csv", "BagSigModelProbs.csv", "BoostedTreeModelProbs.csv", "BoostedTreeSigModelProbs.csv", "GenderModelProbs.csv", "KNNBest6ModelProbs.csv", "KNNBest7ModelProbs.csv", "KNNBest8ModelProbs.csv", "KNNBest9ModelProbs.csv", "KNNFullModelProbs.csv", "LassoRegressionFullModelProbs.csv", "LassoRegressionSigModelProbs.csv", "LDAModelProbs.csv", "LDAReducedModelProbs.csv", "LogisticRegressionFullModelProbs.csv", "LogisticRegressionSigModelProbs.csv", "NeuralNetworkSigModelCVProbs.csv", "NeuralNetworkSigModelProbs.csv", "NullModelProbs.csv", "QDAModelProbs.csv", "QDASigModelProbs.csv", "RandomForestModelProbs.csv", "RandomForestSigModelProbs.csv", "RandomForestTrimmedModelProbs.csv", "RidgeRegressionFullModelProbs.csv", "RidgeRegressionSigModelProbs.csv", "SVMModelProbs.csv", "TreeModelProbs.csv", "TreeSigModelProbs.csv", "WCModelProbs.csv" );

test_models = c( "BoostedTreeModelProbs.csv", "BoostedTreeSigModelProbs.csv", "GenderModelProbs.csv", "LassoRegressionFullModelProbs.csv", "LassoRegressionSigModelProbs.csv", "LDAModelProbs.csv", "LDAReducedModelProbs.csv", "LogisticRegressionFullModelProbs.csv", "LogisticRegressionSigModelProbs.csv", "NeuralNetworkSigModelCVProbs.csv", "NeuralNetworkSigModelProbs.csv", "QDAModelProbs.csv", "QDASigModelProbs.csv", "RandomForestModelProbs.csv", "RandomForestSigModelProbs.csv", "RandomForestTrimmedModelProbs.csv", "RidgeRegressionFullModelProbs.csv", "RidgeRegressionSigModelProbs.csv", "SVMModelProbs.csv", "TreeModelProbs.csv", "TreeSigModelProbs.csv", "WCModelProbs.csv" );

trainModels = data.frame(PassengerId=1:nrow(newTrain), Survived=newTrain$Survived );
testModels = data.frame(PassengerId=1:nrow(newTest));

# read all the train model predictions and store in dataframe
numModels = length( train_models );
for ( i in 1:numModels ){
  trainFileName = train_models[i];
  fp = fileparts( trainFileName );
  modelName = gsub( "TrainProbs", "", fp$name );
  trainModelFrame = read.csv( trainFileName );
  trainModels[ modelName ] = trainModelFrame$Survived;
  # now for test
  testFileName = test_models[i];
  fp = fileparts( testFileName );
  modelName = gsub( "Probs", "", fp$name );
  testModelFrame = read.csv( testFileName );
  testModels[ modelName ] = testModelFrame$Survived;
}

allTrainModels = data.frame(PassengerId=1:nrow(newTrain), Survived=newTrain$Survived );
allTestModels = data.frame(PassengerId=1:nrow(newTest));

numAllModels = length( all_train_models );
for ( i in 1:numAllModels ){
  trainFileName = all_train_models[i];
  fp = fileparts( trainFileName );
  modelName = gsub( "TrainProbs", "", fp$name );
  trainModelFrame = read.csv( trainFileName );
  allTrainModels[ modelName ] = trainModelFrame$Survived;
  # now for test
  testFileName = all_test_models[i];
  fp = fileparts( testFileName );
  modelName = gsub( "Probs", "", fp$name );
  testModelFrame = read.csv( testFileName );
  allTestModels[ modelName ] = testModelFrame$Survived;
}

all_train_model_names = setdiff( names(allTrainModels), c("PassengerId", "Survived") );

# this calculates all correlations between models.  Some corr calcs break because
# they're all one value so standard deviation calc blows up.
modelCors <- trainModels[,!names(trainModels) %in% c("PassengerId","Survived")] %>% 
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'Model_A') %>%
  gather(Model_B, correlation, -Model_A)

corrThresh = 0.75;
modelNames = unique(modelCors$Model_A);

# this recursive function is intended to output groups of models that 
# have inter correlations below threshold
addToModelSet = function( modelSet, corrs, model, corrThresh ){
    #modelSet[ length(modelSet)+1 ] = model;
    if ( ! model %in% modelSet ){
        modelSet = union( modelSet, model );
    }
    idxUsed = c();
    idxUnused = 1:nrow(corrs);
   
    toUse = c();
    for ( idx in idxUnused ){
        thisModel = corrs$Model_A[idx];
        if ( thisModel %in% modelSet ){
            toUse[ length(toUse) + 1] = idx;
        }
    }
   
    for ( idx in toUse ){
        addModel = TRUE;
        modelA = corrs$Model_A[idx];
        modelB = corrs$Model_B[idx];
        correlation = corrs$correlation[idx];
        if ( correlation < corrThresh ){
            # now, check corr with every other member of modelSet
            for ( model in modelSet ){
                # want to check modelB corr with all models in modelSet
              idxModelA = which( corrs$Model_A == model );
              idxModelB = which( corrs$Model_B == modelB );
                if ( model == modelB ){
                    idxUsed[ length(idxUsed) + 1 ] = idx;
                    # correlations of the same model
                    next;
                }
              rightRowIdx = intersect( idxModelA, idxModelB );
              if( length( rightRowIdx ) == 1 && rightRowIdx < nrow(corrs) ){
                rightRow = corrs[ rightRowIdx, ];
              }
              else{
                next;
              }
                rightCorr = rightRow$correlation;
                if ( rightCorr > corrThresh ){
                    idxUsed[ length(idxUsed) + 1 ] = idx;
                    addModel = FALSE;
                    break;
                }
            }
            idxUsed[ length(idxUsed) + 1 ] = idx;
        }
        else{
          addModel = FALSE;
        }
        if ( addModel ){
          idxToUse = setdiff( idxUnused, idxUsed );
          newCorrs = corrs[idxToUse,]
          #newCorrs = corrs;
          modelSet = addToModelSet( modelSet, newCorrs, modelB, corrThresh);
        }
    }
    
    return( modelSet );
}

modelSets = list();
print( "Model sets that have inter-correlations less than threshold:")
for ( model in modelNames ){
  modelSet = addToModelSet( c(), modelCors, model, corrThresh );
  cat( paste(paste(modelSet, collapse=", "),"\n"));
  modelSets[ length(modelSets) + 1 ] = list( modelSet );
}

# now go through each model and find the other models which are correlated below
# threshold (0.75)
# manually choose the set #14
uncorrelatedModelSet = modelSets[[14]];

# now for all models that have passed criteria, create a model
selectedTrainModels = trainModels[,c("Survived", uncorrelatedModelSet)];
ensemble_lr = glm( Survived~., data=selectedTrainModels, family=binomial);
summary( ensemble_lr );
trainSetProbs = predict( ensemble_lr, trainModels, type="response");  # now a vector of probabilities
# probabilities >= 0.5 mean survived, < 0.5 mean perished
threshold = 0.5;
selectedTrainPredictions = as.numeric( trainSetProbs >= threshold );
confusionMatrix( selectedTrainPredictions, trainModels$Survived );

# check to see if threshold 0.5 is appropriate
rocr.pred = prediction( trainSetProbs, trainModels$Survived);
acc.perf = performance( rocr.pred, measure="acc");
plot( acc.perf, main="Manual Ensemble Logistic Regression Model Training Set Accuracy by Threshold" );
# threshold 0.5 is appropriate

# now, how'd we do against the test set?
testSetProbs = predict( ensemble_lr, testModels, type="response");
testPredictions = as.numeric( testSetProbs >= threshold );
ensembleModelManual = data.frame(PassengerId=newTest$PassengerId, Survived=testPredictions);
write.csv( ensembleModelManual, "EnsembleModelManual.csv", row.names=FALSE, quote=FALSE );
ensembleModelTrainManual = data.frame(PassengerId=newTrain$PassengerId, Survived=selectedTrainPredictions);
write.csv( ensembleModelTrainManual, "EnsembleModelManualTrain.csv", row.names=FALSE, quote=FALSE );

# now try just an ensemble model, taking means
ensembleTrainModels = allTrainModels[,all_train_model_names];
# simply take the means across the rows to get "probabilities" of survival for each passenger
ensembleTrainPredictions = rowMeans( ensembleTrainModels );
ensembleTrainCalls = round( ensembleTrainPredictions );
confusionMatrix( ensembleTrainCalls, allTrainModels$Survived );

# check to see if threshold 0.5 is appropriate
rocr.pred = prediction( ensembleTrainPredictions, allTrainModels$Survived);
acc.perf = performance( rocr.pred, measure="acc");
plot( acc.perf, main="All Means Ensemble Logistic Regression Model Training Set Accuracy by Threshold" );
# threshold 0.5 is appropriate

# How'd the ensemble model do against the test set?
ensembleTestModels = testModels[,modelNames];
# simply take the means across the rows to get "probabilities" of survival for each passenger
ensembleTestPredictions = rowMeans( ensembleTestModels );
ensembleTestCalls = round( ensembleTestPredictions );

#fullLogisticRegressionModelTrain = data.frame(PassengerId=train$PassengerId, Survived=predictions);
#fullLogisticRegressionModel = data.frame(PassengerId=test$PassengerId, Survived=fullLogisticPredictions);
# now, create a new test and train data frame with these models as predictors

# write to  file
ensembleModelMeans = data.frame(PassengerId=newTest$PassengerId, Survived=ensembleTestCalls);
write.csv( ensembleModelMeans, "EnsembleModelMeans.csv", row.names=FALSE, quote=FALSE );
ensembleModelTrainMeans = data.frame(PassengerId=newTrain$PassengerId, Survived=ensembleTrainCalls);
write.csv( ensembleModelTrainMeans, "EnsembleModelMeansTrain.csv", row.names=FALSE, quote=FALSE );


## ----cheatModel1, echo=TRUE, eval=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## library(RCurl);
## library(XML);
## library(readr);
## #testIDs = 892:nrow(data_combined);
## #userAgents = c( "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0", "Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36", "Mozilla/5.0 (iPhone; CPU iPhone OS 10_3_1 like Mac OS X) AppleWebKit/603.1.30 (KHTML, like Gecko) Version/10.0 Mobile/14E304 Safari/602.1", "Mozilla/5.0 (compatible; MSIE 9.0; Windows Phone OS 7.5; Trident/5.0; IEMobile/9.0)");
## userAgents = c( "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.78 Safari/537.36", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36 Edge/15.15063" );
## numUserAgents = length( userAgents );
## trainIDs = 1:891;
## testIDs = 892:nrow(data_combined);
## setIDs = 1:nrow(data_combined);
## trainURLS = c();
## setHits = data_combined[,c("PassengerId","Survived")];
## setHits$SurvivorHits = 0;
## setHits$VictimHits = 0;
## setHits$SurvivorFirst = 0;
## setHits$SurvivedPrediction = 0;
## for ( pass_idx in 1:length(setIDs) ){
##   i = setIDs[pass_idx];
##   name = gsub( " ", "+", as.character( data_combined[i,"NameString"] ) ); # remove whitespace
##   sex = as.character( data_combined[i,"Sex"] );
##   searchURL = paste0( "https://www.google.com/search?q=", name, "+", sex, "+titanic" );
##   trainURLS[i] = searchURL;
##   randomUserAgent = userAgents[ round( runif(1, 1, numUserAgents) ) ];
##   wd = getwd();
##   localFile = paste0( wd, "/html/", i, ".html" );
##   if ( file.exists( localFile )){
##     resultsHTML = read_file( localFile );
##   }else{
##     resultsHTML = getURL( searchURL, httpheader=c('User-Agent'=randomUserAgent) );
##     write( resultsHTML, localFile );
##     Sys.sleep( runif( 1, 27, 67)); # randomly sleep between 27 and 67 seconds
##     # this is necessary because if Google thinks you're mining their data with a bot
##     # you will be grounded and sent to your room for a timeout.
##   }
##   # now search for "victim" and "survivor"
##   victimHits = gregexpr("victim", resultsHTML, ignore.case=TRUE);
##   victimHitIndices = which( victimHits[[1]] > -1 );
##   numVictimHits = length( victimHitIndices );
##   setHits[pass_idx,"VictimHits"] = numVictimHits;
##   firstVictimHit = victimHits[[1]][1];
##   survivorHits = gregexpr("survivor", resultsHTML, ignore.case=TRUE);
##   survivorHitIndices = which( survivorHits[[1]] > -1 );
##   numSurvivorHits = length( survivorHitIndices );
##   setHits[pass_idx,"SurvivorHits"] = numSurvivorHits;
##   firstSurvivorHit = survivorHits[[1]][1];
##   #cat( "row ", i, "\n")
##   #assert_that( ( numSurvivorHits > 0 ) || (numVictimHits > 0) );
##   if (!( ( numSurvivorHits > 0 ) || (numVictimHits > 0) ) ){
##     cat( "Could not find 'victim' or 'survivor' keyword for PassengerID ", i, ".  Manually investigate and update .html file with 'survived' or 'victim'.\n")
##   }
##   survived = FALSE;
##   if ( numSurvivorHits > numVictimHits ){
##       survived = TRUE;
##   }
##   else if ( ( numSurvivorHits > 0 ) && ( numVictimHits > 0 ) ){
##     # both are positive, take the min
##     if ( firstSurvivorHit < firstVictimHit ){
##       survived = TRUE;
##     }
##     else{
##       survived = FALSE;
##     }
##     setHits[pass_idx,"SurvivorFirst"] = as.numeric( survived );
##   }
##   survived_flag = 0;
##   if( survived ){
##     survived_flag = 1;
##   }
##   setHits[pass_idx,"SurvivedPrediction"] = survived_flag;
##   if ( !is.na( data_combined[i,"Survived"] ) && survived_flag != data_combined[i,"Survived"] ){
##     #cat( "Prediction does not match truth at ", i, as.character( data_combined[i,"Name"] ), "\n");
##   }
## }
## # evaluate training set performance
## trainingPredictions = setHits$SurvivedPrediction[1:891];
## confusionMatrix( trainingPredictions, data_combined$Survived[1:891])
## 
## cheatTestPredictions = setHits$SurvivedPrediction[892:nrow(data_combined)];
## 
## write.csv( setHits, file="CheatModel1.csv", quote=FALSE);


## ----cheatModel2, echo=TRUE, eval=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## # read in CheatModel1
## cheat <- read.csv("CheatModel1.csv", header = TRUE);
## cheat$SurvivedFactor = as.factor( cheat$Survived );
## cheat$SurvivedTokenMatches = 0;
## cheat$DiedTokenMatches = 0;
## cheat$SurvivedTokenDistance = 0;
## cheat$DiedTokenDistance = 0;
## resourceURL = "http://www.titanic-whitestarships.com/1st_Class_Pass.htm";
## # Example HTML:
## #<strong>Gordon, Sir Cosmo Duff<br>
## #Gordon, Lady Lucile Duff<br>
## #and Maid (Miss Laura Mabel Francatelli)<br>
## #Gracie, Colonel Archibald IV</strong><br>
## #Graham, Mr. George Edward<br>
## #Graham1, Mr. George Edward<br>
## #Graham2, Mr. George Edward<br>
## #Graham3, Mr. George Edward<br>
## #<strong>Graham, Mrs. William Thompson<br>
## #(nee Edith Junkins)<br>
## #Graham, Miss Margaret<br>
## #Greenfield, Mrs. Leo David<br>
## #(nee Blanche Strouse)<br>
## #Greenfield, Mr. William Bertram</strong><br>
## #Guggenheim, Mr. Benjamin<br>
## #and Manservant (Victor Giglio)</font></p>
## cheatSurvived = numeric(nrow(newTest));
## wd = getwd();
## localFile = file.path( wd, "TitanicResource.htm" );
## # know the file is approx 2100 lines.  Initialize each died/survived list to 3000
## died = character(3000);
## survived = character(3000);
## if ( !file.exists( localFile )){  # Do this so we can load the URL once and not spam it everytime we run this script
##   resultsHTML = getURL( resourceURL );
##   write( resultsHTML, localFile );
## }
## # now, parse the file, line-by-line.  Survivors are in bold, denoted by the <strong> tag:
## fid = file( localFile, "r");
## isStrong = FALSE;
## diedCtr = 1;
## survivedCtr = 1;
## while( TRUE ){
##   line = readLines(fid,n=1);
##   if( length(line) == 0 ){
##     break;
##   }
##   # look for </strong>
##   thisLineStartStrong = grepl( "^<strong>", line );
##   thisLineEndStrong = grepl( "</strong>", line );
##   # if line contains <strong
##   # strip html tags
##   rawText = gsub( "<.*?>", "", line );
##   # skip if no comma
##   thisLineHasComma = grepl( ",", rawText );
##   if ( !thisLineHasComma ){
##     next;
##   }
##   # remove punctuation
##   rawText = gsub( "[[:punct:]]", "", rawText );
##   # skip if line (name) is too long
##   if ( nchar( rawText) > 100 ){
##     next;
##   }
##   if( isStrong || thisLineStartStrong ){
##     survived[survivedCtr] = rawText;
##     survivedCtr = survivedCtr + 1;
##   }
##   else{
##     died[diedCtr] = rawText;
##     diedCtr = diedCtr + 1;
##   }
##   isStrong = thisLineStartStrong && !thisLineEndStrong;
## }
## close(fid);
## # truncate the died/survived lists
## survived = sort( survived[1:survivedCtr-1] );
## died = sort( died[1:diedCtr-1] );  # yes, this includes crap lines at the top of list
## #
## 
## # now have two lists and need to do bag of words searching vs. each name
## # for each name, tokenize, then query the died/survived lists
## for( i in 1:nrow( newTest ) ){
##   thisName = newTest[i,"Name"];
##   # remove non-word characters
##   thisName = gsub( "[[:punct:]]", "", thisName );
##   # now tokenize each name and search each died/survived for distance from each token
##   nameTokens = strsplit( thisName, "\\s+")[[1]];
##   numTokens = length( nameTokens );
##   # search through survived
##   survivedTokenMatches = numeric(length(survived));
##   survivedTokenDistance = numeric(length(survived));
##   for ( j in 1:length( survived ) ){
##     thisScore = 0;
##     thisSurvived = survived[j];
##     survivedTokens = strsplit( thisSurvived, "\\s+")[[1]];
##     numSurvivedTokens = length( survivedTokens );
##     # now, take each name token and search for match in survived tokens
##     for ( k in 1:numTokens){
##       thisNameToken = nameTokens[k];
##       if ( thisNameToken %in% survivedTokens ){
##         thisScore = thisScore + 1;
##       }
##     }
##     survivedTokenMatches[j] = thisScore;
##     # go through each name token and find the min distance to survived tokens
##     totalDist = 0;
##     for ( k in 1:numTokens){
##       thisNameToken = nameTokens[k];
##       wordDist = 10;
##       distances = numeric(numSurvivedTokens);
##       for ( m in 1:numSurvivedTokens){
##         thisSurvivedToken = survivedTokens[m];
##         distances[m] = adist( thisNameToken, thisSurvivedToken );
##       }
##       totalDist = totalDist + min( distances );
##     }
##     survivedTokenDistance[j] = totalDist;
##   }
##   bestSurvivedScore = max( survivedTokenMatches );
##   bestSurvivedDistance = min( survivedTokenDistance );
##   cheat[i,"SurvivedTokenHits"] = bestSurvivedScore;
##   cheat[i,"SurvivedTokenDistance"] = bestSurvivedDistance;
## 
##   # search through died
##   diedTokenMatches = numeric(length(died));
##   diedTokenDistance = numeric(length(died));
##   for ( j in 1:length( died ) ){
##     thisScore = 0;
##     thisDied = died[j];
##     diedTokens = strsplit( thisDied, "\\s+")[[1]];
##     numDiedTokens = length( diedTokens );
##     # now, take each name token and search for match in survived tokens
##     for ( k in 1:numTokens){
##       thisNameToken = nameTokens[k];
##       if ( thisNameToken %in% diedTokens ){
##         thisScore = thisScore + 1;
##       }
##     }
##     diedTokenMatches[j] = thisScore;
##     # go through each name token and find the min distance to survived tokens
##     totalDist = 0;
##     for ( k in 1:numTokens){
##       thisNameToken = nameTokens[k];
##       wordDist = 10;
##       distances = numeric(numDiedTokens);
##       for ( m in 1:numDiedTokens){
##         thisDiedToken = diedTokens[m];
##         distances[m] = adist( thisNameToken, thisDiedToken );
##       }
##       totalDist = totalDist + min( distances );
##     }
##     diedTokenDistance[j] = totalDist;
##   }
## 
##   bestDiedScore = max( diedTokenMatches );
##   bestDiedDistance = min( diedTokenDistance );
##   cheat[i,"DiedTokenHits"] = bestSurvivedScore;
##   cheat[i,"DiedTokenDistance"] = bestSurvivedDistance;
##   #cat( thisName, " bestScore = ", bestScore, ", bestMatch = ", survived[bestIdx], "\n");
## }
## 
## # now that we have some cheating scores, build a simple tree model
## library(tree);
## cheat_train = cheat[1:891,];  # use the training data
## cheat_test= cheat[892:nrow(cheat),];  # use the test data
## tree_cheat_model = tree( SurvivedFactor~SurvivorHits+VictimHits+SurvivorFirst+SurvivedTokenHits+SurvivedTokenDistance+DiedTokenHits+DiedTokenDistance,data=cheat_train );
## summary( tree_cheat_model )
## plot( tree_cheat_model )
## text( tree_cheat_model, pretty=0);
## cheat_train_pred = predict( tree_cheat_model, cheat_train, type="class");
## cheat_test_pred = predict( tree_cheat_model, cheat_test, type="class");
## # performance
## confusionMatrix(cheat_train_pred, cheat_train$Survived);
## 
## cheatModelTest = data.frame(PassengerId=newTest$PassengerId, Survived=as.numeric(cheat_test_pred)-1);
## write.csv( cheatModelTest, "CheatModelFull.csv", row.names=FALSE, quote=FALSE );
## 


## ----summary, eval=FALSE, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## library(matlab,quietly=TRUE);
## library(ROCR,quietly=TRUE);
## 
## # these csv models represent the binary 0, 1 predictions of Survival for the different models
## models = c( "BagModel.csv", "BagSigModel.csv", "BoostedTreeModel.csv", "BoostedTreeSigModel.csv", "gender_submission.csv", "KNNBest6Model.csv", "KNNBest7Model.csv", "KNNBest8Model.csv", "KNNBest9Model.csv", "KNNFullModel.csv", "LassoRegressionFullModel.csv", "LassoRegressionSigModel.csv", "LDAModel.csv", "LDAReducedModel.csv", "LogisticRegressionFullModel.csv", "LogisticRegressionSigModel.csv", "NeuralNetworkFullModel.csv", "NeuralNetworkFullModelCV.csv", "NeuralNetworkSigModel.csv", "NeuralNetworkSigModelCV.csv", "QDAModel.csv", "QDASigModel.csv", "RandomForestModel.csv", "RandomForestSigModel.csv", "RandomForestTrimmedModel.csv", "RidgeRegressionFullModel.csv", "RidgeRegressionSigModel.csv", "SVMModel.csv", "NullModel.csv", "TreeModel.csv", "TreeSigModel.csv", "WomenAndChildrenFirstModel.csv", "EnsembleModelManual.csv", "EnsembleModelMeans.csv" );
## 
## trainModels = data.frame(PassengerId=1:nrow(newTrain), Survived=newTrain$Survived );
## testModels = data.frame(PassengerId=1:nrow(newTest));
## modelNames = c();
## 
## # testAnswers is a .csv file representing all the correct predictions for the test set.
## # I created this through trial and error and some cheat models and use it here to compare
## # predictions with the answer key to show in a table (easier than hitting Kaggle every
## # time for a score calculation).  If you don't have this file, just make a default of
## # everyone dying
## testAnswers = data.frame(PassengerId=1:nrow(newTest), Survived=0);
## if( file.exists("TestAnswers.csv")){
##     testAnswers = read.csv( "TestAnswers.csv", header=TRUE );
## }
## 
## trainScores = c();
## testScores = c();
## 
## numModels = length( models );
## for ( i in 1:numModels ){
##   testFileName = models[i];
##   fp = fileparts( testFileName );
##   testName = fp$name;
##   modelNames[i] = testName;
##   trainName = paste0( testName, "Train" );
##   trainFileName = paste0( trainName, fp$ext );
##   trainModelFrame = read.csv( trainFileName );
##   testModelFrame = read.csv( testFileName );
##   trainScores[i] = mean( trainModelFrame$Survived == trainModels$Survived );
##   testScores[i] = mean( testModelFrame$Survived == testAnswers$Survived );
##   trainModelFrame$Score = trainScores[i];
##   testModelFrame$Score = testScores[i];
## }
## types = c( rep("Training Score", length(modelNames)), rep("Test Score", length(modelNames)));
## trainOrder = sort( trainScores, decreasing = TRUE, index.return = TRUE );
## # now reorder according to training score
## modelNames = modelNames[trainOrder$ix];
## trainScores = trainScores[trainOrder$ix];
## testScores = testScores[trainOrder$ix];
## scores = data.frame( Model = c(modelNames,modelNames), Accuracy = c(trainScores,testScores), Type = types );
## scores$Model = factor(scores$Model, levels=modelNames);
## scores$Type = factor(scores$Type, levels=c("Training Score", "Test Score"));
## scores$CVMethod = "-"
## if( !file.exists("ModelResults.csv")){
##     write.csv( scores, "ModelResults.csv", row.names=FALSE, quote=FALSE );
## } else{
##   # why read this in?  Because it might be annotated.  And if you're missing the TestAnswers.csv
##   # file, all the test scores will be wrong
##   scores = read.csv("ModelResults.csv", header=TRUE);
## }
## 
## ggplot( data=scores, aes(x=Model, y=Accuracy, group=Type, colour=Type)) +
##   geom_line() +
##   geom_point() +
##   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
## 
## model_results = scores;
## models = as.character( model_results$Model );
## uniqueModels= unique( as.character( model_results$Model) );
## trainingScores = sort(model_results[model_results$Type=="Training Score","Accuracy"], decreasing=TRUE, index.return=TRUE);
## trainingModels = models[trainingScores$ix];
## cvMethods = model_results[model_results$Type=="Training Score","CVMethod"];
## testScores = c();
## for ( z in 1:length(trainingModels)){
##   thisModel = trainingModels[z];
##   idxTestScore = which(( model_results$Model == thisModel ) & (model_results$Type == "Test Score") );
##   testScores[z] = model_results$Accuracy[idxTestScore];
## }
## annotatedCVMethods = cvMethods[trainingScores$ix];
## if( ! file.exists("TestAnswers.csv")){
##   testScores = c();
##   annotatedCVMethods = c();
## }
## summaryTable = data.frame( Model = trainingModels, CVMethod=annotatedCVMethods, TrainingScore = trainingScores$x, TestScore = testScores);
## knitr::kable( summaryTable, format="markdown", longtable=TRUE)

