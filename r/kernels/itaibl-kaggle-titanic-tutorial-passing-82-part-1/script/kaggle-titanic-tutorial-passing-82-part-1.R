# Hi everyone, this and the following kernels describe in details all steps required (feature engineering and models) 
# in order to obtain a 82% score on the Titanic competition.


#  ======>>>>>>  FOR NOT WASTING YOUR TIME - THE FEATURES ARE LISTED HERE BELOW.
#  ======>>>>>>  PLEASE VOTE UP TO KEEP THIS POST ALIVE.


# The baseline for this data is 61.9%,  obtainable by predicteing Survived=0 for all test cases.
# Just by adding Gender, you can get to 76%.
# Additional parameters like Title (extracted from name), Age (imputed by median) and class (1/2/3 for the ship's classes)  get you to 78%.

# The following features need to be built in order to get to 82%:
# ===============================================================

# FEATURES 1+2. Name length, SYLLABLES: plotting name length (simple nchar) by the Survived feature will show that
# people with longer names have bettter survival chances.
# Since women have significantly longer names (combining family names etc.),
# it's easy to mistake this feature to be correlative to the Gender feature.
# When filtering Men only, the difference is still significant.
# Further filtering by 3rd class only still shows this difference. 
# This maybe explained by several behavioristic features, I will add references at the last kernel.
# I have also added syllable count for each name.
# 
# FEATURES 3+4. SAME FAMILY indicator, SAME TICKET : There are many cases in which same family members baught a ticket in seperate,
# but the ticket numbers are still close. For example, the KINK family baught three seats under ticket 315153,
# and addtional two tickets under 315152 and 315151. This needs to be processed as a feature in itself - 
# large familes had worse survival chances. Note there are two featuers here, one having the number of ppl
# with the exact same ticket number, the other with the number of ppl with approximate number.
#
# FEATURE 6. ADJUSTED FARE: the price (Fare) feature sometimes indicates the total price for several tickets.
# for example, ticket 12749 (Mr and Mrs hays) is registered under 281$ twice, so adjusted fare should be half.
# Remember to remove outliers and to center/scale/BoxCox this feature.
# This may be an informative behavioristic feature as well, within each class, the prices of tickets may 
# indicate ones ability to negotiate, which may come handy when attempting to get a free spot on life-boats...
#
# FEATURES 7. CABIN PREFIX: you don't want to overfit and have the exact cabin number. You DO want just the first letter,
# which is insicating teh deck, and the count of cabins, which usually suggest staff/helpers.
#
# FEATURES 8+9. LOAD and PERSONAL LOAD.
# I assumed kids under 14 and adults above 65 has a certain "load" for survival.
# therefor, familes have aggregate "load" for all family members, and each member will have a personal load:

  #simply bind the family/ticket sets, take the max load, and aggregate across family:
  
  #dfLoad = rbind(dfSameTicket, dfSameFamily)
  #dfLoad$diff1 = 14 - dfLoad$Age
  #dfLoad$diff2 = dfLoad$Age - 65
  #...
  #dfLoad[nr,"diff3"] = max(0,dfLoad[nr,"diff1"], dfLoad[nr,"diff2"])
  #...
  #df[x,"load"] = sum(dfLoad$diff3)

library(dplyr)  
library(ggplot2)
library(readr) 
library("beanplot")

df <- read.csv("../input/train.csv", header=TRUE, as.is=TRUE, stringsAsFactors = T )

# ============== FEATURE 1


df$name_length = nchar(df$Name)
boxplot(df$name_length~df$Survived, main = "Name length by Survival")
beanplot(df$name_length~df$Survived , main = "Name length by Survival")

dfMen = df[df$Sex == "male",]
boxplot(dfMen$name_length~dfMen$Survived, main = "Name length by Survival, just men")
beanplot(dfMen$name_length~dfMen$Survived, main = "Name length by Survival, just men")

dfMen3rd = dfMen[dfMen$Pclass == 3,]
boxplot(dfMen$name_length~dfMen$Survived ,main = "Name length by Survival, just men, 3rd class")
beanplot(dfMen$name_length~dfMen$Survived,main = "Name length by Survival, just men, 3rd class")

# ============== FEATURE 2

df <- as.data.frame(sapply(df, tolower))
df$Name = gsub('\\-', ' ',  df$Name)

getSurName = function(fullname)
{
  fullname = paste0(fullname,",")
  v = unlist(strsplit(fullname, split = '[,. ]'))
  return(v[1])
}

df$Surname <- lapply( df$Name,getSurName)
df$Surname<- as.character(df$Surname)

dfSameFamily = df[df$Surname=="kink",]
#dfSameFamily$delta = abs(dfSameFamily$TicketNum-INSERT_TICKET_HERE)
#dfSameFamily = dfSameFamily[ dfSameFamily$delta<6,]

print(nrow(dfSameFamily))

# additional family members appear in the test file
df <- read.csv("../input/test.csv", header=TRUE, as.is=TRUE, stringsAsFactors = T )

df <- as.data.frame(sapply(df, tolower))
df$Name = gsub('\\-', ' ',  df$Name)

getSurName = function(fullname)
{
  fullname = paste0(fullname,",")
  v = unlist(strsplit(fullname, split = '[,. ]'))
  return(v[1])
}

df$Surname <- lapply( df$Name,getSurName)
df$Surname<- as.character(df$Surname)

dfSameFamily = df[df$Surname=="kink",]
#dfSameFamily$delta = abs(dfSameFamily$TicketNum-INSERT_TICKET_HERE)
#dfSameFamily = dfSameFamily[ dfSameFamily$delta<6,]

print(nrow(dfSameFamily))


# TOTAL FAMILY MEMBERS:
#ID, NAME, TICKET
#70                            kink, mr. vincenz 315151
#185         kink heilmann, miss. luise gretchen 315153
#1057 kink heilmann, mrs. anton (luise heilmann) 315153
#1268                          kink, miss. maria 315152
#1286                   kink heilmann, mr. anton 315153

df$Fare = as.numeric(df$Fare)
df[is.na(df$Fare),"Fare"] = mean(df$Fare, na.rm = T)

removeOutliers <- function(x){
  quantiles <- quantile( x, c(.05, .95 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  return (x)
}
scale_this <- function(x) as.vector(scale(x, center = T, scale = T))
df  =   df %>%  group_by(Pclass) %>%  mutate(Fare = removeOutliers(Fare))
