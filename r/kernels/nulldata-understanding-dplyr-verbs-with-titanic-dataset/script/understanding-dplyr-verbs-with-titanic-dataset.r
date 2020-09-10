
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(dplyr) # Loading Dplyr package

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

train <- read.csv('../input/train.csv',stringsAsFactors = F, header = T)
train #just to see if it's been loaded

train %>% count()

count(train) #Without pipe, passing the df as the first argument
train %>% count() #with pipe, more convient and more readability

select(train,Age) #without pipe 

#multicolumn selection
train %>% select(one_of('Sex','Age')) 

#multicolumn rejection

train %>% select(-one_of('Age','Sex'))

train %>% select(starts_with('P'))

train %>% select(ends_with('e'))

train %>% group_by(Sex) %>% count()

train %>% group_by(Survived, Sex) %>% count()

train %>% group_by(Sex, Survived) %>% count()

train %>% group_by(Survived) %>% summarise(mean(Age))

 #Remember we have got NAs, so mean() wouldn't work and to bypass NAs, na.rm = T must be passed. 

train %>% group_by(Survived) %>% summarise(average_age = mean(Age,na.rm=T))

train %>% mutate(Age_Bracket = ifelse(Age < 18, 'Minor','Major')) %>% select(starts_with('Age'))

#In fact this can be coupled with Survivor list to see the impact of this Age_bracket

train %>% 
mutate(Age_Bracket = ifelse(Age < 18, 'Minor','Major')) %>% 
group_by(Survived,Age_Bracket) %>% 
summarise(pnt = (n()/nrow(train))*100)


train %>% arrange(Fare) %>% tail(22) #Extracting last 22 results after sorting the fare in asending order

## Arrange in descending order

train %>% arrange(desc(Age)) %>% head(10)

train %>% 
filter(Sex == 'male') %>%
group_by(Embarked) %>%
count()

#Getting the count of everyone whose age is lesser than 18

train %>% filter(Age < 18) %>% count()

train %>% filter(grepl('wick',train$Name))


