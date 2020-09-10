
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

# Kaggle_Titanic
# Notes on working through the analysis of survival for the Titanic data set from Kaggle 
    # THIS IS A WORK IN PROGRESS--I welcome feedback/comments on the different parts of this

# Goal: Develop a model to predict whether a person would have survived the sinking of the Titanic
    #(determine the factors that, in combination, contributed to survival)


# STEP 1: SETUP

### libraries imported
#install.packages("mice")
#install.packages("nlme")
#install.packages("readxl")
#install.packages("readr")
#install.packages("VIM")
#install.packages("epitools")

library(ggplot2)  # for data visualization
library(dplyr)    # set of data manipulation functions
library(nlme)   # linear modeling package (MAY NOT NEED THIS)
library(readxl)  # has "subset" function that allows you to generate subset table for rows that have given value in a specific column
library(readr) # CSV file I/O, e.g. the read_csv function
library(mice) # package for imputing missing data
library(VIM) # package for visualizing missing data
library(epitools)
# following https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/
    
### figure out working directory using gwtwd() if in RStudio
### set working directory using setwd()

 train <- read.csv("../input/train.csv") # imported training table and assigned it to object name "train"



### STEP 2: Explore variables (especially missing values) 
summary(train)

# Age appears to be the only variable with missing values (only var with 'NA's' in summary list)

# STEP 2a: Confirm distribution of missing data using formula from mice and VIM libraries, respectively

md.pattern(train) # checks how missing data is distributed in dataset (mice)
# output shows only data missing is from age var

miss_plot <- aggr(train, col=c('navyblue', 'red'), numbers=TRUE, sortVars=TRUE, labels=names(train), ylab=c("Histogram of missing data", "Pattern"))
# graphical way of looking at missing data - confirms "md.pattern" output (using VIM package)

# STEP 2b: impute the missing data

# First must examine whether missingness of age is MAR (missing at random) or MNAR (missing not at random)
# this website has helpful info on types of missing data: http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/mi.html
# Going to try a logistic regression with other variables to determine whether missing data show any pattern

# plot agemissing vs other variables and do some chi squared tests to see whether missing ages are more likely to show up in certain demographics
age <- train$Age # create varible for age (for ease of reference later)
age # show the dataframe to make sure variable is the thing I wanted

#Recode age variable into binary--has value (1), or not (0)
agemissing <- recode(age, .default=1, .missing=0) # create variable that partitions Age into missing and non
agemissing #makes sure recoding worked (can compare to above df to confirm)

# Are females more likely to have missing Age?
sex <- train$Sex
agemiss_sex <- ggplot(data=train, aes(sex, agemissing)) + geom_bar(stat="identity")
show(agemiss_sex) 

# more males are missing age values - but significantly so?

#chi-sq test to see whether males more likely to have missing age value
agemiss_sex_X2 <- chisq.test(sex, agemissing)
agemiss_sex_X2

# chi-sq not sig (p = 0.1187)

# Are third-class passengers more likely to have missing age?
class <- train$Pclass

agemiss_class <- ggplot(data=train, aes(class, agemissing)) + geom_jitter(data=train, aes(class, agemissing), stat="identity", group=agemissing)
show(agemiss_class)

# chart difficult to eyeball, better to rely on chisq test

#Also tried generating a clustered bar chart, but the code below doesn't successfully do that
# agemiss_class <- ggplot(train, aes(class, ..count..)) + geom_bar(aes(fill = agemissing), position = "dodge")

#chi-sq test for agemissing and class
agemiss_class_X2 <- chisq.test(class, agemissing)
agemiss_class_X2

## chi squared test IS significant, need odds table 
oddstable <- oddsratio(class, agemissing)
oddstable

# The umich website listed above suggests using a logistic regression model to predict missingness

agemissing_regr <- glm(formula = agemissing ~ class + sex + train$Survived + train$SibSp + train$Parch + train$Embarked + train$Fare)
# above model leaves out PassengerID, Ticket #, and Cabin, because those seem VERY unlikely to be responsible for why we're missing ages

summary(agemissing_regr)

imputed_train <- mice(data=train, m = 5, method = 'pmm')
summary(imputed_train)

summary(imputed_train$Age) #check whether there are missing values now (there shouldn't be)

#and add the imputed values to the dataset
train_complete = complete(imputed_train, 3) #use the third imputed dataset to complete missing values in 

### STEP 3: Examine characteristics of survivors (start with class, sex, & age, based on anecdotal reports of who survived) 
#1st class passengers, women, & children more likely to survive than other groups

survivors <- subset(train, train$Survived==1)
	# create subset table 'survivors' containing only those who survived (not strictly necessary, but simplifies subsequent lines)

survivors #show survivors table (note that all the values for "Survived" column here should be "1")

# STEP 3a: generate graph showing distributions of survivors and all passengers by sex

survsex_plot <- ggplot(survivors, aes(survivors$Sex)) + geom_bar(aes(survivors$Sex))
survsex_plot
	# creates a bar chart with count of male and female survivors
	# eyeballing shows that there were about twice as many female survivors as male

passsex_plot <- ggplot(train, aes(sex)) + geom_bar(aes(sex))
passsex_plot
    # bar chart with count of all male and female passengers 

# STEP 3b: get an actual count of male and female survivors

survsex <- survivors$Sex  
  # Generate a subset table that includes only data from the "sex" column of the 'survivors' table (may not be strictly necessary, but for the sake of neatness)

sum(survsex=="female")   # generates a sum of the rows for which sex = "female" (quotations bc string)
  # 233 female survivors 

sum(survsex=="male")   # number of male survivors
  # 109 male survivors

# STEP 3c: get total number of male and female passengers

sum(train$Sex=="female")   # 314 female passengers

sum(train$Sex=="male")   # 577 male passengers

# STEP 3d: Check whether proprtionally more females survived than males (out of total number of each)

233/314    # ~74.2% of the female passengers survived
109/577   # only ~18.9% of the male passengers survived

## INTERIM CONCLUSION: proportionally more female than male passengers survived (large different in percentages indicates that this is definitely a variable to consider in larger model)

# Step 3e: generate graph showing distributions of survivors by age

surv_age <- survivors$Age   # create variable for age (for ease of reference)
ggplot(survivors, aes(x=surv_age)) + geom_density(aes(y=..count..))   # density plot approximating count of survivors of different ages
  ## WHY IS DENSITY plot more appropriate than histogram?  Allows for more fine-grained/continuous age display 

# bimodal, skewed distribution, indicating that suvivors clustered primarily in the 20-40yo range, with second, smaller peak under 10 yo
# also note that of the 177 missing data points in Age variable, 52 of them are present in Survivors subset (meaning 125 are in the non-survived group)

# STEP 3f: determine distribution of ages of all passengers

age <- train$Age  # variable for age for table containing all passengers
ggplot(train, aes(x=age)) + geom_density(aes(y=..count..))   # plot of all passangers, by age
  ## ALL PASSENGESR BY AGE PLOT LOOKS VERY SIMILAR to survivor by age plot, but want to compare the two more directly

survpass_ageplot <- ggplot(train, aes(x=age)) + geom_density(aes(y=..count.., group=train$Survived))
survpass_ageplot
	## OVERLAYS both plots, with two lines grouped by "survivial" factor

	## THE ABOVE WORKS, but it is somewhat difficult to distinguish the two distributions (bc both same color)
	## Am trying to figure out how to use reshape package and melt function to restructure the data so I can make the two distributions different colors

##INTERIM CONCLUSION (from b/w graph): proportionally more passengers under 10 survived than individuals over 10

# STEP 3g: generate graph showing distribution of survivors by class

surv_class <- survivors$Pclass    # creates variable for class from survivors data subset 
surv_class <- ggplot(survivors, aes(surv_class)) + geom_bar(aes(surv_class))   # plots the number of survivors in a bar chart by class (1st, 2nd, 3rd)
## Class is a discrete var, so bar plot most appropriate
## More first-class passengers survived than other classes, then 3rd, then 2nd

## CHECK distribution of all passengers by class
class <- train$Pclass   # variable for class from full train dataset
pass_class <- ggplot(train, aes(allclass)) + geom_bar(aes(allclass))   # plots the number of passengers by class

## INTERIM CONCLUSION: visually comparing plots, seems proportionally more 1st class passengers survived than 3rd/2nd class passengers (could do same calculation of actual proportions as did with sex, but moving on to model now

#COULD ALSO MAYBE DO A CHI-square test, or log likelihood, but should be able to do that as part of the larger model 

## POINTS TO CONSIDER when constructing the model
	## potential sparseness of Age variable (177 passengers did not have ages listed, out of 891 total passengers--~20% of data)
	## NEED TO IMPUTE VALUES FOR THIS (examine R tutorial linked on competition page)
	## MULTICOLLINEARITY (check correlation matrix between for predictors)

### STEP 4: generate a logistic regression model of survival with age, sex, and class as predictors
survived <- train$Survived
allsex <- train$Sex  ## created two new variables referencing sex and age in the training dataset
allage <- train$Age

# model_age_sex_class <- glm(formula = survived ~ allage + allsex + allclass, data = train, family = "binomial")
# model_age_sex_class

## There will be several more models with different variables, and models will be compared according to model fit (what the best criterion to use here?  NEed to refresh knowedlge on that)

## OTHER FACTORS to consider
    ## family size, parental status, cabin (some closer to impact?), point of embarkation (not sure how theoretically this might be relevant, but tutorial considers it)       


# STEP XX: use the best fit model to predict survival on the test dataset 
