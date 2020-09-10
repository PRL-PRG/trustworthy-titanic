
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.

# I like this library for Exploratory Data Analysis
library(DataExplorer)

# Import the data into a data frame
traindf <- read_csv('../input/train.csv', col_types = ('nnfcfnn??n?f'))
# I always think it's important to get a quick summary of the data
summary(traindf)

# Let's look at the first few rows of data, just to see what we're dealing with
head(traindf)

# This will give us some very basic information about the data set
plot_intro(traindf)

# Since the last plot showed that we have some missing data, let's take a better look at what that data could be.
plot_missing(traindf)

# Considering we're missing so much data from the Cabin column, I'm going to just drop it.
trimmed_df <- traindf
trimmed_df$Cabin = NULL
trimmed_df$Name = NULL
trimmed_df$Ticket = NULL
str(trimmed_df)
plot_missing(trimmed_df)

# This looks better. We still have some missing data, but it's much less than it was before. We'll figure out how to deal with the missing data going forward. 
# Some of it may just depend on whether or not we need that variable in our model
library(rpart)
library(rpart.plot)
library(C50)

# Let's just create a Logistic model first, just to see what it does. Ultimately, a decision tree will probably be better.
logit_model <- glm(Survived ~., data = trimmed_df, family = binomial("logit"))

summary(logit_model)

plot(logit_model)

logit_predictions <- predict(logit_model, newdata = trimmed_df, type = "response")
library(caret)

tree_model <- rpart(Survived ~., data = trimmed_df)
summary(tree_model)

rpart.plot(tree_model)


