
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions
library(dplyr)
library(ggplot2)
library(tibble)
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

train <- read.csv("../input/train.csv")

head(train)

str(train)

train <- as.tibble(train)

test <- read.csv("../input/test.csv")

name <-train %>% select(Name,SibSp)%>%group_by(SibSp)%>%arrange()



tckt_fare <- train %>% select(Ticket,Fare) %>% arrange()%>%group_by(Ticket)
count(tckt_fare)


tckt_fare <- tckt_fare%>%distinct(Ticket)
tckt_fare
dim(tckt_fare)

nm_cls <- train %>% select(Name,Pclass)%>%unite(sep = " ")%>%arrange()
nm_cls
