
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 
suppressMessages(library(tidyverse))
library(tidyverse)
library(dplyr)
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(stringr)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

list.files("../input")

# Any results you write to the current directory are saved as output.

df_training <- read_csv('../input/train.csv')
df_testing  <- read_csv('../input/test.csv')

df  <- bind_rows(df_training, df_testing)

glimpse(df)



head(df)

survival_table <- df %>% 
    filter(is.finite(Survived)) %>%
    group_by(Pclass, Survived) %>% 
    summarise(n_group = n()) %>%
    group_by(Pclass) %>%    # strictly speaking this line is redundant but it's clearer to understand this way
    mutate(n_class_total = sum(n_group)) %>%
    mutate(percent_of_class = n_group / n_class_total)

survival_table

# Another way to show the same date:
# 'pivot' the table so we see survival status as column headings
survival_table %>%
    select(one_of(c('Pclass','Survived','percent_of_class'))) %>%
    spread("Survived","percent_of_class")

# A further way to show the survival rates. Using ggplot2, we can 
# create a 'heatmap' from coloured tiles, and put labels on to show the values:
survival_table %>%
    ggplot(aes(x=factor(Survived), y = factor(Pclass),   # 'factor' is just to get the scales looking nice
               fill = percent_of_class)) +
    geom_tile() +   
    coord_equal()  + # this just makes all the tiles square instead of rectangular
    labs(title="Survival Rates on RMS Titanic", subtitle="by passenger class, ignoring unknown cases",
         x="Survival Status", y = "Passenger Class") +
    geom_text(aes(label= percent_of_class), colour="white") +
    geom_text(aes(label= sprintf("%1.1f%%",      # 'sprintf...' is to format the percentages in a legible way
                                 100*percent_of_class)), 
              nudge_y = -0.2, colour="pink")


# first, create a couple of new variables (columns)
# variable 'deck' using the first character in the Cabin number column
df <- df %>% 
    mutate(deck = str_sub(Cabin, 1,1))

# a 'child' variable. Note we create this as a 'factor', this makes it easier to read later
df <- df %>%
    mutate(ac = Age>=16, adult_child = factor(Age>=16, labels=c('child','adult')))

# make the 'embarked' variable more readable, and put them in the correct order of the voyage
# recall, C = Cherbourg, Q = Queenstown, S = Southampton
df <- df %>%
    mutate(embarked_name = factor(Embarked, 
                                  levels=c('S','C','Q'), 
                                  labels=c('Southampton','Cherbourg','Queenstown')))

head(df)


# now, get a general view of the distribution of ticket prices
# note, the y-axis is meaningless, it's just there so that we can 'jitter'
# the individual points to make them a bit more legible
df %>% 
    filter(Fare>0) %>%
    ggplot(aes(x=Fare, y=0)) +
    geom_jitter(aes(colour=factor(Pclass)))
    

# What can we do to make this picture clearer to understand?
# Firstly, let's use a 'log scale' to make the low fares more visible
df %>% 
    filter(Fare>0) %>%
    ggplot(aes(x=Fare, y=0)) +
    geom_jitter(aes(colour=factor(Pclass))) +
    scale_x_continuous(trans='log2')

# next, we can use a 'density' graph. This shows roughly the distribution
# of number of passengers at each level of fare
df %>% 
    filter(Fare>0) %>%
    mutate(class = factor(Pclass)) %>%
    ggplot(aes(x=Fare)) +
    geom_density(aes(group=class, colour=class, fill=class, alpha=0.15)) +
    scale_x_continuous(trans='log2')

df %>% 
    filter(Fare>0) %>%
    mutate(class = factor(Pclass)) %>%
    ggplot(aes(x=Fare)) +
    geom_density(aes(group=class, colour=class, fill=class, alpha=0.15)) +
    scale_x_continuous(trans='log2') +
    facet_wrap(~deck)

df %>% 
    filter(Fare>0 & !is.na(embarked_name)) %>%
    mutate(class = factor(Pclass)) %>%
    ggplot(aes(x=Fare)) +
    geom_density(aes(group=class, colour=class, fill=class, alpha=0.15)) +
    scale_x_continuous(trans='log2') +
    coord_cartesian(ylim=c(0,3)) +
    facet_wrap(~embarked_name) +
    theme(legend.position='top') +
    scale_alpha(guide='none') +
    labs(title="Fares on RMS Titanic", subtitle="by embarkation point")



