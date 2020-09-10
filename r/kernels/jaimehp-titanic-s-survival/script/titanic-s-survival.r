
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

library(xgboost)
library(Matrix)
library(readr)
library(stringr)
library(lattice)
library(caret)

df_all <- read.csv('../input/train.csv')
dim(df_all)
head(df_all)

df_val <- read.csv('../input/test.csv')
dim(df_val)
head(df_val)

random <- runif(nrow(df_all))
df_all$train <- as.numeric(random < 0.7)

df_train <- df_all[df_all$train==1,-1]
dim(df_train)
df_test <- df_all[df_all$train==0,-1]
dim(df_test)

sparse_matrix <- sparse.model.matrix(Survived ~ .-1, data = df_train)



df_all$train <- as.numeric(random < 0.7)
head(df_all)
