# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

train <- read.table("../input/train.csv", 
                 header = TRUE,
                 sep = ",")
test <- read.table("../input/test.csv", 
                 header = TRUE,
                 sep = ",")

# Any results you write to the current directory are saved as output.
set.seed(1)
library(rpart)
tree <- rpart(Survived ~ Pclass+ Sex+ Age+  Parch, train, method = "class")

# Predict the values of the test set: pred
pred <- predict(tree, test, type="class")

# Construct the confusion matrix: conf
pred

library(rattle)
fancyRpartPlot(tree)
write.csv(pred)