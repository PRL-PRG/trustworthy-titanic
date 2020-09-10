
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(dplyr) # data manipulation



# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test  <- read.csv("../input/test.csv", stringsAsFactors= F)

#str(train)
#str(test)

#Extracting subinformations from Name

table(test$Name, test$Sex)#Prints names of passengers encoded in the "test" file 
 


#together <- bind_rows(train, test)
#str(together)