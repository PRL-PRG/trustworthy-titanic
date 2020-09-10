
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

full <- bind_rows(train, test)
str(full)
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

rare_title <- c('Capt', 'Col', 'Don', 'Dona', 'Dr', 'Jonkheer',
                'Lady', 'Major', 'Rev', 'Sir', 'the Countess')

full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms'] <- 'Miss'
full$Title[full$Title == 'Mme'] <- 'Mrs'
full$Title[full$Title %in% rare_title] <- 'Rare Title'

# Show title counts by sex
table(full$Sex, full$Title)

# We can inspect the train data. The results of this are printed in the log tab below
#summary(train)

# Here we will plot the passenger survival by class
#train$Survived <- factor(train$Survived, levels=c(1,0))
#levels(train$Survived) <- c("Survived", "Died")
#train$Pclass <- as.factor(train$Pclass)
#levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

#png("1_survival_by_class.png", width=800, height=600)
#mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
#           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
#           off=c(0), cex.axis=1.4)
#dev.off()
