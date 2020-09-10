


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

train <- list.files(path = "../input/Titanic:Machine Learning from Disaster/train")
test <- list.files(path = "../input/Titanic:Machine Learning from Disaster/test")




# combining the datasets: DPLYR - bind_rows
combined <- bind_rows(train, test)

# Univariate Analysis ####
# Age
hist(combined$Age)
summary(combined$Age) # Min is 17 Months Baby and Max is 80 Years.

# Survived Column
table(train$Survived)
round(prop.table(table(train$Survived))*100,2) # convert the data in %age
# 62% People Died and ~38% people survived

# Create a Histogram of Age basis Survived Column
# People who Survived or Died in terms of Age.
p1 <- ggplot(train, aes(Age, fill=factor(Survived)))+geom_histogram(bins = 30)+
  labs(title="Titanic: Survived Basis Age")

# Gender Wise Survival
p2 <- ggplot(train, aes(Sex,fill=factor(Survived)))+
  geom_bar(stat = "count", position = "dodge")+
  labs(title="Titanic: Survived Basis Gender")

# Age Vs S & Gender Vs S
# Combine all these 03...Age Gender and Survived
p3 <- ggplot(train, aes(Age, fill=factor(Survived)))+geom_histogram(bins = 30)+
  xlab("Age")+ylab("Count")+facet_grid(.~Sex)+ggtitle("Survived ~ Age Vs Sex")

library(cowplot)
plot1 <- plot_grid(p1,p2, nrow = 1)
plot_grid(p3, plot1, ncol = 1)

# Family (SIBSP + PARCH)
combined$Family <- combined$SibSp+combined$Parch+1 # 1 is added to signify the people travel Alone.

# Large Families Sink Together whereas Lone Travellers or Small Families have better survival chances.
combined$Family==1 # if family equates to 01 - Singles else if the family >1 <5 - Small else large
combined$family_cat[combined$Family==1] <- "Singles" # Case 01
combined$family_cat[combined$Family>1 & combined$Family<5] <- "Small"
combined$family_cat[combined$Family>4] <- "Large"
table(combined$family_cat)
combined$family_cat <- factor(combined$family_cat)


# Name of the Passengers - Extract the Titles and create a new col of Titles
?strsplit
sapply(strsplit(combined$Name, split = ","), head,1)
firstname <- sapply(strsplit(combined$Name, split = ","), tail,1)
titles <- sapply(strsplit(firstname, split = ". "), head,1)
table(titles)

# Create Column of Titles
combined <- mutate(combined, titles)

# We will put Others for the rest of the names
Title.Ignore <- c(" Capt"," Col", " Don", " Dona", " Dr", " Jonkheer", " Lady",
                  " Major", " Mlle", " Mme", " Rev", " Sir", " th")

# To Convert these titles into others
combined$titles[combined$titles %in% Title.Ignore] <- "Others"
combined$titles[combined$titles==" Ms"] <- " Miss"
str(combined$titles)
combined$titles <- factor(combined$titles)
table(combined$titles)

# Split the Data
train_new <- combined[1:891,]
test_new <- combined[892:1309,]
test_new$Survived <- NULL # Removing Survived Col

# Missing Values - na.roughfix
train_new$Name <- factor(train_new$Name)
train_new$Ticket <- factor(train_new$Ticket)
train_new$Cabin <- factor(train_new$Cabin)

library(randomForest)
sum(is.na(train_new))
train_new <- na.roughfix(train_new)

str(test_new)
test_new$Name <- factor(test_new$Name)
test_new$Ticket <- factor(test_new$Ticket)
test_new$Cabin <- factor(test_new$Cabin)
sum(is.na(test_new))
test_new <- na.roughfix(test_new)

# time to make a decision tree model and do some predictions

library(rpart)
titanic.dtree <- rpart(Survived~Pclass+Sex+Age+SibSp+Parch+
                         Fare+Embarked+Family+family_cat+titles,
                       data = train_new, method = "class")
library(partykit)
plot(as.party(titanic.dtree))

# Prediction 
titanic.predict <- predict(titanic.dtree, test_new, type = "class")

solution <- data.frame(PassengerId=test_new$PassengerId, 
                       Survived=titanic.predict)
write.csv(solution, file = "Titanic DTREE Submission.csv", row.names = F)


