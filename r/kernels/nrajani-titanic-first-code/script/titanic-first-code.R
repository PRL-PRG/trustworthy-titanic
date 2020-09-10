
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
summary(train)
str(train)
head(train)
train$Sex <- gsub("female", 1, train$Sex)
train$Sex <- gsub("male", 0, train$Sex)
master_vector <- grep("Master.", train$Name, fixed = TRUE)
miss_vector = grep("Miss.", train$Name, fixed=TRUE)
mrs_vector = grep("Mrs.", train$Name, fixed=TRUE)
mr_vector = grep("Mr.", train$Name, fixed=TRUE)
dr_vector = grep("Dr.", train$Name, fixed=TRUE)
for (i in master_vector) {
train$Name = "Master"
}
for(i in miss_vector) {
  train$Name[i] = "Miss"
}
for(i in mrs_vector) {
  train$Name[i] = "Mrs"
}
for(i in mr_vector) {
  train$Name[i] = "Mr"
}
for(i in dr_vector) {
  train$Name[i] = "Dr"
}
master_age <- round(mean(train$Age[train$Name == "Master"], na.rm = TRUE), digits = 2)
miss_age <- round(mean(train$Age[train$Name == "Miss"], na.rm = TRUE), digits = 2)
mrs_age <- round(mean(train$Age[train$Name == "Mrs"], na.rm = TRUE), digits = 2)
mr_age <- round(mean(train$Age[train$Name == "Mr"], na.rm = TRUE), digits = 2)
dr_age <- round(mean(train$Age[train$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(train)) {
  if (is.na(train[i,5])) {
    if (train$Name[i] == "Master") {
      train$Age[i] = master_age
    } else if (train$Name[i] == "Miss") {
      train$Age[i] = miss_age
    } else if (train$Name[i] == "Mrs") {
      train$Age[i] = mrs_age
    } else if (train$Name[i] == "Mr") {
      train$Age[i] = mr_age
    } else if (train$Name[i] == "Dr") {
      train$Age[i] = dr_age
    } else {
      print("Uncaught Title")
    }
  }
}
for(i in 1:nrow(train)) {
  x = train$SibSp[i]
  y = train$Parch[i]
  train$Family[i] = x + y + 1
}
for(i in 1:nrow(train)) {
  if(train$Name[i] == "Mrs" & train$Parch[i] > 0) {
    train$Mother[i] = 1
  } else {
    train$Mother[i] = 2
  }
}