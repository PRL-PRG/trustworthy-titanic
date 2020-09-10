
# include dependencies

library(rattle)
library(rpart.plot)
library(RColorBrewer)
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # visualization
#library('mice') # imputation
#library('randomForest') # classification algorithm

# load data 
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# Generating Title
full  <- bind_rows(train, test)
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'
# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                      function(x) strsplit(x, split = '[,.]')[[1]][1])

# filling the cabin category
full$CabinCategory <- gsub('[0-9]', '', full$Cabin)
full$CabinCategory <- sapply(full$CabinCategory,  
                      function(x) strsplit(x, split = ' ')[[1]][1])

# filling the ticket number
full$TicketNumber <- gsub('[A-z]|[.]|[,]|[ ]', '', full$Ticket)
full$TicketNumber <- gsub('', '0', full$TicketNumber)
full$TicketNumber <- sapply(full$TicketNumber,  
                      function(x) as.numeric(x))

# generate input for the rpart
test_new <- full[892:1309,]
train_new <- full[1:891,]

#train_new$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
#test_new$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
write.csv(test_new, file = "test_with_title.csv", row.names = FALSE)
write.csv(train_new, file = "train_with_title.csv", row.names = FALSE)

# started based on https://campus.datacamp.com/courses/kaggle-r-tutorial-on-machine-learning/chapter-2-from-icebergs-to-trees?ex=7

# Finish the command
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title,
                      data = train_new, method = "class")

# Visualize my_tree_five
fancyRpartPlot(my_tree_five)

# Make prediction
my_prediction <- predict(my_tree_five, test_new, type = "class")

# Make results ready for submission
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution0.80383.csv", row.names = FALSE)


my_tree_six <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + CabinCategory + TicketNumber,
                      data = train_new, method = "class")

# Visualize my_tree_six
fancyRpartPlot(my_tree_six)

# Make prediction
my_prediction <- predict(my_tree_six, test_new, type = "class")

# Make results ready for submission
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution_new.csv", row.names = FALSE)
