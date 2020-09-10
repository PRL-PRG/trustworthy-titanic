
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(ggplot2)
library(dplyr, pos = 2)
library(tree)
library(mice)
library(randomForest)
library(ggthemes)

DATE <- format(Sys.time(),"%Y%m%d%H%M")

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test  <- read.csv("../input/test.csv", stringsAsFactors = FALSE)

full <- bind_rows(train,test)

summary(full)
str(full)



# Imputing Missing Values,..
factor_vars <- c("PassengerId","Pclass","Sex","Ticket","Cabin","Embarked")
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(123)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf')
mice_output <- complete(mice_mod)
full$Age <- mice_output$Age
full$Fare <- mice_output$Fare

md.pattern(full)

train <- full[1:891,]
test <- full[892:1309,]

# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                         data = train)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = paste0('titanic_rf_',DATE,'.csv'), row.names = F)
