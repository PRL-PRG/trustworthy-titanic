## ---- echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressWarnings(suppressMessages(library(tidyverse))) 
suppressWarnings(suppressMessages(library(caret))) #for machine learning
suppressWarnings(suppressMessages(library(mice))) #for imputation
suppressWarnings(suppressMessages(library(Amelia))) #for imputation
suppressWarnings(suppressMessages(library(randomForest))) #For prediction


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Use na.strings to replace blank spaces by NAs
train <- read.csv('../input/train.csv', header = TRUE, stringsAsFactors = FALSE, na.strings=c("","NA"))

test <- read.csv('../input/test.csv', header = TRUE, stringsAsFactors = FALSE, na.strings=c("","NA"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$Dataset <- "train"

test$Dataset <- "test"

full_data <- bind_rows(train, test)
glimpse(full_data)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fctr <- c("PassengerId", "Survived", "Pclass", "Sex", "Embarked", "Dataset")
full_data[fctr] <- lapply(full_data[fctr], function(x) as.factor(x))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(full_data)


## ---- fig.width=9, fig.height=8--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#grep titles and add it to another variable
full_data$Title <- gsub("(.*, )|(\\..*)", "", full_data$Name)

#Plot titles
ggplot(full_data, aes(Title)) + geom_bar() + ylab("Number of times a title appears in the dataset") + xlab("Title") + ggtitle("Titles in Titanic data")

#Show titles by sex
table(full_data$Sex, full_data$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Combining less frequent titles
others <- c("Jonkheer", "Sir", "Major", "Dr", "Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Rev")

#Reassign 
full_data$Title[full_data$Title == "Mme"]    <- "Mrs"
full_data$Title[full_data$Title == "Ms"]     <- "Miss"
full_data$Title[full_data$Title == "Mlle"]   <- "Miss"
full_data$Title[full_data$Title %in% others] <- "Other"

#Show titles by sex again
table(full_data$Sex, full_data$Title)

#Convert to factor
full_data$Title <- as.factor(full_data$Title)


## ---- fig.width=9, fig.height=8--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missmap(full_data, col=c("wheat", "darkred"), y.cex = 0.4, x.cex = 0.8, legend = TRUE, rank.order = TRUE)


## ----warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_data %>% filter(is.na(Fare))


## ----echo=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full_data[full_data$Pclass == '3' & full_data$Embarked == 'S', ], 
  aes(x = Fare)) +
  geom_density(fill = 'lightblue', alpha = 0.5) + 
  geom_vline(aes(xintercept = median(Fare, na.rm = TRUE)),
    colour = 'red', linetype = 'dashed', lwd = 1) +
  geom_text(aes(x=median(Fare, na.rm = TRUE), y=-0.005, label=round(median(Fare, na.rm = TRUE),       digits = 2)), hjust=-0.3, size=6)  +    
  scale_x_continuous() +
  ggtitle(label = "Median for passengers who embarked \n in Southampton as 3rd class") +
  theme(panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold", size = 15))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_data$Fare[full_data$PassengerId[1044]] <- 8.05


## ----warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_data %>% filter(is.na(Embarked))


## ----warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_data %>%
  group_by(Embarked, Pclass) %>%
  filter(Pclass == "1") %>%
  summarise(median = median(Fare))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_data$Embarked[full_data$PassengerId[c(62, 830)]] <- "C"


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#set seed
set.seed(123)

#Perform MICE imputation, keeping only useful variables
full_data_mice <- mice(full_data[, !names(full_data) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method = "rf")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create a density plot to compare the estimate values and the original ones.
densityplot(full_data_mice)


## ----warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Save the complete output
mice_output <- complete(full_data_mice)

#Create a chart comparing the original data and the new imputed data
plot <- ggplot() + 
geom_density(aes(x=full_data$Age, y=..density..), fill = "red", alpha=.5) + geom_density(aes(x=mice_output$Age, y=..density..), fill = "yellow",alpha=.5) + theme(panel.background = element_rect(fill = 'white', colour = 'white')) + ggtitle("Comparison between original and imputed data") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Age") + ylab("Density")

plot


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_data$Age <- mice_output$Age


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(is.na(full_data$Age))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- full_data %>% filter(Dataset=="train") %>% select(everything(), -Dataset)
test <- full_data %>% filter(Dataset=="test") %>% select(everything(), -Dataset)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)

rf_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data=train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(train$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dotplot(rf_model$importance)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

