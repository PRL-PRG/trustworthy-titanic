
library(ggplot2)
library(tidyverse)
library(knitr)
library(corrplot)
library(caret)
library(randomForest)
library(gbm)
library(vcd)
library(varSelRF)
library(ROCR)

train_data <- read.csv("../input/train.csv")
test_data <- read.csv("../input/test.csv")

str(train_data)

dim(train_data)

head(train_data, n = 10)

colSums(is.na(train_data))

summary(train_data)

train_data$Survived <- as.factor(train_data$Survived)

ggplot(data = train_data, aes(x = Survived, fill = Sex)) +
  geom_bar() +
  geom_text(aes(label = scales::percent(..count.. / sum(..count..))), stat = 'count', position = position_stack(0.5)) +
  ggtitle("Survived vs Sex") + xlab("Survived") + ylab ("Count") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

train_data$Grouping_Age <-
  cut(train_data$Age, breaks <- c(seq(0, 100, by <- 18), Inf))

ggplot(data = train_data, aes(x = Grouping_Age, y = Survived)) +
  geom_bar(aes(fill = Survived), stat = "identity") +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Survived vs Age") + xlab("Age Range") + ylab ("Survived or No Survived") +
  theme_classic() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(na.omit(train_data), aes(Survived, Age)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()

train_data$Pclass <- as.factor(train_data$Pclass)

ggplot(data = train_data, aes(x = Pclass, y = Survived)) +
  geom_bar(aes(fill = Survived), stat = "identity") +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Ticket Class vs Survived", x = "Ticket Class", y = "Survived or No Survived") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = train_data, aes(x = Embarked, fill=Survived)) +
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.5), size=3) +
  labs(title = "Survived vs Embarked", x = "Embarked", y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

d1 <- ggplot(data = train_data, aes(Parch)) +
  geom_bar(stat = "Count") +
  theme_minimal()

d2 <- ggplot(data = train_data, aes(SibSp)) +
  geom_bar(stat = "Count") +
  theme_minimal()

grid.arrange(d1, d2, nrow = 1)

#Creating Total Family
train_data$Total_Family <- train_data$SibSp + train_data$Parch + 1

train_data %>%
  group_by(Survived, Total_Family) %>%
  summarise_(n= ~n()) %>%
  mutate(prop=prop.table(n)) %>%
  kable()

#Converting factor to visualize it
train_data$Total_Family <- as.factor(train_data$Total_Family)

ggplot(data = train_data, aes(x = Grouping_Age, fill = Total_Family)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..),
    position = position_stack(vjust = 0.5), size = 2) +
  labs(title = "Age vs Total_Family", x = "Age", y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

#Convert back to numeric 
train_data$Total_Family <- as.numeric(train_data$Total_Family)

train_data$Ticket_Code <- gsub('\\s[0-9]+|[0-9]{2,}|\\.', "", train_data$Ticket)

train_data$Ticket_Number <- gsub('\\D+', '', train_data$Ticket)

#Create new table for ticket number size
Ticket_Number_Size <-
  train_data %>%
  group_by(Ticket_Number) %>%
  tally()

#Converting to Data Frame
Ticket_Number_Size <- as.data.frame(Ticket_Number_Size)

#Join both table and adding to train_data
train_data <-
  train_data %>%
  inner_join(Ticket_Number_Size, by = "Ticket_Number")

train_data <-
  rename(train_data, Ticket_Size = n)

#Add ticket number for "Line" tickets which seems empty value on the table.
train_data <-
  train_data %>%
  mutate(Ticket_Number = ifelse(Ticket_Number == "", 0000 , Ticket_Number))

#Adding Grouping Column
#0 = Non-Group Passenger
#1 = Grouping Passenger

train_data <-
  train_data %>%
  mutate(Grouping_Ticket = ifelse(Ticket_Size == 1, 0, 1))

#Converting Factor 
train_data$Grouping_Ticket <- as.factor(train_data$Grouping_Ticket)

table(train_data$Grouping_Ticket)

train_data <- 
  train_data %>%
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm=TRUE), Age))

train_data <-
  train_data %>%
  mutate(Embarked = ifelse(Embarked == "", 4 , Embarked))

train_data$Embarked <- as.factor(train_data$Embarked)

#Fare variable is continues. So, first we can check the range, distribution or etc.
range(train_data$Fare)

ggplot(data = train_data,
       aes(x = Fare, fill = Survived),
       binwidth = bwidth) +
  geom_area(stat = "bin" , binwidth = 25) +  geom_density(alpha = .2) + 
  theme_classic() + scale_fill_brewer(palette ="Paired") +
  labs(title = "Fare Density", x = "Fare", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))

# Also some passenger's ticket price is zero.

#First examine them
train_data %>%
  filter(Fare == 0)

train_data <- train_data %>%
  mutate(Categorical_Fare = cut(
    Fare,
    breaks = c(-Inf, 0, 10, 50, 100, 200, 300, 600),
    labels = c("0", "1-10", "11-50", "51 - 100", "101-200", "201-300", "500+")
  ))

train_data$Categorical_Fare <- as.factor(train_data$Categorical_Fare)

##Visualize
ggplot(data = train_data, aes(x = Categorical_Fare, y = Survived)) +
  geom_bar(aes(fill = Survived), stat = "identity") + scale_fill_brewer(palette =
                                                                          "Accent") + theme_classic() +
  labs(title = "Fare Category vs Survived", x = "Fare Category", y = "Survived or No Survived")

numeric_data <- train_data[, c(6:8, 10, 14, 17)]
corr_matrix <- cor(numeric_data, method = "pearson")

corrplot.mixed(corr_matrix, tl.cex = .6, addrect = 2)

#First some categorical parameters will be visualizing.

mosaic(
  ~ Pclass + Sex + Grouping_Ticket + Survived,
  data = train_data,
  gp = shading_hcl,
  main = "Mosaic Plot for Categorical Variables"
)

#Survived vs Pclass
table1 <- xtabs(~ Survived + Pclass, data = train_data)
assocstats(table1)

#Survived vs Sex
table2 <- xtabs(~ Survived + Sex, data = train_data)
assocstats(table2)

#Survived vs Embarked
table3 <- xtabs(~ Survived + Embarked, data = train_data)
assocstats(table3)

#Survived vs Grouping Ticket
table4 <- xtabs(~ Survived + Grouping_Ticket, data = train_data)
assocstats(table4)

#Survived vs Categorical Fare
table5 <- xtabs(~ Survived + Categorical_Fare, data = train_data)
assocstats(table5)

train_data_model <- train_data[, c(2:3, 5:6, 12, 14, 18:19)]

str(test_data)

#Convert Pclass to factor
test_data$Pclass <- as.factor(test_data$Pclass)

#Convert Embaked names same with Train Data
test_data <- test_data %>%
  mutate(Embarked = recode(Embarked, C = "2",
                          Q = "3",
                          S = "4"))

test_data %>%
  group_by(Embarked) %>%
  summarize(n = n()) %>%
  kable()

#Checking missing values
colSums(is.na(test_data))

#Age
test_data <- 
  test_data %>%
  mutate(Age = ifelse(is.na(Age), mean(Age, na.rm=TRUE), Age))

#Fare
test_data <- 
  test_data %>%
  mutate(Fare = ifelse(is.na(Fare), mean(Fare, na.rm=TRUE), Fare))

#Create total_family group for test data
test_data$Total_Family <- test_data$SibSp + test_data$Parch + 1

#Create Grouping Ticket parameter for test data

test_data$Ticket_Number <- gsub('\\D+', '', test_data$Ticket)


#Create new table for ticket number size
Ticket_Number_Size <-
  test_data %>%
  group_by(Ticket_Number) %>%
  tally()

#Converting to Data Frame
Ticket_Number_Size <- as.data.frame(Ticket_Number_Size)

#Join both table and adding to train_data
test_data <-
  test_data %>%
  inner_join(Ticket_Number_Size, by = "Ticket_Number")

test_data <-
  rename(test_data, Ticket_Size = n)

#Add ticket number for Line tickets which seems empty value on the table.
test_data <-
  test_data %>%
  mutate(Ticket_Number = ifelse(Ticket_Number == "", 0000 , Ticket_Number))

#Adding Grouping Column
#0 = Non-Group Passenger
#1 = Grouping Passenger

test_data <-
  test_data %>%
  mutate(Grouping_Ticket = ifelse(Ticket_Size == 1, 0, 1))

test_data$Grouping_Ticket <- as.factor(test_data$Grouping_Ticket)

table(test_data$Grouping_Ticket)

#Converting Fare to categorical data
test_data <- test_data %>%
  mutate(Categorical_Fare = cut(Fare, breaks = c(-Inf, 0, 10, 50, 100, 200, 300, 600), labels=c("0", "1-10", "11-50", "51 - 100","101-200", "201-300", "500+")))

test_data$Categorical_Fare <- as.factor(test_data$Categorical_Fare)   

str(test_data)

#Choosing parameters for test_data
test_data_model <- test_data[, c(2, 4:5, 11:12, 15:16)]

str(train_data_model)
str(test_data_model)

GBM_model <- gbm(Survived ~ ., 
           data = train_data_model, distribution = "multinomial",
           bag.fraction = 0.5, n.trees = 1000, interaction.depth =3, shrinkage = 0.01, n.minobsinnode = 10, cv.folds = 10)

#Visualization

par(mar = c(5, 8, 1, 1))
summary(
 GBM_model, 
  cBars = 10,
  method = relative.influence,
  las = 2)

RF_variables <- varSelRF(train_data_model[, -1], train_data_model[, 1], ntree = 1000,
                       c.sd=1, returnFirstForest = TRUE)

print(RF_variables)

train_data_model <- train_data_model[, c(1:4, 6, 8)]

trainIndex <- createDataPartition(y=train_data_model$Survived, p=0.70, list=FALSE)

train_set <- train_data_model[trainIndex,]
test_set <- train_data_model[-trainIndex,]

#10 times repeated 10 fold cross validation will be used.
set.seed(123)
trCont <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 10)

SVM_model <- train(
  Survived ~ .,
  data = train_set,
  method = "svmRadial",
  trControl = trCont
)
print(SVM_model)

#Prediction
SVM_pred <- predict(SVM_model, test_set)
#Confusion Matrix
SVM_Conf <- confusionMatrix(SVM_pred, test_set$Survived)
print(SVM_Conf)

RF_model <- randomForest(Survived ~ .,
                        data = train_set,
                        importance = TRUE,
                       ntree = 1000)
print(RF_model) 
varImpPlot(RF_model)
importance(RF_model)

##Cross validation method.

RF_model_caret <- train(
  Survived ~  .,
  data = train_set,
  method = "rf",
  trControl = trCont
)
print(RF_model_caret)

#Prediction
RF_pred <- predict(RF_model_caret, test_set)

#Confusion Matrix
RF_Conf <- confusionMatrix(RF_pred, test_set$Survived)
print(RF_Conf)

XGBM_model <- train(
  Survived ~  .,
  data = train_set,
  method = "xgbTree",
  trainControl = trCont
)
print(XGBM_model)

#Prediction
XGBM_pred <- predict(XGBM_model, test_set)

#Confusion Matrix
XGBM_Conf <- confusionMatrix(XGBM_pred, test_set$Survived)
print(XGBM_Conf)

pred_combine <- cbind(SVM_pred, RF_pred, XGBM_pred)

prediction_matrix <- prediction(pred_combine, labels = matrix(test_set$Survived, 
                                              nrow = length(test_set$Survived), ncol = 3) )

performance_matrix <- performance(prediction_matrix, "tpr", "fpr")
plot(performance_matrix, col = as.list(1:3), main = "ML-ROC Curves", 
     type = "l",xlab= "False Positive Rate", ylab="True Positive Rate")
legend(x = "bottomright", legend = c("SVM", "RF", "GBM"), fill = 1:3)

RF_pred_last <- predict(RF_model_caret, test_data)

final_file <- data_frame(PassengerId= test_data[, 1], Survived = RF_pred_last)
Output_prediction <- write.csv(final_file, file = "Prediction_Results_RF.csv", row.names = FALSE)

