
library(dplyr)            # For data frame manipulation and the %>%  pipe
library(ggplot2)          # For visualizations
library(gridExtra)        # For plotting multiple plots
library(mice)             # multivariate imputation by chained equations
library(randomForest)     # random forest model

train<-read.table("../input/train.csv",sep=",",header=TRUE) 
test<-read.table("../input/test.csv",sep=",",header=TRUE)
full  <- bind_rows(train, test) # bind training & test data

str(train, give.attr = FALSE)

#stat descriptive

#survived
group_colors <- c("0" = "tomato", "1" = "limegreen", "male" = "skyblue", "female" = "pink")
train$Survived <- factor(train$Survived)
ggplot (train, aes(x = Survived)) + 
  geom_bar(fill = c("0" = "tomato", "1" = "limegreen")) +
  labs(title = "Survival on the Titanic", x = "Survival", y = "Number of Passengers")

#age and gender
ggplot(train, aes(x = Sex)) + 
  geom_bar(aes(fill = Survived), position = "fill") +
  scale_fill_manual(values = group_colors) +
  labs(title = "Survival by Sex", x = "Sex", y = "Proportion of Passengers")

ggplot(train, aes(x = Age)) + 
  geom_histogram(aes(fill = Sex), binwidth = 2) +
  scale_fill_manual(values = group_colors) +
  labs(title = "Distribution of Passenger Age by Sex", x = "Age", y = "Number of Passengers")

ggplot (train, aes(x = Age)) + 
  geom_histogram(aes(fill = Survived), binwidth = 2) +
  scale_fill_manual(values = group_colors) +
  labs(title = "Distribution of Passenger Age by Survival", x = "Age", y = "Number of Passengers")


ggplot (train, aes(x = Pclass)) + 
  geom_bar(aes(fill = Survived)) +
  scale_fill_manual(values = group_colors) +
  labs(title = "Distribution of Passenger Class by Survival", x = "Passenger Class", y = "Number of Passengers")

ggplot (train, aes(x = Fare)) + 
  geom_histogram(aes(fill = Survived), binwidth = 10) +
  scale_fill_manual(values = group_colors) +
  labs(title = "Distribution of Journey Fare by Survival", x = "Fare Paid", y = "Number of Passengers")

ggplot (train, aes(x = Embarked)) + 
  geom_bar(aes(fill = Survived)) +
  scale_fill_manual(values = group_colors) +
  labs(title = "Distribution of Journey Origin by Survival", x = "Origin (port of embarkment)", y = "Number of Passengers")



#analyse des donnnées 

test$Survived <- NA                 # Adding the missing varriable to the test set
combined <- bind_rows(train, test)  # For performing feature engineering on the entire data set

str(combined, give.attr = FALSE)

#Quels sont les données manquantes ? 
sapply(combined, function(x) sum(is.na(x)))

combined$Survived <- factor(combined$Survived)
combined$Pclass <- factor(combined$Pclass)
combined$Sex <- factor(combined$Sex)
combined$Embarked <- factor(combined$Embarked)
str(combined, give.attr = FALSE)

#imputation des données manquantes 
set.seed(1234)    # set seed for reproduceible results
imputes <- mice(combined[c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")], method = "rf") # imputed using random forest methods
imputes_output <- complete(imputes)

#vérification qualité imputation
impute_age <- ggplot(imputes_output, aes(x = Age)) + 
  geom_histogram(binwidth = 2, fill = "thistle") +
  labs(x = "Imputed Age")
age <- ggplot(train, aes(x = Age)) + geom_histogram(binwidth = 2)
grid.arrange(age, impute_age, ncol = 2)

impute_embarked <- ggplot(imputes_output, aes(x = Embarked)) + 
  geom_bar(fill = "thistle") +
  labs(x = "Imputed Origin")
embarked <- ggplot(train, aes(x = Embarked)) + geom_bar() + labs(x = "Origin")
grid.arrange(embarked, impute_embarked, ncol = 2)


combined$Age <- imputes_output$Age
combined$Fare <- imputes_output$Fare
combined$Embarked <- imputes_output$Embarked

sapply(combined, function(x) sum(is.na(x)))


#création nouvelle variable avec genre donné par le nom des passagers
combined$Title <- factor(gsub('(.*, )|(\\..*)', '', combined$Name))
table(combined$Title)

#Nouvelle variable en combinant  Sibsp et Parch pour avoir une variable famille
combined$FamSize <- combined$SibSp + combined$Parch + 1 
ggplot(combined, aes(x = FamSize)) + 
  geom_bar() +
  labs(x = "Family Size", y = "Number of Passengers", title = "Family Size of Passengers")

#Nouvelle vraiable enfant
combined$child <- NA
combined$child[combined$Age <= 16] <- TRUE
combined$child[combined$Age > 16] <- FALSE
str(combined, give.attr = FALSE)


#Taille de la famille
combined$FamilySize<-combined$Parch+combined$SibSp+1

#Mère ? 
combined$Mother<-0
combined$Mother[combined$Sex=='female' & combined$Parch>0 & combined$Age>18 & combined$Title!='Miss']<-1

#Enfant ? 
combined$Child<-0
combined$Child[combined$Parch>0 & combined$Age<=12]<-1

#Maintenant que l'apprentissage et le travail préliminaire est fait, on resépare nos bases : 
train <- combined[1:891,]
test <- combined[892:1309,]


#Random Forest 

rf_titanic <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamSize + child, data = train, ntree = 1000)
rf_titanic

plot(rf_titanic)

#importance des valeurs dans notre modèle
vimp <- importance(rf_titanic)
vimp_df <- data.frame(Var = row.names(vimp), vimp)
vimp_df %>% arrange(desc(MeanDecreaseGini))


predicted <- predict(rf_titanic, newdata = test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = predicted)
write.csv(solution, "../input/gender_submission.csv", row.names = FALSE)


