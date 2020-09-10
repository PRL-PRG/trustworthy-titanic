
library("readr")
library("ggplot2")
library("randomForest")
library("tibble")
library("stringr")
library("dplyr")
library("truncnorm")

train_df <- read_csv("../input/train.csv")
test_df <- read_csv("../input/test.csv")

#-1 used in Survived column for test_df, bind train_df after test_df
combined_df <- test_df %>%
  add_column(Survived = -1.0, .after = "PassengerId") %>% 
  rbind(train_df)

# extract title from name for both dfs
combined_df$Title <- str_extract(combined_df$Name, pattern = "([A-Za-z]+)\\.")

# making titles more consistent
combined_df$Title[combined_df$Title %in% c("Mlle.","Ms.")] <- "Miss."
combined_df$Title[combined_df$Title %in% c("Mme.")] <- "Mrs."
combined_df$Title <- str_replace(combined_df$Title, c("^((?!Master.|Miss.|Mr.|Mrs.).)*$"), "Rare.")

# # remove ticket, cabin, and Name columns on both dfs
combined_df <- subset(combined_df, select = -c(Ticket, Cabin, Name))

#calculate age data in train_df; consider using both data set
age_summ <- train_df %>% 
  group_by(Pclass, Sex) %>% 
  summarize(mean = mean(Age, na.rm = TRUE), sd = sd(Age, na.rm = TRUE), 
            min = min(Age, na.rm = TRUE), max = max(Age, na.rm = TRUE))

#calculate the number of NAs with Age for each group
num_NA <- combined_df %>%
  group_by(Pclass, Sex) %>%
  summarize(count = sum(is.na(Age)))

#replce NA values within each group with a random number generated from a truncated normal 
#distribution based on age_summ

k <- 1
# i is Pclass, j is sex 
for (i in 1:3)
{
  for (j in c("female","male")){
    combined_df$Age[combined_df$Pclass == i & 
                    combined_df$Sex == j & 
                    is.na(combined_df$Age)] <- rtruncnorm(num_NA[[k,3]], a = age_summ[[k,5]], 
                                                          b= age_summ[[k,6]], mean = age_summ[[k,3]], 
                                                          sd = age_summ[[k,4]])
    k <- k+1
  }
}

# bin age into 5 bins
combined_df$Age[combined_df$Age <= 16] <- 0
combined_df$Age[combined_df$Age > 16 & combined_df$Age <= 32] <- 1
combined_df$Age[combined_df$Age > 32 & combined_df$Age <= 48] <- 2
combined_df$Age[combined_df$Age > 48 & combined_df$Age <= 64] <- 3
combined_df$Age[combined_df$Age > 64] <- 4

# bin family size into 3 bins
combined_df$FamilySize <- combined_df$Parch + combined_df$SibSp + 1

combined_df$FamilySize[combined_df$FamilySize <= 1] <- 0
combined_df$FamilySize[combined_df$FamilySize > 1 & combined_df$FamilySize <= 2] <- 1
combined_df$FamilySize[combined_df$FamilySize > 2] <- 2


# drop Parch and SibSp columns
combined_df <- subset(combined_df, select = -c(Parch, SibSp))

#Replace NA for embarked with most common occurrence 
combined_df[419:1309,][is.na(combined_df$Embarked[419:1309]),]$Embarked <- "S"

#convert categorical variables to numerical
combined_df$Title <- as.numeric(plyr::mapvalues(combined_df$Title, from = c("Mr.", "Miss.", "Mrs.", "Master.", "Rare."), to = c(1,2,3,4,5)))
combined_df$Sex <- as.numeric(plyr::mapvalues(combined_df$Sex, from = c("female", "male"), to = c(1,0)))
combined_df$Embarked <- as.numeric(plyr::mapvalues(combined_df$Embarked, from = c("S", "C", "Q"), to = c(0,1,2)))

#use median Pclass = 3 fare for missing value in test data set
combined_df[1:418,][is.na(combined_df$Fare[1:418]),]$Fare <- median(combined_df[419:1309,][combined_df$Pclass[419:1309] == 3,]$Fare, na.rm = TRUE)

# create fare bands
combined_df$Fare[combined_df$Fare <= 7.91] <- 0
combined_df$Fare[combined_df$Fare > 7.91 & combined_df$Fare <= 14.45] <- 1
combined_df$Fare[combined_df$Fare > 14.45 & combined_df$Fare <= 31] <- 2
combined_df$Fare[combined_df$Fare > 31] <- 3

#run randomForest
set.seed(415)

train_df_Final <- subset(combined_df[419:1309,], select = -c(PassengerId))
test_df_Final <- subset(combined_df[1:418,], select = -c(Survived))

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare +
                      Embarked + Title + FamilySize,
                    data=train_df_Final, 
                    importance=TRUE, 
                    ntree=2000)

prediction <- predict(fit, test_df_Final)
submit <- data.frame(PassengerId = test_df_Final$PassengerId, Survived = prediction)

write.csv(submit, file = "submit.csv", row.names = FALSE)



