# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(scales)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(mice)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

full <- bind_rows(train, test)


full$Title = gsub('(.*, )|(\\..*)', '', full$Name)

rare_title <- c('Capt','Col','Dr','Sir','the Countess','Lady','Major','','Don', 'Dona', 'Jonkheer', 'Mme', 'Rev', 'the Countness' )

full$Title <- as.character(full$Title)
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']        <- 'Miss' 
full$Title[full$Title == 'Mme']        <- 'Mrs' 
full$Title[full$Title %in% rare_title] <- 'Rare_Title'

table(full$Sex, full$Title)

full$Surname <- sapply(full$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep='_')

ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) + 
          geom_bar(stat='count', position='dodge') + 
          scale_x_continuous(breaks=c(1:11)) + 
          labs(x = 'Family Size') + 
          theme_few()

full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

mosaicplot(table(full$FsizeD, full$Survived),
           main = 'Family Size by Survived', shade = TRUE)

full$Deck <- sapply(full$Cabin, function(X) strsplit(X, NULL)[[1]][1])

embark_fare <- full %>% filter(PassengerId != 62 & PassengerId != 830)

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

full$Embarked[c(62, 830)] <- 'C'

full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

sum(is.na(full$Age))

factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

set.seed(129)

mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf')

mice_output <- complete(mice_mod)

par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

full$Age <- mice_output$Age

full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

full$Child <- factor(full$Child)
full$Mother <- factor(full$Mother)

md.pattern(full)

train <- full[1:891,]
test <- full[892:1309,]

set.seed(754)

rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child + Mother, data = train)

plot(rf_model, ylim=c(0,0.36))

legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance),
                            Importance = round(importance[, 'MeanDecreaseGini'], 2))

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

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
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)


dev.off()
