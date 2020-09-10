library('ggplot2') 
library('ggthemes') 
library('scales') 
library('dplyr') 
library('mice') 
library('randomForest') 

train<- read.csv("train.csv", stringsAsFactors = F)
test<- read.csv("test.csv", stringsAsFactors = F)
complete<- bind_rows(train, test)

#to extract the title of the passeengers
complete$Title<- gsub('(.*,)|(\\..*)','',complete$Name)

rare_title<- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don','Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

complete$Title[complete$Title == 'Mlle']<- 'Miss' 
complete$Title[complete$Title == 'Ms']<- 'Miss'
complete$Title[complete$Title == 'Mme']<- 'Mrs' 
complete$Title[complete$Title %in% rare_title]<- 'Rare Title'

#extract family name
complete$Surname <- sapply(complete$Name,function(x) strsplit(x, split = '[,.]')[[1]][1])

complete$Fsize <- complete$SibSp + complete$Parch + 1


complete$Family <- paste(complete$Surname, complete$Fsize, sep='_')


ggplot(complete, aes(x = Fsize, fill = factor(Survived))) + 
geom_bar(stat='count', position='dodge') + 
scale_x_continuous(breaks=c(1:11)) +
labs(x = 'Family Size') +
theme_few()


complete$FsizeD[complete$Fsize == 1] <- 'singleton'
complete$FsizeD[complete$Fsize < 5 & complete$Fsize > 1] <- 'small'
complete$FsizeD[complete$Fsize > 4] <- 'large'

mosaicplot(table(complete$FsizeD, complete$Survived), main='Family Size by Survival', shade=TRUE)

complete$Deck<-factor(sapply(complete$Cabin, function(x) strsplit(x, NULL)[[1]][1]))


embark_fare <- complete %>% filter(PassengerId != 62 & PassengerId != 830)


ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
    colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

complete$Embarked[c(62, 830)] <- 'C'



ggplot(complete[complete$Pclass == '3' & complete$Embarked == 'S', ], 
  aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
    colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()


complete$Fare[1044] <- median(complete[complete$Pclass == '3' & complete$Embarked == 'S', ]$complete, na.rm = TRUE)

sum(is.na(complete$Age))


factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Title','Surname','Family','FsizeD')

complete[factor_vars] <- lapply(complete[factor_vars], function(x) as.factor(x))

set.seed(129)


mice_mod <- mice(complete[, !names(complete) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

mice_output <- complete(mice_mod)

complete$Age <- mice_output$Age

complete$Child[complete$Age < 18] <- 'Child'
complete$Child[complete$Age >= 18] <- 'Adult'

complete$Mother <- 'Not Mother'
complete$Mother[complete$Sex == 'female' & complete$Parch > 0 & complete$Age > 18 & complete$Title != 'Miss'] <- 'Mo'


complete$Child  <- factor(complete$Child)
complete$Mother <- factor(complete$Mother)

md.pattern(complete)

train1<- complete[1:891,]
test1<- complete[892:1309,]
set.seed(754)


rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                                            Fare + Embarked + Title + 
                                            FsizeD + Child + Mother,
                                            data = train1)

plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))


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


prediction <- predict(rf_model, test1)

prediction<- prediction[complete.cases(prediction)]

solution <- data.frame(PassengerID = test1$PassengerId, Survived = prediction)


write.csv(solution, file = 'Titanic_Solution.csv', row.names = F)
