
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
#library(randomForest)

# The train and test data is stored in the ../input directory
df.train <- read.csv("../input/train.csv")
df.test  <- read.csv("../input/test.csv")


#install.packages('Amelia')
library(Amelia)
library(ggplot2)
library(dplyr)
library(scales)

print(head(df.train))
print(str(df.train))
print(head(df.test))
print(str(df.test))
full <- bind_rows(df.train, df.test)

# missing map with Amelia
help("missmap")
missmap(full, main ="Missing MAP", col =c("yellow", "black"), legend= FALSE)
# about 20% of the age is missing, reasonable to perform imputation
#extend the window big enough if you get an error

ggplot(df.train, aes(Survived)) + geom_bar()
ggplot(df.train, aes(Pclass)) + geom_bar(aes(fill = factor(Pclass)))

ggplot(df.train, aes(Sex)) + geom_bar(aes(fill = factor(Sex)))

ggplot(df.train, aes(Age)) + geom_histogram(bins= 20, alpha =0.5, fill = 'blue')

ggplot(df.train, aes(SibSp)) + geom_bar()
#mostly single ppl

ggplot(df.train, aes(Fare)) + geom_histogram()
ggplot(df.train, aes(Fare)) + geom_histogram(fill = 'green', color ='black', alpha =0.5)
#most ppl paid low fares

#explore data cleaning
#fill in avg age by passenger class
pl <- ggplot(full, aes(Pclass, Age))
pl  <- pl + geom_boxplot(aes(group =Pclass, fill = factor(Pclass), alpha = 0.4))
pl + scale_y_continuous(breaks = seq(min(0), max(80), by =2)) + theme_bw()

impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}

  fixed.ages <- impute_age(full$Age,full$Pclass)

  full$Age <- fixed.ages
####
  full[1044,]
# missing the fare value
  ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ],
         aes(x= Fare)) +
    geom_density(fill ='#99d6ff', alpha =0.4) +
    geom_vline(aes(xintercept = median(Fare, na.rm =T)),
               colour ='red', linetype ='dashed', lwd =1) +
    scale_x_continuous(labels = dollar_format())

  #replace missing fare with the median dare for the class/ embarkment
  full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm =TRUE)


missmap(full, main="Titanic Training Data - Missings Map", 
col=c("yellow", "black"), legend=FALSE)
#all black now

#bulding model
##final cleanup of data to remove feautre we dont need
str(full)
full <- select(full, -PassengerId, -Name, -Ticket, -Cabin, -Parch)

head(full)
str(full)
#survived, class, SibSp, Parch should be factor and not int
full$Survived <- factor(full$Survived)
full$Pclass <- factor(full$Pclass)
full$SibSp <- factor(full$SibSp)
str(full)
#embarked has one blank level. ignore for now
# split the data  bACK TO train and test data sets
train.1 <- full[1:891,]
test.1 <- full[892:1309,]
str(train.1)
str(test.1)

######
#training the model
######
log.model <- glm(formula=Survived ~ . , family = binomial(link='logit'),data = train.1)
summary(log.model)

######
#predictions
######
str(train.1)
str(test.1)
model <- predict(log.model,newdata=test.1,type='response')
solution <-data.frame(PassengerID = df.test$PassengerId, Survived = model)
str(solution)
solution$Survived[solution$Survived < 0.5] <- 0
solution$Survived[solution$Survived >= 0.5] <- 1
head(solution)
write.csv(solution, file = 'logistic_titanic_model_sol.csv', row.names= F)


dev.off()
