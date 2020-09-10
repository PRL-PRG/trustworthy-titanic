
# This R script will run on our backend. You can write arbitrary code here!

# packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('VIM')
library('randomForest') # classification algorithm

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
summary(train)

#Here we will bind train and test data set
full  <- bind_rows(train, test)
summary(full)

#let's check how many people has survived by sex
table(train$Survived,train$Sex)

barplot(table(train$Survived, train$Sex), sub="Survival by Sex", ylab="number of passengers", col=c("firebrick1","snow"))
legend("topleft",legend = c("Died","Survived"),fill=c("firebrick1","snow"),inset = .05)

#survival by sex
ggplot(train, aes(x = Sex, fill = factor(Survived))) + geom_bar(stat = 'count',position = 'dodge') + scale_fill_brewer(palette = "Set1")  + theme_bw()

#surrvival by family size
full$Fsize <- full$SibSp + full$Parch + 1

ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
        geom_bar(stat='count', position='dodge') +
        scale_x_continuous(breaks=c(1:11)) +
        labs(x = 'Family Size') +
        theme_bw()
        
#in our data set,we have na values ,now we try to put appropriate values to those na values.
#as we see in summary there is only one NA value in fare,lets check which row has NA value
which(is.na(full$Fare))
#let's inspect 1044 row
full[1044,]

#as, we clearly see these passanger has class 3 and embarked from 's',let's check it's median value
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
        geom_density(fill = 'khaki2', alpha=0.4) + 
        geom_vline(aes(xintercept=median(Fare, na.rm=T)),
                   colour='red', linetype='dashed', lwd=1)+ 
        geom_vline(aes(xintercept=mean(Fare, na.rm=T)),
                   colour='green', linetype='dashed', lwd=1) +
        scale_x_continuous(labels=dollar_format()) +
        theme_bw()
        
#by, inspecting graph we can say it's better to replace median value than mean value as NA value.

full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S',]$Fare, na.rm = TRUE)

#there are 263 na values in the Age variable.here i use mice package to predict na values
md.pattern(full[ , !names(full) %in% c('Survived', 'Ticket', 'Name' ,'Cabin', 'Embarked')])
aggr_plot <- aggr(full$Age, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

mice_mod <- mice(full[ , !names(full) %in% c('Survived')],method = 'pmm')
mice_output <- complete(mice_mod)
full$Age <- mice_output$Age
par(mfrow=c(1,2))
 hist(full$Age, freq=F, main= "Age: Original Data",xlab = "Age", 
            col="darkorchid4", ylim=c(0,0.03))
 hist(mice_output$Age, freq=F, main="Age: MICE Output", xlab = "Age",
             col="darkorchid1", ylim=c(0,0.03))      
             
 ##if you search for NA values in Embarked than it results FALSE. so here we need to define NA values for string 
 table(is.na(full$Embarked))
 
 full$Embarked[full$Embarked == ""] = NA
 table(is.na(full$Embarked))
 sum(is.na(full$Embarked))
 which(is.na(full$Embarked))
 
 #now, we need to assign appropriate values for these NA value.for these let look at row 62 and 830
 full[62,]
 full[830,]
 
 #both of these passangers were paid $80 fare and belonged from class 1
em_fare = full %>% filter(PassengerId != 60 & PassengerId != 830) 
ggplot(em_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + 
        geom_boxplot() +
        geom_hline(aes(yintercept=80), 
                   colour='blue', linetype='dashed', lwd=1) +
        scale_y_continuous(labels=dollar_format())
        theme_bw()
        
full$Embarked[c(62, 830)] <- 'C'        
 
###
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Parch','SibSp','Fsize')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))   
###    
 md.pattern(full)
 
 train <- full[1:891,]
 test <- full[892:1309,]
 abc = full[1:418,]
 
rf_model <- randomForest( Survived ~ Age + Sex + Pclass  +Parch+SibSp + Embarked,data = train) 
 predictabc = predict(rf_model,newdata = abc)
 table(abc$Survived,round(predictabc))
 
#importance of variable
 impor = importance(rf_model)
 Importance = round(impor,2)
 varimpor <- data.frame(Variables = row.names(impor),  Importance = round(impor,2))
 
# Create a rank variable based on importance
 
 rankimpor <- varimpor %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))
 


        

