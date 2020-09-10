
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
library(rpart)
library(stringr)
library(doSNOW)
 library(randomForest)
 library(rattle)
 library(rpart.plot)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(party)
library(rpart.plot)
library(xgboost)
library(drat)
library(beanplot)



library(RCurl, quietly = T)

library(tidyverse, quietly = T)


library(ggplot2)
library(gridExtra, quietly = T)
library(beanplot, quietly = T)
library(caret, quietly = T)
library(stringr, quietly = T)
library(party, quietly = T)
library(xgboost, quietly = T)
library(skimr, quietly = T)
library(alluvial, quietly = T)
library(pROC, quietly = T)
library(ggrepel, quietly = T)
library(Amelia)

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.

## changing Survivde / dead instead of 0 -1
prep_data <- function(D) {
    if (!is.null(D$Survived)) {
        D$Survived <- factor(D$Survived,
                             levels = c(1, 0),
                             labels = c('Survived', 'Dead'))
    }
    D$Pclass <- factor(D$Pclass,
                       levels = c(1, 2, 3),
                       labels = c('P1', 'P2', 'P3'))
   
    D
}
train <- prep_data(train)


#Basic  EDA
prop.table(table(train$Survived))
summary(train$Sex)
summary(test$Sex)
histogram(train$Sex)
histogram(test$Sex)
 prop.table(table(train$Sex, train$Survived))

prop.table(table(train$Sex, train$Survived), 1)

summary(train$Age)
summary(test$Age)
## cont
p1 <- ggplot(data = train, aes(x = Age)) + geom_histogram(aes(fill = Survived), bins = 40) + coord_flip()
p2 <- ggplot(data = train, aes(x = Fare)) + geom_histogram(aes(fill = Survived), bins = 40) + coord_flip()
grid.arrange(p1, p2, nrow = 1)



## EDa categorical
get_legend <- function(myggplot) {
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}
p <- lapply(X = c('Pclass', 'Sex', 'SibSp', 'Parch', 'Embarked'),
            FUN = function(x) ggplot(data = train) +
                aes_string(x = x, fill = 'Survived') +
                    geom_bar(position = "dodge") +
                        theme(legend.position = "none"))
            legend <- get_legend(ggplot(data = train, aes(x = Pclass, fill = Survived)) + geom_bar())
grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]],
             legend, layout_matrix = cbind(c(1, 2, 3),
                                          c(4, 5, 3),
                                          c(6, 6, 6)),
             widths = c(3, 3, 1))

                        ##beanplots
ggplot(train, aes(y = Age, x = Pclass)) + geom_boxplot(aes(fill = Survived)) + theme_bw()

beanplot(Age ~ Survived * Pclass, side = 'b', train, col = list('yellow', 'orange'),
         border = c('yellow2', 'darkorange'), ll = 0.05, boxwex = .5,
         main = 'Passenger survival by pclass and Age', xlab = 'Passenger Class', ylab = 'Age')
legend('topright', fill = c('yellow', 'orange'), legend = c("Dead", "Survived"), bty = 'n', cex = .8)




## family  Eda
ggplot(train, aes(y = SibSp, x = Parch)) +
    geom_jitter(aes(color = Survived, shape = Pclass)) +
    theme_bw() +
    scale_shape(solid = F) +
    geom_vline(xintercept = 3, color = 'darkred', lty = 2) +
    geom_hline(yintercept = 3, color = 'red', lty = 2)


#aluvial
train %>%
    mutate(Age_Group = case_when(
        Age < 18 ~ 'Child',
        Age >= 18 ~ 'Adult'
    )) %>%
    group_by(Survived, Sex, Pclass, Age_Group) %>%
    summarise(N = n()) %>%
    ungroup %>%
    na.omit -> alluvial_table

alluvial(alluvial_table[, c(-5)],
         freq = alluvial_table$N,
         cex = 0.8,
         col = ifelse(alluvial_table$Survived == "Survived", "blue", "forestgreen"))


ggplot(train, aes(x = Sex , y = Age)) +
    geom_jitter(shape = 21, alpha = .6, col = 'blue') +
    stat_summary(aes(y = Age, group = 1), fun.y = median, colour = "red", geom = "point", group = 1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    labs(caption = 'red points are median values')

ggplot(combi, aes(x = Sex, y = SimpTitle)) +
    geom_jitter(shape = 21, alpha = .6, col = 'blue') +
    stat_summary(aes(y = Age, group = 1), fun.y = median, colour = "red", geom = "point", group = 1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    labs(caption = 'red points are median values')




##combine datasets

test$Survived <- NA
test<-prep_data(test)
combi <- (rbind(train, test))
combi$Survived<-as.factor(combi$Survived)

## getting title

combi$Name <- as.character(combi$Name)

strsplit(combi$Name[1], split = '[,.]')
strsplit(combi$Name[1], split = '[,.]')[[1]]
strsplit(combi$Name[1], split = '[,.]')[[1]][2]




combi$Title <- sapply(combi$Name, FUN = function(x) { strsplit(x, split = '[,.]')[[1]][2] })
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)

ggplot(combi, aes(Title)) +
    geom_bar()





combi$Title[combi$Title %in% c('Ms', 'Miss')] <- 'Miss'
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Miss'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Mrs'

combi <- combi %>%
    mutate(SimpTitle = sub(".*?(Mr|Miss|Mrs|Master|$).*", "\\1", Title))

weirdos <- combi[combi$SimpTitle == "", c("PassengerId", "Survived", "Pclass", "Name", "Sex", "Title", "SimpTitle")]

m.weirdo_vec <- as.matrix(weirdos %>%
  filter(Sex == "male") %>%
  select(PassengerId))

combi[combi$PassengerId %in% m.weirdo_vec, "SimpTitle"] <- 'Mr'

combi$SimpTitle[combi$PassengerId %in% c('797')] <- 'Mrs'

combi[combi$Title == 'Dr',]

ggplot(combi, aes(SimpTitle)) +
    geom_bar()



weirdos$Survived<-as.factor(weirdos$Survived)

combi$Title <- factor(combi$Title)
combi$SimpTitle <- factor(combi$SimpTitle)


### Famili size feature

combi$FamilySize <- combi$SibSp + combi$Parch + 1

ggplot(combi[1:891,], aes(FamilySize, fill = Survived)) +
    geom_bar(position = "dodge") +
    scale_x_continuous(breaks = c(1:11)) +
    labs(x = 'Family Size')

boxplot(combi$FamilySize)
##PARty size
boxplot(combi$Fare)
ticket.party.size <- rep(0, nrow(combi))
avg.fare <- rep(0.0, nrow(combi))
tickets <- unique(combi$Ticket)



for (i in 1:length(tickets)) {
    current.ticket <- tickets[i]
    party.indexes <- which(combi$Ticket == current.ticket)
    current.avg.fare <- combi[party.indexes[1], "Fare"] / length(party.indexes)

    for (k in 1:length(party.indexes)) {
        ticket.party.size[party.indexes[k]] <- length(party.indexes)
        avg.fare[party.indexes[k]] <- current.avg.fare
    }
}

combi$PartySize <- ticket.party.size
combi$AvgFare <- avg.fare
ggplot(combi[1:891,], aes(PartySize, fill = Survived)) +
    geom_bar(position = "dodge") +
    scale_x_continuous(breaks = c(1:11)) +
    labs(x = 'Party Size')

summary(combi$AvgFare)
boxplot(combi$AvgFare)




ggplot(combi, aes(y = AvgFare, x = Pclass)) + geom_boxplot(aes(fill = Survived)) + theme_bw()

ggplot(combi, aes(y = AvgFare, x = Fare)) +
    geom_jitter(aes(color = Survived, shape = Pclass)) +
    theme_bw() +
    scale_shape(solid = F) +
    geom_vline(xintercept = 3, color = 'darkred', lty = 2) +
    geom_hline(yintercept = 3, color = 'red', lty = 2)


beanplot(AvgFare ~ Survived * Pclass, side = 'b', combi, col = list('yellow', 'orange'),
         border = c('yellow2', 'darkorange'), ll = 0.05, boxwex = .5,
         main = 'Passenger survival by pclass and Age', xlab = 'Passenger Class', ylab = 'Age')
legend('topright', fill = c('yellow', 'orange'), legend = c("Dead", "Survived"), bty = 'n', cex = .8)


#FamilyID
combi$Surname <- sapply(combi$Name, FUN = function(x) { strsplit(x, split = '[,.]')[[1]][1] })
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "")


combi$FamilyID[combi$FamilySize <= 2] <- 'Small'


table(combi$FamilyID)






famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

##combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)






ggplot(combi, aes(x = SimpTitle, y = Age)) +
    geom_jitter(color = 'blue', shape = 21, alpha = .7) +
    stat_summary(aes(y = Age, group = 1), fun.y = median, colour = "red", geom = "point", group = 1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(caption = 'red points are median values')


ggplot(combi, aes(x = PartySize, y = Age)) +
    geom_jitter(color = 'blue', shape = 21, alpha = .7) +
    stat_summary(aes(y = Age, group = 1), fun.y = median, colour = "red", geom = "point", group = 1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(caption = 'red points are median values')

summary(combi$Age)
median(combi$Age, na.rm = T)
boxplot(combi$Age)
top.qrt.age <- boxplot.stats(combi$Age)$stats[5]
age.outlier.filter <- combi$Age < top.qrt.age
combi[age.outlier.filter,]


Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + SimpTitle + PartySize,
                  data = combi[!is.na(combi$Age),],
                  method = "anova")
 combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

##Missing Emb

combi[combi$Embarked == "",]

embark.na.rm <- combi[1:891,] %>%
    filter(PassengerId != 62 & PassengerId != 830)
ggplot(embark.na.rm, aes(x = Embarked, y = Fare, fill = factor(Sex))) +
    geom_boxplot() +
    geom_hline(aes(yintercept = 80),
             colour = 'red', linetype = 'dashed', lwd = 2)



#which(combi$Embarked == '')

 combi$Embarked[c(62, 830)] = "C"
 combi$Embarked <- factor(combi$Embarked)






##Missing Fare
top.qrt.fare <- boxplot.stats(combi$Fare)$stats[5]
fare.outlier.filter <- combi$Fare < top.qrt.fare
nonoutlier.fares.data <- combi[fare.outlier.filter,]

fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
Fare.model <- lm(
  formula = fare.equation,
  data = nonoutlier.fares.data
)
Fare.rows <- combi[is.na(combi$Fare), c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")]

Fare.predictions <- predict(Fare.model, Fare.rows)

combi[is.na(combi$Fare), "Fare"] <- Fare.predictions
table(is.na(combi$Fare))

## as factors
combi$Pclass <- factor(combi$Pclass)
combi$Title <- factor(combi$Title)

train <- combi[1:891,]
test <- combi[892:1309,]




fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                                       Embarked + SimpTitle +  FamilyID + PartySize ,
                 data = train,
                 controls = cforest_unbiased(ntree = 2000, mtry = 3))

Prediction <- predict(fit, newdata = test, OOB = TRUE, type = "response")
NROW(Prediction)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)


submit_data <- function(D) {
    if (!is.null(D$Survived)) {
        D$Survived <- factor(D$Survived,
        levels = c('Survived', 'Dead'),
        labels = c(1, 0))
    }
   
    D
}
submit<-submit_data(submit)
write.csv(submit, file = "ctree4.csv", row.names = FALSE)

write_csv(submit, "ctree4.csv")
