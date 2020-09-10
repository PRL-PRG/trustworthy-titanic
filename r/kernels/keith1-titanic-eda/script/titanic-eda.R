## ----load_libraries--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library('caret')
library('dplyr')
library('ggplot2')
library('gplots')
library('knitr')
library('readr')
library('reshape2')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# load data
train <- read_csv('../input/train.csv')
test <- read_csv('../input/test.csv')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# preview data
dim(train)
dim(test)

colnames(train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
apply(train, 2, function (x) { sum(is.na(x)) })


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create a version of the training set with categorical variables 
# converted to factors
ftrain <- as.data.frame(train)

for (x in c('Survived', 'Pclass', 'Sex', 'Embarked')) {
    ftrain[,x] = factor(ftrain[,x])
}

# create alternate versions of the training set without Name, Ticket, or Cabin;
# these are all near-unique fields.
train_alt  <- train  %>% select(-Name, -Ticket, -Cabin)
ftrain_alt <- ftrain %>% select(-Name, -Ticket, -Cabin)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ftrain_long <- melt(ftrain_alt %>% select(-Age, -Fare), 
                    id.vars=c('PassengerId', 'Survived'))

ggplot(ftrain_long, aes(value)) + 
	geom_bar(aes(fill=Survived), position='dodge', alpha=0.7) + 
	facet_wrap(~variable, scales='free')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Indices of features
#ind <- 3:ncol(ftrain_alt)
numeric_cols <- c('Age', 'SibSp', 'Parch', 'Fare') 
numeric_ind <- which(colnames(ftrain_alt) %in% numeric_cols)

featurePlot(x=ftrain_alt[,numeric_ind],
            y=ftrain_alt$Survived,
            plot="box",
            scales=list(y=list(relation="free"),
                        x=list(rot=90)),
            auto.key=list(columns=2))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(ftrain, aes(Sex, fill=Survived)) + geom_bar(position="dodge")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(ftrain, aes(Age, fill=Survived)) + 
    geom_histogram(alpha=0.5, position="identity")

ggplot(ftrain, aes(Age, fill=Survived)) + 
    geom_density(alpha=0.5, position="identity")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(ftrain, aes(Fare, fill=Survived)) + 
    geom_histogram(alpha=0.5, position="identity")

ggplot(ftrain, aes(Fare, fill=Survived)) + 
    geom_density(alpha=0.5, position="identity")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create a version of the training matrix with all numeric features
train_alt_num <- train_alt
train_alt_num$Sex <- as.numeric(factor(train_alt$Sex))
train_alt_num$Embarked <- as.numeric(factor(train_alt$Embarked))

# impute age and embarked to remove NAs
train_alt_num$Age[is.na(train_alt_num$Age)] <- median(train_alt_num$Age, na.rm=TRUE)

embark_mode <- as.numeric(names(sort(table(train_alt_num$Embarked), decreasing=TRUE))[1])
train_alt_num$Embarked[is.na(train_alt_num$Embarked)] <- embark_mode

# everything but the outcome
ind <- 2:ncol(train_alt_num)

cor_mat <- cor(train_alt_num[,ind], method='spearman')
heatmap.2(cor_mat, trace='none', symbreaks=FALSE, symkey=FALSE)
          #cellnote=round(cor_mat, 4))

