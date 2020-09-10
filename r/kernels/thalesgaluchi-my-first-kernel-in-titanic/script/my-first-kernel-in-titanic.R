my_train <- read.csv('../input/train.csv', dec='.', stringsAsFactors=FALSE, header=TRUE)
test <- read.csv('../input/test.csv', dec='.', header=TRUE)

my_train$Age[is.na(my_train$Age)] <- 200

#Tabela PassengerId, Name, Ticket, Cabin
my_train.surv <- my_train[,c(2,3,5,6,7,8,10,12)]
my_train.surv$Age <- as.numeric(as.character(my_train.surv$Age))
