# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
# Any results you write to the current directory are saved as output.


################################
# First half of the code does basic preprocessing
# Second half of the code does deep learning


library(keras)
library(kyotil)
empty2na=function(x) {x[x==""]=NA; x = as.factor(as.character(x))}

pwd="../input"

dat=read.csv(file=pwd%+%"/train.csv",stringsAsFactors =T)
names(dat)=tolower(names(dat))
dat.test=read.csv(file=pwd%+%"/test.csv",stringsAsFactors =T)
names(dat.test)=tolower(names(dat.test))
summary(dat)
summary(dat.test)

# missing data
dat$embarked=empty2na(dat$embarked)
dat$cabin=empty2na(dat$cabin)
dat.test$cabin=empty2na(dat.test$cabin)

# rename some colums
names(dat)[names(dat)=="survived"]="y"

# fill in missing values for age and embarked
# in train data age is missing for 177, embarked is missing for 2
# in test data age is missing for 86
dat$age.1=dat$age; dat$age.1[is.na(dat$age.1)]=median(dat$age,na.rm=T)
dat$embarked.1=dat$embarked; dat$embarked.1[is.na(dat$embarked.1)]="S"
dat.test$age.1=dat.test$age; dat.test$age.1[is.na(dat.test$age.1)]=median(dat.test$age,na.rm=T)
dat.test$embarked.1=dat.test$embarked

# derived column
# train data
dat$ticket.share=table(dat$ticket)[dat$ticket] # number of people who share a ticket
dat$fare0=dat$fare==0; dat$lfare=log(1+dat$fare)
dat$pclass.f=as.factor(dat$pclass)
dat$class_gender=factor(paste("class"%+%dat$pclass, dat$sex,sep="_"))

# test data
dat.test$pclass.f=as.factor(dat.test$pclass)
dat.test$fare0=dat.test$fare==0; dat.test$lfare=log(1+dat.test$fare)
dat.test$fare0[is.na(dat.test$fare0)]=FALSE; dat.test$lfare[is.na(dat.test$lfare)]=median(dat.test$lfare, na.rm=T)
dat.test$ticket.share=table(dat.test$ticket)[dat.test$ticket] # number of people who share a ticket

save(dat, dat.test, file="titanic.Rdata")

# sex, pclass
# age
# sibsp parch
# fare 
# embarked
# cabin 
# name
# ticket 

##################################################
# Deep learning model 1

x_train=model.matrix(~ pclass.f + sex + age.1 + sibsp + embarked.1 + fare0 + lfare + parch + ticket.share, dat) [,-1] 
y_train <- to_categorical(dat$y, 2)

ver=6; seed=6; epo=295
    use_session_with_seed(seed, disable_gpu = FALSE, disable_parallel_cpu = FALSE)
    
    model <- keras_model_sequential() 
    model %>% 
      layer_dense(units = 128, activation = 'relu', input_shape = c(ncol(x_train))) %>% 
      layer_dropout(rate = 0.5) %>% 
      layer_dense(units = 128, activation = 'relu') %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = 128, activation = 'relu') %>%
      layer_dropout(rate = 0.4) %>%
      layer_dense(units = 2, activation = 'softmax')
    summary(model)
    
    model %>% compile(
      loss = 'categorical_crossentropy',
      optimizer = optimizer_rmsprop(),
      metrics = c('accuracy')
    )
    
    history <- model %>% fit(
      x_train, y_train, 
      epochs = epo, batch_size = nrow(dat)*0.8, 
      validation_split = 0.2
    )
    
    mypdf(file="dl_v"%+%ver%+%"_history_seed"%+%seed%+%"_epoch"%+%epo)
        plot(history$metrics$acc,       col="red", type="l", ylim=c(0.75,1), ylab="loss", xlab="epoch")
        lines(history$metrics$val_acc, col="blue")
    dev.off()

pred.train <- model %>% predict_classes(x_train)
model %>% evaluate(x_train, y_train)


# how to save model from previous epochs? not sure. for now, the workaround is to train to the desired epoch
x_test=model.matrix(~ pclass.f + sex + age.1 + sibsp + embarked.1 + fare0 + lfare + parch + ticket.share, dat.test) [,-1] 
pred <- model %>% predict_classes(x_test)
mywrite.csv(cbind(PassengerId=dat.test$passengerid, Survived=pred), file="DL1_submission")




##################################################
# Deep learning model 1

x_train=model.matrix(~ pclass.f + sex + age.1 + sibsp + embarked.1, dat)     [,-1] 
y_train <- to_categorical(dat$y, 2)

ver=1; seed=1; epo=300
    use_session_with_seed(seed, disable_gpu = FALSE, disable_parallel_cpu = FALSE)
    
    model <- keras_model_sequential() 
    model %>% 
      layer_dense(units = 256, activation = 'relu', input_shape = c(ncol(x_train))) %>% 
      layer_dropout(rate = 0.4) %>% 
      layer_dense(units = 128, activation = 'relu') %>%
      layer_dropout(rate = 0.3) %>%
      layer_dense(units = 2, activation = 'softmax')
    summary(model)
    
    model %>% compile(
      loss = 'categorical_crossentropy',
      optimizer = optimizer_rmsprop(),
      metrics = c('accuracy')
    )
    
    history <- model %>% fit(
      x_train, y_train, 
      epochs = epo, batch_size = nrow(dat)*0.8, 
      validation_split = 0.2
    )

model %>% evaluate(x_train, y_train)


# how to save model from previous epochs? not sure. for now, the workaround is to train to the desired epoch
x_test=model.matrix(~ pclass.f + sex + age.1 + sibsp + embarked.1, dat.test) [,-1] 
pred <- model %>% predict_classes(x_test)
mywrite.csv(cbind(PassengerId=dat.test$passengerid, Survived=pred), file="DL2_submission")
