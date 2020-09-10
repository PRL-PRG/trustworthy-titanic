
if(!require("pacman") ) install.packages("pacman")
pacman::p_load(tidyverse,corrgram,faraway,metaSEM,mgcv)

train <- read_csv(
    '../input/train.csv',
    col_types=cols(
        pclass = col_factor(NULL),
        survived = col_factor(NULL),
        sex = col_factor(NULL),
        ticket = col_factor(NULL),
        cabin = col_factor(NULL),
        embarked = col_factor(NULL),
        boat = col_factor(NULL),
        body = col_factor(NULL),
        home.dest = col_factor(NULL)
    )
)
problems(train)
summary(train)

test <- read_csv(
    '../input/test-fare.csv',
    col_types=cols(
        pclass = col_factor(NULL),
        survived = col_factor(NULL),
        sex = col_factor(NULL),
        ticket = col_factor(NULL),
        cabin = col_factor(NULL),
        embarked = col_factor(NULL),
        boat = col_factor(NULL),
        body = col_factor(NULL),
        home.dest = col_factor(NULL)
    )
)
problems(test)
summary(test)

#for debug
lapply(train,nlevels) %>% as.data.frame

test$fare <- NA
merged = rbind(train,test)
train2 <- as.data.frame(train)
for( i in colnames(merged)){
    if( is.factor(merged[[i]])) {
        levels(train[[i]]) <- levels(merged[[i]])
        levels(test[[i]]) <- levels(merged[[i]])
    }
}

#for debug
lapply(train,nlevels) %>% as.data.frame

train.complete <- na.omit(train)
train$log.fare <- log(1+train$fare)
train$sqrt.fare <- sqrt(train$fare)
options(repr.plot.width=7, repr.plot.height=14)
op <- par(no.readonly = TRUE)
par(mfrow = c(4,2))
hist(train$fare, main="Histogram of Fare")
boxplot(train$fare,main="Fare")
hist(train$log.fare, main="Histogram of Log Fare")
boxplot(train$log.fare,main="Log Fare")
hist(train$sqrt.fare, main="Histogram of Sqrt Fare")
boxplot(train$sqrt.fare,main="Sqrt Fare")
dotchart(train$fare,group=train$pclass, main="Fare by passenger class")
plot(train$pclass, train$sex, main="Fare by Age")
par(op)

options(repr.plot.width=10,repr.plot.height=10)
corrgram(train, order=TRUE, lower.panel=panel.pts,text.panel=panel.txt,upper.panel=panel.shade)

train.int <- sapply(train[-3],as.integer) #drop name
cor(train.int, use="pairwise.complete.obs")

#colnames(train)
dm= data.matrix(train[c("pclass","survived","sex")]) #use columns of our model only
faraway::vif(dm)

lm1 <- lm( log.fare ~ pclass + survived + sex, data=train)
summary(lm1)

#Type 3 ANOVA in R
drop1(lm1,.~.,test="F") #do single term deletions and calculate f-statistic p values from those

#Special syntax for dropping terms
lm1 <- update(lm1, . ~ . -survived) 
summary(lm1)

op <- par(mfrow = c(2,2))
plot(lm1)
par(op)

e <- rstandard(lm1)
df <- train #the lm excludes NA; so we must do so here as well
op <- par(mfrow = c(2,2))
hist(e)
plot( e ~ df$pclass )
plot( e ~ df$sex )
par(op)

predictions <- predict(lm1, newdata=test)
head( predictions )
test <- tibble::rowid_to_column(test, "i")
df.submission <- tbl_df(data.frame('Id' = as.character(test$Id), 'Estimated' = as.character(predictions)) ) #disable scientific notation, which throws off scoring
write_csv( df.submission, 'submission.csv')

train$pclass.int = as.numeric(levels(train$pclass))[train$pclass]
gam1 <- gam( log.fare ~ s(pclass.int,bs="cs",k=2)  + sex, data=train)
summary(gam1)

options(repr.plot.width=7, repr.plot.height=7)
plot.gam(gam1)

op <- par(mfrow = c(2,2))
gam.check(gam1)
par(op)

anova(lm1,gam1)


