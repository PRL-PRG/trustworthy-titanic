
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

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.

if(T){
  lstInputFileNmList <- list()
  lstInputFileNmList[["train"]] <- "train.csv"
  lstInputFileNmList[["test"]] <- "test.csv"
  lstInputFileNmList[["submit"]] <- "gender_submission.csv"
}

#Load data
datTrain <- read.csv('../input/train.csv', header=T, stringsAsFactors=F)
test <- read.csv('../input/test.csv', header=T, stringsAsFactors=F)

datTrain$Age_log <- log(1+abs(datTrain$Age))
datTrain$Fare_log <- log(1+abs(datTrain$Fare))

dim(datTrain)

nrow(datTrain)

ncol(datTrain)

colnames(datTrain)

str(datTrain)

#knitr::kable(head(datTrain), format="pandoc", caption="") # Original Satou san7s R code 
head(datTrain)

# knitr::kable(tail(datTrain), format="pandoc", caption="")
tail(datTrain) 

summary(datTrain)

quantile(datTrain[,"Age"], na.rm=T, probs=seq(0,1,0.05))

quantile(datTrain[,"Age"], na.rm=T, probs=seq(0.95,1,0.005))

# Set plot size into width: 10 and height: 7 (Default: 7)
options(repr.plot.width=10, repr.plot.height=8)

## Reference
## Rをノートブック形式で利用する
## https://qiita.com/sbtseiji/items/876d2618702416892f7f
# JupyterのR環境はreprというパッケージを使用しており，そのオプションを設定すればプロットのサイズを変更できるようです。初期値では縦横とも7のサイズになっているので，それぞれ4に変更します。

p <- ggplot(datTrain, aes(x=Age))
p <- p + geom_histogram(colour="black", fill="white")
p <- p + theme(axis.text.x=element_text(size=14, face="bold"),
               axis.text.y=element_text(size=14, face="bold"))
plot(p)

p <- ggplot(datTrain, aes(x=Age_log))
p <- p + geom_histogram(colour="black", fill="white")
p <- p + theme(axis.text.x=element_text(size=14, face="bold"),
               axis.text.y=element_text(size=14, face="bold"))
plot(p)

p <- ggplot(datTrain, aes(x=Fare))
p <- p + geom_histogram(colour="black", fill="white")
p <- p + theme(axis.text.x=element_text(size=14, face="bold"),
               axis.text.y=element_text(size=14, face="bold"))
plot(p)

p <- ggplot(datTrain, aes(x=Fare_log))
p <- p + geom_histogram(colour="black", fill="white")
p <- p + theme(axis.text.x=element_text(size=14, face="bold"),
               axis.text.y=element_text(size=14, face="bold"))
plot(p)

p <- ggplot(datTrain, aes(x=Sex, y=Age, group=Sex))
p <- p + geom_boxplot(outlier.shape=NA, notch=F, coef=1.0)
p <- p + stat_summary(fun.y=mean, geom="point", shape=20, size=3, fill="white", show.legend=FALSE)
p <- p + theme(axis.text.x=element_text(size=14, face="bold"),
               axis.text.y=element_text(size=14, face="bold"))
plot(p)

p <- ggplot(datTrain, aes(x=Pclass, y=Fare_log, group=Pclass))
#p <- p + geom_boxplot(outlier.shape=NA, notch=F, coef=1.0)
p <- p + geom_boxplot()
p <- p + stat_summary(fun.y=mean, geom="point", shape=20, size=3, fill="white", show.legend=FALSE)
p <- p + theme(axis.text.x=element_text(size=14, face="bold"),
               axis.text.y=element_text(size=14, face="bold"))
plot(p)

fillFlg_eff <- datTrain[,"Fare_log"]>2.0
datTrain_eff <- datTrain[fillFlg_eff,]

p <- ggplot(datTrain_eff, aes(x=Pclass, y=Fare_log, group=Pclass))
#p <- p + geom_boxplot(outlier.shape=NA, notch=F, coef=1.0)
p <- p + geom_boxplot()
p <- p + stat_summary(fun.y=mean, geom="point", shape=20, size=3, fill="white", show.legend=FALSE)
p <- p + theme(axis.text.x=element_text(size=14, face="bold"),
               axis.text.y=element_text(size=14, face="bold"))
plot(p)

p <- ggplot(datTrain, aes(x=Survived, y=Age, group=Survived))
#p <- p + geom_boxplot(outlier.shape=NA, notch=F, coef=1.0)
p <- p + geom_boxplot()
p <- p + stat_summary(fun.y=mean, geom="point", shape=20, size=3, fill="white", show.legend=FALSE)
p <- p + theme(axis.text.x=element_text(size=14, face="bold"),
               axis.text.y=element_text(size=14, face="bold"))
plot(p)

fillFlg_eff <- datTrain[,"Fare_log"]>2.0
datTrain_eff <- datTrain[fillFlg_eff,]

p <- ggplot(datTrain_eff, aes(x=Survived, y=Fare_log, group=Survived))
#p <- p + geom_boxplot(outlier.shape=NA, notch=F, coef=1.0)
p <- p + geom_boxplot()
p <- p + stat_summary(fun.y=mean, geom="point", shape=20, size=3, fill="white", show.legend=FALSE)
p <- p + theme(axis.text.x=element_text(size=14, face="bold"),
               axis.text.y=element_text(size=14, face="bold"))
plot(p)

library(psych)
psych::pairs.panels(datTrain[,c("Survived","Age","Embarked")], pch = 16)

library(psych)
psych::pairs.panels(datTrain[,c("Survived","Pclass","Fare","Sex")])

library(psych)
psych::pairs.panels(datTrain[,c("Survived","SibSp","Parch","Cabin")])

library(psych)
psych::pairs.panels(datTrain[,c("Survived","Pclass","Cabin")])
