
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

# load packages
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(car)
library(corrplot)
library(caret)
library(makedummies)

#Load data
train <- read.csv('../input/train.csv', header=T, stringsAsFactors=F)
test <- read.csv('../input/test.csv', header=T, stringsAsFactors=F)

head(train)

head(test)

# 必要な列のみを抽出
#train_vari <- train[,c(2,3,5:8,10,12)] # Original Honma san's R code.
train_vari <- train[,c("Survived","Pclass", "Sex","Age", "SibSp", "Parch", "Fare","Embarked")]


# ブランクをNAに変える
train_vari$Embarked[train_vari$Embarked==""] <- NA

# NAを取り除く
train_naomit <- na.omit(train_vari)

# SEX（性別）の置き換え
# female:0,male:1
train_naomit[,3] <- ifelse(train_naomit[,"Sex"]=="female",0,1)

# Embarked（乗船した港）の置き換え
# C:0,Q:1,S:2
train_naomit[,8] <- ifelse(train_naomit[,"Embarked"]=="C",0,ifelse(train_naomit[,"Embarked"]=="Q",1,2))

# ここからロジスティック回帰
# attachはデータをセットする的なモノ。以降はデータ名を指定しなくてもよくなる
# attach(train_naomit)
# ロジスティック回帰モデル作る
glm_result <- glm(Survived~Pclass+Sex+SibSp+Age+Embarked, family="binomial", data=train_naomit)

# test の整形
## test <- read.csv("~/R/titanic/Rin/test.csv", stringsAsFactors=F, na.strings=(c("NA", "")))
## test <- test[,c(1,2,4,5,6,7,9,11)]
test_naomit <- test[, c("Pclass", "Sex","Age", "SibSp", "Parch", "Fare","Embarked")]

# test Age欠損データを平均入れる
#test_naomit <- test
test_naomit$Age <- ifelse(is.na(test_naomit$Age),median(na.omit(test_naomit$Age)),test_naomit$Age)
test_naomit$Fare <- ifelse(is.na(test_naomit$Fare),median(na.omit(test_naomit$Fare)),test_naomit$Fare)

# 置き換え
test_naomit[,"Sex"] <- ifelse(test_naomit[,"Sex"]=="female",0,1)
test_naomit[,"Embarked"] <- ifelse(test_naomit[,"Embarked"]=="C",0,ifelse(test_naomit[,"Embarked"]=="Q",1,2))

# テスト予測
test_Survived <- ifelse(predict(glm_result,newdata=test_naomit,type="response")>=0.5,1,0)
#output <- cbind(test_naomit$PassengerId,test_Survived) # Original Honma san's R code.
output <- cbind(test$PassengerId,test_Survived)
colnames(output) <- c("PassengerId", "Survived")
output
#write.csv(output,"~/R/titanic/Rout/output_logi_1.csv") # Original Honma san's R code.
write.csv(output,"output_logi_1.csv", row.names=FALSE)
