
library(ggplot2)

train <- read.csv("../input/train.csv")
test <- read.csv("../input//test.csv")

train$Pclass <- factor(train$Pclass)
levels(train$Pclass) <- c("1st","2nd","3rd")

train$Survived <- factor(train$Survived, levels = c(1,0))
levels(train$Survived) <- c("Survived","Died")

train$SibSp <- factor(train$SibSp)
train$Parch <- factor(train$Parch)

# 100% Stacked van: Pclass, Sex, Embarked, SibSp, Parch
for (i in 1:(ncol(train))){
  if(names(train)[i] %in% c("Pclass","Sex","Embarked","SibSp","Parch")){
    mydf <- data.frame(prop.table(table(train$Survived, train[, i]),1))
    print(
      ggplot(mydf, aes(Var1, Freq, fill = Var2)) + 
        geom_bar(position = "stack", stat = "identity") +
        scale_y_continuous(labels=scales::percent) +
        labs(x='Survived', y='Percentage per klasse', fill=names(train)[i]) +
        ggtitle(names(train)[i])
    )
  }
}


# Mosaicplot for: Sex, Pclass en Survived
mosaicplot(train$Sex ~ train$Pclass + train$Survived , main="Passenger Survival by Class ans Gender",
           color=c("#8dd3c7", "#fb8072"), 
           shade=FALSE,  xlab="", ylab="",
           off=c(2), cex.axis=0.6)


summary(train$Age)

# Nieuwe variabelen maken
# Age categorie
train$AgeCat <- cut(train$Age,c(0,1,16,20,40,60,80))
# Fare categorie
train$FareCat <- cut(train$Fare,c(0,20,50,500))
levels(train$FareCat) <- c("Cheap 0-20","Normal 20-50","Expensive 50+")

table(train$FareCat)