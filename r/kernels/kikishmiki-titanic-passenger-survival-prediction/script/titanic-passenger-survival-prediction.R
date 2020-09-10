## ----setup, include=FALSE, message = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(Amelia)
library(randomForest)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(grid)


## ----load------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv("../input/train.csv", header = TRUE, na.strings = c("", NA))
test <- read.csv("../input/test.csv", header = TRUE, na.strings = c("", NA))
whole <- rbind(train[,-2], test)



## ----wrangle---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missmap(whole, col = c("yellow", "blue"), main = "The Titanic Passenger Data")


## ----plot data, warning = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(x = as.factor(Survived))) +
  geom_bar(stat="count", width = 0.5) +
  labs(title = "Passenger Fate", x = "Survived", y = "Count") +
  theme_bw()


ggplot(train, aes(x = Sex)) +
  geom_bar(stat="count", width = 0.5) +
  labs(title = "Passenger Sex", x = "Sex", y = "Count") +
  theme_bw()


ggplot(train, aes(x = as.factor(Pclass))) +
  geom_bar(stat="count", width = 0.5) +
  labs(title = "Passenger Class", x = "Class", y = "Count") +
  theme_bw()

ggplot(train, aes(x = Age)) +
  geom_bar(stat="count") +
  labs(title = "Passenger Age", x = "Age", y = "Count") +
  annotate("text", x = 70, y = 27.5, label = paste("median age =", median(train$Age, na.rm = TRUE))) +
  theme_bw()



## ----plot data 2, warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(whole, aes(x = as.factor(Pclass), y = Age)) +
  geom_boxplot() +
  labs(title = "Passenger Age by Class (whole dataset)", x = "Class", y = "Age") +
  theme_bw()


a <- whole$Age[which(whole$Pclass == 1)]
c <- whole$Age[which(whole$Pclass == 3)]
wt <- wilcox.test(a, c)
footnote <- paste("1st class ages significantly differs from 3rd (p-value =", round(wt$p.value, 2), ")")
grid.text(footnote, x = unit(300, "native"), y = unit(50, "native"))



## ----titles----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# remember that everything in R is a function call including indexing operators [], thus the following
# two calls to sapply are equivalent: sapply(char.vec, "[", 2) and sapply(char.vec, function(x) {x[2]})
titles = sapply(strsplit(sapply(strsplit(as.character(whole$Name), ","), function(x){ x[2] }), "\\."), "[", 1)
titles = gsub(" ", "", titles, fixed = TRUE)
table(titles)
 
# We can clean this up a bit so the titles are no so fragmented
titles[which(titles == "Mme")] <- "Mrs"
titles[which(titles == "Ms")] <- "Miss"
titles[which(titles == "Mlle")] <- "Miss"

# Let's group all rare titles (those with occurences less than three) into one category
tab.count <- table(titles)
rare.titles = names(tab.count[which(tab.count <= 2)])
titles[which(titles %in% rare.titles)] <- "Grand"

# Let's add these titles to the data set
whole$Title <- titles 



## ----plot data 3, warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(whole, aes(x = as.factor(Title), y = Age)) +
  geom_boxplot() +
  labs(title = "Passenger Age by Title (whole dataset)", x = "Title", y = "Age") +
  theme_bw()


## ----age mean--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mean(whole$Age, na.rm = TRUE)


## ----missing age-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# get the index of all rows with missing values for Age
ind = which(is.na(whole$Age))
table(whole[ind, c("Title", "Pclass")])


## ----class_title_age-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# There has to be a way to generate a summary statistic (e.g. mean) inside geom_tile
summary.tab = aggregate(Age ~ Title + Pclass, whole, mean)
ggplot(summary.tab, aes(x = as.factor(Title), y = Pclass)) +
  geom_tile(aes(fill = Age), na.rm = TRUE, stat = "identity") +
  labs(title = "Passenger Age by Title and Class (whole dataset)", x = "Title", y = "Class") +
  theme_bw()


## ----impute age------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ind = which(is.na(whole$Age))
for (i in ind) {
  title.i <- whole[i, "Title"]
  class.i <- whole[i, "Pclass"]
  age.approx <- round(summary.tab[which(summary.tab$Title == title.i & summary.tab$Pclass == class.i), "Age"])
  
  whole[i, "Age"] <- age.approx
}


## ----class_embarked_fare---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary.tab = aggregate(Fare ~ Pclass + Embarked, whole, mean)
ggplot(summary.tab, aes(x = as.factor(Embarked), y = Pclass)) +
  geom_tile(aes(fill = Fare), na.rm = TRUE, stat = "identity") +
  labs(title = "Passenger Fare by Port of Embarkment and Class (whole dataset)", x = "Title", y = "Class") +
  theme_bw()


## ----impute fare-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ind = which(is.na(whole$Fare))
for (i in ind) {
  class.i <- whole[i, "Pclass"]
  port.i <- whole[i, "Embarked"]
  
  fare.approx <- round(summary.tab[which(summary.tab$Pclass == class.i & summary.tab$Embarked == port.i), "Fare"])
  
  whole[i, "Fare"] <- fare.approx
}



## ----class_embarked--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary.tab = table(whole$Embarked, whole$Pclass)
ggplot(whole, aes(x = Pclass, y = Embarked), na.rm = TRUE) +
  geom_count() +
  labs(title = "Passenger Embarkment Port by Class (whole dataset)", x = "Class", y = "Embarked") +
  theme_bw()


## ----fare_embarked---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(whole, aes(x = factor(Embarked), y = Fare)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binpositions = "all", binwidth = 0.8, na.rm = TRUE) +
  labs(title = "Passenger Embarkment Fare by Port of Embarkment (whole dataset)", x = "Embarked", y = "Fare") +
  theme_bw()


## ----fare_embarked2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(whole, aes(x = factor(Embarked), y = Fare), na.rm = TRUE) +
  geom_boxplot() +
  labs(title = "Passenger Embarkment Fare by Port of Embarkment (whole dataset)", x = "Embarked", y = "Fare") +
  theme_bw()


## ----fare_embarked_stats_test----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
wilcox.test(whole$Fare[which(whole$Embarked == "C")], whole$Fare[which(whole$Embarked == "S")])


## ----median_fare_by_embarked-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
median(na.omit(whole$Fare[which(whole$Embarked == "S")]))
median(na.omit(whole$Fare[which(whole$Embarked == "C")]))


## ----impute embarked-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ind = which(is.na(whole$Embarked))
for (i in ind) {
  whole[i, "Embarked"] <- "S"
}


## ----wrangle2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missmap(whole, col = c("yellow", "blue"), main = "The Titanic Passenger Data")


## ----family_size-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# visualise realtionship between family size and survival 
whole$FamilySize <- whole$SibSp + whole$Parch + 1

# but restrict the index to 
ind = which(whole$PassengerId %in% train$PassengerId) 
temp.data = cbind(Survived = train[,2], whole[ind,])

ggplot(temp.data, aes(x = FamilySize, fill = as.factor(Survived))) +
  geom_bar(position = "dodge", stat = "count") + 
  scale_x_continuous(breaks = c(1:11)) +
  labs(title = "Passenger Fate by Family Size (whole dataset)") +
  theme_few()



## ----family_size_category--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

whole$FamilyCategory[whole$FamilySize == 1] <- "single"
whole$FamilyCategory[whole$FamilySize == 2] <- "couple"
whole$FamilyCategory[whole$FamilySize >= 3 & whole$FamilySize <= 5] <- "small family"
whole$FamilyCategory[whole$FamilySize >= 6] <- "large family"

# but restrict the index to 
ind = which(whole$PassengerId %in% train$PassengerId) 
temp.data = cbind(Survived = train[,2], whole[ind,])


ggplot(temp.data, aes(x = FamilyCategory, fill = as.factor(Survived))) +
  geom_bar(position = "dodge", stat = "count") + 
  labs(title = "Passenger Fate by Family Size Category (whole dataset)") +
  theme_few()



## ----other_factors---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(temp.data, aes(x = Pclass, fill = as.factor(Survived))) +
  geom_bar(position = "dodge", stat = "count") + 
  labs(title = "Passenger Fate by Class", x = "Survived") +
  theme_few()

ggplot(temp.data, aes(x = Sex, fill = as.factor(Survived))) +
  geom_bar(position = "dodge", stat = "count") + 
  labs(title = "Passenger Fate by Sex", x = "Survived") +
  theme_few()

ggplot(temp.data, aes(x = as.factor(Survived), y = Age)) +
  geom_violin(scale = "area") + 
  geom_boxplot(width = 0.1) +
  labs(title = "Passenger Fate by Age", x = "Survived") +
  theme_few()

ggplot(temp.data, aes(x = as.factor(Survived), y = Fare)) +
  geom_violin(scale = "area") + 
  geom_boxplot(width = 0.1) +
  labs(title = "Passenger Fate by Fare", x = "Survived") +
  theme_few()




## ----rf_training, results = "hide"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# we have engineered several new features which now reside in 'whole'
# so use whole split into train and test and not the originals
ind = which(whole$PassengerId %in% train$PassengerId) 
train <- cbind(Survived = train$Survived, whole[ind,]) 
test <- whole[-ind,]

set.seed(93549)
model <- randomForest(formula = as.formula( as.factor(Survived) ~ Sex + Pclass + Fare + Embarked + FamilySize), 
                     data = train, do.trace = T, ntree = 500, nodesize = 5)

# Show model error
plot(model, ylim=c(0,0.36))
legend('topright', colnames(model$err.rate), col=1:3, fill=1:3)



## ----rf_testing------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# test on the a partition test
prediction <- predict(model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction) and write to file
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)


