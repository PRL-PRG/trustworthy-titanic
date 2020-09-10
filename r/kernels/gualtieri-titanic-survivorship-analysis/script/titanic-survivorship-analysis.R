## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# install.packages("xtable")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("plyr")
# install.packages("vcd")
# install.packages("png")
# install.packages("caret")
# install.packages("randomForest")
# install.packages("ggrepel")
# install.packages("party")
# install.packages("Amelia")
# install.packages("mice")
# install.packages("mlbench")
# install.packages("party")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("rattle")
# install.packages("ROCR")
# install.packages("Hmisc")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressMessages(library(caret))                                 # Machine learning
suppressMessages(library(rpart))
suppressMessages(library(e1071))
suppressMessages(library(randomForest))
suppressMessages(library(mlbench))
suppressMessages(library(party))
suppressMessages(library(ggplot2))                               # Plotting
suppressMessages(library(lattice))
suppressMessages(library(grid))
suppressMessages(library(gridExtra, warn.conflicts = FALSE))
suppressMessages(library(ggrepel))
suppressMessages(library(vcd))
suppressMessages(library(rpart.plot))
suppressMessages(library(rattle))
suppressMessages(library(ROCR))
suppressMessages(library(mice))                                  # Data imputation
suppressMessages(library(xtable))                                # Pretty printing dataframes
suppressMessages(library(plyr, warn.conflicts = FALSE))          # Manipulating dataframes
suppressMessages(library(Hmisc))
suppressMessages(library(Amelia))                                # Missing data
suppressMessages(library(dplyr, warn.conflicts = FALSE))
suppressMessages(library(stringr))                               # String operations


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('../input/train.csv', na.strings=c("NA", "NULL", ""), stringsAsFactors = F)
test  <- read.csv('../input/test.csv', na.strings=c("NA", "NULL", ""), stringsAsFactors = F)
all <- rbind.fill(train, test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainRows <- nrow(train)
testRows <- nrow(test)
totalRows <- nrow(train) + nrow(test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
renderTable <- function(data) {
  print(xtable(data), type = "html")
}

sampleDataFrame <- function(data, size) {
  sampleIndex <- sample(1:nrow(train), size)
  return(data[sampleIndex, ])
}


## ---- results="asis"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
renderTable(sampleDataFrame(train, 10))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(train)


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missmap(train, main = "Missing Values (Training Data-set)", col = c("red", "lightgrey"))


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missmap(test, main = "Missing Values (Testing Data-set)", col = c("red", "lightgrey"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missingCabinRows <- nrow(all[is.na(all$Cabin), ])
missingAgeRows <- nrow(all[is.na(all$Age), ])
missingFareRows <- nrow(all[is.na(all$Fare), ])
missingEmbarkedRows <- nrow(all[is.na(all$Embarked), ])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
estimateMissingVariables <- function(data) {
  predictors <- c("Age", "Sex", "Fare", "Pclass", "SibSp", "Parch", "Embarked", "Title")
  set.seed(345)
  capture.output(model <- mice(data[, names(data) %in% predictors], method='rf'))
  output <- complete(model)
  data$Age <- output$Age
  data$Fare <- output$Fare
  return(data)
}

fixedAll <- estimateMissingVariables(all)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
customHistogram <- function(data, column, title) {
  missing = nrow(data[is.na(data[,  column]), ])
  ggplot(data=data, aes_string(x = column)) +
  geom_histogram(bins = 20, na.rm = TRUE, fill = "blue", alpha = 0.2) +
  xlab(paste(column, "(", title, ", NA Count: ", missing, ")"))
}


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
allAge <- customHistogram(all, "Age", "Original")
fixedAllAge <- customHistogram(fixedAll, "Age", "Fixed")

allFare <- customHistogram(all, "Fare", "Original")
fixedAllFare <- customHistogram(fixedAll, "Fare", "Fixed")

grid.arrange(allAge, fixedAllAge, 
             allFare, fixedAllFare,
             ncol=2, nrow=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all <- fixedAll
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


## ----missingEmbarked, results='asis'---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missingEmbarkedIndex <- is.na(train$Embarked)
renderTable(train[missingEmbarkedIndex, ])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
estimateMissingEmbarked <- function(data) {
  missing <- data[is.na(data$Embarked), ]
  present <- data[!is.na(data$Embarked), ]

  fol <- formula(Embarked ~ Sex + Age + Fare + Pclass + SibSp + Parch)
  model <- rpart(fol, method='class', data=present)
  missing$Embarked <-predict(model, missing, type="class")
  all <- rbind.fill(missing, present)
  all <- all[with(all, order(PassengerId)), ]
  return(all)
}

all <- estimateMissingEmbarked(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


## ----fixedEmbarked, results='asis'-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
renderTable(train[missingEmbarkedIndex, ])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
toFactor <- function(data) {
   columns <- intersect(names(data), c("Survived", "Sex", "Embarked", "Pclass", "Ticket"))
  data[, columns] <- lapply(data[, columns] , factor)
  return(data)
}

all <- toFactor(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sample <- head(train, 12)
sample$Name


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namePattern <- "(.+),\\s*(.+?)\\..+"

extractSurname <- function(name) {
  return(str_match_all(name, namePattern)[[1]][2])
}

addSurname <- function(data) {
  data$Surname <- sapply(data$Name, extractSurname)
  data[, "Surname"] <- as.factor(data[, "Surname"])
  return(data)
}

all <- addSurname(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
extractTitle <- function(name) {
  return(str_match_all(name, namePattern)[[1]][3])
}

addTitle <- function(data) {
  data$Title <- sapply(data$Name, extractTitle)
  data[, "Title"] <- as.factor(data[, "Title"])
  return(data)
}

all <- addTitle(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
countBarchart <- function(data, column, title) {
   ggplot(data, aes_string(x=column)) + 
   geom_bar(fill = "blue", alpha = 0.2) +
   geom_text(stat='count', aes(label=sprintf("%d\n(%d %%)", ..count.., round(..count.. * 100/sum(..count..), 0)), vjust=0)) +
   xlab(paste(column, "(", title, ")"))
}

countBarchart(train, "Title", "Overall")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
addTitleWO <- function(data) {
  frequent <- c("Mr", "Miss", "Mrs", "Master")
  data$TitleWO <- sapply(data$Name, extractTitle)
  data$TitleWO[!(data$Title %in% frequent)] <- "Rare"
  data[, "TitleWO"] <- as.factor(data[, "TitleWO"])
  return(data)
}

all <- addTitleWO(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]

countBarchart(train, "TitleWO", "Overall")


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultCountBarchart <- function(data, column, categoryColumn) {
  survivors <- plyr::count(data, vars=c(column, categoryColumn))
  survivors <- group_by_(survivors, column) %>% dplyr::mutate(Percentage = round(freq * 100 / sum(freq)))

  ggplot(data = survivors, aes_string(x = column, y = "Percentage", fill = categoryColumn)) +
    geom_bar(stat="identity", position = "dodge") +
    geom_text(aes(label=sprintf("%d\n(%d %%)", freq, Percentage)))
}

categoricalResultCountBarchart(train, "TitleWO", "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultCountBarchart(train, "Sex", "Survived")


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultHistogram <- function(data, column, categoryColumn, breaks) {
  groupColumn <- paste0(column, "Group")
  suppressWarnings(data[, groupColumn] <- cut2(data[, column], g=breaks, digits=0))
  survivors <- plyr::count(data, vars=c(groupColumn, categoryColumn))
  survivors <- group_by_(survivors, groupColumn) %>% dplyr::mutate(Percentage = round(freq * 100 / sum(freq)))

  ggplot(data = survivors, aes_string(x = groupColumn, y = "Percentage", fill = categoryColumn)) +
    geom_bar(stat="identity", position = "dodge") +
    geom_text(aes(label=sprintf("%d\n(%d %%)", freq, Percentage))) +
    xlab(column)
}

categoricalResultHistogram(train, "Age", "Survived", 10)


## ---- fig.width = 6, fig.height = 12---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
createBoxPlotLabels <- function(data, column) {
  meta <- boxplot.stats(data[, column])
  labels <-data.frame(value=round(median(data[, column]), 4), label="Median")
  labels <-rbind(labels,data.frame(value=round(mean(data[, column]), 4), label="Mean"))
  labels <-rbind(labels,data.frame(value=meta$stats[2], label="1st Quartile"))
  labels <-rbind(labels,data.frame(value=meta$stats[4], label="3rd Quartile"))
  if(length(meta$out) > 0) labels <-rbind(labels,data.frame(value=round(median(meta$out), 4), label="Outliers Median"))
  if(length(meta$out) > 0) labels <-rbind(labels,data.frame(value=round(mean(meta$out), 4), label="Outliers Mean"))
  return(labels)
}

customBoxPlot <- function(data, column, title) {
  labels <- createBoxPlotLabels(data, column)
  ggplot(data, aes_string(x="factor(0)", y=column)) +
  geom_boxplot(fill = "blue", alpha=0.2) +
  geom_point(position = position_jitter(width = 0.2), color = "darkblue") +
  geom_label_repel(data=labels,
                   aes(x=factor(0), y=value, label=paste0(label, ": ", value)),
                   colour="red", angle=0, size=3,
                   point.padding = unit(1.0, "lines"), box.padding = unit(0.5, "lines")) +
  xlab(title)
}

customBoxPlot(all, "Age", "Overall")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
quarter1Indexes <- function(data, column) {
  meta <- boxplot.stats(data[, column])
  q1 <- meta$stats[2]
  return(which(data[, column] < q1))
}

quarter3Indexes <- function(data, column) {
  meta <- boxplot.stats(data[, column])
  q3 <- meta$stats[4]
  return(which( data[, column] > q3))
}

outliersMedian <- function(data, column) {
  meta <- boxplot.stats(data[, column])
  return(median(meta$out))
}

addAgeWO <- function(data) {
  data$AgeWO <- data$Age
  q1Median <- median(data$Age[quarter1Indexes(data, "Age")])
  q3Median <- median(data$Age[quarter3Indexes(data, "Age")])
  data$AgeWO[quarter1Indexes(data, "Age")] <- q1Median
  data$AgeWO[quarter3Indexes(data, "Age")] <- q3Median
  return(data)
}

all <- addAgeWO(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ageHistogram <- customHistogram(all, "Age", "Overall")
ageWOHistogram <- customHistogram(all, "AgeWO", "Overall")

grid.arrange(ageHistogram, ageWOHistogram, ncol = 2)


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultHistogram(train, "AgeWO", "Survived", 10)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
addAgeWO <- function(data) {
  data$AgeWO <- data$Age
  data$AgeWO[data$Age <13] <- 12
  q3Median <- median(data$Age[quarter3Indexes(data, "Age")])
  data$AgeWO[quarter3Indexes(data, "Age")] <- q3Median
  return(data)
}

all <- addAgeWO(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultHistogram(train, "AgeWO", "Survived", 10)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
addIsChild <- function(data) {
  data$IsChild <- data$Age < 12
  data[, "IsChild"] <- as.factor(data[, "IsChild"])
  return(data)
}

all <- addIsChild(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]

categoricalResultCountBarchart(train, "IsChild", "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
verticalLine <- function(value, name, color) {
  label <- paste("paste(", shQuote(name), ", ", shQuote("\\n"), ", ", value, ")")
  geomObj <- list(geom_vline(aes_string(xintercept=value), colour=color, linetype='dashed'),
                  geom_text(aes_string(x=value, y = 0, label=label)))
  return(geomObj)
}

customDensityPlot <- function(data, xColumn, title) {
  minXColumn <- paste("min(", xColumn, ", na.rm = TRUE)")
  medianXColumn <- paste("median(", xColumn, ", na.rm = TRUE)")
  maxXColumn <- paste("max(", xColumn, ", na.rm = TRUE)")
  
  ggplot(data, aes_string(x = xColumn)) + 
  geom_density(na.rm = TRUE, fill = "blue", alpha=0.2) +
  xlab(paste(xColumn, "(", title, ")")) +

  verticalLine(minXColumn,  "min", "blue") +
  verticalLine(medianXColumn, "median", "red") +
  verticalLine(maxXColumn, "max", "blue")
}


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
customDensityPlot(all, "Fare", "Overall")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cData <- all[all$Embarked == "C", ]
c1Data <- cData[cData$Pclass == 1, ]
c2Data <- cData[cData$Pclass == 2, ]
c3Data <- cData[cData$Pclass == 3, ]

sData <- all[all$Embarked == "S", ]
s1Data <- sData[sData$Pclass == 1, ]
s2Data <- sData[sData$Pclass == 2, ]
s3Data <- sData[sData$Pclass == 3, ]

qData <- all[all$Embarked == "Q", ]
q1Data <- qData[qData$Pclass == 1, ]
q2Data <- qData[qData$Pclass == 2, ]
q3Data <- qData[qData$Pclass == 3, ]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
c1Density <- customDensityPlot(c1Data, "Fare", "1st-class, C")
c2Density <- customDensityPlot(c2Data, "Fare", "2nd-class, C")
c3Density <- customDensityPlot(c3Data, "Fare", "3rd-class, C")

s1Density <- customDensityPlot(s1Data, "Fare", "1st-class, S")
s2Density <- customDensityPlot(s2Data, "Fare", "2nd-class, S")
s3Density <- customDensityPlot(s3Data, "Fare", "3rd-class, S")

q1Density <- customDensityPlot(q1Data, "Fare", "1st-class, Q")
q2Density <- customDensityPlot(q2Data, "Fare", "2nd-class, Q")
q3Density <- customDensityPlot(q3Data, "Fare", "3rd-class, Q")


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
grid.arrange(c1Density, s1Density, q1Density, 
             c2Density, s2Density, q2Density, 
             c3Density, s3Density, q3Density, 
             ncol=3, nrow=3)


## ----embarkedAnalysis, results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
renderTable(train[missingEmbarkedIndex, ])


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultHistogram(train, "Fare", "Survived", 6)


## ---- fig.width = 6, fig.height = 12---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
customBoxPlot(all, "Fare", "Overall")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
addFareWO <- function(data) {
  data$FareWO <- data$Fare
  q1Median <- median(data$Age[quarter1Indexes(data, "Fare")])
  q3Median <- median(data$Age[quarter3Indexes(data, "Fare")])
  data$FareWO[quarter1Indexes(data, "Fare")] <- q1Median
  data$FareWO[quarter3Indexes(data, "Fare")] <- q3Median
  return(data)
}

all <- addFareWO(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fareHistogram <- customHistogram(all, "Fare", "Overall")
fareWOHistogram <- customHistogram(all, "FareWO", "Overall")

grid.arrange(fareHistogram, fareWOHistogram, ncol = 2)


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultHistogram(train, "FareWO", "Survived", 6)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultCountBarchart(train, "Pclass", "Survived")


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultCountBarchart(train, "SibSp", "Survived")


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultCountBarchart(train, "Parch", "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
addIsMother <- function(data) {
  data$IsMother <- data$Age > 21 & data$Sex == "female" & data$Parch > 0
  data[, "IsMother"] <- as.factor(data[, "IsMother"])
  return(data)
}

all <- addIsMother(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]

categoricalResultCountBarchart(train, "IsMother", "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultCountBarchart(train, "Embarked", "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cData <- train[train$Embarked == "C", ]
qData <- train[train$Embarked == "Q", ]
sData <- train[train$Embarked == "S", ]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cAge <- customHistogram(cData, "Age", "Embarked = C")
qAge <- customHistogram(qData, "Age", "Embarked = Q")
sAge <- customHistogram(sData, "Age", "Embarked = S")

cFare <- customHistogram(cData, "Fare", "Embarked = C")
qFare <- customHistogram(qData, "Fare", "Embarked = Q")
sFare <- customHistogram(sData, "Fare", "Embarked = S")

cSex <- countBarchart(cData, "Sex", "Embarked = C")
qSex <- countBarchart(qData, "Sex", "Embarked = Q")
sSex <- countBarchart(sData, "Sex", "Embarked = S")


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
grid.arrange(cAge, qAge, sAge, 
             cFare, qFare, sFare, 
             cSex, qSex, sSex, 
             ncol=3, nrow=3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
addFamilySize <- function(data) {
  data$FamilySize <- data$SibSp + data$Parch + 1
  return(data)
}

all <- addFamilySize(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
computeSurvivalRatePerColumn <- function(data, column) {
  rates <- plyr::count(data, vars=c(column, "Survived"))
  rates <- group_by_(rates, column) %>% dplyr::mutate(SurvivalRate = round(freq * 100 / sum(freq)))
  rates <- rates[rates$Survived == 1, ]
  rates <- rates[, which(names(rates) %in% c(column, "SurvivalRate"))]
  names(rates)[names(rates) == "SurvivalRate"] <- paste0("SurvivalRateBy", column)
  return(rates)
}

addSurvivalRate <- function(column, data, rateData) {
  rates <- computeSurvivalRatePerColumn(rateData, column)
  rateColumn <- paste0("SurvivalRateBy", column)
  
  if(rateColumn %in% names(data)) {
    data <- data[ , -which(names(data) %in% c(rateColumn))]
  }
  data <- left_join(data, rates,by=column)
  data[is.na(data[, rateColumn]), rateColumn] <- 0
  return(data)
}

all <- addSurvivalRate("Surname", all, train)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
countBarchart(train, "SurvivalRateBySurname", "Overall")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
addFamilyID <- function(data) {
  data$FamilyID <- paste0(data$Surname, as.character(data$FamilySize))
  data[, "FamilyID"] <- as.factor(data[, "FamilyID"])
  return(data)
}

all <- addFamilyID(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MIN_FAMILY_SIZE <- 3

tooSmallFamiliesIndexes <- function(data) {
  return(which(data$FamilySize < MIN_FAMILY_SIZE))
}

addFamilyIDWO <- function(data) {
  data$FamilyIDWO <- paste0(data$Surname, as.character(data$FamilySize))
  data$FamilyIDWO[tooSmallFamiliesIndexes(data)] <- paste0("FamilySize<", toString(MIN_FAMILY_SIZE))
  data[, "FamilyIDWO"] <- as.factor(data[, "FamilyIDWO"])
  return(data)
}

all <- addFamilyIDWO(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]

all <- addSurvivalRate("FamilyIDWO", all, train)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cabins <- train[!is.na(train$Cabin), ]$Cabin
head(cabins, 12)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
allPassengers <- nrow(train)
firstClassPercent <- round(nrow(train[train$Pclass == 1, ]) * 100 / allPassengers)
secondClassPercent <- round(nrow(train[train$Pclass == 2, ]) * 100 / allPassengers)
thirdClassPercent <- round(nrow(train[train$Pclass == 3, ]) * 100 / allPassengers)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
survivors <- nrow(train[train$Survived == 1, ])
firstClassSurvivorsPercent <- round(nrow(train[train$Pclass == 1 & train$Survived == 1, ]) * 100 / survivors)
secondClassSurvivorsPercent <- round(nrow(train[train$Pclass == 2 & train$Survived == 1, ]) * 100 / survivors)
thirdClassSurvivorsPercent <- round(nrow(train[train$Pclass == 3 & train$Survived == 1, ]) * 100 / survivors)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
extractDeck <- function(cabin) {
  return(toString(unique(strsplit(cabin, "[^A-Z]+")[[1]])))
}

addDeck <- function(data) {
  data$Deck <- sapply(data$Cabin, extractDeck)
  data[, "Deck"] <- as.factor(data[, "Deck"])
  return(data)
}

all <- addDeck(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
countBarchart(train, "Deck", "Overall")


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultCountBarchart(train, "Deck", "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
normalizeList <- function(elems) {
  elems <- unique(elems)
  elems <- elems[!is.na(elems)]
  s <- paste(elems, collapse = " ")
  if(str_length(trimws(s)) == 0) {
    s <- NA
  }
  return(s)
}

cabins <- all[with(all, order(Ticket)), which(names(all) %in% c("Ticket", "Cabin"))]
cabins <- cabins %>%
  group_by(Ticket) %>%
  summarise(Cabin = list(Cabin))
cabins$Cabin <- sapply(cabins$Cabin, normalizeList)

ticketsMissngCabinRows <- nrow(cabins[is.na(cabins$Cabin), ])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
aggregateFunction <- function(s) {
  return(paste(s, collapse = " ~ "))
}

tickets <- train[with(train, order(Ticket)), which(names(train) %in% c("Ticket", "Name", "Surname", "Title", "Sex"))]
tickets <- tickets %>%
  group_by(Ticket) %>%
  summarise(Names = aggregateFunction(Name), 
            Surnames = aggregateFunction(Surname), 
            Titles = aggregateFunction(Title), 
            Genders = aggregateFunction(Sex),
            People = n())
tickets <- tickets[tickets$People > 1, ]


## ---- results="asis"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
renderTable(tickets)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all <- addSurvivalRate("Ticket", all, train)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
countBarchart(train, "SurvivalRateByTicket", "Overall")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
randomForestRandomCrossValidation <- function(fml, data, metric, radius, repeats) {
  set.seed(345)
  control <- trainControl(method="repeatedcv", number=10, repeats=repeats, search="random")
  
  rfCV <- train(fml, data=data, method="rf", metric=metric, tuneLength=radius, trControl=control, importance = TRUE)
  return(rfCV)
}

fml <-Survived ~ Sex + Age + Fare + Pclass + SibSp + Parch + FamilySize
plot(randomForestRandomCrossValidation(fml, train, "Accuracy", 10, 1))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fml <-Survived ~ Sex + Age + Fare + Pclass + SibSp + Parch + FamilySize + Embarked + TitleWO + FamilyIDWO
#plot(randomForestRandomCrossValidation(fml, train, "Accuracy", 10, 3))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mtryTunning <- function(data) {
  set.seed(345)
  suppressMessages(mtry <- tuneRF(data[, names(data) %in% attr(terms(fml), "term.labels")],
                                  data[, names(data) %in% c("Survived")],
                                  stepFactor=3.0,
                                  improve=1e-8,
                                  ntree=500,
                                  trace=FALSE))
  return(mtry)
}

fml <-Survived ~ Sex + Age + Fare + Pclass + SibSp + Parch + FamilySize + Embarked + TitleWO
print(mtryTunning(train))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
RandomForestBuilder <- function() {
  mtry = 1
  
  name = paste("Random Forest (randomForest) mtry:", mtry)
  
  model = function(fml, data) {
    set.seed(345)
    numVars <- length(attr(terms(fml), "term.labels"))
    maxMtry <- floor(sqrt(numVars))
    
    if (mtry > maxMtry) {
      return(randomForest(formula(fml), data = data))
    }
    return(randomForest(formula(fml), data = data, mtry = mtry))
  }
  
  predictions = function(model, data) {
    return(stats::predict(model, newdata = data, type = "class"))
  }
  
  probabilities = function(model, data) {
    result <- predict(model, newdata=data, type="prob")
    return(result[, 2])
  }
  
  return(list(name=name, model=model, predictions=predictions, probabilities=probabilities))
}

RandomForestRandomCVBuilder <- function() {
  name = "Random Forest with Random Search Cross Validation (rpart)"
  
  model = function(fml, data) {
    set.seed(345)
    return(randomForestRandomCrossValidation(fml, data, "Accuracy", radius=10, repeats=3))
  }
  
  predictions = function(model, data) {
    return(stats::predict(model, newdata = data, type = "raw"))
  }
  
  probabilities = function(model, data) {
    result <- predict(model, newdata=data, type="prob")
    return(result[, 2])
  }
  
  return(list(name=name, model=model, predictions=predictions, probabilities=probabilities))
}

DecisionTreeBuilder <- function() {
  name = "Decision Tree (rpart)"
  
  model = function(fml, data) {
    set.seed(345)
    return(rpart(fml, data = data, method="class"))
  }
  
  predictions = function(model, data) {
    return(stats::predict(model, newdata = data, type = "class"))
  }
  
  probabilities = function(model, data) {
    result <- predict(model, newdata=data, type="prob")
    return(result[, 2])
  }
  
  return(list(name=name, model=model, predictions=predictions, probabilities=probabilities))
}

ConditionalInferenceBuilder <- function() {
  mtry = 3
  
  name = paste("Conditional Inference Forest (party) mtry: ", mtry)
  
  model = function(fml, data) {
    set.seed(345)
    
    numVars <- length(attr(terms(fml), "term.labels"))
    maxMtry <- floor(sqrt(numVars))
    
    if (mtry > maxMtry) {
      return(cforest(fml, data = data, controls=cforest_unbiased()))
    }
    return(cforest(fml, data = data, controls=cforest_unbiased(mtry = mtry)))
  }
  
  predictions = function(model, data) {
    return(stats::predict(model, newdata = data, type = "response"))
  }
  
  probabilities = function(model, data) {
    result <- predict(model, newdata=data, type="prob")
    result <- lapply(result, `[`, 2)
    return(result)
  }
  
  return(list(name=name, model=model, predictions=predictions, probabilities=probabilities))
}

SVMBuilder <- function(fml, data) {
  name = "SVM (e1071)"
  
  model = function(fml, data) {
    set.seed(345)
    return(svm(formula(fml), data = data, probability=TRUE))
  }
  
  predictions = function(model, data) {
    return(stats::predict(model, newdata = data, type = "class"))
  }
  
  probabilities = function(model, data) {
    result <- stats::predict(model, newdata = data, probability=TRUE)
    return(attr(result, "probabilities")[, 2])
  }
  
  return(list(name=name, model=model, predictions=predictions, probabilities=probabilities))
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
builder <- DecisionTreeBuilder()
fml <-Survived ~ Sex + Age + Fare + Pclass + SibSp + Parch + FamilySize + Embarked + TitleWO + FamilyIDWO
model <- builder$model(fml, train)
fancyRpartPlot(model)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fmeasure <- function(confusion) {
  p <- confusion$byClass["Pos Pred Value"]
  r <- confusion$byClass["Sensitivity"]
  f <- 2 * ((p * r) / (p + r))
  return(f)
}

accuracy <- function(confusion) {
  return(confusion$overall["Accuracy"])
}

createFormulaName <- function(fml) {
  formulaName <- paste(format(fml), collapse = "")
  formulaName <- str_replace_all(formulaName, "[\\s]", "")
  return(formulaName)
}

evaluateModel <- function(fml, ModelBuilder, predictionColumn, trainData, testData) {
  formula <- createFormulaName(fml)
  modelBuilder <- ModelBuilder()
  model <- modelBuilder$model(fml, trainData)
  predictions <- modelBuilder$predictions(model, testData)
  confusion <- confusionMatrix(data = predictions, reference = testData$Survived)
  evaluation <- data.frame(model = modelBuilder$name, 
                           formula = formula,
                           accuracy = accuracy(confusion), 
                           fmeasure = fmeasure(confusion), 
                           stringsAsFactors=FALSE)
  return(evaluation)
}

evaluateModels <- function(formulas, ModelBuilders, predictionColumn, data, testData = NULL) {
  set.seed(3456)
  if(is.null(testData)) {
    trainIdx <- createDataPartition(data$Survived, p = 0.6, list = FALSE, times = 1)
    trainData <- data[trainIdx,]
    testData <- data[-trainIdx,]
  } else {
    trainData <- data
  }
  
  evaluations <- data.frame(model = character(), 
                            formula = character(), 
                            accuracy = numeric(0), 
                            fmeasure = numeric(0), 
                            stringsAsFactors=FALSE)

  for (ModelBuilder in ModelBuilders) {
    for (fml in formulas) {
      evaluation <- evaluateModel(fml, ModelBuilder, predictionColumn, trainData, testData)
      evaluations <- bind_rows(evaluations, evaluation)
    }
  }
  renderTable(evaluations)
}


## ---- results="asis"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainIdx <- createDataPartition(train$Survived, p = 0.6, list = FALSE, times = 1)
trainData <- train[trainIdx,]
testData <- train[-trainIdx,]
trainData <- addSurvivalRate("Surname", trainData, trainData)
testData <- addSurvivalRate("Surname", testData, trainData)


trainData <- addSurvivalRate("Ticket", trainData, trainData)
testData <- addSurvivalRate("Ticket", testData, trainData)

trainData <- addSurvivalRate("FamilyID", trainData, trainData)
testData <- addSurvivalRate("FamilyID", testData, trainData)

trainData <- addSurvivalRate("FamilyIDWO", trainData, trainData)
testData <- addSurvivalRate("FamilyIDWO", testData, trainData)

formulas <- c(Survived ~ Sex,
              Survived ~ Sex + Age,
              Survived ~ Sex + Age + Fare,
              Survived ~ Sex + Age + Fare + Pclass,
              Survived ~ Sex + Age + Fare + Pclass + SibSp,
              Survived ~ Sex + Age + Fare + Pclass + SibSp + Parch,
              Survived ~ Sex + Age + Fare + Pclass + FamilySize,
              Survived ~ Sex + Age + Fare + Pclass + FamilySize + Embarked,
              Survived ~ Sex + Age + Fare + Pclass + FamilySize + Embarked + Title,
              Survived ~ Sex + Age + Fare + Pclass + FamilySize + Embarked + TitleWO,
              Survived ~ Sex + AgeWO + Fare + Pclass + FamilySize + Embarked + TitleWO,
              Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO,
              Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO + IsChild,
              Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO + IsChild + IsMother)

models <- c(RandomForestBuilder, SVMBuilder)

evaluateModels(formulas, models, "Survived", trainData, testData)


## ---- results="asis"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
formulas <- c(Survived ~ Sex + Age + Fare + Pclass + FamilySize + Embarked,
              Survived ~ Sex + Age + Fare + Pclass + FamilySize + Embarked + Title,
              Survived ~ Sex + Age + Fare + Pclass + FamilySize + Embarked + TitleWO,
              Survived ~ Sex + AgeWO + Fare + Pclass + FamilySize + Embarked + TitleWO,
              Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO)

models <- c(DecisionTreeBuilder, ConditionalInferenceBuilder, RandomForestRandomCVBuilder)

#evaluateModels(formulas, models, "Survived", trainData, testData)


## ---- fig.width = 16, fig.height = 8---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
createPrediction <- function(fml, ModelBuilder, testData) {
  modelBuilder <- ModelBuilder()
  model <- modelBuilder$model(fml, trainData)
  probabilities <- modelBuilder$probabilities(model, testData)
  return(prediction(as.numeric(probabilities), testData$Survived))
}

createPerformance <- function(prediction) {
  return(performance(prediction, measure = "tpr", x.measure = "fpr"))
}

createAuc <- function(prediction) {
  auc <- performance(prediction, measure = "auc")
  return(auc@y.values[[1]])
}

buildModelName <- function(fml, ModelBuilder, auc) {
  formulaName <- createFormulaName(fml)
  aucName <- paste("(AUC: ", auc, ")")
  modelBuilder <- ModelBuilder()
  name <- paste0(modelBuilder$name, "\n", formulaName, "\n", aucName, "\n")
  return(name)
}

createRocPlot <- function(roc) {
  return(list(geom_ribbon(data = roc, aes(x=FPR, fill = Model, ymin=0, ymax=TPR), alpha = 0.2),
              geom_line(data = roc, aes(x=FPR, y=TPR, color = Model))))
}

generateAllROCs <- function(formulas, ModelBuilders, trainData, testData) {
  rocPlots <- c()
  for (ModelBuilder in ModelBuilders) {
    for (fml in formulas) {
      modelBuilder <- ModelBuilder()
      prediction <- createPrediction(fml, ModelBuilder, testData)
      performance <- createPerformance(prediction)
      auc <- createAuc(prediction)
      roc <- data.frame(FPR=unlist(performance@x.values),
                        TPR=unlist(performance@y.values),
                        Model=rep(buildModelName(fml, ModelBuilder, auc), each=length(performance@x.values)))
      rocPlots <- c(rocPlots, createRocPlot(roc))
    }
  }
  
  roc <- data.frame(FPR=c(0.0, 1.0),
                    TPR=c(0.0, 1.0),
                    Model=rep("Line of No-discrimination\n", each=2))
  rocPlots <- c(rocPlots, createRocPlot(roc))
  ggplot() + rocPlots + coord_fixed()
}

formulas <- c(Survived ~ Sex + Age + Fare + Pclass + FamilySize + Embarked + TitleWO,
              Survived ~ Sex + AgeWO + Fare + Pclass + FamilySize + Embarked + TitleWO)

models <- c(RandomForestBuilder, SVMBuilder, DecisionTreeBuilder)

generateAllROCs(formulas, models, trainData, testData)


## ---- fig.width = 16, fig.height = 8---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
models <- c(ConditionalInferenceBuilder, RandomForestRandomCVBuilder)

#generateAllROCs(formulas, models, trainData, testData)


## ---- results="asis"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
formulas <- c(Survived ~ Sex + Age + Fare + Pclass + SibSp + Parch + FamilySize + Embarked + TitleWO + FamilyIDWO)

models <- c(SVMBuilder, DecisionTreeBuilder, ConditionalInferenceBuilder, RandomForestRandomCVBuilder)
#evaluateModels(formulas, models, "Survived", train)


## ---- fig.width = 16, fig.height = 8---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
models <- c(DecisionTreeBuilder, ConditionalInferenceBuilder, RandomForestRandomCVBuilder)

#generateAllROCs(formulas, models, trainData, testData)


## ---- fig.width = 12, fig.height = 6---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
customtImpPlot <- function(model, title) {
  if(!is(model, "RandomForest") & !is(model, "randomForest")) {
    model <- model$finalModel
  }
  if (is(model, "randomForest")) {
    varImpPlot(model, type=2, main=title)
  } else {
    importance <- varimp(model)
    dotchart(importance[order(importance)], main = title)
  }
}

varImpPlotAll <- function(formulas, ModelBuilders) {
    par(mfrow=c(length(formulas), length(ModelBuilders))) 
    varImpPlots <- c()
    for (ModelBuilder in ModelBuilders) {
      for (fml in formulas) {
        builder <- ModelBuilder()
        model <- builder$model(formula(fml), data=train)
        title <- paste(builder$name, "\n", createFormulaName(fml))
        customtImpPlot(model, title)
      }
    }
}

formulas <- c(Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO)

models <- c(RandomForestBuilder)

varImpPlotAll(formulas, models)


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
formulas <- c(Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO,
              Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO + FamilyIDWO)

models <- c(ConditionalInferenceBuilder)

#varImpPlotAll(formulas, models)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
buildOutputName <- function(fml, modelName) {
  formulaName <- paste0(format(fml), collapse = "")
  output <- paste0(modelName, formulaName)
  output <- str_replace_all(output, "[^\\w]", "")
  output <- paste0(output, ".csv")
  return(output)
}

generatePredictions <- function(fml, ModelBuilder, predictionColumn, trainData, testData) {
  modelBuilder <- ModelBuilder()
  model <- modelBuilder$model(fml, trainData)
  predictions <- modelBuilder$predictions(model, testData)
  solution <- data.frame(PassengerID = testData$PassengerId, Survived = predictions)
  output <- buildOutputName(fml, modelBuilder$name)
  write.csv(solution, file = output, row.names = FALSE)
  testData$Survived <- predictions
  return(testData)
}

generateAllPredictions <- function(formulas, ModelBuilders, predictionColumn, trainData, testData) {
  for (ModelBuilder in ModelBuilders) {
    for (fml in formulas) {
      evaluation <- generatePredictions(fml, ModelBuilder, predictionColumn, trainData, testData)
    }
  }
}

fml <-Survived ~ Sex + AgeWO + FareWO + Pclass + SibSp + Parch + FamilySize + Embarked + TitleWO + FamilyIDWO
test <- generatePredictions(fml, ConditionalInferenceBuilder, "Survived", train, test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
countBarchart(test, "Survived", "Overall")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultCountBarchart(test, "Sex", "Survived")


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultHistogram(test, "AgeWO", "Survived", 10)


## ---- fig.width = 12, fig.height = 12--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultHistogram(test, "FareWO", "Survived", 10)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
categoricalResultCountBarchart(test, "Pclass", "Survived")

