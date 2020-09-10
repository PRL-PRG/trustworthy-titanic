
fr_train = read.csv('../input/train.csv', na.strings='')
fr_test = read.csv('../input/test.csv', na.strings='')

fr_test$Survived = rep(NA, nrow(fr_test))
fr_train$Set = rep("train", nrow(fr_train))
fr_test$Set = rep("test", nrow(fr_test))

fr_titanic = rbind(fr_train, fr_test)

dim(fr_train)
head(fr_train)

summary(fr_train)

naSummary = function(df) {
    naCount = sapply(df, function(col) {
        sum(is.na(col))
    })
    
    return (data.frame(naCount, naPc=naCount/nrow(df)))
}

naSummary(fr_titanic)

str(fr_titanic)

fr_titanic$Survived = as.factor(fr_titanic$Survived)
fr_titanic = subset(fr_titanic, select=-c(PassengerId))

str(fr_titanic)

mcl = fr_titanic$Embarked[which.max(fr_titanic$Embarked)]
fr_titanic$Embarked[which(is.na(fr_train$Embarked))] = mcl

naSummary(fr_titanic)

fr_titanic$Fare[which(is.na(fr_titanic$Fare))] = mean(fr_titanic$Fare, na.rm=TRUE)
naSummary(fr_titanic)

getTitle = function(name) {
    postcom = trimws(strsplit(as.character(name), ',')[[1]][2])
    title = strsplit(postcom, ' ')[[1]][1]
    return (substr(title, 1, nchar(title)-1))
}

fr_titanic$Title = as.factor(sapply(fr_titanic$Name, getTitle))
fr_titanic = fr_titanic[c("Survived", 
                        "Pclass",  
                        "Name",
                        "Title",
                        "Sex", 
                        "Age", 
                        "SibSp", 
                        "Parch", 
                        "Ticket", 
                        "Fare", 
                        "Cabin", 
                        "Embarked",
                        "Set")]

head(fr_titanic)

table(fr_titanic$Title)

mr_alias = c('Dr', 'Rev', 'Major', 'Col', 'Jonkheer', 'Don', 'Sir', 'Capt')
mrs_alias = c('Dona', 'Lady', 'Mme', 'th')
miss_alias = c('Mlle', 'Ms')

fr_titanic$Title[which(fr_titanic$Title %in% mr_alias)] = 'Mr'
fr_titanic$Title[which(fr_titanic$Title %in% mrs_alias)] = 'Mrs'
fr_titanic$Title[which(fr_titanic$Title %in% miss_alias)] = 'Miss'

fr_titanic$Title = droplevels(fr_titanic$Title)
summary(fr_titanic$Title)

rr_train = subset(fr_titanic, select=-c(Survived, Name, Ticket, Cabin, Set))
rr_train = na.omit(rr_train) # Omits test data as Survived values are all NA's

head(rr_train)

lm.fit = lm(Age ~ ., data=rr_train)
summary(lm.fit)

plot(as.factor(rr_train$Pclass), rr_train$Age, xlab="Passenger Class", ylab="Age")

plot(as.factor(rr_train$SibSp), rr_train$Age, xlab="# siblings + spouses", ylab="Age")

lm.fit = lm(Age ~ Pclass + Title + SibSp + Embarked, data=rr_train)

summary(lm.fit)

missing_age_masters = which(fr_titanic$Title == 'Master' & is.na(fr_titanic$Age))
fr_titanic[missing_age_masters, ]

set.seed(10) # this makes the kernel results reproducible

det_imputed = predict(lm.fit, fr_titanic[which(is.na(fr_titanic$Age)), ])
random_imputed = rnorm(length(det_imputed), det_imputed, abs(residuals(lm.fit)))

# We need to round the values to integers and floor them at a value of 1
det_imputed[which(det_imputed < 0)] = 1
det_imputed = round(det_imputed)

random_imputed[which(random_imputed < 0)] = 1
random_imputed = round(random_imputed)

par(mfrow=c(3,1))
hist(rr_train$Age, breaks=10, freq=F)
hist(det_imputed, breaks=10, freq=F)
hist(random_imputed, breaks=10, freq=F)

library(ggplot2)

kde_mat = rbind(cbind(rr_train$Age, rep("Original", length(rr_train$Age))),
        cbind(det_imputed, rep("Deterministic Imputation", length(det_imputed))),
        cbind(random_imputed, rep("Randomised Imputation", length(random_imputed))))

kde_df = data.frame(Age=as.numeric(kde_mat[,1]), Source=as.factor(kde_mat[,2]))

ggplot(kde_df, aes(x=Source, y=Age, fill=Source)) + geom_violin()

fr_titanic$Age[which(is.na(fr_titanic$Age))] = random_imputed
naSummary(fr_titanic)

fr_titanic[missing_age_masters,]

library(reshape2)

fr_titanic$hasCabin = as.factor(!is.na(fr_titanic$Cabin))

# Make a convenience feature which is directly descriptive
Survived_Desc = ifelse(fr_titanic$Survived == 1, "Survived", "Died")

freq = table(fr_titanic$hasCabin, Survived_Desc)
freq_df = as.data.frame.matrix(freq)

freq_df = data.frame(Cabin=row.names(freq_df), freq_df)
freq_df = melt(freq_df, id.vars="Cabin")

ggplot(freq_df, aes(x=Cabin, y=value)) + geom_bar(aes(fill = variable), position = "dodge", stat="identity")

fr_titanic$Deck = sapply(fr_titanic$Cabin, function(cabin) {
    substr(as.character(cabin), 1, 1)
})

fr_titanic$Deck[which(is.na(fr_titanic$Deck))] = "None"
fr_titanic$Deck = as.factor(fr_titanic$Deck)

summary(fr_titanic$Deck)

fr_titanic$Deck[which(fr_titanic$Deck == 'T')] = as.factor('None')
fr_titanic$Deck = droplevels(fr_titanic$Deck)

freq = table(fr_titanic$Deck, Survived_Desc)
freq_df = as.data.frame.matrix(freq)
freq_df = data.frame(Deck=row.names(freq_df), freq_df)

freq_df = melt(freq_df, id.vars="Deck")

ggplot(freq_df, aes(x=Deck, y=value)) + geom_bar(aes(fill = variable), position = "dodge", stat="identity")

inGroup = as.factor(duplicated(fr_titanic$Ticket))

freq_df = as.data.frame.matrix(table(inGroup, Survived_Desc))
freq_df = data.frame(inGroup=row.names(freq_df), freq_df)

freq_df = melt(freq_df, id.vars="inGroup")

ggplot(freq_df, aes(x=inGroup, y=value)) + geom_bar(aes(fill = variable), position = "dodge", stat="identity")

fr_titanic$inGroup = inGroup

fr_train = fr_titanic[which(fr_titanic$Set == 'train'), ]

uniq_ages = sort(unique(fr_train$Age))

survival_rates_by_age = sapply(uniq_ages, function(age) {
    mean(fr_train[which(fr_train$Age == age), ]$Survived == 1)
})

uniq_ages

fr_train$Age = round(fr_train$Age)
uniq_ages = sort(unique(fr_train$Age))

survival_rates_by_age = sapply(uniq_ages, function(age) {
    selection = fr_train[which(fr_train$Age == age), ]$Survived
    mean(selection == 1)
})

age_surv_rate_df = data.frame(Age=uniq_ages, SurvivalRate=survival_rates_by_age)
ggplot(age_surv_rate_df, aes(x=Age, y=SurvivalRate)) + geom_col()

age_bins = split(uniq_ages, ceiling(seq_along(uniq_ages)/5))
binned_surv_rate = sapply(age_bins, function(bin) {
    mean(age_surv_rate_df[which(age_surv_rate_df$Age %in% bin), ]$SurvivalRate)
})

binned_age_surv_df = data.frame(AgeBin=factor(names(age_bins), levels=names(age_bins)), SurvivalRate=binned_surv_rate)
ggplot(binned_age_surv_df, aes(x=AgeBin, y=SurvivalRate)) + geom_col()

fr_titanic$AgeGroup = sapply(fr_titanic$Age, function(age) {
    if (age < 5) {
        return (1)
    } else if (age >= 5 && age < 18) {
        return (2)
    } else {
        return (3)
    }
})

table(as.factor(fr_titanic$AgeGroup))

fr_train = fr_titanic[which(fr_titanic$Set == 'train'),]
fr_test = fr_titanic[which(fr_titanic$Set == 'test'),]

fr_train = subset(fr_train, select=-c(Name, Ticket, Set, Cabin))
fr_test = subset(fr_test, select=-c(Name, Ticket, Set, Cabin, Survived))

head(fr_train)
str(fr_train)
str(fr_test)

library(e1071)

set.seed(7)

tuned = tune(svm, Survived ~ ., data=fr_train, kernel="linear", scale=TRUE, 
             ranges=list(cost=seq(0.3, 0.6, length=20)))

summary(tuned)
svm.fit = tuned$best.model

library(randomForest)

set.seed(7)
rf.fit = randomForest(Survived ~ ., data=fr_train)
summary(rf.fit$err.rate)

# Thanks to Megan Risdal's excellent Titanic kernel for this little error rate graph
plot(rf.fit, ylim=c(0,0.36))
legend('top', colnames(rf.fit$err.rate), col=1:3, fill=1:3, bty="n")

varImpPlot(rf.fit)

rf.preds = predict(rf.fit, fr_test)
rf.submission = data.frame(PassengerId=names(rf.preds), Survived=rf.preds)
write.csv(rf.submission, file="rf_submission.csv", row.names=FALSE)

svm.preds = predict(svm.fit, fr_test)
svm.submission = data.frame(PassengerId=names(svm.preds), Survived=svm.preds)
write.csv(svm.submission, file="svm_submission.csv", row.names=FALSE)
