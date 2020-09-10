	# Loading the caret library...
library(caret)
library(caretEnsemble)
											# PREPARATION |
	# Reading in the training and testing cohorts!
test <- read.csv("../input/test.csv", colClasses = c("numeric",rep("factor",3), rep("numeric",3), "factor", "numeric", rep("factor",2)))
train <- read.csv("../input/train.csv", colClasses = c("numeric",rep("factor",4), rep("numeric",3), "factor", "numeric", rep("factor",2)))
summary(train) # It looks like Embarked has 2 missing... and age has 177!
						# It seems like most embark from Southampton.
train$Embarked[train$Embarked == ""] <- "S" 						# Let's impute that.
hist(train$Age)														# According to the summary -- and this -- age has a bit of a skew, with a max of 80!
train$Age[is.na(train$Age)] <- median(train$Age, na.rm=TRUE)		# So, let's impute the median, not the mean.

											# PASSENGER ID | X
	# Is passenger ID relevant? Meaning, were they lended out in an order where it correlated with survival (inversely or positively?)
boxplot(train$PassengerId~train$Survived)	# No. The quartiles are a little more narrow for those who survived, but the mean is about the same and shows no sort of ordering.

											# PCLASS | O
	# Do people in a certain passenger class survive more or less than others?
temp <- chisq.test(train$Pclass,train$Survived)
		# 	Pearson's Chi-squared test
		# 
		# data:  train$Pclass and train$Survived
		# X-squared = 102.89, df = 2, p-value < 2.2e-16	
	# Apparently!
summary(train$Pclass[train$Survived == 0])
			#  1   2   3 
			# 80  97 372 
summary(train$Pclass[train$Survived == 1])
			# 1   2   3 
			# 136  87 119 
temp$observed
temp$expected	# In particular, being in passenger class 3 isn't good for getting off the Titanic.

										# SEX | O
	# Did women really get on the lifeboats disproportionally more than men?
chisq.test(train$Sex,train$Survived)

		# 	Pearson's Chi-squared test with Yates' continuity correction
		#
		# data:  train$Sex and train$Survived
		# X-squared = 260.72, df = 1, p-value < 2.2e-16
	# Hugely!

										# AGE | X
	# Are there patterns in a person's age and survival?
boxplot(train$Age~train$Survived)
	# Hmm, they look a bit different. However, we imputed a lot of datapoints here. Anything else?
wilcox.test(train$Age ~ train$Survived)
		# 	Wilcoxon rank sum test with continuity correction
		# 
		# data:  train$Age by train$Survived
		# W = 97978, p-value = 0.2697
		# alternative hypothesis: true location shift is not equal to 0
	# Doesn't look like it!
	
										# SIBLINGS/SPOUSES | O
	# Does the number of siblings or spouses change survival?
boxplot(train$SibSp~train$Survived)
	# Honestly, it doesn't look like it; too few people have more than 1-2 on board!
wilcox.test(train$SibSp ~ train$Survived)
		# 	Wilcoxon rank sum test with continuity correction
		# 
		# data:  train$SibSp by train$Survived
		# W = 85775, p-value = 0.008017
		# alternative hypothesis: true location shift is not equal to 0
	# Tests positively. What are the means, since those are being tested?
mean(train$SibSp[train$Survived == 1]) # 0.4736842
mean(train$SibSp[train$Survived == 1]) # 0.5537341
	# So, the means ARE technically different!
train$SibSpBinary <- 0	# Let's recode it to a factor, where 1 = 1 or more siblings/spouses.
train$SibSpBinary[train$SibSp > 0] <- 1
chisq.test(train$SibSpBinary,train$Survived)
	# Still looks good!
		# Pearson's Chi-squared test with Yates' continuity correction
		#
		# data:  train$SibSpBinary and train$Survived
		# X-squared = 11.456, df = 1, p-value = 0.0007128
	
										# PARENTS/CHILDREN | O
	# Do the number of parents or children change survival?
boxplot(train$Parch~train$Survived)	# Wow, that looks very different!
wilcox.test(train$Parch ~ train$Survived)
		# Wilcoxon rank sum test with continuity correction
		# 
		# data:  train$Parch by train$Survived
		# W = 82385, p-value = 3.712e-05
		# alternative hypothesis: true location shift is not equal to 0
	# Looks like a very significant difference.
	
										# FARE | O
	# How about the cost of fare?
boxplot(train$Fare~train$Survived) # It looks like those who survived often paid more for tickets.
wilcox.test(train$Fare ~ train$Survived)
			# Wilcoxon rank sum test with continuity correction
			# 
			# data:  train$Fare by train$Survived
			# W = 57806, p-value < 2.2e-16
			# alternative hypothesis: true location shift is not equal to 0
	# Looks like it! Does it have any relation to passenger class?
kruskal.test(train$Fare ~ train$Pclass)
			# Kruskal-Wallis rank sum test
			# 
			# data:  train$Fare by train$Pclass
			# Kruskal-Wallis chi-squared = 438.04, df = 2, p-value < 2.2e-16
	# Hugely!
boxplot(train$Fare ~ train$Pclass)
	# Passengers in class 1, (who we know tend to survive more) pay a pretty penny. Multicollinear? Maybe.

										# EMBARKED | O
	# Anything special about the port they embarked from? We know Southampton is where most come from!
temp <- chisq.test(train$Embarked,train$Survived)
		# Pearson's Chi-squared test
		# 
		# data:  train$Embarked and train$Survived
		# X-squared = 25.964, df = 2, p-value = 2.301e-06
train$observed
train$expected
	# Actually, more people from Cherbourg died than expected.

	# Now, to look at the more complex covariates we can add.
										# NAMES
	# Names. We always see first and last names. 
	# We also see titles, aliases, nicknames, middle names and names of varying lengths.
											# TITLES | O
train$Title <- gsub("\\..*", "", train$Name)
train$Title <- as.factor(gsub(".*, ", "", train$Title))
summary(train$Title)	# It seems like most people who aren't Mr, Mrs and Miss have higher social ranks...
train$Title[train$Title == "Mlle" | train$Title == "Ms"] <- "Miss"
train$Title[train$Title == "Mme"] <- "Mrs"	# Grouping the ladies together.
train$Title <- as.character(train$Title)
train$Title[train$Title != "Mrs" & train$Title != "Miss" & train$Title != "Mr" & train$Title != "Master"] <- "Other"
train$Title <- as.factor(train$Title)
temp <- chisq.test(train$Title, train$Survived)
		# 	Pearson's Chi-squared test
		# 
		# data:  train$Title and train$Survived
		# X-squared = 288.12, df = 4, p-value < 2.2e-16
temp$observed
temp$expected	# Well, we know well that more women survived than men. But it also looks like more married men and women lived, too!
											# ALIASES | X
	# Some people have other names. They seem to occasionally include a nickname, or perhaps their maiden name.
train$Alias <- 0
train$Alias[grepl("\\(", train$Name)] <- 1 
train$Alias <- as.factor(train$Alias)
# 	Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  train$Alias and train$Survived
# X-squared = 105.04, df = 1, p-value < 2.2e-16
temp <- chisq.test(train$Alias, train$Survived)
temp$observed
temp$expected	# So, looks like if you had an alias you were disproportionately likely to live. But it seems a bit fishy...
temp <- chisq.test(train$Alias, train$Title) # (Ignoring the error, just looking at observed)
temp$observed	# There's the answer; aliases were mostly maiden names held by Mrs.
											# NICKNAMES | X
train$NickName <- 0
train$NickName[grepl("\"", train$Name)] <- 1 
temp <- chisq.test(train$NickName, train$Survived)
			# Pearson's Chi-squared test with Yates' continuity correction
			# 
			# data:  train$NickName and train$Survived
			# X-squared = 24.968, df = 1, p-value = 5.83e-07
	# Same question as above.
temp$observed	# There really are precious few with nicknames; 53, out of 891 variables, most of them Miss followed by Mr).
											# SURNAMES | X
train$Surname <- as.factor(gsub(",.*", "", train$Name))
summary(train$Surname)	# So : Looks like a sum of people share a surname with others. The issue is that it might run into our other Parch/SibSp variables...
train$SurnameBinary <- 0
train$SurnameBinary[duplicated(train$Surname)] <- 1
chisq.test(train$SurnameBinary, train$Survived)
		# Pearson's Chi-squared test with Yates' continuity correction
		# 
		# data:  train$SurnameBinary and train$Survived
		# X-squared = 0.31246, df = 1, p-value = 0.5762
	# Surprisingly, a hard no! Does this predict some issue with our other relation variables?

											# CABIN | X
	# This one is tricky. It's hugely sparse, and some people have more than one!
train$Cabin <- as.factor(substr(train$Cabin, 0, 1))	# Let's just take the first cabin listed (assuming the higher floor is "A")
summary(train$Cabin)	# 687 don't have one listed, and it seems like it's distributed around deck C, left-skewed.
train$Cabin[train$Cabin == ""] <- NA
chisq.test(train$Cabin, train$Survived)
			# Pearson's Chi-squared test
			# 
			# data:  train$Cabin and train$Survived
			# X-squared = 10.301, df = 7, p-value = 0.1722
	# As it stands, no, and not worth imputing.

											# TICKET | X
	# Everyone has a ticket, and there are some patterns. However, it's a bit tricky to mine.
	# It's easier to mine the prefixes than the numeric values.
train$TicketPrefix <- toupper(gsub(" .*$", "", train$Ticket)) 	# Firstly, let's separate those prefixes
train$TicketPrefix <- gsub("\\.", "", train$TicketPrefix) 	# Removing periods for unity...
train$TicketPrefix <- gsub("/", "", train$TicketPrefix) 	# ...and slashes...
train$TicketPrefix <- as.factor(gsub("[0-9]", "", train$TicketPrefix))	# ...and numbers!
train$TicketNumber <- as.numeric(gsub("[^0-9]", "", train$Ticket))	# Just number! Maybe I'll play with this, someday.
summary(train$TicketPrefix)	# There is a large mix of IDs attached. What if we just make it a binary variable?
train$HasPrefix <- 0
train$HasPrefix [train$TicketPrefix != ""] <- 1
chisq.test(train$HasPrefix, train$Survived)	# Absolutely not.

											# BUILDING A MODEL
	# Let's see what can actually help the model.
# model <- train(Survived ~ Pclass + Sex + SibSpBinary + Parch + Fare + Embarked + Title, data = train, method="rf")
# varImp(model)	# Looks like everything contributed to the model (relatively) with the exception of the Embarked (from) flag, so let's remove it. SibSp also doesn't seem to necessary.
model <- train(Survived ~ Pclass + Sex + Parch + Fare + Title, data = train, method="rf")

											# FITTING A MODEL
summary(test)
test$Embarked[test$Embarked == ""]	<- "S"
test$Age[is.na(test$Age)] <- median(test$Age, na.rm=TRUE)
test$Fare[is.na(test$Fare)] <- median(test$Fare, na.rm=TRUE)
test$Title <- as.character(gsub("\\..*", "", test$Name))
test$Title <- gsub(".*, ", "", test$Title)
test$Title[test$Title == "Mlle" | test$Title == "Ms"] <- "Miss"
test$Title[test$Title != "Mrs" & test$Title != "Miss" & test$Title != "Mr" & test$Title != "Master"] <- "Other"
test$Title <- as.factor(test$Title)
test$Survived <- predict(model, test)
write.csv(test[c(1,13)], "predictions.csv", row.names = FALSE)