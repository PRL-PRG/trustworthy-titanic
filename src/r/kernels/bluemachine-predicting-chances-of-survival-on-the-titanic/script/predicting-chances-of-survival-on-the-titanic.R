After studying data science and R for almost a year, I thought its time for me to make my first submission. 
To begin with, I picked up this competition. Comments and suggestions are most welcome.
Packages used:
For this submission, I have used the following packages:
ggplot2
ggthemes
dplyr
randomForest
sqldf
stringr

Since I have some PL/SQL development experience too, I have used **sqldf** package in this submission.
It made my work easy and really helped to get some logical information. I however, strongly believe that to learn R, its important to leave the comfort zone and thus I have shared the codes using both sqldf and other functions.

Finally, I too have used random forest classification method at the end to derive the probability of survival of the passengers on the Titanic. 


1. Loading the packages

library("ggplot2")
library("ggthemes")
library("dplyr")
library("randomForest")
library("sqldf")
library("stringr")
2. Merging data into single data frame
Assuming that both test and train CSVs need work in terms of treating missing values, transformation, etc; 
we will merge them into one dataframe first so that these operations need to be performed only once. Before we can merge these data frames, we will have to add survived column to the test data frame.

train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/train.csv", stringsAsFactors = F)
test$Survived <- 0
merged_data <- rbind(train, test)
str(merged_data)

3. Data munging
The next step that I decided to take is to check the structure of the data frame and vectors using different ways and try to find out if there are:
	1) any missing values like NULLs and empty strings
	2) any columns that need some modifications to make more sense

#Using unique function we will see which data needs cleansing/transformation or if there are any missing values.

unique(merged_data$Pclass)
unique(merged_data$Name)
unique(merged_data$Sex)
unique(merged_data$Age)
unique(merged_data$SibSp)
unique(merged_data$Parch)
unique(merged_data$Ticket)
unique(merged_data$Fare)
unique(merged_data$Cabin)
unique(merged_data$Embarked)

I have used SQLDF package to find out the number of missing values(only for the columns that are not OK above)

sqldf("select count(*) from merged_data where Name is null")
sqldf("select count(*) from merged_data where age is null")
sqldf("select count(*) from merged_data where ticket is null")
sqldf("select count(*) from merged_data where fare is null")
sqldf("select count(*) from merged_data where cabin is null")
sqldf("select count(*) from merged_data where embarked is null")
The same can be done using the combination of sum and is.na too
sum(is.na(merged_data$Name))
sum(is.na(merged_data$Age))
sum(is.na(merged_data$Ticket))
sum(is.na(merged_data$Fare))
sum(is.na(merged_data$Cabin))
sum(is.na(merged_data$Embarked))
So, before we can make any sense out of this data we will need to do some cleansing. We will also need to decide which column needs to be picked first since the data is interdependent.

3.1 Missing-data imputation (Age)
There are a lot of missing values in the dataset and age is an important factor that can contribute to the survival of the passengers.
you can use different functions in different packages(for example mice) for imputation based on the missing data and its relationship with other columns. 
I have however, created a function here based on the observed data for age. 

random.imp <- function(a) {
    missing <- is.na(a)
    count.missing <- sum(missing)
    not_missing.data <- a[!missing]
    imputed <- a
    imputed[missing] <- sample(not_missing.data, count.missing, replace = TRUE)
    return(imputed)
}
Age <- random.imp(merged_data$Age)
imputed_age <- data.frame(Age)
Lets create a graph to see if the imputed data for Age looks similar to the non-imputed data. if it will then we can say that our imputation function has worked well.
par(mfrow = c(1, 2))
hist(merged_data$Age, freq = F, main = "Age: Original Data", col = "darkgreen", ylim = c(0, 0.04))
hist(imputed_age$Age, freq = F, main = "Age: Imputed Output", col = "lightgreen", ylim = c(0, 0.04))
The output of the graph looks good and therefore this function suits our requirement. We will thus update the age with the new imputed age.
merged_data$Age <- imputed_age$Age
3.2 Extracting surnames

we need the surname to find out the family size given the fact that we do have the columns Parch and SibSp collumns to support this value. I have used strsplit function and extracted everything before the comma(,). The output can be properly formatted using sapply. below are the two ways in which this can be achieved.

Surname <- sapply(strsplit(merged_data$Name, ","), `[`, 1)
merged_data$Surname <- Surname
3.3 Extracting Titles
We do have the column sex to know the gender of the passenger. But to understand if there were any male or female kids in the ship too, we need to extract the titles.
Extracting titles is a bit tricky. Here we have used str_locate() to find the beginning and end of the Title. beginning must be 2 spaces after the comma(,). So we have used +2.
End should be one space before the dot(.) so we have used -1. [,1] has been used for all the rows.

title <- str_sub(merged_data$Name, str_locate(merged_data$Name, ",")[, 1] + 2, str_locate(merged_data$Name, "\\.")[, 1] - 1)
merged_data$Titles <- title
3.3.1: Simplifying the titles
For the sake of simplicity, I decided to have only 4 titles viz. Mr., Mrs., Master and Miss.

unique(merged_data$Titles)
merged_data$Titles <- gsub("Dona|Lady|Madame|the Countess|Mme", "Mrs", merged_data$Titles)
merged_data$Titles <- gsub("Don|Jonkheer|Sir|Dr|Major|Col|Capt|Rev", "Mr", merged_data$Titles)
merged_data$Titles <- gsub("Ms|Mlle", "Miss", merged_data$Titles)
unique(merged_data$Titles)
Looks good now.
merged_data$Titles <- as.factor(merged_data$Titles)
3.4. Age category
Its natural to think that people of certain age may have higher survival rate as compared to others. We therefore would create the age category as well

merged_data$Age_category[merged_data$Age < 18] <- "Child"
merged_data$Age_category[merged_data$Age >= 18] <- "Adult"

3.5. Missing fare
Because the fare can depend on Age category and title, we can now find the missing fare too even though there is just one missing value and it may not have a huge impact on our prediction.

sqldf("select * from merged_data where fare is null")

We see that passengerid 1044 has missing fare. I am sharing two methods here.

merged_data$Fare[1044] <- mean(merged_data[merged_data$Pclass == "3" & merged_data$Embarked == "S" & merged_data$Age_category == "Adult" & merged_data$Titles == "Mr", ]$Fare, na.rm = TRUE)

3.6. Missing Embarkment values
We can find the missing embarkment values using sqldf function or which function.

sqldf("select * from merged_data where embarked isnull or Embarked=''")

The next step is to find the average fare nearest to 80 keeping in mind that the passenger is a female travelling in 1st class and category is adult. I have shared two methods below

sqldf("select avg(fare),embarked from merged_data where Pclass=1 and Age_category ='Adult' and Sex='female' and Ticket!=113572 group by embarked")
mean(merged_data[merged_data$Pclass == "1" & merged_data$Age_category == "Adult" & merged_data$Sex == "female" & merged_data$Embarked == "C" & merged_data$Ticket != 113572, ]$Fare, na.rm = FALSE)
mean(merged_data[merged_data$Pclass == "1" & merged_data$Age_category == "Adult" & merged_data$Sex == "female" & merged_data$Embarked == "Q" & merged_data$Ticket != 113572, ]$Fare, na.rm = FALSE)
mean(merged_data[merged_data$Pclass == "1" & merged_data$Age_category == "Adult" & merged_data$Sex == "female" & merged_data$Embarked == "S" & merged_data$Ticket != 113572, ]$Fare, na.rm = FALSE)

We see that the average fare at Embankment Q is closest to 80. Thus we insert Q into the missing embankment values

merged_data$Embarked[merged_data$Embarked == "" & merged_data$Ticket == "113572"] <- "Q"
3.7 impact of family size on survival
Its very natural to think that in a situation where its the question of life, family size does matter. 
Lets first create the new column Fsize and then later on in section 4, check if it can help us in improving our prediction or not.

merged_data$Fsize <- sqldf("select (sibsp+parch+1) family_size from merged_data")
merged_data$Fsize <- merged_data$SibSp + merged_data$Parch + 1
str(merged_data$Fsize)

3.8. Sorting out the cabin names
cabin names are a bit un-formatted. So, I have cleaned up this column a bit.

merged_data$cabin_name <- str_sub(merged_data$Cabin, 1, 1)
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Titles", "Surname", "Fsize")
merged_data[factor_vars] <- lapply(merged_data[factor_vars], function(x) as.factor(x))

4. Finding the relationship between survival and different columns
I'm now done with cleaning up the data. I have created new column that I think are required and took care of missing values mostly(except cabin).
At this stage I decided to ask myself the questions that may influence the prediction.


Q1: has the travel class influenced the rate of survival?
Based on the outcome below, its a clear that passenger class has big impact on the chances on survival.

survived_by_class <- sqldf(c("select sum(survived) total_survived,count(survived) total_passengers, ((sum(survived)*100/count(survived))) percentage_survived, PClass from merged_data where passengerid<=891 group by PClass order by PClass"))
barplot(as.matrix(survived_by_class$percentage_survived), col = rainbow(11), beside = T, ylab = "Percent_survived", xlab = "Class", names.arg = c("1", "2", "3"), axis.lty = 1, offset = 0, ylim = c(0, 100))
legend("topright", c("1", "2", "3"), cex = 0.9, bty = "n", fill = rainbow(5))

Q2: Has gender influenced the rate of survival?
Based on the outcome below, its a clear that gender has a big impact on the chances on survival.

survived_by_gender <- sqldf(c("select sum(survived) total_survived,count(survived) total_passengers, ((sum(survived)*100/count(survived))) percentage_survived, Sex from merged_data where passengerid<=891 group by Sex order by Sex"))
barplot(as.matrix(survived_by_gender$percentage_survived), col = rainbow(11), beside = T, ylab = "Percent_survived", xlab = "Gender", names.arg = c("FEMALE", "MALE"), axis.lty = 1, offset = 0, ylim = c(0, 100))
legend("topright", c("MALE", "FEMALE"), cex = 0.9, bty = "n", fill = rainbow(5))

Q3: has age played any role?
From the output below it looks like that there is a bit of influence here.

survived_by_age <- sqldf(c("select Age_category, sum(survived) as Survived, (sum(survived)*100)/count(passengerid) as pct_survived from merged_data where (survived =1 or survived =0) and passengerid<=891 group by Age_category"))
barplot(as.matrix(survived_by_age$pct_survived), col = rainbow(11), beside = T, ylab = "%survived", xlab = "Age", names.arg = c("Adult", "Child"), axis.lty = 1, offset = 0, ylim = c(0, 100))
legend("topright", c("Adult", "Child"), cex = 0.6, bty = "n", fill = rainbow(5))

Q4: Has familysize any impact on chances of survival?
The output is really quite interesting.  Passengers with no family have a low survival rate. Passengers with a family size of 2, 3 and 4 have very high chances. Finally, passengers with family size of 5,6,7,8 and 11 have a low rate of survival. This really makes sense. So, the family size dues infulence the chances of survival.

survived_by_family <- sqldf(c("select sum(survived) total_survived,count(survived) total_passengers, ((sum(survived)*100/count(survived))) percentage_survived, Fsize from merged_data where passengerid<=891 group by Fsize order by Fsize"))
barplot(as.matrix(survived_by_family$percentage_survived), col = rainbow(11), beside = T, ylab = "Percent_survived", xlab = "Family_size", names.arg = c("1", "2", "3", "4", "5", "6", "7", "8", "11"), axis.lty = 1, offset = 0, ylim = c(0, 100))
legend("topright", c("1", "2", "3", "4", "5", "6", "7", "8", "11"), cex = 0.6, bty = "n", fill = rainbow(5))

Q5: Does being a mother impacted the survival rate?
There is a bit of impact it seems although not much.

merged_data$Mother <- "Not Mother"
merged_data$Mother[merged_data$Sex == "female" & merged_data$Parch > 0 & merged_data$Age > 18 & merged_data$Title != "Miss"] <- "Mother"
survived_by_mother <- sqldf(c("select Mother as is_mother,sum(survived) total_survived,count(passengerid) total_female_passengers, ((sum(survived)*100/count(passengerid))) percentage_females_survived from merged_data where passengerid<=891 and sex ='female' group by Mother order by mother"))
barplot(as.matrix(survived_by_mother$percentage_females_survived), col = rainbow(11), beside = T, ylab = "Percent_survived", xlab = "Is_Mother", names.arg = c("Mother", "Not_mother"), axis.lty = 1, offset = 0, ylim = c(0, 100))
legend("topright", c("Mother", "Not_mother"), cex = 0.9, bty = "n", fill = rainbow(5))

Q6: Does being a father impacted the survival rate?
The results clearly shows that the rat of survivals of the fathers are almost 50% to that of non-fathers

merged_data$Father <- "Not Father"
merged_data$Father[merged_data$Sex == "male" & merged_data$Parch > 0 & merged_data$Age > 18 & merged_data$Title != "Master"] <- "Father"
table(merged_data$Father, merged_data$Survived)
survived_by_father <- sqldf(c("select Father as is_father,sum(survived) total_survived,count(passengerid) total_male_passengers, ((sum(survived)*100/count(passengerid))) percentage_males_survived from merged_data where passengerid<=891 and sex ='male' group by Father order by Father"))
barplot(as.matrix(survived_by_father$percentage_males_survived), col = rainbow(11), beside = T, ylab = "Percent_survived", xlab = "Is_Father", names.arg = c("Father", "Not_Father"), axis.lty = 1, offset = 0, ylim = c(0, 100))
legend("topright", c("Father", "Not_Father"), cex = 0.9, bty = "n", fill = rainbow(5))

6.Splitting the data

merged_data$Mother <- factor(merged_data$Mother)
merged_data$Father <- factor(merged_data$Father)
merged_data$Pclass <- factor(merged_data$Pclass)
merged_data$Sex <- factor(merged_data$Sex)
merged_data$Embarked <- factor(merged_data$Embarked)
merged_data$Titles <- factor(merged_data$Titles)
merged_data$Age_category <- factor(merged_data$Age_category)
train <- merged_data[1:891, ]
test <- merged_data[892:1309, ]

7. Building the model
Finally, its time to build the prediction model based on the factors that contributes the most.

set.seed(123)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + Fsize + Father + Mother + Titles + Age_category, data = train)
plot(rf_model, ylim = c(0, 0.36))
legend("topright", colnames(rf_model$err.rate), col = 1:3, fill = 1:3, border = 1)

8. Ranking the prediction variables
We would now see the ranking given to the variables that we have chosen to build the model

importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[, "MeanDecreaseGini"], 2))
rankImportance <- varImportance %>% mutate(Rank = paste0("#", dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat = "identity") + geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust = 0, vjust = 0.55, size = 4, colour = "red") + labs(x = "Variables") + coord_flip() + theme_few()

9. Making the prediction
We will not predict the survival of passengers from test dataset based on the model that we have created.

prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = "rf_mod_Solution.csv", row.names = F)
