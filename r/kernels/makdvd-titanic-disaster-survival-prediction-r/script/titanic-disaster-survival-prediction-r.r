
# File path:
list.files(path = "../input")


# Load packages:
library(tidyverse) 
library(readr)
library(tidyverse)
library(ggplot2)

# Read datasets:
train_data <- read_csv("../input/train.csv")
test_data <- read_csv("../input/test.csv")

# See structure of the datasets:
glimpse(train_data)
glimpse(test_data)
# See 3 head observation of the datasets:
head(train_data, n=3)
head(test_data, n=3)

# Find missing data points in train_data:
miss_survived <- any(is.na(train_data$Survived))
miss_pclass <- any(is.na(train_data$Pclass))
miss_sex <- any(is.na(train_data$Sex))
miss_age <- any(is.na(train_data$Age))
# 177 missing value in Age column
miss_age_sum <- sum(is.na(train_data$Age))            
miss_fare <- any(is.na(train_data$Fare))
miss_cabin <- any(is.na(train_data$Cabin))
# 678 missing value in Cabin column
miss_cabin_sum <- sum(is.na(train_data$Cabin))        
miss_embarked <- any(is.na(train_data$Embarked))
# 2 missing value in Embarked column
miss_embarked_sum <- sum(is.na(train_data$Embarked))  

# Remove passengers with missing Embarked or Age:
ctrain_data <- subset(train_data, is.na(Embarked)==FALSE & is.na(Age)==FALSE)

# Extract first letter of Cabin column:
ctrain_data$Cabin <- substring(ctrain_data$Cabin, 1,1)

# Change type of ctrain_data variables to categorical:
ctrain_data$Survived <- as.factor(ctrain_data$Survived)
ctrain_data$Pclass <- as.factor(ctrain_data$Pclass)
ctrain_data$Sex <- as.factor(ctrain_data$Sex)
ctrain_data$Embarked <- as.factor(ctrain_data$Embarked)

# Plot survived vs sex:
ggplot(ctrain_data, aes(x=Sex, fill=Survived))+    
  geom_bar(alpha=0.2)
# Looks like male passengers are more likely to die

# Look at same graph with proportion. 
ggplot(ctrain_data, aes(x=Sex, fill=Survived))+    
  geom_bar(alpha=0.2, position = "fill")
# Sex seems to be significant variable.

 # See age impact:
ggplot(ctrain_data, aes(x=Age, fill=Survived))+    
  geom_density(alpha=0.2)
# Age does not seem to have a direct impact on survival.

# See age and sex variable together:  
ggplot(ctrain_data, aes(x=Sex, y=Age, color=Survived))+
  geom_jitter(alpha=0.5)
# younger male tend to survive.
# Women are more likely to survive. 

# See fare ticket impact: 
ggplot(ctrain_data, aes(x=Fare, fill=Survived))+
  geom_density(alpha=0.2)
# passenger with expensive ticket fares are more likely to survive. 

# Look at class impact: 
ggplot(ctrain_data, aes(x=Pclass, fill=Survived))+
  geom_bar(alpha=0.2)

# Sex and class are correlated. 
ggplot(ctrain_data, aes(x=factor(1), fill=Sex))+
  geom_bar(alpha=0.5, width =1, position ="fill")+
  facet_grid(.~Pclass) +
  coord_polar(theta = "y")
# About 70% of class 3 passenger are male.

# See ticket fare and class together: 
ggplot(ctrain_data, aes(x=Pclass, y=Fare, color=Survived))+
  geom_jitter(alpha=0.5)
# Ticket fare and class are correlated.

# See Embarkation site effect:
ggplot(ctrain_data, aes(x=Embarked, fill=Survived))+
  geom_bar(alpha=0.2)
# here is the same graph with ratio: 
ggplot(ctrain_data, aes(x=Embarked, fill=Survived))+
  geom_bar(alpha=0.2, position="fill")
# Seems Embarked C has higher survival rate. 

# Look at both class and embarked site:  
ggplot(ctrain_data, aes(x=Pclass, color=Survived))+
  geom_bar(alpha=0.5)+
  facet_grid(~Embarked)
# Embarked C has higher ratio of class 1 and class 1 passengers are those who survive. 

# Look at both fare and embarked site: 
ggplot(ctrain_data, aes(y=Fare, color=Survived))+
  geom_boxplot(alpha=0.5)+
  facet_grid(~Embarked) +
  theme(axis.text.x = element_blank())

# Look at Embarked and Sex combination: 
ggplot(ctrain_data, aes(x=Sex, fill=Survived))+
  geom_bar(alpha=0.5)+
  facet_grid(~Embarked)
# Embarked S has higher ratio of male passenger who are more likely to die. 

# Look at Cabine effect:
ggplot(ctrain_data, aes(x=factor(Cabin), fill=Survived))+
  geom_bar(alpha=0.2)
# Notice that almost all passengers in cabine T died
ggplot(ctrain_data, aes(x=factor(Cabin), fill=Survived))+
  geom_bar(alpha=0.2, position="fill")

# Fit logistic regression model without interactions:
fit1 <- glm(Survived~Sex+Age+Pclass+Fare+Cabin+Embarked, data=ctrain_data, family = binomial) 
summary(fit1)


# Drop insignificant variables:
fit2 <- glm(Survived~Sex+Age+Cabin, data=ctrain_data, family = binomial) 
summary(fit2)

# Fit logestic regression model with 2 level interactions:
fit3 <- glm(Survived~Sex+Age+Pclass+Fare+Cabin+Embarked+
              Sex*Age+Sex*Pclass+Sex*Fare+Sex*Embarked+
               Age*Fare+Age*Embarked+
              Fare*Embarked, 
            data=ctrain_data, family = binomial) 
summary(fit3)

# Drop insignificant variables:
fit4 <- glm(Survived~Sex+Age+Age*Fare, data=ctrain_data, family = binomial) 
summary(fit4)

# Chose the best fit: (fit4)
fit4 <- glm(Survived~Sex+Age+Age*Fare, data=ctrain_data, family = binomial)
pchisq(deviance(fit4), df.residual(fit4), lower=TRUE)
# Model is adequate.

# Using Chi test to check if we can drop any variable of fit4:
drop1(fit4,test="Chi")
# No need to drop variables

# Fitted response value with fit4: 
fitted_response <- fitted(fit4)

# Manipulation of test_data:
ctest_data <- test_data[ , -c(1,3,6,7,8)]
ctest_data$Cabin <- substring(ctest_data$Cabin, 1,1)
ctest_data$Pclass <- as.factor(ctest_data$Pclass)
ctest_data$Sex <- as.factor(ctest_data$Sex)
ctest_data$Embarked <- as.factor(ctest_data$Embarked)

# Find missing values in ctest_data:
miss_pclass_t <- any(is.na(ctest_data$Pclass))
miss_sex_t <- any(is.na(ctest_data$Sex))
miss_age_t <- any(is.na(ctest_data$Age))
# 86 missing values in Age column.
miss_age_t_sum <- sum(is.na(ctest_data$Age))
miss_fare_t <- any(is.na(ctest_data$Fare))
# 1 missing value in Fare column.
miss_fare_t_sum <- sum(is.na(ctest_data$Fare))
miss_cabin_t <- any(is.na(ctest_data$Cabin))
# 327 missing values in Cabin column.
miss_cabin_t_sum <- sum(is.na(ctest_data$Cabin))
miss_embarked_t <- any(is.na(ctest_data$Embarked))


# Estimating missing values in age column: 
ggplot(ctrain_data, aes(x=Age, color=Sex))+
  geom_histogram(alpha=0.5)+
  facet_grid(~Pclass)
# median is better estimator for age compared to mean since age distribution is skewed.
# Sex and age are not correlated.
# Age and class are correlated.

# Find medians of age in each class type:
median_age <- train_data %>%
  filter(!is.na(Age)) %>%
  group_by(Pclass) %>%
  summarize(median(Age))

# Estimate age with class type:
ctest_data<- ctest_data %>%
  mutate(Age=ifelse((is.na(Age) & Pclass==1) ,unlist(median_age[1,2]),Age)) %>%
  mutate(Age=ifelse((is.na(Age) & Pclass==2) ,unlist(median_age[2,2]),Age)) %>%
  mutate(Age=ifelse((is.na(Age) & Pclass==3) ,unlist(median_age[3,2]),Age))

# Estimating missing value in fare column:
ggplot(ctrain_data, aes(y=Fare, color=Sex))+
  geom_boxplot(alpha=0.5)+
  facet_grid(~Pclass) +
  theme(axis.text.x = element_blank())
# median is better estimator for ticket fare compared to mean due to skewness.
# ticket fare and class are correlated.

# Find medians of ticket fare in each class type:
median_fare <- train_data %>%
  filter(!is.na(Fare)) %>%
  group_by(Pclass) %>%
  summarize(median(Fare))

# Estimate ticket fare with class type:
ctest_data <- ctest_data %>%
   mutate(Fare=ifelse((is.na(Fare) & Pclass==1) ,unlist(median_fare[1,2]),Fare)) %>%
   mutate(Fare=ifelse((is.na(Fare) & Pclass==2) ,unlist(median_fare[2,2]),Fare)) %>%
   mutate(Fare=ifelse((is.na(Fare) & Pclass==3) ,unlist(median_fare[3,2]),Fare)) 

# Predict ctest_data with fit4: (chance of surviving for each passenger)
fit4 <- glm(Survived~Sex+Age+Age*Fare, data=ctrain_data, family = binomial)
fitted_test <- predict.glm(fit4, ctest_data, type="response")
# Change probability to binary variable: (survived or died)
trs <- function(x, k=0.5){ if (x>=k) {return(1)} else {return(0)} }
predict_test <- lapply(fitted_test, trs)

# My prediction is predict_test :)
head(predict_test, n=20)
