rm(list = ls())
library(ggplot2)
library(gridExtra)
library(dplyr)
path <- "../data/"
if (!dir.exists(path)) {
    path <- "../input/"
}
load_data <- function(file) {
    return(read.csv(paste0(path, file)))
}
train <- load_data("train.csv")
test <- load_data("test.csv")
test$Survived <- rep(NA, nrow(test))
comb <- rbind(train, test)
train_index <- comb$PassengerId[!is.na(comb$Survived)]
test_index <- comb$PassengerId[is.na(comb$Survived)]
comb$Set <- ifelse(comb$PassengerId %in% train_index, "Train", "Test")
dftotal <- comb %>% filter(Survived == 0 | Survived == 1) %>% select(Survived, Sex, Set)
dfsurvived <- comb %>% filter(Survived == 1) %>% select(Survived, Sex, Set)
dftotal$count <- "Total"
dfsurvived$count <- "Survived"
df <- rbind(dftotal, dfsurvived)
ggplot(df, aes(Sex, fill = count)) + geom_bar(position = "dodge") + labs(title = "Survival per Gender in Train Set") + xlab("Gender")
total <- 2224
killed <- 1502
true_survival_rate <- 100 * (total - killed)/total
pop_count <- comb %>% filter(Set == "Train") %>% count(Sex)
pop_surv_count <- comb %>% filter(Set == "Train" & Survived == 1) %>% count(Sex)
surv_likelihood <- pop_surv_count$n/pop_count$n
comb$Prediction <- ifelse(surv_likelihood[comb$Sex] > 0.5, 1, 0)
train_survival_rate <- 100 * sum(pop_surv_count$n)/sum(pop_count$n)
print(sprintf("Train set survival rate %2.1f percent", train_survival_rate))
print(sprintf("Survival rate overestimation in train set = %2.1f percent", train_survival_rate - true_survival_rate))
map.gen <- c(female = 1, male = 2)
weight <- ifelse(comb$Set == "Train", 100/length(train_index), 100/length(test_index))
ggplot(comb, aes(x = Sex, fill = Set)) + geom_bar(position = "dodge", aes(weight = weight)) + labs(title = "Gender Distribution in Train and Test Sets", x = "Gender", y = "Percent")
print(sprintf("Female to population ratio in train set = %2.1f percent", 100 * pop_count$n[map.gen["female"]]/sum(pop_count$n)))
print(sprintf("Male   to population ratio in train set = %2.1f percent", 100 * pop_count$n[map.gen["male"]]/sum(pop_count$n)))
train_gender_survived <- table(train$Sex[train$Survived == 1])
print(sprintf("Female survival rate in train set = %2.1f percent", 100 * pop_surv_count$n[map.gen["female"]]/pop_count$n[map.gen["female"]]))
print(sprintf("Male   survival rate in train set = %2.1f percent", 100 * pop_surv_count$n[map.gen["male"]]/pop_count$n[map.gen["female"]]))
test_pop_count <- comb[test_index, ] %>% count(Sex)
print(sprintf("Female to population ratio in test set = %2.1f percent", 100 * test_pop_count$n[map.gen["female"]]/sum(test_pop_count$n)))
print(sprintf("Male   to population ratio in test set = %2.1f percent", 100 * test_pop_count$n[map.gen["male"]]/sum(test_pop_count$n)))
test_expected_surv = test_pop_count$n * surv_likelihood
accuracy <- ifelse(surv_likelihood > 0.5, surv_likelihood, 1 - surv_likelihood)
print(sprintf("Optimal gender only predicted score            %2.4f ", sum(test_pop_count$n * accuracy)/sum(test_pop_count$n)))
print(sprintf("Actual leader board (LB) score on the test set %2.4f ", 0.76555))
print(sprintf("Train set relative overstimation on LB         %2.4f ", (sum(test_pop_count$n * accuracy)/sum(test_pop_count$n)/0.76555)))
submit <- data.frame(PassengerId = test_index, Survived = comb$Prediction[test_index])
write.csv(submit, file = paste0("gender_only.csv"), row.names = FALSE, quote = F)
