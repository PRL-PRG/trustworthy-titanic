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
dftotal <- comb %>% filter(Survived == 0 | Survived == 1) %>% select(Survived, Pclass, Set)
dfsurvived <- comb %>% filter(Survived == 1) %>% select(Survived, Pclass, Set)
dftotal$count <- "Total"
dfsurvived$count <- "Survived"
df <- rbind(dftotal, dfsurvived)
plot1 <- ggplot(df, aes(Pclass, fill = count)) + geom_bar(position = "dodge") + labs(title = "Survival per Class") + xlab("Class")
pop_count <- dftotal %>% count(Pclass)
pop_surv_count <- dfsurvived %>% count(Pclass)
surv_likelihood <- pop_surv_count$n/pop_count$n
comb$Prediction <- ifelse(surv_likelihood[comb$Pclass] > 0.5, 1, 0)
weight <- ifelse(comb$Set == "Train", 100/length(train_index), 100/length(test_index))
plot2 <- ggplot(comb, aes(x = Pclass, fill = Set)) + geom_bar(position = "dodge", aes(weight = weight)) + labs(title = "Pclass Distribution", x = "Pclass", y = "Percent")
test_pop_count <- comb[test_index, ] %>% count(Pclass)
accuracy <- ifelse(surv_likelihood > 0.5, surv_likelihood, 1 - surv_likelihood)
lb <- 0.6555
print(sprintf("Estimated score from train set                 %2.4f ", sum(comb$Survived == comb$Prediction, na.rm = T)/length(train_index)))
print(sprintf("Optimal Pclass only predicted score            %2.4f ", sum(test_pop_count$n * accuracy)/sum(test_pop_count$n)))
print(sprintf("Actual leader board (LB) score on the test set %2.4f ", lb))
print(sprintf("Train set relative overstimation on LB         %2.4f ", (sum(test_pop_count$n * accuracy)/sum(test_pop_count$n)/lb)))
submit <- data.frame(PassengerId = test_index, Survived = comb$Prediction[test_index])
write.csv(submit, file = paste0("class_only.csv"), row.names = FALSE, quote = F)
grid.arrange(plot1, plot2, ncol = 1)
