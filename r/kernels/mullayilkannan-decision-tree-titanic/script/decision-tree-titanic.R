library(dplyr)
library(rpart)
dataset <- read.csv("../input/train.csv")
smp_siz = floor(0.8 * nrow(dataset))
print(smp_siz)
set.seed(123)
train_ind = sample(seq_len(nrow(dataset)), size = smp_siz)
train = dataset[train_ind, ]
test = dataset[-train_ind, ]
attach(train)
fit <- rpart(Survived ~ Age + Sex + Pclass + SibSp, method = "class")
plot(fit, uniform = TRUE, main = "Classification Tree for titanic")
text(fit, use.n = TRUE, all = TRUE, cex = 0.8)
z <- table(predict(fit, test, type = "class"), test$Survived)
print(z)
accuracy = sum(diag(z))/nrow(test) * 100
print(accuracy)
val_set <- read.csv("../input/test.csv")
summary(val_set)
results <- predict(fit, val_set, type = "class")
my_solution <- data.frame(PassengerId = val_set$PassengerId, Survived = results)
my_solution
write.csv(my_solution, "d_tree.csv", row.names = FALSE)
