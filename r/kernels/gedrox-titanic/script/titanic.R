library(h2o)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")

prepare <- function(df) {
  if (!is.null(df$Survived)) {
    df$Survived <- factor(df$Survived, levels = c(1, 0))
    levels(df$Survived) <- c("Survived", "Died")
  }
  df$Pclass <- as.factor(df$Pclass)
  levels(df$Pclass) <- c("1st Class", "2nd Class", "3rd Class")
  df
}

train <- prepare(train)
test <- prepare(test)

#h2o.shutdown(FALSE)
h2o.init()
h2o.removeAll()
tr <- as.h2o(train)
spl <- h2o.splitFrame(tr, c(0.8))
T1 <- h2o.assign(spl[[1]], "train.hex")
T2 <- h2o.assign(spl[[2]], "valid.hex")
T3 <- as.h2o(test, "test.hex")

y <- c("Survived")
ignore <- c("Name", "PassengerId", "Ticket", "Cabin")
x <- setdiff(setdiff(names(tr), y), ignore)

rf <- h2o.randomForest(
  training_frame = T1,
  validation_frame = T2,
  x = x,
  y = y
)

summary(rf)

res <- h2o.predict(object = rf, newdata = T3)

out <- data.frame(PassengerId = test$PassengerId, Survived = as.vector(as.numeric(res$predict)))

write.csv(out, file = "titanic.csv", row.names = FALSE)
