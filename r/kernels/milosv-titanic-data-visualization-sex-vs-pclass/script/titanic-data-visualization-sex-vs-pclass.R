library(lattice)
da_train <- read.table(file = "../input/train.csv", header = T, sep = ",")

da_train$Pclass <- factor(da_train$Pclass, labels = c("Upper", "Middle", "Lower"))
da_train$Clr <- ifelse(da_train$Survived == 1, "green","red")
da_train$Pch <- ifelse(da_train$Survived == 1, "263A","271d")
da_train$Alpha <- runif(dim(da_train)[1], 0.3, 0.6)

xyplot(Pclass ~ Sex, data = da_train, 
       pch = -as.hexmode(da_train$Pch),
       col=da_train$Clr, cex = 2, 
       alpha = 0.4, 
       aspect = 0.5,
       jitter.x = T, jitter.y = T, xlab = "", ylab = "",
       par.settings = list(background = list(col= "black"),
                           axis.line = list(col = "transparent"),
                           axis.text = list(col = "white")),
       key = list(space = "top", adj = 1, columns = 2,
                  title="Survived\n",
                  text = list(c("No", "Yes")),
                  cex = 0.8, col = "white",
                  points = list(pch = -as.hexmode(unique(da_train$Pch)))
                  )
)