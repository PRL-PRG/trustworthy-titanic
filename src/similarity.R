install.packages("SimilaR")

library(SimilaR)

help("SimilaR")

s1 = source('./titanic-day1.R')
s2 = source('./titanic-day2.R')


SimilaR_fromTwoFunctions('./titanic-day1.R',
                         './titanic-day2.R',
                         returnType = "data.frame",
                         #fileTypes = "file",
                         aggregation = "tnorm")



f1 <- function(x) {x*x}
f2 <- function(x,y) {x+y}

## A data frame is returned: 1 row, 4 columns
SimilaR_fromTwoFunctions(f1,
                         f2,
                         returnType = "data.frame",
                         aggregation = "tnorm")


SimilaR_fromDirectory(
  './',
  returnType = "data.frame",
  fileTypes = "file",
  aggregation = "tnorm"
)

help(SimilaR_fromDirectory)

SimilaR_fromDirectory('./',
                      returnType = "data.frame",
                      fileTypes="file",
                      aggregation = "sym")
