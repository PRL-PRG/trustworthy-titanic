## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(readr)
library(magrittr)
library(tidyverse)
library(scales)

Train <- read_csv("../input/train.csv")
Test <- read_csv("../input/test.csv")
D <- bind_rows(Train, Test)
summary(D)
D$Pclass %<>% as.character()

