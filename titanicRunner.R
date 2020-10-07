# Listing packages
packages <- installed.packages()
# Loading packages]
lapply(packages, require, character.only = TRUE)

library(jsonlite)
library(gdata)

#-----------will clear all objects includes hidden objects

rm(list = ls(all.names = TRUE))
gc()

#--------get directories

setwd("~/trustworthy_titanic")
dirs = list.dirs(path = './r/kernels', full.names = FALSE, recursive = FALSE)
#dirs = rev(dirs)

#-----------detatch packages

detach.packages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}


#-----------get libы

get.libs <- function(filepath){
  pack = c()
  f.l = readLines(filepath)
    for (ln in f.l)
    {
      if (grepl('^library', ln)) pack = c(pack, ln)
    }
  return(pack)
}


#-------------get function calls____Version2

for (dir in dirs)
{
  try(
    {
      print(dir)
      detach.packages()
      library(NCmisc)
      library(jsonlite)
      library(stringr)
      d = paste("~/trustworthy_titanic/r/kernels/",dir, sep="")
      d = paste(d, "/script", sep="")
      print(d)
      setwd(d)
      f.name <- list.files(path = ".", pattern = "\\.R$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
      libs = get.libs(f.name)
      lapply(libs, write, "test.R", append=TRUE)
      sys.source("test.R")
      file.remove("test.R")
      lst <- list.functions.in.file(f.name, alphabetic = TRUE)
      exportJSON <- toJSON(lst)
      write(exportJSON, paste(str_remove(f.name, ".R"), ".json", sep=""))
      #rm(list=setdiff(ls(), "dirs")) #will clear all objects includes hidden objects.
      #gc()
    }
  )
}


#----------get runtimes

names = c()

for (dir in dirs)
  try(
    {
      print(dir)
      d = paste("~~/titanic/notebooks/r/kernels/",dir, sep="")
      d = paste(d, "/script", sep="")
      setwd(d)
      f.name <- list.files(path = ".", pattern = "\\.R$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
      start_time <- Sys.time()
      rcmd("BATCH", f.name, timeout = 300)
      end_time <- Sys.time()
      t = end_time - start_time;
      t = as.numeric(t, units = "secs")
      print(t)
      n = cbind(dir, t)
      names = rbind(names, n)
    }
  )

setwd("~/~/titanic/notebooks")
write.csv(names, file ="timesDirs.csv", row.names=F)