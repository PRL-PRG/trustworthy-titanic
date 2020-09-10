# Listing packages
packages <- installed.packages()
# Loading packages]
lapply(packages, require, character.only = TRUE)

#-------------get function calls

for (dir in dirs)
{
  try(
    {
      print(dir)
      d = paste("~/titanic/notebooksnotebooks/r/kernels/",dir, sep="")
      d = paste(d, "/script", sep="")
      print(d)
      setwd(d)
      f.name <- list.files(path = ".", pattern = "\\.R$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
      print(f.name)
      lst <- list.functions.in.file(f.name, alphabetic = TRUE)
      exportJSON <- toJSON(lst)
      write(exportJSON, paste(str_remove(f.name, ".R"), ".json", sep=""))
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