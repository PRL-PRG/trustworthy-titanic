# Listing packages
packages <- installed.packages()
# Loading packages]
lapply(packages, require, character.only = TRUE)

install.packages("formatR")

library("formatR")

library(NCmisc)


is_function = function (expr) {
  if (! is_assign(expr))
    return(FALSE)
  value = expr[[3]]
  is.call(value) && as.character(value[[1]]) == 'function'
}

function_name = function (expr)
  as.character(expr[[2]])

is_assign = function (expr)
  is.call(expr) && as.character(expr[[1]]) %in% c('=', '<-', 'assign')


listFunctions <- function(filename) {
  temp.env <- new.env()
  sys.source(filename, envir = temp.env)
  functions <- lsf.str(envir=temp.env)
  rm(temp.env)
  return(functions)
}

func = listFunctions('titanic-day1.R')

file_parsed = parse('./titanic-day1.R')
functions = Filter(is_function, file_parsed)
function_names = unlist(Map(function_name, functions))


typeof(file_parsed[7])

test.env <- new.env()
sys.source('titanic-day1.R', envir = test.env)
lsf.str(envir=test.env)
lst <- list.functions.in.file('./titanic-test.R', alphabetic = TRUE)
rm(test.env)

typeof(lst)

lst = as.vector(lst)
dimnames(lst)


get.calls <- function(filepath) {
  code <- parse(filepath)
  tokens <- as.list(code)
  calls <- c()
  while (TRUE) {
    any_unpacked <- FALSE
    for (ii in seq_along(tokens)) {
      part <- tokens[[ii]]
      # Calls always have the function name as the first element
      if (is.call(part)) {
        fun_token <- part[[1]]
        calls <- c(calls, deparse(fun_token))
      }
      # Expressions have a length
      if (length(part) > 1) {
        tokens[[ii]] <- as.list(part)
        any_unpacked <- TRUE
      }
    } 
    tokens <- unlist(tokens)
    if (!any_unpacked) break
  }
  unique(calls)
}

code <- parse('./titanic-test.R')

get.calls('./titanic-test.R')

get.calls("./titanic-day1.R")

code <- parse("./titanic-day1.R")
tokens <- as.list(code)

tokens[1]

for (ii in seq_along(tokens)) print(tokens[ii])

print(tokens[ii])
  }

detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages()

test.env <- new.env()
(.packages())
rm(test.env)

#-----------testt

detachAllPackages()

get.libs <- function(filepath){
  pack = c()
  f.l = readLines(filepath)
  for (ln in f.l)
  {
    if (grepl('^library', ln)) pack = c(pack, ln)
  }
  return(pack)
}

file = 'titanic-day1.R'

libs = get.libs(file)


lapply(libs, write, "test2.R", append=TRUE)




tidy_source(source = './titanic-day1.R', keep.comment=FALSE)

writeLines(as.character(parse('./titanic-day1.R')), "./out2.R")
