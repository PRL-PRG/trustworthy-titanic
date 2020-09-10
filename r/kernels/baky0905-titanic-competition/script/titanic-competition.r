
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.

train_data  <- read_csv("../input/train.csv")

head(train_data)

train_data %>% 
    select(Survived,Sex,Age,Pclass)   %>% 
    mutate(SurvivedDesc = case_when(
                          Survived == 0 ~ "Died",
                        Survived == 1 ~ "Alive"))  %>% 
    ggplot(aes(x = Pclass, fill = SurvivedDesc )) +
    geom_bar(position = "fill") 
    

read_csv("../input/test.csv")

train_data  %>% 
    purrr::map_df( ~ sum(is.na(.)))

train_data <- train_data  %>% 
    na_if("")
library(tidyverse)
### in order to reassign it in place you should use  %<>% 
train_data %<>% 
    na_if("")

train_data  %>% 
    purrr::map_df( ~ sum(is.na(.)))

train_data  %>% 
    select_if(is.numeric)  %>% 
    skimr::skim()

train_data  %>% 
    purrr:: map_df(~n_distinct(.))

train_data  %>% 
    purrr:: map_df(~n_distinct(.)) %>% 
    tidyr::gather(question, num_distinct_answers)  %>% 
    arrange(desc(num_distinct_answers))

train_data  %>% 
    count(Age, sort = TRUE)
