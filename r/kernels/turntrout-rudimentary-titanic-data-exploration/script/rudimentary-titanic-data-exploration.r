
libraries <- c('readr', 'data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
lapply(libraries, require, character.only = TRUE)

train = read_csv("../input/train.csv")
test = read_csv("../input/test.csv")

str(train)

colSums(sapply(train, is.na))

plot_Missing <- function(data_in, title = NULL){
  # create a data frame of the binarized data (0 for missing, 1 for not missing)
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  # order the features by how much data they are missing
  temp_df <- temp_df[,order(colSums(temp_df))]
  # convert data to tileable entity (not sure about these two lines)
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  # add axis labels
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  # construct the graph
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}


plot_Missing(train[,colSums(is.na(train)) > 0])

length(which(train$Parch == 0 & train$Age<18))

train <- mutate(train, Nanny = Age < 18 & Parch == 0)
train$Nanny
