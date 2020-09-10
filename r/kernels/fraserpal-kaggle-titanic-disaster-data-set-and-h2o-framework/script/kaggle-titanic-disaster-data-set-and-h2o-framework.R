## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library('h2o'))
suppressPackageStartupMessages(library('ggplot2'))
suppressPackageStartupMessages(library('reshape2'))
suppressPackageStartupMessages(library('stringr'))
suppressPackageStartupMessages(library('plyr'))
suppressPackageStartupMessages(library('DT'))

workdir = normalizePath("../input/")

h2o.init()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
h2o.no_progress()
train <- h2o.importFile(path = normalizePath(paste(workdir, '/train.csv', sep = "")))
val <- h2o.importFile(path = normalizePath(paste(workdir, '/test.csv', sep = "")))
summary(train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
temp = c("Name", "Cabin")
for( i in temp){
  train[, i] <- NULL
  val[, i] <- NULL
}

train <- as.numeric(train)
val <- as.numeric(val)
train$Survived <- as.factor(train$Survived)

summary(train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
split <- h2o.splitFrame(data = train, ratios = 0.7)
train_split <- split[[1]]
test_split <- split[[2]]

response <- 'Survived'
predictors <- setdiff(names(train_split), response)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_dl <- h2o.deeplearning(x = predictors,
                          y = response,
                          training_frame = train_split,
                          validation_frame = test_split,
                          distribution = 'multinomial',
                          hidden = c(10, 10),
                          epochs = 5)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# KAGGLE RESULTS GENERATION
pred <- h2o.predict(model_dl, newdata = val)
pred <- as.list(pred)
class <- as.data.frame(ifelse(pred >= .5,1,0))
passengerid <- as.data.frame(val[,1])
class <- cbind(passengerid, class)
colnames(class) <- c("PassengerId", "Survived")
write.csv(class[1:418,], "submission.csv", row.names=F) # Do not know why, I have more rows than normal in class object.



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# DEEP LEARNING TOPOLOGY TEST FUNCTION
run_DL_test <- function(extra_params) {
  model_test <- do.call(h2o.deeplearning, modifyList(list(x = predictors,
                                                          y = response,
                                                          training_frame = train_split,
                                                          validation_frame = test_split,
                                                          distribution = 'multinomial'),
                                                          extra_params))
  
  idx <- paste(names(extra_params), extra_params, sep = "=", collapse=" ")
  
  # Model Metrics
  sampleshist <- model_test@model$scoring_history$samples
  samples <- sampleshist[length(sampleshist)]
  time <- model_test@model$run_time/1000
  
  training_MSE <- (tail(model_test@model$scoring_history$training_rmse, n=1))^2
  training_logloss <- tail(model_test@model$scoring_history$training_logloss, n=1)
  training_auc <- tail(model_test@model$scoring_history$training_auc, n=1)
  
  test_MSE <- (tail(model_test@model$scoring_history$validation_rmse, n=1))^2
  test_logloss <- tail(model_test@model$scoring_history$validation_logloss, n=1)
  test_auc <- tail(model_test@model$scoring_history$validation_auc, n=1)
  
  
  #print("Deep Learning Model Topology Test Metrics")
  #print(paste0("Parameters        : ", idx))
  #print(paste0("Samples           : ", samples))
  #print(paste0("training speed    : ", samples/time, " samples/sec"))
  #print(paste0("training time     : ", time, " sec"))
  #print(paste0("Training MSE      : ", training_MSE))
  #print(paste0("Training Logloss  : ", training_logloss))
  #print(paste0("Training AUC      : ", training_auc))
  #print(paste0("Validation MSE    : ", test_MSE))
  #print(paste0("Validation Logloss: ", test_logloss))
  #print(paste0("Validation AUC    : ", test_auc))
  #writeLines("\n")
  
  # Group Results
  c(idx, samples, sprintf("%.3f", time), sprintf("%.3f", samples/time), sprintf("%.3f", training_MSE), 
    sprintf("%.3f", training_logloss), sprintf("%.3f", training_auc), sprintf("%.3f", test_MSE), 
    sprintf("%.3f", test_logloss), sprintf("%.3f", test_auc))
}

#########################################################################################

# EXPORT METRICS TO DF and CSV FILE FUNCTION
build_nn_topology_test_results <- function(results, file) {
  table <- matrix(unlist(results), ncol = 10, byrow = TRUE)
  
  colnames(table) <- c("Parameters", "Samples", "Training_Time", "Training_Speed", 
                       "Training_MSE", "Training_Logloss", "Training_AUC",
                       "Validation_MSE", "Validation_Logloss", "Validation_AUC")
  #write.csv(table, file.path(workdir,file), col.names = T, row.names=F, quote=T, sep=",")
  return(as.data.frame(table))

}



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# DEEP LEARNING TOPOLOGY TEST EXECUTION
# Parameters
EPOCHS = 1

NN_topology_test <- list(
list(hidden=c(32),             epochs=EPOCHS),
list(hidden=c(128),            epochs=EPOCHS),
list(hidden=c(256),            epochs=EPOCHS),
list(hidden=c(512),            epochs=EPOCHS),
list(hidden=c(32,32),          epochs=EPOCHS),
list(hidden=c(128,128),        epochs=EPOCHS),
list(hidden=c(256,256),        epochs=EPOCHS),
list(hidden=c(512,512),        epochs=EPOCHS),
list(hidden=c(32,32,32),       epochs=EPOCHS),
list(hidden=c(128,128,128),    epochs=EPOCHS),
list(hidden=c(256,256,256),    epochs=EPOCHS),
list(hidden=c(512,512,512),    epochs=EPOCHS))

# Execution
nn_topology_results_df <- build_nn_topology_test_results(lapply(NN_topology_test, run_DL_test), "network_topology_test.csv")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ploting_nn_topology_test_metrics <- function(df_name) {
  # CSV FILE IMPORT TO DATAFRAME
  # dataframe <- read.csv(file = normalizePath(paste(workdir, file_name, sep = "")), header = T, sep = ",")
  dataframe <- df_name
  
  # MELTED DATAFRAMES TO PLOT
  dataframe2 <- melt(dataframe, id = c(names(dataframe)[!names(dataframe) %in% c("Training_MSE", "Validation_MSE")]))
  dataframe3 <- melt(dataframe, id = c(names(dataframe)[!names(dataframe) %in% c("Training_AUC", "Validation_AUC")]))
  
  # PLOT OBJECT: MSE GRAPH
  plot_MSE <- ggplot(dataframe2, aes(x = Parameters, y = value, fill = variable)) +
    geom_bar(stat = "identity", width = .5, position = "dodge") +
    theme_minimal() + 
    theme(axis.text.x =  element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
        labs(x = NULL, y = 'MSE', fill = NULL)

  # PLOT OBJECT: AUC GRAPH
  plot_AUC <- ggplot(dataframe3, aes(x = Parameters, y = value, fill = variable)) +
    geom_bar(stat = "identity", width = .5, position = "dodge") +
    theme_minimal() + 
    theme(axis.text.x =  element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
    labs(x = NULL, y = 'AUC', fill = NULL) # +  coord_cartesian(ylim = c(0.75, 0.85))
  
  # PLOT OBJECT: TRAINING SPEED GRAPH
  plot_ts <- ggplot(dataframe, aes(x = Parameters, y = Training_Speed)) +
    geom_point() +
    theme_minimal() + 
    theme(axis.text.x =  element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
    labs(x = NULL, y = 'Training Speed [Samples/sec]')

  dl_results <- list("plotMSE" = plot_MSE, "plotAUC" = plot_AUC, "plot_ts" = plot_ts)
  return(dl_results)
}



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

dl_results <- ploting_nn_topology_test_metrics(nn_topology_results_df)
dl_results$plotMSE
dl_results$plotAUC
dl_results$plot_ts



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# DEEP LEARNING MODEL FUNCTION
run_dl_model <- function(dl_extra_params) {
  
  model_dl <- do.call(h2o.deeplearning, modifyList(list(x = predictors,
                                                          y = response,
                                                          training_frame = train_split,
                                                          validation_frame = test_split,
                                                          distribution = 'multinomial'),
                                                          dl_extra_params))
  # Metrics  
  dl_auc <- h2o.auc(model_dl)
  dl_mse <- h2o.mse(model_dl)
  dl_time <- model_dl@model$run_time/1000
  
  # Group Results
  idx <- paste(names(dl_extra_params), dl_extra_params, sep = "=", collapse=" ")
  dl_res <- c("DL", idx, sprintf("%.3f", dl_time), sprintf("%.3f", dl_mse), sprintf("%.3f", dl_auc))
  
  # Print Metrics
  #print(paste0("Model: ", 'Deep Learning'))
  #print(paste0("Parameters: ", idx))
  #print(paste0("Time: ", dl_time))
  #print(paste0("MSE: ", dl_mse))
  #print(paste0("AUC: ", dl_auc))
  #writeLines("\n")
  
  return(dl_res)
  
}
  
#########################################################################################

# GENERALIZED LINEAR MODEL FUNCTION
run_glm_model <- function(glm_extra_params) {
  
  model_glm <- do.call(h2o.glm, modifyList(list(x = predictors,
                       y = response,
                       training_frame = train_split,
                       validation_frame = test_split,
                       family = "binomial"),
                       glm_extra_params))
  # Metrics
  glm_auc <- h2o.auc(model_glm)
  glm_mse <- h2o.mse(model_glm)
  glm_time <- model_glm@model$scoring_history$duration[length(model_glm@model$scoring_history$duration)]
  glm_time <- str_extract(glm_time, "\\d+\\.*\\d*")
  
  # Group Results
  idx <- paste(names(glm_extra_params), glm_extra_params, sep = "=", collapse=" ")
  glm_res <- c("GLM", idx, sprintf("%s", glm_time), sprintf("%.3f", glm_mse), sprintf("%.3f", glm_auc))
  
  # Print Metrics
  #print(paste0("Model: ", 'Generalized Linear Model'))
  #print(paste0("Parameters: ", idx))
  #print(paste0("Time: ", glm_time))
  #print(paste0("MSE: ", glm_mse))
  #print(paste0("AUC: ", glm_auc))
  #writeLines("\n")
  
  return(glm_res)
}
  
#########################################################################################

# GRADIENT BOOSTING MACHINE MODEL FUNCTION
run_gbm_model <- function(gbm_extra_params) {

  model_gbm <- do.call(h2o.gbm, modifyList(list(x = predictors,
                       y = response,
                       training_frame = train_split,
                       validation_frame = test_split),
                       gbm_extra_params))
  # Metrics
  gbm_auc  <- h2o.auc(model_gbm)
  gbm_mse  <- h2o.mse(model_gbm)
  gbm_time <- model_gbm@model$scoring_history$duration[length(model_gbm@model$scoring_history$duration)]
  gbm_time <- str_extract(gbm_time, "\\d+\\.*\\d*")
  
  # Group Results
  idx <- paste(names(gbm_extra_params), gbm_extra_params, sep = "=", collapse=" ")
  gbm_res <- c("GBM", idx, sprintf("%s", gbm_time), sprintf("%.3f", gbm_mse), sprintf("%.3f", gbm_auc))
  
  # Print Metrics
  #print(paste0("Model: ", 'Gradient Boosting MAchine'))
  #print(paste0("Parameters: ", idx))
  #print(paste0("Time: ", gbm_time))
  #print(paste0("MSE: ", gbm_mse))
  #print(paste0("AUC: ", gbm_auc))
  #writeLines("\n")
  
  return(gbm_res)
}
  
#########################################################################################

# DISTRIBUTED RANDOM FOREST MODEL FUNCTION
run_drf_model <- function(drf_extra_params) {

  model_drf <- do.call(h2o.randomForest, modifyList(list(x = predictors,
                       y = response,
                       training_frame = train_split,
                       validation_frame = test_split),
                       drf_extra_params))
  # Metrics
  drf_auc  <- h2o.auc(model_drf)
  drf_mse  <- h2o.mse(model_drf)
  drf_time <- model_drf@model$scoring_history$duration[length(model_drf@model$scoring_history$duration)]
  drf_time <- str_extract(drf_time, "\\d+\\.*\\d*")
  
  # Group Results
  idx <- paste(names(drf_extra_params), drf_extra_params, sep = "=", collapse=" ")
  drf_res <- c("DRF", idx, sprintf("%s", drf_time), sprintf("%.3f", drf_mse), sprintf("%.3f", drf_auc))

  # Print Metrics
  #print(paste0("Model: ", 'Distributed Random Forest'))
  #print(paste0("Parameters: ", idx))
  #print(paste0("Time: ", drf_time))
  #print(paste0("MSE: ", drf_mse))
  #print(paste0("AUC: ", drf_auc))
  #writeLines("\n")
  
  return(drf_res)
}

#########################################################################################

# EXPORT METRICS TO CSV FILE
build_model_benchmark_results <- function(results, file) {

  table <- matrix(unlist(results), ncol = 5, byrow = TRUE)
  colnames(table) <- c("Model_type", "Parameters", "Time", "MSE", "AUC")
  
  # write.csv(table, file.path(workdir,file),
  #          col.names = T, row.names=F, quote=T, sep=",")
  
  return(as.data.frame(table))
}



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# BENCHMARK PARAMETERS
DL_params <- list(
  list(hidden=c(20),          epochs=EPOCHS),
  list(hidden=c(20,20,20,20), epochs=EPOCHS))

GLM_params <- list(
  list(alpha=0.0),
  list(alpha=0.5),
  list(alpha=1.0))

GBM_params <- list(
  list(ntrees=10,  balance_classes=F),
  list(ntrees=100, balance_classes=F),
  list(ntrees=10,  balance_classes=T),
  list(ntrees=100, balance_classes=T))

DRF_params <- list(
  list(ntrees=10,  balance_classes=F),
  list(ntrees=100, balance_classes=F),
  list(ntrees=10,  balance_classes=T),
  list(ntrees=100, balance_classes=T))

# TESTS EXECUTION
dl_metrics  <- lapply(DL_params,  run_dl_model)
glm_metrics <- lapply(GLM_params, run_glm_model)
gbm_metrics <- lapply(GBM_params, run_gbm_model)
drf_metrics <- lapply(DRF_params, run_drf_model)

# GROUP ALL RESULTS AND EXPORT THEM TO CSV FILE
total_results <- rbind(dl_metrics, glm_metrics, gbm_metrics, drf_metrics)
model_benchmark_results_df <- build_model_benchmark_results(total_results, 'model_benchmark.csv')
datatable(model_benchmark_results_df)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ploting_benchmark_metrics <- function(df_name, file_name) {
  # CSV FILE IMPORT TO DATAFRAME
  # dataframe <- read.csv(file = normalizePath(paste(workdir, file_name, sep = "")), header = T, sep = ",")
  dataframe <- df_name
  
  # PLOT OBJECT: MSE GRAPH
  plot_MSE <- ggplot(dataframe, aes(x = paste(Model_type, Parameters), y = MSE)) +
    geom_bar(stat = "identity", width = .5) +
    theme_minimal() + 
    theme(axis.text.x =  element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
        labs(x = NULL, y = 'MSE', fill = NULL)

  # PLOT OBJECT: AUC GRAPH
  plot_AUC <- ggplot(dataframe, aes(x = paste(Model_type, Parameters), y = AUC)) +
    geom_bar(stat = "identity", width = .5) +
    theme_minimal() + 
    theme(axis.text.x =  element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
    labs(x = NULL, y = 'AUC', fill = NULL) # + coord_cartesian(ylim = c(0.7, 1))
  
  # PLOT OBJECT: TRAINING SPEED GRAPH
  plot_ts <- ggplot(dataframe, aes(x = paste(Model_type, Parameters), y = Time)) +
    geom_bar(stat = "identity", width = .5) +
    theme_minimal() + 
    theme(axis.text.x =  element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
    labs(x = NULL, y = 'Training Time [sec]')

  dl_results <- list("plotMSE" = plot_MSE, "plotAUC" = plot_AUC, "plot_ts" = plot_ts)
  return(dl_results)
}



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

benchmark_plots <- ploting_benchmark_metrics(model_benchmark_results_df, "/model_benchmark")
benchmark_plots$plotMSE
benchmark_plots$plotAUC
benchmark_plots$plot_ts


