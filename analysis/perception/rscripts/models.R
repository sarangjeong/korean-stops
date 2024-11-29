# Analysis of experimental results of
# Korean stop contrast, perception, young (pilot)
# created by Sarang Jeong on June 6, 2021

##########
# set-up #
##########

# set working directory
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load libraries
library(tidyverse)

save_or_load_model <- function(model_path, data, model_function, force=FALSE) {
  # Ensure the directory exists
  model_dir <- dirname(model_path)
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }
  
  if (!file.exists(model_path) | force) {
    cat("Fitting the model...\n")
    model <- model_function(data)  # Call the function to fit the model
    saveRDS(model, model_path)     # Save the model to the specified path
  } else {
    cat("Model file found. Loading the saved model...\n")
    model <- readRDS(model_path)  # Load the saved model
  }
  return(model)
}