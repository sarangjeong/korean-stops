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
library(brms)
library(broom.mixed)
library(patchwork)
source("preprocessing.R")
source("plot.R")
source("models.R")

times <- read.csv("../data/korean_stops_perception_3_poa_all_ages-time_in_minutes.csv", stringsAsFactors = TRUE)
worker_id <- read.csv("../data/korean_stops_perception_3_poa_all_ages-workerids.csv", stringsAsFactors = TRUE)

sprintf("The number of participants: %d", nrow(times))
worker_times <- times %>%
  left_join(.,worker_id,by=c("workerid"))

worker_times <- worker_times %>%
  filter(prolific_participant_id != "<no-id>")
sprintf("The number of valid participants: %d", nrow(worker_times))

sprintf("Average experiment time of valid participants: %f", mean(worker_times$time_in_minutes))

