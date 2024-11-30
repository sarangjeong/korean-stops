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

##################
# data wrangling #
##################

basic_data_preprocessing <- function(
    workerids_csv_path,
    trials_csv_path,
    subject_information_csv_path
) {
  # load worker id data & filter only those w prolific id
  worker_id <- read.csv(workerids_csv_path, stringsAsFactors = TRUE)
  workerid_with_prolific_id <- worker_id$workerid[worker_id$prolific_participant_id != "<no-id>"]
  workerid_with_no_prolific_id <- worker_id$workerid[worker_id$prolific_participant_id == "<no-id>"]
  workerid_with_prolific_id
  workerid_with_no_prolific_id
  
  # load trial data
  trials <- read.csv(trials_csv_path, stringsAsFactors = TRUE)
  
  # load subject info data
  subject_info <- read.csv(subject_information_csv_path)
  subject_info <- subject_info %>%
    mutate(across(c(enjoyment, equipment_type, fairprice, gender, impairment), as.factor))
  
  # remove rows with <no-id>
  trials <- trials %>%
    filter(workerid %in% workerid_with_prolific_id)
  subject_info <- subject_info %>%
    filter(workerid %in% workerid_with_prolific_id)
  
  # remove unnecessary columns 
  drops <- c("proliferate.condition", "error")
  trials <- trials[ , !(names(trials) %in% drops)]
  subject_info <- subject_info[ , !(names(subject_info) %in% drops)]
  
  # change subject age based on manual check
  subject_info$age[subject_info$age == 196707] <- 2024-1967
  subject_info$age[subject_info$age == -59] <- 59
  
  # merge data
  nrow(trials)
  stops <- trials %>%
    left_join(.,subject_info,by=c("workerid"))
  nrow(stops) # check if # of rows did not change
  
  # change column names for subject & condition
  stops <- stops %>%
    rename(subject = workerid,
           rt = response_time_milliseconds
    )
  stops$subject = as.factor(stops$subject)
  
  # drop audio check & practice rows
  stops <- stops[stops$id != "check" & stops$id != "practice" ,]
  
  # drop unnecessary columns
  stops$error <- NULL; stops$response_practice <- NULL
  
  ### exclusion ###
  
  # 1) hearing impairment: 1
  stops <- stops[stops$impairment!=1 ,]
  
  # 2) kyungsang dialect: 3 (I manually checked the subject info data to identify KS speakers)
  # TODO: if I collect more data, do the manual checking again!
  ks_speaker <- c("426")
  stops <- stops[!(stops$subject %in% ks_speaker) ,]
  
  # 3) more than 3 non-aspirated responses for vot >= 7 & f0 >= 7: 1 participant ("453")
  stops[stops$f0 >= 7 & stops$vot >= 7 & stops$response!="asp",]$subject
  stops <- stops[(stops$subject != 453) ,]
  
  # 4) not_heard == TRUE
  stops <- stops[(stops$not_heard == "False") ,]
  
  # drop empty factor levels after exclusion
  stops$response = as.character(stops$response); stops$response = as.factor(stops$response)
  stops$poa = as.character(stops$poa); stops$poa = as.factor(stops$poa)
  stops$word = as.character(stops$word); stops$word = as.factor(stops$word)
  stops$subject = as.character(stops$subject); stops$subject = as.factor(stops$subject)
  stops$id = as.character(stops$id); stops$id = as.factor(stops$id)
  stops$gender = as.character(stops$gender); stops$gender = as.factor(stops$gender)
  
  ### data wrangling for ANALYSIS ###
  
  # create binary columns for each response
  stops$lenis = 0
  stops[stops$response == "lenis" ,]$lenis = 1
  stops$tense = 0
  stops[stops$response == "tense" ,]$tense = 1
  stops$asp = 0
  stops[stops$response == "asp" ,]$asp = 1
  
  # create columns for centered f0 & vot
  stops <- stops %>% 
    mutate(sf0 = scale(f0), svot = scale(vot), sage = scale(age))
  ### data wrangling for PLOTS ###
  # TODO: plot young vs old separately
  
  return(stops)
}

plot_data_preprocessing <- function(
    stops
) {
  # make a new df, create response count columns
  stops_mean <- stops %>%
    group_by(f0, vot) %>%
    summarise(lenis_num = sum(response=="lenis"), 
              tense_num = sum(response=="tense"),
              asp_num = sum(response=="asp"))
  
  # sum of response counts for each stimulus
  stops_mean$all_num = stops_mean$lenis_num + stops_mean$tense_num + stops_mean$asp_num
  
  stops %>% count(f0, vot, response) # check if numbers are correct
  
  # create response % columns
  stops_mean$lenis <- stops_mean$lenis_num / stops_mean$all_num
  stops_mean$tense <- stops_mean$tense_num / stops_mean$all_num
  stops_mean$asp <- stops_mean$asp_num / stops_mean$all_num
  
  stops_mean$lenis + stops_mean$tense + stops_mean$asp # check if columns add up to 1
  
  # drop count columns 
  stops_mean <- subset(stops_mean, select = -c(lenis_num, tense_num, asp_num, all_num))
  
  # create predominant response column
  stops_mean$predominant <- "none"
  stops_mean$predominant_num <- 0
  
  # assign predominant category name
  stops_mean[(stops_mean$lenis > stops_mean$tense) & (stops_mean$lenis > stops_mean$asp) ,]$predominant <- "lenis"
  stops_mean[(stops_mean$tense > stops_mean$lenis) & (stops_mean$tense > stops_mean$asp) ,]$predominant <- "tense"
  stops_mean[(stops_mean$asp > stops_mean$lenis) & (stops_mean$asp > stops_mean$tense) ,]$predominant <- "asp"
  
  # assign predominant category %
  stops_mean[stops_mean$predominant=="lenis" ,]$predominant_num <- stops_mean[stops_mean$predominant=="lenis" ,]$lenis
  stops_mean[stops_mean$predominant=="tense" ,]$predominant_num <- stops_mean[stops_mean$predominant=="tense" ,]$tense
  stops_mean[stops_mean$predominant=="asp" ,]$predominant_num <- stops_mean[stops_mean$predominant=="asp" ,]$asp
  
  # initial
  stops_mean$label <- toupper(substr(stops_mean$predominant, 1, 1))
  
  return(stops_mean)
}
