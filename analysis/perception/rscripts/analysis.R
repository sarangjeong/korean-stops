# Analysis of experimental results of
# Korean stop contrast, perception, young (pilot)
# created by Sarang Jeong on June 6, 2021

##########
# set-up #
##########

# load libraries
library(tidyverse)
library(lme4)
library(ggplot2)
library(MuMIn)
source("helpers.R")

# set working directory
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

##################
# data wrangling #
##################

# load trial data
trials <- read.csv("../data/korean_stop_contrast_perception_labial_pilot_young-trials.csv", stringsAsFactors = TRUE)

# load time data
whole_time <- read.csv("../data/korean_stop_contrast_perception_labial_pilot_young-time_in_minutes.csv", stringsAsFactors = TRUE)
trial_1_time <- read.csv("../data/korean_stop_contrast_perception_labial_pilot_young-triral_1_time_in_minutes.csv", stringsAsFactors = TRUE)
trial_2_time <- read.csv("../data/korean_stop_contrast_perception_labial_pilot_young-triral_2_time_in_minutes.csv", stringsAsFactors = TRUE)
trial_3_time <- read.csv("../data/korean_stop_contrast_perception_labial_pilot_young-triral_3_time_in_minutes.csv", stringsAsFactors = TRUE)

# load subject info data whose encoding I manually changed to ANSI to fix Korean encoding problem
subject_info <- read.csv("../data/korean_stop_contrast_perception_labial_pilot_young-subject_information_ANSI.csv")
subject_info$assess = as.factor(subject_info$assess)
subject_info$gender = as.factor(subject_info$gender)

# remove unnecessary columns from time & subject data
drops <- c("proliferate.condition", "error")
whole_time <- whole_time[ , !(names(whole_time) %in% drops)]
trial_1_time <- trial_1_time[ , !(names(trial_1_time) %in% drops)]
trial_2_time <- trial_2_time[ , !(names(trial_2_time) %in% drops)]
trial_3_time <- trial_3_time[ , !(names(trial_3_time) %in% drops)]
subject_info <- subject_info[ , !(names(subject_info) %in% drops)]

# merge data
nrow(trials)
stops <- trials %>%
  left_join(.,whole_time,by=c("workerid")) %>%
  left_join(.,trial_1_time,by=c("workerid")) %>%
  left_join(.,trial_2_time,by=c("workerid")) %>%
  left_join(.,trial_3_time,by=c("workerid")) %>%
  left_join(.,subject_info,by=c("workerid"))
nrow(stops) # check if # of rows did not change

# change column names for subject & condition
stops <- stops %>%
  rename(subject = workerid,
         condition = proliferate.condition
  )
stops$subject = as.factor(stops$subject)

# drop audio check & practice rows
stops <- stops[stops$id != "check" & stops$id != "practice" ,]
nrow(stops)

# drop unnecessary columns
stops$error <- NULL; stops$response_practice <- NULL

### exclusion ###

# 1) hearing impairment: 1
stops <- stops[stops$impairment!=1 ,]
nrow(stops)

# 2) kyungsang dialect: 3 (I manually checked the subject info data to identify KS speakers; 1 KS speaker has already been excluded for hearing impairment)
ks_speaker <- c("6", "8", "12")
stops <- stops[!(stops$subject %in% ks_speaker) ,]
nrow(stops)

# 3) more than 3 non-aspirated responses for vot >= 6 & f0 >= 7: 0 ("18" has 2 such responses)
stops[stops$f0 >= 7 & stops$vot >= 6 & stops$response!="asp",]$subject

# drop empty factor levels after exclusion
stops$response = as.character(stops$response); stops$response = as.factor(stops$response)
stops$poa = as.character(stops$poa); stops$poa = as.factor(stops$poa)
stops$word = as.character(stops$word); stops$word = as.factor(stops$word)
stops$subject = as.character(stops$subject); stops$subject = as.factor(stops$subject)
stops$id = as.character(stops$id); stops$id = as.factor(stops$id)
stops$gender = as.character(stops$gender); stops$gender = as.factor(stops$gender)
summary(stops) # check each column

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
  mutate(sf0 = scale(f0), svot = scale(vot))

### data wrangling for PLOTS ###

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

# manually change the values when the largest category is not single (i.e. tied first place)

# identify tied cases
stops_mean[stops_mean$predominant == "none",] #f0==4 & vot==3, f0==4 & vot==4

# f0==4 & vot==3
stops_mean[stops_mean$f0 == 4 & stops_mean$vot==3 ,]$predominant <- "tense & lenis"
stops_mean[stops_mean$f0 == 4 & stops_mean$vot==3 ,]$predominant_num <- stops_mean[stops_mean$f0 == 4 & stops_mean$vot==3 ,]$lenis

# f0==4 & vot==4
stops_mean[stops_mean$f0 == 4 & stops_mean$vot==4 ,]$predominant <- "asp & lenis"
stops_mean[stops_mean$f0 == 4 & stops_mean$vot==4 ,]$predominant_num <- stops_mean[stops_mean$f0 == 4 & stops_mean$vot==4 ,]$lenis

stops_mean[stops_mean$f0 == 4 & (stops_mean$vot==3 | stops_mean$vot==4) ,] # check

# new column for plot label

# initial
stops_mean$label <- toupper(substr(stops_mean$predominant, 1, 1))

# manually change the values when the largest category is not single
stops_mean[stops_mean$f0 == 4 & stops_mean$vot==3 ,]$label <- "TL"
stops_mean[stops_mean$f0 == 4 & stops_mean$vot==4 ,]$label <- "AL"

#########
# PLOTS #
#########

# LENIS, continuous, with %

g.lenis <- ggplot(stops_mean, aes(x = vot, f0)) +
  geom_tile(aes(fill = lenis * 100)) +
  geom_text(aes(label = round(lenis * 100, 1))) +
  scale_fill_continuous(low = "white", high = "magenta", name = "lenis %", 
                        limits = c(0, 100), 
                        breaks = seq(0, 100, by=25)) +
  labs(x = "VOT", y = "F0",
       title ="Lenis responses across F0 and VOT continua") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
g.lenis

ggsave(
  "../graphs/lenis.png",
#  "../graphs/lenis.pdf",
  plot = g.lenis,
#  device = "pdf",
  scale = 1,
  width = 10,
  height = 6,
  dpi = "retina",
)

# TENSE, continuous, with %

g.tense <- ggplot(stops_mean, aes(vot, f0)) +
  geom_tile(aes(fill = tense * 100)) +
  geom_text(aes(label = round(tense * 100, 1))) +
  scale_fill_continuous(low = "white", high = "yellow", name = "tense %") +
  labs(x = "VOT", y = "F0",
       title ="Tense responses across F0 and VOT continua") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
g.tense

ggsave(
  "../graphs/tense.png",
#  "../graphs/tense.pdf",
  plot = g.tense,
#  device = "pdf",
  scale = 1,
  width = 10,
  height = 6,
  dpi = "retina",
)

# ASP, continuous, with %

g.asp <- ggplot(stops_mean, aes(x = vot, f0)) +
  geom_tile(aes(fill = asp * 100)) +
  geom_text(aes(label = round(asp * 100, 1))) +
  scale_fill_continuous(low = "white", high = "cyan", name = "aspirated %") +
  labs(x = "VOT", y = "F0",
       title ="Aspirated responses across F0 and VOT continua") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
g.asp

ggsave(
  "../graphs/asp_test.png",
#  "../graphs/asp_test.pdf",
  plot = g.asp,
#  device = "pdf",
  width = 10,
  height = 6,
  scale = 1,
  dpi = "retina",
)

# OVERALL, continuous, with name

g.overall <- ggplot(stops_mean, aes(x = vot, f0)) +
  geom_tile(aes(fill = predominant_num * 100)) +
  geom_text(aes(label = label)) +
  scale_fill_continuous(low = "white", high = "slategray", name = "Predominant category %", 
                        limits = c(40, 100), 
                        breaks = seq(40, 100, by=20)) +
  labs(x = "VOT", y = "F0",
       title ="Overall responses across F0 and VOT continua",
       caption = "A = aspirated, L = lenis, T = tense") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
g.overall

ggsave(
  "../graphs/overall.pdf",
  plot = g.overall,
  device = "pdf",
  scale = 1,
  dpi = "retina",
)

# THREE, continuous, with name + %

g.three <- ggplot(stops_mean, aes(vot, f0)) +
  geom_tile(aes(fill = I(rgb(1 - stops_mean$asp, 1 - stops_mean$lenis, 1 - stops_mean$tense))))+
  #               color = c("cyan", "yellow", "magenta"))) +
  # scale_color_manual(values = c("cyan", "yellow", "magenta")) +
  geom_text(aes(label =  paste(label, as.character(round(predominant_num * 100, 1))))) +
  labs(x = "VOT", y = "F0",
       title ="Mean responses across F0 and VOT continua",
       caption = "A = aspirated, L = lenis, T = tense") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 12))
g.three

ggsave(
  "../graphs/three_test.png",
  #    "../graphs/three_test.pdf",
  plot = g.three,
#  device = "pdf",
  width = 10,
  height = 6,
  scale = 1,
  dpi = "retina",
)


##########
# models #
##########

### fixed effects only --> as expected!

m = glm(asp ~ f0 + vot, data=stops, family="binomial")
summary(m) # (Intercept) -9.04852 ; f0 0.89095 ; vot 0.96718
m = glm(lenis ~ f0 + vot, data=stops, family="binomial")
summary(m) # (Intercept) 2.29026 ; f0 -1.11823 ; vot 0.19733
m = glm(tense ~ f0 + vot, data=stops, family="binomial")
summary(m) # (Intercept) 2.62940 ; f0 0.40588 ; vot -1.66699

### interaction + random

# ASP #

m.asp <- glmer(asp ~ sf0*svot +(1+sf0*svot|subject), data=stops, family="binomial")
summary(m.asp) # FAILED TO CONVERGE
 # restart
ss <- getME(m.asp, c("theta","fixef"))
m.asp1 <- update(m.asp, start=ss, control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(m.asp1) # restart did not work
 # nAGQ = 0
m.asp2 = glmer(asp ~ sf0*svot +(1+sf0*svot|subject), data=stops, nAGQ = 0, family="binomial")
summary(m.asp2)  # nAGQ fixed convergence problem! coefficients are similar; but this is less accurate
m.asp3 = glmer(asp ~ sf0*svot +(1+sf0+svot|subject), data=stops, family="binomial")
summary(m.asp3) ### removing interaction in random effect fixed the problem!

# LENIS #

m.lenis <- glmer(lenis ~ sf0*svot +(1+sf0*svot|subject), data=stops, family="binomial")
summary(m.lenis) # FAILED TO CONVERGE
 # restart
ss <- getME(m.lenis, c("theta","fixef"))
m.lenis1 <- update(m.lenis, start=ss)
summary(m.lenis1) ### restart fixed convergence problem!

# TENSE #

m.tense <- glmer(tense ~ sf0*svot +(1+sf0*svot|subject), data=stops, family="binomial")
summary(m.tense) # FAILED TO CONVERGE
 # restart
ss <- getME(m.tense, c("theta","fixef"))
m.tense1 <- update(m.tense, start=ss)
summary(m.tense1) # restart did not fix convergence problem
 # restart with bumped max
m.tense2 <- update(m.tense, start=ss, control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(m.tense2) # restart with bumped max did not fix convergence problem
 # check singularity
tt <- getME(m.tense2,"theta")
ll <- getME(m.tense2,"lower")
min(tt[ll==0]) # not zero, so not this problem
 # Double-checking gradient calculations
derivs1 <- m.tense@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1)) # 0.04
max(pmin(abs(sc_grad1),abs(derivs1$gradient))) # 0.04 ; it should be less than 0.001
dd <- update(m.tense,devFunOnly=TRUE)
pars <- unlist(getME(m.tense,c("theta","fixef")))
grad2 <- grad(dd,pars)
hess2 <- hessian(dd,pars)
sc_grad2 <- solve(hess2,grad2)
max(pmin(abs(sc_grad2),abs(grad2))) # 0.04 ; not different... so it did not solve convergence problem
 # Try a different optimizer: bobyqa
m.tense3 <- update(m.tense,start=ss,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(m.tense3) # NO CONVERGENCE ERROR! just singular warning
tt <- getME(m.tense3,"theta")
tt
ll <- getME(m.tense,"lower") # get the lower bounds on random effects parameters
min(tt[ll==0]) # this is a very low theta value for subject.sf0:svot, so we proceed by simplifying the random effects structure to not include this term
 # remove sigularity problem by excluding interaction in random effect
m.tense4 <- glmer(tense ~ sf0*svot +(1+sf0+svot|subject), data=stops, family="binomial")
summary(m.tense4) # FAILED TO CONVERGE
 # restart
ss <- getME(m.tense4,c("theta","fixef"))
m.tense5 <- update(m.tense4,start=ss)
summary(m.tense5) ### no interaction in random effect + restart fixed convergence problem!

# Test collinearity of final models --> all below 4, so good

vif.mer(m.asp1)
vif.mer(m.lenis1)
vif.mer(m.tense5)

### FINAL MODELS ###

summary(m.asp3)
summary(m.lenis1)
summary(m.tense5)

### save df as csv file ###
write.csv(stops, file="../../../data/perception/stops.csv")
write.csv(stops_mean, file="../../../data/perception/stops_mean.csv")
