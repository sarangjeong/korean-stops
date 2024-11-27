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
library(lme4)
library(ggplot2)
library(MuMIn)
library(brms)
library(glmmTMB)
source("helpers.R")

##################
# data wrangling #
##################

# load worker id data & filter only those w prolific id
worker_id <- read.csv("../data/korean_stops_perception_3_poa_all_ages-workerids.csv", stringsAsFactors = TRUE)
workerid_with_prolific_id <- worker_id$workerid[worker_id$prolific_participant_id != "<no-id>"]
workerid_with_no_prolific_id <- worker_id$workerid[worker_id$prolific_participant_id == "<no-id>"]
workerid_with_prolific_id
workerid_with_no_prolific_id

# load trial data
trials <- read.csv("../data/korean_stops_perception_3_poa_all_ages-trials.csv", stringsAsFactors = TRUE)

# load subject info data
subject_info <- read.csv("../data/korean_stops_perception_3_poa_all_ages-subject_information.csv")
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
view(subject_info)
subject_info$age[subject_info$age == 196707] <- 2024-1967

# merge data
nrow(trials)
stops <- trials %>%
  left_join(.,subject_info,by=c("workerid"))
nrow(stops) # check if # of rows did not change

view(stops)
# change column names for subject & condition
stops <- stops %>%
  rename(subject = workerid,
         rt = response_time_milliseconds
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

# 2) kyungsang dialect: 3 (I manually checked the subject info data to identify KS speakers)
# TODO: if I collect more data, do the manual checking again!
ks_speaker <- c("426")
stops <- stops[!(stops$subject %in% ks_speaker) ,]
nrow(stops)

# 3) more than 3 non-aspirated responses for vot >= 7 & f0 >= 7: 1 participant ("453")
stops[stops$f0 >= 7 & stops$vot >= 7 & stops$response!="asp",]$subject
stops <- stops[(stops$subject != 453) ,]
nrow(stops)

# 4) not_heard == TRUE
stops <- stops[(stops$not_heard == "False") ,]
nrow(stops)

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
  mutate(sf0 = scale(f0), svot = scale(vot), sage = scale(age))
view(stops)
### data wrangling for PLOTS ###
# TODO: plot young vs old separately

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

# identify tied cases --> turns out none; TODO: skip the below steps
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

# manually change the values when the largest category is not single; TODO: skip this step
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

# TODO: decide btwn `vot * age + f0 * age` vs. `vot * f0 * age`

stops$response <- relevel(stops$response, ref = "lenis")

brm_model <- brm(
  formula = response ~ svot * sage + sf0 * sage + (1 + svot + sf0 | subject),
  data = stops,
  family = categorical(link = "logit"),  # Multinomial logistic regression
  cores = 4,                             # Use multiple cores for faster computation
  iter = 2000,                           # Number of iterations (adjust as needed)
  control = list(adapt_delta = 0.95)     # Helps convergence for complex models
)
summary(brm_model)

lenis_asp <- stops %>%
  filter(lenis==1 | asp==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

lenis_tense <- stops %>%
  filter(lenis==1 | tense==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

asp_tense <- stops %>%
  filter(asp==1 | tense==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

# fixed effects only

m.lenis_asp = glm(asp ~ sf0 * sage + svot * sage, data=lenis_asp, family="binomial")
summary(m.lenis_asp)

m.lenis_tense = glm(tense ~ sf0 * sage + svot * sage, data=lenis_tense, family="binomial")
summary(m.lenis_tense)

m.asp_tense = glm(asp ~ sf0 * sage + svot * sage, data=asp_tense, family="binomial")
summary(m.asp_tense)

# Check the collinearity
cor(lenis_asp[c("sf0", "svot", "sage")])


control = glmerControl(optimizer = "bobyqa", 
                       optCtrl = list(maxfun = 1e5))

# Lenis vs Asp
m.random.lenis_asp = glmer(
  asp ~ sf0 * sage + svot * sage + (1 + sf0 + svot | subject),
  data=lenis_asp,
  family="binomial",
  control = control
)
summary(m.random.lenis_asp)

# Lenis vs Tense
m.random.lenis_tense = glmer(
  tense ~ sf0 * sage + svot * sage + (1 + sf0 + svot | subject),
  data=lenis_tense,
  family="binomial",
  control = control
)
summary(m.random.lenis_tense)

# Asp vs Tense
m.random.asp_tense = glmer(
  asp ~ sf0 * sage + svot * sage + (1 + sf0 + svot | subject),
  data=asp_tense,
  family="binomial",
  control = control
)
summary(m.random.asp_tense)

# --- Legacy ---
  
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
