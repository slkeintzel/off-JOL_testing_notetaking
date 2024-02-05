# ----------------------------------------------------------------------------#
# Data preparation and Analysis for 
# Experts' and Novices' Metacognitive Judgements about Effectiveness of 
# Retrieval Practice and Note-Taking - 2023

# ----------------------------------------------------------------------------#

# R version used: R version 4.3.1 (2023-06-16) -- "Beagle Scouts"
# R Studio version used: 2023.06.1 "Mountain Hydrangea"

# packages
library(dplyr) # dplyr_1.1.2 
library(tidyr) # tidyr_1.3.0 
library(psych) # psych_2.3.3 
library(ggplot2) # ggplot2_3.4.2
library(jtools) # jtools_2.2.2

library(car) # car_3.1-2
library(afex) # afex_1.3-0
library(emmeans) # emmeans_1.8.5
library(effectsize) # effectsize_0.8.3

# ----------------------------------------------------------------------------#
# "Study 1: 2x3 Design" ----
# ----------------------------------------------------------------------------#
dat_stud <- read.csv("Dat_student.csv", 
                     sep = ";") # students rated for themselves

# ----------------------------------------------------------------------------#
# Data preparation ----
# ----------------------------------------------------------------------------#
# Exclude test data (Started before 2021-12-01)
dat_stud <- subset(dat_stud, subset = (!grepl("2021-11-30", STARTED)))

# Select relevant variables from data frame
dat_stud <- dat_stud %>%
  select(CASE, contains("TE0"), # Testing Items
         contains("NO0"), # Note Taking Items
         contains("SD"), # Demographics
         contains("V1")) # Ending Questions

## exclusion of participants ----
# check: Participants with missing values on V0101 ("Do you know the Testing Effect?")
sum(is.na(dat_stud$V101)) # should be 0

# "Do you know the Testing Effect?"  V101 (1 Yes, 2 No)
sum(dat_stud$V101 == 1, na.rm = T)

# Explanations of the Testing Effect
dat_stud$V102_01[which(dat_stud$V101 == 1)]

# exclude both students with correct explanations
dat_stud <- dat_stud[(dat_stud$V101 == 2),]

# ----------------------------------------------------------------------------#
## Description of the sample ----
# ----------------------------------------------------------------------------#
# Sample size: 
nrow(dat_stud) # 46

### students ----
# Age in years SD02_01
psych::describe(dat_stud$SD02_01) 

# one participant answered 2 - obvious typo --> replace age = 2 with NA
dat_stud$SD02_01[which(dat_stud$SD02_01 == 2)] <- NA

# descriptive statistics for age
psych::describe(dat_stud$SD02_01) # n = 45, mean 23.2, sd 2.72
hist(dat_stud$SD02_01)

# gender SD01
# 1 = female 
# 2 = male
# 3 = diverse
# 4 = not indicated
# -9 = not answered
table(dat_stud$SD01) # 22 female, 23 male, 1 diverse

# major subject (other: SD14_11)
# 1 = Psychologie
# 2 = Lehramt
# 3 = Naturwissenschaften
# 4 = Ingenieurswissenschaften und Technik
# 5 = Gesundheitswissenschaften
# 6 = Gesellschaftswissenschaften
# 7 = Sozialwissenschaften
# 8 = Wirtschaftswissenschaften
# 9 = Kulturwissenschaften
# 10 = Rechtswissenschaften
# 11 = Sonstiges:
#   -9 = nicht beantwortet
table(dat_stud$SD14) # no psychology
dat_stud$SD14_11

# minor subject SD20 (other: SD20_08)
# 1 = Psychologie
# 2 = Lehramt
# 3 = Naturwissenschaften und Technik
# 4 = Gesellschaftswissenschaften
# 5 = Sozialwissenschaften
# 6 = Wirtschaftswissenschaften
# 7 = Kulturwissenschaften
# 8 = Sonstiges:
#   -9 = nicht beantwortet
table(dat_stud$SD20)
dat_stud$SD20_08

# semestre SD21_01
psych::describe(dat_stud$SD21_01) # n = 45 mean 5.69, sd 3.12
hist(dat_stud$SD21_01)

# How did your Judgements come about? V103_01
# dat_stud$V103_01
# write.table(dat_stud$V103_01, "students_explanations.txt", sep="\t", row.names=FALSE, na= "")

# Comments V104_01
# dat_stud$V104_01
# write.table(dat_stud$V104_01, "students_comments.txt", sep="\t", row.names=FALSE, na= "")


# ----------------------------------------------------------------------------#
## Format final data frame ----
# ----------------------------------------------------------------------------#
# select only relevant variables
dat_stud <- dat_stud %>%
  select(CASE, contains("TE0"), contains("NO0"))

# Dataframe for JASP use (Bayesian Model Comparisons)
# write.csv(dat_stud, "JASPdat_students1.csv")

# long format 
dat_long <- tidyr::pivot_longer(dat_stud, # data frame
                                cols = c(-"CASE"), 
                                names_to = "condition", 
                                values_to = "value")

# create variable learn.meth with 1 = note taking, 0 = testing
dat_long$learn.meth <- ifelse(grepl(("NO"), dat_long$condition), 1, #"note taking", 
                              0) #"testing")

# create variable time with 0 = 5 minutes, 1 = 1 week, 2 = 2 weeks
dat_long$time <- ifelse(grepl(("01_01"), dat_long$condition) | 
                          grepl(("02"), dat_long$condition), 0, #"5 min", 
                        ifelse(grepl(("03") , dat_long$condition) | 
                                 grepl(("04"), dat_long$condition), 1, #"1 week", 
                               2)) #"2 weeks")) 

# distinguish between dvs: JOL ends with 01_01, 03_01 and 05_01
dat_long$AV <- ifelse(grepl(("01_01"), dat_long$condition) | 
                        grepl(("03_01"), dat_long$condition) | 
                        grepl(("05_01"), dat_long$condition), "JOL", "confidence")

# long format with factors in columns
dat <- pivot_wider(dat_long, 
                   id_cols = c("CASE", "time", "learn.meth"),
                   names_from = "AV", 
                   values_from = "value")

# correct classes for variables
dat$time.f <- as.factor(dat$time) # 0 = 5 mins, 1 = 1 week, 2 = 2 weeks
dat$learn.meth.f <- as.factor(dat$learn.meth) # 0 = testing, 1 = note-taking
dat_long$time.f <- as.factor(dat_long$time) # 0 = 5 mins, 1 = 1 week, 2 = 2 weeks
dat_long$learn.meth.f <- as.factor(dat_long$learn.meth) # 0 = testing, 1 = note-taking
dat$JOL <- as.numeric(dat$JOL) # off-JOL variable
dat$confidence <- as.numeric(dat$confidence) # confidence ratings in off-JOL
dat_long$value <- as.numeric(dat_long$value) # containing each DVs (AV) value in long format

# ----------------------------------------------------------------------------#
# check if assumptions for ANOVA are met ----
# ----------------------------------------------------------------------------#
## Normal distribution ----

# graphical inspection and shapiro-wilck test 

# JOL
# Testing
hist(dat_stud$TE01_01, xlim = c(0,100))
shapiro.test(dat_stud$TE01_01)
hist(dat_stud$TE03_01, xlim = c(0,100)) # bimodal 
shapiro.test(dat_stud$TE03_01) # s
hist(dat_stud$TE05_01, xlim = c(0,100)) # skewed
shapiro.test(dat_stud$TE05_01) # s 
# Note-taking
hist(dat_stud$NO01_01, xlim = c(0,100)) 
shapiro.test(dat_stud$NO01_01) # s 
hist(dat_stud$NO03_01, xlim = c(0,100)) # bimodal 
shapiro.test(dat_stud$NO03_01) # s
hist(dat_stud$NO05_01, xlim = c(0,100)) # bimodal 
shapiro.test(dat_stud$NO05_01) # s 

# confidence ratings
# Testing
hist(dat_stud$TE02_01, xlim = c(0,100)) 
shapiro.test(dat_stud$TE02_01) # s 
hist(dat_stud$TE04_01, xlim = c(0,100)) # bimod 
shapiro.test(dat_stud$TE04_01) # s
hist(dat_stud$TE06_01, xlim = c(0,100)) # skewed
shapiro.test(dat_stud$TE06_01) # s 
# Note-taking
hist(dat_stud$NO02_01, xlim = c(0,100)) 
shapiro.test(dat_stud$NO02_01) # s
hist(dat_stud$NO04_01, xlim = c(0,100)) # skewed 
shapiro.test(dat_stud$NO04_01) # s
hist(dat_stud$NO06_01, xlim = c(0,100))
shapiro.test(dat_stud$NO06_01) # s

## no grave violations considering n = 46 in each cell 


## Homoskedacity ----
# JOL
leveneTest(value ~ time.f * learn.meth.f, 
           data = dat_long[dat_long$AV == "JOL",], 
           center = median) # s 
# sd per condition JOL
aggregate(JOL ~ time.f * learn.meth.f, 
          dat,
          function(x) sd(x)) # increasing variance by time

# confidence in JOL rating
leveneTest(value ~ time.f * learn.meth.f, 
           data = dat_long[dat_long$AV == "confidence",], 
           center = median) # n.s.
# sd per condition confidence
aggregate(confidence ~ time.f * learn.meth.f, 
          dat,
          function(x) sd(x)) 

## heteroskedasticity needs to be addressed for JOL Analysis


## Sphericity ----
# covariance matrix: 
cov(dat_stud[,c("TE01_01","TE03_01","TE05_01","NO01_01","NO03_01","NO05_01")])
# correlation matrix: 
cor(dat_stud[,c("TE01_01","TE03_01","TE05_01","NO01_01","NO03_01","NO05_01")])

## (included in summary(anova), GG correction applied) 

# ----------------------------------------------------------------------------#
# ANOVA ----
# ----------------------------------------------------------------------------#

## JOL ----

# descriptive statistics JOL
describeBy(dat$JOL, 
           list(dat$time.f, dat$learn.meth.f), 
           mat=TRUE,
           digits=0)

describeBy(dat$JOL, 
           list(dat$time.f), 
           mat=TRUE,
           digits=0)

describeBy(dat$JOL, 
           list(dat$learn.meth.f), 
           mat=TRUE,
           digits=0)

# JOL NHST ANOVA (homoskedasticity violated) 
anova.JOL <- afex::aov_car(JOL ~ Error(CASE/(time.f * learn.meth.f)), 
                     data = dat, 
                     type = 3) 
summary(anova.JOL) # includes sphericity test

# time.f sig p < .001 (sphericity corrected)
# learn.meth.f sig p = 0.017

# GG corrected df
anova.JOL

# residual vs fitted plot
plot(x = fitted(anova.JOL), y = resid(anova.JOL)) 

### effectsizes ----
## generalized eta squared
eta_squared(
  anova.JOL,
  generalized = TRUE, # only manipulated independent variables in this desing
  ci = 0.95,
  verbose = TRUE)

## partial eta squared
eta_squared(
  anova.JOL,
  partial = TRUE,
  ci = 0.95,
  verbose = TRUE)

## partial omega squared 
omega_squared(
  anova.JOL,
  partial = TRUE,
  ci = 0.95,
  verbose = TRUE)

# post tests time.f
posthoc.JOL.lm <- emmeans(anova.JOL, specs = "time.f") 
pairs(posthoc.JOL.lm) # all s. 

## confidence ----

# descriptive statistics confidence
describeBy(dat$confidence, 
           list(dat$time.f, dat$learn.meth.f), 
           mat=TRUE,
           digits=0)

mean(dat$confidence)
sd(dat$confidence)

# confidence NHST ANOVA
anova.conf <- aov_car(confidence ~ Error(CASE/time.f * learn.meth.f), 
                      data = dat,
                      type = 3) 
summary(anova.conf) # includes sphericity test

# n.s.

# GG corrected df
anova.conf

# residual plot
plot(x = fitted(anova.conf), y = resid(anova.conf)) 

### effectsizes ----
## generalized eta squared
eta_squared(
  anova.conf,
  generalized = TRUE, # only manipulated independent variables in this design
  ci = 0.95,
  verbose = TRUE)

## partial eta squared
eta_squared(
  anova.conf,
  partial = TRUE,
  ci = 0.95,
  verbose = TRUE)

## partial omega squared 
omega_squared(
  anova.conf,
  partial = TRUE,
  ci = 0.95,
  verbose = TRUE)


## Linear Mixed Model to account for heteroskedasticity and sphericity violation in JOL ANOVA
library(nlme) # nlme_3.1-162

lmm.JOL.heterosk3.timeonly <- nlme::lme(fixed = JOL ~ time.f, # fixed effect for time
                                      data = dat,  
                                      random = ~ 1 + time.f + learn.meth.f | CASE, # random effect for time and learning method, no interaction (for identification)
                                      weights =  varIdent(~ 1 | time.f), # allows for heteroskedasticity on level 1 predictor time 
                                      correlation = corSymm(), # account for violation of sphericity (e.g. autocorrelation estimated freely)
                                      method = "ML",
                                      control = list(maxIter = 20000, msMaxIter = 10000, msMaxEval = 20000, opt = "nlm",
                                                     pnlsMaxIter = "1000", niterEM = "1000") # increased iterations and different optimizer for convergence
)

lmm.JOL.heterosk3.noint <- nlme::lme(fixed = JOL ~ time.f + learn.meth.f,# fixed effect for time and learning method
                                    data = dat,  
                                    random = ~ 1 + time.f + learn.meth.f | CASE, # random effect for time and learning method, no interaction (for identification)
                                    weights =  varIdent(~ 1 | time.f), # allows for heteroskedasticity on level 1 predictor time 
                                    correlation = corSymm(), # account for violation of sphericity (e.g. autocorrelation estimated freely)
                                    method = "ML",
                                    control = list(maxIter = 20000, msMaxIter = 10000, msMaxEval = 20000, opt = "nlm",
                                                   pnlsMaxIter = "1000", niterEM = "1000") # increased iterations and different optimizer for convergence
)

lmm.JOL.heterosk3.full <- nlme::lme(fixed = JOL ~ time.f * learn.meth.f, # fixed effect for time and learning method and interaction
                                   data = dat,  
                                   random = ~ 1 + time.f + learn.meth.f | CASE, # random effect for time and learning method, no interaction (for identification)
                                   weights =  varIdent(~ 1 | time.f), # allows for heteroskedasticity on level 1 predictor time 
                                   correlation = corSymm(), # account for violation of sphericity (e.g. autocorrelation estimated freely)
                                   method = "ML",
                                   control = list(maxIter = 20000, msMaxIter = 10000, msMaxEval = 20000, opt = "nlm",
                                                  pnlsMaxIter = "1000", niterEM = "1000") # increased iterations and different optimizer for convergence
)

# Likelihood Ratio test for model selection
anova(lmm.JOL.heterosk3.timeonly, lmm.JOL.heterosk3.noint, lmm.JOL.heterosk3.full)
# prefers Model with both main effects and no interaction
summary(lmm.JOL.heterosk3.noint)

# regression diagnostics

# residual plots for each learning method - no more heteroskedasticity
plot(x = dat$time[dat$learn.meth == 0], y = resid(lmm.JOL.heterosk3.noint, level = 0, type = "response")[dat$learn.meth == 0])
plot(x = dat$time[dat$learn.meth == 1], y = resid(lmm.JOL.heterosk3.noint, level = 0, type = "response")[dat$learn.meth == 1])

# fitted value plots for each learning method
plot(x = dat$time[dat$learn.meth == 0], y = fitted(lmm.JOL.heterosk3.noint)[dat$learn.meth == 0])
plot(x = dat$time[dat$learn.meth == 1], y = fitted(lmm.JOL.heterosk3.noint)[dat$learn.meth == 1])

# residual correlation between time 1 and time 2
cor(resid(lmm.JOL.heterosk3.noint, level = 0, type = "response")[dat$time.f == 0],
    resid(lmm.JOL.heterosk3.noint, level = 0, type = "response")[dat$time.f == 1])
# residual correlation between time 2 and time 3
cor(resid(lmm.JOL.heterosk3.noint, level = 0, type = "response")[dat$time.f == 1],
    resid(lmm.JOL.heterosk3.noint, level = 0, type = "response")[dat$time.f == 2]) 

# ----------------------------------------------------------------------------#
# Plots ----
# ----------------------------------------------------------------------------#
# nice violine plots

# JOL for Students and Teachers by learning method over time
plot_JOL <- afex_plot(anova.JOL, x = "time.f", trace = "learn.meth.f",
                      mapping = c("shape", "color"),
                      factor_levels = list(time.f = c("5 Minutes", "1 Week", "2 Weeks"),
                                           learn.meth.f = c("Testing", "Note-Taking")),
                      legend_title = "Learning method",
                      error = "within",
                      error_ci = T,
                      data_geom = geom_violin) +
  labs(y = "Offline Judgement of Learning (%)", x = "Time Interval")

plot_JOL + theme_minimal() # or theme_bw()
plot_JOL + theme_apa() 

# confidence for Students and Teachers by learning method over time
plot_conf <- afex_plot(anova.conf, x = "time.f", trace = "learn.meth.f",
                       mapping = c("shape", "color"),
                       factor_levels = list(time.f = c("5 Ḿinutes", "1 Week", "2 Weeks"),
                                            learn.meth.f = c("Testing", "Note-Taking")), 
                       legend_title = "Learning Method",
                       error = "within",
                       error_ci = T,
                       data_geom = geom_violin) +
  labs(y = "Confidence Ratings (%)", x = "Time Interval")

plot_conf + theme_minimal() # or theme_bw()
plot_conf + theme_apa() 

# clear environment for next analysis 
rm(list = ls())

# ----------------------------------------------------------------------------#
# "Study 2: 2x3x3 Design" ----
# ----------------------------------------------------------------------------#
# Data 
dat_teach <- read.csv("Dat_teach.csv", 
                      sep = ";")
dat_stud <- read.csv("Dat_student_2.csv",
                      sep = ";") # students rated for a high school student

# ----------------------------------------------------------------------------#
# Data preparation ----
# ----------------------------------------------------------------------------#
# Exclude test data (Started before 2021-12-01)
dat_teach <- subset(dat_teach, subset = (!grepl("2021-11-30", STARTED)))

dat_stud <- subset(dat_stud, subset = (!grepl("2022-06-07", STARTED)))

# Select relevant variables from data frame
dat_teach <- dat_teach %>%
  select(CASE, contains("TE0"), contains("NO0"), contains("SD"), contains("V1"))

dat_stud <- dat_stud %>%
  select(CASE, contains("TE0"), contains("NO0"), contains("SD"), contains("V1"))

## exclusion of participants ----
# check: Participants with missing values on V0101 ("Do you know the Testing Effect?")
sum(is.na(dat_teach$V101)) # should be 0
sum(is.na(dat_stud$V101)) # should be 0

### teachers knowledge of the testing effect ----
# "Do you know the Testing Effect?" V101 (1 Yes, 2 No)
sum(dat_teach$V101 == 1, na.rm = T)

# Explanations of the Testing Effect
dat_teach$V102_01[which(dat_teach$V101 == 1)]

# exclude teacher with correct explanation
dat_teach <- dat_teach[!(dat_teach$V102_01 == 
                           "Durch Tests wie Multiple-Choice-Fragen erh\xf6ht sich die Erinnerungsleistung zu gelesenen Texten"
),]

### students knowledge of the testing effect ----
# "Do you know the Testing Effect?"  V101 (1 Yes, 2 No)
sum(dat_stud$V101 == 1, na.rm = T)

# Explanations of the Testing Effect
dat_stud$V102_01[which(dat_stud$V101 == 1)]

# exclude students with correct explanations
dat_stud <- dat_stud[!(dat_stud$V102_01 == 
                         "Retrieval practice (Abruf) ist bei langfristigem Lernen besser f\xfcr die Behaltensleistung als wiederholendes Lernen" |
                         dat_stud$V102_01 == 
                         "Abruf aus dem Ged\xe4chtnis ist hilfreicher um eine Information langfristig zu behalten als reine Wiederholung "
),]

# ----------------------------------------------------------------------------#
## Description of the sample ----
# ----------------------------------------------------------------------------#
# Sample sizes: 
nrow(dat_teach) # 46
nrow(dat_stud) # 47 --> one will be excluded due to not being a student
dat_stud <- dat_stud[!dat_stud$SD14_11 == "kein Studium ",] # "not a student"
nrow(dat_stud) # now 46 

### teachers----
# Age in years SD02_01
describe(dat_teach$SD02_01) # mean 43.24, sd 11.16
hist(dat_teach$SD02_01)

# gender SD01
# 1 = female 
# 2 = male
# 3 = diverse
# 4 = not indicated
# -9 = not answered
table(dat_teach$SD01) # 25 female, 19 male, 1 not indicated

# school SD14 (other: SD14_08)
# 1 = Grundschule 
# 2 = Gesamtschule
# 3 = Hauptschule/Werkrealschule
# 4 = Realschule/Oberschule
# 5 = Gymnasium
# 9 = Förderschule
# 8 = Sonstiges:
#   -9 = nicht beantwortet
table(dat_teach$SD14) # 2 Grundschule, 3 Gesamtschule, 18 Realschule, 21 Gymnasium, 1 Berufskolleg, 1 Freiberufl.
dat_teach$SD14_08

# job experience in years SD21_01
describe(dat_teach$SD21_01) # mean 15.91, sd 11.6
hist(dat_teach$SD21_01)

# How did your Judgements come about? V103_01
# dat_teach$V103_01
# write.table(dat_teach$V103_01, "teacher_explanations.txt", sep="\t", row.names=FALSE, na= "")

# Comments V104_01
# dat_teach$V104_01
# write.table(dat_teach$V104_01, "teacher_comments.txt", sep="\t", row.names=FALSE, na= "")

### students ----
# Age in years SD02_01
describe(dat_stud$SD02_01) # n = 46, mean 23.78, sd 3.04, min 19 max 32

hist(dat_stud$SD02_01)

# gender SD01
# 1 = female 
# 2 = male
# 3 = diverse
# 4 = not indicated
# -9 = not answered
table(dat_stud$SD01) # 20 female, 25 male, 1 not indicated

# major (other: SD14_11)
# 1 = Psychologie
# 2 = Lehramt
# 3 = Naturwissenschaften
# 4 = Ingenieurswissenschaften und Technik
# 5 = Gesundheitswissenschaften
# 6 = Gesellschaftswissenschaften
# 7 = Sozialwissenschaften
# 8 = Wirtschaftswissenschaften
# 9 = Kulturwissenschaften
# 10 = Rechtswissenschaften
# 11 = Sonstiges:
#   -9 = nicht beantwortet
table(dat_stud$SD14) # 5 psychology
dat_stud$SD14_11

# second subject SD20 (other: SD20_08)
# 1 = Psychologie
# 2 = Lehramt
# 3 = Naturwissenschaften und Technik
# 4 = Gesellschaftswissenschaften
# 5 = Sozialwissenschaften
# 6 = Wirtschaftswissenschaften
# 7 = Kulturwissenschaften
# 8 = Sonstiges:
#   -9 = nicht beantwortet
table(dat_stud$SD20) # 2 psychology
dat_stud$SD20_08

# semestre SD21_01
describe(dat_stud$SD21_01) # n = 46 mean 6.22, sd 3.55, min 1, max 16
hist(dat_stud$SD21_01)

# How did your Judgements come about? V103_01
# dat_stud$V103_01
# write.table(dat_stud$V103_01, "students_explanations.txt", sep="\t", row.names=FALSE, na= "")

# Comments V104_01
# dat_stud$V104_01
# write.table(dat_stud$V104_01, "students_comments.txt", sep="\t", row.names=FALSE, na= "")

# ----------------------------------------------------------------------------#
## Format final data frame ----
# ----------------------------------------------------------------------------#
# select only relevant variables
dat_teach <- dat_teach %>%
  select(CASE, contains("TE0"), contains("NO0"))
dat_stud <- dat_stud %>%
  select(CASE, contains("TE0"), contains("NO0"))

# create new variable to code expertise
dat_teach$expertise <- 1 # "teacher"
dat_stud$expertise <- 0 #"student"

# New CASE Variable (to avoid same participant numbers for teachers and students)
dat_teach$CASE <- 1:nrow(dat_teach) + 100
dat_stud$CASE <- 1:nrow(dat_stud) 

# create final dataframe with students and teachers
dat_tot <- rbind(dat_teach, dat_stud) 

# Dataframe for JASP use (Bayesian Model Comparisons)
# write.csv(dat_tot, "JASPdat_teach_vs_stud2.csv")

# long format 
dat_long <- tidyr::pivot_longer(dat_tot, # data frame
                                cols = c(-"CASE", -"expertise"), 
                                names_to = "condition", 
                                values_to = "value")

dat_long$learn.meth <- ifelse(grepl(("NO"), dat_long$condition), 1, #"note taking", 
                              0) #"testing")

dat_long$time <- ifelse(grepl(("01_01"), dat_long$condition) | 
                          grepl(("02"), dat_long$condition), 0, #"5 min", 
                        ifelse(grepl(("03") , dat_long$condition) | 
                                 grepl(("04"), dat_long$condition), 1, #"1 week", 
                               2)) #"2 weeks")) 

dat_long$AV <- ifelse(grepl(("01_01"), dat_long$condition) | 
                        grepl(("03"), dat_long$condition) | 
                        grepl(("05"), dat_long$condition), "JOL", "confidence")

dat <- pivot_wider(dat_long, 
                   id_cols = c("CASE", "time", "learn.meth", "expertise"),
                   names_from = "AV", 
                   values_from = "value")

# correct classes for variables
dat$time.f <- as.factor(dat$time) # 0 = 5 mins, 1 = 1 week, 2 = 2 weeks
dat$learn.meth.f <- as.factor(dat$learn.meth) # 0 = testing, 1 = note-taking
dat$expertise.f <- as.factor(dat$expertise) # 0 = students, 1 = teachers
dat_long$time.f <- as.factor(dat_long$time) # 0 = 5 mins, 1 = 1 week, 2 = 2 weeks
dat_long$learn.meth.f <- as.factor(dat_long$learn.meth) # 0 = testing, 1 = note-taking
dat_long$expertise.f <- as.factor(dat_long$expertise) # 0 = students, 1 = teachers
dat$JOL <- as.numeric(dat$JOL) # off-JOL variable
dat$confidence <- as.numeric(dat$confidence) # confidence ratings in off-JOL
dat_long$value <- as.numeric(dat_long$value) # containing each DVs (AV) value in long format

# ----------------------------------------------------------------------------#
# check if assumptions for ANOVA are met ----
# ----------------------------------------------------------------------------#
## Normal distribution ----

# graphical inspection and shapiro-wilck test 

# JOL
## teachers
# Testing
hist(dat_teach$TE01_01, xlim = c(0,100))
shapiro.test(dat_teach$TE01_01)
hist(dat_teach$TE03_01, xlim = c(0,100)) 
shapiro.test(dat_teach$TE03_01) # s
hist(dat_teach$TE05_01, xlim = c(0,100)) # skewed
shapiro.test(dat_teach$TE05_01) # s 
# Note-taking
hist(dat_teach$NO01_01, xlim = c(0,100)) 
shapiro.test(dat_teach$NO01_01) 
hist(dat_teach$NO03_01, xlim = c(0,100))
shapiro.test(dat_teach$NO03_01)
hist(dat_teach$NO05_01, xlim = c(0,100)) # skewed
shapiro.test(dat_teach$NO05_01) # s 

## students
# Testing
hist(dat_stud$TE01_01, xlim = c(0,100))
shapiro.test(dat_stud$TE01_01)
hist(dat_stud$TE03_01, xlim = c(0,100)) # skewed!
shapiro.test(dat_stud$TE03_01) # s
hist(dat_stud$TE05_01, xlim = c(0,100)) # skewed
shapiro.test(dat_stud$TE05_01) # s 
# Note-taking
hist(dat_stud$NO01_01, xlim = c(0,100)) # skewed
shapiro.test(dat_stud$NO01_01) # s 
hist(dat_stud$NO03_01, xlim = c(0,100)) 
shapiro.test(dat_stud$NO03_01) 
hist(dat_stud$NO05_01, xlim = c(0,100)) # skewed 
shapiro.test(dat_stud$NO05_01) # s 

# confidence ratings
## teachers
# Testing
hist(dat_teach$TE02_01, xlim = c(0,100)) # bimod
shapiro.test(dat_teach$TE02_01) # s 
hist(dat_teach$TE04_01, xlim = c(0,100)) 
shapiro.test(dat_teach$TE04_01) # s
hist(dat_teach$TE06_01, xlim = c(0,100)) 
shapiro.test(dat_teach$TE06_01) # s 
# Note-taking
hist(dat_teach$NO02_01, xlim = c(0,100)) # bimod
shapiro.test(dat_teach$NO02_01) # s
hist(dat_teach$NO04_01, xlim = c(0,100)) # skewed 
shapiro.test(dat_teach$NO04_01) # s
hist(dat_teach$NO06_01, xlim = c(0,100)) # skewed
shapiro.test(dat_teach$NO06_01) # s

# students
# Testing
hist(dat_stud$TE02_01, xlim = c(0,100)) # skewed
shapiro.test(dat_stud$TE02_01) # s 
hist(dat_stud$TE04_01, xlim = c(0,100)) 
shapiro.test(dat_stud$TE04_01) # s
hist(dat_stud$TE06_01, xlim = c(0,100))
shapiro.test(dat_stud$TE06_01) 
# Note-taking
hist(dat_stud$NO02_01, xlim = c(0,100)) 
shapiro.test(dat_stud$NO02_01) # s
hist(dat_stud$NO04_01, xlim = c(0,100))
shapiro.test(dat_stud$NO04_01) 
hist(dat_stud$NO06_01, xlim = c(0,100))
shapiro.test(dat_stud$NO06_01) 

## overall no grave violations considering n = 46 in each cell


## Homoskedacity ----
leveneTest(value ~ time.f * learn.meth.f * expertise.f, 
           data = dat_long[dat_long$AV == "JOL",], 
           center = median) # n.s. 
# sd per condition JOL
aggregate(JOL ~ time.f * learn.meth.f * expertise.f, 
          dat,
          function(x) sd(x)) # increasing variance by time

leveneTest(value ~ time.f * learn.meth.f * expertise.f, 
           data = dat_long[dat_long$AV == "confidence",], 
           center = median) # n.s.
# sd per condition confidence
aggregate(confidence ~ time.f * learn.meth.f * expertise.f, 
          dat,
          function(x) sd(x)) 

## no correction needed


## Sphericity ----
# (included in summary(anova), GG correction applied) 

## Equality of covariances ----
library(rstatix) # Version ‘0.7.0’ 

# subset with relevant variables (first subject variable, then between factor)
box_sub_JOL <- dat_tot %>%
  select(expertise, "TE01_01","TE03_01","TE05_01","NO01_01","NO03_01","NO05_01") 

box_sub_JOL$expertise <- as.factor(box_sub_JOL$expertise)

box_sub_conf <- dat_tot %>%
  select(expertise, "TE02_01","TE04_01","TE06_01","NO02_01","NO04_01","NO06_01") 

box_sub_conf$expertise <- as.factor(box_sub_conf$expertise)

# box_m test
box_m(box_sub_JOL[, -1], box_sub_JOL$expertise) #s

box_m(box_sub_conf[, -1], box_sub_JOL$expertise) #s

# Covariance Matrices per expertise
cov(dat_stud[,c("TE01_01","TE03_01","TE05_01","NO01_01","NO03_01","NO05_01")])
cov(dat_teach[,c("TE01_01","TE03_01","TE05_01","NO01_01","NO03_01","NO05_01")])
# Correlation Matrices per expertise
cor(dat_stud[,c("TE01_01","TE03_01","TE05_01","NO01_01","NO03_01","NO05_01")])
cor(dat_teach[,c("TE01_01","TE03_01","TE05_01","NO01_01","NO03_01","NO05_01")])

# unload package (interferes with effectsize)
detach("package:rstatix")

## not met, but test is also sensitive to violations of the normality assumption
# + sphericity assumption is not met (see below), GG correction applied

# ----------------------------------------------------------------------------#
# ANOVA ----
# ----------------------------------------------------------------------------#

## JOL ----

# descriptive statistics JOL
describeBy(dat$JOL, 
           list(dat$time.f, dat$learn.meth.f, dat$expertise.f), 
           mat=TRUE,
           digits=0)

describeBy(dat$JOL, 
           dat$time.f, 
           mat=TRUE,
           digits=0)

describeBy(dat$JOL, 
           dat$learn.meth.f, 
           mat=TRUE,
           digits=0)

# JOL NHST ANOVA (homoskedasticity violated) 
anova.JOL <- afex::aov_car(JOL ~ expertise.f + Error(CASE/(time.f * learn.meth.f)), 
                           data = dat, 
                           type = 3) 
summary(anova.JOL) # includes sphericity test

# time.f sig p < .001 (sphericity corrected)
# learn.meth.f sig p < .001 

# GG corrected df
anova.JOL

# residual vs fitted plot
plot(x = fitted(anova.JOL), y = resid(anova.JOL)) 

### effectsizes ----
## generalized eta squared
eta_squared(
  anova.JOL,
  generalized = "expertise", # expertise is no manipulated independent variables 
  ci = 0.95,
  verbose = TRUE)

## partial eta squared
eta_squared(
  anova.JOL,
  partial = TRUE,
  ci = 0.95,
  verbose = TRUE)

## partial omega squared 
omega_squared(
  anova.JOL,
  partial = TRUE,
  ci = 0.95,
  verbose = TRUE)

# post tests time.f
posthoc.JOL.lm <- emmeans(anova.JOL, specs = "time.f") 
pairs(posthoc.JOL.lm) # all s. 

## confidence ----

# descriptive statistics confidence
describeBy(dat$confidence, 
           list(dat$time.f, dat$learn.meth.f, dat$expertise.f), 
           mat=TRUE,
           digits=0)
mean(dat$confidence)
sd(dat$confidence)

# confidence NHST ANOVA
anova.conf <- aov_car(confidence ~ expertise.f + Error(CASE/time.f * learn.meth.f), 
                      data = dat,
                      type = 3) 
summary(anova.conf) # includes sphericity test

# n.s.

# GG corrected df
anova.conf

# residual plot
plot(x = fitted(anova.conf), y = resid(anova.conf)) 

### effectsizes ----
## generalized eta squared
eta_squared(
  anova.conf,
  generalized = "expertise", # expertise is no manipulated independent variables 
  ci = 0.95,
  verbose = TRUE)

## partial eta squared
eta_squared(
  anova.conf,
  partial = TRUE,
  ci = 0.95,
  verbose = TRUE)

## partial omega squared 
omega_squared(
  anova.conf,
  partial = TRUE,
  ci = 0.95,
  verbose = TRUE)


# ----------------------------------------------------------------------------#
# Plots ----
# ----------------------------------------------------------------------------#
# nice violine plots

library("jtools")

# JOL for Students and Teachers by learning method over time
plot_JOL <- afex_plot(anova.JOL, x = "time.f", trace = "learn.meth.f", panel = "expertise.f",
                      mapping = c("shape", "color"),
                      factor_levels = list(time.f = c("5 Minutes", "1 Week", "2 Weeks"),
                                           learn.meth.f = c("Testing", "Note-taking"),
                                           expertise.f = c("Students", "Teachers")), 
                      legend_title = "Learning Method",
                      error = "model",
                      error_ci = T,
                      data_geom = geom_violin) +
  labs(y = "Offline Judgement of Learning (%)", x = "Time Interval")

plot_JOL + theme_minimal() # or theme_bw()
plot_JOL + theme_apa() 

# confidence for Students and Teachers by learning method over time
plot_conf <- afex_plot(anova.conf, x = "time.f", trace = "learn.meth.f", panel = "expertise.f",
                       mapping = c("shape", "color"),
                       factor_levels = list(time.f = c("5 Minutes", "1 Week", "2 Weeks"),
                                            learn.meth.f = c("Testing", "Note-taking"),
                                            expertise.f = c("Students", "Teachers")), 
                       legend_title = "Learning Method",
                       error = "model",
                       error_ci = T,
                       data_geom = geom_violin) +
  labs(y = "Confidence Ratings (%)", x = "Time Interval")

plot_conf + theme_minimal() # or theme_bw()
plot_conf + theme_apa() 



# ----------------------------------------------------------------------------#
# Jul 2023 

