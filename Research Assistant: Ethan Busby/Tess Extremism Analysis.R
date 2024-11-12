###TESS EXTREMISM ANALYSIS
###Kendra Pinegar; July 11, 2023

###NOTE: Plots surrounded by "### (plot) ###" were exported and sent to Dr. Busby

###### 
###Load Tess into my global environment

View(tess_long)
library(cregg)
library(tidyverse)
library(ggplot2)

#####
###Interactions with respondent's party

#Creating the regression variables that I'm using for interactions
f1.g.1 <- extreme ~ race + demdescribe + repdescribe + civil + norm + talk + viol + law
f2.g.1 <- vote ~ race + demdescribe + repdescribe + civil + norm + talk + viol + law
f3.g.1 <- speech ~ race + demdescribe + repdescribe + civil + norm + talk + viol + law
f4.g.1 <- teach ~ race + demdescribe + repdescribe + civil + norm + talk + viol + law

#Turn pid_respondent into a 3-category party variable
tess_long$party_respondent[tess_long$pid_respondent %in% c(1, 2, 3)] = "Democrat"
tess_long$party_respondent[tess_long$pid_respondent == 4] = "Independent"
tess_long$party_respondent[tess_long$pid_respondent %in% c(5, 6, 7)] = "Republican"
tess_long$party_respondent = factor(tess_long$party_respondent,
                                    levels = c("Democrat", "Independent", "Republican"))

#I can't run anything yet because there are 18 missing observations in party_respondent, so remove those
tess_long <- subset(tess_long, party_respondent == "Democrat" | party_respondent == "Independent" | party_respondent == "Republican")
#Now the issue has been fixed, so I should be good to move on

#####
###Interactions: Extreme, Vote, Speech, and Teach
###Step 1: determine if there's a significant difference between the values using anova
###Step 2: if significant, calculate marginal means and difference in marginal means
###Step 3: plot both marginal means and difference in marginal means

#Extreme
cj_extreme_rparty = cj_anova(tess_long, f1.g.1, by = ~party_respondent)

cj_extreme_rparty
#Significant (p=0.000)

mms_extreme_rparty = cj(tess_long, f1.g.1,
                        estimate = "mm", id = ~CaseId, by= ~party_respondent)

diff_extreme_rparty = cj(tess_long, f1.g.1,
                         estimate = "mm_diff", id = ~CaseId, by = ~party_respondent)

###
plot(mms_extreme_rparty, xlab = "Extreme", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

plot(rbind(mms_extreme_rparty, diff_extreme_rparty), xlab = "Extreme", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol=3L)

#Vote
cj_vote_rparty = cj_anova(tess_long, f2.g.1, by = ~party_respondent)

cj_vote_rparty
#Significant (p=0.000)

mms_vote_rparty = cj(tess_long, f2.g.1,
                     estimate = "mm", id = ~CaseId, by = ~party_respondent)

diff_vote_rparty = cj(tess_long, f2.g.1,
                      estimate = "mm_diff", id = ~CaseId, by = ~party_respondent)

###
plot(mms_vote_rparty, xlab = "Vote", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

plot(rbind(mms_vote_rparty, diff_vote_rparty), xlab = "Vote", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)

#Speech
cj_speech_rparty = cj_anova(tess_long, f3.g.1, by = ~party_respondent)

cj_speech_rparty
#Significant (p=0.000)

mms_speech_rparty = cj(tess_long, f3.g.1,
                       estimate = "mm", id = ~CaseId, by = ~party_respondent)

diff_speech_rparty = cj(tess_long, f3.g.1,
                        estimate = "mm_diff", id = ~CaseId, by = ~party_respondent)

###
plot(mms_speech_rparty, xlab = "Speech", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

plot(rbind(mms_speech_rparty, diff_speech_rparty), xlab = "Speech", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)

#Teach
cj_teach_rparty = cj_anova(tess_long, f4.g.1, by = ~party_respondent)

cj_teach_rparty
#Significant (p=0.000)

mms_teach_rparty = cj(tess_long, f4.g.1,
                      estimate = "mm", id = ~CaseId, by = ~party_respondent)

diff_teach_rparty = cj(tess_long, f4.g.1,
                       estimate = "mm_diff", id = ~CaseId, by = ~party_respondent)

###
plot(mms_teach_rparty, xlab = "Teach", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

plot(rbind(mms_teach_rparty, diff_teach_rparty), xlab = "Teach") + ggplot2::facet_wrap(~BY, ncol = 3L)

#####
###Creating binary variables for extreme characteristics
###Variables to focus on: demdescribe, repdescribe, civil, norm, talk, viol, law, ideology (being far-left or far-right)

#First, let's identify the extremist categories in each variable
table(tess_long$demdescribe)
#"D-Extreme"
table(tess_long$repdescribe)
#"R-Extreme"
table(tess_long$civil)
#"Uncivil"
table(tess_long$norm)
#"Anti-American", "Anti-Christian", "Racist", "Sexist"
table(tess_long$talk)
#"Almost always"
table(tess_long$viol)
#"Violence is often necessary"
table(tess_long$law)
#"More important to do what people want"
table(tess_long$ownpid)
#"Far-left Democrat", "Far-right Republican"

tess_long$demdescribe_extreme = 0
tess_long$demdescribe_extreme[tess_long$demdescribe == "D-Extreme"] = 1

tess_long$repdescribe_extreme = 0
tess_long$repdescribe_extreme[tess_long$repdescribe == "R-Extreme"] = 1

tess_long$uncivil = 0
tess_long$uncivil[tess_long$civil == "Uncivil"] = 1

tess_long$norm_antichristian = 0
tess_long$norm_antichristian[tess_long$norm == "Anti-Christian"] = 1

tess_long$norm_antiamerican = 0
tess_long$norm_antiamerican[tess_long$norm == "Anti-American"] = 1

tess_long$norm_sexist = 0
tess_long$norm_sexist[tess_long$norm == "Sexist"] = 1

tess_long$norm_racist = 0
tess_long$norm_racist[tess_long$norm == "Racist"] = 1

tess_long$talk_always = 0
tess_long$talk_always[tess_long$talk == "Almost always"] = 1

tess_long$viol_supports = 0
tess_long$viol_supports[tess_long$viol == "Violence is often necessary"] = 1

tess_long$law_people = 0
tess_long$law_people[tess_long$law == "More important to do what people want"] = 1

tess_long$ideo_farright = 0
tess_long$ideo_farright[tess_long$ownpid == "Far-right Republican"] = 1

tess_long$ideo_farleft = 0
tess_long$ideo_farleft[tess_long$ownpid == "Far-left Democrat"] = 1

#These are all of the conjoint profile attributes that can be considered extreme

#Count_extreme1 includes demdescribe and repdescribe
tess_long$count_extreme1 = tess_long$demdescribe_extreme +
  tess_long$repdescribe_extreme +
  tess_long$uncivil +
  tess_long$norm_antichristian +
  tess_long$norm_antiamerican +
  tess_long$norm_sexist +
  tess_long$norm_racist +
  tess_long$talk_always +
  tess_long$viol_supports +
  tess_long$law_people +
  tess_long$ideo_farright +
  tess_long$ideo_farleft

#Count_extreme2 excludes demdescribe and repdescribe
tess_long$count_extreme2 = tess_long$uncivil +
  tess_long$norm_antichristian +
  tess_long$norm_antiamerican +
  tess_long$norm_sexist +
  tess_long$norm_racist +
  tess_long$talk_always +
  tess_long$viol_supports +
  tess_long$law_people +
  tess_long$ideo_farright +
  tess_long$ideo_farleft

#Turn the variables into factor variables
tess_long$count_extreme1 = as.factor(tess_long$count_extreme1)
tess_long$count_extreme2 = as.factor(tess_long$count_extreme2)

##Merge the edge numbers of count1 and count2
table(tess_long$count_extreme1)
tess_long$count_extreme1[tess_long$count_extreme1 == 0] = 1
tess_long$count_extreme1[tess_long$count_extreme1 == 8] = 7
table(tess_long$count_extreme2)
tess_long$count_extreme2[tess_long$count_extreme2 == 0] = 1
tess_long$count_extreme2[tess_long$count_extreme2 == 6] = 5

#Create equations for the count_extreme1 and 2 variables with JUST the count variable
f1.c.1 <- extreme ~ count_extreme1
f2.c.1 <- vote ~ count_extreme1
f3.c.1 <- speech ~ count_extreme1
f4.c.1 <- teach ~ count_extreme1

f1.c.2 <- extreme ~ count_extreme2
f2.c.2 <- vote ~ count_extreme2
f3.c.2 <- speech ~ count_extreme2
f4.c.2 <- teach ~ count_extreme2

#####
###Basic conjoint analysis for count1 and count2

#Count_extreme1
mms_extreme_count1 = cj(tess_long, f1.c.1,
                        estimate = "mm", id = ~CaseId)
plot(mms_extreme_count1, vline = 0.5)

mms_vote_count1 = cj(tess_long, f2.c.1,
                     estimate = "mm", id = ~CaseId)
plot(mms_vote_count1, vline = 0.5)

mms_speech_count1 = cj(tess_long, f3.c.1,
                       estimate = "mm", id = ~CaseId)
plot(mms_speech_count1, vline = 0.5)

mms_teach_count1 = cj(tess_long, f4.c.1,
                      estimate = "mm", id = ~CaseId)
plot(mms_teach_count1, vline = 0.5)

#Count_extreme2
mms_extreme_count2 = cj(tess_long, f1.c.2,
                        estimate = "mm", id = ~CaseId)
plot(mms_extreme_count2, vline= 0.5)

mms_vote_count2 = cj(tess_long, f2.c.2,
                     estimate = "mm", id = ~CaseId)
plot(mms_vote_count2, vline = 0.5)

mms_speech_count2 = cj(tess_long, f3.c.2,
                       estimate = "mm", id = ~CaseId)
plot(mms_speech_count2, vline = 0.5)

mms_teach_count2 = cj(tess_long, f4.c.2,
                      estimate = "mm", id = ~CaseId)
plot(mms_teach_count2, vline = 0.5)

plot(rbind(mms_extreme_count1, mms_extreme_count2), xlab = "Extreme", vline = 0.5)
plot(rbind(mms_vote_count1, mms_vote_count2), xlab = "Vote", vline = 0.5)
plot(rbind(mms_speech_count1, mms_speech_count2), xlab = "Speech", vline = 0.5)
plot(rbind(mms_teach_count1, mms_teach_count2), xlab = "Teach", vline = 0.5)

#####
###Interactions with party variable

#Run the interactions with party variable: Count_extreme1
#Extreme
cj_extreme_rparty_count1=cj_anova(tess_long, f1.c.1, by = ~party_respondent)

cj_extreme_rparty_count1
#Significant (p=0.003)

mms_extreme_rparty_count1=cj(tess_long, f1.c.1,
                             id = ~CaseId, estimate = "mm", by= ~party_respondent)

diff_extreme_rparty_count1=cj(tess_long, f1.c.1, 
                       id = ~CaseId, estimate = "mm_diff", by = ~party_respondent)

###
plot(mms_extreme_rparty_count1, xlab = "Extreme", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

plot(rbind(mms_extreme_rparty_count1, diff_extreme_rparty_count1), xlab = "Extreme", vline = 0) + ggplot2::facet_wrap(~BY, ncol=3L)

#Vote
cj_vote_rparty_count1=cj_anova(tess_long, f2.c.1, by = ~party_respondent)

cj_vote_rparty_count1
#Significant (0.000)

mms_vote_rparty_count1=cj(tess_long, f2.c.1,
                          id = ~CaseId, estimate = "mm", by = ~party_respondent)

diff_vote_rparty_count1=cj(tess_long, f2.c.1,
                           id = ~CaseId, estimate = "mm_diff", by = ~party_respondent)

###
plot(mms_vote_rparty_count1, xlab = "Vote", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

plot(rbind(mms_vote_rparty_count1, diff_vote_rparty_count1), xlab = "Vote", vline = 0) + ggplot2::facet_wrap(~BY, ncol = 3L)

#Speech
cj_speech_rparty_count1=cj_anova(tess_long, f3.c.1, by = ~party_respondent)

cj_speech_rparty_count1
#Significant (0.000)

mms_speech_rparty_count1=cj(tess_long, f3.c.1,
                            id = ~CaseId, estimate = "mm", by = ~party_respondent)

diff_speech_rparty_count1=cj(tess_long, f3.c.1,
                             id = ~CaseId, estimate = "mm_diff", by = ~party_respondent)

###
plot(mms_speech_rparty_count1, xlab = "Speech", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

plot(rbind(mms_speech_rparty_count1, diff_speech_rparty_count1), xlab = "Speech", vline = 0) + ggplot2::facet_wrap(~BY, ncol = 3L)

#Teach
cj_teach_rparty_count1=cj_anova(tess_long, f4.c.1, by = ~party_respondent)

cj_teach_rparty_count1
#Significant (0.000)

mms_teach_rparty_count1=cj(tess_long, f4.c.1,
                           id = ~CaseId, estimate = "mm", by = ~party_respondent)

diff_teach_rparty_count1=cj(tess_long, f4.c.1,
                            id = ~CaseId, estimate = "mm_diff", by = ~party_respondent)

###
plot(mms_teach_rparty_count1, xlab = "Teach", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

plot(rbind(mms_teach_rparty_count1, diff_teach_rparty_count1), xlab = "Teach", vline = 0) + ggplot2::facet_wrap(~BY, ncol = 3L)

#Run the interactions with party variable: Count_extreme2
#Extreme
cj_extreme_rparty_count2=cj_anova(tess_long, f1.c.2, by = ~party_respondent)

cj_extreme_rparty_count2
#Significant (0.000)

mms_extreme_rparty_count2=cj(tess_long, f1.c.2,
                             id = ~CaseId, estimate = "mm", by = ~party_respondent)

diff_extreme_rparty_count2=cj(tess_long, f1.c.2,
                              id = ~CaseId, estimate = "mm_diff", by = ~party_respondent)

###
plot(mms_extreme_rparty_count2, xlab = "Extreme", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###
plot(rbind(mms_extreme_rparty_count2, diff_extreme_rparty_count2), xlab =  "Extreme", vline = 0) + ggplot2::facet_wrap(~BY, ncol = 3L)

#Vote
cj_vote_rparty_count2=cj_anova(tess_long, f2.c.2, by = ~party_respondent)

cj_vote_rparty_count2
#Significant (0.000)

mms_vote_rparty_count2=cj(tess_long, f2.c.2,
                          id = ~CaseId, estimate = "mm", by = ~party_respondent)

diff_vote_rparty_count2=cj(tess_long, f2.c.2,
                           id = ~CaseId, estimate = "mm_diff", by = ~party_respondent)

###
plot(mms_vote_rparty_count2, xlab = "Vote", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

plot(rbind(mms_vote_rparty_count2, diff_vote_rparty_count2), xlab = "Vote", vline = 0) + ggplot2::facet_wrap(~BY, ncol = 3L)

#Speech
cj_speech_rparty_count2=cj_anova(tess_long, f3.c.2, by = ~party_respondent)

cj_speech_rparty_count2
#Significant (0.000)

mms_speech_rparty_count2=cj(tess_long, f3.c.2,
                            id = ~CaseId, estimate = "mm", by = ~party_respondent)

diff_speech_rparty_count2=cj(tess_long, f3.c.2,
                             id = ~CaseId, estimate = "mm_diff", by = ~party_respondent)

###
plot(mms_speech_rparty_count2, xlab = "Speech", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

plot(rbind(mms_speech_rparty_count2, diff_speech_rparty_count2), xlab = "Speech", vline = 0) + ggplot2::facet_wrap(~BY, ncol = 3L)

#Teach
cj_teach_rparty_count2=cj_anova(tess_long, f4.c.2, by = ~party_respondent)

cj_teach_rparty_count2
#Significant (0.000)

mms_teach_rparty_count2=cj(tess_long, f4.c.2,
                           id = ~CaseId, estimate = "mm", by = ~party_respondent)

diff_teach_rparty_count2=cj(tess_long, f4.c.2,
                            id = ~CaseId, estimate = "mm_diff", by = ~party_respondent)

###
plot(mms_teach_rparty_count2, xlab = "Teach", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

plot(rbind(mms_teach_rparty_count2, diff_teach_rparty_count2), xlab = "Teach", vline = 0) + ggplot2::facet_wrap(~BY, ncol = 3L)

#####
###Testing out graphs for condensed plots

###Describe Own
##Extreme
f1.h.1 <- extreme ~ gender + race + party_match + describe_own + party_ext + civil + norm + talk + viol + law

mms_extreme_describeown = mm(tess_long, f1.h.1, id = ~CaseId)

plot(mms_extreme_describeown, vline = 0.5, xlab = "Extreme")

##Vote
f2.h.1 <- vote ~ gender + race + party_match + describe_own + party_ext + civil + norm + talk + viol + law

mms_vote_describeown = mm(tess_long, f2.h.1, id = ~CaseId)

plot(mms_vote_describeown, vline = 0.25, xlab = "Vote")

###Describe Respondent Party
##Extreme
f1.h.2 <- extreme ~ gender + race + party_match + describe_rparty + party_ext + civil + norm + talk + viol + law

mms_extreme_describerparty = mm(tess_long, f1.h.2, id = ~CaseId)

plot(mms_extreme_describerparty, vline = 0.5, xlab = "Extreme")

##Vote
f2.h.2 <- vote ~ gender + race + party_match + describe_rparty + party_ext + civil + norm + talk + viol + law

mms_vote_describerparty = mm(tess_long, f2.h.2, id = ~CaseId)

plot(mms_vote_describerparty, vline = 0.25, xlab = "Vote")

###Demdescribe and Repdescribe
##Extreme
f1.h.3 <- extreme ~ sim_score + party_ext + demdescribe + repdescribe + civil + norm + talk + viol + law

mms_extreme_sim = mm(tess_long, f1.h.3, id = ~CaseId)

plot(mms_extreme_sim, vline = 0.5, xlab = "Extreme")

##Vote
f2.h.3 <- vote ~ sim_score + party_ext + demdescribe + repdescribe + civil + norm + talk + viol + law

mms_vote_sim = mm(tess_long, f2.h.3, id = ~CaseId)

plot(mms_vote_sim, xlab = "Vote")

###Demograpics
##Extreme
f1.h.4 <- extreme ~ sim_gender + sim_race + party_match + party_ext + demdescribe + repdescribe + civil + norm + talk + viol + law

mms_extreme_demographics = mm(tess_long, f1.h.4, id = ~CaseId)

plot(mms_extreme_demographics, vline = 0.5, xlab = "Extreme")

##Vote
f2.h.4 <- vote ~ sim_gender + sim_race + party_match + party_ext + demdescribe + repdescribe + civil + norm + talk + viol + law

mms_vote_demographics = mm(tess_long, f2.h.4, id = ~CaseId)

plot(mms_vote_demographics, vline = 0.5, xlab = "Vote")

###Doing the condensed plots
library(cowplot)
#This command plots the results and saves them as a hires image
plot(mms_extreme_sim, vline=0.50, xlab="Extreme")
plot(mms_extreme_demographics, vline = 0.50, xlab = "Extreme")
dev.off()

###Norm and viol
##Extreme
mms_small_extreme_nv = mms_extreme_demographics[mms_extreme_demographics$feature == "norm"|mms_extreme_demographics$feature == "viol",]

mms_small_extreme_nv

###
plot(mms_small_extreme_nv, vline = 0.5, xlab = "Extreme")
###

###Viol
mms_small_extreme_viol = mms_extreme_demographics[mms_extreme_demographics$feature == "viol",]

mms_small_extreme_viol

###
plot(mms_small_extreme_viol, vline = 0.5, xlab = "Extreme")
###

##Vote
mms_small_vote_nv = mms_vote_demographics[mms_vote_demographics$feature == "norm"|mms_vote_demographics$feature == "viol",]

mms_small_vote_nv

###
plot(mms_small_vote_nv, xlab = "Vote")
###

###Demdescribe and repdescribe
##Extreme
mms_small_extreme1 = mms_extreme_sim[mms_extreme_sim$feature=="demdescribe"|mms_extreme_sim$feature=="repdescribe",]

mms_small_extreme1

###
plot(mms_small_extreme1, vline = 0.50, xlab = "Extreme")
###

##Vote
mms_small_vote1 = mms_vote_sim[mms_vote_sim$feature == "demdescribe"|mms_extreme_sim$feature == "repdescribe",]

mms_small_vote1

###
plot(mms_small_vote1, xlab = "Vote")
###

###Describe_own
##Extreme
mms_small_extreme2 = mms_extreme_describeown[mms_extreme_describeown$feature == "describe_own",]

mms_small_extreme2

###
plot(mms_small_extreme2, vline = 0.50, xlab = "Extreme")
###

##Vote
mms_small_vote2 = mms_vote_describeown[mms_vote_describeown$feature == "describe_own",]

mms_small_vote2

###
plot(mms_small_vote2, xlab = "Vote")
###

###Describe_rparty
##Extreme
mms_small_extreme3 = mms_extreme_describerparty[mms_extreme_describerparty$feature == "describe_rparty",]

mms_small_extreme3

###
plot(mms_small_extreme3, vline = 0.50, xlab = "Extreme")
###

##Vote
mms_small_vote3 = mms_vote_describerparty[mms_vote_describerparty$feature == "describe_rparty",]

mms_small_vote3

###
plot(mms_small_vote3, xlab = "Vote")
###

#####
###Party Interactions

###Norm and viol
##Extreme
cj_extreme_rparty_nv = cj_anova(tess_long, f1.h.4, by = ~party_respondent)

cj_extreme_rparty_nv
#Significant (0.000)

mms_extreme_rparty_nv = cj(tess_long, f1.h.4,
                           by = ~party_respondent, id = ~CaseId, estimate = "mm")

mms_rparty_nv = mms_extreme_rparty_nv[mms_extreme_rparty_nv$feature == "norm"|mms_extreme_rparty_nv$feature == "viol",]

###
plot(mms_rparty_nv, xlab = "Extreme", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

##Vote
cj_vote_rparty_nv = cj_anova(tess_long, f2.h.4, by = ~party_respondent)

cj_vote_rparty_nv
#Significant (0.000)

mms_vote_rparty_nv = cj(tess_long, f2.h.4,
                        by = ~party_respondent, id = ~CaseId, estimate = "mm")

mms_rparty_nv_2 = mms_vote_rparty_nv[mms_vote_rparty_nv$feature == "norm"|mms_vote_rparty_nv$feature == "viol",]

###
plot(mms_rparty_nv_2, xlab = "Vote", vline = 0.25) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

###Demdescribe and repdescribe
##Extreme
cj_extreme_rparty_small1 = cj_anova(tess_long, f1.h.3, by = ~party_respondent)

cj_extreme_rparty_small1
#Significant (0.000)

mms_extreme_rparty_small1 = cj(tess_long, f1.h.3,
                               by = ~party_respondent, id = ~CaseId, estimate = "mm")

mms_rparty_small1 = mms_extreme_rparty_small1[mms_extreme_rparty_small1$feature == "demdescribe"|mms_extreme_rparty_small1$feature == "repdescribe",]

###
plot(mms_rparty_small1, xlab = "Extreme", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

##Vote
cj_vote_rparty_small1 = cj_anova(tess_long, f2.h.3, by = ~party_respondent)

cj_vote_rparty_small1
#Significant (0.000)

mms_vote_rparty_small1 = cj(tess_long, f2.h.3,
                            by = ~party_respondent, id = ~CaseId, estimate = "mm")

mms_rparty_small1_2 = mms_vote_rparty_small1[mms_vote_rparty_small1$feature == "demdescribe"|mms_vote_rparty_small1$feature == "repdescribe",]

###
plot(mms_rparty_small1_2, xlab = "Vote", vline = 0.25) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

###Describe_own
##Extreme
cj_extreme_rparty_small2 = cj_anova(tess_long, f1.h.1, by = ~party_respondent)

cj_extreme_rparty_small2
#Significant (0.000)

mms_extreme_rparty_small2 = cj(tess_long, f1.h.1,
                               by = ~party_respondent, id = ~CaseId, estimate = "mm")

mms_rparty_small2 = mms_extreme_rparty_small2[mms_extreme_rparty_small2$feature == "describe_own",]

###
plot(mms_rparty_small2, xlab = "Extreme", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

##Vote
cj_vote_rparty_small2 = cj_anova(tess_long, f2.h.1, by = ~party_respondent)

cj_vote_rparty_small2
#Significant (0.000)

mms_vote_rparty_small2 = cj(tess_long, f2.h.1,
                            by = ~party_respondent, id = ~CaseId, estimate = "mm")

mms_rparty_small2_2 = mms_vote_rparty_small2[mms_vote_rparty_small2$feature == "describe_own",]

###
plot(mms_rparty_small2_2, vline = 0.25, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 3L)
###

#####
###Dropping independents here so that describe_rparty will work
tess_noind=tess_long[tess_long$party_respondent!="Independent",]
tess_noind=tess_noind[!is.na(tess_noind$CaseId),]
table(tess_noind$describe_rparty)
tess_noind$describe_rparty=factor(tess_noind$describe_rparty,
                                  levels=c("R party calls extreme", "R party calls moderate",
                                           "R party calls typical", "R party calls unusual"))
tess_noind$party_respondent=factor(tess_noind$party_respondent,
                                   levels=c("Democrat", "Republican"))

###Describe_rparty
##Extreme
cj_extreme_rparty_small3 = cj_anova(tess_noind, f1.h.2, by = ~party_respondent)

cj_extreme_rparty_small3
#Significant (0.000)

mms_extreme_rparty_small3 = cj(tess_noind, f1.h.2,
                               by = ~party_respondent, id = ~CaseId, estimate = "mm")

mms_rparty_small3 = mms_extreme_rparty_small3[mms_extreme_rparty_small3$feature == "describe_rparty",]

###
plot(mms_rparty_small3, xlab = "Extreme", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)
###

##Vote
cj_vote_rparty_small3 = cj_anova(tess_noind, f2.h.2, by = ~party_respondent)

cj_vote_rparty_small3
#Significant (0.000)

mms_vote_rparty_small3_2 = cj(tess_noind, f2.h.2,
                              by = ~party_respondent, id = ~CaseId, estimate = "mm")

###
mms_rparty_small3_2 = mms_vote_rparty_small3_2[mms_vote_rparty_small3_2$feature == "describe_rparty",]

###
plot(mms_rparty_small3_2, xlab = "Vote", vline = 0.25) + ggplot2::facet_wrap(~BY, ncol = 3L)







#From here, you could run the same commands on the Academic sample. Start by doing that

#Once you feel good about that let Dr. Busby know and we'll go from there

####Subset of attributes graphing example####
mms_extreme_2=cj(tess_long, extreme ~ norm + viol + civil + demdescribe + repdescribe, 
                 id = ~CaseId, estimate = "mm")

#Full image
plot(mms_extreme_2, vline=0.50, xlab="Extreme")

mms_vote_2=cj(tess_long, extreme ~ norm + viol + civil + demdescribe + repdescribe,
              id = ~CaseId, estimate = "mm")

#Full image
plot(mms_vote_2, vline = 0.5, xlab = "Vote")

mms_teach_2=cj(tess_long, extreme ~ norm + viol + civil + demdescribe + repdescribe,
               id = ~CaseId, estimate = "mm")

#Full image
plot(mms_teach_2, vline = 0.5, xlab = "Teach")

mms_speech_2=cj(tess_long, extreme ~ norm + viol + civil + demdescribe + repdescribe,
                id = ~CaseId, estimate = "mm")

#Full image
plot(mms_speech_2, vline = 0.5, xlab = "Speech")

#Let's say we want to Zoom in on the violence, compromise, and rule of law attributes only
#We could re-run the analysis with only those things as attributes, but it would be
#better to use the more complete model and then only graph the attributes we want to look at

#Because the conjoint analysis creates a data object of results - the "mms_extreme_2" object -
#we can selectively plot only part of that object.

#To start, just examine the object itself like this:
mms_extreme_2

#Let's make a smaller version of that object that only includes the attributes we want to plot
mms_small_extreme = mms_extreme_2[mms_extreme_2$feature=="civil"|mms_extreme_2$feature=="viol"|mms_extreme_2$feature=="norm",]
#Look at it to make sure it worked
mms_small_extreme

#Now we can just plot that object
plot(mms_small_extreme, vline=0.50, xlab="Extreme")

#Alternatively, we could do a messier command like this where we don't have to make a new data object;
#instead, we just use the bracketed conditional in the graphing command:
plot(mms_extreme_2[mms_extreme_2$feature=="civil"|mms_extreme_2$feature=="viol"|mms_extreme_2$feature=="norm",], vline=0.50, xlab="Extreme")

#Creates the same graph!

####Interaction graphing example####
#sometimes we run an interaction in a conjoint and want to plot only parts of the 
#results - like only graphing the difference or only for one subgroup

#Start with this interaction, which evaluates the idea that people with higher levels of 
#self-rated extremism may react to the attributes differently:
mms_extreme_int=cj(tess_long, extreme ~ viol + civil + law + talk + race + gender + demdescribe + repdescribe,
                   id = ~CaseId, estimate = "mm", by=~party_respondent)

plot(mms_extreme_int, group="party_respondent")
#This is a bit messy, showing the estimates for all of the different groups all at once. You
#can plot this a little cleaner with a command like this that uses the {cowplot} package.
#This uses the same subsetting logic as the commands above to choose different levels of
#the moderating/interaction variable (referring to the "BY" column). The xlim option
#makes the scale of the X axis the same for the graphs):
p1=plot(mms_extreme_int[mms_extreme_int$BY=="Independent",], xlim=c(0.200, 0.900))
p2=plot(mms_extreme_int[mms_extreme_int$BY=="Democrat",], xlim=c(0.200, 0.900))

library(cowplot)
combined_plot=plot_grid(p1+theme(legend.position="none")+scale_color_manual(values=rep("black", 13)),
                        p2+ theme(legend.position="none")+scale_color_manual(values=rep("black", 13)),
                        labels=c("Independent", "Democrat"),
                        label_size = 12, label_y=0.05)
combined_plot

#This can be useful, but doesn't directly show the differences between the groups.
#If we want that, we should redo the conjoint model like this:

mms_extreme_int=cj(tess_long, extreme ~ viol + civil + law + talk + race + gender + demdescribe + repdescribe,
                   id = ~CaseId, estimate = "mm_diff", by=~party_respondent)

#This graph is basically meaningless:
plot(mms_extreme_int)

#This version is more helpful:
plot(mms_extreme_int)+ggplot2::facet_wrap(~BY, ncol=3L)

#This is more helpful - it shows the differences between the categories. Two things:
#We may want different comparisons than it does by default - by default, it 
#compares the lowest level of the variable to all of the others. To change this,
#we would just need to reorder the levels of the interaction variable with a command 
#something like this:

#Then if we ran the conjoint, the comparison points would change

#Second, we may want to graph just one or two of these. We can do this like this:
p1=plot(mms_extreme_int[mms_extreme_int$BY=="Republican - Democrat",], xlim=c(0, 0.5))
p2=plot(mms_extreme_int[mms_extreme_int$BY=="Independent - Democrat",], xlim=c(0, 0.5))

library(cowplot)
combined_plot=plot_grid(p1+theme(legend.position="none")+scale_color_manual(values=rep("black", 13)),
                        p2+ theme(legend.position="none")+scale_color_manual(values=rep("black", 13)),
                        labels=c("Republican - Democrat", "Independent - Democrat"),
                        label_size = 12, label_x=0.17)
combined_plot
#These plots show the difference in how each of the groups responded to each level
#of the attributes. A score of 0 means that there was no difference (the two groups
#responded to the attribute level in the same way). Higher levels mean that the two groups
#responded differently to that attribute level.