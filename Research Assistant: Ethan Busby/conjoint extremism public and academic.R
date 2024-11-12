#####2022 Conjoint Analysis (Plots)#####
#Load in the packages and data
library(cregg)
library(ggplot2)
library(dplyr)
library(stringr)
library(car)


load("Cleaned 2022 Extremism Survey.RData")

#####Recoding Conjoint_PS22#####
#Remove non-participants
conjoint_PS22 <- subset(conjoint_PS22, !Consent == "I DO NOT agree to participate in this study")

#Any variable in this command must be a factor variable. So let's do some recoding to set that up
table(conjoint_PS22$self_extr_recoded)
conjoint_PS22$self_extr_recoded_2 <- car::recode(conjoint_PS22$self_extr_recoded, '
                                         "0"="Not at all extreme";
                                         "0.25"="A little extreme";
                                         "0.5"="Somewhat extreme";
                                         "0.75"="Extreme";
                                         "1"="Very extreme" ')

conjoint_PS22$self_extr_recoded_2=factor(conjoint_PS22$self_extr_recoded_2, 
                                         levels=c("Not at all extreme", "A little extreme",
                                                  "Somewhat extreme", "Extreme",
                                                  "Very extreme"))

conjoint_PS22$gender_2[conjoint_PS22$gender_recoded==0]="Man"
conjoint_PS22$gender_2[conjoint_PS22$gender_recoded==1]="Woman"
conjoint_PS22$gender_2=as.factor(conjoint_PS22$gender_2)

#Ideological extremity
conjoint_PS22$ideo_ext[conjoint_PS22$ideo7_recoded==0.5]="Moderate/non-ideological"
conjoint_PS22$ideo_ext[conjoint_PS22$ideo7_recoded==(1/3)|conjoint_PS22$ideo7_recoded==(2/3)]="Somewhat ideological"
conjoint_PS22$ideo_ext[conjoint_PS22$ideo7_recoded==(1/6)|conjoint_PS22$ideo7_recoded==(5/6)]="Ideological"
conjoint_PS22$ideo_ext[conjoint_PS22$ideo7_recoded==0|conjoint_PS22$ideo7_recoded==1]="Very ideological"

conjoint_PS22$ideo_ext=factor(conjoint_PS22$ideo_ext, 
                                         levels=c("Moderate/non-ideological", "Somewhat ideological",
                                                  "Ideological", "Very ideological"))

#Ideology
conjoint_PS22$ideo_group[conjoint_PS22$ideo7_recoded<0.5]="Liberal"
conjoint_PS22$ideo_group[conjoint_PS22$ideo7_recoded==0.5]="Moderate"
conjoint_PS22$ideo_group[conjoint_PS22$ideo7_recoded>0.5]="Conservative"

conjoint_PS22$ideo_group=factor(conjoint_PS22$ideo_group, 
                              levels=c("Liberal", "Moderate","Conservative"))

#Partisan extremity
conjoint_PS22$part_ext[conjoint_PS22$pid_recoded==0.5]="Indep."
conjoint_PS22$part_ext[conjoint_PS22$pid_recoded==(1/3)|conjoint_PS22$pid_recoded==(2/3)]="Leaner"
conjoint_PS22$part_ext[conjoint_PS22$pid_recoded==(1/6)|conjoint_PS22$pid_recoded==(5/6)]="Weak partisan"
conjoint_PS22$part_ext[conjoint_PS22$pid_recoded==1|conjoint_PS22$pid_recoded==0]="Strong partisan"

conjoint_PS22$part_ext=factor(conjoint_PS22$part_ext, 
                              levels=c("Indep.", "Leaner",
                                       "Weak partisan", "Strong partisan"))
#Partisanship
conjoint_PS22$part_group[conjoint_PS22$pid_recoded<0.5]="Dem"
conjoint_PS22$part_group[conjoint_PS22$pid_recoded==0.5]="Ind"
conjoint_PS22$part_group[conjoint_PS22$pid_recoded>0.5]="Rep"

conjoint_PS22$part_group=factor(conjoint_PS22$part_group, 
                              levels=c("Dem", "Ind",
                                      "Rep"))

#Race
conjoint_PS22$race[conjoint_PS22$race.ethnicity == "Hispanic/Latino(a)"] = "Hispanic"
conjoint_PS22$race[conjoint_PS22$race.ethnicity == "White"] = "White"
conjoint_PS22$race[conjoint_PS22$race.ethnicity == "Black or African American"] = "Black"
conjoint_PS22$race[conjoint_PS22$race.ethnicity == "Asian or Asian American"] = "Asian"
conjoint_PS22$race[str_detect(conjoint_PS22$race.ethnicity, ",")] = "Other"
conjoint_PS22$race[conjoint_PS22$race.ethnicity == "Other"] = "Other"
conjoint_PS22$race[conjoint_PS22$race.ethnicity == "American Indian or Alaska Native"] = "Other"
conjoint_PS22$race[conjoint_PS22$race.ethnicity == "Hawaiian or Pacific Islander"] = "Other"
conjoint_PS22$race[conjoint_PS22$race.ethnicity == "Middle Eastern or North African"] = "Other"
conjoint_PS22$race=as.factor(conjoint_PS22$race)


#Interest in politics
conjoint_PS22$interest_2[conjoint_PS22$interest_recoded==0]="Not at all interested"
conjoint_PS22$interest_2[conjoint_PS22$interest_recoded==0.25]="Not very interested"
conjoint_PS22$interest_2[conjoint_PS22$interest_recoded==0.5]="Somewhat interested"
conjoint_PS22$interest_2[conjoint_PS22$interest_recoded==0.75]="Interested"
conjoint_PS22$interest_2[conjoint_PS22$interest_recoded==1]="Very interested"

conjoint_PS22$interest_2=factor(conjoint_PS22$interest_2, 
                                levels=c("Very interested", "Interested", "Somewhat interested",
                                         "Not very interested", "Not at all interested"))

#####Extremism Count Variable Creation (Conjoint_PS22)#####
table(conjoint_PS22$civil_b)
conjoint_PS22$uncivil[conjoint_PS22$civil_b==1]=0
conjoint_PS22$uncivil[conjoint_PS22$civil_b==0]=1
table(conjoint_PS22$uncivil)

table(conjoint_PS22$violence_b)
conjoint_PS22$violence_support[conjoint_PS22$violence_b==0]=0
conjoint_PS22$violence_support[conjoint_PS22$violence_b==1]=1
table(conjoint_PS22$violence_support)

table(conjoint_PS22$compromise_b)
conjoint_PS22$compromise_opp[conjoint_PS22$compromise_b==1]=0
conjoint_PS22$compromise_opp[conjoint_PS22$compromise_b==0]=1
table(conjoint_PS22$compromise_opp)

table(conjoint_PS22$law_b)
conjoint_PS22$law_oppose[conjoint_PS22$law_b==1]=0
conjoint_PS22$law_oppose[conjoint_PS22$law_b==0]=1
table(conjoint_PS22$law_oppose)

table(conjoint_PS22$talk_always)

table(conjoint_PS22$party_farleftdem)

table(conjoint_PS22$party_farrightrep)

#Abortion, gun regulation, business
table(conjoint_PS22$abortion_prof)
table(conjoint_PS22$guns_prof)
table(conjoint_PS22$business_prof)

conjoint_PS22$abortion_prof <- car::recode(conjoint_PS22$abortion_prof, '
                                           "Strongly opposes" = "Strongly opposes restricting access to abortion";
                                           "Opposes" = "Opposes restricting access to abortion";
                                           "Undecided" = "Undecided about restricting access to abortion";
                                           "Supports" = "Supports restricting access to abortion";
                                           "Strongly supports" = "Strongly supports restricting access to abortion"
                                           ', levels = c("Strongly opposes restricting access to abortion", "Opposes restricting access to abortion", "Undecided about restricting access to abortion", "Supports restricting access to abortion", "Strongly supports restricting access to abortion"))

conjoint_PS22$guns_prof <- car::recode(conjoint_PS22$guns_prof, '
                                       "Strongly opposes" = "Strongly opposes increasing gun regulation";
                                       "Opposes" = "Opposes increasing gun regulation";
                                       "Undecided" = "Undecided about increasing gun regulation";
                                       "Supports" = "Supports increasing gun regulation";
                                       "Strongly supports" = "Strongly supports increasing gun regulation"
                                       ', levels = c("Strongly opposes increasing gun regulation", "Opposes increasing gun regulation", "Undecided about increasing gun regulation", "Supports increasing gun regulation", "Strongly supports increasing gun regulation"))

conjoint_PS22$business_prof <- car::recode(conjoint_PS22$business_prof, '
                                           "Strongly opposes" = "Strongly opposes reducing regulations on businesses";
                                           "Opposes" = "Opposes reducing regulations on businesses";
                                           "Undecided" = "Undecided about reducing regulations on businesses";
                                           "Supports" = "Supports reducing regulations on businesses";
                                           "Strongly supports" = "Strongly supports reducing regulations on businesses"
                                           ', levels = c("Strongly opposes reducing regulations on businesses", "Opposes reducing regulations on businesses", "Undecided about reducing regulations on businesses", "Supports reducing regulations on businesses", "Strongly supports reducing regulations on businesses"))

conjoint_PS22$abortion_prof_count=0
conjoint_PS22$abortion_prof_count[conjoint_PS22$abortion_prof=="Strongly opposes restricting access to abortion"]=1
conjoint_PS22$abortion_prof_count[conjoint_PS22$abortion_prof=="Strongly supports restricting access to abortion"]=1
table(conjoint_PS22$abortion_prof_count)

conjoint_PS22$guns_prof_count=0
conjoint_PS22$guns_prof_count[conjoint_PS22$guns_prof=="Strongly opposes increasing gun regulation"]=1
conjoint_PS22$guns_prof_count[conjoint_PS22$guns_prof=="Strongly supports increasing gun regulation"]=1
table(conjoint_PS22$guns_prof_count)

conjoint_PS22$business_prof_count=0
conjoint_PS22$business_prof_count[conjoint_PS22$business_prof=="Strongly opposes reducing regulations on businesses"]=1
conjoint_PS22$business_prof_count[conjoint_PS22$business_prof=="Strongly supports reducing regulations on businesses"]=1
table(conjoint_PS22$business_prof_count)

#Adding extremist traits together
conjoint_PS22$count = conjoint_PS22$uncivil + conjoint_PS22$violence_support + 
  conjoint_PS22$compromise_opp + conjoint_PS22$law_oppose + conjoint_PS22$talk_always +
  conjoint_PS22$party_farleftdem + conjoint_PS22$party_farrightrep + conjoint_PS22$abortion_prof_count +
  conjoint_PS22$guns_prof_count + conjoint_PS22$business_prof_count

table(conjoint_PS22$count)

conjoint_PS22$count=as.factor(conjoint_PS22$count)

table(conjoint_PS22$count)
conjoint_PS22$count[conjoint_PS22$count==0]=1
conjoint_PS22$count[conjoint_PS22$count==9]=8
table(conjoint_PS22$count)

conjoint_PS22$count=as.factor(conjoint_PS22$count)


mms_extreme=cj(conjoint_PS22, extreme_R ~ violence + compromise + civil + law +
                 talk + party + race_prof + gender_prof+self_extr_recoded_2+gender_2+
                 ideo_ext + ideo_group + part_ext + part_group + race + interest_2, 
               id = ~ResponseId, estimate = "mm")

#This command plots the results and saves them as a hires image
jpeg("Example conjoint figure.jpeg", width=7, height=10, units="in", res=600)
plot(mms_extreme, vline=0.50, xlab="Extreme")
dev.off()

jpeg("Example conjoint figure.jpeg", width=7, height=10, units="in", res=600)
plot(mms_extreme, vline=0.50, xlab="Extreme")
dev.off()

#graph with only model attributes and no respondent characteristics
mms_extreme_2=cj(conjoint_PS22, extreme_R ~ violence + compromise + civil + law +
                 talk + party + race_prof + gender_prof, 
               id = ~ResponseId, estimate = "mm")

jpeg("Example conjoint figure_2.jpeg", width=7, height=10, units="in", res=600)
plot(mms_extreme_2, vline=0.50, xlab="Extreme")
dev.off()

#Interaction model
table(conjoint_PS22$part_group)

mms_extreme_int=cj(conjoint_PS22, extreme_R ~ violence + compromise + civil + law +
                     talk + party + race_prof + gender_prof,
                   id = ~ResponseId, estimate = "mm", by= ~part_group)

jpeg("Interaction conjoint figure.jpeg", width=7, height=10, units="in", res=600)
plot(mms_extreme_int, vline=0.50, xlab="Extreme") + ggplot2::facet_wrap(~BY, ncol = 3L)
dev.off()

#Conjoint with only count variable
mms_extreme_public_count=cj(conjoint_PS22, extreme_R ~ count, 
                              id = ~ResponseId, estimate = "mm")

jpeg("Example public conjoint figure count.jpeg", width=7, height=10, units="in", res=600)
plot(mms_extreme_public_count, vline=0.50, xlab="Extreme")
dev.off()


#####Modified Conjoint Graphs#####
##Subset of attributes graphing
mms_extreme_2_public=cj(conjoint_PS22, extreme_R ~ violence + compromise + civil + law +
                   talk + party + race_prof + gender_prof, 
                 id = ~ResponseId, estimate = "mm")

#Full image
plot(mms_extreme_2_public, vline=0.50, xlab="Extreme")


####Violence
mms_extreme_norm_public = mms_extreme_2_public[mms_extreme_2_public$feature == "violence",]

plot(mms_extreme_norm_public, vline = 0.5, xlab = "Extreme")

mms_extreme_int=cj(conjoint_PS22, extreme_R ~ violence + compromise + civil + law +
                     talk + party + race_prof + gender_prof, 
                   id = ~ResponseId, estimate = "mm", by=~self_extr_recoded_2)

plot(mms_extreme_int, group="self_extr_recoded_2")


#####Party Match Variable Creation#####
table(conjoint_PS22$party, conjoint_PS22$part_group)

conjoint_PS22$party_match=NA

conjoint_PS22$party_match[conjoint_PS22$party == "Far-left Democrat" & conjoint_PS22$part_group == "Rep"] = 0
conjoint_PS22$party_match[conjoint_PS22$party == "Democrat" & conjoint_PS22$part_group == "Rep"] = 0
conjoint_PS22$party_match[conjoint_PS22$party == "Moderate Democrat" & conjoint_PS22$part_group == "Rep"] = 0

conjoint_PS22$party_match[conjoint_PS22$party == "Far-left Democrat" & conjoint_PS22$part_group == "Ind"] = 0
conjoint_PS22$party_match[conjoint_PS22$party == "Democrat" & conjoint_PS22$part_group == "Ind"] = 0
conjoint_PS22$party_match[conjoint_PS22$party == "Moderate Democrat" & conjoint_PS22$part_group == "Ind"] = 0

conjoint_PS22$party_match[conjoint_PS22$party == "Far-right Republican" & conjoint_PS22$part_group == "Dem"] = 0
conjoint_PS22$party_match[conjoint_PS22$party == "Republican" & conjoint_PS22$part_group == "Dem"] = 0

conjoint_PS22$party_match[conjoint_PS22$party == "Far-right Republican" & conjoint_PS22$part_group == "Ind"] = 0
conjoint_PS22$party_match[conjoint_PS22$party == "Republican" & conjoint_PS22$part_group == "Ind"] = 0

conjoint_PS22$party_match[conjoint_PS22$party == "Independent" & conjoint_PS22$part_group == "Rep"] = 0
conjoint_PS22$party_match[conjoint_PS22$party == "Independent" & conjoint_PS22$part_group == "Dem"] = 0

conjoint_PS22$party_match[conjoint_PS22$party == "Far-left Democrat" & conjoint_PS22$part_group == "Dem"] = 1
conjoint_PS22$party_match[conjoint_PS22$party == "Democrat" & conjoint_PS22$part_group == "Dem"] = 1
conjoint_PS22$party_match[conjoint_PS22$party == "Moderate Democrat" & conjoint_PS22$part_group == "Dem"] = 1

conjoint_PS22$party_match[conjoint_PS22$party == "Republican" & conjoint_PS22$part_group == "Rep"] = 1
conjoint_PS22$party_match[conjoint_PS22$party == "Far-right Republican" & conjoint_PS22$part_group == "Rep"] = 1

conjoint_PS22$party_match[conjoint_PS22$party == "Independent" & conjoint_PS22$part_group == "Ind"] = 1

conjoint_PS22$party_match = as.factor(conjoint_PS22$party_match)

table(conjoint_PS22$party_match)


#####Interactions Using Party Match Variable#####
conjoint_PS22$party_match=as.factor(conjoint_PS22$party_match)
mms_extreme_int_2=cj(conjoint_PS22, extreme_R ~ violence + compromise + civil + law +
                     talk + party + race_prof + gender_prof,
                   id = ~ResponseId, estimate = "mm", by= ~party_match)


jpeg("Interaction conjoint figure party match.jpeg", width=7, height=10, units="in", res=600)
plot(mms_extreme_int_2, vline=0.50, xlab="Extreme") + ggplot2::facet_wrap(~BY, ncol = 3L)
dev.off()

#####Similarity score: conjoint_PS22#####
#Gender
conjoint_PS22$sim_gender[conjoint_PS22$gender_prof == "Female" & conjoint_PS22$gender == "Female"] = "Gender match"
conjoint_PS22$sim_gender[conjoint_PS22$gender_prof == "Male" & conjoint_PS22$gender == "Male"] = "Gender match"
conjoint_PS22$sim_gender[conjoint_PS22$gender_prof == "Non-binary" & conjoint_PS22$gender == "Non-binary"] = "Gender match"
conjoint_PS22$sim_gender[conjoint_PS22$gender_prof == "Female" & conjoint_PS22$gender == "Male"] = "Gender mismatch"
conjoint_PS22$sim_gender[conjoint_PS22$gender_prof == "Female" & conjoint_PS22$gender == "Non-binary"] = "Gender mismatch"
conjoint_PS22$sim_gender[conjoint_PS22$gender_prof == "Male" & conjoint_PS22$gender == "Female"] = "Gender mismatch"
conjoint_PS22$sim_gender[conjoint_PS22$gender_prof == "Male" & conjoint_PS22$gender == "Non-binary"] = "Gender mismatch"
conjoint_PS22$sim_gender[conjoint_PS22$gender_prof == "Non-binary" & conjoint_PS22$gender == "Female"] = "Gender mismatch"
conjoint_PS22$sim_gender[conjoint_PS22$gender_prof == "Non-binary" & conjoint_PS22$gender == "Male"] = "Gender mismatch"

#Race
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "Hispanic American" & conjoint_PS22$race == "Hispanic"] = "Race match"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "Black American" & conjoint_PS22$race == "Black"] = "Race match"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "Asian American" & conjoint_PS22$race == "Asian"] = "Race match"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "White American" & conjoint_PS22$race == "White"] = "Race match"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "Hispanic American" & conjoint_PS22$race == "Asian"] = "Race mismatch"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "Hispanic American" & conjoint_PS22$race == "Black"] = "Race mismatch"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "Hispanic American" & conjoint_PS22$race == "White"] = "Race mismatch"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "Black American" & conjoint_PS22$race == "Asian"] = "Race mismatch"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "Black American" & conjoint_PS22$race == "Hispanic"] = "Race mismatch"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "Black American" & conjoint_PS22$race == "White"] = "Race mismatch"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "Asian American" & conjoint_PS22$race == "Black"] = "Race mismatch"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "Asian American" & conjoint_PS22$race == "Hispanic"] = "Race mismatch"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "Asian American" & conjoint_PS22$race == "White"] = "Race mismatch"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "White American" & conjoint_PS22$race == "Asian"] = "Race mismatch"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "White American" & conjoint_PS22$race == "Black"] = "Race mismatch"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "White American" & conjoint_PS22$race == "Hispanic"] = "Race mismatch"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "Hispanic American" & conjoint_PS22$race == "Other"] = "Race mismatch"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "Black American" & conjoint_PS22$race == "Other"] = "Race mismatch"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "Asian American" & conjoint_PS22$race == "Other"] = "Race mismatch"
conjoint_PS22$sim_race[conjoint_PS22$race_prof == "White American" & conjoint_PS22$race == "Other"] = "Race mismatch"

#Party has already been coded

#Sim_score variable
conjoint_PS22$sim_score[conjoint_PS22$sim_gender == "Gender mismatch" & conjoint_PS22$sim_race == "Race mismatch" & conjoint_PS22$party_match == 0] = "0 traits shared"

conjoint_PS22$sim_score[conjoint_PS22$sim_gender == "Gender mismatch" & conjoint_PS22$sim_race == "Race mismatch" & conjoint_PS22$party_match == 1] = "1 trait shared"
conjoint_PS22$sim_score[conjoint_PS22$sim_gender == "Gender mismatch" & conjoint_PS22$sim_race == "Race match" & conjoint_PS22$party_match == 0] = "1 trait shared"
conjoint_PS22$sim_score[conjoint_PS22$sim_gender == "Gender match" & conjoint_PS22$sim_race == "Race mismatch" & conjoint_PS22$party_match == 0] = "1 trait shared"

conjoint_PS22$sim_score[conjoint_PS22$sim_gender == "Gender mismatch" & conjoint_PS22$sim_race == "Race match" & conjoint_PS22$party_match == 1] = "2 traits shared"
conjoint_PS22$sim_score[conjoint_PS22$sim_gender == "Gender match" & conjoint_PS22$sim_race == "Race match" & conjoint_PS22$party_match == 0] = "2 traits shared"
conjoint_PS22$sim_score[conjoint_PS22$sim_gender == "Gender match" & conjoint_PS22$sim_race == "Race mismatch" & conjoint_PS22$party_match == 1] = "2 traits shared"

conjoint_PS22$sim_score[conjoint_PS22$sim_gender == "Gender match" & conjoint_PS22$sim_race == "Race match" & conjoint_PS22$party_match == 1] = "3 traits shared"

conjoint_PS22$sim_gender = as.factor(conjoint_PS22$sim_gender)
conjoint_PS22$sim_race = as.factor(conjoint_PS22$sim_race)
conjoint_PS22$sim_score = as.factor(conjoint_PS22$sim_score)

conjoint_PS22$party_match <- car::recode(conjoint_PS22$party_match, '
                                         "0" = "Doesnt match";
                                         "1" = "Match" ')

#Sim_score plots

mms_simscore_extreme_public = cj(conjoint_PS22, extreme_R ~ sim_score + violence + compromise + civil + talk, id = ~ResponseId, estimate = "mm")

plot(mms_simscore_extreme_public, vline = 0.5, xlab = "Extreme")

mms_sim_extreme_public = cj(conjoint_PS22, extreme_R ~ sim_gender + sim_race + party_match + violence + compromise + civil + talk, id = ~ResponseId, estimate = "mm")

plot(mms_sim_extreme_public, vline = 0.5, xlab = "Extreme")

save(conjoint_PS22, file = "Public Sample 2022 Analysis Data.RData")


#####Recoding Conjoint_AS22#####
#Removing non-participants
conjoint_AS22 <- subset(conjoint_AS22, !Consent == "I DO NOT agree to participate in this study")

#Any variable in this command must be a factor variable. So let's do some recoding to set that up
table(conjoint_AS22$self_extr_recoded)
conjoint_AS22$self_extr_recoded_2=recode(conjoint_AS22$self_extr_recoded,
                                    "0"="Not at all extreme",
                                    "0.25"="A little extreme",
                                    "0.5"="Somewhat extreme",
                                    "0.75"="Extreme",
                                    "1"="Very extreme")
conjoint_AS22$self_extr_recoded_2=factor(conjoint_AS22$self_extr_recoded_2, 
                                    levels=c("Not at all extreme", "A little extreme",
                                             "Somewhat extreme", "Extreme",
                                             "Very extreme"))
table(conjoint_AS22$self_extr_recoded_2)

#gender
table(conjoint_AS22$gender_recoded)

conjoint_AS22$gender_2[conjoint_AS22$gender_recoded==0]="Man"
conjoint_AS22$gender_2[conjoint_AS22$gender_recoded==1]="Woman"
conjoint_AS22$gender_2=as.factor(conjoint_AS22$gender_2)
table(conjoint_AS22$gender_2)

#Ideological extremity
table(conjoint_AS22$ideo7_recoded)
conjoint_AS22$ideo_ext[conjoint_AS22$ideo7_recoded==0.5]="Moderate/non-ideological"
conjoint_AS22$ideo_ext[conjoint_AS22$ideo7_recoded==(1/3)|conjoint_AS22$ideo7_recoded==(2/3)]="Somewhat ideological"
conjoint_AS22$ideo_ext[conjoint_AS22$ideo7_recoded==(1/6)|conjoint_AS22$ideo7_recoded==(5/6)]="Ideological"
conjoint_AS22$ideo_ext[conjoint_AS22$ideo7_recoded==0|conjoint_AS22$ideo7_recoded==1]="Very ideological"

conjoint_AS22$ideo_ext=factor(conjoint_AS22$ideo_ext, 
                         levels=c("Moderate/non-ideological", "Somewhat ideological",
                                  "Ideological", "Very ideological"))
table(conjoint_AS22$ideo_ext)

#Ideology
table(conjoint_AS22$ideo7_recoded)
conjoint_AS22$ideo_group[conjoint_AS22$ideo7_recoded<0.5]="Liberal"
conjoint_AS22$ideo_group[conjoint_AS22$ideo7_recoded==0.5]="Moderate"
conjoint_AS22$ideo_group[conjoint_AS22$ideo7_recoded>0.5]="Conservative"

conjoint_AS22$ideo_group=factor(conjoint_AS22$ideo_group, 
                           levels=c("Liberal", "Moderate","Conservative"))
table(conjoint_AS22$ideo_group)

#Partisan extremity
table(conjoint_AS22$pid_recoded)
conjoint_AS22$part_ext[conjoint_AS22$pid_recoded==0.5]="Indep."
conjoint_AS22$part_ext[conjoint_AS22$pid_recoded==(1/3)|conjoint_AS22$pid_recoded==(2/3)]="Leaner"
conjoint_AS22$part_ext[conjoint_AS22$pid_recoded==(1/6)|conjoint_AS22$pid_recoded==(5/6)]="Weak partisan"
conjoint_AS22$part_ext[conjoint_AS22$pid_recoded==1|conjoint_AS22$pid_recoded==0]="Strong partisan"

conjoint_AS22$part_ext=factor(conjoint_AS22$part_ext, 
                         levels=c("Indep.", "Leaner",
                                  "Weak partisan", "Strong partisan"))
table(conjoint_AS22$part_ext)


#Partisanship
table(conjoint_AS22$pid_recoded)
conjoint_AS22$part_group[conjoint_AS22$pid_recoded<0.5]="Dem"
conjoint_AS22$part_group[conjoint_AS22$pid_recoded==0.5]="Ind"
conjoint_AS22$part_group[conjoint_AS22$pid_recoded>0.5]="Rep"

conjoint_AS22$part_group=factor(conjoint_AS22$part_group, 
                           levels=c("Dem", "Ind",
                                    "Rep"))
table(conjoint_AS22$part_group)

#Race
conjoint_AS22$race[conjoint_AS22$race.ethnicity == "Hispanic/Latino(a)"] = "Hispanic"
conjoint_AS22$race[conjoint_AS22$race.ethnicity == "White"] = "White"
conjoint_AS22$race[conjoint_AS22$race.ethnicity == "Black or African American"] = "Black"
conjoint_AS22$race[conjoint_AS22$race.ethnicity == "Asian or Asian American"] = "Asian"
conjoint_AS22$race[str_detect(conjoint_AS22$race.ethnicity, ",")] = "Other"
conjoint_AS22$race[conjoint_AS22$race.ethnicity == "Other"] = "Other"
conjoint_AS22$race[conjoint_AS22$race.ethnicity == "American Indian or Alaska Native"] = "Other"
conjoint_AS22$race[conjoint_AS22$race.ethnicity == "Hawaiian or Pacific Islander"] = "Other"
conjoint_AS22$race[conjoint_AS22$race.ethnicity == "Middle Eastern or North African"] = "Other"
conjoint_AS22$race=as.factor(conjoint_AS22$race)
table(conjoint_AS22$race)

#Interest in politics
table(conjoint_AS22$interest_recoded)
conjoint_AS22$interest_2[conjoint_AS22$interest_recoded==0]="Not at all interested"
conjoint_AS22$interest_2[conjoint_AS22$interest_recoded==0.25]="Not very interested"
conjoint_AS22$interest_2[conjoint_AS22$interest_recoded==0.5]="Somewhat interested"
conjoint_AS22$interest_2[conjoint_AS22$interest_recoded==0.75]="Interested"
conjoint_AS22$interest_2[conjoint_AS22$interest_recoded==1]="Very interested"

conjoint_AS22$interest_2=factor(conjoint_AS22$interest_2, 
                           levels=c("Very interested", "Interested", "Somewhat interested",
                                    "Not very interested", "Not at all interested"))
table(conjoint_AS22$interest_2)


#position
table(conjoint_AS22$position)
table(conjoint_AS22$position_recoded)

#department
table(conjoint_AS22$department)

#instiution
table(conjoint_AS22$institution)

#focus
table(conjoint_AS22$focus)

table(conjoint_AS22$party)
table(conjoint_AS22$part_group)


#####Extremism Count Variable Creation (Conjoint_AS22)#####
table(conjoint_AS22$civil_b)
conjoint_AS22$uncivil[conjoint_AS22$civil_b==1]=0
conjoint_AS22$uncivil[conjoint_AS22$civil_b==0]=1
table(conjoint_AS22$uncivil)

table(conjoint_AS22$violence_b)
conjoint_AS22$violence_support[conjoint_AS22$violence_b==0]=0
conjoint_AS22$violence_support[conjoint_AS22$violence_b==1]=1
table(conjoint_AS22$violence_support)

table(conjoint_AS22$compromise_b)
conjoint_AS22$compromise_opp[conjoint_AS22$compromise_b==1]=0
conjoint_AS22$compromise_opp[conjoint_AS22$compromise_b==0]=1
table(conjoint_AS22$compromise_opp)

table(conjoint_AS22$law_b)
conjoint_AS22$law_oppose[conjoint_AS22$law_b==1]=0
conjoint_AS22$law_oppose[conjoint_AS22$law_b==0]=1
table(conjoint_AS22$law_oppose)

table(conjoint_AS22$talk_always)

table(conjoint_AS22$party_farleftdem)

table(conjoint_AS22$party_farrightrep)

#abortion, gun regulation, business
table(conjoint_AS22$abortion_prof)
table(conjoint_AS22$guns_prof)
table(conjoint_AS22$business_prof)

conjoint_AS22$abortion_prof <- car::recode(conjoint_AS22$abortion_prof, '
                                           "Strongly opposes" = "Strongly opposes restricting access to abortion";
                                           "Opposes" = "Opposes restricting access to abortion";
                                           "Undecided" = "Undecided about restricting access to abortion";
                                           "Supports" = "Supports restricting access to abortion";
                                           "Strongly supports" = "Strongly supports restricting access to abortion"
                                           ', levels = c("Strongly opposes restricting access to abortion", "Opposes restricting access to abortion", "Undecided about restricting access to abortion", "Supports restricting access to abortion", "Strongly supports restricting access to abortion"))

conjoint_AS22$guns_prof <- car::recode(conjoint_AS22$guns_prof, '
                                       "Strongly opposes" = "Strongly opposes increasing gun regulation";
                                       "Opposes" = "Opposes increasing gun regulation";
                                       "Undecided" = "Undecided about increasing gun regulation";
                                       "Supports" = "Supports increasing gun regulation";
                                       "Strongly supports" = "Strongly supports increasing gun regulation"
                                       ', levels = c("Strongly opposes increasing gun regulation", "Opposes increasing gun regulation", "Undecided about increasing gun regulation", "Supports increasing gun regulation", "Strongly supports increasing gun regulation"))

conjoint_AS22$business_prof <- car::recode(conjoint_AS22$business_prof, '
                                           "Strongly opposes" = "Strongly opposes reducing regulations on businesses";
                                           "Opposes" = "Opposes reducing regulations on businesses";
                                           "Undecided" = "Undecided about reducing regulations on businesses";
                                           "Supports" = "Supports reducing regulations on businesses";
                                           "Strongly supports" = "Strongly supports reducing regulations on businesses"
                                           ', levels = c("Strongly opposes reducing regulations on businesses", "Opposes reducing regulations on businesses", "Undecided about reducing regulations on businesses", "Supports reducing regulations on businesses", "Strongly supports reducing regulations on businesses"))

conjoint_AS22$abortion_prof_count=0
conjoint_AS22$abortion_prof_count[conjoint_AS22$abortion_prof=="Strongly opposes restricting access to abortion"]=1
conjoint_AS22$abortion_prof_count[conjoint_AS22$abortion_prof=="Strongly supports restricting access to abortion"]=1
table(conjoint_AS22$abortion_prof_count)


conjoint_AS22$guns_prof_count=0
conjoint_AS22$guns_prof_count[conjoint_AS22$guns_prof=="Strongly opposes increasing gun regulation"]=1
conjoint_AS22$guns_prof_count[conjoint_AS22$guns_prof=="Strongly supports increasing gun regulation"]=1
table(conjoint_AS22$guns_prof_count)


conjoint_AS22$business_prof_count=0
conjoint_AS22$business_prof_count[conjoint_AS22$business_prof=="Strongly opposes reducing regulations on businesses"]=1
conjoint_AS22$business_prof_count[conjoint_AS22$business_prof=="Strongly supports reducing regulations on businesses"]=1
table(conjoint_AS22$business_prof_count)

#adding extremist traits together 

conjoint_AS22$count = conjoint_AS22$uncivil + conjoint_AS22$violence_support + 
  conjoint_AS22$compromise_opp + conjoint_AS22$law_oppose + conjoint_AS22$talk_always +
  conjoint_AS22$party_farleftdem + conjoint_AS22$party_farrightrep + conjoint_AS22$abortion_prof_count +
  conjoint_AS22$guns_prof_count + conjoint_AS22$business_prof_count

table(conjoint_AS22$count)
conjoint_AS22$count=as.factor(conjoint_AS22$count)

table(conjoint_AS22$count)
conjoint_AS22$count[conjoint_AS22$count==0]=1
conjoint_AS22$count[conjoint_AS22$count==9]=8
table(conjoint_AS22$count)
conjoint_AS22$count=as.factor(conjoint_AS22$count)

#####Conjoints#####
mms_extreme_academic=cj(conjoint_AS22, extreme_R ~ violence + compromise + civil + law +
                 talk + party + race_prof + gender_prof+self_extr_recoded_2+gender_2+
                 ideo_ext + ideo_group + part_ext + part_group + race + interest_2, 
               id = ~ResponseId, estimate = "mm")


jpeg("Example academic conjoint figure.jpeg", width=7, height=10, units="in", res=600)
plot(mms_extreme_academic, vline=0.50, xlab="Extreme")
dev.off()



#no respondent characteristics 
mms_extreme_academic_2=cj(conjoint_AS22, extreme_R ~ violence + compromise + civil + law +
                          talk + party + race_prof + gender_prof,
                        id = ~ResponseId, estimate = "mm")


jpeg("Example academic conjoint figure no respondent traits.jpeg", width=7, height=10, units="in", res=600)
plot(mms_extreme_academic_2, vline=0.50, xlab="Extreme")
dev.off()



## additional analysis for academic sample, creating count of extremist traits

#conjoint with count variable
mms_extreme_academic_count=cj(conjoint_AS22, extreme_R ~ count + violence + compromise + civil + law + talk + party + race_prof + gender_prof, id = ~ResponseId, estimate = "mm")


jpeg("Example academic conjoint figure count.jpeg", width=7, height=10, units="in", res=600)
plot(mms_extreme_academic_count, vline=0.50, xlab="Extreme")
dev.off()




###Modified conjoint graphs 

####Subset of attributes graphing example####
mms_extreme_2_academic=cj(conjoint_PS22, extreme_R ~ violence + compromise + civil + law +
                   talk + party + race_prof + gender_prof, 
                 id = ~ResponseId, estimate = "mm")

#Full image
plot(mms_extreme_2_academic, vline=0.50, xlab="Extreme")

#################This plot won't work; probably because abortion_prof, guns_prof, and business_prof all have the same factor names??? error that factor[23] is duplicate, so IDK
mms_extreme_1 = cj(conjoint_PS22, extreme_R ~ violence + compromise + civil + law + talk + party + abortion_prof + guns_prof + business_prof,
                   id = ~ResponseId, estimate = "mm")


#Let's say we want to Zoom in on the violence, compromise, and rule of law attributes only
#We could re-run the analysis with only those things as attributes, but it would be
#better to use the more complete model and then only graph the attributes we want to look at

#Because the conjoint analysis creates a data object of results - the "mms_extreme_2" object -
#we can selectively plot only part of that object.

###Violence
mms_extreme_norm_academic = mms_extreme_2_academic[mms_extreme_2_academic$feature == "violence",]

plot(mms_extreme_norm_academic, vline = 0.5, xlab = "Extreme")

#interaction with respondent party
table(conjoint_PS22$part_group)


mms_extreme_aca_int=cj(conjoint_AS22, extreme_R ~ violence + compromise + civil + law +
                     talk + party + race_prof + gender_prof,
                   id = ~ResponseId, estimate = "mm", by= ~part_group)


jpeg("Interaction conjoint figure academic.jpeg", width=7, height=10, units="in", res=600)
plot(mms_extreme_aca_int, vline=0.50, xlab="Extreme") + ggplot2::facet_wrap(~BY, ncol = 3L)
dev.off()

#####Party Match Variable Creation#####
table(conjoint_AS22$party, conjoint_AS22$part_group)

conjoint_AS22$party_match = NA

conjoint_AS22$party_match[conjoint_AS22$party == "Far-left Democrat" & conjoint_AS22$part_group == "Rep"] = 0
conjoint_AS22$party_match[conjoint_AS22$party == "Democrat" & conjoint_AS22$part_group == "Rep"] = 0
conjoint_AS22$party_match[conjoint_AS22$party == "Moderate Democrat" & conjoint_AS22$part_group == "Rep"] = 0

conjoint_AS22$party_match[conjoint_AS22$party == "Far-left Democrat" & conjoint_AS22$part_group == "Ind"] = 0
conjoint_AS22$party_match[conjoint_AS22$party == "Democrat" & conjoint_AS22$part_group == "Ind"] = 0
conjoint_AS22$party_match[conjoint_AS22$party == "Moderate Democrat" & conjoint_AS22$part_group == "Ind"] = 0

conjoint_AS22$party_match[conjoint_AS22$party == "Far-right Republican" & conjoint_AS22$part_group == "Dem"] = 0
conjoint_AS22$party_match[conjoint_AS22$party == "Republican" & conjoint_AS22$part_group == "Dem"] = 0

conjoint_AS22$party_match[conjoint_AS22$party == "Far-right Republican" & conjoint_AS22$part_group == "Ind"] = 0
conjoint_AS22$party_match[conjoint_AS22$party == "Republican" & conjoint_AS22$part_group == "Ind"] = 0

conjoint_AS22$party_match[conjoint_AS22$party == "Independent" & conjoint_AS22$part_group == "Rep"] = 0
conjoint_AS22$party_match[conjoint_AS22$party == "Independent" & conjoint_AS22$part_group == "Dem"] = 0

conjoint_AS22$party_match[conjoint_AS22$party == "Far-left Democrat" & conjoint_AS22$part_group == "Dem"] = 1
conjoint_AS22$party_match[conjoint_AS22$party == "Democrat" & conjoint_AS22$part_group == "Dem"] = 1
conjoint_AS22$party_match[conjoint_AS22$party == "Moderate Democrat" & conjoint_AS22$part_group == "Dem"] = 1

conjoint_AS22$party_match[conjoint_AS22$party == "Republican" & conjoint_AS22$part_group == "Rep"] = 1
conjoint_AS22$party_match[conjoint_AS22$party == "Far-right Republican" & conjoint_AS22$part_group == "Rep"] = 1

conjoint_AS22$party_match[conjoint_AS22$party == "Independent" & conjoint_AS22$part_group == "Ind"] = 1

table(conjoint_AS22$party_match)


#conjoint with interaction using party match variable
conjoint_AS22$party_match=as.factor(conjoint_AS22$party_match)
mms_extreme_aca_int_2=cj(conjoint_AS22, extreme_R ~ violence + compromise + civil + law +
                       talk + party + race_prof + gender_prof,
                     id = ~ResponseId, estimate = "mm", by= ~party_match)


jpeg("Interaction conjoint figure party match academic.jpeg", width=7, height=10, units="in", res=600)
plot(mms_extreme_aca_int_2, vline=0.50, xlab="Extreme") + ggplot2::facet_wrap(~BY, ncol = 3L)
dev.off()



# conjoint_PS22$count = conjoint_PS22$uncivil + conjoint_PS22$violence_support + conjoint_PS22$compromise_opp + conjoint_PS22$law_oppose + conjoint_PS22$talk_always + conjoint_PS22$party_farleftdem + conjoint_PS22$party_farrightrep + conjoint_PS22$abortion_prof_count + conjoint_PS22$guns_prof_count + conjoint_PS22$business_prof_count



mm_extreme_count_public = cj(conjoint_PS22, extreme_R ~ abortion_prof + guns_prof + business_prof, id = ~ResponseId, estimate = "mm")

plot(mm_extreme_count_public, xlab = "Extreme", vline = 0.5)

# conjoint_AS22$count = conjoint_AS22$uncivil + conjoint_AS22$violence_support + conjoint_AS22$compromise_opp + conjoint_AS22$law_oppose + conjoint_AS22$talk_always + conjoint_AS22$party_farleftdem + conjoint_AS22$party_farrightrep + conjoint_AS22$abortion_prof_count + conjoint_AS22$guns_prof_count + conjoint_AS22$business_prof_count


mm_extreme_count_academic = cj(conjoint_AS22, extreme_R ~ abortion_prof + guns_prof + business_prof, id = ~ResponseId, estimate = "mm")

plot(mm_extreme_count_academic, xlab = "Extreme", vline = 0.5)

#####Similarity score: conjoint_AS22#####
#Gender
conjoint_AS22$sim_gender[conjoint_AS22$gender_prof == "Female" & conjoint_AS22$gender == "Female"] = "Gender match"
conjoint_AS22$sim_gender[conjoint_AS22$gender_prof == "Male" & conjoint_AS22$gender == "Male"] = "Gender match"
conjoint_AS22$sim_gender[conjoint_AS22$gender_prof == "Non-binary" & conjoint_AS22$gender == "Non-binary"] = "Gender match"
conjoint_AS22$sim_gender[conjoint_AS22$gender_prof == "Female" & conjoint_AS22$gender == "Male"] = "Gender mismatch"
conjoint_AS22$sim_gender[conjoint_AS22$gender_prof == "Female" & conjoint_AS22$gender == "Non-binary"] = "Gender mismatch"
conjoint_AS22$sim_gender[conjoint_AS22$gender_prof == "Male" & conjoint_AS22$gender == "Female"] = "Gender mismatch"
conjoint_AS22$sim_gender[conjoint_AS22$gender_prof == "Male" & conjoint_AS22$gender == "Non-binary"] = "Gender mismatch"
conjoint_AS22$sim_gender[conjoint_AS22$gender_prof == "Non-binary" & conjoint_AS22$gender == "Female"] = "Gender mismatch"
conjoint_AS22$sim_gender[conjoint_AS22$gender_prof == "Non-binary" & conjoint_AS22$gender == "Male"] = "Gender mismatch"

#Race
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "Hispanic American" & conjoint_AS22$race == "Hispanic"] = "Race match"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "Black American" & conjoint_AS22$race == "Black"] = "Race match"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "Asian American" & conjoint_AS22$race == "Asian"] = "Race match"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "White American" & conjoint_AS22$race == "White"] = "Race match"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "Hispanic American" & conjoint_AS22$race == "Asian"] = "Race mismatch"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "Hispanic American" & conjoint_AS22$race == "Black"] = "Race mismatch"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "Hispanic American" & conjoint_AS22$race == "White"] = "Race mismatch"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "Black American" & conjoint_AS22$race == "Asian"] = "Race mismatch"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "Black American" & conjoint_AS22$race == "Hispanic"] = "Race mismatch"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "Black American" & conjoint_AS22$race == "White"] = "Race mismatch"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "Asian American" & conjoint_AS22$race == "Black"] = "Race mismatch"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "Asian American" & conjoint_AS22$race == "Hispanic"] = "Race mismatch"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "Asian American" & conjoint_AS22$race == "White"] = "Race mismatch"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "White American" & conjoint_AS22$race == "Asian"] = "Race mismatch"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "White American" & conjoint_AS22$race == "Black"] = "Race mismatch"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "White American" & conjoint_AS22$race == "Hispanic"] = "Race mismatch"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "Hispanic American" & conjoint_AS22$race == "Other"] = "Race mismatch"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "Black American" & conjoint_AS22$race == "Other"] = "Race mismatch"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "Asian American" & conjoint_AS22$race == "Other"] = "Race mismatch"
conjoint_AS22$sim_race[conjoint_AS22$race_prof == "White American" & conjoint_AS22$race == "Other"] = "Race mismatch"

#Party has already been coded

#Sim_score variable
conjoint_AS22$sim_score[conjoint_AS22$sim_gender == "Gender mismatch" & conjoint_AS22$sim_race == "Race mismatch" & conjoint_AS22$party_match == 0] = "0 traits shared"

conjoint_AS22$sim_score[conjoint_AS22$sim_gender == "Gender mismatch" & conjoint_AS22$sim_race == "Race mismatch" & conjoint_AS22$party_match == 1] = "1 trait shared"
conjoint_AS22$sim_score[conjoint_AS22$sim_gender == "Gender mismatch" & conjoint_AS22$sim_race == "Race match" & conjoint_AS22$party_match == 0] = "1 trait shared"
conjoint_AS22$sim_score[conjoint_AS22$sim_gender == "Gender match" & conjoint_AS22$sim_race == "Race mismatch" & conjoint_AS22$party_match == 0] = "1 trait shared"

conjoint_AS22$sim_score[conjoint_AS22$sim_gender == "Gender mismatch" & conjoint_AS22$sim_race == "Race match" & conjoint_AS22$party_match == 1] = "2 traits shared"
conjoint_AS22$sim_score[conjoint_AS22$sim_gender == "Gender match" & conjoint_AS22$sim_race == "Race match" & conjoint_AS22$party_match == 0] = "2 traits shared"
conjoint_AS22$sim_score[conjoint_AS22$sim_gender == "Gender match" & conjoint_AS22$sim_race == "Race mismatch" & conjoint_AS22$party_match == 1] = "2 traits shared"

conjoint_AS22$sim_score[conjoint_AS22$sim_gender == "Gender match" & conjoint_AS22$sim_race == "Race match" & conjoint_AS22$party_match == 1] = "3 traits shared"

conjoint_AS22$sim_gender = as.factor(conjoint_AS22$sim_gender)
conjoint_AS22$sim_race = as.factor(conjoint_AS22$sim_race)
conjoint_AS22$sim_score = as.factor(conjoint_AS22$sim_score)

conjoint_AS22$party_match <- car::recode(conjoint_AS22$party_match, '
                                         "0" = "Doesnt match";
                                         "1" = "Match" ')

conjoint_AS22$party_match = as.factor(conjoint_AS22$party_match)

#Sim_score plots
mms_simscore_extreme_academic = cj(conjoint_AS22, extreme_R ~ sim_score + violence + compromise + civil + talk, id = ~ResponseId, estimate = "mm")

plot(mms_simscore_extreme_academic, vline = 0.5, xlab = "Extreme")

mms_sim_extreme_academic = cj(conjoint_AS22, extreme_R ~ sim_gender + sim_race + party_match + violence + compromise + civil + talk, id = ~ResponseId, estimate = "mm")

plot(mms_sim_extreme_academic, vline = 0.5, xlab = "Extreme")

save(conjoint_AS22, file = "Academic Sample 2022 Analysis Data.RData")


mms_extreme_p2022 = cj(conjoint_PS22, extreme_R ~ gender_prof + race_prof + party_match + part_ext + civil + compromise + talk + violence + law,
                       id = ~ResponseId, estimate = "mm")

plot(mms_extreme_p2022, vline = 0.5, xlab = "Extreme, 2022 Public")

mms_extreme_a2022 = cj(conjoint_AS22, extreme_R ~ gender_prof + race_prof + party_match + part_ext + civil + compromise + talk + violence + law,
                       id = ~ResponseId, estimate = "mm")
plot(mms_extreme_a2022, vline = 0.5, xlab = "Extreme, 2022 Academic")

mms_extreme_2021 = cj(tess_long, extreme ~ gender + race + party_match + party_ext + civil + talk + viol + law,
                      id = ~CaseId, estimate = "mm")
plot(mms_extreme_2021, vline = 0.5, xlab = "Extreme, 2021")

mms_extreme_2023 = cj(poc_23, extreme_R ~ gender_prof + race_prof + party_match + party_ext_prof + civil + compromise + talk + violence + law + listening,
                      id = ~ ResponseId, estimate = "mm")
plot(mms_extreme_2023, vline = 0.5, xlab = "Extreme, 2023")
