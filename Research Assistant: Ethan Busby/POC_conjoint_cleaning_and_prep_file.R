### POC Conjoint Cleaning and Prep File
#Set your working directory to the "POC survey - 2023" folder in box

library(readxl)
library(stringr)
library(dplyr)
library(cregg)

poc <- read.csv("POC survey 2023_raw.csv")

###Cleaning up data
poc <- subset(poc, Consent == "I agree to participate in this study")
poc <- subset(poc, age != "Under 18")

poc_state <- data.frame(poc$state, poc$Q81)

#Renaming variables so that the reshaping commands will work
poc1 = rename(poc, extreme1 = conjoint1_extreme, extreme2 = conjoint2_extreme,
            extreme3 = conjoint3_extreme, extreme4 = conjoint4_extreme,
            vote1 = conjoint1_vote, vote2 = conjoint2_vote, vote3 = conjoint3_vote,
            vote4 = conjoint4_vote, listen1 = conjoint1_listen, listen2 = conjoint2_listen,
            listen3 = conjoint3_listen, listen4 = conjoint4_listen,
            similar1 = conjoint1_similar, similar2 = conjoint2_similar,
            similar3 = conjoint3_similar, similar4 = conjoint4_similar,
            Time1 = conjoint1_time_Page.Submit, Time2 = conjoint2_time_Page.Submit,
            Time3 = conjoint3_time_Page.Submit, Time4 = conjoint4_time_Page.Submit,
            exampleprofile = traits0, self_extr = extreme_self,
            asian_extremism_rating = extreme_rating_asian,
            black_extremism_rating = extreme_rating_black,
            latinx_extremism_rating = extreme_rating_latinx,
            white_extremism_rating = extreme_rating_white)

#Reshaping; making the dataset long instead of wide
poc_23 = reshape(poc1, idvar = "ResponseId",
                 varying = list(c(grep("traits", names(poc1))),
                                c(grep("extreme", names(poc1))),
                                c(grep("vote", names(poc1))),
                                c(grep("listen", names(poc1))),
                                c(grep("similar", names(poc1))),
                                c(grep("Time", names(poc1)))),
                 sep = "",
                 v.names = c("profile",
                             "extreme",
                             "vote",
                             "listen",
                             "similar",
                             "Time"),
                 timevar = "profilenum",
                 times = c("1", "2", "3", "4"),
                 new.row.names = 1:6728,
                 direction = "long")

#Checks to make sure that the reshaping worked
table(poc_23$profilenum)
mean(table(poc_23$ResponseId))
min(table(poc_23$ResponseId))
max(table(poc_23$ResponseId))

table(poc_23$extreme[poc_23$ResponseId=="R_ugd9urOJj5rFdy9"&poc_23$profilenum==1])==table(poc1$extreme1[poc1$ResponseId=="R_ugd9urOJj5rFdy9"])
table(poc_23$extreme[poc_23$ResponseId=="R_ugd9urOJj5rFdy9"&poc_23$profilenum==2])==table(poc1$extreme2[poc1$ResponseId=="R_ugd9urOJj5rFdy9"])
table(poc_23$extreme[poc_23$ResponseId=="R_ugd9urOJj5rFdy9"&poc_23$profilenum==3])==table(poc1$extreme3[poc1$ResponseId=="R_ugd9urOJj5rFdy9"])
table(poc_23$extreme[poc_23$ResponseId=="R_ugd9urOJj5rFdy9"&poc_23$profilenum==4])==table(poc1$extreme4[poc1$ResponseId=="R_ugd9urOJj5rFdy9"])

table(poc_23$extreme[poc_23$ResponseId=="R_3h1fbvFAwEpQHbX"&poc_23$profilenum==1])==table(poc1$extreme1[poc1$ResponseId=="R_3h1fbvFAwEpQHbX"])
table(poc_23$extreme[poc_23$ResponseId=="R_3h1fbvFAwEpQHbX"&poc_23$profilenum==2])==table(poc1$extreme2[poc1$ResponseId=="R_3h1fbvFAwEpQHbX"])
table(poc_23$extreme[poc_23$ResponseId=="R_3h1fbvFAwEpQHbX"&poc_23$profilenum==3])==table(poc1$extreme3[poc1$ResponseId=="R_3h1fbvFAwEpQHbX"])
table(poc_23$extreme[poc_23$ResponseId=="R_3h1fbvFAwEpQHbX"&poc_23$profilenum==4])==table(poc1$extreme4[poc1$ResponseId=="R_3h1fbvFAwEpQHbX"])

table(poc_23$vote[poc_23$ResponseId=="R_vMIHD2kz1mOQr6x"&poc_23$profilenum==1])==table(poc1$vote1[poc1$ResponseId=="R_vMIHD2kz1mOQr6x"])
table(poc_23$vote[poc_23$ResponseId=="R_vMIHD2kz1mOQr6x"&poc_23$profilenum==2])==table(poc1$vote2[poc1$ResponseId=="R_vMIHD2kz1mOQr6x"])
table(poc_23$vote[poc_23$ResponseId=="R_vMIHD2kz1mOQr6x"&poc_23$profilenum==3])==table(poc1$vote3[poc1$ResponseId=="R_vMIHD2kz1mOQr6x"])
table(poc_23$vote[poc_23$ResponseId=="R_vMIHD2kz1mOQr6x"&poc_23$profilenum==4])==table(poc1$vote4[poc1$ResponseId=="R_vMIHD2kz1mOQr6x"])

table(poc_23$listen[poc_23$ResponseId=="R_u1A7X8Sk6DDOhnH"&poc_23$profilenum==1])==table(poc1$listen1[poc1$ResponseId=="R_u1A7X8Sk6DDOhnH"])
table(poc_23$listen[poc_23$ResponseId=="R_u1A7X8Sk6DDOhnH"&poc_23$profilenum==2])==table(poc1$listen2[poc1$ResponseId=="R_u1A7X8Sk6DDOhnH"])
table(poc_23$listen[poc_23$ResponseId=="R_u1A7X8Sk6DDOhnH"&poc_23$profilenum==3])==table(poc1$listen3[poc1$ResponseId=="R_u1A7X8Sk6DDOhnH"])
table(poc_23$listen[poc_23$ResponseId=="R_u1A7X8Sk6DDOhnH"&poc_23$profilenum==4])==table(poc1$listen4[poc1$ResponseId=="R_u1A7X8Sk6DDOhnH"])

table(poc_23$similar[poc_23$ResponseId=="R_2ciQ7mjLzj6QbLd"&poc_23$profilenum==1])==table(poc1$similar1[poc1$ResponseId=="R_2ciQ7mjLzj6QbLd"])
table(poc_23$similar[poc_23$ResponseId=="R_2ciQ7mjLzj6QbLd"&poc_23$profilenum==2])==table(poc1$similar2[poc1$ResponseId=="R_2ciQ7mjLzj6QbLd"])
table(poc_23$similar[poc_23$ResponseId=="R_2ciQ7mjLzj6QbLd"&poc_23$profilenum==3])==table(poc1$similar3[poc1$ResponseId=="R_2ciQ7mjLzj6QbLd"])
table(poc_23$similar[poc_23$ResponseId=="R_2ciQ7mjLzj6QbLd"&poc_23$profilenum==4])==table(poc1$similar4[poc1$ResponseId=="R_2ciQ7mjLzj6QbLd"])

#List of variables to recode: understood, interest, deathpenalty, age, education, gender, race, focus, income, pid, ideo7, relig, bornagain, interest, talk, deathpenalty, affirmativeaction, abortion, wall, imports, econ_inequality, millionairetax, obamacare, vaccineschools, climate, assaultrifles, trans_military, lgbt_jobs, school_prayers, social_media, auth_1, dem_satis, trust_people, trust_gov, efficacy, pid_recode_dem, pid_recode_rep, responsible_nchar, responsible_done_nchar, democracy_nchar, feedback_nchar
#Age
poc_23$age_recoded = 0
poc_23$age_recoded[poc_23$age == "18-24 years old"] = 1/6
poc_23$age_recoded[poc_23$age == "25-34 years old"] = 1/3
poc_23$age_recoded[poc_23$age == "35-44 years old"] = 1/2
poc_23$age_recoded[poc_23$age == "45-54 years old"] = 2/3
poc_23$age_recoded[poc_23$age == "55-64 years old"] = 5/6
poc_23$age_recoded[poc_23$age == "65+ years old"] = 1

#Education: question, what to do with education?
poc_23$education_recoded[poc_23$education == "Some high school or less"] = 0
poc_23$education_recoded[poc_23$education == "High school diploma or GED"] = 1/5
poc_23$education_recoded[poc_23$education == "Some college, but no degree"] = 2/5
poc_23$education_recoded[poc_23$education == "Associates or technical degree"] = 3/5
poc_23$education_recoded[poc_23$education == "Bachelor's degree"] = 4/5
poc_23$education_recoded[poc_23$education == "Graduate or professional degree (MA, MS, MBA, PhD, JD, MD, DDS etc.)"] = 1

#Gender
poc_23$gender_recoded[poc_23$gender == "Male"] = 0
poc_23$gender_recoded[poc_23$gender == "Non-binary"] = 0.5
poc_23$gender_recoded[poc_23$gender == "Female"] = 1

#Race
poc_23$race[str_detect(poc_23$race.ethnicity, ",")] = "Other"
poc_23$race[poc_23$race.ethnicity == "Other"] = "Other"
poc_23$race[str_detect(poc_23$race.ethnicity, "Hispanic")] = "Hispanic"
poc_23$race[poc_23$race.ethnicity == "Black or African American"] = "Black"
poc_23$race[poc_23$race.ethnicity == "Asian or Asian American"] = "Asian"
poc_23$race[poc_23$race.ethnicity == "White"] = "White"
poc_23$race[poc_23$race.ethnicity == "Hawaiian or Pacific Islander"] = "Other"
poc_23$race[poc_23$race.ethnicity == "Middle Eastern or North African"] = "Other"
poc_23$race[poc_23$race.ethnicity == "American Indian or Alaska Native"] = "Other"
poc_23$race = as.factor(poc_23$race)

#Income
poc_23$income_recoded[poc_23$income == "Under $25,000"] = 0
poc_23$income_recoded[poc_23$income == "Between $25,000 and $49,999"] = 1/6
poc_23$income_recoded[poc_23$income == "Between $50,000 and $74,999"] = 1/3
poc_23$income_recoded[poc_23$income == "Between $75,000 and $99,999"] = 1/2
poc_23$income_recoded[poc_23$income == "Between $100,000 and $199,999"] = 2/3
poc_23$income_recoded[poc_23$income == "Between $200,000 and $299,999"] = 5/6
poc_23$income_recoded[poc_23$income == "$300,000 or more"] = 1

#Pid
poc_23$pid_recoded[poc_23$pid == "Strong Democrat"] = 0
poc_23$pid_recoded[poc_23$pid == "Not so strong Democrat"] = 1/6
poc_23$pid_recoded[poc_23$pid == "Independent leaning Democrat"] = 1/3
poc_23$pid_recoded[poc_23$pid == "Other"] = 1/2
poc_23$pid_recoded[poc_23$pid == "Independent"] = 1/2
poc_23$pid_recoded[poc_23$pid == "Independent leaning Republican"] = 2/3
poc_23$pid_recoded[poc_23$pid == "Not so strong Republican"] = 5/6
poc_23$pid_recoded[poc_23$pid == "Strong Republican"] = 1

#Ideo7
poc_23$ideo7_recoded[poc_23$ideo7 == "Extremely liberal"] = 0
poc_23$ideo7_recoded[poc_23$ideo7 == "Liberal"] = 1/6
poc_23$ideo7_recoded[poc_23$ideo7 == "Slightly liberal"] = 1/3
poc_23$ideo7_recoded[poc_23$ideo7 == "Haven't thought much about this"] = 1/2
poc_23$ideo7_recoded[poc_23$ideo7 == "Moderate; middle of the road"] = 1/2
poc_23$ideo7_recoded[poc_23$ideo7 == "Slightly conservative"] = 2/3
poc_23$ideo7_recoded[poc_23$ideo7 == "Conservative"] = 5/6
poc_23$ideo7_recoded[poc_23$ideo7 == "Extremely conservative"] = 1

#Self_extr
poc_23$self_extr = factor(poc_23$self_extr, levels = c("Not at all extreme",
                                                       "A little extreme",
                                                       "Somewhat extreme",
                                                       "Extreme",
                                                       "Very extreme"))

poc_23$self_extr_recoded[poc_23$self_extr == "Not at all extreme"] = 0
poc_23$self_extr_recoded[poc_23$self_extr == "A little extreme"] = 0.25
poc_23$self_extr_recoded[poc_23$self_extr == "Somewhat extreme"] = 0.5
poc_23$self_extr_recoded[poc_23$self_extr == "Extreme"] = 0.75
poc_23$self_extr_recoded[poc_23$self_extr == "Very extreme"] = 1

#Relig
poc_23$relig_recoded[poc_23$relig == "Protestant"] = 0
poc_23$relig_recoded[poc_23$relig == "Roman Catholic"] = 1/11
poc_23$relig_recoded[poc_23$relig == "Mormon"] = 2/11
poc_23$relig_recoded[poc_23$relig == "Eastern or Greek Orthodox"] = 3/11
poc_23$relig_recoded[poc_23$relig == "Muslim"] = 4/11
poc_23$relig_recoded[poc_23$relig == "Jewish"] = 5/11
poc_23$relig_recoded[poc_23$relig == "Buddhist"] = 6/11
poc_23$relig_recoded[poc_23$relig == "Hindu"] = 7/11
poc_23$relig_recoded[poc_23$relig == "Atheist"] = 8/11
poc_23$relig_recoded[poc_23$relig == "Agnostic"] = 9/11
poc_23$relig_recoded[poc_23$relig == "Other"] = 10/11
poc_23$relig_recoded[poc_23$relig == "None/Not Religious"] = 1

#Bornagain
poc_23$bornagain_recoded[poc_23$bornagain == "No"] = 0
poc_23$bornagain_recoded[poc_23$bornagain == "Yes"] = 1

#Understood
#1
poc_23$understood_1_recoded[poc_23$understood_1 == "Not at all true"] = 0
poc_23$understood_1_recoded[poc_23$understood_1 == "A little true"] = 0.25
poc_23$understood_1_recoded[poc_23$understood_1 =="Somewhat true"] = 0.5
poc_23$understood_1_recoded[poc_23$understood_1 == "Mostly true"] = 0.75
poc_23$understood_1_recoded[poc_23$understood_1 == "Completely true"] = 1

#2
poc_23$understood_2_recoded[poc_23$understood_2 == "Not at all true"] = 0
poc_23$understood_2_recoded[poc_23$understood_2 == "A little true"] = 0.25
poc_23$understood_2_recoded[poc_23$understood_2 =="Somewhat true"] = 0.5
poc_23$understood_2_recoded[poc_23$understood_2 == "Mostly true"] = 0.75
poc_23$understood_2_recoded[poc_23$understood_2 == "Completely true"] = 1

#3
poc_23$understood_3_recoded[poc_23$understood_3 == "Not at all true"] = 0
poc_23$understood_3_recoded[poc_23$understood_3 == "A little true"] = 0.25
poc_23$understood_3_recoded[poc_23$understood_3 =="Somewhat true"] = 0.5
poc_23$understood_3_recoded[poc_23$understood_3 == "Mostly true"] = 0.75
poc_23$understood_3_recoded[poc_23$understood_3 == "Completely true"] = 1

#4
poc_23$understood_4_recoded[poc_23$understood_4 == "Not at all true"] = 0
poc_23$understood_4_recoded[poc_23$understood_4 == "A little true"] = 0.25
poc_23$understood_4_recoded[poc_23$understood_4 =="Somewhat true"] = 0.5
poc_23$understood_4_recoded[poc_23$understood_4 == "Mostly true"] = 0.75
poc_23$understood_4_recoded[poc_23$understood_4 == "Completely true"] = 1

#Interest
poc_23$interest_recoded[poc_23$interest == "Not at all interested"] = 0
poc_23$interest_recoded[poc_23$interest == "Not very interested"] = 0.25
poc_23$interest_recoded[poc_23$interest == "Somewhat interested"] = 0.5
poc_23$interest_recoded[poc_23$interest == "Interested"] = 0.75
poc_23$interest_recoded[poc_23$interest == "Very interested"] = 1

#Talk
poc_23$talk_recoded[poc_23$talk == "Zero"] = 0
poc_23$talk_recoded[poc_23$talk == "One"] = 1/7
poc_23$talk_recoded[poc_23$talk == "Two"] = 2/7
poc_23$talk_recoded[poc_23$talk == "Three"] = 3/7
poc_23$talk_recoded[poc_23$talk == "Four"] = 4/7
poc_23$talk_recoded[poc_23$talk == "Five"] = 5/7
poc_23$talk_recoded[poc_23$talk == "Six"] = 6/7
poc_23$talk_recoded[poc_23$talk == "Seven"] = 1

#Deathpenalty
poc_23$deathpenalty_recoded[poc_23$deathpenalty == "Strongly oppose"] = 0
poc_23$deathpenalty_recoded[poc_23$deathpenalty == "Oppose"] = 1/6
poc_23$deathpenalty_recoded[poc_23$deathpenalty == "Somewhat oppose"] = 1/3
poc_23$deathpenalty_recoded[poc_23$deathpenalty == "Neither favor nor oppose"] = 1/2
poc_23$deathpenalty_recoded[poc_23$deathpenalty == "Somewhat favor"] = 2/3
poc_23$deathpenalty_recoded[poc_23$deathpenalty == "Favor"] = 5/6
poc_23$deathpenalty_recoded[poc_23$deathpenalty == "Strongly favor"] = 1

#Affirmativeaction
poc_23$affirmativeaction_recoded[poc_23$affirmativeaction == "Strongly oppose"] = 0
poc_23$affirmativeaction_recoded[poc_23$affirmativeaction == "Oppose"] = 1/6
poc_23$affirmativeaction_recoded[poc_23$affirmativeaction == "Somewhat oppose"] = 1/3
poc_23$affirmativeaction_recoded[poc_23$affirmativeaction == "Neither favor nor oppose"] = 1/2
poc_23$affirmativeaction_recoded[poc_23$affirmativeaction == "Somewhat favor"] = 2/3
poc_23$affirmativeaction_recoded[poc_23$affirmativeaction == "Favor"] = 5/6
poc_23$affirmativeaction_recoded[poc_23$affirmativeaction == "Strongly favor"] = 1

#Abortion
poc_23$abortion_recoded[poc_23$abortion == "Strongly oppose"] = 0
poc_23$abortion_recoded[poc_23$abortion == "Oppose"] = 1/6
poc_23$abortion_recoded[poc_23$abortion == "Somewhat oppose"] = 1/3
poc_23$abortion_recoded[poc_23$abortion == "Neither favor nor oppose"] = 1/2
poc_23$abortion_recoded[poc_23$abortion == "Somewhat favor"] = 2/3
poc_23$abortion_recoded[poc_23$abortion == "Favor"] = 5/6
poc_23$abortion_recoded[poc_23$abortion == "Strongly favor"] = 1

#Wall
poc_23$wall_recoded[poc_23$wall == "Strongly oppose"] = 0
poc_23$wall_recoded[poc_23$wall == "Oppose"] = 1/6
poc_23$wall_recoded[poc_23$wall == "Somewhat oppose"] = 1/3
poc_23$wall_recoded[poc_23$wall == "Neither favor nor oppose"] = 1/2
poc_23$wall_recoded[poc_23$wall == "Somewhat favor"] = 2/3
poc_23$wall_recoded[poc_23$wall == "Favor"] = 5/6
poc_23$wall_recoded[poc_23$wall == "Strongly favor"] = 1

#Imports
poc_23$imports_recoded[poc_23$imports == "Strongly oppose"] = 0
poc_23$imports_recoded[poc_23$imports == "Oppose"] = 1/6
poc_23$imports_recoded[poc_23$imports == "Somewhat oppose"] = 1/3
poc_23$imports_recoded[poc_23$imports == "Neither favor nor oppose"] = 1/2
poc_23$imports_recoded[poc_23$imports == "Somewhat favor"] = 2/3
poc_23$imports_recoded[poc_23$imports == "Favor"] = 5/6
poc_23$imports_recoded[poc_23$imports == "Strongly favor"] = 1

#Econ_inequality
poc_23$econ_inequality_recoded[poc_23$econ_inequality == "Strongly oppose"] = 0
poc_23$econ_inequality_recoded[poc_23$econ_inequality == "Oppose"] = 1/6
poc_23$econ_inequality_recoded[poc_23$econ_inequality == "Somewhat oppose"] = 1/3
poc_23$econ_inequality_recoded[poc_23$econ_inequality == "Neither favor nor oppose"] = 1/2
poc_23$econ_inequality_recoded[poc_23$econ_inequality == "Somewhat favor"] = 2/3
poc_23$econ_inequality_recoded[poc_23$econ_inequality == "Favor"] = 5/6
poc_23$econ_inequality_recoded[poc_23$econ_inequality == "Strongly favor"] = 1

#Millionaretax
poc_23$millionairetax_recoded[poc_23$millionairetax == "Strongly oppose"] = 0
poc_23$millionairetax_recoded[poc_23$millionairetax == "Oppose"] = 1/6
poc_23$millionairetax_recoded[poc_23$millionairetax == "Somewhat oppose"] = 1/3
poc_23$millionairetax_recoded[poc_23$millionairetax == "Neither favor nor oppose"] = 1/2
poc_23$millionairetax_recoded[poc_23$millionairetax == "Somewhat favor"] = 2/3
poc_23$millionairetax_recoded[poc_23$millionairetax == "Favor"] = 5/6
poc_23$millionairetax_recoded[poc_23$millionairetax == "Strongly favor"] = 1

#Obamacare
poc_23$obamacare_recoded[poc_23$obamacare == "Strongly oppose"] = 0
poc_23$obamacare_recoded[poc_23$obamacare == "Oppose"] = 1/6
poc_23$obamacare_recoded[poc_23$obamacare == "Somewhat oppose"] = 1/3
poc_23$obamacare_recoded[poc_23$obamacare == "Neither favor nor oppose"] = 1/2
poc_23$obamacare_recoded[poc_23$obamacare == "Somewhat favor"] = 2/3
poc_23$obamacare_recoded[poc_23$obamacare == "Favor"] = 5/6
poc_23$obamacare_recoded[poc_23$obamacare == "Strongly favor"] = 1

#Vaccineschools
poc_23$vaccineschools_recoded[poc_23$vaccineschools == "Strongly oppose"] = 0
poc_23$vaccineschools_recoded[poc_23$vaccineschools == "Oppose"] = 1/6
poc_23$vaccineschools_recoded[poc_23$vaccineschools == "Somewhat oppose"] = 1/3
poc_23$vaccineschools_recoded[poc_23$vaccineschools == "Neither favor nor oppose"] = 1/2
poc_23$vaccineschools_recoded[poc_23$vaccineschools == "Somewhat favor"] = 2/3
poc_23$vaccineschools_recoded[poc_23$vaccineschools == "Favor"] = 5/6
poc_23$vaccineschools_recoded[poc_23$vaccineschools == "Strongly favor"] = 1

#Climate
poc_23$climate_recoded[poc_23$climate == "Strongly oppose"] = 0
poc_23$climate_recoded[poc_23$climate == "Oppose"] = 1/6
poc_23$climate_recoded[poc_23$climate == "Somewhat oppose"] = 1/3
poc_23$climate_recoded[poc_23$climate == "Neither favor nor oppose"] = 1/2
poc_23$climate_recoded[poc_23$climate == "Somewhat favor"] = 2/3
poc_23$climate_recoded[poc_23$climate == "Favor"] = 5/6
poc_23$climate_recoded[poc_23$climate == "Strongly favor"] = 1

#Assaultrifles
poc_23$assaultrifles_recoded[poc_23$assaultrifles == "Strongly oppose"] = 0
poc_23$assaultrifles_recoded[poc_23$assaultrifles == "Oppose"] = 1/6
poc_23$assaultrifles_recoded[poc_23$assaultrifles == "Somewhat oppose"] = 1/3
poc_23$assaultrifles_recoded[poc_23$assaultrifles == "Neither favor nor oppose"] = 1/2
poc_23$assaultrifles_recoded[poc_23$assaultrifles == "Somewhat favor"] = 2/3
poc_23$assaultrifles_recoded[poc_23$assaultrifles == "Favor"] = 5/6
poc_23$assaultrifles_recoded[poc_23$assaultrifles == "Strongly favor"] = 1

#Trans_military
poc_23$trans_military_recoded[poc_23$trans_military == "Strongly oppose"] = 0
poc_23$trans_military_recoded[poc_23$trans_military == "Oppose"] = 1/6
poc_23$trans_military_recoded[poc_23$trans_military == "Somewhat oppose"] = 1/3
poc_23$trans_military_recoded[poc_23$trans_military == "Neither favor nor oppose"] = 1/2
poc_23$trans_military_recoded[poc_23$trans_military == "Somewhat favor"] = 2/3
poc_23$trans_military_recoded[poc_23$trans_military == "Favor"] = 5/6
poc_23$trans_military_recoded[poc_23$trans_military == "Strongly favor"] = 1

#Lgbt_jobs
poc_23$lgbt_jobs_recoded[poc_23$lgbt_jobs == "Strongly oppose"] = 0
poc_23$lgbt_jobs_recoded[poc_23$lgbt_jobs == "Oppose"] = 1/6
poc_23$lgbt_jobs_recoded[poc_23$lgbt_jobs == "Somewhat oppose"] = 1/3
poc_23$lgbt_jobs_recoded[poc_23$lgbt_jobs == "Neither favor nor oppose"] = 1/2
poc_23$lgbt_jobs_recoded[poc_23$lgbt_jobs == "Somewhat favor"] = 2/3
poc_23$lgbt_jobs_recoded[poc_23$lgbt_jobs == "Favor"] = 5/6
poc_23$lgbt_jobs_recoded[poc_23$lgbt_jobs == "Strongly favor"] = 1

#School_prayers
poc_23$school_prayers_recoded[poc_23$school_prayers == "Strongly oppose"] = 0
poc_23$school_prayers_recoded[poc_23$school_prayers == "Oppose"] = 1/6
poc_23$school_prayers_recoded[poc_23$school_prayers == "Somewhat oppose"] = 1/3
poc_23$school_prayers_recoded[poc_23$school_prayers == "Neither favor nor oppose"] = 1/2
poc_23$school_prayers_recoded[poc_23$school_prayers == "Somewhat favor"] = 2/3
poc_23$school_prayers_recoded[poc_23$school_prayers == "Favor"] = 5/6
poc_23$school_prayers_recoded[poc_23$school_prayers == "Strongly favor"] = 1

#Social_media
poc_23$social_media_recoded[poc_23$social_media == "Strongly oppose"] = 0
poc_23$social_media_recoded[poc_23$social_media == "Oppose"] = 1/6
poc_23$social_media_recoded[poc_23$social_media == "Somewhat oppose"] = 1/3
poc_23$social_media_recoded[poc_23$social_media == "Neither favor nor oppose"] = 1/2
poc_23$social_media_recoded[poc_23$social_media == "Somewhat favor"] = 2/3
poc_23$social_media_recoded[poc_23$social_media == "Favor"] = 5/6
poc_23$social_media_recoded[poc_23$social_media == "Strongly favor"] = 1

#Dem_satis
poc_23$dem_satis_recoded[poc_23$dem_satis == "Not at all satisfied"] = 0
poc_23$dem_satis_recoded[poc_23$dem_satis == "Not very satisfied"] = 1/3
poc_23$dem_satis_recoded[poc_23$dem_satis == "Fairly satsified"] = 2/3
poc_23$dem_satis_recoded[poc_23$dem_satis == "Very satisfied"] = 1

#Trust_people
poc_23$trust_people_recoded[poc_23$trust_people == "Never"] = 0
poc_23$trust_people_recoded[poc_23$trust_people == "Some of the time"] = 0.25
poc_23$trust_people_recoded[poc_23$trust_people == "About half of the time"] = 0.5
poc_23$trust_people_recoded[poc_23$trust_people == "Most of the time"] = 0.75
poc_23$trust_people_recoded[poc_23$trust_people == "Always"] = 1

#Trust_gov
poc_23$trust_gov_recoded[poc_23$trust_gov == "Never"] = 0
poc_23$trust_gov_recoded[poc_23$trust_gov == "Some of the time"] = 0.25
poc_23$trust_gov_recoded[poc_23$trust_gov == "About half of the time"] = 0.5
poc_23$trust_gov_recoded[poc_23$trust_gov == "Most of the time"] = 0.75
poc_23$trust_gov_recoded[poc_23$trust_gov == "Always"] = 1

#Efficacy
#1
poc_23$efficacy_1_recoded[poc_23$efficacy_1 == "Strongly agree"] = 0
poc_23$efficacy_1_recoded[poc_23$efficacy_1 == "Agree"] = 1/6
poc_23$efficacy_1_recoded[poc_23$efficacy_1 == "Somewhat agree"] = 1/3
poc_23$efficacy_1_recoded[poc_23$efficacy_1 == "Neither agree nor disagree"] = 1/2
poc_23$efficacy_1_recoded[poc_23$efficacy_1 == "Somewhat disagree"] = 2/3
poc_23$efficacy_1_recoded[poc_23$efficacy_1 == "Disagree"] = 5/6
poc_23$efficacy_1_recoded[poc_23$efficacy_1 == "Strongly disagree"] = 1

#2
poc_23$efficacy_2_recoded[poc_23$efficacy_2 == "Strongly agree"] = 0
poc_23$efficacy_2_recoded[poc_23$efficacy_2 == "Agree"] = 1/6
poc_23$efficacy_2_recoded[poc_23$efficacy_2 == "Somewhat agree"] = 1/3
poc_23$efficacy_2_recoded[poc_23$efficacy_2 == "Neither agree nor disagree"] = 1/2
poc_23$efficacy_2_recoded[poc_23$efficacy_2 == "Somewhat disagree"] = 2/3
poc_23$efficacy_2_recoded[poc_23$efficacy_2 == "Disagree"] = 5/6
poc_23$efficacy_2_recoded[poc_23$efficacy_2 == "Strongly disagree"] = 1

#3
poc_23$efficacy_3_recoded[poc_23$efficacy_3 == "Strongly agree"] = 0
poc_23$efficacy_3_recoded[poc_23$efficacy_3 == "Agree"] = 1/6
poc_23$efficacy_3_recoded[poc_23$efficacy_3 == "Somewhat agree"] = 1/3
poc_23$efficacy_3_recoded[poc_23$efficacy_3 == "Neither agree nor disagree"] = 1/2
poc_23$efficacy_3_recoded[poc_23$efficacy_3 == "Somewhat disagree"] = 2/3
poc_23$efficacy_3_recoded[poc_23$efficacy_3 == "Disagree"] = 5/6
poc_23$efficacy_3_recoded[poc_23$efficacy_3 == "Strongly disagree"] = 1

#4
poc_23$efficacy_4_recoded[poc_23$efficacy_4 == "Strongly agree"] = 0
poc_23$efficacy_4_recoded[poc_23$efficacy_4 == "Agree"] = 1/6
poc_23$efficacy_4_recoded[poc_23$efficacy_4 == "Somewhat agree"] = 1/3
poc_23$efficacy_4_recoded[poc_23$efficacy_4 == "Neither agree nor disagree"] = 1/2
poc_23$efficacy_4_recoded[poc_23$efficacy_4 == "Somewhat disagree"] = 2/3
poc_23$efficacy_4_recoded[poc_23$efficacy_4 == "Disagree"] = 5/6
poc_23$efficacy_4_recoded[poc_23$efficacy_4 == "Strongly disagree"] = 1

#5
poc_23$efficacy_5_recoded[poc_23$efficacy_5 == "Strongly agree"] = 0
poc_23$efficacy_5_recoded[poc_23$efficacy_5 == "Agree"] = 1/6
poc_23$efficacy_5_recoded[poc_23$efficacy_5 == "Somewhat agree"] = 1/3
poc_23$efficacy_5_recoded[poc_23$efficacy_5 == "Neither agree nor disagree"] = 1/2
poc_23$efficacy_5_recoded[poc_23$efficacy_5 == "Somewhat disagree"] = 2/3
poc_23$efficacy_5_recoded[poc_23$efficacy_5 == "Disagree"] = 5/6
poc_23$efficacy_5_recoded[poc_23$efficacy_5 == "Strongly disagree"] = 1

#Race.importance
poc_23$raceimportance_recoded[poc_23$race.importance == "Not at all important"] = 0
poc_23$raceimportance_recoded[poc_23$race.importance == "A little important"] = 0.25
poc_23$raceimportance_recoded[poc_23$race.importance == "Moderately important"] = 0.5
poc_23$raceimportance_recoded[poc_23$race.importance == "Very important"] = 0.75
poc_23$raceimportance_recoded[poc_23$race.importance == "Extremely important"] = 1

#Race.linked.fate
poc_23$racelinkedfate_recoded[poc_23$race.linked.fate == "Not at all"] = 0
poc_23$racelinkedfate_recoded[poc_23$race.linked.fate == "Not very much"] = 1/3
poc_23$racelinkedfate_recoded[poc_23$race.linked.fate == "A moderate amount"] = 2/3
poc_23$racelinkedfate_recoded[poc_23$race.linked.fate == "A lot"] = 1

#Violence_justified
poc_23$violence_justified_recoded[poc_23$violence_justified == "Not at all"] = 0
poc_23$violence_justified_recoded[poc_23$violence_justified == "A little"] = 0.25
poc_23$violence_justified_recoded[poc_23$violence_justified == "A moderate amount"] = 0.5
poc_23$violence_justified_recoded[poc_23$violence_justified == "A lot"] = 0.75
poc_23$violence_justified_recoded[poc_23$violence_justified == "A great deal"] = 1

#Reducing.extremism
#1
poc_23$reducing_extremism_1_recoded[poc_23$reducing.extremism_1 == "Very ineffective"] = 0
poc_23$reducing_extremism_1_recoded[poc_23$reducing.extremism_1 == "Ineffective"] = 1/6
poc_23$reducing_extremism_1_recoded[poc_23$reducing.extremism_1 == "Somewhat ineffective"] = 1/3
poc_23$reducing_extremism_1_recoded[poc_23$reducing.extremism_1 == "Neither ineffective nor effective"] = 1/2
poc_23$reducing_extremism_1_recoded[poc_23$reducing.extremism_1 == "Somewhat effective"] = 2/3
poc_23$reducing_extremism_1_recoded[poc_23$reducing.extremism_1 == "Effective"] = 5/6
poc_23$reducing_extremism_1_recoded[poc_23$reducing.extremism_1 == "Very effective"] = 1

#2
poc_23$reducing_extremism_2_recoded[poc_23$reducing.extremism_2 == "Very ineffective"] = 0
poc_23$reducing_extremism_2_recoded[poc_23$reducing.extremism_2 == "Ineffective"] = 1/6
poc_23$reducing_extremism_2_recoded[poc_23$reducing.extremism_2 == "Somewhat ineffective"] = 1/3
poc_23$reducing_extremism_2_recoded[poc_23$reducing.extremism_2 == "Neither ineffective nor effective"] = 1/2
poc_23$reducing_extremism_2_recoded[poc_23$reducing.extremism_2 == "Somewhat effective"] = 2/3
poc_23$reducing_extremism_2_recoded[poc_23$reducing.extremism_2 == "Effective"] = 5/6
poc_23$reducing_extremism_2_recoded[poc_23$reducing.extremism_2 == "Very effective"] = 1

#3
poc_23$reducing_extremism_3_recoded[poc_23$reducing.extremism_3 == "Very ineffective"] = 0
poc_23$reducing_extremism_3_recoded[poc_23$reducing.extremism_3 == "Ineffective"] = 1/6
poc_23$reducing_extremism_3_recoded[poc_23$reducing.extremism_3 == "Somewhat ineffective"] = 1/3
poc_23$reducing_extremism_3_recoded[poc_23$reducing.extremism_3 == "Neither ineffective nor effective"] = 1/2
poc_23$reducing_extremism_3_recoded[poc_23$reducing.extremism_3 == "Somewhat effective"] = 2/3
poc_23$reducing_extremism_3_recoded[poc_23$reducing.extremism_3 == "Effective"] = 5/6
poc_23$reducing_extremism_3_recoded[poc_23$reducing.extremism_3 == "Very effective"] = 1

#4
poc_23$reducing_extremism_4_recoded[poc_23$reducing.extremism_4 == "Very ineffective"] = 0
poc_23$reducing_extremism_4_recoded[poc_23$reducing.extremism_4 == "Ineffective"] = 1/6
poc_23$reducing_extremism_4_recoded[poc_23$reducing.extremism_4 == "Somewhat ineffective"] = 1/3
poc_23$reducing_extremism_4_recoded[poc_23$reducing.extremism_4 == "Neither ineffective nor effective"] = 1/2
poc_23$reducing_extremism_4_recoded[poc_23$reducing.extremism_4 == "Somewhat effective"] = 2/3
poc_23$reducing_extremism_4_recoded[poc_23$reducing.extremism_4 == "Effective"] = 5/6
poc_23$reducing_extremism_4_recoded[poc_23$reducing.extremism_4 == "Very effective"] = 1

#Target_self
poc_23$target_self_recoded[poc_23$target_self == "Almost never"] = 0
poc_23$target_self_recoded[poc_23$target_self == "Rarely"] = 0.25
poc_23$target_self_recoded[poc_23$target_self == "Sometimes"] = 0.5
poc_23$target_self_recoded[poc_23$target_self == "A moderate amount"] = 0.75
poc_23$target_self_recoded[poc_23$target_self == "Frequently"] = 1

#Target_general
poc_23$target_general_recoded[poc_23$target_general == "Almost never"] = 0
poc_23$target_general_recoded[poc_23$target_general == "Rarely"] = 0.25
poc_23$target_general_recoded[poc_23$target_general == "A moderate amount of time"] = 0.5
poc_23$target_general_recoded[poc_23$target_general == "Often"] = 0.75
poc_23$target_general_recoded[poc_23$target_general == "Almost always"] = 1

#Extreme ratings
#Asian
poc_23$asian_extreme_R[poc_23$asian_extremism_rating == "1 - Not at all extreme"] = 0
poc_23$asian_extreme_R[poc_23$asian_extremism_rating == "2"] = 1/6
poc_23$asian_extreme_R[poc_23$asian_extremism_rating == "3"] = 1/3
poc_23$asian_extreme_R[poc_23$asian_extremism_rating == "4"] = 1/2
poc_23$asian_extreme_R[poc_23$asian_extremism_rating == "5"] = 2/3
poc_23$asian_extreme_R[poc_23$asian_extremism_rating == "6"] = 5/6
poc_23$asian_extreme_R[poc_23$asian_extremism_rating == "7 - Extreme"] = 1

#Black
poc_23$black_extreme_R[poc_23$black_extremism_rating == "1 - Not at all extreme"] = 0
poc_23$black_extreme_R[poc_23$black_extremism_rating == "2"] = 1/6
poc_23$black_extreme_R[poc_23$black_extremism_rating == "3"] = 1/3
poc_23$black_extreme_R[poc_23$black_extremism_rating == "4"] = 1/2
poc_23$black_extreme_R[poc_23$black_extremism_rating == "5"] = 2/3
poc_23$black_extreme_R[poc_23$black_extremism_rating == "6"] = 5/6
poc_23$black_extreme_R[poc_23$black_extremism_rating == "7 - Extreme"] = 1

#White
poc_23$white_extreme_R[poc_23$white_extremism_rating == "1 - Not at all extreme"] = 0
poc_23$white_extreme_R[poc_23$white_extremism_rating == "2"] = 1/6
poc_23$white_extreme_R[poc_23$white_extremism_rating == "3"] = 1/3
poc_23$white_extreme_R[poc_23$white_extremism_rating == "4"] = 1/2
poc_23$white_extreme_R[poc_23$white_extremism_rating == "5"] = 2/3
poc_23$white_extreme_R[poc_23$white_extremism_rating == "6"] = 5/6
poc_23$white_extreme_R[poc_23$white_extremism_rating == "7 - Extreme"] = 1

#Hispanic
poc_23$hispanic_extreme_R[poc_23$latinx_extremism_rating == "1 - Not at all extreme"] = 0
poc_23$hispanic_extreme_R[poc_23$latinx_extremism_rating == "2"] = 1/6
poc_23$hispanic_extreme_R[poc_23$latinx_extremism_rating == "3"] = 1/3
poc_23$hispanic_extreme_R[poc_23$latinx_extremism_rating == "4"] = 1/2
poc_23$hispanic_extreme_R[poc_23$latinx_extremism_rating == "5"] = 2/3
poc_23$hispanic_extreme_R[poc_23$latinx_extremism_rating == "6"] = 5/6
poc_23$hispanic_extreme_R[poc_23$latinx_extremism_rating == "7 - Extreme"] = 1

#Targets
#1
poc_23$targets_1_recoded[poc_23$targets_1 == "Not at all dangerous"] = 0
poc_23$targets_1_recoded[poc_23$targets_1 == "A little dangerous"] = 0.25
poc_23$targets_1_recoded[poc_23$targets_1 == "Moderately dangerous"] = 0.5
poc_23$targets_1_recoded[poc_23$targets_1 == "Dangerous"] = 0.75
poc_23$targets_1_recoded[poc_23$targets_1 == "Very dangerous"] = 1

#2
poc_23$targets_2_recoded[poc_23$targets_2 == "Not at all dangerous"] = 0
poc_23$targets_2_recoded[poc_23$targets_2 == "A little dangerous"] = 0.25
poc_23$targets_2_recoded[poc_23$targets_2 == "Moderately dangerous"] = 0.5
poc_23$targets_2_recoded[poc_23$targets_2 == "Dangerous"] = 0.75
poc_23$targets_2_recoded[poc_23$targets_2 == "Very dangerous"] = 1

#3
poc_23$targets_3_recoded[poc_23$targets_3 == "Not at all dangerous"] = 0
poc_23$targets_3_recoded[poc_23$targets_3 == "A little dangerous"] = 0.25
poc_23$targets_3_recoded[poc_23$targets_3 == "Moderately dangerous"] = 0.5
poc_23$targets_3_recoded[poc_23$targets_3 == "Dangerous"] = 0.75
poc_23$targets_3_recoded[poc_23$targets_3 == "Very dangerous"] = 1

#4
poc_23$targets_4_recoded[poc_23$targets_4 == "Not at all dangerous"] = 0
poc_23$targets_4_recoded[poc_23$targets_4 == "A little dangerous"] = 0.25
poc_23$targets_4_recoded[poc_23$targets_4 == "Moderately dangerous"] = 0.5
poc_23$targets_4_recoded[poc_23$targets_4 == "Dangerous"] = 0.75
poc_23$targets_4_recoded[poc_23$targets_4 == "Very dangerous"] = 1

#5
poc_23$targets_5_recoded[poc_23$targets_5 == "Not at all dangerous"] = 0
poc_23$targets_5_recoded[poc_23$targets_5 == "A little dangerous"] = 0.25
poc_23$targets_5_recoded[poc_23$targets_5 == "Moderately dangerous"] = 0.5
poc_23$targets_5_recoded[poc_23$targets_5 == "Dangerous"] = 0.75
poc_23$targets_5_recoded[poc_23$targets_5 == "Very dangerous"] = 1

#Going to make a series of binary variables for each LEVEL of each attribute
#Party
poc_23$party_farleftdem = as.numeric(str_detect(poc_23$profile, "Far-left Democrat"))
poc_23$party_moderatedem = as.numeric(str_detect(poc_23$profile, "Moderate Democrat"))

poc_23$party_dem = as.numeric(str_detect(poc_23$profile, "Democrat"))
#Get rid of far-left/moderates from the dem variable
poc_23$party_dem[poc_23$party_farleftdem == 1] = 0
poc_23$party_dem[poc_23$party_moderatedem == 1] = 0

poc_23$party_independent = as.numeric(str_detect(poc_23$profile, "Independent"))

poc_23$party_farrightrep = as.numeric(str_detect(poc_23$profile, "Far-right Republican"))
poc_23$party_moderaterep = as.numeric(str_detect(poc_23$profile, "Moderate Republican"))

poc_23$party_rep = as.numeric(str_detect(poc_23$profile, "Republican"))
#Get rid of far-right/moderates from the rep variable
poc_23$party_rep[poc_23$party_farrightrep == 1] = 0
poc_23$party_rep[poc_23$party_moderaterep == 1] = 0

poc_23$party_prof[poc_23$party_farleftdem == 1] = "Far-left Democrat"
poc_23$party_prof[poc_23$party_dem == 1] = "Democrat"
poc_23$party_prof[poc_23$party_moderatedem == 1] = "Moderate Democrat"
poc_23$party_prof[poc_23$party_independent == 1] = "Independent"
poc_23$party_prof[poc_23$party_moderaterep == 1] = "Moderate Republican"
poc_23$party_prof[poc_23$party_rep == 1] = "Republican"
poc_23$party_prof[poc_23$party_farrightrep == 1] = "Far-right Republican"
poc_23$party_prof = factor(poc_23$party_prof, levels = c("Far-left Democrat",
                                               "Democrat",
                                               "Moderate Democrat",
                                               "Independent",
                                               "Moderate Republican",
                                               "Republican",
                                               "Far-right Republican"))

#Party extreme
poc_23$party_ext_prof[poc_23$party_farleftdem == 1] = "Extreme Partisan"
poc_23$party_ext_prof[poc_23$party_farrightrep == 1] = "Extreme Partisan"
poc_23$party_ext_prof[poc_23$party_dem == 1] = "Partisan"
poc_23$party_ext_prof[poc_23$party_rep == 1] = "Partisan"
poc_23$party_ext_prof[poc_23$party_moderatedem == 1] = "Moderate Partisan"
poc_23$party_ext_prof[poc_23$party_moderaterep == 1] = "Moderate Partisan"
poc_23$party_ext_prof[poc_23$party_independent == 1] = "Independent"
poc_23$party_ext_prof = factor(poc_23$party_ext_prof, levels = c("Independent",
                                                                 "Moderate Partisan",
                                                                 "Partisan",
                                                                 "Extreme Partisan"))

#Gender
poc_23$gender_female = as.numeric(str_detect(poc_23$profile, "Female"))
poc_23$gender_male = as.numeric(str_detect(poc_23$profile, "Male"))
poc_23$gender_nonbinary = as.numeric(str_detect(poc_23$profile, "Non-binary"))

poc_23$gender_prof[poc_23$gender_female == 1] = "Female"
poc_23$gender_prof[poc_23$gender_male == 1] = "Male"
poc_23$gender_prof[poc_23$gender_nonbinary == 1] = "Non-binary"
poc_23$gender_prof = factor(poc_23$gender_prof, levels = c("Female", "Male", "Non-binary"))

#Race
poc_23$race_asian = as.numeric(str_detect(poc_23$profile, "Asian American"))
poc_23$race_black = as.numeric(str_detect(poc_23$profile, "Black American"))
poc_23$race_hispanic = as.numeric(str_detect(poc_23$profile, "Hispanic American"))
poc_23$race_white = as.numeric(str_detect(poc_23$profile, "White American"))

poc_23$race_prof[poc_23$race_asian == 1] = "Asian American"

poc_23$race_prof[poc_23$race_black == 1] = "Black American"
poc_23$race_prof[poc_23$race_hispanic == 1 ] = "Hispanic American"
poc_23$race_prof[poc_23$race_white == 1] = "White American"
poc_23$race_prof = factor(poc_23$race_prof, levels = c("Hispanic American",
                                                       "Black American",
                                                       "Asian American",
                                                       "White American"))

#Violence
poc_23$violence_supports = as.numeric(str_detect(poc_23$profile, "Violent actions are often necessary in politics"))
poc_23$violence_opposes = as.numeric(str_detect(poc_23$profile, "Nonviolence is always the best political strategy"))

poc_23$violence[poc_23$violence_supports == 1] = "Supports political violence"
poc_23$violence[poc_23$violence_opposes == 1] = "Opposes political violence"
poc_23$violence = factor(poc_23$violence, levels = c("Opposes political violence",
                                                     "Supports political violence"))
#Compromise
poc_23$compromise_supports = as.numeric(str_detect(poc_23$profile, "Compromise is often necessary in politics"))
poc_23$compromise_opposes = as.numeric(str_detect(poc_23$profile, "Compromise is selling out on your principles"))

poc_23$compromise[poc_23$compromise_supports == 1] = "Supports compromise"
poc_23$compromise[poc_23$compromise_opposes == 1] = "Opposes compromise"
poc_23$compromise = factor(poc_23$compromise, levels = c("Opposes compromise",
                                                         "Supports compromise"))

#Rule of law
poc_23$law_supports = as.numeric(str_detect(poc_23$profile, "Obeying the law is part of what makes America a democacy")) #Just a spelling error here; "democracy" was spelled "democacy"
poc_23$law_opposes = as.numeric(str_detect(poc_23$profile, "It is more important to do what most Americans want than to follow the rules"))

poc_23$law[poc_23$law_supports == 1] = "Supports rule of law"
poc_23$law[poc_23$law_opposes == 1] = "Opposes rule of law"
poc_23$law = factor(poc_23$law, levels = c("Opposes rule of law",
                                           "Supports rule of law"))

#Listening
poc_23$listening_supports = as.numeric(str_detect(poc_23$profile, "It is important to listen both to people you do and do not agree with"))
poc_23$listening_mostlysupports = as.numeric(str_detect(poc_23$profile, "It is almost always most important to listen to other points of view"))
poc_23$listening_opposes = as.numeric(str_detect(poc_23$profile, "With a few exceptions, it’s really only necessary to listen to people you agree with"))

poc_23$listening[poc_23$listening_supports == 1] = "Supports listening"
poc_23$listening[poc_23$listening_mostlysupports == 1] = "Mostly supports listening"
poc_23$listening[poc_23$listening_opposes == 1] = "Opposes listening"
poc_23$listening = factor(poc_23$listening, levels = c("Opposes listening",
                                                       "Mostly supports listening",
                                                       "Supports listening"))
#Civility
poc_23$civil_supports = as.numeric(str_detect(poc_23$profile, "Decent, honest"))
poc_23$civil_opposes = as.numeric(str_detect(poc_23$profile, "Liars, morons"))

poc_23$civil[poc_23$civil_supports == 1] = "Civil"
poc_23$civil[poc_23$civil_opposes == 1] = "Uncivil"
poc_23$civil = factor(poc_23$civil, levels = c("Uncivil",
                                               "Civil"))

#Talks about politics
poc_23$talk_always = as.numeric(str_detect(poc_23$profile, "Almost always"))
poc_23$talk_sometimes = as.numeric(str_detect(poc_23$profile, "Sometimes"))
poc_23$talk_never = as.numeric(str_detect(poc_23$profile, "Almost never"))

poc_23$talk[poc_23$talk_always == 1] = "Always"
poc_23$talk[poc_23$talk_sometimes == 1] = "Sometimes"
poc_23$talk[poc_23$talk_never == 1] = "Never"
poc_23$talk = factor(poc_23$talk, levels = c("Always", "Sometimes", "Never"))

#Abortions, gun regulations, and businesses
poc_23$abortion_position[poc_23$attribute1 == "Views on restricting access to abortion"] = 1
poc_23$abortion_position[poc_23$attribute2 == "Views on restricting access to abortion"] = 2
poc_23$abortion_position[poc_23$attribute3 == "Views on restricting access to abortion"] = 3
poc_23$abortion_position[poc_23$attribute4 == "Views on restricting access to abortion"] = 4
poc_23$abortion_position[poc_23$attribute5 == "Views on restricting access to abortion"] = 5
poc_23$abortion_position[poc_23$attribute6 == "Views on restricting access to abortion"] = 6
poc_23$abortion_position[poc_23$attribute7 == "Views on restricting access to abortion"] = 7
poc_23$abortion_position[poc_23$attribute8 == "Views on restricting access to abortion"] = 8
poc_23$abortion_position[poc_23$attribute9 == "Views on restricting access to abortion"] = 9
poc_23$abortion_position[poc_23$attribute10 == "Views on restricting access to abortion"] = 10
poc_23$abortion_position[poc_23$attribute11 == "Views on restricting access to abortion"] = 11
poc_23$abortion_position[poc_23$attribute12 == "Views on restricting access to abortion"] = 12

poc_23$guns_position[poc_23$attribute1 == "Views on increasing gun regulations"] = 1
poc_23$guns_position[poc_23$attribute2 == "Views on increasing gun regulations"] = 2
poc_23$guns_position[poc_23$attribute3 == "Views on increasing gun regulations"] = 3
poc_23$guns_position[poc_23$attribute4 == "Views on increasing gun regulations"] = 4
poc_23$guns_position[poc_23$attribute5 == "Views on increasing gun regulations"] = 5
poc_23$guns_position[poc_23$attribute6 == "Views on increasing gun regulations"] = 6
poc_23$guns_position[poc_23$attribute7 == "Views on increasing gun regulations"] = 7
poc_23$guns_position[poc_23$attribute8 == "Views on increasing gun regulations"] = 8
poc_23$guns_position[poc_23$attribute9 == "Views on increasing gun regulations"] = 9
poc_23$guns_position[poc_23$attribute10 == "Views on increasing gun regulations"] = 10
poc_23$guns_position[poc_23$attribute11 == "Views on increasing gun regulations"] = 11
poc_23$guns_position[poc_23$attribute12 == "Views on increasing gun regulations"] = 12

poc_23$business_position[poc_23$attribute1 == "Views on reducing regulations on businesses"] = 1
poc_23$business_position[poc_23$attribute2 == "Views on reducing regulations on businesses"] = 2
poc_23$business_position[poc_23$attribute3 == "Views on reducing regulations on businesses"] = 3
poc_23$business_position[poc_23$attribute4 == "Views on reducing regulations on businesses"] = 4
poc_23$business_position[poc_23$attribute5 == "Views on reducing regulations on businesses"] = 5
poc_23$business_position[poc_23$attribute6 == "Views on reducing regulations on businesses"] = 6
poc_23$business_position[poc_23$attribute7 == "Views on reducing regulations on businesses"] = 7
poc_23$business_position[poc_23$attribute8 == "Views on reducing regulations on businesses"] = 8
poc_23$business_position[poc_23$attribute9 == "Views on reducing regulations on businesses"] = 9
poc_23$business_position[poc_23$attribute10 == "Views on reducing regulations on businesses"] = 10
poc_23$business_position[poc_23$attribute11 == "Views on reducing regulations on businesses"] = 11
poc_23$business_position[poc_23$attribute12 == "Views on reducing regulations on businesses"] = 12

poc_23$sopposes = as.numeric(str_count(poc_23$profile, "Strongly opposes")) #3 indicates that all three positions are Strongly opposes
poc_23$abortion_prof[poc_23$sopposes == 3] = "Strongly opposes"
poc_23$guns_prof[poc_23$sopposes == 3] = "Strongly opposes"
poc_23$business_prof[poc_23$sopposes == 3] = "Strongly opposes"

poc_23$opposes = as.numeric(str_count(poc_23$profile, "Opposes")) #3 indicates that all three positions are Opposes
poc_23$abortion_prof[poc_23$opposes == 3] = "Opposes"
poc_23$guns_prof[poc_23$opposes == 3] = "Opposes"
poc_23$business_prof[poc_23$opposes == 3] = "Opposes"

poc_23$ssupports = as.numeric(str_count(poc_23$profile, "Strongly supports")) #3 indicates that all three positions are Strongly supports
poc_23$abortion_prof[poc_23$ssupports == 3] = "Strongly supports"
poc_23$guns_prof[poc_23$ssupports == 3] = "Strongly supports"
poc_23$business_prof[poc_23$ssupports == 3] = "Strongly supports"

poc_23$supports = as.numeric(str_count(poc_23$profile, "Supports")) #3 indicates that all three positions are Supports
poc_23$abortion_prof[poc_23$supports == 3] = "Supports"
poc_23$guns_prof[poc_23$supports == 3] = "Supports"
poc_23$business_prof[poc_23$supports == 3] = "Supports"
 
poc_23$undecided = as.numeric(str_count(poc_23$profile, "Undecided")) #3 indicates that all three positions are Undecideds
poc_23$abortion_prof[poc_23$undecided == 3] = "Undecided"
poc_23$guns_prof[poc_23$undecided == 3] = "Undecided"
poc_23$business_prof[poc_23$undecided ==3] = "Undecided"

#Let's extract the other combinations for attribute levels
poc_23$responsive_temp = str_extract_all(poc_23$profile, "Strongly opposes|Opposes|Undecided|Supports|Strongly supports")
#Clean this up so it can be turned into a factor and manipulated
poc_23$responsive_temp2=paste(poc_23$responsive_temp)
poc_23$responsive_temp3=str_replace(poc_23$responsive_temp2, "c", "")
poc_23$responsive_temp3=str_replace_all(poc_23$responsive_temp3, ",", "")
poc_23$responsive_temp3=str_replace_all(poc_23$responsive_temp2, "\"", "")
poc_23$responsive_temp3=str_replace_all(poc_23$responsive_temp3, "c", "")
poc_23$responsive_temp3=str_replace_all(poc_23$responsive_temp3, "[(]", "")
poc_23$responsive_temp3=str_replace_all(poc_23$responsive_temp3, "[)]", "")
poc_23$responsive_temp3=str_replace_all(poc_23$responsive_temp3, "Undeided", "Undecided")
poc_23$responsive_fields=as.factor(poc_23$responsive_temp3)
poc_23$responsive_temp=NULL
poc_23$responsive_temp2=NULL
poc_23$responsive_temp3=NULL

table(poc_23$profile[poc_23$responsive_fields == "Strongly opposes, Strongly opposes, Strongly opposes"])

#The respondents who didn't complete the conjoint are displaying the profile variable as empty, which leads the str_extract_all to label the responsive_temp variable as character(0); the commands below include code to remove the remaining "harater0"

#Abortion
poc_23$abortion_prof = ifelse(poc_23$abortion_position > poc_23$guns_position &
                                poc_23$abortion_position > poc_23$business_position,
                              word(poc_23$responsive_fields, 3, sep = ", "), poc_23$abortion_prof)

poc_23$abortion_prof = ifelse(poc_23$abortion_position < poc_23$guns_position &
                                poc_23$abortion_position < poc_23$business_position,
                              word(poc_23$responsive_fields, 1, sep = ", "), poc_23$abortion_prof)

poc_23$abortion_prof = ifelse(poc_23$abortion_position > poc_23$guns_position &
                                poc_23$abortion_position < poc_23$business_position,
                              word(poc_23$responsive_fields, 2, sep = ", "), poc_23$abortion_prof)

poc_23$abortion_prof = ifelse(poc_23$abortion_position < poc_23$guns_position &
                                poc_23$abortion_position > poc_23$business_position,
                              word(poc_23$responsive_fields, 2, sep = ", "), poc_23$abortion_prof)

poc_23$abortion_prof = ifelse(poc_23$abortion_prof == "harater0", NA, poc_23$abortion_prof)

poc_23$abortion_prof = factor(poc_23$abortion_prof, levels = c("Strongly opposes", "Opposes",
                                                               "Undecided", "Supports", "Strongly supports"))

#Guns
poc_23$guns_prof=ifelse(poc_23$abortion_position<poc_23$guns_position &
                                 poc_23$guns_position>poc_23$business_position,
                               word(poc_23$responsive_fields, 3, sep=", "), poc_23$guns_prof)

poc_23$guns_prof=ifelse(poc_23$abortion_position>poc_23$guns_position &
                                 poc_23$guns_position<poc_23$business_position,
                               word(poc_23$responsive_fields, 1, sep=", "), poc_23$guns_prof)

poc_23$guns_prof=ifelse(poc_23$abortion_position>poc_23$guns_position &
                                 poc_23$guns_position>poc_23$business_position,
                               word(poc_23$responsive_fields, 2, sep=", "), poc_23$guns_prof)

poc_23$guns_prof=ifelse(poc_23$abortion_position<poc_23$guns_position &
                                 poc_23$guns_position<poc_23$business_position,
                               word(poc_23$responsive_fields, 2, sep=", "), poc_23$guns_prof)

poc_23$guns_prof = ifelse(poc_23$guns_prof == "harater0", NA, poc_23$guns_prof)

poc_23$guns_prof = factor(poc_23$guns_prof, levels = c("Strongly opposes", "Opposes",
                                                       "Undecided", "Supports", "Strongly supports"))

#Business
poc_23$business_prof=ifelse(poc_23$abortion_position<poc_23$business_position &
                                     poc_23$guns_position<poc_23$business_position,
                                   word(poc_23$responsive_fields, 3, sep=", "), poc_23$business_prof)

poc_23$business_prof=ifelse(poc_23$abortion_position>poc_23$business_position &
                                     poc_23$guns_position>poc_23$business_position,
                                   word(poc_23$responsive_fields, 1, sep=", "), poc_23$business_prof)

poc_23$business_prof=ifelse(poc_23$abortion_position>poc_23$business_position &
                                     poc_23$guns_position<poc_23$business_position,
                                   word(poc_23$responsive_fields, 2, sep=", "), poc_23$business_prof)

poc_23$business_prof=ifelse(poc_23$abortion_position<poc_23$business_position &
                                     poc_23$guns_position>poc_23$business_position,
                                   word(poc_23$responsive_fields, 2, sep=", "), poc_23$business_prof)

poc_23$business_prof = ifelse(poc_23$business_prof == "harater0", NA, poc_23$business_prof)

poc_23$business_prof = factor(poc_23$business_prof, levels = c("Strongly opposes", "Opposes",
                                                               "Undecided", "Supports", "Strongly supports"))

#Last step - let's remove observations from the dataset if the profile was missing- this means they weren't shown anything
poc_23$profile = ifelse(poc_23$profile == "", NA, poc_23$profile)
poc_23 = poc_23[!is.na(poc_23$profile),]

#Party_group
poc_23$party_group[poc_23$pid_recoded > 0.5] = "Republican"
poc_23$party_group[poc_23$pid_recoded < 0.5] = "Democrat"
poc_23$party_group[poc_23$pid_recoded == 0.5] = "Independent"
poc_23$party_group = factor(poc_23$party_group, levels = c("Democrat",
                                                           "Independent",
                                                           "Republican"))

#Party Match
poc_23$party_match[poc_23$party_group == "Democrat" & poc_23$party_prof == "Far-left Democrat"] = "Party match"
poc_23$party_match[poc_23$party_group == "Democrat" & poc_23$party_prof == "Democrat"] = "Party match"
poc_23$party_match[poc_23$party_group == "Democrat" & poc_23$party_prof == "Moderate Democrat"] = "Party match"

poc_23$party_match[poc_23$party_group == "Republican" & poc_23$party_prof == "Far-right Republican"] = "Party match"
poc_23$party_match[poc_23$party_group == "Republican" & poc_23$party_prof == "Republican"] = "Party match"
poc_23$party_match[poc_23$party_group == "Republican" & poc_23$party_prof == "Moderate Republican"] = "Party match"

poc_23$party_match[poc_23$party_group == "Independent" & poc_23$party_prof == "Independent"] = "Party match"

poc_23$party_match[poc_23$party_group == "Democrat" & poc_23$party_prof == "Far-right Republican"] = "Party mismatch"
poc_23$party_match[poc_23$party_group == "Democrat" & poc_23$party_prof == "Republican"] = "Party mismatch"
poc_23$party_match[poc_23$party_group == "Democrat" & poc_23$party_prof == "Moderate Republican"] = "Party mismatch"
poc_23$party_match[poc_23$party_group == "Democrat" & poc_23$party_prof == "Independent"] = "Party mismatch"

poc_23$party_match[poc_23$party_group == "Republican" & poc_23$party_prof == "Far-left Democrat"] = "Party mismatch"
poc_23$party_match[poc_23$party_group == "Republican" & poc_23$party_prof == "Democrat"] = "Party mismatch"
poc_23$party_match[poc_23$party_group == "Republican" & poc_23$party_prof == "Moderate Democrat"] = "Party mismatch"
poc_23$party_match[poc_23$party_group == "Republican" & poc_23$party_prof == "Independent"] = "Party mismatch"

poc_23$party_match[poc_23$party_group == "Independent" & poc_23$party_prof == "Far-left Democrat"] = "Party mismatch"
poc_23$party_match[poc_23$party_group == "Independent" & poc_23$party_prof == "Democrat"] = "Party mismatch"
poc_23$party_match[poc_23$party_group == "Independent" & poc_23$party_prof == "Moderate Democrat"] = "Party mismatch"

poc_23$party_match[poc_23$party_group == "Independent" & poc_23$party_prof == "Far-right Republican"] = "Party mismatch"
poc_23$party_match[poc_23$party_group == "Independent" & poc_23$party_prof == "Republican"] = "Party mismatch"
poc_23$party_match[poc_23$party_group == "Independent" & poc_23$party_prof == "Moderate Republican"] = "Party mismatch"

poc_23$party_match = factor(poc_23$party_match, levels = c("Party mismatch",
                                                           "Party match"))

#Recoding the dependent variables to numeric variables
poc_23$extreme_R[poc_23$extreme == "1 - Not at all extreme"] = 0
poc_23$extreme_R[poc_23$extreme == "2 - Slightly extreme"] = 0.25
poc_23$extreme_R[poc_23$extreme == "3 - Somewhat extreme"] = 0.5
poc_23$extreme_R[poc_23$extreme == "4 - Extreme"] = 0.75
poc_23$extreme_R[poc_23$extreme == "5 = Very extreme"] = 1

poc_23$vote_R[poc_23$vote == "1 - Definitely would NOT vote"] = 0
poc_23$vote_R[poc_23$vote == "2 - Probably would NOT vote"] = 0.25
poc_23$vote_R[poc_23$vote == "3 - Unsure"] = 0.5
poc_23$vote_R[poc_23$vote == "4 - Probably would vote"] = 0.75
poc_23$vote_R[poc_23$vote == "5 - Definitely would vote"] = 1

poc_23$similar_R[poc_23$similar == "1 - Not at all similar to you"] = 0
poc_23$similar_R[poc_23$similar == "2 - A little similar to you"] = 0.25
poc_23$similar_R[poc_23$similar == "3 - Somewhat similar to you"] = 0.5
poc_23$similar_R[poc_23$similar == "4 - Similar to you"] = 0.75
poc_23$similar_R[poc_23$similar == "5 - Extremely similar to you"] = 1

poc_23$listen_R[poc_23$listen == "1 - Not at all likely to listen"] = 0
poc_23$listen_R[poc_23$listen == "2 - A little likely to listen"] = 0.25
poc_23$listen_R[poc_23$listen == "3 - Somewhat likely to listen"] = 0.5
poc_23$listen_R[poc_23$listen == "4 - Likely to listen"] = 0.75
poc_23$listen_R[poc_23$listen == "5 - Very likely to listen"] = 1

#Initial Analysis
mms_extreme = cj(poc_23, extreme_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_extreme, vline = 0.5, xlab = "Extreme")

mms_vote = cj(poc_23, vote_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_vote, vline = 0.5, xlab = "Vote")

mms_similar = cj(poc_23, similar_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_similar, vline = 0.5, xlab = "Similarity")

mms_listen = cj(poc_23, listen_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_listen, vline = 0.5, xlab = "Listens")

#Initial Analysis by Race

#White
poc_white = subset(poc_23, poc_23$race == "White")

mms_extreme_white = cj(poc_white, extreme_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_extreme_white, vline = 0.5, xlab = "Extreme")

mms_vote_white = cj(poc_white, vote_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_vote_white, vline = 0.5, xlab = "Vote")

mms_similar_white = cj(poc_white, similar_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_similar_white, vline = 0.5, xlab = "Similarity")

mms_listen_white = cj(poc_white, listen_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_listen_white, vline = 0.5, xlab = "Listens")

#black
poc_black = subset(poc_23, poc_23$race == "Black")

mms_extreme_black = cj(poc_black, extreme_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_extreme_black, vline = 0.5, xlab = "Extreme")

mms_vote_black = cj(poc_black, vote_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_vote_black, vline = 0.5, xlab = "Vote")

mms_similar_black = cj(poc_black, similar_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_similar_black, vline = 0.5, xlab = "Similarity")

mms_listen_black = cj(poc_black, listen_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_listen_black, vline = 0.5, xlab = "Listens")

#hispanic
poc_hispanic = subset(poc_23, poc_23$race == "Hispanic")

mms_extreme_hispanic = cj(poc_hispanic, extreme_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_extreme_hispanic, vline = 0.5, xlab = "Extreme")

mms_vote_hispanic = cj(poc_hispanic, vote_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_vote_hispanic, vline = 0.5, xlab = "Vote")

mms_similar_hispanic = cj(poc_hispanic, similar_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_similar_hispanic, vline = 0.5, xlab = "Similarity")

mms_listen_hispanic = cj(poc_hispanic, listen_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_listen_hispanic, vline = 0.5, xlab = "Listens")

#asian
poc_asian = subset(poc_23, poc_23$race == "Asian")

mms_extreme_asian = cj(poc_asian, extreme_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_extreme_asian, vline = 0.5, xlab = "Extreme")

mms_vote_asian = cj(poc_asian, vote_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_vote_asian, vline = 0.5, xlab = "Vote")

mms_similar_asian = cj(poc_asian, similar_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_similar_asian, vline = 0.5, xlab = "Similarity")

mms_listen_asian = cj(poc_asian, listen_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")
plot(mms_listen_asian, vline = 0.5, xlab = "Listens")
