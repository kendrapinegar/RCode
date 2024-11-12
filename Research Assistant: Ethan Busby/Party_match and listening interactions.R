###Listening Interaction Analysis

####POC 2023####
poc_23 = read.csv("POC Cleaned Data.csv")
#party_match interactions
extreme_formula = extreme_R ~ sim_gender + sim_race + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + listening + law
vote_formula = vote_R ~ sim_gender + sim_race + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + listening + law
similar_formula = similar_R ~ sim_gender + sim_race + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + listening + law
listen_formula = listen_R ~ sim_gender + sim_race + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + listening + law

#extreme party_match
cj_extreme_partymatch = cj_anova(poc_23, extreme_formula, by = ~party_match)
cj_extreme_partymatch
#not significant (p=0.1119)

#vote party_match
cj_vote_partymatch = cj_anova(poc_23, vote_formula, by = ~party_match)
cj_vote_partymatch
#significant (p=0.0000)

mms_vote_partymatch = cj(poc_23, vote_formula,
                         id = ~ResponseId, estimate = "mm", by= ~party_match)

diff_vote_partymatch = cj(poc_23, vote_formula,
                          id = ~ResponseId, estimate = "mm_diff", by = ~party_match)

plot(mms_vote_partymatch, xlab = "Vote", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(rbind(mms_vote_partymatch, diff_vote_partymatch), xlab = "Vote", vline = 0) + ggplot2::facet_wrap(~BY, ncol = 3L)

#similar party_match
cj_similar_partymatch = cj_anova(poc_23, similar_formula, by = ~party_match)
cj_similar_partymatch
#not significant (p=0.1082)

#listen party_match
cj_listen_partymatch = cj_anova(poc_23, listen_formula, by = ~party_match)
cj_listen_partymatch
#significant (p=0.0000)

mms_listen_partymatch = cj(poc_23, listen_formula,
                           id = ~ResponseId, estimate = "mm", by = ~party_match)

diff_listen_partymatch = cj(poc_23, listen_formula,
                            id = ~ResponseId, estimate = "mm_diff", by = ~party_match)

plot(mms_listen_partymatch, xlab = "Listen", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(rbind(mms_listen_partymatch, diff_listen_partymatch), xlab = "Listen", vline = 0) + ggplot2::facet_wrap(~BY, ncol = 3L)

#listening interactions
extreme_formula_1 = extreme_R ~ sim_gender + sim_race + party_match + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + law
vote_formula_1 = vote_R ~ sim_gender + sim_race + party_match + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + law
similar_formula_1 = similar_R ~ sim_gender + sim_race + party_match + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + law
listen_formula_1 = listen_R ~ sim_gender + sim_race + party_match + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + law

#extreme
cj_extreme_listen = cj_anova(poc_23, extreme_formula_1, by = ~listening)
cj_extreme_listen
#not significant (p=0.1059)

#vote
cj_vote_listen = cj_anova(poc_23, vote_formula_1, by = ~listening)
cj_vote_listen
#not signficiant (p=0.2508)

#similar
cj_similar_listen = cj_anova(poc_23, similar_formula_1, by = ~listening)
cj_similar_listen
#not significant (p=0.7196)

#listen
cj_listen_listen = cj_anova(poc_23, listen_formula_1, by = ~listening)
cj_listen_listen
#significant (p=0.0005)

mms_listen_listen = cj(poc_23, listen_formula_1,
                       id = ~ResponseId, estimate = "mm", by = ~listening)

diff_listen_listen = cj(poc_23, listen_formula_1,
                        id = ~ResponseId, estimate = "mm_diff", by = ~listening)

plot(mms_listen_listen, xlab = "Listen", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(rbind(mms_listen_listen, diff_listen_listen), xlab = "Listen", vline = 0, ) + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(diff_listen_listen, xlab = "Listen", vline = 0) + ggplot2::facet_wrap(~BY, ncol = 3L)

####Public 2022####
#load the data set in using "Cleaned 2022 Extremism Survey" under the "data" folder on box

#party_match interactions
extreme_formula = extreme_R ~ sim_gender + sim_race + party_ext_prof + violence + compromise + civil + talk + law
vote_formula = vote_R ~ sim_gender + sim_race + party_ext_prof + violence + compromise + civil + talk + law
ft_formula = ft_R ~ sim_gender + sim_race + party_ext_prof + violence + compromise + civil + talk + law
#party_ext_prof, abortion_match, guns_match (variables not currently coded)

#I needed to get rid of missing values
conjoint_PS22 <- subset(conjoint_PS22, conjoint_PS22$gender == "Female" | conjoint_PS22$gender == "Male" | conjoint_PS22$gender == "Non-binary")
conjoint_PS22 <- subset(conjoint_PS22, conjoint_PS22$pid == "Independent" | conjoint_PS22$pid == "Independent leaning Democrat" | conjoint_PS22$pid == "Independent leaning Republican" | conjoint_PS22$pid == "Not so strong Democrat" | conjoint_PS22$pid == "Not so strong Republican" | conjoint_PS22$pid == "Other" | conjoint_PS22$pid == "Strong Democrat" | conjoint_PS22$pid == "Strong Republican")

#this coding is not included in the downloaded dataset, so I pulled it from the cleaning file
#Partisanship
table(conjoint_PS22$pid_recoded)
conjoint_PS22$part_group[conjoint_PS22$pid_recoded<0.5]="Dem"
conjoint_PS22$part_group[conjoint_PS22$pid_recoded==0.5]="Ind"
conjoint_PS22$part_group[conjoint_PS22$pid_recoded>0.5]="Rep"

conjoint_PS22$part_group=factor(conjoint_PS22$part_group, 
                                levels=c("Dem", "Ind",
                                         "Rep"))
table(conjoint_PS22$part_group)

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
table(conjoint_PS22$race)

#Party match
conjoint_PS22$party_match = NA

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

table(conjoint_PS22$party_match)

#####Similarity score: conjoint_PS22
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
conjoint_PS22$party_match = as.factor(conjoint_PS22$party_match)

#Party extreme
conjoint_PS22$party_ext_prof[conjoint_PS22$party_farleftdem == 1] = "Extreme Partisan"
conjoint_PS22$party_ext_prof[conjoint_PS22$party_farrightrep == 1] = "Extreme Partisan"
conjoint_PS22$party_ext_prof[conjoint_PS22$party_dem == 1] = "Partisan"
conjoint_PS22$party_ext_prof[conjoint_PS22$party_rep == 1] = "Partisan"
conjoint_PS22$party_ext_prof[conjoint_PS22$party_moderatedem == 1] = "Moderate Partisan"
conjoint_PS22$party_ext_prof[conjoint_PS22$party_moderaterep == 1] = "Moderate Partisan"
conjoint_PS22$party_ext_prof[conjoint_PS22$party_independent == 1] = "Independent"
conjoint_PS22$party_ext_prof = factor(conjoint_PS22$party_ext_prof, levels = c("Independent",
                                                                 "Moderate Partisan",
                                                                 "Partisan",
                                                                 "Extreme Partisan"))


#extreme party_match
cj_extreme_partymatch = cj_anova(conjoint_PS22, extreme_formula, by = ~party_match)
cj_extreme_partymatch
#not significant (p=0.801)

#vote party_match
cj_vote_partymatch = cj_anova(conjoint_PS22, vote_formula, by = ~party_match)
cj_vote_partymatch
#significant (p=0.0000)

mms_vote_partymatch = cj(conjoint_PS22, vote_formula, id = ~ResponseId, estimate = "mm", by = ~party_match)

diff_vote_partymatch = cj(conjoint_PS22, vote_formula, id = ~ResponseId, estimate = "mm_diff", by = ~party_match)

plot(mms_vote_partymatch, vline = 0.5, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(rbind(mms_vote_partymatch, diff_vote_partymatch), vline = 0, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 3L)

#ft party_match
cj_ft_partymatch = cj_anova(conjoint_PS22, ft_formula, by = ~party_match)
cj_ft_partymatch
#significant (p=0.000)

mms_ft_partymatch = cj(conjoint_PS22, ft_formula, id = ~ResponseId, estimate = "mm", by = ~party_match)

diff_ft_partymatch = cj(conjoint_PS22, ft_formula, id = ~ResponseId, estimate = "mm_diff", by = ~party_match)

plot(mms_ft_partymatch, vline = 0.5, xlab = "FT Rating") + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(rbind(mms_ft_partymatch, diff_ft_partymatch), vline = 0, xlab = "FT Rating") + ggplot2::facet_wrap(~BY, ncol = 3L)

#compromise interactions (in place of listening)
extreme_formula_compromise = extreme_R ~ sim_gender + sim_race + party_match + party_ext_prof + violence + civil + talk + law
vote_formula_compromise = vote_R ~ sim_gender + sim_race + party_match + party_ext_prof + violence + civil + talk + law
ft_formula_compromise = ft_R ~ sim_gender + sim_race + party_match + party_ext_prof + violence + civil + talk + law

#extreme
cj_extreme_compromise = cj_anova(conjoint_PS22, extreme_formula_compromise, by = ~compromise)
cj_extreme_compromise
#not significant (p=0.1145)

#vote
cj_vote_compromise = cj_anova(conjoint_PS22, vote_formula_compromise, by = ~compromise)
cj_vote_compromise
#significant (p=0.03544)

mms_vote_compromise = cj(conjoint_PS22, vote_formula_compromise, id = ~ResponseId, estimate = "mm", by = ~compromise)

diff_vote_compromise = cj(conjoint_PS22, vote_formula_compromise, id = ~ResponseId, estimate = "mm_diff", by = ~compromise)

plot(mms_vote_compromise, vline = 0.5, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(rbind(mms_vote_compromise, diff_vote_compromise), vline = 0, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 3L)

#ft
cj_ft_compromise = cj_anova(conjoint_PS22, ft_formula_compromise, by = ~compromise)
cj_ft_compromise
#not significant (p=0.1204)

#civil interactions (in place of listening)
extreme_formula_civil = extreme_R ~ sim_gender + sim_race + party_match + party_ext_prof + violence + compromise + talk + law
vote_formula_civil = vote_R ~ sim_gender + sim_race + party_match + party_ext_prof + violence + compromise + talk + law
ft_formula_civil = ft_R ~ sim_gender + sim_race + party_match + party_ext_prof + violence + compromise + talk + law

#extreme
cj_extreme_civil = cj_anova(conjoint_PS22, extreme_formula_civil, by = ~civil)
cj_extreme_civil
#significant (p=0.000)

mms_extreme_civil = cj(conjoint_PS22, extreme_formula_civil, id = ~ResponseId, estimate = "mm", by = ~civil)

diff_extreme_civil = cj(conjoint_PS22, extreme_formula_civil, id = ~ResponseId, estimate = "mm_diff", by = ~civil)

plot(mms_extreme_civil, vline = 0.5, xlab = "Extreme") + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(rbind(mms_extreme_civil, diff_extreme_civil), vline = 0, xlab = "Extreme") + ggplot2::facet_wrap(~BY, ncol = 3L)

#vote
cj_vote_civil = cj_anova(conjoint_PS22, vote_formula_civil, by = ~civil)
cj_vote_civil
#significant (p=0.000)

mms_vote_civil = cj(conjoint_PS22, vote_formula_civil, id = ~ResponseId, estimate = "mm", by = ~civil)

diff_vote_civil = cj(conjoint_PS22, vote_formula_civil, id = ~ResponseId, estimate = "mm_diff", by = ~civil)

plot(mms_vote_civil, vline = 0.5, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(rbind(mms_vote_civil, diff_vote_civil), vline = 0, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 3L)

#ft
cj_ft_civil = cj_anova(conjoint_PS22, ft_formula_civil, by = ~civil)
cj_ft_civil
#significant (p=0.0000)

mms_ft_civil = cj(conjoint_PS22, ft_formula_civil, id = ~ResponseId, estimate = "mm", by = ~civil)

diff_ft_civil = cj(conjoint_PS22, ft_formula_civil, id = ~ResponseId, estimate = "mm_diff", by = ~civil)

plot(mms_ft_civil, vline = 0.5, xlab = "FT Rating") + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(rbind(mms_ft_civil, diff_ft_civil), vline = 0, xlab = "FT Rating") + ggplot2::facet_wrap(~BY, ncol = 3L)

