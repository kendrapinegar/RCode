###Coding and Analysis for Similarity Counts, Extremism Counts, Results by Race, Quality Check, Etc.

poc_23 = read.csv("POC Cleaned Data.csv")

#####Creating the Sim Score Variables#####
#Gender
poc_23$sim_gender[poc_23$gender == "Female" & poc_23$gender_prof == "Female"] = "Gender match"
poc_23$sim_gender[poc_23$gender == "Male" & poc_23$gender_prof == "Male"] = "Gender match"
poc_23$sim_gender[poc_23$gender == "Non-binary" & poc_23$gender_prof == "Non-binary"] = "Gender match"
poc_23$sim_gender[poc_23$gender == "Female" & poc_23$gender_prof == "Male"] = "Gender mismatch"
poc_23$sim_gender[poc_23$gender == "Female" & poc_23$gender_prof == "Non-binary"] = "Gender mismatch"
poc_23$sim_gender[poc_23$gender == "Male" & poc_23$gender_prof == "Non-binary"] = "Gender mismatch"
poc_23$sim_gender[poc_23$gender == "Male" & poc_23$gender_prof == "Female"] = "Gender mismatch"
poc_23$sim_gender[poc_23$gender == "Non-binary" & poc_23$gender_prof == "Female"] = "Gender mismatch"
poc_23$sim_gender[poc_23$gender == "Non-binary" & poc_23$gender_prof == "Male"] = "Gender mismatch"

#Race
poc_23$sim_race[poc_23$race == "Asian" & poc_23$race_prof == "Asian American"] = "Race match"
poc_23$sim_race[poc_23$race == "Black" & poc_23$race_prof == "Black American"] = "Race match"
poc_23$sim_race[poc_23$race == "Hispanic" & poc_23$race_prof == "Hispanic American"] = "Race match"
poc_23$sim_race[poc_23$race == "White" & poc_23$race_prof == "White American"] = "Race match"
poc_23$sim_race[poc_23$race == "Asian" & poc_23$race_prof == "Black American"] = "Race mismatch"
poc_23$sim_race[poc_23$race == "Asian" & poc_23$race_prof == "Hispanic American"] = "Race mismatch"
poc_23$sim_race[poc_23$race == "Asian" & poc_23$race_prof == "White American"] = "Race mismatch"
poc_23$sim_race[poc_23$race == "Black" & poc_23$race_prof == "Asian American"] = "Race mismatch"
poc_23$sim_race[poc_23$race == "Black" & poc_23$race_prof == "Hispanic American"] = "Race mismatch"
poc_23$sim_race[poc_23$race == "Black" & poc_23$race_prof == "White American"] = "Race mismatch"
poc_23$sim_race[poc_23$race == "Hispanic" & poc_23$race_prof == "Asian American"] = "Race mismatch"
poc_23$sim_race[poc_23$race == "Hispanic" & poc_23$race_prof == "Black American"] = "Race mismatch"
poc_23$sim_race[poc_23$race == "Hispanic" & poc_23$race_prof == "White American"] = "Race mismatch"
poc_23$sim_race[poc_23$race == "White" & poc_23$race_prof == "Asian American"] = "Race mismatch"
poc_23$sim_race[poc_23$race == "White" & poc_23$race_prof == "Black American"] = "Race mismatch"
poc_23$sim_race[poc_23$race == "White" & poc_23$race_prof == "Hispanic American"] = "Race mismatch"
poc_23$sim_race[poc_23$race == "Other" & poc_23$race_prof == "Asian American"] = "Race mismatch"
poc_23$sim_race[poc_23$race == "Other" & poc_23$race_prof == "Black American"] = "Race mismatch"
poc_23$sim_race[poc_23$race == "Other" & poc_23$race_prof == "Hispanic American"] = "Race mismatch"
poc_23$sim_race[poc_23$race == "Other" & poc_23$race_prof == "White American"] = "Race mismatch"

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
#Abortion issue match
poc_23$abortion_match[poc_23$abortion_recoded == 0 & poc_23$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_23$abortion_match[poc_23$abortion_recoded == 1/6 & poc_23$abortion_prof == "Opposes"] = "Abortion issue match"
poc_23$abortion_match[poc_23$abortion_recoded == 1/3 & poc_23$abortion_prof == "Opposes"] = "Abortion issue match"
poc_23$abortion_match[poc_23$abortion_recoded == 1/2 & poc_23$abortion_prof == "Undecided"] = "Abortion issue match"
poc_23$abortion_match[poc_23$abortion_recoded == 2/3 & poc_23$abortion_prof == "Supports"] = "Abortion issue match"
poc_23$abortion_match[poc_23$abortion_recoded == 5/6 & poc_23$abortion_prof == "Supports"] = "Abortion issue match"
poc_23$abortion_match[poc_23$abortion_recoded == 1 & poc_23$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_23$abortion_match[poc_23$abortion_recoded == 0 & poc_23$abortion_prof == "Opposes"] = "Abortion issue match"
poc_23$abortion_match[poc_23$abortion_recoded == 0 & poc_23$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 0 & poc_23$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 0 & poc_23$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_23$abortion_match[poc_23$abortion_recoded == 1/6 & poc_23$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_23$abortion_match[poc_23$abortion_recoded == 1/6 & poc_23$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 1/6 & poc_23$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 1/6 & poc_23$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_23$abortion_match[poc_23$abortion_recoded == 1/3 & poc_23$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_23$abortion_match[poc_23$abortion_recoded == 1/3 & poc_23$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 1/3 & poc_23$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 1/3 & poc_23$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_23$abortion_match[poc_23$abortion_recoded == 1/2 & poc_23$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 1/2 & poc_23$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 1/2 & poc_23$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 1/2 & poc_23$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_23$abortion_match[poc_23$abortion_recoded == 2/3 & poc_23$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 2/3 & poc_23$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 2/3 & poc_23$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 2/3 & poc_23$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_23$abortion_match[poc_23$abortion_recoded == 5/6 & poc_23$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 5/6 & poc_23$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 5/6 & poc_23$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 5/6 & poc_23$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_23$abortion_match[poc_23$abortion_recoded == 1 & poc_23$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 1 & poc_23$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 1 & poc_23$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_23$abortion_match[poc_23$abortion_recoded == 1 & poc_23$abortion_prof == "Supports"] = "Abortion issue match"

poc_23$abortion_match = factor(poc_23$abortion_match, levels = c("Abortion issue mismatch",
                                                                 "Abortion issue match"))
#Guns issue match
poc_23$guns_match[poc_23$assaultrifles_recoded == 0 & poc_23$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1/6 & poc_23$guns_prof == "Opposes"] = "Guns issue match"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1/3 & poc_23$guns_prof == "Opposes"] = "Guns issue match"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1/2 & poc_23$guns_prof == "Undecided"] = "Guns issue match"
poc_23$guns_match[poc_23$assaultrifles_recoded == 2/3 & poc_23$guns_prof == "Supports"] = "Guns issue match"
poc_23$guns_match[poc_23$assaultrifles_recoded == 5/6 & poc_23$guns_prof == "Supports"] = "Guns issue match"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1 & poc_23$guns_prof == "Strongly supports"] = "Guns issue match"

poc_23$guns_match[poc_23$assaultrifles_recoded == 0 & poc_23$guns_prof == "Opposes"] = "Guns issue match"
poc_23$guns_match[poc_23$assaultrifles_recoded == 0 & poc_23$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 0 & poc_23$guns_prof == "Supports"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 0 & poc_23$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_23$guns_match[poc_23$assaultrifles_recoded == 1/6 & poc_23$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1/6 & poc_23$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1/6 & poc_23$guns_prof == "Supports"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1/6 & poc_23$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_23$guns_match[poc_23$assaultrifles_recoded == 1/3 & poc_23$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1/3 & poc_23$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1/3 & poc_23$guns_prof == "Supports"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1/3 & poc_23$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_23$guns_match[poc_23$assaultrifles_recoded == 1/2 & poc_23$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1/2 & poc_23$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1/2 & poc_23$guns_prof == "Supports"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1/2 & poc_23$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_23$guns_match[poc_23$assaultrifles_recoded == 2/3 & poc_23$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 2/3 & poc_23$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 2/3 & poc_23$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 2/3 & poc_23$guns_prof == "Strongly supports"] = "Guns issue match"

poc_23$guns_match[poc_23$assaultrifles_recoded == 5/6 & poc_23$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 5/6 & poc_23$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 5/6 & poc_23$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 5/6 & poc_23$guns_prof == "Strongly supports"] = "Guns issue match"

poc_23$guns_match[poc_23$assaultrifles_recoded == 1 & poc_23$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1 & poc_23$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1 & poc_23$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_23$guns_match[poc_23$assaultrifles_recoded == 1 & poc_23$guns_prof == "Supports"] = "Guns issue match"

poc_23$guns_match = factor(poc_23$guns_match, levels = c("Guns issue mismatch",
                                                         "Guns issue match"))

#Sim variable
poc_23$sim_score[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch"] = "0 traits shared"

poc_23$sim_score[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch"] = "1 trait shared"
poc_23$sim_score[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party mismatch"] = "1 trait shared"
poc_23$sim_score[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party match"] = "1 trait shared"

poc_23$sim_score[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party mismatch"] = "2 traits shared"
poc_23$sim_score[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match"] = "2 traits shared"
poc_23$sim_score[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party match"] = "2 traits shared"

poc_23$sim_score[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match"] = "3 traits shared"

poc_23$sim_gender = factor(poc_23$sim_gender, levels = c("Gender mismatch",
                                                         "Gender match"))

poc_23$sim_race = factor(poc_23$sim_race, levels = c("Race mismatch",
                                                     "Race match"))

poc_23$sim_score = factor(poc_23$sim_score, levels = c("0 traits shared",
                                                       "1 trait shared",
                                                       "2 traits shared",
                                                       "3 traits shared"))

#0 traits
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue mismatch" & poc_23$guns_match == "Guns issue mismatch"] = "0 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & is.na(poc_23$abortion_match) & is.na(poc_23$guns_match)] = "0 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue mismatch" & is.na(poc_23$guns_match)] = "0 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & is.na(poc_23$abortion_match) & poc_23$guns_match == "Guns issue mismatch"] = "0 traits shared"

#1 trait
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue mismatch" & poc_23$guns_match == "Guns issue mismatch"] = "1 trait shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & is.na(poc_23$abortion_match) & is.na(poc_23$guns_match)] = "1 trait shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue mismatch" & is.na(poc_23$guns_match)] = "1 trait shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & is.na(poc_23$abortion_match) & poc_23$guns_match == "Guns issue mismatch"] = "1 trait shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue mismatch" & poc_23$guns_match == "Guns issue mismatch"] = "1 trait shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party mismatch" & is.na(poc_23$abortion_match) & is.na(poc_23$guns_match)] = "1 trait shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue mismatch" & is.na(poc_23$guns_match)] = "1 trait shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party mismatch" & is.na(poc_23$abortion_match) & poc_23$guns_match == "Guns issue mismatch"] = "1 trait shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue mismatch" & poc_23$guns_match == "Guns issue mismatch"] = "1 trait shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party match" & is.na(poc_23$abortion_match) & is.na(poc_23$guns_match)] = "1 trait shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue mismatch" & is.na(poc_23$guns_match)] = "1 trait shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party match" & is.na(poc_23$abortion_match) & poc_23$guns_match == "Guns issue mismatch"] = "1 trait shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue match" & poc_23$guns_match == "Guns issue mismatch"] = "1 trait shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue match" & is.na(poc_23$guns_match)] = "1 trait shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue mismatch" & poc_23$guns_match == "Guns issue match"] = "1 trait shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & is.na(poc_23$abortion_match) & poc_23$guns_match == "Guns issue match"] = "1 trait shared"

#2 traits
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue mismatch" & poc_23$guns_match == "Guns issue mismatch"] = "2 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party mismatch" & is.na(poc_23$abortion_match) & is.na(poc_23$guns_match)] = "2 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue mismatch" & is.na(poc_23$guns_match)] = "2 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party mismatch" & is.na(poc_23$abortion_match) & poc_23$guns_match == "Guns issue mismatch"] = "2 traits shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue mismatch" & poc_23$guns_match == "Guns issue mismatch"] = "2 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match" & is.na(poc_23$abortion_match) & is.na(poc_23$guns_match)] = "2 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue mismatch" & is.na(poc_23$guns_match)] = "2 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match" & is.na(poc_23$abortion_match) & poc_23$guns_match == "Guns issue mismatch"] = "2 traits shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue match" & poc_23$guns_match == "Guns issue mismatch"] = "2 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue match" & is.na(poc_23$guns_match)] = "2 traits shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue match" & poc_23$guns_match == "Guns issue match"] = "2 traits shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue mismatch" & poc_23$guns_match == "Guns issue match"] = "2 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & is.na(poc_23$abortion_match) & poc_23$guns_match == "Guns issue match"] = "2 traits shared"

#3 traits
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue mismatch" & poc_23$guns_match == "Guns issue mismatch"] = "3 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match" & is.na(poc_23$abortion_match) & is.na(poc_23$guns_match)] = "3 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue mismatch" & is.na(poc_23$guns_match)] = "3 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match" & is.na(poc_23$abortion_match) & poc_23$guns_match == "Guns issue mismatch"] = "3 traits shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue match" & poc_23$guns_match == "Guns issue mismatch"] = "3 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue match" & is.na(poc_23$guns_match)] = "3 traits shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue match" & poc_23$guns_match == "Guns issue match"] = "3 traits shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue match" & poc_23$guns_match == "Guns issue match"] = "3 traits shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue mismatch" & poc_23$guns_match == "Guns issue match"] = "3 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party mismatch" & is.na(poc_23$abortion_match) & poc_23$guns_match == "Guns issue match"] = "3 traits shared"

#4 traits
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue match" & poc_23$guns_match == "Guns issue mismatch"] = "4 traits shared"
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue match" & is.na(poc_23$guns_match)] = "4 traits shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender mismatch" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue match" & poc_23$guns_match == "Guns issue match"] = "4 traits shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race mismatch" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue match" & poc_23$guns_match == "Guns issue match"] = "4 traits shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party mismatch" & poc_23$abortion_match == "Abortion issue match" & poc_23$guns_match == "Guns issue match"] = "4 traits shared"

poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match" & is.na(poc_23$abortion_match) & poc_23$guns_match == "Guns issue match"] = "4 traits shared"

#5 traits
poc_23$sim_score2[poc_23$sim_gender == "Gender match" & poc_23$sim_race == "Race match" & poc_23$party_match == "Party match" & poc_23$abortion_match == "Abortion issue match" & poc_23$guns_match == "Guns issue match"] = "5 traits shared"

poc_23$sim_score2[poc_23$sim_score2 == "3 traits shared"] = "3, 4, or 5 traits shared"
poc_23$sim_score2[poc_23$sim_score2 == "4 traits shared"] = "3, 4, or 5 traits shared"
poc_23$sim_score2[poc_23$sim_score2 == "5 traits shared"] = "3, 4, or 5 traits shared"

poc_23$sim_score2 = factor(poc_23$sim_score2, levels = c("0 traits shared",
                                                         "1 trait shared",
                                                         "2 traits shared",
                                                         "3, 4, or 5 traits shared"))

#refactor the variables for conjoint analysis
poc_23$gender_prof = factor(poc_23$gender_prof, levels = c("Female", "Male", "Non-binary"))

poc_23$race_prof = factor(poc_23$race_prof, levels = c("Hispanic American",
                                                       "Black American",
                                                       "Asian American",
                                                       "White American"))

poc_23$party_prof = factor(poc_23$party_prof, levels = c("Far-left Democrat",
                                                         "Democrat",
                                                         "Moderate Democrat",
                                                         "Independent",
                                                         "Moderate Republican",
                                                         "Republican",
                                                         "Far-right Republican"))

poc_23$party_match = factor(poc_23$party_match, levels = c("Party mismatch",
                                                           "Party match"))

poc_23$violence = factor(poc_23$violence, levels = c("Opposes political violence", "Supports political violence"))

poc_23$compromise = factor(poc_23$compromise, levels = c("Opposes compromise", "Supports compromise"))

poc_23$law = factor(poc_23$law, levels = c("Opposes rule of law", "Supports rule of law"))

poc_23$listening = factor(poc_23$listening, levels = c("Opposes listening", "Mostly supports listening", "Supports listening"))

poc_23$civil = factor(poc_23$civil, levels = c("Uncivil", "Civil"))

poc_23$talk = factor(poc_23$talk, levels = c("Always", "Sometimes", "Never"))

poc_23$party_ext_prof = factor(poc_23$party_ext_prof, levels = c("Independent", "Moderate Partisan", "Partisan", "Extreme Partisan"))

#####Creating the Extremism Count Variable#####
#List: violence_opposes, compromise_opposes, law_opposes, listening_opposes, civil_opposes, talks_always, party_farleftdem, party_farrightrep, abortion, guns, business

poc_23$abortion_prof_extr = 0
poc_23$abortion_prof_extr[poc_23$abortion_prof == "Strongly opposes"] = 1
poc_23$abortion_prof_extr[poc_23$abortion_prof == "Strongly supports"] = 1

poc_23$guns_prof_extr = 0
poc_23$guns_prof_extr[poc_23$guns_prof == "Strongly opposes"] = 1
poc_23$guns_prof_extr[poc_23$guns_prof == "Strongly supports"] = 1

poc_23$business_prof_extr = 0
poc_23$business_prof_extr[poc_23$business_prof == "Strongly opposes"] = 1
poc_23$business_prof_extr[poc_23$business_prof == "Strongly supports"] = 1

poc_23$violence_supports = as.numeric(poc_23$violence_supports)
poc_23$compromise_opposes = as.numeric(poc_23$compromise_opposes)
poc_23$law_opposes = as.numeric(poc_23$law_opposes)
poc_23$listening_opposes = as.numeric(poc_23$listening_opposes)
poc_23$civil_opposes = as.numeric(poc_23$civil_opposes)
poc_23$talk_always = as.numeric(poc_23$talk_always)
poc_23$party_farleftdem = as.numeric(poc_23$party_farleftdem)
poc_23$party_farrightrep = as.numeric(poc_23$party_farrightrep)

poc_23$count = poc_23$violence_supports + poc_23$compromise_opposes + poc_23$law_opposes + poc_23$listening_opposes + poc_23$civil_opposes + poc_23$talk_always + poc_23$party_farleftdem + poc_23$party_farrightrep + poc_23$abortion_prof_extr + poc_23$guns_prof_extr + poc_23$business_prof_extr

poc_23$count[poc_23$count == 0] = 1
poc_23$count[poc_23$count == 10] = 8
poc_23$count[poc_23$count == 9] = 8
table(poc_23$count)
poc_23$count = as.factor(poc_23$count)

#Conjoint analysis with extreme count
library(cregg)
#Extreme
mms_extreme_count = cj(poc_23, extreme_R ~ count + violence + compromise + civil + law + talk + listening + party_prof + race_prof + gender_prof, id = ~ResponseId, estimate = "mm")
plot(mms_extreme_count, vline = 0.5, xlab = "Extreme")

mms_extreme_count_only = mms_extreme_count[mms_extreme_count$feature == "count",]
plot(mms_extreme_count_only, vline = 0.5, xlab = "Extreme")

#Vote
mms_vote_count = cj(poc_23, vote_R ~ count + violence + compromise + civil + law + talk + listening + party_prof + race_prof + gender_prof, id = ~ResponseId, estimate = "mm")
plot(mms_vote_count, vline = 0.5, xlab = "Vote")

mms_vote_count_only = mms_vote_count[mms_vote_count$feature == "count",]
plot(mms_vote_count_only, vline = 0.5, xlab = "Vote")

#Similar
mms_similar_count = cj(poc_23, similar_R ~ count + violence + compromise + civil + law + talk + listening + party_prof + race_prof + gender_prof, id = ~ResponseId, estimate = "mm")
plot(mms_similar_count, vline = 0.5, xlab = "Similarity")

mms_similar_count_only = mms_similar_count[mms_similar_count$feature == "count",]
plot(mms_similar_count_only, vline = 0.5, xlab = "Similarity")

#Listen
mms_listen_count = cj(poc_23, listen_R ~ count + violence + compromise + civil + law + talk + listening + party_prof + race_prof + gender_prof, id = ~ResponseId, estimate = "mm")

plot(mms_listen_count, vline = 0.5, xlab = "Listen")

mms_listen_count_only = mms_listen_count[mms_listen_count$feature == "count",]
plot(mms_listen_count_only, vline = 0.5, xlab = "Listen")

#Sim_score conjoint analysis
#extreme
mms_simscore_extreme = cj(poc_23, extreme_R ~ sim_score + party_ext_prof + violence + compromise + civil + talk + listening + law, id = ~ResponseId, estimate = "mm")

plot(mms_simscore_extreme, vline = 0.5, xlab = "Extreme")

mms_simscore_all_extreme = cj(poc_23, extreme_R ~ sim_score2 + party_ext_prof + violence + compromise + civil + talk + listening + law, id = ~ResponseId, estimate = "mm")

plot(mms_simscore_all_extreme, vline = 0.5, xlab = "Extreme")

mms_sim_extreme = cj(poc_23, extreme_R ~ sim_gender + sim_race + party_match + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + listening + law, id = ~ResponseId, estimate = "mm")

plot(mms_sim_extreme, vline = 0.5, xlab = "Extreme")

#vote
mms_simscore_vote = cj(poc_23, vote_R ~ sim_score + party_ext_prof + violence + compromise + civil + talk + listening + law, id = ~ResponseId, estimate = "mm")

plot(mms_simscore_vote, vline = 0.5, xlab = "Vote")

mms_simscore_all_vote = cj(poc_23, vote_R ~ sim_score2 + party_ext_prof + violence + compromise + civil + talk + listening + law, id = ~ResponseId, estimate = "mm")

plot(mms_simscore_all_vote, vline = 0.5, xlab = "Vote")

mms_sim_vote = cj(poc_23, vote_R ~ sim_gender + sim_race + party_match + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + listening + law, id = ~ResponseId, estimate = "mm")

plot(mms_sim_vote, vline = 0.5, xlab = "Vote")

#similar
mms_simscore_similar = cj(poc_23, similar_R ~ sim_score + party_ext_prof + violence + compromise + civil + talk + listening + law, id = ~ResponseId, estimate = "mm")

plot(mms_simscore_similar, vline = 0.5, xlab = "Similarity")

mms_simscore_all_similar = cj(poc_23, similar_R ~ sim_score2 + party_ext_prof + violence + compromise + civil + talk + listening + law, id = ~ResponseId, estimate = "mm")

plot(mms_simscore_all_similar, vline = 0.5, xlab = "Similarity")

mms_sim_similar = cj(poc_23, similar_R ~ sim_gender + sim_race + party_match + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + listening + law, id = ~ResponseId, estimate = "mm")

plot(mms_sim_similar, vline = 0.5, xlab = "Similarity")

#listen
mms_simscore_listen = cj(poc_23, listen_R ~ sim_score + party_ext_prof + violence + compromise + civil + talk + listening + law, id = ~ResponseId, estimate = "mm")

plot(mms_simscore_listen, vline = 0.5, xlab = "Listen")

mms_simscore_all_listening = cj(poc_23, listen_R ~ sim_score2 + party_ext_prof + violence + compromise + civil + talk + listening + law, id = ~ResponseId, estimate = "mm")

plot(mms_simscore_all_listening, vline = 0.5, xlab = "Listening")

mms_sim_listening = cj(poc_23, listen_R ~ sim_gender + sim_race + party_match + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + listening + law, id = ~ResponseId, estimate = "mm")

plot(mms_sim_listening, vline = 0.5, xlab = "Listening")

#####Sim Scores#####

#party_match interactions
extreme_formula = extreme_R ~ sim_gender + sim_race + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + listening + law
vote_formula = vote_R ~ sim_gender + sim_race + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + listening + law
similar_formula = similar_R ~ sim_gender + sim_race + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + listening + law
listen_formula = listen_R ~ sim_gender + sim_race + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + listening + law

#extreme party_match
cj_extreme_partymatch = cj_anova(poc_23, extreme_formula, by = ~party_match)
cj_extreme_partymatch
#significant (p=0.0245)

mms_extreme_partymatch = cj(poc_23, extreme_formula, id = ~ResponseId, estimate = "mm", by = ~party_match)

diff_extreme_partymatch = cj(poc_23, extreme_formula, id = ~ResponseId, estimate = "mm_diff", by = ~party_match)

plot(mms_extreme_partymatch, vline = 0.5, xlab = "Extreme") + ggplot2::facet_wrap(~BY, ncol = 3L)

#vote party_match
cj_vote_partymatch = cj_anova(poc_23, vote_formula, by = ~party_match)
cj_vote_partymatch
#significant (p=0.0006)

mms_vote_partymatch = cj(poc_23, vote_formula,
                             id = ~ResponseId, estimate = "mm", by= ~party_match)

diff_vote_partymatch = cj(poc_23, vote_formula,
                              id = ~ResponseId, estimate = "mm_diff", by = ~party_match)

plot(mms_vote_partymatch, xlab = "Vote", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)

#similar party_match
cj_similar_partymatch = cj_anova(poc_23, similar_formula, by = ~party_match)
cj_similar_partymatch
#not significant (p=0.3981)

#listen party_match
cj_listen_partymatch = cj_anova(poc_23, listen_formula, by = ~party_match)
cj_listen_partymatch
#significant (p=0.0004)

mms_listen_partymatch = cj(poc_23, listen_formula,
                           id = ~ResponseId, estimate = "mm", by = ~party_match)

diff_listen_partymatch = cj(poc_23, listen_formula,
                            id = ~ResponseId, estimate = "mm_diff", by = ~party_match)

plot(mms_listen_partymatch, xlab = "Listen", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)

#listening interactions
extreme_formula_1 = extreme_R ~ sim_gender + sim_race + party_match + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + law
vote_formula_1 = vote_R ~ sim_gender + sim_race + party_match + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + law
similar_formula_1 = similar_R ~ sim_gender + sim_race + party_match + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + law
listen_formula_1 = listen_R ~ sim_gender + sim_race + party_match + abortion_match + guns_match + party_ext_prof + violence + compromise + civil + talk + law

#extreme listening
cj_extreme_listen = cj_anova(poc_23, extreme_formula_1, by = ~listening)
cj_extreme_listen
#not significant (p=0.3128)

#vote listening
cj_vote_listen = cj_anova(poc_23, vote_formula_1, by = ~listening)
cj_vote_listen
#not significant (p=0.7954)

#similar listening
cj_similar_listen = cj_anova(poc_23, similar_formula_1, by = ~listening)
cj_similar_listen
#not significant (p=0.4485)

#listen listening
cj_listen_listen = cj_anova(poc_23, listen_formula_1, by = ~listening)
cj_listen_listen
#significant (p=0.0359)

mms_listen_listen = cj(poc_23, listen_formula_1,
                       id = ~ResponseId, estimate = "mm", by = ~listening)

diff_listen_listen = cj(poc_23, listen_formula_1,
                        id = ~ResponseId, estimate = "mm_diff", by = ~listening)

plot(mms_listen_listen, xlab = "Listen", vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)

###Compliance Analysis
# - Duration..in..seconds.
# - Q_RecaptchaScore
# - attention_check1 (strongly agree, strongly disagree)
# - open_ext_time_First.Click, open_ext_time_Last.Click
# - conjoint1_time_First.Click, conjoint1_time_Last.Click
# - conjoint2_time_First.Click, conjoint2_time_Last.Click
# - conjoint3_time_First.Click, conjoint3_time_Last.Click
# - conjoint4_time_First.Click, conjoint4_time_Last.Click
# - what is Time?

#####Sim_score Analysis by Race: Black#####
poc_black = subset(poc_23, poc_23$race == "Black")

#Gender
poc_black$sim_gender[poc_black$gender == "Female" & poc_black$gender_prof == "Female"] = "Gender match"
poc_black$sim_gender[poc_black$gender == "Male" & poc_black$gender_prof == "Male"] = "Gender match"
poc_black$sim_gender[poc_black$gender == "Non-binary" & poc_black$gender_prof == "Non-binary"] = "Gender match"
poc_black$sim_gender[poc_black$gender == "Female" & poc_black$gender_prof == "Male"] = "Gender mismatch"
poc_black$sim_gender[poc_black$gender == "Female" & poc_black$gender_prof == "Non-binary"] = "Gender mismatch"
poc_black$sim_gender[poc_black$gender == "Male" & poc_black$gender_prof == "Non-binary"] = "Gender mismatch"
poc_black$sim_gender[poc_black$gender == "Male" & poc_black$gender_prof == "Female"] = "Gender mismatch"
poc_black$sim_gender[poc_black$gender == "Non-binary" & poc_black$gender_prof == "Female"] = "Gender mismatch"
poc_black$sim_gender[poc_black$gender == "Non-binary" & poc_black$gender_prof == "Male"] = "Gender mismatch"

poc_black$sim_gender = factor(poc_black$sim_gender, levels = c("Gender mismatch",
                                                               "Gender match"))

#Race
poc_black$sim_race[poc_black$race == "Asian" & poc_black$race_prof == "Asian American"] = "Race match"
poc_black$sim_race[poc_black$race == "Black" & poc_black$race_prof == "Black American"] = "Race match"
poc_black$sim_race[poc_black$race == "Hispanic" & poc_black$race_prof == "Hispanic American"] = "Race match"
poc_black$sim_race[poc_black$race == "White" & poc_black$race_prof == "White American"] = "Race match"
poc_black$sim_race[poc_black$race == "Asian" & poc_black$race_prof == "Black American"] = "Race mismatch"
poc_black$sim_race[poc_black$race == "Asian" & poc_black$race_prof == "Hispanic American"] = "Race mismatch"
poc_black$sim_race[poc_black$race == "Asian" & poc_black$race_prof == "White American"] = "Race mismatch"
poc_black$sim_race[poc_black$race == "Black" & poc_black$race_prof == "Asian American"] = "Race mismatch"
poc_black$sim_race[poc_black$race == "Black" & poc_black$race_prof == "Hispanic American"] = "Race mismatch"
poc_black$sim_race[poc_black$race == "Black" & poc_black$race_prof == "White American"] = "Race mismatch"
poc_black$sim_race[poc_black$race == "Hispanic" & poc_black$race_prof == "Asian American"] = "Race mismatch"
poc_black$sim_race[poc_black$race == "Hispanic" & poc_black$race_prof == "Black American"] = "Race mismatch"
poc_black$sim_race[poc_black$race == "Hispanic" & poc_black$race_prof == "White American"] = "Race mismatch"
poc_black$sim_race[poc_black$race == "White" & poc_black$race_prof == "Asian American"] = "Race mismatch"
poc_black$sim_race[poc_black$race == "White" & poc_black$race_prof == "Black American"] = "Race mismatch"
poc_black$sim_race[poc_black$race == "White" & poc_black$race_prof == "Hispanic American"] = "Race mismatch"
poc_black$sim_race[poc_black$race == "Other" & poc_black$race_prof == "Asian American"] = "Race mismatch"
poc_black$sim_race[poc_black$race == "Other" & poc_black$race_prof == "Black American"] = "Race mismatch"
poc_black$sim_race[poc_black$race == "Other" & poc_black$race_prof == "Hispanic American"] = "Race mismatch"
poc_black$sim_race[poc_black$race == "Other" & poc_black$race_prof == "White American"] = "Race mismatch"


poc_black$sim_race = factor(poc_black$sim_race, levels = c("Race mismatch",
                                                         "Race match"))

#Party_group
poc_black$party_group[poc_black$pid_recoded > 0.5] = "Republican"
poc_black$party_group[poc_black$pid_recoded < 0.5] = "Democrat"
poc_black$party_group[poc_black$pid_recoded == 0.5] = "Independent"
poc_black$party_group = factor(poc_black$party_group, levels = c("Democrat",
                                                           "Independent",
                                                           "Republican"))

#Party Match
poc_black$party_match[poc_black$party_group == "Democrat" & poc_black$party_prof == "Far-left Democrat"] = "Party match"
poc_black$party_match[poc_black$party_group == "Democrat" & poc_black$party_prof == "Democrat"] = "Party match"
poc_black$party_match[poc_black$party_group == "Democrat" & poc_black$party_prof == "Moderate Democrat"] = "Party match"

poc_black$party_match[poc_black$party_group == "Republican" & poc_black$party_prof == "Far-right Republican"] = "Party match"
poc_black$party_match[poc_black$party_group == "Republican" & poc_black$party_prof == "Republican"] = "Party match"
poc_black$party_match[poc_black$party_group == "Republican" & poc_black$party_prof == "Moderate Republican"] = "Party match"

poc_black$party_match[poc_black$party_group == "Independent" & poc_black$party_prof == "Independent"] = "Party match"

poc_black$party_match[poc_black$party_group == "Democrat" & poc_black$party_prof == "Far-right Republican"] = "Party mismatch"
poc_black$party_match[poc_black$party_group == "Democrat" & poc_black$party_prof == "Republican"] = "Party mismatch"
poc_black$party_match[poc_black$party_group == "Democrat" & poc_black$party_prof == "Moderate Republican"] = "Party mismatch"
poc_black$party_match[poc_black$party_group == "Democrat" & poc_black$party_prof == "Independent"] = "Party mismatch"

poc_black$party_match[poc_black$party_group == "Republican" & poc_black$party_prof == "Far-left Democrat"] = "Party mismatch"
poc_black$party_match[poc_black$party_group == "Republican" & poc_black$party_prof == "Democrat"] = "Party mismatch"
poc_black$party_match[poc_black$party_group == "Republican" & poc_black$party_prof == "Moderate Democrat"] = "Party mismatch"
poc_black$party_match[poc_black$party_group == "Republican" & poc_black$party_prof == "Independent"] = "Party mismatch"

poc_black$party_match[poc_black$party_group == "Independent" & poc_black$party_prof == "Far-left Democrat"] = "Party mismatch"
poc_black$party_match[poc_black$party_group == "Independent" & poc_black$party_prof == "Democrat"] = "Party mismatch"
poc_black$party_match[poc_black$party_group == "Independent" & poc_black$party_prof == "Moderate Democrat"] = "Party mismatch"

poc_black$party_match[poc_black$party_group == "Independent" & poc_black$party_prof == "Far-right Republican"] = "Party mismatch"
poc_black$party_match[poc_black$party_group == "Independent" & poc_black$party_prof == "Republican"] = "Party mismatch"
poc_black$party_match[poc_black$party_group == "Independent" & poc_black$party_prof == "Moderate Republican"] = "Party mismatch"

poc_black$party_match = factor(poc_black$party_match, levels = c("Party mismatch",
                                                           "Party match"))
#Abortion issue match
poc_black$abortion_match[poc_black$abortion_recoded == 0 & poc_black$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_black$abortion_match[poc_black$abortion_recoded == 1/6 & poc_black$abortion_prof == "Opposes"] = "Abortion issue match"
poc_black$abortion_match[poc_black$abortion_recoded == 1/3 & poc_black$abortion_prof == "Opposes"] = "Abortion issue match"
poc_black$abortion_match[poc_black$abortion_recoded == 1/2 & poc_black$abortion_prof == "Undecided"] = "Abortion issue match"
poc_black$abortion_match[poc_black$abortion_recoded == 2/3 & poc_black$abortion_prof == "Supports"] = "Abortion issue match"
poc_black$abortion_match[poc_black$abortion_recoded == 5/6 & poc_black$abortion_prof == "Supports"] = "Abortion issue match"
poc_black$abortion_match[poc_black$abortion_recoded == 1 & poc_black$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_black$abortion_match[poc_black$abortion_recoded == 0 & poc_black$abortion_prof == "Opposes"] = "Abortion issue match"
poc_black$abortion_match[poc_black$abortion_recoded == 0 & poc_black$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 0 & poc_black$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 0 & poc_black$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_black$abortion_match[poc_black$abortion_recoded == 1/6 & poc_black$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_black$abortion_match[poc_black$abortion_recoded == 1/6 & poc_black$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 1/6 & poc_black$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 1/6 & poc_black$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_black$abortion_match[poc_black$abortion_recoded == 1/3 & poc_black$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_black$abortion_match[poc_black$abortion_recoded == 1/3 & poc_black$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 1/3 & poc_black$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 1/3 & poc_black$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_black$abortion_match[poc_black$abortion_recoded == 1/2 & poc_black$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 1/2 & poc_black$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 1/2 & poc_black$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 1/2 & poc_black$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_black$abortion_match[poc_black$abortion_recoded == 2/3 & poc_black$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 2/3 & poc_black$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 2/3 & poc_black$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 2/3 & poc_black$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_black$abortion_match[poc_black$abortion_recoded == 5/6 & poc_black$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 5/6 & poc_black$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 5/6 & poc_black$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 5/6 & poc_black$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_black$abortion_match[poc_black$abortion_recoded == 1 & poc_black$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 1 & poc_black$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 1 & poc_black$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_black$abortion_match[poc_black$abortion_recoded == 1 & poc_black$abortion_prof == "Supports"] = "Abortion issue match"

poc_black$abortion_match = factor(poc_black$abortion_match, levels = c("Abortion issue mismatch",
                                                                 "Abortion issue match"))
#Guns issue match
poc_black$guns_match[poc_black$assaultrifles_recoded == 0 & poc_black$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1/6 & poc_black$guns_prof == "Opposes"] = "Guns issue match"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1/3 & poc_black$guns_prof == "Opposes"] = "Guns issue match"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1/2 & poc_black$guns_prof == "Undecided"] = "Guns issue match"
poc_black$guns_match[poc_black$assaultrifles_recoded == 2/3 & poc_black$guns_prof == "Supports"] = "Guns issue match"
poc_black$guns_match[poc_black$assaultrifles_recoded == 5/6 & poc_black$guns_prof == "Supports"] = "Guns issue match"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1 & poc_black$guns_prof == "Strongly supports"] = "Guns issue match"

poc_black$guns_match[poc_black$assaultrifles_recoded == 0 & poc_black$guns_prof == "Opposes"] = "Guns issue match"
poc_black$guns_match[poc_black$assaultrifles_recoded == 0 & poc_black$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 0 & poc_black$guns_prof == "Supports"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 0 & poc_black$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_black$guns_match[poc_black$assaultrifles_recoded == 1/6 & poc_black$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1/6 & poc_black$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1/6 & poc_black$guns_prof == "Supports"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1/6 & poc_black$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_black$guns_match[poc_black$assaultrifles_recoded == 1/3 & poc_black$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1/3 & poc_black$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1/3 & poc_black$guns_prof == "Supports"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1/3 & poc_black$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_black$guns_match[poc_black$assaultrifles_recoded == 1/2 & poc_black$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1/2 & poc_black$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1/2 & poc_black$guns_prof == "Supports"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1/2 & poc_black$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_black$guns_match[poc_black$assaultrifles_recoded == 2/3 & poc_black$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 2/3 & poc_black$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 2/3 & poc_black$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 2/3 & poc_black$guns_prof == "Strongly supports"] = "Guns issue match"

poc_black$guns_match[poc_black$assaultrifles_recoded == 5/6 & poc_black$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 5/6 & poc_black$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 5/6 & poc_black$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 5/6 & poc_black$guns_prof == "Strongly supports"] = "Guns issue match"

poc_black$guns_match[poc_black$assaultrifles_recoded == 1 & poc_black$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1 & poc_black$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1 & poc_black$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_black$guns_match[poc_black$assaultrifles_recoded == 1 & poc_black$guns_prof == "Supports"] = "Guns issue match"

poc_black$guns_match = factor(poc_black$guns_match, levels = c("Guns issue mismatch",
                                                         "Guns issue match"))

poc_black$sim_gender = factor(poc_black$sim_gender, levels = c("Gender mismatch",
                                                         "Gender match"))

poc_black$sim_race = factor(poc_black$sim_race, levels = c("Race mismatch",
                                                     "Race match"))

#black party_match interactions
#extreme party_match
cj_extreme_black_partymatch = cj_anova(poc_black, extreme_formula, by = ~party_match)
cj_extreme_black_partymatch
#not significant (p=0.3834)

#vote party_match
cj_vote_black_partymatch = cj_anova(poc_black, vote_formula, by = ~party_match)
cj_vote_black_partymatch
#not significant (p=0.934)

#similar party_match
cj_similar_black_partymatch = cj_anova(poc_black, similar_formula, by = ~party_match)
cj_similar_black_partymatch
#not significant (p=0.838)

#listen party_match
cj_listen_black_partymatch = cj_anova(poc_black, listen_formula, by = ~party_match)
cj_listen_black_partymatch
#not significant (p=0.5281)

#black listening interactions
#extreme listening
cj_extreme_black_listen = cj_anova(poc_black, extreme_formula, by = ~listening)
cj_extreme_black_listen
#not significant (p=0.5986)

#vote listening
cj_vote_black_listen = cj_anova(poc_black, vote_formula, by = ~listening)
cj_vote_black_listen
#not significant (p=0.3775)

#similar listening
cj_similar_black_listen = cj_anova(poc_black, similar_formula, by = ~listening)
cj_similar_black_listen
#not significant (p=0.5046)

#listen listening
cj_listen_black_listen = cj_anova(poc_black, listen_formula, by = ~listening)
cj_listen_black_listen
#not significant (p=0.3872)

#####Sim_score Analysis by Race: White#####
poc_white = subset(poc_23, poc_23$race == "White")

#Gender
poc_white$sim_gender[poc_white$gender == "Female" & poc_white$gender_prof == "Female"] = "Gender match"
poc_white$sim_gender[poc_white$gender == "Male" & poc_white$gender_prof == "Male"] = "Gender match"
poc_white$sim_gender[poc_white$gender == "Non-binary" & poc_white$gender_prof == "Non-binary"] = "Gender match"
poc_white$sim_gender[poc_white$gender == "Female" & poc_white$gender_prof == "Male"] = "Gender mismatch"
poc_white$sim_gender[poc_white$gender == "Female" & poc_white$gender_prof == "Non-binary"] = "Gender mismatch"
poc_white$sim_gender[poc_white$gender == "Male" & poc_white$gender_prof == "Non-binary"] = "Gender mismatch"
poc_white$sim_gender[poc_white$gender == "Male" & poc_white$gender_prof == "Female"] = "Gender mismatch"
poc_white$sim_gender[poc_white$gender == "Non-binary" & poc_white$gender_prof == "Female"] = "Gender mismatch"
poc_white$sim_gender[poc_white$gender == "Non-binary" & poc_white$gender_prof == "Male"] = "Gender mismatch"

poc_white$sim_gender = factor(poc_white$sim_gender, levels = c("Gender mismatch",
                                                               "Gender match"))

#Race
poc_white$sim_race[poc_white$race == "Asian" & poc_white$race_prof == "Asian American"] = "Race match"
poc_white$sim_race[poc_white$race == "Black" & poc_white$race_prof == "Black American"] = "Race match"
poc_white$sim_race[poc_white$race == "Hispanic" & poc_white$race_prof == "Hispanic American"] = "Race match"
poc_white$sim_race[poc_white$race == "White" & poc_white$race_prof == "White American"] = "Race match"
poc_white$sim_race[poc_white$race == "Asian" & poc_white$race_prof == "Black American"] = "Race mismatch"
poc_white$sim_race[poc_white$race == "Asian" & poc_white$race_prof == "Hispanic American"] = "Race mismatch"
poc_white$sim_race[poc_white$race == "Asian" & poc_white$race_prof == "White American"] = "Race mismatch"
poc_white$sim_race[poc_white$race == "Black" & poc_white$race_prof == "Asian American"] = "Race mismatch"
poc_white$sim_race[poc_white$race == "Black" & poc_white$race_prof == "Hispanic American"] = "Race mismatch"
poc_white$sim_race[poc_white$race == "Black" & poc_white$race_prof == "White American"] = "Race mismatch"
poc_white$sim_race[poc_white$race == "Hispanic" & poc_white$race_prof == "Asian American"] = "Race mismatch"
poc_white$sim_race[poc_white$race == "Hispanic" & poc_white$race_prof == "Black American"] = "Race mismatch"
poc_white$sim_race[poc_white$race == "Hispanic" & poc_white$race_prof == "White American"] = "Race mismatch"
poc_white$sim_race[poc_white$race == "White" & poc_white$race_prof == "Asian American"] = "Race mismatch"
poc_white$sim_race[poc_white$race == "White" & poc_white$race_prof == "Black American"] = "Race mismatch"
poc_white$sim_race[poc_white$race == "White" & poc_white$race_prof == "Hispanic American"] = "Race mismatch"
poc_white$sim_race[poc_white$race == "Other" & poc_white$race_prof == "Asian American"] = "Race mismatch"
poc_white$sim_race[poc_white$race == "Other" & poc_white$race_prof == "Black American"] = "Race mismatch"
poc_white$sim_race[poc_white$race == "Other" & poc_white$race_prof == "Hispanic American"] = "Race mismatch"
poc_white$sim_race[poc_white$race == "Other" & poc_white$race_prof == "White American"] = "Race mismatch"


poc_white$sim_race = factor(poc_white$sim_race, levels = c("Race mismatch",
                                                           "Race match"))

#Party_group
poc_white$party_group[poc_white$pid_recoded > 0.5] = "Republican"
poc_white$party_group[poc_white$pid_recoded < 0.5] = "Democrat"
poc_white$party_group[poc_white$pid_recoded == 0.5] = "Independent"
poc_white$party_group = factor(poc_white$party_group, levels = c("Democrat",
                                                                 "Independent",
                                                                 "Republican"))

#Party Match
poc_white$party_match[poc_white$party_group == "Democrat" & poc_white$party_prof == "Far-left Democrat"] = "Party match"
poc_white$party_match[poc_white$party_group == "Democrat" & poc_white$party_prof == "Democrat"] = "Party match"
poc_white$party_match[poc_white$party_group == "Democrat" & poc_white$party_prof == "Moderate Democrat"] = "Party match"

poc_white$party_match[poc_white$party_group == "Republican" & poc_white$party_prof == "Far-right Republican"] = "Party match"
poc_white$party_match[poc_white$party_group == "Republican" & poc_white$party_prof == "Republican"] = "Party match"
poc_white$party_match[poc_white$party_group == "Republican" & poc_white$party_prof == "Moderate Republican"] = "Party match"

poc_white$party_match[poc_white$party_group == "Independent" & poc_white$party_prof == "Independent"] = "Party match"

poc_white$party_match[poc_white$party_group == "Democrat" & poc_white$party_prof == "Far-right Republican"] = "Party mismatch"
poc_white$party_match[poc_white$party_group == "Democrat" & poc_white$party_prof == "Republican"] = "Party mismatch"
poc_white$party_match[poc_white$party_group == "Democrat" & poc_white$party_prof == "Moderate Republican"] = "Party mismatch"
poc_white$party_match[poc_white$party_group == "Democrat" & poc_white$party_prof == "Independent"] = "Party mismatch"

poc_white$party_match[poc_white$party_group == "Republican" & poc_white$party_prof == "Far-left Democrat"] = "Party mismatch"
poc_white$party_match[poc_white$party_group == "Republican" & poc_white$party_prof == "Democrat"] = "Party mismatch"
poc_white$party_match[poc_white$party_group == "Republican" & poc_white$party_prof == "Moderate Democrat"] = "Party mismatch"
poc_white$party_match[poc_white$party_group == "Republican" & poc_white$party_prof == "Independent"] = "Party mismatch"

poc_white$party_match[poc_white$party_group == "Independent" & poc_white$party_prof == "Far-left Democrat"] = "Party mismatch"
poc_white$party_match[poc_white$party_group == "Independent" & poc_white$party_prof == "Democrat"] = "Party mismatch"
poc_white$party_match[poc_white$party_group == "Independent" & poc_white$party_prof == "Moderate Democrat"] = "Party mismatch"

poc_white$party_match[poc_white$party_group == "Independent" & poc_white$party_prof == "Far-right Republican"] = "Party mismatch"
poc_white$party_match[poc_white$party_group == "Independent" & poc_white$party_prof == "Republican"] = "Party mismatch"
poc_white$party_match[poc_white$party_group == "Independent" & poc_white$party_prof == "Moderate Republican"] = "Party mismatch"

poc_white$party_match = factor(poc_white$party_match, levels = c("Party mismatch",
                                                                 "Party match"))
#Abortion issue match
poc_white$abortion_match[poc_white$abortion_recoded == 0 & poc_white$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_white$abortion_match[poc_white$abortion_recoded == 1/6 & poc_white$abortion_prof == "Opposes"] = "Abortion issue match"
poc_white$abortion_match[poc_white$abortion_recoded == 1/3 & poc_white$abortion_prof == "Opposes"] = "Abortion issue match"
poc_white$abortion_match[poc_white$abortion_recoded == 1/2 & poc_white$abortion_prof == "Undecided"] = "Abortion issue match"
poc_white$abortion_match[poc_white$abortion_recoded == 2/3 & poc_white$abortion_prof == "Supports"] = "Abortion issue match"
poc_white$abortion_match[poc_white$abortion_recoded == 5/6 & poc_white$abortion_prof == "Supports"] = "Abortion issue match"
poc_white$abortion_match[poc_white$abortion_recoded == 1 & poc_white$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_white$abortion_match[poc_white$abortion_recoded == 0 & poc_white$abortion_prof == "Opposes"] = "Abortion issue match"
poc_white$abortion_match[poc_white$abortion_recoded == 0 & poc_white$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 0 & poc_white$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 0 & poc_white$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_white$abortion_match[poc_white$abortion_recoded == 1/6 & poc_white$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_white$abortion_match[poc_white$abortion_recoded == 1/6 & poc_white$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 1/6 & poc_white$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 1/6 & poc_white$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_white$abortion_match[poc_white$abortion_recoded == 1/3 & poc_white$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_white$abortion_match[poc_white$abortion_recoded == 1/3 & poc_white$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 1/3 & poc_white$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 1/3 & poc_white$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_white$abortion_match[poc_white$abortion_recoded == 1/2 & poc_white$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 1/2 & poc_white$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 1/2 & poc_white$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 1/2 & poc_white$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_white$abortion_match[poc_white$abortion_recoded == 2/3 & poc_white$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 2/3 & poc_white$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 2/3 & poc_white$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 2/3 & poc_white$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_white$abortion_match[poc_white$abortion_recoded == 5/6 & poc_white$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 5/6 & poc_white$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 5/6 & poc_white$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 5/6 & poc_white$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_white$abortion_match[poc_white$abortion_recoded == 1 & poc_white$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 1 & poc_white$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 1 & poc_white$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_white$abortion_match[poc_white$abortion_recoded == 1 & poc_white$abortion_prof == "Supports"] = "Abortion issue match"

poc_white$abortion_match = factor(poc_white$abortion_match, levels = c("Abortion issue mismatch",
                                                                       "Abortion issue match"))
#Guns issue match
poc_white$guns_match[poc_white$assaultrifles_recoded == 0 & poc_white$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1/6 & poc_white$guns_prof == "Opposes"] = "Guns issue match"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1/3 & poc_white$guns_prof == "Opposes"] = "Guns issue match"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1/2 & poc_white$guns_prof == "Undecided"] = "Guns issue match"
poc_white$guns_match[poc_white$assaultrifles_recoded == 2/3 & poc_white$guns_prof == "Supports"] = "Guns issue match"
poc_white$guns_match[poc_white$assaultrifles_recoded == 5/6 & poc_white$guns_prof == "Supports"] = "Guns issue match"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1 & poc_white$guns_prof == "Strongly supports"] = "Guns issue match"

poc_white$guns_match[poc_white$assaultrifles_recoded == 0 & poc_white$guns_prof == "Opposes"] = "Guns issue match"
poc_white$guns_match[poc_white$assaultrifles_recoded == 0 & poc_white$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 0 & poc_white$guns_prof == "Supports"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 0 & poc_white$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_white$guns_match[poc_white$assaultrifles_recoded == 1/6 & poc_white$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1/6 & poc_white$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1/6 & poc_white$guns_prof == "Supports"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1/6 & poc_white$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_white$guns_match[poc_white$assaultrifles_recoded == 1/3 & poc_white$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1/3 & poc_white$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1/3 & poc_white$guns_prof == "Supports"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1/3 & poc_white$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_white$guns_match[poc_white$assaultrifles_recoded == 1/2 & poc_white$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1/2 & poc_white$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1/2 & poc_white$guns_prof == "Supports"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1/2 & poc_white$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_white$guns_match[poc_white$assaultrifles_recoded == 2/3 & poc_white$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 2/3 & poc_white$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 2/3 & poc_white$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 2/3 & poc_white$guns_prof == "Strongly supports"] = "Guns issue match"

poc_white$guns_match[poc_white$assaultrifles_recoded == 5/6 & poc_white$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 5/6 & poc_white$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 5/6 & poc_white$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 5/6 & poc_white$guns_prof == "Strongly supports"] = "Guns issue match"

poc_white$guns_match[poc_white$assaultrifles_recoded == 1 & poc_white$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1 & poc_white$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1 & poc_white$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_white$guns_match[poc_white$assaultrifles_recoded == 1 & poc_white$guns_prof == "Supports"] = "Guns issue match"

poc_white$guns_match = factor(poc_white$guns_match, levels = c("Guns issue mismatch",
                                                               "Guns issue match"))

poc_white$sim_gender = factor(poc_white$sim_gender, levels = c("Gender mismatch",
                                                               "Gender match"))

poc_white$sim_race = factor(poc_white$sim_race, levels = c("Race mismatch",
                                                           "Race match"))

#white party_match interactions
#extreme party_match
cj_extreme_white_partymatch = cj_anova(poc_white, extreme_formula, by = ~party_match)
cj_extreme_white_partymatch
#not significant (p=0.8707)

#vote party_match
cj_vote_white_partymatch = cj_anova(poc_white, vote_formula, by = ~party_match)
cj_vote_white_partymatch
#not significant (p=0.469)

#similar party_match
cj_similar_white_partymatch = cj_anova(poc_white, similar_formula, by = ~party_match)
cj_similar_white_partymatch
#not significant (p=0.3573)

#listen party_match
cj_listen_white_partymatch = cj_anova(poc_white, listen_formula, by = ~party_match)
cj_listen_white_partymatch
#not significant (p=0.9316)

#white listening interactions
#extreme listening
cj_extreme_white_listen = cj_anova(poc_white, extreme_formula, by = ~listening)
cj_extreme_white_listen
#not significant (p=0.8855)

#vote listening
cj_vote_white_listen = cj_anova(poc_white, vote_formula, by = ~listening)
cj_vote_white_listen
#significant (p=0.0071)

mms_vote_white_listen = cj(poc_white, vote_formula_1, id = ~ResponseId, estimate = "mm", by = ~listening)

plot(mms_vote_white_listen, vline = 0.5, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 3L)

#similar listening
cj_similar_white_listen = cj_anova(poc_white, similar_formula, by = ~listening)
cj_similar_white_listen
#not significant (p=0.7443)

#listen listening
cj_listen_white_listen = cj_anova(poc_white, listen_formula, by = ~listening)
cj_listen_white_listen
#not significant (p=0.5213)

#####Sim_score Analysis by Race: Hispanic#####
poc_hispanic = subset(poc_23, poc_23$race == "Hispanic")

#Gender
poc_hispanic$sim_gender[poc_hispanic$gender == "Female" & poc_hispanic$gender_prof == "Female"] = "Gender match"
poc_hispanic$sim_gender[poc_hispanic$gender == "Male" & poc_hispanic$gender_prof == "Male"] = "Gender match"
poc_hispanic$sim_gender[poc_hispanic$gender == "Non-binary" & poc_hispanic$gender_prof == "Non-binary"] = "Gender match"
poc_hispanic$sim_gender[poc_hispanic$gender == "Female" & poc_hispanic$gender_prof == "Male"] = "Gender mismatch"
poc_hispanic$sim_gender[poc_hispanic$gender == "Female" & poc_hispanic$gender_prof == "Non-binary"] = "Gender mismatch"
poc_hispanic$sim_gender[poc_hispanic$gender == "Male" & poc_hispanic$gender_prof == "Non-binary"] = "Gender mismatch"
poc_hispanic$sim_gender[poc_hispanic$gender == "Male" & poc_hispanic$gender_prof == "Female"] = "Gender mismatch"
poc_hispanic$sim_gender[poc_hispanic$gender == "Non-binary" & poc_hispanic$gender_prof == "Female"] = "Gender mismatch"
poc_hispanic$sim_gender[poc_hispanic$gender == "Non-binary" & poc_hispanic$gender_prof == "Male"] = "Gender mismatch"

poc_hispanic$sim_gender = factor(poc_hispanic$sim_gender, levels = c("Gender mismatch",
                                                               "Gender match"))

#Race
poc_hispanic$sim_race[poc_hispanic$race == "Asian" & poc_hispanic$race_prof == "Asian American"] = "Race match"
poc_hispanic$sim_race[poc_hispanic$race == "Black" & poc_hispanic$race_prof == "Black American"] = "Race match"
poc_hispanic$sim_race[poc_hispanic$race == "Hispanic" & poc_hispanic$race_prof == "Hispanic American"] = "Race match"
poc_hispanic$sim_race[poc_hispanic$race == "White" & poc_hispanic$race_prof == "White American"] = "Race match"
poc_hispanic$sim_race[poc_hispanic$race == "Asian" & poc_hispanic$race_prof == "Black American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Asian" & poc_hispanic$race_prof == "Hispanic American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Asian" & poc_hispanic$race_prof == "White American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Black" & poc_hispanic$race_prof == "Asian American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Black" & poc_hispanic$race_prof == "Hispanic American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Black" & poc_hispanic$race_prof == "White American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Hispanic" & poc_hispanic$race_prof == "Asian American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Hispanic" & poc_hispanic$race_prof == "Black American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Hispanic" & poc_hispanic$race_prof == "White American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "White" & poc_hispanic$race_prof == "Asian American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "White" & poc_hispanic$race_prof == "Black American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "White" & poc_hispanic$race_prof == "Hispanic American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Other" & poc_hispanic$race_prof == "Asian American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Other" & poc_hispanic$race_prof == "Black American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Other" & poc_hispanic$race_prof == "Hispanic American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Other" & poc_hispanic$race_prof == "White American"] = "Race mismatch"


poc_hispanic$sim_race = factor(poc_hispanic$sim_race, levels = c("Race mismatch",
                                                           "Race match"))

#Party_group
poc_hispanic$party_group[poc_hispanic$pid_recoded > 0.5] = "Republican"
poc_hispanic$party_group[poc_hispanic$pid_recoded < 0.5] = "Democrat"
poc_hispanic$party_group[poc_hispanic$pid_recoded == 0.5] = "Independent"
poc_hispanic$party_group = factor(poc_hispanic$party_group, levels = c("Democrat",
                                                                 "Independent",
                                                                 "Republican"))

#Party Match
poc_hispanic$party_match[poc_hispanic$party_group == "Democrat" & poc_hispanic$party_prof == "Far-left Democrat"] = "Party match"
poc_hispanic$party_match[poc_hispanic$party_group == "Democrat" & poc_hispanic$party_prof == "Democrat"] = "Party match"
poc_hispanic$party_match[poc_hispanic$party_group == "Democrat" & poc_hispanic$party_prof == "Moderate Democrat"] = "Party match"

poc_hispanic$party_match[poc_hispanic$party_group == "Republican" & poc_hispanic$party_prof == "Far-right Republican"] = "Party match"
poc_hispanic$party_match[poc_hispanic$party_group == "Republican" & poc_hispanic$party_prof == "Republican"] = "Party match"
poc_hispanic$party_match[poc_hispanic$party_group == "Republican" & poc_hispanic$party_prof == "Moderate Republican"] = "Party match"

poc_hispanic$party_match[poc_hispanic$party_group == "Independent" & poc_hispanic$party_prof == "Independent"] = "Party match"

poc_hispanic$party_match[poc_hispanic$party_group == "Democrat" & poc_hispanic$party_prof == "Far-right Republican"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Democrat" & poc_hispanic$party_prof == "Republican"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Democrat" & poc_hispanic$party_prof == "Moderate Republican"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Democrat" & poc_hispanic$party_prof == "Independent"] = "Party mismatch"

poc_hispanic$party_match[poc_hispanic$party_group == "Republican" & poc_hispanic$party_prof == "Far-left Democrat"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Republican" & poc_hispanic$party_prof == "Democrat"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Republican" & poc_hispanic$party_prof == "Moderate Democrat"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Republican" & poc_hispanic$party_prof == "Independent"] = "Party mismatch"

poc_hispanic$party_match[poc_hispanic$party_group == "Independent" & poc_hispanic$party_prof == "Far-left Democrat"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Independent" & poc_hispanic$party_prof == "Democrat"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Independent" & poc_hispanic$party_prof == "Moderate Democrat"] = "Party mismatch"

poc_hispanic$party_match[poc_hispanic$party_group == "Independent" & poc_hispanic$party_prof == "Far-right Republican"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Independent" & poc_hispanic$party_prof == "Republican"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Independent" & poc_hispanic$party_prof == "Moderate Republican"] = "Party mismatch"

poc_hispanic$party_match = factor(poc_hispanic$party_match, levels = c("Party mismatch",
                                                                 "Party match"))
#Abortion issue match
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 0 & poc_hispanic$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/6 & poc_hispanic$abortion_prof == "Opposes"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/3 & poc_hispanic$abortion_prof == "Opposes"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/2 & poc_hispanic$abortion_prof == "Undecided"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 2/3 & poc_hispanic$abortion_prof == "Supports"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 5/6 & poc_hispanic$abortion_prof == "Supports"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1 & poc_hispanic$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 0 & poc_hispanic$abortion_prof == "Opposes"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 0 & poc_hispanic$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 0 & poc_hispanic$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 0 & poc_hispanic$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/6 & poc_hispanic$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/6 & poc_hispanic$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/6 & poc_hispanic$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/6 & poc_hispanic$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/3 & poc_hispanic$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/3 & poc_hispanic$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/3 & poc_hispanic$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/3 & poc_hispanic$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/2 & poc_hispanic$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/2 & poc_hispanic$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/2 & poc_hispanic$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/2 & poc_hispanic$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 2/3 & poc_hispanic$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 2/3 & poc_hispanic$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 2/3 & poc_hispanic$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 2/3 & poc_hispanic$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 5/6 & poc_hispanic$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 5/6 & poc_hispanic$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 5/6 & poc_hispanic$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 5/6 & poc_hispanic$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1 & poc_hispanic$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1 & poc_hispanic$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1 & poc_hispanic$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1 & poc_hispanic$abortion_prof == "Supports"] = "Abortion issue match"

poc_hispanic$abortion_match = factor(poc_hispanic$abortion_match, levels = c("Abortion issue mismatch",
                                                                       "Abortion issue match"))
#Guns issue match
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 0 & poc_hispanic$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/6 & poc_hispanic$guns_prof == "Opposes"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/3 & poc_hispanic$guns_prof == "Opposes"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/2 & poc_hispanic$guns_prof == "Undecided"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 2/3 & poc_hispanic$guns_prof == "Supports"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 5/6 & poc_hispanic$guns_prof == "Supports"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1 & poc_hispanic$guns_prof == "Strongly supports"] = "Guns issue match"

poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 0 & poc_hispanic$guns_prof == "Opposes"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 0 & poc_hispanic$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 0 & poc_hispanic$guns_prof == "Supports"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 0 & poc_hispanic$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/6 & poc_hispanic$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/6 & poc_hispanic$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/6 & poc_hispanic$guns_prof == "Supports"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/6 & poc_hispanic$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/3 & poc_hispanic$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/3 & poc_hispanic$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/3 & poc_hispanic$guns_prof == "Supports"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/3 & poc_hispanic$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/2 & poc_hispanic$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/2 & poc_hispanic$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/2 & poc_hispanic$guns_prof == "Supports"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/2 & poc_hispanic$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 2/3 & poc_hispanic$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 2/3 & poc_hispanic$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 2/3 & poc_hispanic$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 2/3 & poc_hispanic$guns_prof == "Strongly supports"] = "Guns issue match"

poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 5/6 & poc_hispanic$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 5/6 & poc_hispanic$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 5/6 & poc_hispanic$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 5/6 & poc_hispanic$guns_prof == "Strongly supports"] = "Guns issue match"

poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1 & poc_hispanic$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1 & poc_hispanic$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1 & poc_hispanic$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1 & poc_hispanic$guns_prof == "Supports"] = "Guns issue match"

poc_hispanic$guns_match = factor(poc_hispanic$guns_match, levels = c("Guns issue mismatch",
                                                               "Guns issue match"))

poc_hispanic$sim_gender = factor(poc_hispanic$sim_gender, levels = c("Gender mismatch",
                                                               "Gender match"))

poc_hispanic$sim_race = factor(poc_hispanic$sim_race, levels = c("Race mismatch",
                                                           "Race match"))

#hispanic party_match interactions
#extreme party_match
cj_extreme_hispanic_partymatch = cj_anova(poc_hispanic, extreme_formula, by = ~party_match)
cj_extreme_hispanic_partymatch
#not significant (p=0.8211)

#vote party_match
cj_vote_hispanic_partymatch = cj_anova(poc_hispanic, vote_formula, by = ~party_match)
cj_vote_hispanic_partymatch
#not significant (p=0.213)

#similar party_match
cj_similar_hispanic_partymatch = cj_anova(poc_hispanic, similar_formula, by = ~party_match)
cj_similar_hispanic_partymatch
#not significant (p=0.1238)

#listen party_match
cj_listen_hispanic_partymatch = cj_anova(poc_hispanic, listen_formula, by = ~party_match)
cj_listen_hispanic_partymatch
#not significant (p=0.2237)

#hispanic listening interactions
#extreme listening
cj_extreme_hispanic_listen = cj_anova(poc_hispanic, extreme_formula, by = ~listening)
cj_extreme_hispanic_listen
#not significant (p=0.5)

#vote listening
cj_vote_hispanic_listen = cj_anova(poc_hispanic, vote_formula, by = ~listening)
cj_vote_hispanic_listen
#not significant (p=0.54)

#similar listening
cj_similar_hispanic_listen = cj_anova(poc_hispanic, similar_formula, by = ~listening)
cj_similar_hispanic_listen
#not significant (p=0.5855)

#listen listening
cj_listen_hispanic_listen = cj_anova(poc_hispanic, listen_formula, by = ~listening)
cj_listen_hispanic_listen
#not significant (p=0.622)
#####Sim_score Analysis by Race: Asian#####
poc_asian = subset(poc_23, poc_23$race == "Asian")

#Gender
poc_asian$sim_gender[poc_asian$gender == "Female" & poc_asian$gender_prof == "Female"] = "Gender match"
poc_asian$sim_gender[poc_asian$gender == "Male" & poc_asian$gender_prof == "Male"] = "Gender match"
poc_asian$sim_gender[poc_asian$gender == "Non-binary" & poc_asian$gender_prof == "Non-binary"] = "Gender match"
poc_asian$sim_gender[poc_asian$gender == "Female" & poc_asian$gender_prof == "Male"] = "Gender mismatch"
poc_asian$sim_gender[poc_asian$gender == "Female" & poc_asian$gender_prof == "Non-binary"] = "Gender mismatch"
poc_asian$sim_gender[poc_asian$gender == "Male" & poc_asian$gender_prof == "Non-binary"] = "Gender mismatch"
poc_asian$sim_gender[poc_asian$gender == "Male" & poc_asian$gender_prof == "Female"] = "Gender mismatch"
poc_asian$sim_gender[poc_asian$gender == "Non-binary" & poc_asian$gender_prof == "Female"] = "Gender mismatch"
poc_asian$sim_gender[poc_asian$gender == "Non-binary" & poc_asian$gender_prof == "Male"] = "Gender mismatch"

poc_asian$sim_gender = factor(poc_asian$sim_gender, levels = c("Gender mismatch",
                                                                     "Gender match"))

#Race
poc_asian$sim_race[poc_asian$race == "Asian" & poc_asian$race_prof == "Asian American"] = "Race match"
poc_asian$sim_race[poc_asian$race == "Black" & poc_asian$race_prof == "Black American"] = "Race match"
poc_asian$sim_race[poc_asian$race == "Hispanic" & poc_asian$race_prof == "Hispanic American"] = "Race match"
poc_asian$sim_race[poc_asian$race == "White" & poc_asian$race_prof == "White American"] = "Race match"
poc_asian$sim_race[poc_asian$race == "Asian" & poc_asian$race_prof == "Black American"] = "Race mismatch"
poc_asian$sim_race[poc_asian$race == "Asian" & poc_asian$race_prof == "Hispanic American"] = "Race mismatch"
poc_asian$sim_race[poc_asian$race == "Asian" & poc_asian$race_prof == "White American"] = "Race mismatch"
poc_asian$sim_race[poc_asian$race == "Black" & poc_asian$race_prof == "Asian American"] = "Race mismatch"
poc_asian$sim_race[poc_asian$race == "Black" & poc_asian$race_prof == "Hispanic American"] = "Race mismatch"
poc_asian$sim_race[poc_asian$race == "Black" & poc_asian$race_prof == "White American"] = "Race mismatch"
poc_asian$sim_race[poc_asian$race == "Hispanic" & poc_asian$race_prof == "Asian American"] = "Race mismatch"
poc_asian$sim_race[poc_asian$race == "Hispanic" & poc_asian$race_prof == "Black American"] = "Race mismatch"
poc_asian$sim_race[poc_asian$race == "Hispanic" & poc_asian$race_prof == "White American"] = "Race mismatch"
poc_asian$sim_race[poc_asian$race == "White" & poc_asian$race_prof == "Asian American"] = "Race mismatch"
poc_asian$sim_race[poc_asian$race == "White" & poc_asian$race_prof == "Black American"] = "Race mismatch"
poc_asian$sim_race[poc_asian$race == "White" & poc_asian$race_prof == "Hispanic American"] = "Race mismatch"
poc_asian$sim_race[poc_asian$race == "Other" & poc_asian$race_prof == "Asian American"] = "Race mismatch"
poc_asian$sim_race[poc_asian$race == "Other" & poc_asian$race_prof == "Black American"] = "Race mismatch"
poc_asian$sim_race[poc_asian$race == "Other" & poc_asian$race_prof == "Hispanic American"] = "Race mismatch"
poc_asian$sim_race[poc_asian$race == "Other" & poc_asian$race_prof == "White American"] = "Race mismatch"


poc_asian$sim_race = factor(poc_asian$sim_race, levels = c("Race mismatch",
                                                                 "Race match"))

#Party_group
poc_asian$party_group[poc_asian$pid_recoded > 0.5] = "Republican"
poc_asian$party_group[poc_asian$pid_recoded < 0.5] = "Democrat"
poc_asian$party_group[poc_asian$pid_recoded == 0.5] = "Independent"
poc_asian$party_group = factor(poc_asian$party_group, levels = c("Democrat",
                                                                       "Independent",
                                                                       "Republican"))

#Party Match
poc_asian$party_match[poc_asian$party_group == "Democrat" & poc_asian$party_prof == "Far-left Democrat"] = "Party match"
poc_asian$party_match[poc_asian$party_group == "Democrat" & poc_asian$party_prof == "Democrat"] = "Party match"
poc_asian$party_match[poc_asian$party_group == "Democrat" & poc_asian$party_prof == "Moderate Democrat"] = "Party match"

poc_asian$party_match[poc_asian$party_group == "Republican" & poc_asian$party_prof == "Far-right Republican"] = "Party match"
poc_asian$party_match[poc_asian$party_group == "Republican" & poc_asian$party_prof == "Republican"] = "Party match"
poc_asian$party_match[poc_asian$party_group == "Republican" & poc_asian$party_prof == "Moderate Republican"] = "Party match"

poc_asian$party_match[poc_asian$party_group == "Independent" & poc_asian$party_prof == "Independent"] = "Party match"

poc_asian$party_match[poc_asian$party_group == "Democrat" & poc_asian$party_prof == "Far-right Republican"] = "Party mismatch"
poc_asian$party_match[poc_asian$party_group == "Democrat" & poc_asian$party_prof == "Republican"] = "Party mismatch"
poc_asian$party_match[poc_asian$party_group == "Democrat" & poc_asian$party_prof == "Moderate Republican"] = "Party mismatch"
poc_asian$party_match[poc_asian$party_group == "Democrat" & poc_asian$party_prof == "Independent"] = "Party mismatch"

poc_asian$party_match[poc_asian$party_group == "Republican" & poc_asian$party_prof == "Far-left Democrat"] = "Party mismatch"
poc_asian$party_match[poc_asian$party_group == "Republican" & poc_asian$party_prof == "Democrat"] = "Party mismatch"
poc_asian$party_match[poc_asian$party_group == "Republican" & poc_asian$party_prof == "Moderate Democrat"] = "Party mismatch"
poc_asian$party_match[poc_asian$party_group == "Republican" & poc_asian$party_prof == "Independent"] = "Party mismatch"

poc_asian$party_match[poc_asian$party_group == "Independent" & poc_asian$party_prof == "Far-left Democrat"] = "Party mismatch"
poc_asian$party_match[poc_asian$party_group == "Independent" & poc_asian$party_prof == "Democrat"] = "Party mismatch"
poc_asian$party_match[poc_asian$party_group == "Independent" & poc_asian$party_prof == "Moderate Democrat"] = "Party mismatch"

poc_asian$party_match[poc_asian$party_group == "Independent" & poc_asian$party_prof == "Far-right Republican"] = "Party mismatch"
poc_asian$party_match[poc_asian$party_group == "Independent" & poc_asian$party_prof == "Republican"] = "Party mismatch"
poc_asian$party_match[poc_asian$party_group == "Independent" & poc_asian$party_prof == "Moderate Republican"] = "Party mismatch"

poc_asian$party_match = factor(poc_asian$party_match, levels = c("Party mismatch",
                                                                       "Party match"))
#Abortion issue match
poc_asian$abortion_match[poc_asian$abortion_recoded == 0 & poc_asian$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1/6 & poc_asian$abortion_prof == "Opposes"] = "Abortion issue match"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1/3 & poc_asian$abortion_prof == "Opposes"] = "Abortion issue match"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1/2 & poc_asian$abortion_prof == "Undecided"] = "Abortion issue match"
poc_asian$abortion_match[poc_asian$abortion_recoded == 2/3 & poc_asian$abortion_prof == "Supports"] = "Abortion issue match"
poc_asian$abortion_match[poc_asian$abortion_recoded == 5/6 & poc_asian$abortion_prof == "Supports"] = "Abortion issue match"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1 & poc_asian$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_asian$abortion_match[poc_asian$abortion_recoded == 0 & poc_asian$abortion_prof == "Opposes"] = "Abortion issue match"
poc_asian$abortion_match[poc_asian$abortion_recoded == 0 & poc_asian$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 0 & poc_asian$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 0 & poc_asian$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_asian$abortion_match[poc_asian$abortion_recoded == 1/6 & poc_asian$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1/6 & poc_asian$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1/6 & poc_asian$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1/6 & poc_asian$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_asian$abortion_match[poc_asian$abortion_recoded == 1/3 & poc_asian$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1/3 & poc_asian$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1/3 & poc_asian$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1/3 & poc_asian$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_asian$abortion_match[poc_asian$abortion_recoded == 1/2 & poc_asian$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1/2 & poc_asian$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1/2 & poc_asian$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1/2 & poc_asian$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_asian$abortion_match[poc_asian$abortion_recoded == 2/3 & poc_asian$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 2/3 & poc_asian$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 2/3 & poc_asian$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 2/3 & poc_asian$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_asian$abortion_match[poc_asian$abortion_recoded == 5/6 & poc_asian$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 5/6 & poc_asian$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 5/6 & poc_asian$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 5/6 & poc_asian$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_asian$abortion_match[poc_asian$abortion_recoded == 1 & poc_asian$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1 & poc_asian$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1 & poc_asian$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_asian$abortion_match[poc_asian$abortion_recoded == 1 & poc_asian$abortion_prof == "Supports"] = "Abortion issue match"

poc_asian$abortion_match = factor(poc_asian$abortion_match, levels = c("Abortion issue mismatch",
                                                                             "Abortion issue match"))
#Guns issue match
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 0 & poc_asian$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1/6 & poc_asian$guns_prof == "Opposes"] = "Guns issue match"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1/3 & poc_asian$guns_prof == "Opposes"] = "Guns issue match"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1/2 & poc_asian$guns_prof == "Undecided"] = "Guns issue match"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 2/3 & poc_asian$guns_prof == "Supports"] = "Guns issue match"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 5/6 & poc_asian$guns_prof == "Supports"] = "Guns issue match"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1 & poc_asian$guns_prof == "Strongly supports"] = "Guns issue match"

poc_asian$guns_match[poc_asian$assaultrifles_recoded == 0 & poc_asian$guns_prof == "Opposes"] = "Guns issue match"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 0 & poc_asian$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 0 & poc_asian$guns_prof == "Supports"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 0 & poc_asian$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1/6 & poc_asian$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1/6 & poc_asian$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1/6 & poc_asian$guns_prof == "Supports"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1/6 & poc_asian$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1/3 & poc_asian$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1/3 & poc_asian$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1/3 & poc_asian$guns_prof == "Supports"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1/3 & poc_asian$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1/2 & poc_asian$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1/2 & poc_asian$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1/2 & poc_asian$guns_prof == "Supports"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1/2 & poc_asian$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_asian$guns_match[poc_asian$assaultrifles_recoded == 2/3 & poc_asian$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 2/3 & poc_asian$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 2/3 & poc_asian$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 2/3 & poc_asian$guns_prof == "Strongly supports"] = "Guns issue match"

poc_asian$guns_match[poc_asian$assaultrifles_recoded == 5/6 & poc_asian$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 5/6 & poc_asian$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 5/6 & poc_asian$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 5/6 & poc_asian$guns_prof == "Strongly supports"] = "Guns issue match"

poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1 & poc_asian$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1 & poc_asian$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1 & poc_asian$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_asian$guns_match[poc_asian$assaultrifles_recoded == 1 & poc_asian$guns_prof == "Supports"] = "Guns issue match"

poc_asian$guns_match = factor(poc_asian$guns_match, levels = c("Guns issue mismatch",
                                                                     "Guns issue match"))

poc_asian$sim_gender = factor(poc_asian$sim_gender, levels = c("Gender mismatch",
                                                                     "Gender match"))

poc_asian$sim_race = factor(poc_asian$sim_race, levels = c("Race mismatch",
                                                                 "Race match"))

#asian party_match interactions
#extreme party_match
cj_extreme_asian_partymatch = cj_anova(poc_asian, extreme_formula, by = ~party_match)
cj_extreme_asian_partymatch
#not significant (p=0.4694)

#vote party_match
cj_vote_asian_partymatch = cj_anova(poc_asian, vote_formula, by = ~party_match)
cj_vote_asian_partymatch
#significant (p=0.0375)

mms_vote_asian_partymatch = cj(poc_asian, vote_formula, id = ~ResponseId, estimate = "mm", by = ~party_match)

plot(mms_vote_asian_partymatch, vline = 0.5, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 3L)

#similar party_match
cj_similar_asian_partymatch = cj_anova(poc_asian, similar_formula, by = ~party_match)
cj_similar_asian_partymatch
#significant (p=0.0068)

mms_similar_asian_partymatch = cj(poc_asian, similar_formula, id = ~ResponseId, estimate = "mm", by = ~party_match)
plot(mms_similar_asian_partymatch, vline = 0.5, xlab = "Similarity") + ggplot2::facet_wrap(~BY, ncol = 3L)

#listen party_match
cj_listen_asian_partymatch = cj_anova(poc_asian, listen_formula, by = ~party_match)
cj_listen_asian_partymatch
#significant (p=0.0077)

mms_listen_asian_partymatch = cj(poc_asian, listen_formula, id = ~ResponseId, estimate = "mm", by = ~party_match)
plot(mms_listen_asian_partymatch, vline = 0.5, xlab = "Listen") + ggplot2::facet_wrap(~BY, ncol = 3L)

#asian listening interactions
#extreme listening
cj_extreme_asian_listen = cj_anova(poc_asian, extreme_formula, by = ~listening)
cj_extreme_asian_listen
#not significant (p=0.8539)

#vote listening
cj_vote_asian_listen = cj_anova(poc_asian, vote_formula, by = ~listening)
cj_vote_asian_listen
#not significant (p=0.4586)

#similar listening
cj_similar_asian_listen = cj_anova(poc_asian, similar_formula, by = ~listening)
cj_similar_asian_listen
#not significant (p=0.2842)

#listen listening
cj_listen_asian_listen = cj_anova(poc_asian, listen_formula, by = ~listening)
cj_listen_asian_listen
#not significant (p=0.9363)

#####Interaction with Profile Number#####

poc_23$profilenum = as.factor(poc_23$profilenum)

cj_extreme_profile = cj_anova(poc_23, extreme_R ~ sim_gender + sim_race + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~profilenum)
cj_extreme_profile
#not significant (p=0.5852)

cj_vote_profile = cj_anova(poc_23, vote_R ~ sim_gender + sim_race + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~profilenum)
cj_vote_profile
#significant (p=0.009893)

mms_vote_profile = cj(poc_23, vote_R ~ sim_gender + sim_race + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law,
                      id = ~ResponseId, estimate = "mm", by = ~profilenum)

plot(mms_vote_profile, vline = 0.35, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 2L)

cj_similar_profile = cj_anova(poc_23, similar_R ~ sim_gender + sim_race + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~profilenum)
cj_similar_profile
#not significant (p=0.131)

cj_listen_profile = cj_anova(poc_23, listen_R ~ sim_gender + sim_race + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~profilenum)
cj_listen_profile
#significant (p=0.02805)

mms_listen_profile = cj(poc_23, listen_R ~ sim_gender + sim_race + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law,
                              id = ~ResponseId, estimate = "mm", by = ~profilenum)
plot(mms_listen_profile, vline = 0.5, xlab = "Listen") + ggplot2::facet_wrap(~BY, ncol = 2L)

#####Profile 1 (Vote and Listen)

mms_vote_profile1 = cj(subset(poc_23, profilenum == 1), vote_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")

plot(mms_vote_profile1, vline = 0.5, xlab = "Vote")

mms_listen_profile1 = cj(subset(poc_23, profilenum == 1), listen_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm")

plot(mms_listen_profile1, vline = 0.5, xlab = "Listen")

#####Racial Importance Analysis#####
poc_23$race.linked.fate = factor(poc_23$race.linked.fate, level = c("Not at all",
                                                                    "Not very much",
                                                                    "A moderate amount",
                                                                    "A lot"))
poc_23$race.importance = factor(poc_23$race.importance, level = c("Not at all important",
                                                                  "A little important",
                                                                  "Moderately important",
                                                                  "Very important",
                                                                  "Extremely important"))
#race.importance
cj_extreme_raceimportance = cj_anova(poc_23, extreme_R ~ sim_race + race.linked.fate + sim_gender + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~race.importance)
cj_extreme_raceimportance
#not significant (p=0.3723)

cj_vote_raceimportance = cj_anova(poc_23, vote_R ~ sim_race + race.linked.fate + sim_gender + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~race.importance)
cj_vote_raceimportance
#not significant (p=0.4889)

cj_similar_raceimportance = cj_anova(poc_23, similar_R ~ sim_race + race.linked.fate + sim_gender + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~race.importance)
cj_similar_raceimportance
#not significant (p=0.5278)

cj_listen_raceimportance = cj_anova(poc_23, listen_R ~ sim_race + race.linked.fate + sim_gender + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~race.importance)
cj_listen_raceimportance
#not significant (p=0.7439)

#race.linked.fate
cj_extreme_linkedfate = cj_anova(poc_23, extreme_R ~ sim_race + race.importance + sim_gender + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~race.linked.fate)
cj_extreme_linkedfate
#not significant (p=0.9483)

cj_vote_linkedfate = cj_anova(poc_23, vote_R ~ sim_race + race.importance + sim_gender + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~race.linked.fate)
cj_vote_linkedfate
#significant (p=0.005676)

mms_vote_linkedfate = cj(poc_23, vote_R ~ sim_race + race.importance + sim_gender + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law,
                         id = ~ResponseId, estimate = "mm", by = ~race.linked.fate)

plot(mms_vote_linkedfate, vline = 0.5, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 2L)

cj_similar_linkedfate = cj_anova(poc_23, similar_R ~ sim_race + race.importance + sim_gender + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~race.linked.fate)
cj_similar_linkedfate
#not significant (p=0.5922)

cj_listen_linkedfate = cj_anova(poc_23, listen_R ~ sim_race + race.importance + sim_gender + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~race.linked.fate)
cj_listen_linkedfate
#significant (p=0.00114)

mms_listen_linkedfate = cj(poc_23, listen_R ~ sim_race + race.importance + sim_gender + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law,
                           id = ~ResponseId, estimate = "mm", by = ~race.linked.fate)
plot(mms_listen_linkedfate, vline = 0.5, xlab = "Listen") + ggplot2::facet_wrap(~BY, ncol = 2L)

#party_match
cj_extreme_simrace = cj_anova(poc_23, extreme_R ~ race.importance + race.linked.fate + sim_gender + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~sim_race)
cj_extreme_simrace
#not significant (p=0.6749)

cj_vote_simrace = cj_anova(poc_23, vote_R ~ race.importance + race.linked.fate + sim_gender + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~sim_race)
cj_vote_simrace
#not significant (p=0.8448)

cj_similar_simrace = cj_anova(poc_23, similar_R ~ race.importance + race.linked.fate + sim_gender + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~sim_race)
cj_similar_simrace
#not significant (p=0.3496)

cj_listen_simrace = cj_anova(poc_23, listen_R ~ race.importance + race.linked.fate + sim_gender + abortion_match + guns_match + party_match + party_ext_prof + violence + compromise + civil + talk + listening + law, by = ~sim_race)
cj_listen_simrace
#not significant (p=0.5863)

#####Quality Check#####

#variables to include: the duration in seconds one, attention_check1

#attention_check1 binary variable for correct
library(stringr)

poc_23$paid_attention = 0
poc_23$paid_attention[str_detect(poc_23$attention_check1, "Strongly agree,Strongly disagree")] = 1

cj_paid_attention_extreme = cj_anova(poc_23, extreme_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening,
                                     id = ~ResponseId, by = ~paid_attention)
cj_paid_attention_extreme
#not significant (p=0.4459)

cj_paid_attention_vote = cj_anova(poc_23, vote_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening,
                                  id = ~ResponseId, by = ~paid_attention)

cj_paid_attention_vote
#not significant (p=0.2044)

cj_paid_attention_similar = cj_anova(poc_23, similar_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening,
                                     id = ~ResponseId, by = ~paid_attention)

cj_paid_attention_similar
#not significant (p=0.3368)

cj_paid_attention_listen = cj_anova(poc_23, listen_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening,
                                    id = ~ResponseId, by = ~paid_attention)

cj_paid_attention_listen
#not significant (p=0.5414)

#duration variable for completed in a reasonable amount of time
poc_23$nottoofast = 0
poc_23$nottoofast[poc_23$Duration..in.seconds. > 500] = 1

cj_nottoofast_extreme = cj_anova(poc_23, extreme_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening,
                              id = ~ResponseId, by = ~nottoofast)

cj_nottoofast_extreme
#not significant (p=0.8629)

cj_nottoofast_vote = cj_anova(poc_23, vote_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening,
                           id = ~ResponseId, by = ~nottoofast)

cj_nottoofast_vote
#not significant (p=0.1829)

cj_nottoofast_similar = cj_anova(poc_23, similar_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening,
                              id = ~ResponseId, by = ~nottoofast)

cj_nottoofast_similar
#not significant (p=0.3417)

cj_nottoofast_listen = cj_anova(poc_23, listen_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening,
                             id = ~ResponseId, by = ~nottoofast)

cj_nottoofast_listen
#not significant (p=0.1557)

#t-tests for differences between surveys

#duration in seconds
#create the duration variable (in minutes)
poc_23$duration = poc_23$Duration..in.seconds./60

t.test(tess_long$duration, poc$duration)
#significant (p=0.000) difference between 2021 data and 2023 data (took longer in 2023 than 2021)

conjoint_AS22$duration = conjoint_AS22$Duration..in.seconds./60

conjoint_PS22$Duration..in.seconds. = as.numeric(conjoint_PS22$Duration..in.seconds.)
conjoint_PS22$duration = conjoint_PS22$Duration..in.seconds./60

t.test(conjoint_AS22$duration, poc_23$duration)
#significant (p=0.000) difference between 2022 academic data and 2023 data (took longer in 2022 academic than 2023)

t.test(conjoint_PS22$duration, poc_23$duration)
#significant (p=0.001) difference between 2022 public data and 2023 data (took longer in 2023 than 2022 public)

#attention check
conjoint_PS22$paid_attention = 0
conjoint_PS22$paid_attention[str_detect(conjoint_PS22$attention_check1, "Strongly agree,Strongly disagree")] = 1

t.test(conjoint_PS22$paid_attention, poc_23$paid_attention)
#significant (p=0.000) difference between 2022 public data and 2023 data (more people paid attention in 2023 than in 2022 public)

#example graphs for memo
mms_paid_attention_extreme = cj(subset(poc_23, paid_attention == 1), extreme_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening,
                                id = ~ResponseId, estimate = "mm")
plot(mms_extreme, vline = 0.5, xlab = "Extreme")
plot(mms_paid_attention_extreme, vline = 0.5, xlab = "Extreme (Subset Paid Attention)")

mms_nottoofast_extreme = cj(subset(poc_23, Duration..in.seconds. > 500), extreme_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening,
                         id = ~ResponseId, estimate = "mm")
plot(mms_nottoofast_extreme, vline = 0.5, xlab = "Extreme (Subset Not Too Fast)")

#####Non-Multiethnic Hispanic Variable Analysis######
poc_23_hispanic = poc_23
poc_23_hispanic$race[poc_23_hispanic$race.ethnicity == "Hispanic/latino(a)"] = "Hispanic"
poc_23_hispanic$race[str_detect(poc_23_hispanic$race.ethnicity, ",")] = "Other"

poc_hispanic = subset(poc_23_hispanic, poc_23_hispanic$race == "Hispanic")

#Gender
poc_hispanic$sim_gender[poc_hispanic$gender == "Female" & poc_hispanic$gender_prof == "Female"] = "Gender match"
poc_hispanic$sim_gender[poc_hispanic$gender == "Male" & poc_hispanic$gender_prof == "Male"] = "Gender match"
poc_hispanic$sim_gender[poc_hispanic$gender == "Non-binary" & poc_hispanic$gender_prof == "Non-binary"] = "Gender match"
poc_hispanic$sim_gender[poc_hispanic$gender == "Female" & poc_hispanic$gender_prof == "Male"] = "Gender mismatch"
poc_hispanic$sim_gender[poc_hispanic$gender == "Female" & poc_hispanic$gender_prof == "Non-binary"] = "Gender mismatch"
poc_hispanic$sim_gender[poc_hispanic$gender == "Male" & poc_hispanic$gender_prof == "Non-binary"] = "Gender mismatch"
poc_hispanic$sim_gender[poc_hispanic$gender == "Male" & poc_hispanic$gender_prof == "Female"] = "Gender mismatch"
poc_hispanic$sim_gender[poc_hispanic$gender == "Non-binary" & poc_hispanic$gender_prof == "Female"] = "Gender mismatch"
poc_hispanic$sim_gender[poc_hispanic$gender == "Non-binary" & poc_hispanic$gender_prof == "Male"] = "Gender mismatch"

poc_hispanic$sim_gender = factor(poc_hispanic$sim_gender, levels = c("Gender mismatch",
                                                                     "Gender match"))

#Race
poc_hispanic$sim_race[poc_hispanic$race == "Asian" & poc_hispanic$race_prof == "Asian American"] = "Race match"
poc_hispanic$sim_race[poc_hispanic$race == "Black" & poc_hispanic$race_prof == "Black American"] = "Race match"
poc_hispanic$sim_race[poc_hispanic$race == "Hispanic" & poc_hispanic$race_prof == "Hispanic American"] = "Race match"
poc_hispanic$sim_race[poc_hispanic$race == "White" & poc_hispanic$race_prof == "White American"] = "Race match"
poc_hispanic$sim_race[poc_hispanic$race == "Asian" & poc_hispanic$race_prof == "Black American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Asian" & poc_hispanic$race_prof == "Hispanic American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Asian" & poc_hispanic$race_prof == "White American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Black" & poc_hispanic$race_prof == "Asian American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Black" & poc_hispanic$race_prof == "Hispanic American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Black" & poc_hispanic$race_prof == "White American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Hispanic" & poc_hispanic$race_prof == "Asian American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Hispanic" & poc_hispanic$race_prof == "Black American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Hispanic" & poc_hispanic$race_prof == "White American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "White" & poc_hispanic$race_prof == "Asian American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "White" & poc_hispanic$race_prof == "Black American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "White" & poc_hispanic$race_prof == "Hispanic American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Other" & poc_hispanic$race_prof == "Asian American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Other" & poc_hispanic$race_prof == "Black American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Other" & poc_hispanic$race_prof == "Hispanic American"] = "Race mismatch"
poc_hispanic$sim_race[poc_hispanic$race == "Other" & poc_hispanic$race_prof == "White American"] = "Race mismatch"


poc_hispanic$sim_race = factor(poc_hispanic$sim_race, levels = c("Race mismatch",
                                                                 "Race match"))

#Party_group
poc_hispanic$party_group[poc_hispanic$pid_recoded > 0.5] = "Republican"
poc_hispanic$party_group[poc_hispanic$pid_recoded < 0.5] = "Democrat"
poc_hispanic$party_group[poc_hispanic$pid_recoded == 0.5] = "Independent"
poc_hispanic$party_group = factor(poc_hispanic$party_group, levels = c("Democrat",
                                                                       "Independent",
                                                                       "Republican"))

#Party Match
poc_hispanic$party_match[poc_hispanic$party_group == "Democrat" & poc_hispanic$party_prof == "Far-left Democrat"] = "Party match"
poc_hispanic$party_match[poc_hispanic$party_group == "Democrat" & poc_hispanic$party_prof == "Democrat"] = "Party match"
poc_hispanic$party_match[poc_hispanic$party_group == "Democrat" & poc_hispanic$party_prof == "Moderate Democrat"] = "Party match"

poc_hispanic$party_match[poc_hispanic$party_group == "Republican" & poc_hispanic$party_prof == "Far-right Republican"] = "Party match"
poc_hispanic$party_match[poc_hispanic$party_group == "Republican" & poc_hispanic$party_prof == "Republican"] = "Party match"
poc_hispanic$party_match[poc_hispanic$party_group == "Republican" & poc_hispanic$party_prof == "Moderate Republican"] = "Party match"

poc_hispanic$party_match[poc_hispanic$party_group == "Independent" & poc_hispanic$party_prof == "Independent"] = "Party match"

poc_hispanic$party_match[poc_hispanic$party_group == "Democrat" & poc_hispanic$party_prof == "Far-right Republican"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Democrat" & poc_hispanic$party_prof == "Republican"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Democrat" & poc_hispanic$party_prof == "Moderate Republican"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Democrat" & poc_hispanic$party_prof == "Independent"] = "Party mismatch"

poc_hispanic$party_match[poc_hispanic$party_group == "Republican" & poc_hispanic$party_prof == "Far-left Democrat"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Republican" & poc_hispanic$party_prof == "Democrat"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Republican" & poc_hispanic$party_prof == "Moderate Democrat"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Republican" & poc_hispanic$party_prof == "Independent"] = "Party mismatch"

poc_hispanic$party_match[poc_hispanic$party_group == "Independent" & poc_hispanic$party_prof == "Far-left Democrat"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Independent" & poc_hispanic$party_prof == "Democrat"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Independent" & poc_hispanic$party_prof == "Moderate Democrat"] = "Party mismatch"

poc_hispanic$party_match[poc_hispanic$party_group == "Independent" & poc_hispanic$party_prof == "Far-right Republican"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Independent" & poc_hispanic$party_prof == "Republican"] = "Party mismatch"
poc_hispanic$party_match[poc_hispanic$party_group == "Independent" & poc_hispanic$party_prof == "Moderate Republican"] = "Party mismatch"

poc_hispanic$party_match = factor(poc_hispanic$party_match, levels = c("Party mismatch",
                                                                       "Party match"))
#Abortion issue match
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 0 & poc_hispanic$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/6 & poc_hispanic$abortion_prof == "Opposes"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/3 & poc_hispanic$abortion_prof == "Opposes"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/2 & poc_hispanic$abortion_prof == "Undecided"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 2/3 & poc_hispanic$abortion_prof == "Supports"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 5/6 & poc_hispanic$abortion_prof == "Supports"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1 & poc_hispanic$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 0 & poc_hispanic$abortion_prof == "Opposes"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 0 & poc_hispanic$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 0 & poc_hispanic$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 0 & poc_hispanic$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/6 & poc_hispanic$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/6 & poc_hispanic$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/6 & poc_hispanic$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/6 & poc_hispanic$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/3 & poc_hispanic$abortion_prof == "Strongly opposes"] = "Abortion issue match"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/3 & poc_hispanic$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/3 & poc_hispanic$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/3 & poc_hispanic$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/2 & poc_hispanic$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/2 & poc_hispanic$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/2 & poc_hispanic$abortion_prof == "Supports"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1/2 & poc_hispanic$abortion_prof == "Strongly supports"] = "Abortion issue mismatch"

poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 2/3 & poc_hispanic$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 2/3 & poc_hispanic$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 2/3 & poc_hispanic$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 2/3 & poc_hispanic$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 5/6 & poc_hispanic$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 5/6 & poc_hispanic$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 5/6 & poc_hispanic$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 5/6 & poc_hispanic$abortion_prof == "Strongly supports"] = "Abortion issue match"

poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1 & poc_hispanic$abortion_prof == "Strongly opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1 & poc_hispanic$abortion_prof == "Opposes"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1 & poc_hispanic$abortion_prof == "Undecided"] = "Abortion issue mismatch"
poc_hispanic$abortion_match[poc_hispanic$abortion_recoded == 1 & poc_hispanic$abortion_prof == "Supports"] = "Abortion issue match"

poc_hispanic$abortion_match = factor(poc_hispanic$abortion_match, levels = c("Abortion issue mismatch",
                                                                             "Abortion issue match"))
#Guns issue match
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 0 & poc_hispanic$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/6 & poc_hispanic$guns_prof == "Opposes"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/3 & poc_hispanic$guns_prof == "Opposes"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/2 & poc_hispanic$guns_prof == "Undecided"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 2/3 & poc_hispanic$guns_prof == "Supports"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 5/6 & poc_hispanic$guns_prof == "Supports"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1 & poc_hispanic$guns_prof == "Strongly supports"] = "Guns issue match"

poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 0 & poc_hispanic$guns_prof == "Opposes"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 0 & poc_hispanic$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 0 & poc_hispanic$guns_prof == "Supports"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 0 & poc_hispanic$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/6 & poc_hispanic$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/6 & poc_hispanic$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/6 & poc_hispanic$guns_prof == "Supports"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/6 & poc_hispanic$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/3 & poc_hispanic$guns_prof == "Strongly opposes"] = "Guns issue match"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/3 & poc_hispanic$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/3 & poc_hispanic$guns_prof == "Supports"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/3 & poc_hispanic$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/2 & poc_hispanic$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/2 & poc_hispanic$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/2 & poc_hispanic$guns_prof == "Supports"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1/2 & poc_hispanic$guns_prof == "Strongly supports"] = "Guns issue mismatch"

poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 2/3 & poc_hispanic$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 2/3 & poc_hispanic$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 2/3 & poc_hispanic$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 2/3 & poc_hispanic$guns_prof == "Strongly supports"] = "Guns issue match"

poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 5/6 & poc_hispanic$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 5/6 & poc_hispanic$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 5/6 & poc_hispanic$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 5/6 & poc_hispanic$guns_prof == "Strongly supports"] = "Guns issue match"

poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1 & poc_hispanic$guns_prof == "Strongly opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1 & poc_hispanic$guns_prof == "Opposes"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1 & poc_hispanic$guns_prof == "Undecided"] = "Guns issue mismatch"
poc_hispanic$guns_match[poc_hispanic$assaultrifles_recoded == 1 & poc_hispanic$guns_prof == "Supports"] = "Guns issue match"

poc_hispanic$guns_match = factor(poc_hispanic$guns_match, levels = c("Guns issue mismatch",
                                                                     "Guns issue match"))

poc_hispanic$sim_gender = factor(poc_hispanic$sim_gender, levels = c("Gender mismatch",
                                                                     "Gender match"))

poc_hispanic$sim_race = factor(poc_hispanic$sim_race, levels = c("Race mismatch",
                                                                 "Race match"))

#hispanic party_match interactions
#extreme party_match
cj_extreme_hispanic_partymatch = cj_anova(poc_hispanic, extreme_formula, by = ~party_match)
cj_extreme_hispanic_partymatch
#not significant (p=0.8211)

#vote party_match
cj_vote_hispanic_partymatch = cj_anova(poc_hispanic, vote_formula, by = ~party_match)
cj_vote_hispanic_partymatch
#not significant (p=0.213)
#would've been significant

#similar party_match
cj_similar_hispanic_partymatch = cj_anova(poc_hispanic, similar_formula, by = ~party_match)
cj_similar_hispanic_partymatch
#not significant (p=0.1238)

#listen party_match
cj_listen_hispanic_partymatch = cj_anova(poc_hispanic, listen_formula, by = ~party_match)
cj_listen_hispanic_partymatch
#not significant (p=0.2237)

#hispanic listening interactions
#extreme listening
cj_extreme_hispanic_listen = cj_anova(poc_hispanic, extreme_formula, by = ~listening)
cj_extreme_hispanic_listen
#not significant (p=0.5)

#vote listening
cj_vote_hispanic_listen = cj_anova(poc_hispanic, vote_formula, by = ~listening)
cj_vote_hispanic_listen
#not significant (p=0.54)

#similar listening
cj_similar_hispanic_listen = cj_anova(poc_hispanic, similar_formula, by = ~listening)
cj_similar_hispanic_listen
#not significant (p=0.5855)

#listen listening
cj_listen_hispanic_listen = cj_anova(poc_hispanic, listen_formula, by = ~listening)
cj_listen_hispanic_listen

#####Gender Exploration#####
poc_23$gender_female = as.factor(poc_23$gender_female)
#do respondents mark female profiles as more extreme
#extreme
cj_gender_extreme = cj_anova(poc_23, extreme_R ~ race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, by = ~gender_female)
cj_gender_extreme
#not significant (p=0.7515)

#vote
cj_gender_vote = cj_anova(poc_23, vote_R ~ race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, by = ~gender_female)
cj_gender_vote
#not significant (p=0.7809)

#similar
cj_gender_similar = cj_anova(poc_23, similar_R ~ race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, by = ~gender_female)
cj_gender_similar
#not significant (p=0.9848)

#listen
cj_gender_listen = cj_anova(poc_23, listen_R ~ race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, by = ~gender_female)
cj_gender_listen
#not significant (p=0.8983)

#do female respondents rate profiles as more extreme
poc_23 = subset(poc_23, poc_23$ResponseId != "R_2AL9pD5u2Ii9PaY")
poc_23$gender = as.factor(poc_23$gender)

#extreme
cj_gender_extreme_1 = cj_anova(poc_23, extreme_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, by = ~gender)
cj_gender_extreme_1
#significant (p=0.007)

mms_gender_extreme_1 = cj(poc_23, extreme_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm", by = ~gender)
diff_gender_extreme_1 = cj(poc_23, extreme_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm_differences", by = ~gender)

plot(mms_gender_extreme_1, vline = 0.5, xlab = "Extreme") + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(diff_gender_extreme_1, vline = 0, xlab = "Extreme") + ggplot2::facet_wrap(~BY, ncol = 3L)

#vote
cj_gender_vote_1 = cj_anova(poc_23, vote_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, by = ~gender)
cj_gender_vote_1
#significant (p=0.000)

mms_gender_vote_1 = cj(poc_23, vote_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm", by = ~gender)
diff_gender_vote_1 = cj(poc_23, vote_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm_differences", by = ~gender)

plot(mms_gender_vote_1, vline = 0.5, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(diff_gender_vote_1, vline = 0, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 3L)

#similar
cj_gender_similar_1 = cj_anova(poc_23, similar_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, by = ~gender)
cj_gender_similar_1
#significant (p=0.000)

mms_gender_similar_1 = cj(poc_23, similar_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm", by = ~gender)
diff_gender_similar_1 = cj(poc_23, similar_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm_differences", by = ~gender)

plot(mms_gender_similar_1, vline = 0.5, xlab = "Similar") + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(diff_gender_similar_1, vline = 0, xlab = "Similar") + ggplot2::facet_wrap(~BY, ncol = 3L)

#listen
cj_gender_listen_1 = cj_anova(poc_23, listen_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, by = ~gender)
cj_gender_listen_1
#significant (p=0.000)

mms_gender_listen_1 = cj(poc_23, listen_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm", by = ~gender)
diff_gender_listen_1 = cj(poc_23, listen_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm_differences", by = ~gender)

plot(mms_gender_listen_1, vline = 0.5, xlab = "Listen") + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(diff_gender_listen_1, vline = 0, xlab = "Listen") + ggplot2::facet_wrap(~BY, ncol = 3L)

#do female respondents rate profiles as more extreme (binary)
poc_23$gender_resp_female = "Not Female"
poc_23$gender_resp_female[poc_23$gender == "Female"] = "Female"
poc_23$gender_resp_female = factor(poc_23$gender_resp_female, levels = c("Not Female", "Female"))

#extreme
cj_gender_extreme_2 = cj_anova(poc_23, extreme_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, by = ~gender_resp_female)
cj_gender_extreme_2
#significant (p=0.002)

mms_gender_extreme_2 = cj(poc_23, extreme_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm", by = ~gender_resp_female)
diff_gender_extreme_2 = cj(poc_23, extreme_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm_differences", by = ~gender_resp_female)

plot(mms_gender_extreme_2, vline = 0.5, xlab = "Extreme") + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(diff_gender_extreme_2, vline = 0, xlab = "Extreme") + ggplot2::facet_wrap(~BY, ncol = 3L)

#vote
cj_gender_vote_2 = cj_anova(poc_23, vote_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, by = ~gender_resp_female)
cj_gender_vote_2
#significant (p=0.000)

mms_gender_vote_2 = cj(poc_23, vote_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm", by = ~gender_resp_female)
diff_gender_vote_2 = cj(poc_23, vote_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm_differences", by = ~gender_resp_female)

plot(mms_gender_vote_2, vline = 0.5, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(diff_gender_vote_2, vline = 0, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 3L)

#similar
cj_gender_similar_2 = cj_anova(poc_23, similar_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, by = ~gender_resp_female)
cj_gender_similar_2
#significant (p=0.001)

mms_gender_similar_2 = cj(poc_23, similar_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm", by = ~gender_resp_female)
diff_gender_similar_2 = cj(poc_23, similar_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm_differences", by = ~gender_resp_female)

plot(mms_gender_similar_2, vline = 0.5, xlab = "Similar") + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(diff_gender_similar_2, vline = 0, xlab = "Similar") + ggplot2::facet_wrap(~BY, ncol = 3L)

#listen
cj_gender_listen_2 = cj_anova(poc_23, listen_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, by = ~gender_resp_female)
cj_gender_listen_2
#significant (p=0.000)

mms_gender_listen_2 = cj(poc_23, listen_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm", by = ~gender_resp_female)
diff_gender_listen_2 = cj(poc_23, listen_R ~ gender_prof + race_prof + party_prof + party_match + civil + talk + violence + compromise + law + listening, id = ~ResponseId, estimate = "mm_differences", by = ~gender_resp_female)

plot(mms_gender_listen_2, vline = 0.5, xlab = "Listen") + ggplot2::facet_wrap(~BY, ncol = 3L)

plot(diff_gender_listen_2, vline = 0, xlab = "Listen") + ggplot2::facet_wrap(~BY, ncol = 3L)
