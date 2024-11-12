###Conjoint Gender Analysis
library(cregg)

#2021 data
tess_long$gender = as.factor(tess_long$gender)
tess_long$gender_respondent = as.factor(tess_long$gender_respondent)

#do respondents rank different genders as more/less extreme?
#extreme
cj_extreme_tess = cj_anova(tess_long, extreme ~ race + ownpid + demdescribe + repdescribe + civil + norm + talk + viol + law, id = ~CaseId, by = ~gender)
cj_extreme_tess
#not significant (p=0.1136)

#vote
cj_vote_tess = cj_anova(tess_long, vote ~ race + ownpid + demdescribe + repdescribe + civil + norm + talk + viol + law, id = ~CaseId, by = ~gender)
cj_vote_tess
#not significant (p=0.2916)

#speech
cj_speech_tess = cj_anova(tess_long, speech ~ race + ownpid + demdescribe + repdescribe + civil + norm + talk + viol + law, id = ~CaseId, by = ~gender)
cj_speech_tess
#not significant (p=0.2935)

#teach
cj_teach_tess = cj_anova(tess_long, teach ~ race + ownpid + demdescribe + repdescribe + civil + norm + talk + viol + law, id = ~CaseId, by = ~gender)
cj_teach_tess
#not significant (p=0.5973)

#do respondents rank profiles as more/less extreme because of their own gender?
#extreme
cj_extreme_tess_1 = cj_anova(tess_long, extreme ~ gender + race + ownpid + demdescribe + repdescribe + civil + norm + talk + viol + law, id = ~CaseId, by = ~gender_respondent)
cj_extreme_tess_1
#significant (p=0.0014)

mms_extreme_tess_1 = cj(tess_long, extreme ~ gender + race + ownpid + demdescribe + repdescribe + civil + norm + talk + viol + law, id = ~CaseId, estimate = "mm", by = ~gender_respondent)
plot(mms_extreme_tess_1, vline = 0.5, xlab = "Extreme") + ggplot2::facet_wrap(~BY, ncol = 3L)

mms_diff_extreme_tess_1 = cj(tess_long, extreme ~ gender + race + ownpid + demdescribe + repdescribe + civil + norm + talk + viol + law, id = ~CaseId, estimate = "mm_difference", by = ~gender_respondent)
plot(mms_diff_extreme_tess_1, vline = 0, xlab = "Extreme") + ggplot2::facet_wrap(~BY, ncol = 3L)

#vote
cj_vote_tess_1 = cj_anova(tess_long, vote ~ gender + race + ownpid + demdescribe + repdescribe + civil + norm + talk + viol + law, id = ~CaseId, by = ~gender_respondent)
cj_vote_tess_1
#not significant (p=0.0654)

#speech
cj_speech_tess_1 = cj_anova(tess_long, speech ~ gender + race + ownpid + demdescribe + repdescribe + civil + norm + talk + viol + law, id = ~CaseId, by = ~gender_respondent)
cj_speech_tess_1
#significant (p=0.0000)

mms_speech_tess_1 = cj(tess_long, speech ~ gender + race + ownpid + demdescribe + repdescribe + civil + norm + talk + viol + law, id = ~CaseId, estimate = "mm", by = ~gender_respondent)
plot(mms_speech_tess_1, vline = 0.5, xlab = "Speech") + ggplot2::facet_wrap(~BY, ncol = 3L)

mms_diff_speech_tess_1 = cj(tess_long, speech ~ gender + race + ownpid + demdescribe + repdescribe + civil + norm + talk + viol + law, id = ~CaseId, estimate = "mm_difference", by = ~gender_respondent)
plot(mms_diff_speech_tess_1, vline = 0, xlab = "Speech") + ggplot2::facet_wrap(~BY, ncol = 3L)

#teach
cj_teach_tess_1 = cj_anova(tess_long, teach ~ gender + race + ownpid + demdescribe + repdescribe + civil + norm + talk + viol + law, id = ~CaseId, by = ~gender_respondent)
cj_teach_tess_1
#significant (p=0.0000)

mms_teach_tess_1 = cj(tess_long, teach ~ gender + race + ownpid + demdescribe + repdescribe + civil + norm + talk + viol + law, id = ~CaseId, estimate = "mm", by = ~gender_respondent)
plot(mms_teach_tess_1, vline = 0.5, xlab = "Teach") + ggplot2::facet_wrap(~BY, ncol = 3L)

mms_diff_teach_tess_1 = cj(tess_long, teach ~ gender + race + ownpid + demdescribe + repdescribe + civil + norm + talk + viol + law, id = ~CaseId, estimate = "mm_difference", by = ~gender_respondent)
plot(mms_diff_teach_tess_1, vline = 0, xlab = "Teach") + ggplot2::facet_wrap(~BY, ncol = 3L)

#2022 public data
conjoint_PS22$gender_prof = as.factor(conjoint_PS22$gender_prof)
conjoint_PS22$gender[conjoint_PS22$gender == ""] = NA
conjoint_PS22$gender = as.factor(conjoint_PS22$gender)

#do respondents rank different genders as more/less extreme?
#extreme
cj_extreme_p22 = cj_anova(conjoint_PS22, extreme_R ~ race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, by = ~gender_prof)
cj_extreme_p22
#not significant (p=0.0659)

#vote
cj_vote_p22 = cj_anova(conjoint_PS22, vote_R ~ race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, by = ~gender_prof)
cj_vote_p22
#not significant (p=0.0580)

#ft
cj_ft_p22 = cj_anova(conjoint_PS22, ft_R ~ race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, by = ~gender_prof)
cj_ft_p22
#not significant (p=0.1605)

#do respondents rank profiles as more/less extreme because of their own gender?
#extreme
cj_extreme_p22_1 = cj_anova(conjoint_PS22, extreme_R ~ gender_prof + race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, by = ~gender)
cj_extreme_p22_1
#significant (p=0.0457)

mms_extreme_p22_1 = cj(conjoint_PS22, extreme_R ~ gender_prof + race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, estimate = "mm", by = ~gender)
plot(mms_extreme_p22_1, vline = 0.5, xlab = "Extreme") + ggplot2::facet_wrap(~BY, ncol = 3L)

mms_diff_extreme_p22_1 = cj(conjoint_PS22, extreme_R ~ gender_prof + race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, estimate = "mm_difference", by = ~gender)
plot(mms_diff_extreme_p22_1, vline = 0, xlab = "Extreme") + ggplot2::facet_wrap(~BY, ncol = 3L)

#vote
cj_vote_p22_1 = cj_anova(conjoint_PS22, vote_R ~ gender_prof + race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, by = ~gender)
cj_vote_p22_1
#significant (p=0.0004)

mms_vote_p22_1 = cj(conjoint_PS22, vote_R ~ gender_prof + race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, estimate = "mm", by = ~gender)
plot(mms_vote_p22_1, vline = 0.5, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 3L)

mms_diff_vote_p22_1 = cj(conjoint_PS22, vote_R ~ gender_prof + race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, estimate = "mm_difference", by = ~gender)
plot(mms_diff_vote_p22_1, vline = 0, xlab = "Vote") + ggplot2::facet_wrap(~BY, ncol = 3L)

#ft
cj_ft_p22_1 = cj_anova(conjoint_PS22, ft_R ~ gender_prof + race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, by = ~gender)
cj_ft_p22_1
#significant (p=0.0000)

mms_ft_p22_1 = cj(conjoint_PS22, ft_R ~ gender_prof + race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, estimate = "mm", by = ~gender)
plot(mms_ft_p22_1, vline = 0.5, xlab = "FT") + ggplot2::facet_wrap(~BY, ncol = 3L)

mms_diff_ft_p22_1 = cj(conjoint_PS22, ft_R ~ gender_prof + race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, estimate = "mm_difference", by = ~gender)
plot(mms_diff_ft_p22_1, vline = 0, xlab = "FT") + ggplot2::facet_wrap(~BY, ncol = 3L)

#2022 academic data

#do respondents rank different genders as more/less extreme?
#extreme
cj_extreme_a22 = cj_anova(conjoint_AS22, extreme_R ~ race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, by = ~gender_prof)
cj_extreme_a22
#not significant (p=0.561)

#vote
cj_vote_a22 = cj_anova(conjoint_AS22, vote_R ~ race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, by = ~gender_prof)
cj_vote_a22
#not significant (p=0.1286)

#ft
cj_ft_a22 = cj_anova(conjoint_AS22, ft_R ~ race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, by = ~gender_prof)
cj_ft_a22
#not significant (p=0.0561)

#do respondents rank profiles as more/less extreme because of their own gender?
#extreme
cj_extreme_a22_1 = cj_anova(conjoint_AS22, extreme_R ~ gender_prof + race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, by = ~gender)
cj_extreme_a22_1
#not significant (p=0.6018)

#vote
cj_vote_a22_1 = cj_anova(conjoint_AS22, vote_R ~ gender_prof + race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, by = ~gender)
cj_vote_a22_1
#not significant (p=0.1456)

#ft
cj_ft_a22_1 = cj_anova(conjoint_AS22, ft_R ~ gender_prof + race_prof + party + civil + talk + violence + compromise + law, id = ~ResponseId, by = ~gender)
cj_ft_a22_1
#not significant (p=0.2622)