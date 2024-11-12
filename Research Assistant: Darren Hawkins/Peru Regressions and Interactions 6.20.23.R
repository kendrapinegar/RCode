###Initial Regressions and Findings of Peru
##June 20, 2023
#Last Updated: August 10, 2023


###Import Datasets (without Bad Completes) --------------------------------------
m_dem <- read_excel("MX_Data_Excluding_Bad_Completes 7.26.23.xlsx")
p_dem <- read_excel("PE_Data_Excluding_Bad_Completes 7.26.23.xlsx")


###Basic Regressions -------------------------------------------------------
#Basic regressions, no controls
lm_frexp1_nc <- lm(frexp1_recode ~ treat, data = p_dem)
summary(lm_frexp1_nc)

lm_frexp2_nc <- lm(frexp2 ~ treat, data = p_dem)
summary(lm_frexp2_nc)

lm_frassc1_nc <- lm(frassc1 ~ treat, data = p_dem)
summary(lm_frassc1_nc)

lm_frassc2_nc <- lm(frassc2_recode ~ treat, data = p_dem)
summary(lm_frassc2_nc)

lm_frassc3_nc <- lm(frassc3 ~ treat, data = p_dem)
summary(lm_frassc3_nc)

lm_unisuff1_nc <- lm(unisuff1 ~ treat, data = p_dem)
summary(lm_unisuff1_nc)

lm_unisuff2_nc <- lm(unisuff2_recode ~ treat, data = p_dem)
summary(lm_unisuff2_nc)

lm_decelec1_nc <- lm(decelec1 ~ treat, data = p_dem)
summary(lm_decelec1_nc)

lm_decelec2_nc <- lm(decelec2_recode ~ treat, data = p_dem)
summary(lm_decelec2_nc)

lm_frelect1_nc <- lm(frelect1_recode ~ treat, data = p_dem)
summary(lm_frelect1_nc)

lm_frelect2_nc <- lm(frelect2 ~ treat, data = p_dem)
summary(lm_frelect2_nc)

lm_judcnstr1_nc <- lm(judcnstr1_recode ~ treat, data = p_dem)
summary(lm_judcnstr1_nc)

lm_judcnstr2_nc <- lm(judcnstr2 ~ treat, data = p_dem)
summary(lm_judcnstr2_nc)

lm_legcnstr1_nc <- lm(legcnstr1 ~ treat, data = p_dem)
summary(lm_legcnstr1_nc)

lm_legcnstr2_nc <- lm(legcnstr2_recode ~ treat, data = p_dem)
summary(lm_legcnstr2_nc)

lm_eqlaw1_nc <- lm(eqlaw1 ~ treat, data = p_dem)
summary(lm_eqlaw1_nc)

lm_eqlaw2_nc <- lm(eqlaw2_recode ~ treat, data = p_dem)
summary(lm_eqlaw2_nc)

#Output for basic regressions, no controls
library(stargazer)
library(writexl)
nc <- list(lm_frexp1_nc, lm_frexp2_nc, lm_frassc1_nc, lm_frassc2_nc, lm_frassc3_nc, lm_unisuff1_nc, lm_unisuff2_nc, lm_decelec1_nc, lm_decelec2_nc, lm_frelect1_nc, lm_frelect2_nc, lm_judcnstr1_nc, lm_judcnstr2_nc, lm_legcnstr1_nc, lm_legcnstr2_nc, lm_eqlaw1_nc, lm_eqlaw2_nc, type="text")
lm_nc <- stargazer(nc, type="text")
lm_nc_matrix <- as.matrix(lm_nc)
View(lm_nc_matrix)
write_xlsx(as.data.frame(lm_nc_matrix), path= "Initial_Regression_Results1.xlsx")


#Basic regressions, some controls (no state)
lm_frexp1 <- lm(frexp1_recode ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_frexp1)

lm_frexp2 <- lm(frexp2 ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_frexp2)

lm_frassc1 <- lm(frassc1 ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_frassc1)

lm_frassc2 <- lm(frassc2_recode ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_frassc2)

lm_frassc3 <- lm(frassc3 ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_frassc3)

lm_unisuff1 <- lm(unisuff1 ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_unisuff1)

lm_unisuff2 <- lm(unisuff2_recode ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_unisuff2)

lm_decelec1 <- lm(decelec1 ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_decelec1)

lm_decelec2 <- lm(decelec2_recode ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_decelec2)

lm_frelect1 <- lm(frelect1_recode ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_frelect1)

lm_frelect2 <- lm(frelect2 ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_frelect2)

lm_judcnstr1 <- lm(judcnstr1_recode ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_judcnstr1)

lm_judcnstr2 <- lm(judcnstr2 ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_judcnstr2)

lm_legcnstr1 <- lm(legcnstr1 ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_legcnstr1)

lm_legcnstr2 <- lm(legcnstr2_recode ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_legcnstr2)

lm_eqlaw1 <- lm(eqlaw1 ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_eqlaw1)

lm_eqlaw2 <- lm(eqlaw2_recode ~ treat + age + gender + living + edu_n + income_n, data = p_dem)
summary(lm_eqlaw2)

#Output for basic regressions, controls (no state)
controls <- list(lm_frexp1, lm_frexp2, lm_frassc1, lm_frassc2, lm_frassc3, lm_unisuff1, lm_unisuff2, lm_decelec1, lm_decelec2, lm_frelect1, lm_frelect2, lm_judcnstr1, lm_judcnstr2, lm_legcnstr1, lm_legcnstr2, lm_eqlaw1, lm_eqlaw2, type="text")
lm_controls <- stargazer(controls, type="text")
lm_controls_matrix <- as.matrix(lm_controls)
View(lm_controls_matrix)
write_xlsx(as.data.frame(lm_controls_matrix), path= "Initial_Regression_Results1.xlsx")


#Basic regressions, some controls (with state)
lm_frexp1_s <- lm(frexp1_recode ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_frexp1_s)

lm_frexp2_s <- lm(frexp2 ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_frexp2_s)

lm_frassc1_s <- lm(frassc1 ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_frassc1_s)

lm_frassc2_s <- lm(frassc2_recode ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_frassc2_s)

lm_frassc3_s <- lm(frassc3 ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_frassc3_s)

lm_unisuff1_s <- lm(unisuff1 ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_unisuff1_s)

lm_unisuff2_s <- lm(unisuff2_recode ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_unisuff2_s)

lm_decelec1_s <- lm(decelec1 ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_decelec1_s)

lm_decelec2_s <- lm(decelec2_recode ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_decelec2_s)

lm_frelect1_s <- lm(frelect1_recode ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_frelect1_s)

lm_frelect2_s <- lm(frelect2 ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_frelect2_s)

lm_judcnstr1_s <- lm(judcnstr1_recode ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_judcnstr1_s)

lm_judcnstr2_s <- lm(judcnstr2 ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_judcnstr2_s)

lm_legcnstr1_s <- lm(legcnstr1 ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_legcnstr1_s)

lm_legcnstr2_s <- lm(legcnstr2_recode ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_legcnstr2_s)

lm_eqlaw1_s <- lm(eqlaw1 ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_eqlaw1_s)

lm_eqlaw2_s <- lm(eqlaw2_recode ~ treat + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(lm_eqlaw2_s)

#Output for basic regressions, controls (with state)
controls_state <- list(lm_frexp1_s, lm_frexp2_s, lm_frassc1_s, lm_frassc2_s, lm_frassc3_s, lm_unisuff1_s, lm_unisuff2_s, lm_decelec1_s, lm_decelec2_s, lm_frelect1_s, lm_frelect2_s, lm_judcnstr1_s, lm_judcnstr2_s, lm_legcnstr1_s, lm_legcnstr2_s, lm_eqlaw1_s, lm_eqlaw2_s, type="text")
lm_controls_state <- stargazer(controls_state, type="text")
lm_controls_state_matrix <- as.matrix(lm_controls_state)
View(lm_controls_state_matrix)
write_xlsx(as.data.frame(lm_controls_state_matrix), path= "Initial_Regression_Results1.xlsx")


###Interactions ------------------------------------------------------------
#Treatment effects, interaction with party, no controls
int_party_frexp1_nc <- lm(frexp1_recode ~ treat * party, data = p_dem)
summary(int_party_frexp1_nc)

int_party_frexp2_nc <- lm(frexp2 ~ treat * party, data = p_dem)
summary(int_party_frexp2_nc)

int_party_frassc1_nc <- lm(frassc1 ~ treat * party, data = p_dem)
summary(int_party_frassc1_nc)

int_party_frassc2_nc <- lm(frassc2_recode ~ treat * party, data = p_dem)
summary(int_party_frassc2_nc)

int_party_frassc3_nc <- lm(frassc3 ~ treat * party, data = p_dem)
summary(int_party_frassc3_nc)

int_party_unisuff1_nc <- lm(unisuff1 ~ treat * party, data = p_dem)
summary(int_party_unisuff1_nc)

int_party_unisuff2_nc <- lm(unisuff2_recode ~ treat * party, data = p_dem)
summary(int_party_unisuff2_nc)

int_party_decelec1_nc <- lm(decelec1 ~ treat * party, data = p_dem)
summary(int_party_decelec1_nc)

int_party_decelec2_nc <- lm(decelec2_recode ~ treat * party, data = p_dem)
summary(int_party_decelec2_nc)

int_party_frelect1_nc <- lm(frelect1_recode ~ treat * party, data = p_dem)
summary(int_party_frelect1_nc)

int_party_frelect2_nc <- lm(frelect2 ~ treat * party, data = p_dem)
summary(int_party_frelect2_nc)

int_party_judcnstr1_nc <- lm(judcnstr1_recode ~ treat * party, data = p_dem)
summary(int_party_judcnstr1_nc)

int_party_judcnstr2_nc <- lm(judcnstr2 ~ treat * party, data = p_dem)
summary(int_party_judcnstr2_nc)

int_party_legcnstr1_nc <- lm(legcnstr1 ~ treat * party, data = p_dem)
summary(int_party_legcnstr1_nc)

int_party_legcnstr2_nc <- lm(legcnstr2_recode ~ treat * party, data = p_dem)
summary(int_party_legcnstr2_nc)

int_party_eqlaw1_nc <- lm(eqlaw1 ~ treat * party, data = p_dem)
summary(int_party_eqlaw1_nc)

int_party_eqlaw2_nc <- lm(eqlaw2_recode ~ treat * party, data = p_dem)
summary(int_party_eqlaw2_nc)

#Output for treatment effects, interaction with party, no controls
party_nc <- list(int_party_frexp1_nc, int_party_frexp2_nc, int_party_frassc1_nc, int_party_frassc2_nc, int_party_frassc3_nc, int_party_unisuff1_nc, int_party_unisuff2_nc, int_party_decelec1_nc, int_party_decelec2_nc, int_party_frelect1_nc, int_party_frelect2_nc, int_party_judcnstr1_nc, int_party_judcnstr2_nc, int_party_legcnstr1_nc, int_party_legcnstr2_nc, int_party_eqlaw1_nc, int_party_eqlaw2_nc, type = "text")
int_party_nc <- stargazer(party_nc, type = "text")
int_party_nc_matrix <- as.matrix(int_party_nc)
View(int_party_nc_matrix)
write_xlsx(as.data.frame(int_party_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with party, controls (no state)
int_party_frexp1 <- lm(frexp1_recode ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_frexp1)

int_party_frexp2 <- lm(frexp2 ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_frexp2)

int_party_frassc1 <- lm(frassc1 ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_frassc1)

int_party_frassc2 <- lm(frassc2_recode ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_frassc2)

int_party_frassc3 <- lm(frassc3 ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_frassc3)

int_party_unisuff1 <- lm(unisuff1 ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_unisuff1)

int_party_unisuff2 <- lm(unisuff2_recode ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_unisuff2)

int_party_decelec1 <- lm(decelec1 ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_decelec1)

int_party_decelec2 <- lm(decelec2_recode ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_decelec2)

int_party_frelect1 <- lm(frelect1_recode ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_frelect1)

int_party_frelect2 <- lm(frelect2 ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_frelect2)

int_party_judcnstr1 <- lm(judcnstr1_recode ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_judcnstr1)

int_party_judcnstr2 <- lm(judcnstr2 ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_judcnstr2)

int_party_legcnstr1 <- lm(legcnstr1 ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_legcnstr1)

int_party_legcnstr2 <- lm(legcnstr2_recode ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_legcnstr2)

int_party_eqlaw1 <- lm(eqlaw1 ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_eqlaw1)

int_party_eqlaw2 <- lm(eqlaw2_recode ~ treat * party + age + gender + living + edu_n + income_n, data = p_dem)
summary(int_party_eqlaw2)

#Output for treatment effects, interaction with party, controls (no state)
party_controls <- list(int_party_frexp1, int_party_frexp2, int_party_frassc1, int_party_frassc2, int_party_frassc3, int_party_unisuff1, int_party_unisuff2, int_party_decelec1, int_party_decelec2, int_party_frelect1, int_party_frelect2, int_party_judcnstr1, int_party_judcnstr2, int_party_legcnstr1, int_party_legcnstr2, int_party_eqlaw1, int_party_eqlaw2, type = "text")
int_party_controls <- stargazer(party_controls, type = "text")
int_party_controls_matrix <- as.matrix(int_party_controls)
View(int_party_controls_matrix)
write_xlsx(as.data.frame(int_party_controls_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with party, controls (with state)
int_party_frexp1_s <- lm(frexp1_recode ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_frexp1_s)

int_party_frexp2_s <- lm(frexp2 ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_frexp2_s)

int_party_frassc1_s <- lm(frassc1 ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_frassc1_s)

int_party_frassc2_s <- lm(frassc2_recode ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_frassc2_s)

int_party_frassc3_s <- lm(frassc3 ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_frassc3_s)

int_party_unisuff1_s <- lm(unisuff1 ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_unisuff1_s)

int_party_unisuff2_s <- lm(unisuff2_recode ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_unisuff2_s)

int_party_decelec1_s <- lm(decelec1 ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_decelec1_s)

int_party_decelec2_s <- lm(decelec2_recode ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_decelec2_s)

int_party_frelect1_s <- lm(frelect1_recode ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_frelect1_s)

int_party_frelect2_s <- lm(frelect2 ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_frelect2_s)

int_party_judcnstr1_s <- lm(judcnstr1_recode ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_judcnstr1_s)

int_party_judcnstr2_s <- lm(judcnstr2 ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_judcnstr2_s)

int_party_legcnstr1_s <- lm(legcnstr1 ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_legcnstr1_s)

int_party_legcnstr2_s <- lm(legcnstr2_recode ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_legcnstr2_s)

int_party_eqlaw1_s <- lm(eqlaw1 ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_eqlaw1_s)

int_party_eqlaw2_s <- lm(eqlaw2_recode ~ treat * party + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_party_eqlaw2_s)

#Output for treatment effects, interaction with party, controls (with state)
party_controls_state <- list(int_party_frexp1_s, int_party_frexp2_s, int_party_frassc1_s, int_party_frassc2_s, int_party_frassc3_s, int_party_unisuff1_s, int_party_unisuff2_s, int_party_decelec1_s, int_party_decelec2_s, int_party_frelect1_s, int_party_frelect2_s, int_party_judcnstr1_s, int_party_judcnstr2_s, int_party_legcnstr1_s, int_party_legcnstr2_s, int_party_eqlaw1_s, int_party_eqlaw2_s, type = "text")
int_party_controls_state <- stargazer(party_controls_state, type = "text")
int_party_controls_state_matrix <- as.matrix(int_party_controls_state)
View(int_party_controls_state_matrix)
write_xlsx(as.data.frame(int_party_controls_state_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with pers_econ_recode, no controls
class(p_dem$pers_econ_recode)
p_dem$pers_econ_recode <- as.numeric(p_dem$pers_econ_recode)

int_persecon_frexp1_nc <- lm(frexp1_recode ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_frexp1_nc)

int_persecon_frexp2_nc <- lm(frexp2 ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_frexp2_nc)

int_persecon_frassc1_nc <- lm(frassc1 ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_frassc1_nc)

int_persecon_frassc2_nc <- lm(frassc2_recode ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_frassc2_nc)

int_persecon_frassc3_nc <- lm(frassc3 ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_frassc3_nc)

int_persecon_unisuff1_nc <- lm(unisuff1 ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_unisuff1_nc)

int_persecon_unisuff2_nc <- lm(unisuff2_recode ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_unisuff2_nc)

int_persecon_decelec1_nc <- lm(decelec1 ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_decelec1_nc)

int_persecon_decelec2_nc <- lm(decelec2_recode ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_decelec2_nc)

int_persecon_frelect1_nc <- lm(frelect1_recode ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_frelect1_nc)

int_persecon_frelect2_nc <- lm(frelect2 ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_frelect2_nc)

int_persecon_judcnstr1_nc <- lm(judcnstr1_recode ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_judcnstr1_nc)

int_persecon_judcnstr2_nc <- lm(judcnstr2 ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_judcnstr2_nc)

int_persecon_legcnstr1_nc <- lm(legcnstr1 ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_legcnstr1_nc)

int_persecon_legcnstr2_nc <- lm(legcnstr2_recode ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_legcnstr2_nc)

int_persecon_eqlaw1_nc <- lm(eqlaw1 ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_eqlaw1_nc)

int_persecon_eqlaw2_nc <- lm(eqlaw2_recode ~ treat * pers_econ_recode, data = p_dem)
summary(int_persecon_eqlaw2_nc)

#Output for treatment effects, interaction with pers_econ_recode, no controls
persecon_nc <- list(int_persecon_frexp1_nc, int_persecon_frexp2_nc, int_persecon_frassc1_nc, int_persecon_frassc2_nc, int_persecon_frassc3_nc, int_persecon_unisuff1_nc, int_persecon_unisuff2_nc, int_persecon_decelec1_nc, int_persecon_decelec2_nc, int_persecon_frelect1_nc, int_persecon_frelect2_nc, int_persecon_judcnstr1_nc, int_persecon_judcnstr2_nc, int_persecon_legcnstr1_nc, int_persecon_legcnstr2_nc, int_persecon_eqlaw1_nc, int_persecon_eqlaw2_nc, type = "text")
int_persecon_nc <- stargazer(persecon_nc, type = "text")
int_persecon_nc_matrix <- as.matrix(int_persecon_nc)
View(int_persecon_nc_matrix)
write_xlsx(as.data.frame(int_persecon_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with pers_econ_recode, controls (with state)
int_persecon_frexp1 <- lm(frexp1_recode ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_frexp1)

int_persecon_frexp2 <- lm(frexp2 ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_frexp2)

int_persecon_frassc1 <- lm(frassc1 ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_frassc1)

int_persecon_frassc2 <- lm(frassc2_recode ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_frassc2)

int_persecon_frassc3 <- lm(frassc3 ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_frassc3)

int_persecon_unisuff1 <- lm(unisuff1 ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_unisuff1)

int_persecon_unisuff2 <- lm(unisuff2_recode ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_unisuff2)

int_persecon_decelec1 <- lm(decelec1 ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_decelec1)

int_persecon_decelec2 <- lm(decelec2_recode ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_decelec2)

int_persecon_frelect1 <- lm(frelect1_recode ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_frelect1)

int_persecon_frelect2 <- lm(frelect2 ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_frelect2)

int_persecon_judcnstr1 <- lm(judcnstr1_recode ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_judcnstr1)

int_persecon_judcnstr2 <- lm(judcnstr2 ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_judcnstr2)

int_persecon_legcnstr1 <- lm(legcnstr1 ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_legcnstr1)

int_persecon_legcnstr2 <- lm(legcnstr2_recode ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_legcnstr2)

int_persecon_eqlaw1 <- lm(eqlaw1 ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_eqlaw1)

int_persecon_eqlaw2 <- lm(eqlaw2_recode ~ treat * pers_econ_recode + age + gender + living + edu_n + income_n + state, data = p_dem)
summary(int_persecon_eqlaw2)

#Output for treatment effects, interactions with pers_econ_recode, controls (with state)
persecon <- list(int_persecon_frexp1, int_persecon_frexp2, int_persecon_frassc1, int_persecon_frassc2, int_persecon_frassc3, int_persecon_unisuff1, int_persecon_unisuff2, int_persecon_decelec1, int_persecon_decelec2, int_persecon_frelect1, int_persecon_frelect2, int_persecon_judcnstr1, int_persecon_judcnstr2, int_persecon_legcnstr1, int_persecon_legcnstr2, int_persecon_eqlaw1, int_persecon_eqlaw2, type = "text")
int_persecon <- stargazer(persecon, type = "text")
int_persecon_matrix <- as.matrix(int_persecon)
View(int_persecon_matrix)
write_xlsx(as.data.frame(int_persecon_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with pres, no controls
p_dem$pres <- as.factor(p_dem$pres)
int_pres_frexp1_nc <- lm(frexp1_recode ~ treat * pres, data = p_dem)
summary(int_pres_frexp1_nc)

int_pres_frexp2_nc <- lm(frexp2 ~ treat * pres, data = p_dem)
summary(int_pres_frexp2_nc)

int_pres_frassc1_nc <- lm(frassc1 ~ treat * pres, data = p_dem)
summary(int_pres_frassc1_nc)

int_pres_frassc2_nc <- lm(frassc2_recode ~ treat * pres, data = p_dem)
summary(int_pres_frassc2_nc)

int_pres_frassc3_nc <- lm(frassc3 ~ treat * pres, data = p_dem)
summary(int_pres_frassc3_nc)

int_pres_unisuff1_nc <- lm(unisuff1 ~ treat * pres, data = p_dem)
summary(int_pres_unisuff1_nc)

int_pres_unisuff2_nc <- lm(unisuff2_recode ~ treat * pres, data = p_dem)
summary(int_pres_unisuff2_nc)

int_pres_decelec1_nc <- lm(decelec1 ~ treat * pres, data = p_dem)
summary(int_pres_decelec1_nc)

int_pres_decelec2_nc <- lm(decelec2_recode ~ treat * pres, data = p_dem)
summary(int_pres_decelec2_nc)

int_pres_frelect1_nc <- lm(frelect1_recode ~ treat * pres, data = p_dem)
summary(int_pres_frelect1_nc)

int_pres_frelect2_nc <- lm(frelect2 ~ treat * pres, data = p_dem)
summary(int_pres_frelect2_nc)

int_pres_judcnstr1_nc <- lm(judcnstr1_recode ~ treat * pres, data = p_dem)
summary(int_pres_judcnstr1_nc)

int_pres_judcnstr2_nc <- lm(judcnstr2 ~ treat * pres, data = p_dem)
summary(int_pres_judcnstr2_nc)

int_pres_legcnstr1_nc <- lm(legcnstr1 ~ treat * pres, data = p_dem)
summary(int_pres_legcnstr1_nc)

int_pres_legcnstr2_nc <- lm(legcnstr2_recode ~ treat * pres, data = p_dem)
summary(int_pres_legcnstr2_nc)

int_pres_eqlaw1_nc <- lm(eqlaw1 ~ treat * pres, data = p_dem)
summary(int_pres_eqlaw1_nc)

int_pres_eqlaw2_nc <- lm(eqlaw2_recode ~ treat * pres, data = p_dem)
summary(int_pres_eqlaw2_nc)

#Output for treatment effects, interactions with pres, no controls
pres_nc <- list(int_pres_frexp1_nc, int_pres_frexp2_nc, int_pres_frassc1_nc, int_pres_frassc2_nc, int_pres_frassc3_nc, int_pres_unisuff1_nc, int_pres_unisuff2_nc, int_pres_decelec1_nc, int_pres_decelec2_nc, int_pres_frelect1_nc, int_pres_frelect2_nc, int_pres_judcnstr1_nc, int_pres_judcnstr2_nc, int_pres_legcnstr1_nc, int_pres_legcnstr2_nc, int_pres_eqlaw1_nc, int_pres_eqlaw2_nc, type = "text")
int_pres_nc <- stargazer(pres_nc, type = "text")
int_pres_nc_matrix <- as.matrix(int_pres_nc)
View(int_pres_nc_matrix)
write_xlsx(as.data.frame(int_pres_nc_matrix), path = "Interaction_Regression_Results.xlsx")

#Treatment effects, interaction with pres, controls (with state)
summary(int_pres_frexp1 <- lm(frexp1_recode ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_frexp2 <- lm(frexp2 ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_frassc1 <- lm(frassc1 ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_frassc2 <- lm(frassc2_recode ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_frassc3 <- lm(frassc3 ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_unisuff1 <- lm(unisuff1 ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_unisuff2 <- lm(unisuff2_recode ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_decelec1 <- lm(decelec1 ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_decelec2 <- lm(decelec2_recode ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_frelect1 <- lm(frelect1_recode ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_frelect2 <- lm(frelect2 ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_judcnstr1 <- lm(judcnstr1_recode ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_judcnstr2 <- lm(judcnstr2 ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_legcnstr1 <- lm(legcnstr1 ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_legcnstr2 <- lm(legcnstr2_recode ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_eqlaw1 <- lm(eqlaw1 ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_pres_eqlaw2 <- lm(eqlaw2_recode ~ treat * pres + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with pres, controls (with state)
pres <- list(int_pres_frexp1, int_pres_frexp2, int_pres_frassc1, int_pres_frassc2, int_pres_frassc3, int_pres_unisuff1, int_pres_unisuff2, int_pres_decelec1, int_pres_decelec2, int_pres_frelect1, int_pres_frelect2, int_pres_judcnstr1, int_pres_judcnstr2, int_pres_legcnstr1, int_pres_legcnstr2, int_pres_eqlaw1, int_pres_eqlaw2, type = "text")
int_pres <- stargazer(pres, type = "text")
int_pres_matrix <- as.matrix(int_pres)
View(int_pres_matrix)
write_xlsx(as.data.frame(int_pres_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with pers_vio_recode, no controls
p_dem$pers_vio_recode <- as.numeric(p_dem$pers_vio_recode)
summary(int_persvio_frexp1_nc <- lm(frexp1_recode ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_frexp2_nc <- lm(frexp2 ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_frassc1_nc <- lm(frassc1 ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_frassc2_nc <- lm(frassc2_recode ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_frassc3_nc <- lm(frassc3 ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_unisuff1_nc <- lm(unisuff1 ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_unisuff2_nc <- lm(unisuff2_recode ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_decelec1_nc <- lm(decelec1 ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_decelec2_nc <- lm(decelec2_recode ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_frelect1_nc <- lm(frelect1_recode ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_frelect2_nc <- lm(frelect2 ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_judcnstr1_nc <- lm(judcnstr1_recode ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_judcnstr2_nc <- lm(judcnstr2 ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_legcnstr1_nc <- lm(legcnstr1 ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_legcnstr2_nc <- lm(legcnstr2_recode ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_eqlaw1_nc <- lm(eqlaw1 ~ treat * pers_vio_recode, data = p_dem))

summary(int_persvio_eqlaw2_nc <- lm(eqlaw2_recode ~ treat * pers_vio_recode, data = p_dem))

#Output for treatment effects, interactions with pres, no controls
persvio_nc <- list(int_persvio_frexp1_nc, int_persvio_frexp2_nc, int_persvio_frassc1_nc, int_persvio_frassc2_nc, int_persvio_frassc3_nc, int_persvio_unisuff1_nc, int_persvio_unisuff2_nc, int_persvio_decelec1_nc, int_persvio_decelec2_nc, int_persvio_frelect1_nc, int_persvio_frelect2_nc, int_persvio_judcnstr1_nc, int_persvio_judcnstr2_nc, int_persvio_legcnstr1_nc, int_persvio_legcnstr2_nc, int_persvio_eqlaw1_nc, int_persvio_eqlaw2_nc, type = "text")
int_persvio_nc <- stargazer(persvio_nc, type = "text")
int_persvio_nc_matrix <- as.matrix(int_persvio_nc)
View(int_persvio_nc_matrix)
write_xlsx(as.data.frame(int_persvio_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with pers_vio_recode, controls (with state)
summary(int_persvio_frexp1 <- lm(frexp1_recode ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_frexp2 <- lm(frexp2 ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_frassc1 <- lm(frassc1 ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_frassc2 <- lm(frassc2_recode ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_frassc3 <- lm(frassc3 ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_unisuff1 <- lm(unisuff1 ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_unisuff2 <- lm(unisuff2_recode ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_decelec1 <- lm(decelec1 ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_decelec2 <- lm(decelec2_recode ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_frelect1 <- lm(frelect1_recode ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_frelect2 <- lm(frelect2 ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_judcnstr1 <- lm(judcnstr1_recode ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_judcnstr2 <- lm(judcnstr2 ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_legcnstr1 <- lm(legcnstr1 ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_legcnstr2 <- lm(legcnstr2_recode ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_eqlaw1 <- lm(eqlaw1 ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persvio_eqlaw2 <- lm(eqlaw2_recode ~ treat * pers_vio_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with pers_vio_recode, controls (with state)
persvio <- list(int_persvio_frexp1, int_persvio_frexp2, int_persvio_frassc1, int_persvio_frassc2, int_persvio_frassc3, int_persvio_unisuff1, int_persvio_unisuff2, int_persvio_decelec1, int_persvio_decelec2, int_persvio_frelect1, int_persvio_frelect2, int_persvio_judcnstr1, int_persvio_judcnstr2, int_persvio_legcnstr1, int_persvio_legcnstr2, int_persvio_eqlaw1, int_persvio_eqlaw2, type = "text")
int_persvio <- stargazer(persvio, type = "text")
int_persvio_matrix <- as.matrix(int_persvio)
View(int_persvio_matrix)
write_xlsx(as.data.frame(int_persvio_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with count_econ, no controls
p_dem$count_econ <- as.numeric(p_dem$count_econ)
summary(int_countecon_frexp1_nc <- lm(frexp1_recode ~ treat * count_econ, data = p_dem))

summary(int_countecon_frexp2_nc <- lm(frexp2 ~ treat * count_econ, data = p_dem))

summary(int_countecon_frassc1_nc <- lm(frassc1 ~ treat * count_econ, data = p_dem))

summary(int_countecon_frassc2_nc <- lm(frassc2_recode ~ treat * count_econ, data = p_dem))

summary(int_countecon_frassc3_nc <- lm(frassc3 ~ treat * count_econ, data = p_dem))

summary(int_countecon_unisuff1_nc <- lm(unisuff1 ~ treat * count_econ, data = p_dem))

summary(int_countecon_unisuff2_nc <- lm(unisuff2_recode ~ treat * count_econ, data = p_dem))

summary(int_countecon_decelec1_nc <- lm(decelec1 ~ treat * count_econ, data = p_dem))

summary(int_countecon_decelec2_nc <- lm(decelec2_recode ~ treat * count_econ, data = p_dem))

summary(int_countecon_frelect1_nc <- lm(frelect1_recode ~ treat * count_econ, data = p_dem))

summary(int_countecon_frelect2_nc <- lm(frelect2 ~ treat * count_econ, data = p_dem))

summary(int_countecon_judcnstr1_nc <- lm(judcnstr1_recode ~ treat * count_econ, data = p_dem))

summary(int_countecon_judcnstr2_nc <- lm(judcnstr2 ~ treat * count_econ, data = p_dem))

summary(int_countecon_legcnstr1_nc <- lm(legcnstr1 ~ treat * count_econ, data = p_dem))

summary(int_countecon_legcnstr2_nc <- lm(legcnstr2_recode ~ treat * count_econ, data = p_dem))

summary(int_countecon_eqlaw1_nc <- lm(eqlaw1 ~ treat * count_econ, data = p_dem))

summary(int_countecon_eqlaw2_nc <- lm(eqlaw2_recode ~ treat * count_econ, data = p_dem))

#Output for treatment effects, interactions with count_econ, no controls
countecon_nc <- list(int_countecon_frexp1_nc, int_countecon_frexp2_nc, int_countecon_frassc1_nc, int_countecon_frassc2_nc, int_countecon_frassc3_nc, int_countecon_unisuff1_nc, int_countecon_unisuff2_nc, int_countecon_decelec1_nc, int_countecon_decelec2_nc, int_countecon_frelect1_nc, int_countecon_frelect2_nc, int_countecon_judcnstr1_nc, int_countecon_judcnstr2_nc, int_countecon_legcnstr1_nc, int_countecon_legcnstr2_nc, int_countecon_eqlaw1_nc, int_countecon_eqlaw2_nc, type = "text")
int_countecon_nc <- stargazer(countecon_nc, type = "text")
int_countecon_nc_matrix <- as.matrix(int_countecon_nc)
View(int_countecon_nc_matrix)
write_xlsx(as.data.frame(int_countecon_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with count_econ, controls (with state)
summary(int_countecon_frexp1 <- lm(frexp1_recode ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_frexp2 <- lm(frexp2 ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_frassc1 <- lm(frassc1 ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_frassc2 <- lm(frassc2_recode ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_frassc3 <- lm(frassc3 ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_unisuff1 <- lm(unisuff1 ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_unisuff2 <- lm(unisuff2_recode ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_decelec1 <- lm(decelec1 ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_decelec2 <- lm(decelec2_recode ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_frelect1 <- lm(frelect1_recode ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_frelect2 <- lm(frelect2 ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_judcnstr1 <- lm(judcnstr1_recode ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_judcnstr2 <- lm(judcnstr2 ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_legcnstr1 <- lm(legcnstr1 ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_legcnstr2 <- lm(legcnstr2_recode ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_eqlaw1 <- lm(eqlaw1 ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countecon_eqlaw2 <- lm(eqlaw2_recode ~ treat * count_econ + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with count_econ, controls (with state)
countecon <- list(int_countecon_frexp1, int_countecon_frexp2, int_countecon_frassc1, int_countecon_frassc2, int_countecon_frassc3, int_countecon_unisuff1, int_countecon_unisuff2, int_countecon_decelec1, int_countecon_decelec2, int_countecon_frelect1, int_countecon_frelect2, int_countecon_judcnstr1, int_countecon_judcnstr2, int_countecon_legcnstr1, int_countecon_legcnstr2, int_countecon_eqlaw1, int_countecon_eqlaw2, type = "text")
int_countecon <- stargazer(countecon, type = "text")
int_countecon_matrix <- as.matrix(int_countecon)
View(int_countecon_matrix)
write_xlsx(as.data.frame(int_countecon_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with count_vio, no controls
p_dem$count_vio <- as.numeric(p_dem$count_vio)
summary(int_countvio_frexp1_nc <- lm(frexp1_recode ~ treat * count_vio, data = p_dem))

summary(int_countvio_frexp2_nc <- lm(frexp2 ~ treat * count_vio, data = p_dem))

summary(int_countvio_frassc1_nc <- lm(frassc1 ~ treat * count_vio, data = p_dem))

summary(int_countvio_frassc2_nc <- lm(frassc2_recode ~ treat * count_vio, data = p_dem))

summary(int_countvio_frassc3_nc <- lm(frassc3 ~ treat * count_vio, data = p_dem))

summary(int_countvio_unisuff1_nc <- lm(unisuff1 ~ treat * count_vio, data = p_dem))

summary(int_countvio_unisuff2_nc <- lm(unisuff2_recode ~ treat * count_vio, data = p_dem))

summary(int_countvio_decelec1_nc <- lm(decelec1 ~ treat * count_vio, data = p_dem))

summary(int_countvio_decelec2_nc <- lm(decelec2_recode ~ treat * count_vio, data = p_dem))

summary(int_countvio_frelect1_nc <- lm(frelect1_recode ~ treat * count_vio, data = p_dem))

summary(int_countvio_frelect2_nc <- lm(frelect2 ~ treat * count_vio, data = p_dem))

summary(int_countvio_judcnstr1_nc <- lm(judcnstr1_recode ~ treat * count_vio, data = p_dem))

summary(int_countvio_judcnstr2_nc <- lm(judcnstr2 ~ treat * count_vio, data = p_dem))

summary(int_countvio_legcnstr1_nc <- lm(legcnstr1 ~ treat * count_vio, data = p_dem))

summary(int_countvio_legcnstr2_nc <- lm(legcnstr2_recode ~ treat * count_vio, data = p_dem))

summary(int_countvio_eqlaw1_nc <- lm(eqlaw1 ~ treat * count_vio, data = p_dem))

summary(int_countvio_eqlaw2_nc <- lm(eqlaw2_recode ~ treat * count_vio, data = p_dem))

#Output for treatment effects, interactions with count_vio, no controls
countvio_nc <- list(int_countvio_frexp1_nc, int_countvio_frexp2_nc, int_countvio_frassc1_nc, int_countvio_frassc2_nc, int_countvio_frassc3_nc, int_countvio_unisuff1_nc, int_countvio_unisuff2_nc, int_countvio_decelec1_nc, int_countvio_decelec2_nc, int_countvio_frelect1_nc, int_countvio_frelect2_nc, int_countvio_judcnstr1_nc, int_countvio_judcnstr2_nc, int_countvio_legcnstr1_nc, int_countvio_legcnstr2_nc, int_countvio_eqlaw1_nc, int_countvio_eqlaw2_nc, type = "text")
int_countvio_nc <- stargazer(countvio_nc, type = "text")
int_countvio_nc_matrix <- as.matrix(int_countvio_nc)
View(int_countvio_nc_matrix)
write_xlsx(as.data.frame(int_countvio_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with count_vio, controls (with state)
summary(int_countvio_frexp1 <- lm(frexp1_recode ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_frexp2 <- lm(frexp2 ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_frassc1 <- lm(frassc1 ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_frassc2 <- lm(frassc2_recode ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_frassc3 <- lm(frassc3 ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_unisuff1 <- lm(unisuff1 ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_unisuff2 <- lm(unisuff2_recode ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_decelec1 <- lm(decelec1 ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_decelec2 <- lm(decelec2_recode ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_frelect1 <- lm(frelect1_recode ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_frelect2 <- lm(frelect2 ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_judcnstr1 <- lm(judcnstr1_recode ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_judcnstr2 <- lm(judcnstr2 ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_legcnstr1 <- lm(legcnstr1 ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_legcnstr2 <- lm(legcnstr2_recode ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_eqlaw1 <- lm(eqlaw1 ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countvio_eqlaw2 <- lm(eqlaw2_recode ~ treat * count_vio + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with count_vio, controls (with state)
countvio <- list(int_countvio_frexp1, int_countvio_frexp2, int_countvio_frassc1, int_countvio_frassc2, int_countvio_frassc3, int_countvio_unisuff1, int_countvio_unisuff2, int_countvio_decelec1, int_countvio_decelec2, int_countvio_frelect1, int_countvio_frelect2, int_countvio_judcnstr1, int_countvio_judcnstr2, int_countvio_legcnstr1, int_countvio_legcnstr2, int_countvio_eqlaw1, int_countvio_eqlaw2, type = "text")
int_countvio <- stargazer(countvio, type = "text")
int_countvio_matrix <- as.matrix(int_countvio)
View(int_countvio_matrix)
write_xlsx(as.data.frame(int_countvio_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with politpart_recode, no controls
p_dem$politpart_recode <- as.numeric(p_dem$politpart_recode)
summary(int_politpart_frexp1_nc <- lm(frexp1_recode ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_frexp2_nc <- lm(frexp2 ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_frassc1_nc <- lm(frassc1 ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_frassc2_nc <- lm(frassc2_recode ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_frassc3_nc <- lm(frassc3 ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_unisuff1_nc <- lm(unisuff1 ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_unisuff2_nc <- lm(unisuff2_recode ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_decelec1_nc <- lm(decelec1 ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_decelec2_nc <- lm(decelec2_recode ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_frelect1_nc <- lm(frelect1_recode ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_frelect2_nc <- lm(frelect2 ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_judcnstr1_nc <- lm(judcnstr1_recode ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_judcnstr2_nc <- lm(judcnstr2 ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_legcnstr1_nc <- lm(legcnstr1 ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_legcnstr2_nc <- lm(legcnstr2_recode ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_eqlaw1_nc <- lm(eqlaw1 ~ treat * politpart_recode, data = p_dem))

summary(int_politpart_eqlaw2_nc <- lm(eqlaw2_recode ~ treat * politpart_recode, data = p_dem))

#Output for treatment effects, interactions with politpart_recode, no controls
politpart_nc <- list(int_politpart_frexp1_nc, int_politpart_frexp2_nc, int_politpart_frassc1_nc, int_politpart_frassc2_nc, int_politpart_frassc3_nc, int_politpart_unisuff1_nc, int_politpart_unisuff2_nc, int_politpart_decelec1_nc, int_politpart_decelec2_nc, int_politpart_frelect1_nc, int_politpart_frelect2_nc, int_politpart_judcnstr1_nc, int_politpart_judcnstr2_nc, int_politpart_legcnstr1_nc, int_politpart_legcnstr2_nc, int_politpart_eqlaw1_nc, int_politpart_eqlaw2_nc, type = "text")
int_politpart_nc <- stargazer(politpart_nc, type = "text")
int_politpart_nc_matrix <- as.matrix(int_politpart_nc)
View(int_politpart_nc_matrix)
write_xlsx(as.data.frame(int_politpart_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with politpart_recode, controls (with state)
summary(int_politpart_frexp1 <- lm(frexp1_recode ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_frexp2 <- lm(frexp2 ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_frassc1 <- lm(frassc1 ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_frassc2 <- lm(frassc2_recode ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_frassc3 <- lm(frassc3 ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_unisuff1 <- lm(unisuff1 ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_unisuff2 <- lm(unisuff2_recode ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_decelec1 <- lm(decelec1 ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_decelec2 <- lm(decelec2_recode ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_frelect1 <- lm(frelect1_recode ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_frelect2 <- lm(frelect2 ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_judcnstr1 <- lm(judcnstr1_recode ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_judcnstr2 <- lm(judcnstr2 ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_legcnstr1 <- lm(legcnstr1 ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_legcnstr2 <- lm(legcnstr2_recode ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_eqlaw1 <- lm(eqlaw1 ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpart_eqlaw2 <- lm(eqlaw2_recode ~ treat * politpart_recode + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with politpart_recode, controls (with state)
politpart <- list(int_politpart_frexp1, int_politpart_frexp2, int_politpart_frassc1, int_politpart_frassc2, int_politpart_frassc3, int_politpart_unisuff1, int_politpart_unisuff2, int_politpart_decelec1, int_politpart_decelec2, int_politpart_frelect1, int_politpart_frelect2, int_politpart_judcnstr1, int_politpart_judcnstr2, int_politpart_legcnstr1, int_politpart_legcnstr2, int_politpart_eqlaw1, int_politpart_eqlaw2, type = "text")
int_politpart <- stargazer(politpart, type = "text")
int_politpart_matrix <- as.matrix(int_politpart)
View(int_politpart_matrix)
write_xlsx(as.data.frame(int_politpart_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with elite, no controls
p_dem$elite <- as.numeric(p_dem$elite)
summary(int_elite_frexp1_nc <- lm(frexp1_recode ~ treat * elite, data = p_dem))

summary(int_elite_frexp2_nc <- lm(frexp2 ~ treat * elite, data = p_dem))

summary(int_elite_frassc1_nc <- lm(frassc1 ~ treat * elite, data = p_dem))

summary(int_elite_frassc2_nc <- lm(frassc2 ~ treat * elite, data = p_dem))

summary(int_elite_frassc3_nc <- lm(frassc3 ~ treat * elite, data = p_dem))

summary(int_elite_unisuff1_nc <- lm(unisuff1 ~ treat * elite, data = p_dem))

summary(int_elite_unisuff2_nc <- lm(unisuff2 ~ treat * elite, data = p_dem))

summary(int_elite_decelec1_nc <- lm(decelec1 ~ treat * elite, data = p_dem))

summary(int_elite_decelec2_nc <- lm(decelec2 ~ treat * elite, data = p_dem))

summary(int_elite_frelect1_nc <- lm(frelect1 ~ treat * elite, data = p_dem))

summary(int_elite_frelect2_nc <- lm(frelect2 ~ treat * elite, data = p_dem))

summary(int_elite_judcnstr1_nc <- lm(judcnstr1 ~ treat * elite, data = p_dem))

summary(int_elite_judcnstr2_nc <- lm(judcnstr2 ~ treat * elite, data = p_dem))

summary(int_elite_legcnstr1_nc <- lm(legcnstr1 ~ treat * elite, data = p_dem))

summary(int_elite_legcnstr2_nc <- lm(legcnstr2 ~ treat * elite, data = p_dem))

summary(int_elite_eqlaw1_nc <- lm(eqlaw1 ~ treat * elite, data = p_dem))

summary(int_elite_eqlaw2_nc <- lm(eqlaw2 ~ treat * elite, data = p_dem))

#Output for treatment effects, interactions with elite, no controls
elite_nc <- list(int_elite_frexp1_nc, int_elite_frexp2_nc, int_elite_frassc1_nc, int_elite_frassc2_nc, int_elite_frassc3_nc, int_elite_unisuff1_nc, int_elite_unisuff2_nc, int_elite_decelec1_nc, int_elite_decelec2_nc, int_elite_frelect1_nc, int_elite_frelect2_nc, int_elite_judcnstr1_nc, int_elite_judcnstr2_nc, int_elite_legcnstr1_nc, int_elite_legcnstr2_nc, int_elite_eqlaw1_nc, int_elite_eqlaw2_nc, type = "text")
int_elite_nc <- stargazer(elite_nc, type = "text")
int_elite_nc_matrix <- as.matrix(int_elite_nc)
View(int_elite_nc_matrix)
write_xlsx(as.data.frame(int_elite_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with elite, controls (with state)
summary(int_elite_frexp1 <- lm(frexp1_recode ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_frexp2 <- lm(frexp2 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_frassc1 <- lm(frassc1 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_frassc2 <- lm(frassc2 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_frassc3 <- lm(frassc3 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_unisuff1 <- lm(unisuff1 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_unisuff2 <- lm(unisuff2 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_decelec1 <- lm(decelec1 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_decelec2 <- lm(decelec2 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_frelect1 <- lm(frelect1 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_frelect2 <- lm(frelect2 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_judcnstr1 <- lm(judcnstr1 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_judcnstr2 <- lm(judcnstr2 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_legcnstr1 <- lm(legcnstr1 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_legcnstr2 <- lm(legcnstr2 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_eqlaw1 <- lm(eqlaw1 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elite_eqlaw2 <- lm(eqlaw2 ~ treat * elite + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with elite, controls (with state)
elite <- list(int_elite_frexp1, int_elite_frexp2, int_elite_frassc1, int_elite_frassc2, int_elite_frassc3, int_elite_unisuff1, int_elite_unisuff2, int_elite_decelec1, int_elite_decelec2, int_elite_frelect1, int_elite_frelect2, int_elite_judcnstr1, int_elite_judcnstr2, int_elite_legcnstr1, int_elite_legcnstr2, int_elite_eqlaw1, int_elite_eqlaw2, type = "text")
int_elite <- stargazer(elite, type = "text")
int_elite_matrix <- as.matrix(int_elite)
View(int_elite_matrix)
write_xlsx(as.data.frame(int_elite_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with state, no controls
summary(int_state_frexp1_nc <- lm(frexp1_recode ~ treat * state, data = p_dem))

summary(int_state_frexp2_nc <- lm(frexp2 ~ treat * state, data = p_dem))

summary(int_state_frassc1_nc <- lm(frassc1 ~ treat * state, data = p_dem))

summary(int_state_frassc2_nc <- lm(frassc2 ~ treat * state, data = p_dem))

summary(int_state_frassc3_nc <- lm(frassc3 ~ treat * state, data = p_dem))

summary(int_state_unisuff1_nc <- lm(unisuff1 ~ treat * state, data = p_dem))

summary(int_state_unisuff2_nc <- lm(unisuff2 ~ treat * state, data = p_dem))

summary(int_state_decelec1_nc <- lm(decelec1 ~ treat * state, data = p_dem))

summary(int_state_decelec2_nc <- lm(decelec2 ~ treat * state, data = p_dem))

summary(int_state_frelect1_nc <- lm(frelect1 ~ treat * state, data = p_dem))

summary(int_state_frelect2_nc <- lm(frelect2 ~ treat * state, data = p_dem))

summary(int_state_judcnstr1_nc <- lm(judcnstr1 ~ treat * state, data = p_dem))

summary(int_state_judcnstr2_nc <- lm(judcnstr2 ~ treat * state, data = p_dem))

summary(int_state_legcnstr1_nc <- lm(legcnstr1 ~ treat * state, data = p_dem))

summary(int_state_legcnstr2_nc <- lm(legcnstr2 ~ treat * state, data = p_dem))

summary(int_state_eqlaw1_nc <- lm(eqlaw1 ~ treat * state, data = p_dem))

summary(int_state_eqlaw2_nc <- lm(eqlaw2 ~ treat * state, data = p_dem))

#Output for treatment effects, interactions with state, no controls
state_nc <- list(int_state_frexp1_nc, int_state_frexp2_nc, int_state_frassc1_nc, int_state_frassc2_nc, int_state_frassc3_nc, int_state_unisuff1_nc, int_state_unisuff2_nc, int_state_decelec1_nc, int_state_decelec2_nc, int_state_frelect1_nc, int_state_frelect2_nc, int_state_judcnstr1_nc, int_state_judcnstr2_nc, int_state_legcnstr1_nc, int_state_legcnstr2_nc, int_state_eqlaw1_nc, int_state_eqlaw2_nc, type = "text")
int_state_nc <- stargazer(state_nc, type = "text")
int_state_nc_matrix <- as.matrix(int_state_nc)
View(int_state_nc_matrix)
write_xlsx(as.data.frame(int_state_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with state, controls
summary(int_state_frexp1 <- lm(frexp1_recode ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_frexp2 <- lm(frexp2 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_frassc1 <- lm(frassc1 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_frassc2 <- lm(frassc2 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_frassc3 <- lm(frassc3 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_unisuff1 <- lm(unisuff1 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_unisuff2 <- lm(unisuff2 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_decelec1 <- lm(decelec1 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_decelec2 <- lm(decelec2 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_frelect1 <- lm(frelect1 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_frelect2 <- lm(frelect2 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_judcnstr1 <- lm(judcnstr1 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_judcnstr2 <- lm(judcnstr2 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_legcnstr1 <- lm(legcnstr1 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_legcnstr2 <- lm(legcnstr2 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_eqlaw1 <- lm(eqlaw1 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_state_eqlaw2 <- lm(eqlaw2 ~ treat * state + age + gender + living + edu_n + income_n, data = p_dem))

#Output for treatment effects, interactions with state, controls
state <- list(int_state_frexp1, int_state_frexp2, int_state_frassc1, int_state_frassc2, int_state_frassc3, int_state_unisuff1, int_state_unisuff2, int_state_decelec1, int_state_decelec2, int_state_frelect1, int_state_frelect2, int_state_judcnstr1, int_state_judcnstr2, int_state_legcnstr1, int_state_legcnstr2, int_state_eqlaw1, int_state_eqlaw2, type = "text")
int_state <- stargazer(state, type = "text")
int_state_matrix <- as.matrix(int_state)
View(int_state_matrix)
write_xlsx(as.data.frame(int_state_matrix), path = "Interaction_Regression_Results.xlsx")


###Rewriting Moderators to be Option 5 vs. All Other Options -----------------------
p_dem$pres_bi1 <- car::recode(p_dem$pres, '
                                     "5" = "1";
                                     "4" = "0";
                                     "3" = "0";
                                     "2" = "0";
                                     "1" = "0"')

p_dem$pers_econ_recode_bi1 <- car::recode(p_dem$pers_econ_recode, '
                                     "5" = "1";
                                     "4" = "0";
                                     "3" = "0";
                                     "2" = "0";
                                     "1" = "0"')

p_dem$pers_vio_recode_bi1 <- car::recode(p_dem$pers_vio_recode, '
                                     "5" = "1";
                                     "4" = "0";
                                     "3" = "0";
                                     "2" = "0";
                                     "1" = "0"')

p_dem$count_econ_bi1 <- car::recode(p_dem$count_econ, '
                                     "5" = "1";
                                     "4" = "0";
                                     "3" = "0";
                                     "2" = "0";
                                     "1" = "0"')

p_dem$count_vio_bi1 <- car::recode(p_dem$count_vio, '
                                     "5" = "1";
                                     "4" = "0";
                                     "3" = "0";
                                     "2" = "0";
                                     "1" = "0"')

p_dem$politpart_recode_bi1 <- car::recode(p_dem$politpart_recode, '
                                     "5" = "1";
                                     "4" = "0";
                                     "3" = "0";
                                     "2" = "0";
                                     "1" = "0"')

p_dem$elite_bi1 <- car::recode(p_dem$elite, '
                                     "5" = "1";
                                     "4" = "0";
                                     "3" = "0";
                                     "2" = "0";
                                     "1" = "0"')


###Interactions with Recoded Moderators ------------------------------------
#Treatment effects, interaction with pres_bi1, no controls
summary(int_presbi1_frexp1_nc <- lm(frexp1_recode ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_frexp2_nc <- lm(frexp2 ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_frassc1_nc <- lm(frassc1 ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_frassc2_nc <- lm(frassc2 ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_frassc3_nc <- lm(frassc3 ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_unisuff1_nc <- lm(unisuff1 ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_unisuff2_nc <- lm(unisuff2 ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_decelec1_nc <- lm(decelec1 ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_decelec2_nc <- lm(decelec2 ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_frelect1_nc <- lm(frelect1 ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_frelect2_nc <- lm(frelect2 ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_judcnstr1_nc <- lm(judcnstr1 ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_judcnstr2_nc <- lm(judcnstr2 ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_legcnstr1_nc <- lm(legcnstr1 ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_legcnstr2_nc <- lm(legcnstr2 ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_eqlaw1_nc <- lm(eqlaw1 ~ treat * pres_bi1, data = p_dem))

summary(int_presbi1_eqlaw2_nc <- lm(eqlaw2 ~ treat * pres_bi1, data = p_dem))

#Output for treatment effects, interactions with pres_bi1, no controls
presbi1_nc <- list(int_presbi1_frexp1_nc, int_presbi1_frexp2_nc, int_presbi1_frassc1_nc, int_presbi1_frassc2_nc, int_presbi1_frassc3_nc, int_presbi1_unisuff1_nc, int_presbi1_unisuff2_nc, int_presbi1_decelec1_nc, int_presbi1_decelec2_nc, int_presbi1_frelect1_nc, int_presbi1_frelect2_nc, int_presbi1_judcnstr1_nc, int_presbi1_judcnstr2_nc, int_presbi1_legcnstr1_nc, int_presbi1_legcnstr2_nc, int_presbi1_eqlaw1_nc, int_presbi1_eqlaw2_nc, type = "text")
int_presbi1_nc <- stargazer(presbi1_nc, type = "text")
int_presbi1_nc_matrix <- as.matrix(int_presbi1_nc)
View(int_presbi1_nc_matrix)
write_xlsx(as.data.frame(int_presbi1_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with pres_bi1, controls
summary(int_presbi1_frexp1 <- lm(frexp1_recode ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_frexp2 <- lm(frexp2 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_frassc1 <- lm(frassc1 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_frassc2 <- lm(frassc2 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_frassc3 <- lm(frassc3 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_unisuff1 <- lm(unisuff1 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_unisuff2 <- lm(unisuff2 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_decelec1 <- lm(decelec1 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_decelec2 <- lm(decelec2 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_frelect1 <- lm(frelect1 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_frelect2 <- lm(frelect2 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_judcnstr1 <- lm(judcnstr1 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_judcnstr2 <- lm(judcnstr2 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_legcnstr1 <- lm(legcnstr1 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_legcnstr2 <- lm(legcnstr2 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_eqlaw1 <- lm(eqlaw1 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi1_eqlaw2 <- lm(eqlaw2 ~ treat * pres_bi1 + age + gender + living + edu_n + income_n, data = p_dem))

#Output for treatment effects, interactions with pres_bi1, controls
presbi1 <- list(int_presbi1_frexp1, int_presbi1_frexp2, int_presbi1_frassc1, int_presbi1_frassc2, int_presbi1_frassc3, int_presbi1_unisuff1, int_presbi1_unisuff2, int_presbi1_decelec1, int_presbi1_decelec2, int_presbi1_frelect1, int_presbi1_frelect2, int_presbi1_judcnstr1, int_presbi1_judcnstr2, int_presbi1_legcnstr1, int_presbi1_legcnstr2, int_presbi1_eqlaw1, int_presbi1_eqlaw2, type = "text")
int_presbi1 <- stargazer(presbi1, type = "text")
int_presbi1_matrix <- as.matrix(int_presbi1)
View(int_presbi1_matrix)
write_xlsx(as.data.frame(int_presbi1_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with pers_econ_recode_bi1, no controls
summary(int_perseconbi1_frexp1_nc <- lm(frexp1_recode ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_frexp2_nc <- lm(frexp2 ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_frassc1_nc <- lm(frassc1 ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_frassc2_nc <- lm(frassc2 ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_frassc3_nc <- lm(frassc3 ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_unisuff1_nc <- lm(unisuff1 ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_unisuff2_nc <- lm(unisuff2 ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_decelec1_nc <- lm(decelec1 ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_decelec2_nc <- lm(decelec2 ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_frelect1_nc <- lm(frelect1 ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_frelect2_nc <- lm(frelect2 ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_judcnstr1_nc <- lm(judcnstr1 ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_judcnstr2_nc <- lm(judcnstr2 ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_legcnstr1_nc <- lm(legcnstr1 ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_legcnstr2_nc <- lm(legcnstr2 ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_eqlaw1_nc <- lm(eqlaw1 ~ treat * pers_econ_recode_bi1, data = p_dem))

summary(int_perseconbi1_eqlaw2_nc <- lm(eqlaw2 ~ treat * pers_econ_recode_bi1, data = p_dem))

#Output for treatment effects, interactions with pers_econ_recode_bi1, no controls
perseconbi1_nc <- list(int_perseconbi1_frexp1_nc, int_perseconbi1_frexp2_nc, int_perseconbi1_frassc1_nc, int_perseconbi1_frassc2_nc, int_perseconbi1_frassc3_nc, int_perseconbi1_unisuff1_nc, int_perseconbi1_unisuff2_nc, int_perseconbi1_decelec1_nc, int_perseconbi1_decelec2_nc, int_perseconbi1_frelect1_nc, int_perseconbi1_frelect2_nc, int_perseconbi1_judcnstr1_nc, int_perseconbi1_judcnstr2_nc, int_perseconbi1_legcnstr1_nc, int_perseconbi1_legcnstr2_nc, int_perseconbi1_eqlaw1_nc, int_perseconbi1_eqlaw2_nc, type = "text")
int_perseconbi1_nc <- stargazer(perseconbi1_nc, type = "text")
int_perseconbi1_nc_matrix <- as.matrix(int_perseconbi1_nc)
View(int_perseconbi1_nc_matrix)
write_xlsx(as.data.frame(int_perseconbi1_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with pers_econ_recode_bi1, controls
summary(int_perseconbi1_frexp1 <- lm(frexp1_recode ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_frexp2 <- lm(frexp2 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_frassc1 <- lm(frassc1 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_frassc2 <- lm(frassc2 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_frassc3 <- lm(frassc3 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_unisuff1 <- lm(unisuff1 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_unisuff2 <- lm(unisuff2 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_decelec1 <- lm(decelec1 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_decelec2 <- lm(decelec2 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_frelect1 <- lm(frelect1 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_frelect2 <- lm(frelect2 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_judcnstr1 <- lm(judcnstr1 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_judcnstr2 <- lm(judcnstr2 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_legcnstr1 <- lm(legcnstr1 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_legcnstr2 <- lm(legcnstr2 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_eqlaw1 <- lm(eqlaw1 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi1_eqlaw2 <- lm(eqlaw2 ~ treat * pers_econ_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with pers_econ_recode_bi1, controls
perseconbi1 <- list(int_perseconbi1_frexp1, int_perseconbi1_frexp2, int_perseconbi1_frassc1, int_perseconbi1_frassc2, int_perseconbi1_frassc3, int_perseconbi1_unisuff1, int_perseconbi1_unisuff2, int_perseconbi1_decelec1, int_perseconbi1_decelec2, int_perseconbi1_frelect1, int_perseconbi1_frelect2, int_perseconbi1_judcnstr1, int_perseconbi1_judcnstr2, int_perseconbi1_legcnstr1, int_perseconbi1_legcnstr2, int_perseconbi1_eqlaw1, int_perseconbi1_eqlaw2, type = "text")
int_perseconbi1 <- stargazer(perseconbi1, type = "text")
int_perseconbi1_matrix <- as.matrix(int_perseconbi1)
View(int_perseconbi1_matrix)
write_xlsx(as.data.frame(int_perseconbi1_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with pers_vio_recode_bi1, no controls
summary(int_persviobi1_frexp1_nc <- lm(frexp1_recode ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_frexp2_nc <- lm(frexp2 ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_frassc1_nc <- lm(frassc1 ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_frassc2_nc <- lm(frassc2 ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_frassc3_nc <- lm(frassc3 ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_unisuff1_nc <- lm(unisuff1 ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_unisuff2_nc <- lm(unisuff2 ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_decelec1_nc <- lm(decelec1 ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_decelec2_nc <- lm(decelec2 ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_frelect1_nc <- lm(frelect1 ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_frelect2_nc <- lm(frelect2 ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_judcnstr1_nc <- lm(judcnstr1 ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_judcnstr2_nc <- lm(judcnstr2 ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_legcnstr1_nc <- lm(legcnstr1 ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_legcnstr2_nc <- lm(legcnstr2 ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_eqlaw1_nc <- lm(eqlaw1 ~ treat * pers_vio_recode_bi1, data = p_dem))

summary(int_persviobi1_eqlaw2_nc <- lm(eqlaw2 ~ treat * pers_vio_recode_bi1, data = p_dem))

#Output for treatment effects, interactions with pers_vio_recode_bi1, no controls
persviobi1_nc <- list(int_persviobi1_frexp1_nc, int_persviobi1_frexp2_nc, int_persviobi1_frassc1_nc, int_persviobi1_frassc2_nc, int_persviobi1_frassc3_nc, int_persviobi1_unisuff1_nc, int_persviobi1_unisuff2_nc, int_persviobi1_decelec1_nc, int_persviobi1_decelec2_nc, int_persviobi1_frelect1_nc, int_persviobi1_frelect2_nc, int_persviobi1_judcnstr1_nc, int_persviobi1_judcnstr2_nc, int_persviobi1_legcnstr1_nc, int_persviobi1_legcnstr2_nc, int_persviobi1_eqlaw1_nc, int_persviobi1_eqlaw2_nc, type = "text")
int_persviobi1_nc <- stargazer(persviobi1_nc, type = "text")
int_persviobi1_nc_matrix <- as.matrix(int_persviobi1_nc)
View(int_persviobi1_nc_matrix)
write_xlsx(as.data.frame(int_persviobi1_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with pers_vio_recode_bi1, controls
summary(int_persviobi1_frexp1 <- lm(frexp1_recode ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_frexp2 <- lm(frexp2 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_frassc1 <- lm(frassc1 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_frassc2 <- lm(frassc2 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_frassc3 <- lm(frassc3 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_unisuff1 <- lm(unisuff1 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_unisuff2 <- lm(unisuff2 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_decelec1 <- lm(decelec1 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_decelec2 <- lm(decelec2 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_frelect1 <- lm(frelect1 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_frelect2 <- lm(frelect2 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_judcnstr1 <- lm(judcnstr1 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_judcnstr2 <- lm(judcnstr2 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_legcnstr1 <- lm(legcnstr1 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_legcnstr2 <- lm(legcnstr2 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_eqlaw1 <- lm(eqlaw1 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi1_eqlaw2 <- lm(eqlaw2 ~ treat * pers_vio_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with pers_vio_recode_bi1, controls
persviobi1 <- list(int_persviobi1_frexp1, int_persviobi1_frexp2, int_persviobi1_frassc1, int_persviobi1_frassc2, int_persviobi1_frassc3, int_persviobi1_unisuff1, int_persviobi1_unisuff2, int_persviobi1_decelec1, int_persviobi1_decelec2, int_persviobi1_frelect1, int_persviobi1_frelect2, int_persviobi1_judcnstr1, int_persviobi1_judcnstr2, int_persviobi1_legcnstr1, int_persviobi1_legcnstr2, int_persviobi1_eqlaw1, int_persviobi1_eqlaw2, type = "text")
int_persviobi1 <- stargazer(persviobi1, type = "text")
int_persviobi1_matrix <- as.matrix(int_persviobi1)
View(int_persviobi1_matrix)
write_xlsx(as.data.frame(int_persviobi1_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with count_econ_bi1, no controls
summary(int_counteconbi1_frexp1_nc <- lm(frexp1_recode ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_frexp2_nc <- lm(frexp2 ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_frassc1_nc <- lm(frassc1 ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_frassc2_nc <- lm(frassc2 ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_frassc3_nc <- lm(frassc3 ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_unisuff1_nc <- lm(unisuff1 ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_unisuff2_nc <- lm(unisuff2 ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_decelec1_nc <- lm(decelec1 ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_decelec2_nc <- lm(decelec2 ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_frelect1_nc <- lm(frelect1 ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_frelect2_nc <- lm(frelect2 ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_judcnstr1_nc <- lm(judcnstr1 ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_judcnstr2_nc <- lm(judcnstr2 ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_legcnstr1_nc <- lm(legcnstr1 ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_legcnstr2_nc <- lm(legcnstr2 ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_eqlaw1_nc <- lm(eqlaw1 ~ treat * count_econ_bi1, data = p_dem))

summary(int_counteconbi1_eqlaw2_nc <- lm(eqlaw2 ~ treat * count_econ_bi1, data = p_dem))

#Output for treatment effects, interactions with count_econ_bi1, no controls
counteconbi1_nc <- list(int_counteconbi1_frexp1_nc, int_counteconbi1_frexp2_nc, int_counteconbi1_frassc1_nc, int_counteconbi1_frassc2_nc, int_counteconbi1_frassc3_nc, int_counteconbi1_unisuff1_nc, int_counteconbi1_unisuff2_nc, int_counteconbi1_decelec1_nc, int_counteconbi1_decelec2_nc, int_counteconbi1_frelect1_nc, int_counteconbi1_frelect2_nc, int_counteconbi1_judcnstr1_nc, int_counteconbi1_judcnstr2_nc, int_counteconbi1_legcnstr1_nc, int_counteconbi1_legcnstr2_nc, int_counteconbi1_eqlaw1_nc, int_counteconbi1_eqlaw2_nc, type = "text")
int_counteconbi1_nc <- stargazer(counteconbi1_nc, type = "text")
int_counteconbi1_nc_matrix <- as.matrix(int_counteconbi1_nc)
View(int_counteconbi1_nc_matrix)
write_xlsx(as.data.frame(int_counteconbi1_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with count_econ_bi1, controls
summary(int_counteconbi1_frexp1 <- lm(frexp1_recode ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_frexp2 <- lm(frexp2 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_frassc1 <- lm(frassc1 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_frassc2 <- lm(frassc2 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_frassc3 <- lm(frassc3 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_unisuff1 <- lm(unisuff1 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_unisuff2 <- lm(unisuff2 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_decelec1 <- lm(decelec1 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_decelec2 <- lm(decelec2 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_frelect1 <- lm(frelect1 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_frelect2 <- lm(frelect2 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_judcnstr1 <- lm(judcnstr1 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_judcnstr2 <- lm(judcnstr2 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_legcnstr1 <- lm(legcnstr1 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_legcnstr2 <- lm(legcnstr2 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_eqlaw1 <- lm(eqlaw1 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi1_eqlaw2 <- lm(eqlaw2 ~ treat * count_econ_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with count_econ_bi1, controls
counteconbi1 <- list(int_counteconbi1_frexp1, int_counteconbi1_frexp2, int_counteconbi1_frassc1, int_counteconbi1_frassc2, int_counteconbi1_frassc3, int_counteconbi1_unisuff1, int_counteconbi1_unisuff2, int_counteconbi1_decelec1, int_counteconbi1_decelec2, int_counteconbi1_frelect1, int_counteconbi1_frelect2, int_counteconbi1_judcnstr1, int_counteconbi1_judcnstr2, int_counteconbi1_legcnstr1, int_counteconbi1_legcnstr2, int_counteconbi1_eqlaw1, int_counteconbi1_eqlaw2, type = "text")
int_counteconbi1 <- stargazer(counteconbi1, type = "text")
int_counteconbi1_matrix <- as.matrix(int_counteconbi1)
View(int_counteconbi1_matrix)
write_xlsx(as.data.frame(int_counteconbi1_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with count_vio_bi1, no controls
summary(int_countviobi1_frexp1_nc <- lm(frexp1_recode ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_frexp2_nc <- lm(frexp2 ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_frassc1_nc <- lm(frassc1 ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_frassc2_nc <- lm(frassc2 ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_frassc3_nc <- lm(frassc3 ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_unisuff1_nc <- lm(unisuff1 ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_unisuff2_nc <- lm(unisuff2 ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_decelec1_nc <- lm(decelec1 ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_decelec2_nc <- lm(decelec2 ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_frelect1_nc <- lm(frelect1 ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_frelect2_nc <- lm(frelect2 ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_judcnstr1_nc <- lm(judcnstr1 ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_judcnstr2_nc <- lm(judcnstr2 ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_legcnstr1_nc <- lm(legcnstr1 ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_legcnstr2_nc <- lm(legcnstr2 ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_eqlaw1_nc <- lm(eqlaw1 ~ treat * count_vio_bi1, data = p_dem))

summary(int_countviobi1_eqlaw2_nc <- lm(eqlaw2 ~ treat * count_vio_bi1, data = p_dem))

#Output for treatment effects, interactions with count_vio_bi1, no controls
countviobi1_nc <- list(int_countviobi1_frexp1_nc, int_countviobi1_frexp2_nc, int_countviobi1_frassc1_nc, int_countviobi1_frassc2_nc, int_countviobi1_frassc3_nc, int_countviobi1_unisuff1_nc, int_countviobi1_unisuff2_nc, int_countviobi1_decelec1_nc, int_countviobi1_decelec2_nc, int_countviobi1_frelect1_nc, int_countviobi1_frelect2_nc, int_countviobi1_judcnstr1_nc, int_countviobi1_judcnstr2_nc, int_countviobi1_legcnstr1_nc, int_countviobi1_legcnstr2_nc, int_countviobi1_eqlaw1_nc, int_countviobi1_eqlaw2_nc, type = "text")
int_countviobi1_nc <- stargazer(countviobi1_nc, type = "text")
int_countviobi1_nc_matrix <- as.matrix(int_countviobi1_nc)
View(int_countviobi1_nc_matrix)
write_xlsx(as.data.frame(int_countviobi1_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with count_vio_bi1, controls
summary(int_countviobi1_frexp1 <- lm(frexp1_recode ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_frexp2 <- lm(frexp2 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_frassc1 <- lm(frassc1 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_frassc2 <- lm(frassc2 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_frassc3 <- lm(frassc3 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_unisuff1 <- lm(unisuff1 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_unisuff2 <- lm(unisuff2 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_decelec1 <- lm(decelec1 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_decelec2 <- lm(decelec2 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_frelect1 <- lm(frelect1 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_frelect2 <- lm(frelect2 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_judcnstr1 <- lm(judcnstr1 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_judcnstr2 <- lm(judcnstr2 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_legcnstr1 <- lm(legcnstr1 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_legcnstr2 <- lm(legcnstr2 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_eqlaw1 <- lm(eqlaw1 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi1_eqlaw2 <- lm(eqlaw2 ~ treat * count_vio_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with count_vio_bi1, controls
countviobi1 <- list(int_countviobi1_frexp1, int_countviobi1_frexp2, int_countviobi1_frassc1, int_countviobi1_frassc2, int_countviobi1_frassc3, int_countviobi1_unisuff1, int_countviobi1_unisuff2, int_countviobi1_decelec1, int_countviobi1_decelec2, int_countviobi1_frelect1, int_countviobi1_frelect2, int_countviobi1_judcnstr1, int_countviobi1_judcnstr2, int_countviobi1_legcnstr1, int_countviobi1_legcnstr2, int_countviobi1_eqlaw1, int_countviobi1_eqlaw2, type = "text")
int_countviobi1 <- stargazer(countviobi1, type = "text")
int_countviobi1_matrix <- as.matrix(int_countviobi1)
View(int_countviobi1_matrix)
write_xlsx(as.data.frame(int_countviobi1_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with politpart_recode_bi1, no controls
summary(int_politpartbi1_frexp1_nc <- lm(frexp1_recode ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_frexp2_nc <- lm(frexp2 ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_frassc1_nc <- lm(frassc1 ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_frassc2_nc <- lm(frassc2 ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_frassc3_nc <- lm(frassc3 ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_unisuff1_nc <- lm(unisuff1 ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_unisuff2_nc <- lm(unisuff2 ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_decelec1_nc <- lm(decelec1 ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_decelec2_nc <- lm(decelec2 ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_frelect1_nc <- lm(frelect1 ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_frelect2_nc <- lm(frelect2 ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_judcnstr1_nc <- lm(judcnstr1 ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_judcnstr2_nc <- lm(judcnstr2 ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_legcnstr1_nc <- lm(legcnstr1 ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_legcnstr2_nc <- lm(legcnstr2 ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_eqlaw1_nc <- lm(eqlaw1 ~ treat * politpart_recode_bi1, data = p_dem))

summary(int_politpartbi1_eqlaw2_nc <- lm(eqlaw2 ~ treat * politpart_recode_bi1, data = p_dem))

#Output for treatment effects, interactions with politpart_recode_bi1, no controls
politpartbi1_nc <- list(int_politpartbi1_frexp1_nc, int_politpartbi1_frexp2_nc, int_politpartbi1_frassc1_nc, int_politpartbi1_frassc2_nc, int_politpartbi1_frassc3_nc, int_politpartbi1_unisuff1_nc, int_politpartbi1_unisuff2_nc, int_politpartbi1_decelec1_nc, int_politpartbi1_decelec2_nc, int_politpartbi1_frelect1_nc, int_politpartbi1_frelect2_nc, int_politpartbi1_judcnstr1_nc, int_politpartbi1_judcnstr2_nc, int_politpartbi1_legcnstr1_nc, int_politpartbi1_legcnstr2_nc, int_politpartbi1_eqlaw1_nc, int_politpartbi1_eqlaw2_nc, type = "text")
int_politpartbi1_nc <- stargazer(politpartbi1_nc, type = "text")
int_politpartbi1_nc_matrix <- as.matrix(int_politpartbi1_nc)
View(int_politpartbi1_nc_matrix)
write_xlsx(as.data.frame(int_politpartbi1_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with politpart_recode_bi1, controls
summary(int_politpartbi1_frexp1 <- lm(frexp1_recode ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_frexp2 <- lm(frexp2 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_frassc1 <- lm(frassc1 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_frassc2 <- lm(frassc2 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_frassc3 <- lm(frassc3 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_unisuff1 <- lm(unisuff1 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_unisuff2 <- lm(unisuff2 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_decelec1 <- lm(decelec1 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_decelec2 <- lm(decelec2 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_frelect1 <- lm(frelect1 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_frelect2 <- lm(frelect2 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_judcnstr1 <- lm(judcnstr1 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_judcnstr2 <- lm(judcnstr2 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_legcnstr1 <- lm(legcnstr1 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_legcnstr2 <- lm(legcnstr2 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_eqlaw1 <- lm(eqlaw1 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi1_eqlaw2 <- lm(eqlaw2 ~ treat * politpart_recode_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with politpart_recode_bi1, controls
politpartbi1 <- list(int_politpartbi1_frexp1, int_politpartbi1_frexp2, int_politpartbi1_frassc1, int_politpartbi1_frassc2, int_politpartbi1_frassc3, int_politpartbi1_unisuff1, int_politpartbi1_unisuff2, int_politpartbi1_decelec1, int_politpartbi1_decelec2, int_politpartbi1_frelect1, int_politpartbi1_frelect2, int_politpartbi1_judcnstr1, int_politpartbi1_judcnstr2, int_politpartbi1_legcnstr1, int_politpartbi1_legcnstr2, int_politpartbi1_eqlaw1, int_politpartbi1_eqlaw2, type = "text")
int_politpartbi1 <- stargazer(politpartbi1, type = "text")
int_politpartbi1_matrix <- as.matrix(int_politpartbi1)
View(int_politpartbi1_matrix)
write_xlsx(as.data.frame(int_politpartbi1_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with elite_bi1, no controls
summary(int_elitebi1_frexp1_nc <- lm(frexp1_recode ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_frexp2_nc <- lm(frexp2 ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_frassc1_nc <- lm(frassc1 ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_frassc2_nc <- lm(frassc2 ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_frassc3_nc <- lm(frassc3 ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_unisuff1_nc <- lm(unisuff1 ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_unisuff2_nc <- lm(unisuff2 ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_decelec1_nc <- lm(decelec1 ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_decelec2_nc <- lm(decelec2 ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_frelect1_nc <- lm(frelect1 ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_frelect2_nc <- lm(frelect2 ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_judcnstr1_nc <- lm(judcnstr1 ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_judcnstr2_nc <- lm(judcnstr2 ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_legcnstr1_nc <- lm(legcnstr1 ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_legcnstr2_nc <- lm(legcnstr2 ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_eqlaw1_nc <- lm(eqlaw1 ~ treat * elite_bi1, data = p_dem))

summary(int_elitebi1_eqlaw2_nc <- lm(eqlaw2 ~ treat * elite_bi1, data = p_dem))

#Output for treatment effects, interactions with elite_bi1, no controls
elitebi1_nc <- list(int_elitebi1_frexp1_nc, int_elitebi1_frexp2_nc, int_elitebi1_frassc1_nc, int_elitebi1_frassc2_nc, int_elitebi1_frassc3_nc, int_elitebi1_unisuff1_nc, int_elitebi1_unisuff2_nc, int_elitebi1_decelec1_nc, int_elitebi1_decelec2_nc, int_elitebi1_frelect1_nc, int_elitebi1_frelect2_nc, int_elitebi1_judcnstr1_nc, int_elitebi1_judcnstr2_nc, int_elitebi1_legcnstr1_nc, int_elitebi1_legcnstr2_nc, int_elitebi1_eqlaw1_nc, int_elitebi1_eqlaw2_nc, type = "text")
int_elitebi1_nc <- stargazer(elitebi1_nc, type = "text")
int_elitebi1_nc_matrix <- as.matrix(int_elitebi1_nc)
View(int_elitebi1_nc_matrix)
write_xlsx(as.data.frame(int_elitebi1_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with elite_bi1, controls
summary(int_elitebi1_frexp1 <- lm(frexp1_recode ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_frexp2 <- lm(frexp2 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_frassc1 <- lm(frassc1 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_frassc2 <- lm(frassc2 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_frassc3 <- lm(frassc3 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_unisuff1 <- lm(unisuff1 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_unisuff2 <- lm(unisuff2 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_decelec1 <- lm(decelec1 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_decelec2 <- lm(decelec2 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_frelect1 <- lm(frelect1 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_frelect2 <- lm(frelect2 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_judcnstr1 <- lm(judcnstr1 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_judcnstr2 <- lm(judcnstr2 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_legcnstr1 <- lm(legcnstr1 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_legcnstr2 <- lm(legcnstr2 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_eqlaw1 <- lm(eqlaw1 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi1_eqlaw2 <- lm(eqlaw2 ~ treat * elite_bi1 + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with elite_bi1, controls
elitebi1 <- list(int_elitebi1_frexp1, int_elitebi1_frexp2, int_elitebi1_frassc1, int_elitebi1_frassc2, int_elitebi1_frassc3, int_elitebi1_unisuff1, int_elitebi1_unisuff2, int_elitebi1_decelec1, int_elitebi1_decelec2, int_elitebi1_frelect1, int_elitebi1_frelect2, int_elitebi1_judcnstr1, int_elitebi1_judcnstr2, int_elitebi1_legcnstr1, int_elitebi1_legcnstr2, int_elitebi1_eqlaw1, int_elitebi1_eqlaw2, type = "text")
int_elitebi1 <- stargazer(elitebi1, type = "text")
int_elitebi1_matrix <- as.matrix(int_elitebi1)
View(int_elitebi1_matrix)
write_xlsx(as.data.frame(int_elitebi1_matrix), path = "Interaction_Regression_Results.xlsx")


###Rewriting Moderators to be Options 4 and 5 vs. All Other Options -------------
p_dem$pres_bi2 <- car::recode(p_dem$pres, '
                                     "5" = "1";
                                     "4" = "0";
                                     "3" = "0";
                                     "2" = "0";
                                     "1" = "0"')

p_dem$pers_econ_recode_bi2 <- car::recode(p_dem$pers_econ_recode, '
                                     "5" = "1";
                                     "4" = "0";
                                     "3" = "0";
                                     "2" = "0";
                                     "1" = "0"')

p_dem$pers_vio_recode_bi2 <- car::recode(p_dem$pers_vio_recode, '
                                     "5" = "1";
                                     "4" = "0";
                                     "3" = "0";
                                     "2" = "0";
                                     "1" = "0"')

p_dem$count_econ_bi2 <- car::recode(p_dem$count_econ, '
                                     "5" = "1";
                                     "4" = "0";
                                     "3" = "0";
                                     "2" = "0";
                                     "1" = "0"')

p_dem$count_vio_bi2 <- car::recode(p_dem$count_vio, '
                                     "5" = "1";
                                     "4" = "0";
                                     "3" = "0";
                                     "2" = "0";
                                     "1" = "0"')

p_dem$politpart_recode_bi2 <- car::recode(p_dem$politpart_recode, '
                                     "5" = "1";
                                     "4" = "0";
                                     "3" = "0";
                                     "2" = "0";
                                     "1" = "0"')

p_dem$elite_bi2 <- car::recode(p_dem$elite, '
                                     "5" = "1";
                                     "4" = "0";
                                     "3" = "0";
                                     "2" = "0";
                                     "1" = "0"')


###Interactions with Recoded Moderators ------------------------------------
#Treatment effects, interaction with pres_bi2, no controls
summary(int_presbi2_frexp1_nc <- lm(frexp1_recode ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_frexp2_nc <- lm(frexp2 ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_frassc1_nc <- lm(frassc1 ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_frassc2_nc <- lm(frassc2 ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_frassc3_nc <- lm(frassc3 ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_unisuff1_nc <- lm(unisuff1 ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_unisuff2_nc <- lm(unisuff2 ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_decelec1_nc <- lm(decelec1 ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_decelec2_nc <- lm(decelec2 ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_frelect1_nc <- lm(frelect1 ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_frelect2_nc <- lm(frelect2 ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_judcnstr1_nc <- lm(judcnstr1 ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_judcnstr2_nc <- lm(judcnstr2 ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_legcnstr1_nc <- lm(legcnstr1 ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_legcnstr2_nc <- lm(legcnstr2 ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_eqlaw1_nc <- lm(eqlaw1 ~ treat * pres_bi2, data = p_dem))

summary(int_presbi2_eqlaw2_nc <- lm(eqlaw2 ~ treat * pres_bi2, data = p_dem))

#Output for treatment effects, interactions with pres_bi2, no controls
presbi2_nc <- list(int_presbi2_frexp1_nc, int_presbi2_frexp2_nc, int_presbi2_frassc1_nc, int_presbi2_frassc2_nc, int_presbi2_frassc3_nc, int_presbi2_unisuff1_nc, int_presbi2_unisuff2_nc, int_presbi2_decelec1_nc, int_presbi2_decelec2_nc, int_presbi2_frelect1_nc, int_presbi2_frelect2_nc, int_presbi2_judcnstr1_nc, int_presbi2_judcnstr2_nc, int_presbi2_legcnstr1_nc, int_presbi2_legcnstr2_nc, int_presbi2_eqlaw1_nc, int_presbi2_eqlaw2_nc, type = "text")
int_presbi2_nc <- stargazer(presbi2_nc, type = "text")
int_presbi2_nc_matrix <- as.matrix(int_presbi2_nc)
View(int_presbi2_nc_matrix)
write_xlsx(as.data.frame(int_presbi2_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with pres_bi2, controls
summary(int_presbi2_frexp1 <- lm(frexp1_recode ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_frexp2 <- lm(frexp2 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_frassc1 <- lm(frassc1 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_frassc2 <- lm(frassc2 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_frassc3 <- lm(frassc3 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_unisuff1 <- lm(unisuff1 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_unisuff2 <- lm(unisuff2 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_decelec1 <- lm(decelec1 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_decelec2 <- lm(decelec2 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_frelect1 <- lm(frelect1 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_frelect2 <- lm(frelect2 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_judcnstr1 <- lm(judcnstr1 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_judcnstr2 <- lm(judcnstr2 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_legcnstr1 <- lm(legcnstr1 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_legcnstr2 <- lm(legcnstr2 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_eqlaw1 <- lm(eqlaw1 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

summary(int_presbi2_eqlaw2 <- lm(eqlaw2 ~ treat * pres_bi2 + age + gender + living + edu_n + income_n, data = p_dem))

#Output for treatment effects, interactions with pres_bi2, controls
presbi2 <- list(int_presbi2_frexp1, int_presbi2_frexp2, int_presbi2_frassc1, int_presbi2_frassc2, int_presbi2_frassc3, int_presbi2_unisuff1, int_presbi2_unisuff2, int_presbi2_decelec1, int_presbi2_decelec2, int_presbi2_frelect1, int_presbi2_frelect2, int_presbi2_judcnstr1, int_presbi2_judcnstr2, int_presbi2_legcnstr1, int_presbi2_legcnstr2, int_presbi2_eqlaw1, int_presbi2_eqlaw2, type = "text")
int_presbi2 <- stargazer(presbi2, type = "text")
int_presbi2_matrix <- as.matrix(int_presbi2)
View(int_presbi2_matrix)
write_xlsx(as.data.frame(int_presbi2_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with pers_econ_recode_bi2, no controls
summary(int_perseconbi2_frexp1_nc <- lm(frexp1_recode ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_frexp2_nc <- lm(frexp2 ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_frassc1_nc <- lm(frassc1 ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_frassc2_nc <- lm(frassc2 ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_frassc3_nc <- lm(frassc3 ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_unisuff1_nc <- lm(unisuff1 ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_unisuff2_nc <- lm(unisuff2 ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_decelec1_nc <- lm(decelec1 ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_decelec2_nc <- lm(decelec2 ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_frelect1_nc <- lm(frelect1 ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_frelect2_nc <- lm(frelect2 ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_judcnstr1_nc <- lm(judcnstr1 ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_judcnstr2_nc <- lm(judcnstr2 ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_legcnstr1_nc <- lm(legcnstr1 ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_legcnstr2_nc <- lm(legcnstr2 ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_eqlaw1_nc <- lm(eqlaw1 ~ treat * pers_econ_recode_bi2, data = p_dem))

summary(int_perseconbi2_eqlaw2_nc <- lm(eqlaw2 ~ treat * pers_econ_recode_bi2, data = p_dem))

#Output for treatment effects, interactions with pers_econ_recode_bi2, no controls
perseconbi2_nc <- list(int_perseconbi2_frexp1_nc, int_perseconbi2_frexp2_nc, int_perseconbi2_frassc1_nc, int_perseconbi2_frassc2_nc, int_perseconbi2_frassc3_nc, int_perseconbi2_unisuff1_nc, int_perseconbi2_unisuff2_nc, int_perseconbi2_decelec1_nc, int_perseconbi2_decelec2_nc, int_perseconbi2_frelect1_nc, int_perseconbi2_frelect2_nc, int_perseconbi2_judcnstr1_nc, int_perseconbi2_judcnstr2_nc, int_perseconbi2_legcnstr1_nc, int_perseconbi2_legcnstr2_nc, int_perseconbi2_eqlaw1_nc, int_perseconbi2_eqlaw2_nc, type = "text")
int_perseconbi2_nc <- stargazer(perseconbi2_nc, type = "text")
int_perseconbi2_nc_matrix <- as.matrix(int_perseconbi2_nc)
View(int_perseconbi2_nc_matrix)
write_xlsx(as.data.frame(int_perseconbi2_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with pers_econ_recode_bi2, controls
summary(int_perseconbi2_frexp1 <- lm(frexp1_recode ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_frexp2 <- lm(frexp2 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_frassc1 <- lm(frassc1 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_frassc2 <- lm(frassc2 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_frassc3 <- lm(frassc3 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_unisuff1 <- lm(unisuff1 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_unisuff2 <- lm(unisuff2 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_decelec1 <- lm(decelec1 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_decelec2 <- lm(decelec2 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_frelect1 <- lm(frelect1 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_frelect2 <- lm(frelect2 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_judcnstr1 <- lm(judcnstr1 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_judcnstr2 <- lm(judcnstr2 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_legcnstr1 <- lm(legcnstr1 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_legcnstr2 <- lm(legcnstr2 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_eqlaw1 <- lm(eqlaw1 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_perseconbi2_eqlaw2 <- lm(eqlaw2 ~ treat * pers_econ_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with pers_econ_recode_bi2, controls
perseconbi2 <- list(int_perseconbi2_frexp1, int_perseconbi2_frexp2, int_perseconbi2_frassc1, int_perseconbi2_frassc2, int_perseconbi2_frassc3, int_perseconbi2_unisuff1, int_perseconbi2_unisuff2, int_perseconbi2_decelec1, int_perseconbi2_decelec2, int_perseconbi2_frelect1, int_perseconbi2_frelect2, int_perseconbi2_judcnstr1, int_perseconbi2_judcnstr2, int_perseconbi2_legcnstr1, int_perseconbi2_legcnstr2, int_perseconbi2_eqlaw1, int_perseconbi2_eqlaw2, type = "text")
int_perseconbi2 <- stargazer(perseconbi2, type = "text")
int_perseconbi2_matrix <- as.matrix(int_perseconbi2)
View(int_perseconbi2_matrix)
write_xlsx(as.data.frame(int_perseconbi2_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with pers_vio_recode_bi2, no controls
summary(int_persviobi2_frexp1_nc <- lm(frexp1_recode ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_frexp2_nc <- lm(frexp2 ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_frassc1_nc <- lm(frassc1 ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_frassc2_nc <- lm(frassc2 ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_frassc3_nc <- lm(frassc3 ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_unisuff1_nc <- lm(unisuff1 ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_unisuff2_nc <- lm(unisuff2 ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_decelec1_nc <- lm(decelec1 ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_decelec2_nc <- lm(decelec2 ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_frelect1_nc <- lm(frelect1 ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_frelect2_nc <- lm(frelect2 ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_judcnstr1_nc <- lm(judcnstr1 ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_judcnstr2_nc <- lm(judcnstr2 ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_legcnstr1_nc <- lm(legcnstr1 ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_legcnstr2_nc <- lm(legcnstr2 ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_eqlaw1_nc <- lm(eqlaw1 ~ treat * pers_vio_recode_bi2, data = p_dem))

summary(int_persviobi2_eqlaw2_nc <- lm(eqlaw2 ~ treat * pers_vio_recode_bi2, data = p_dem))

#Output for treatment effects, interactions with pers_vio_recode_bi2, no controls
persviobi2_nc <- list(int_persviobi2_frexp1_nc, int_persviobi2_frexp2_nc, int_persviobi2_frassc1_nc, int_persviobi2_frassc2_nc, int_persviobi2_frassc3_nc, int_persviobi2_unisuff1_nc, int_persviobi2_unisuff2_nc, int_persviobi2_decelec1_nc, int_persviobi2_decelec2_nc, int_persviobi2_frelect1_nc, int_persviobi2_frelect2_nc, int_persviobi2_judcnstr1_nc, int_persviobi2_judcnstr2_nc, int_persviobi2_legcnstr1_nc, int_persviobi2_legcnstr2_nc, int_persviobi2_eqlaw1_nc, int_persviobi2_eqlaw2_nc, type = "text")
int_persviobi2_nc <- stargazer(persviobi2_nc, type = "text")
int_persviobi2_nc_matrix <- as.matrix(int_persviobi2_nc)
View(int_persviobi2_nc_matrix)
write_xlsx(as.data.frame(int_persviobi2_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with pers_vio_recode_bi2, controls
summary(int_persviobi2_frexp1 <- lm(frexp1_recode ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_frexp2 <- lm(frexp2 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_frassc1 <- lm(frassc1 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_frassc2 <- lm(frassc2 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_frassc3 <- lm(frassc3 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_unisuff1 <- lm(unisuff1 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_unisuff2 <- lm(unisuff2 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_decelec1 <- lm(decelec1 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_decelec2 <- lm(decelec2 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_frelect1 <- lm(frelect1 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_frelect2 <- lm(frelect2 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_judcnstr1 <- lm(judcnstr1 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_judcnstr2 <- lm(judcnstr2 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_legcnstr1 <- lm(legcnstr1 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_legcnstr2 <- lm(legcnstr2 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_eqlaw1 <- lm(eqlaw1 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_persviobi2_eqlaw2 <- lm(eqlaw2 ~ treat * pers_vio_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with pers_vio_recode_bi2, controls
persviobi2 <- list(int_persviobi2_frexp1, int_persviobi2_frexp2, int_persviobi2_frassc1, int_persviobi2_frassc2, int_persviobi2_frassc3, int_persviobi2_unisuff1, int_persviobi2_unisuff2, int_persviobi2_decelec1, int_persviobi2_decelec2, int_persviobi2_frelect1, int_persviobi2_frelect2, int_persviobi2_judcnstr1, int_persviobi2_judcnstr2, int_persviobi2_legcnstr1, int_persviobi2_legcnstr2, int_persviobi2_eqlaw1, int_persviobi2_eqlaw2, type = "text")
int_persviobi2 <- stargazer(persviobi2, type = "text")
int_persviobi2_matrix <- as.matrix(int_persviobi2)
View(int_persviobi2_matrix)
write_xlsx(as.data.frame(int_persviobi2_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with count_econ_bi2, no controls
summary(int_counteconbi2_frexp1_nc <- lm(frexp1_recode ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_frexp2_nc <- lm(frexp2 ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_frassc1_nc <- lm(frassc1 ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_frassc2_nc <- lm(frassc2 ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_frassc3_nc <- lm(frassc3 ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_unisuff1_nc <- lm(unisuff1 ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_unisuff2_nc <- lm(unisuff2 ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_decelec1_nc <- lm(decelec1 ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_decelec2_nc <- lm(decelec2 ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_frelect1_nc <- lm(frelect1 ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_frelect2_nc <- lm(frelect2 ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_judcnstr1_nc <- lm(judcnstr1 ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_judcnstr2_nc <- lm(judcnstr2 ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_legcnstr1_nc <- lm(legcnstr1 ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_legcnstr2_nc <- lm(legcnstr2 ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_eqlaw1_nc <- lm(eqlaw1 ~ treat * count_econ_bi2, data = p_dem))

summary(int_counteconbi2_eqlaw2_nc <- lm(eqlaw2 ~ treat * count_econ_bi2, data = p_dem))

#Output for treatment effects, interactions with count_econ_bi2, no controls
counteconbi2_nc <- list(int_counteconbi2_frexp1_nc, int_counteconbi2_frexp2_nc, int_counteconbi2_frassc1_nc, int_counteconbi2_frassc2_nc, int_counteconbi2_frassc3_nc, int_counteconbi2_unisuff1_nc, int_counteconbi2_unisuff2_nc, int_counteconbi2_decelec1_nc, int_counteconbi2_decelec2_nc, int_counteconbi2_frelect1_nc, int_counteconbi2_frelect2_nc, int_counteconbi2_judcnstr1_nc, int_counteconbi2_judcnstr2_nc, int_counteconbi2_legcnstr1_nc, int_counteconbi2_legcnstr2_nc, int_counteconbi2_eqlaw1_nc, int_counteconbi2_eqlaw2_nc, type = "text")
int_counteconbi2_nc <- stargazer(counteconbi2_nc, type = "text")
int_counteconbi2_nc_matrix <- as.matrix(int_counteconbi2_nc)
View(int_counteconbi2_nc_matrix)
write_xlsx(as.data.frame(int_counteconbi2_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with count_econ_bi2, controls
summary(int_counteconbi2_frexp1 <- lm(frexp1_recode ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_frexp2 <- lm(frexp2 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_frassc1 <- lm(frassc1 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_frassc2 <- lm(frassc2 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_frassc3 <- lm(frassc3 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_unisuff1 <- lm(unisuff1 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_unisuff2 <- lm(unisuff2 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_decelec1 <- lm(decelec1 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_decelec2 <- lm(decelec2 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_frelect1 <- lm(frelect1 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_frelect2 <- lm(frelect2 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_judcnstr1 <- lm(judcnstr1 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_judcnstr2 <- lm(judcnstr2 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_legcnstr1 <- lm(legcnstr1 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_legcnstr2 <- lm(legcnstr2 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_eqlaw1 <- lm(eqlaw1 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_counteconbi2_eqlaw2 <- lm(eqlaw2 ~ treat * count_econ_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with count_econ_bi2, controls
counteconbi2 <- list(int_counteconbi2_frexp1, int_counteconbi2_frexp2, int_counteconbi2_frassc1, int_counteconbi2_frassc2, int_counteconbi2_frassc3, int_counteconbi2_unisuff1, int_counteconbi2_unisuff2, int_counteconbi2_decelec1, int_counteconbi2_decelec2, int_counteconbi2_frelect1, int_counteconbi2_frelect2, int_counteconbi2_judcnstr1, int_counteconbi2_judcnstr2, int_counteconbi2_legcnstr1, int_counteconbi2_legcnstr2, int_counteconbi2_eqlaw1, int_counteconbi2_eqlaw2, type = "text")
int_counteconbi2 <- stargazer(counteconbi2, type = "text")
int_counteconbi2_matrix <- as.matrix(int_counteconbi2)
View(int_counteconbi2_matrix)
write_xlsx(as.data.frame(int_counteconbi2_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with count_vio_bi2, no controls
summary(int_countviobi2_frexp1_nc <- lm(frexp1_recode ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_frexp2_nc <- lm(frexp2 ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_frassc1_nc <- lm(frassc1 ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_frassc2_nc <- lm(frassc2 ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_frassc3_nc <- lm(frassc3 ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_unisuff1_nc <- lm(unisuff1 ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_unisuff2_nc <- lm(unisuff2 ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_decelec1_nc <- lm(decelec1 ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_decelec2_nc <- lm(decelec2 ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_frelect1_nc <- lm(frelect1 ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_frelect2_nc <- lm(frelect2 ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_judcnstr1_nc <- lm(judcnstr1 ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_judcnstr2_nc <- lm(judcnstr2 ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_legcnstr1_nc <- lm(legcnstr1 ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_legcnstr2_nc <- lm(legcnstr2 ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_eqlaw1_nc <- lm(eqlaw1 ~ treat * count_vio_bi2, data = p_dem))

summary(int_countviobi2_eqlaw2_nc <- lm(eqlaw2 ~ treat * count_vio_bi2, data = p_dem))

#Output for treatment effects, interactions with count_vio_bi2, no controls
countviobi2_nc <- list(int_countviobi2_frexp1_nc, int_countviobi2_frexp2_nc, int_countviobi2_frassc1_nc, int_countviobi2_frassc2_nc, int_countviobi2_frassc3_nc, int_countviobi2_unisuff1_nc, int_countviobi2_unisuff2_nc, int_countviobi2_decelec1_nc, int_countviobi2_decelec2_nc, int_countviobi2_frelect1_nc, int_countviobi2_frelect2_nc, int_countviobi2_judcnstr1_nc, int_countviobi2_judcnstr2_nc, int_countviobi2_legcnstr1_nc, int_countviobi2_legcnstr2_nc, int_countviobi2_eqlaw1_nc, int_countviobi2_eqlaw2_nc, type = "text")
int_countviobi2_nc <- stargazer(countviobi2_nc, type = "text")
int_countviobi2_nc_matrix <- as.matrix(int_countviobi2_nc)
View(int_countviobi2_nc_matrix)
write_xlsx(as.data.frame(int_countviobi2_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with count_vio_bi2, controls
summary(int_countviobi2_frexp1 <- lm(frexp1_recode ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_frexp2 <- lm(frexp2 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_frassc1 <- lm(frassc1 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_frassc2 <- lm(frassc2 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_frassc3 <- lm(frassc3 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_unisuff1 <- lm(unisuff1 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_unisuff2 <- lm(unisuff2 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_decelec1 <- lm(decelec1 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_decelec2 <- lm(decelec2 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_frelect1 <- lm(frelect1 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_frelect2 <- lm(frelect2 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_judcnstr1 <- lm(judcnstr1 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_judcnstr2 <- lm(judcnstr2 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_legcnstr1 <- lm(legcnstr1 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_legcnstr2 <- lm(legcnstr2 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_eqlaw1 <- lm(eqlaw1 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_countviobi2_eqlaw2 <- lm(eqlaw2 ~ treat * count_vio_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with count_vio_bi2, controls
countviobi2 <- list(int_countviobi2_frexp1, int_countviobi2_frexp2, int_countviobi2_frassc1, int_countviobi2_frassc2, int_countviobi2_frassc3, int_countviobi2_unisuff1, int_countviobi2_unisuff2, int_countviobi2_decelec1, int_countviobi2_decelec2, int_countviobi2_frelect1, int_countviobi2_frelect2, int_countviobi2_judcnstr1, int_countviobi2_judcnstr2, int_countviobi2_legcnstr1, int_countviobi2_legcnstr2, int_countviobi2_eqlaw1, int_countviobi2_eqlaw2, type = "text")
int_countviobi2 <- stargazer(countviobi2, type = "text")
int_countviobi2_matrix <- as.matrix(int_countviobi2)
View(int_countviobi2_matrix)
write_xlsx(as.data.frame(int_countviobi2_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with politpart_recode_bi2, no controls
summary(int_politpartbi2_frexp1_nc <- lm(frexp1_recode ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_frexp2_nc <- lm(frexp2 ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_frassc1_nc <- lm(frassc1 ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_frassc2_nc <- lm(frassc2 ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_frassc3_nc <- lm(frassc3 ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_unisuff1_nc <- lm(unisuff1 ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_unisuff2_nc <- lm(unisuff2 ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_decelec1_nc <- lm(decelec1 ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_decelec2_nc <- lm(decelec2 ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_frelect1_nc <- lm(frelect1 ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_frelect2_nc <- lm(frelect2 ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_judcnstr1_nc <- lm(judcnstr1 ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_judcnstr2_nc <- lm(judcnstr2 ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_legcnstr1_nc <- lm(legcnstr1 ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_legcnstr2_nc <- lm(legcnstr2 ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_eqlaw1_nc <- lm(eqlaw1 ~ treat * politpart_recode_bi2, data = p_dem))

summary(int_politpartbi2_eqlaw2_nc <- lm(eqlaw2 ~ treat * politpart_recode_bi2, data = p_dem))

#Output for treatment effects, interactions with politpart_recode_bi2, no controls
politpartbi2_nc <- list(int_politpartbi2_frexp1_nc, int_politpartbi2_frexp2_nc, int_politpartbi2_frassc1_nc, int_politpartbi2_frassc2_nc, int_politpartbi2_frassc3_nc, int_politpartbi2_unisuff1_nc, int_politpartbi2_unisuff2_nc, int_politpartbi2_decelec1_nc, int_politpartbi2_decelec2_nc, int_politpartbi2_frelect1_nc, int_politpartbi2_frelect2_nc, int_politpartbi2_judcnstr1_nc, int_politpartbi2_judcnstr2_nc, int_politpartbi2_legcnstr1_nc, int_politpartbi2_legcnstr2_nc, int_politpartbi2_eqlaw1_nc, int_politpartbi2_eqlaw2_nc, type = "text")
int_politpartbi2_nc <- stargazer(politpartbi2_nc, type = "text")
int_politpartbi2_nc_matrix <- as.matrix(int_politpartbi2_nc)
View(int_politpartbi2_nc_matrix)
write_xlsx(as.data.frame(int_politpartbi2_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with politpart_recode_bi2, controls
summary(int_politpartbi2_frexp1 <- lm(frexp1_recode ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_frexp2 <- lm(frexp2 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_frassc1 <- lm(frassc1 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_frassc2 <- lm(frassc2 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_frassc3 <- lm(frassc3 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_unisuff1 <- lm(unisuff1 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_unisuff2 <- lm(unisuff2 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_decelec1 <- lm(decelec1 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_decelec2 <- lm(decelec2 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_frelect1 <- lm(frelect1 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_frelect2 <- lm(frelect2 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_judcnstr1 <- lm(judcnstr1 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_judcnstr2 <- lm(judcnstr2 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_legcnstr1 <- lm(legcnstr1 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_legcnstr2 <- lm(legcnstr2 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_eqlaw1 <- lm(eqlaw1 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_politpartbi2_eqlaw2 <- lm(eqlaw2 ~ treat * politpart_recode_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with politpart_recode_bi2, controls
politpartbi2 <- list(int_politpartbi2_frexp1, int_politpartbi2_frexp2, int_politpartbi2_frassc1, int_politpartbi2_frassc2, int_politpartbi2_frassc3, int_politpartbi2_unisuff1, int_politpartbi2_unisuff2, int_politpartbi2_decelec1, int_politpartbi2_decelec2, int_politpartbi2_frelect1, int_politpartbi2_frelect2, int_politpartbi2_judcnstr1, int_politpartbi2_judcnstr2, int_politpartbi2_legcnstr1, int_politpartbi2_legcnstr2, int_politpartbi2_eqlaw1, int_politpartbi2_eqlaw2, type = "text")
int_politpartbi2 <- stargazer(politpartbi2, type = "text")
int_politpartbi2_matrix <- as.matrix(int_politpartbi2)
View(int_politpartbi2_matrix)
write_xlsx(as.data.frame(int_politpartbi2_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with elite_bi2, no controls
summary(int_elitebi2_frexp1_nc <- lm(frexp1_recode ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_frexp2_nc <- lm(frexp2 ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_frassc1_nc <- lm(frassc1 ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_frassc2_nc <- lm(frassc2 ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_frassc3_nc <- lm(frassc3 ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_unisuff1_nc <- lm(unisuff1 ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_unisuff2_nc <- lm(unisuff2 ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_decelec1_nc <- lm(decelec1 ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_decelec2_nc <- lm(decelec2 ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_frelect1_nc <- lm(frelect1 ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_frelect2_nc <- lm(frelect2 ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_judcnstr1_nc <- lm(judcnstr1 ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_judcnstr2_nc <- lm(judcnstr2 ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_legcnstr1_nc <- lm(legcnstr1 ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_legcnstr2_nc <- lm(legcnstr2 ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_eqlaw1_nc <- lm(eqlaw1 ~ treat * elite_bi2, data = p_dem))

summary(int_elitebi2_eqlaw2_nc <- lm(eqlaw2 ~ treat * elite_bi2, data = p_dem))

#Output for treatment effects, interactions with elite_bi2, no controls
elitebi2_nc <- list(int_elitebi2_frexp1_nc, int_elitebi2_frexp2_nc, int_elitebi2_frassc1_nc, int_elitebi2_frassc2_nc, int_elitebi2_frassc3_nc, int_elitebi2_unisuff1_nc, int_elitebi2_unisuff2_nc, int_elitebi2_decelec1_nc, int_elitebi2_decelec2_nc, int_elitebi2_frelect1_nc, int_elitebi2_frelect2_nc, int_elitebi2_judcnstr1_nc, int_elitebi2_judcnstr2_nc, int_elitebi2_legcnstr1_nc, int_elitebi2_legcnstr2_nc, int_elitebi2_eqlaw1_nc, int_elitebi2_eqlaw2_nc, type = "text")
int_elitebi2_nc <- stargazer(elitebi2_nc, type = "text")
int_elitebi2_nc_matrix <- as.matrix(int_elitebi2_nc)
View(int_elitebi2_nc_matrix)
write_xlsx(as.data.frame(int_elitebi2_nc_matrix), path = "Interaction_Regression_Results.xlsx")


#Treatment effects, interaction with elite_bi2, controls
summary(int_elitebi2_frexp1 <- lm(frexp1_recode ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_frexp2 <- lm(frexp2 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_frassc1 <- lm(frassc1 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_frassc2 <- lm(frassc2 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_frassc3 <- lm(frassc3 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_unisuff1 <- lm(unisuff1 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_unisuff2 <- lm(unisuff2 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_decelec1 <- lm(decelec1 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_decelec2 <- lm(decelec2 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_frelect1 <- lm(frelect1 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_frelect2 <- lm(frelect2 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_judcnstr1 <- lm(judcnstr1 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_judcnstr2 <- lm(judcnstr2 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_legcnstr1 <- lm(legcnstr1 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_legcnstr2 <- lm(legcnstr2 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_eqlaw1 <- lm(eqlaw1 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

summary(int_elitebi2_eqlaw2 <- lm(eqlaw2 ~ treat * elite_bi2 + age + gender + living + edu_n + income_n + state, data = p_dem))

#Output for treatment effects, interactions with elite_bi2, controls
elitebi2 <- list(int_elitebi2_frexp1, int_elitebi2_frexp2, int_elitebi2_frassc1, int_elitebi2_frassc2, int_elitebi2_frassc3, int_elitebi2_unisuff1, int_elitebi2_unisuff2, int_elitebi2_decelec1, int_elitebi2_decelec2, int_elitebi2_frelect1, int_elitebi2_frelect2, int_elitebi2_judcnstr1, int_elitebi2_judcnstr2, int_elitebi2_legcnstr1, int_elitebi2_legcnstr2, int_elitebi2_eqlaw1, int_elitebi2_eqlaw2, type = "text")
int_elitebi2 <- stargazer(elitebi2, type = "text")
int_elitebi2_matrix <- as.matrix(int_elitebi2)
View(int_elitebi2_matrix)
write_xlsx(as.data.frame(int_elitebi2_matrix), path = "Interaction_Regression_Results.xlsx")