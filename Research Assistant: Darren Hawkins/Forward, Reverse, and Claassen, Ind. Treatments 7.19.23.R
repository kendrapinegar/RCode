###Regressions and Interactions with Forward, Reverse, and Claassen Indices; comparing the different treatments to the control separately
##July 19, 2023
#Last updated: August 10, 2023

###Import Datasets (with Bad Completes) and Packages ------------------------------
library(readxl)

m_dem <- read_excel("MX_Data_Including_Bad_Completes 7.26.23.xlsx")
p_dem <- read_excel("PE_Data_Including_Bad_Completes 7.26.23.xlsx")

m_dem <- subset(m_dem, treat == "Control" | treat == "Econ Treat")
p_dem <- subset(p_dem, treat == "Control" | treat == "Econ Treat")


###Coding the Indices with Bad Completes -----------------------------------
#Recoded questions indices, Reverse Index (with bad completes)
m_dem$reverse_index <- (m_dem$frexp2_recode + m_dem$frassc1_recode + m_dem$frassc3_recode + m_dem$unisuff1_recode + m_dem$decelec1_recode + m_dem$frelect2_recode + m_dem$judcnstr2_recode + m_dem$legcnstr1_recode + m_dem$eqlaw1_recode)/9

p_dem$reverse_index <- (p_dem$frexp2_recode + p_dem$frassc1_recode + p_dem$frassc3_recode + p_dem$unisuff1_recode + p_dem$decelec1_recode + p_dem$frelect2_recode + p_dem$judcnstr2_recode + p_dem$legcnstr1_recode + p_dem$eqlaw1_recode)/9

#Not recoded questions indices, Forward Index (with bad completes)
m_dem$forward_index <- (m_dem$frexp1 + m_dem$frassc2 + m_dem$unisuff2 + m_dem$decelec2 + m_dem$frelect1 + m_dem$judcnstr1 + m_dem$legcnstr2 + m_dem$eqlaw2)/8

p_dem$forward_index <- (p_dem$frexp1 + p_dem$frassc2 + p_dem$unisuff2 + p_dem$decelec2 + p_dem$frelect1 + p_dem$judcnstr1 + p_dem$legcnstr2 + p_dem$eqlaw2)/8

#Claassen 7-question indices, Claassen Index (with bad completes)
m_dem$claassen_index <- (m_dem$frexp2_recode + m_dem$frassc1_recode + m_dem$unisuff2 + m_dem$frelect2_recode + m_dem$judcnstr2_recode + m_dem$legcnstr2 + m_dem$eqlaw1_recode)/7

p_dem$claassen_index <- (p_dem$frexp2_recode + p_dem$frassc1_recode + p_dem$unisuff2 + p_dem$frelect2_recode + p_dem$judcnstr2_recode + p_dem$legcnstr2 + p_dem$eqlaw1_recode)/7


###Regressions and Interactions with Bad Completes ---------------------------
##Basic Regressions; controls: age, gender_n, income_n, ideo_1
#Mexico- reverse_index (with bad completes)
summary(reg_index1_mex_nc <- lm(reverse_index~treat, data=m_dem))
summary(reg_index1_mex <- lm(reverse_index ~ treat + age + gender_n + income_n+ ideo_1, data= m_dem))

#Peru- reverse_index (with bad completes)
summary(reg_index1_per_nc <- lm(reverse_index~treat, data=p_dem))
summary(reg_index1_per <- lm(reverse_index ~ treat + age + gender_n + income_n + ideo_1, data = p_dem))

#Mexico- forward_index (with bad completes)
summary(reg_index2_mex_nc <- lm(forward_index ~ treat, data = m_dem))
summary(reg_index2_mex <- lm(forward_index ~ treat + age + gender_n + income_n + ideo_1, data = m_dem))

#Peru- forward_index (with bad completes)
summary(reg_index2_per_nc <- lm(forward_index ~ treat, data = p_dem))
summary(reg_index2_per <- lm(forward_index ~ treat + age + gender_n + income_n + ideo_1, data = p_dem))

#Mexico- claassen_index (with bad completes)
summary(reg_index3_mex_nc <- lm(claassen_index ~ treat, data = m_dem))
summary(reg_index3_mex <- lm(claassen_index ~ treat + age + gender_n + income_n + ideo_1, data = m_dem))

#Peru- claassen_index (with bad completes)
summary(reg_index3_per_nc <- lm(claassen_index ~ treat, data = p_dem))
summary(reg_index3_per <- lm(claassen_index ~ treat + age + gender_n + income_n + ideo_1, data = p_dem))

#Interactions with moderators; controls: age, gender_n, income_n, ideo_1
#Mexico- reverse_index (with bad completes)
summary(int_pres_index1_mex_nc <- lm(reverse_index~treat*pres, data=m_dem))
summary(int_pres_index1_mex <- lm(reverse_index~treat*pres+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persecon_index1_mex_nc <- lm(reverse_index~treat*pers_econ, data=m_dem))
summary(int_persecon_index1_mex <- lm(reverse_index~treat*pers_econ+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persvio_index1_mex_nc <- lm(reverse_index~treat*pers_vio, data=m_dem))
summary(int_persvio_index1_mex <- lm(reverse_index~treat*pers_vio+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countecon_index1_mex_nc <- lm(reverse_index~treat*count_econ_recode, data=m_dem))
summary(int_countecon_index1_mex <- lm(reverse_index~treat*count_econ_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countvio_index1_mex_nc <- lm(reverse_index~treat*count_vio_recode, data=m_dem))
summary(int_countvio_index1_mex <- lm(reverse_index~treat*count_vio_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_polit_index1_mex_nc <- lm(reverse_index~treat*politpart, data=m_dem))
summary(int_polit_index1_mex <- lm(reverse_index~treat*politpart+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_elite_index1_mex_nc <- lm(reverse_index~treat*elite_recode, data=m_dem))
summary(int_elite_index1_mex <- lm(reverse_index~treat*elite_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_party_index1_mex_nc <- lm(reverse_index~treat*party_recode_1, data=m_dem))
summary(int_party_index1_mex <- lm(reverse_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_elite2_index1_mex_nc <- lm(reverse_index~treat*elite_recode_2, data=m_dem))
summary(int_elite2_index1_mex <- lm(reverse_index~treat*elite_recode_2+age+gender_n+income_n+ideo_1, data=m_dem))

#Peru- reverse_index (with bad completes)
summary(int_pres_index1_per_nc <- lm(reverse_index~treat*pres, data=p_dem))
summary(int_pres_index1_per <- lm(reverse_index~treat*pres+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persecon_index1_per_nc <- lm(reverse_index~treat*pers_econ, data=p_dem))
summary(int_persecon_index1_per <- lm(reverse_index~treat*pers_econ+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persvio_index1_per_nc <- lm(reverse_index~treat*pers_vio, data=p_dem))
summary(int_persvio_index1_per <- lm(reverse_index~treat*pers_vio+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countecon_index1_per_nc <- lm(reverse_index~treat*count_econ_recode, data=p_dem))
summary(int_countecon_index1_per <- lm(reverse_index~treat*count_econ_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countvio_index1_per_nc <- lm(reverse_index~treat*count_vio_recode, data=p_dem))
summary(int_countvio_index1_per <- lm(reverse_index~treat*count_vio_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_polit_index1_per_nc <- lm(reverse_index~treat*politpart, data=p_dem))
summary(int_polit_index1_per <- lm(reverse_index~treat*politpart+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_elite_index1_per_nc <- lm(reverse_index~treat*elite_recode, data=p_dem))
summary(int_elite_index1_per <- lm(reverse_index~treat*elite_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_party_index1_per_nc <- lm(reverse_index~treat*party_recode_1, data=p_dem))
summary(int_party_index1_per <- lm(reverse_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_elite2_index1_per_nc <- lm(reverse_index~treat*elite_recode_2, data=p_dem))
summary(int_elite2_index1_per <- lm(reverse_index~treat*elite_recode_2+age+gender_n+income_n+ideo_1, data=p_dem))

#Mexico- forward_index (with bad completes)
summary(int_pres_index2_mex_nc <- lm(forward_index~treat*pres, data=m_dem))
summary(int_pres_index2_mex <- lm(forward_index~treat*pres+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persecon_index2_mex_nc <- lm(forward_index~treat*pers_econ, data=m_dem))
summary(int_persecon_index2_mex <- lm(forward_index~treat*pers_econ+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persvio_index2_mex_nc <- lm(forward_index~treat*pers_vio, data=m_dem))
summary(int_persvio_index2_mex <- lm(forward_index~treat*pers_vio+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countecon_index2_mex_nc <- lm(forward_index~treat*count_econ_recode, data=m_dem))
summary(int_countecon_index2_mex <- lm(forward_index~treat*count_econ_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countvio_index2_mex_nc <- lm(forward_index~treat*count_vio_recode, data=m_dem))
summary(int_countvio_index2_mex <- lm(forward_index~treat*count_vio_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_polit_index2_mex_nc <- lm(forward_index~treat*politpart, data=m_dem))
summary(int_polit_index2_mex <- lm(forward_index~treat*politpart+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_elite_index2_mex_nc <- lm(forward_index~treat*elite_recode, data=m_dem))
summary(int_elite_index2_mex <- lm(forward_index~treat*elite_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_party_index2_mex_nc <- lm(forward_index~treat*party_recode_1, data=m_dem))
summary(int_party_index2_mex <- lm(forward_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_elite2_index2_mex_nc <- lm(forward_index~treat*elite_recode_2, data=m_dem))
summary(int_elite2_index2_mex <- lm(forward_index~treat*elite_recode_2+age+gender_n+income_n+ideo_1, data=m_dem))

#Peru- forward_index (with bad completes)
summary(int_pres_index2_per_nc <- lm(forward_index~treat*pres, data=p_dem))
summary(int_pres_index2_per <- lm(forward_index~treat*pres+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persecon_index2_per_nc <- lm(forward_index~treat*pers_econ, data=p_dem))
summary(int_persecon_index2_per <- lm(forward_index~treat*pers_econ+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persvio_index2_per_nc <- lm(forward_index~treat*pers_vio, data=p_dem))
summary(int_persvio_index2_per <- lm(forward_index~treat*pers_vio+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countecon_index2_per_nc <- lm(forward_index~treat*count_econ_recode, data=p_dem))
summary(int_countecon_index2_per <- lm(forward_index~treat*count_econ_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countvio_index2_per_nc <- lm(forward_index~treat*count_vio_recode, data=p_dem))
summary(int_countvio_index2_per <- lm(forward_index~treat*count_vio_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_polit_index2_per_nc <- lm(forward_index~treat*politpart, data=p_dem))
summary(int_polit_index2_per <- lm(forward_index~treat*politpart+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_elite_index2_per_nc <- lm(forward_index~treat*elite_recode, data=p_dem))
summary(int_elite_index2_per <- lm(forward_index~treat*elite_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_party_index2_per_nc <- lm(forward_index~treat*party_recode_1, data=p_dem))
summary(int_party_index2_per <- lm(forward_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_elite2_index2_per_nc <- lm(forward_index~treat*elite_recode_2, data=p_dem))
summary(int_elite2_index2_per <- lm(forward_index~treat*elite_recode_2+age+gender_n+income_n+ideo_1, data=p_dem))

#Mexico- claassen_index (with bad completes)
summary(int_pres_index3_mex_nc <- lm(claassen_index~treat*pres, data=m_dem))
summary(int_pres_index3_mex <- lm(claassen_index~treat*pres+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persecon_index3_mex_nc <- lm(claassen_index~treat*pers_econ, data=m_dem))
summary(int_persecon_index3_mex <- lm(claassen_index~treat*pers_econ+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persvio_index3_mex_nc <- lm(claassen_index~treat*pers_vio, data=m_dem))
summary(int_persvio_index3_mex <- lm(claassen_index~treat*pers_vio+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countecon_index3_mex_nc <- lm(claassen_index~treat*count_econ_recode, data=m_dem))
summary(int_countecon_index3_mex <- lm(claassen_index~treat*count_econ_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countvio_index3_mex_nc <- lm(claassen_index~treat*count_vio_recode, data=m_dem))
summary(int_countvio_index3_mex <- lm(claassen_index~treat*count_vio_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_polit_index3_mex_nc <- lm(claassen_index~treat*politpart, data=m_dem))
summary(int_polit_index3_mex <- lm(claassen_index~treat*politpart+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_elite_index3_mex_nc <- lm(claassen_index~treat*elite_recode, data=m_dem))
summary(int_elite_index3_mex <- lm(claassen_index~treat*elite_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_party_index3_mex_nc <- lm(claassen_index~treat*party_recode_1, data=m_dem))
summary(int_party_index3_mex <- lm(claassen_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_elite2_index3_mex_nc <- lm(claassen_index~treat*elite_recode_2, data=m_dem))
summary(int_elite2_index3_mex <- lm(claassen_index~treat*elite_recode_2+age+gender_n+income_n+ideo_1, data=m_dem))

#Peru- claassen_index (with bad completes)
summary(int_pres_index3_per_nc <- lm(claassen_index~treat*pres, data=p_dem))
summary(int_pres_index3_per <- lm(claassen_index~treat*pres+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persecon_index3_per_nc <- lm(claassen_index~treat*pers_econ, data=p_dem))
summary(int_persecon_index3_per <- lm(claassen_index~treat*pers_econ+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persvio_index3_per_nc <- lm(claassen_index~treat*pers_vio, data=p_dem))
summary(int_persvio_index3_per <- lm(claassen_index~treat*pers_vio+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countecon_index3_per_nc <- lm(claassen_index~treat*count_econ_recode, data=p_dem))
summary(int_countecon_index3_per <- lm(claassen_index~treat*count_econ_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countvio_index3_per_nc <- lm(claassen_index~treat*count_vio_recode, data=p_dem))
summary(int_countvio_index3_per <- lm(claassen_index~treat*count_vio_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_polit_index3_per_nc <- lm(claassen_index~treat*politpart, data=p_dem))
summary(int_polit_index3_per <- lm(claassen_index~treat*politpart+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_elite_index3_per_nc <- lm(claassen_index~treat*elite_recode, data=p_dem))
summary(int_elite_index3_per <- lm(claassen_index~treat*elite_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_party_index3_per_nc <- lm(claassen_index~treat*party_recode_1, data=p_dem))
summary(int_party_index3_per <- lm(claassen_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_elite2_index3_per_nc <- lm(claassen_index~treat*elite_recode_2, data=p_dem))
summary(int_elite2_index3_per <- lm(claassen_index~treat*elite_recode_2+age+gender_n+income_n+ideo_1, data=p_dem))



###Export Regressions and Interactions ---------------------------------
#Export regressions and interactions
#Regressions (with bad completes)
regressions_mex <- list(reg_index1_mex_nc, reg_index1_mex, reg_index2_mex_nc, reg_index2_mex, reg_index3_mex_nc, reg_index3_mex, type = "text")
regressions_mex <- stargazer(regressions_mex, type = "text")
regressions_mex_matrix <- as.matrix(regressions_mex)
write_xlsx(as.data.frame(regressions_mex_matrix), path = "Results.xlsx")

regressions_per <- list(reg_index1_per_nc, reg_index1_per, reg_index2_per_nc, reg_index2_per, reg_index3_per_nc, reg_index3_per, type="text")
regressions_per <- stargazer(regressions_per, type="text")
regressions_per_matrix <- as.matrix(regressions_per)
write_xlsx(as.data.frame(regressions_per_matrix), path="Results.xlsx")

#Pres interactions (with bad completes)
int_pres_mex <- list(int_pres_index1_mex_nc, int_pres_index1_mex, int_pres_index2_mex_nc, int_pres_index2_mex, int_pres_index3_mex_nc, int_pres_index3_mex, type="text")
int_pres_mex <- stargazer(int_pres_mex, type="text")
int_pres_mex_matrix <- as.matrix(int_pres_mex)
write_xlsx(as.data.frame(int_pres_mex_matrix), path="Results.xlsx")

int_pres_per <- list(int_pres_index1_per_nc, int_pres_index1_per, int_pres_index2_per_nc, int_pres_index2_per, int_pres_index3_per_nc, int_pres_index3_per, type="text")
int_pres_per <- stargazer(int_pres_per, type="text")
int_pres_per_matrix <- as.matrix(int_pres_per)
write_xlsx(as.data.frame(int_pres_per_matrix), path="Results.xlsx")

#Pers_econ interactions (with bad completes)
int_persecon_mex <- list(int_persecon_index1_mex_nc, int_persecon_index1_mex, int_persecon_index2_mex_nc, int_persecon_index2_mex, int_persecon_index3_mex_nc, int_persecon_index3_mex, type="text")
int_persecon_mex <- stargazer(int_persecon_mex, type="text")
int_persecon_mex_matrix <- as.matrix(int_persecon_mex)
write_xlsx(as.data.frame(int_persecon_mex_matrix), path="Results.xlsx")

int_persecon_per <- list(int_persecon_index1_per_nc, int_persecon_index1_per, int_persecon_index2_per_nc, int_persecon_index2_per, int_persecon_index3_per_nc, int_persecon_index3_per, type="text")
int_persecon_per <- stargazer(int_persecon_per, type="text")
int_persecon_per_matrix <- as.matrix(int_persecon_per)
write_xlsx(as.data.frame(int_persecon_per_matrix), path="Results.xlsx")

#Pers_vio interactions (with bad completes)
int_persvio_mex <- list(int_persvio_index1_mex_nc, int_persvio_index1_mex, int_persvio_index2_mex_nc, int_persvio_index2_mex, int_persvio_index3_mex_nc, int_persvio_index3_mex, type="text")
int_persvio_mex <- stargazer(int_persvio_mex, type="text")
int_persvio_mex_matrix <- as.matrix(int_persvio_mex)
write_xlsx(as.data.frame(int_persvio_mex_matrix), path="Results.xlsx")

int_persvio_per <- list(int_persvio_index1_per_nc, int_persvio_index1_per, int_persvio_index2_per_nc, int_persvio_index2_per, int_persvio_index3_per_nc, int_persvio_index3_per, type="text")
int_persvio_per <- stargazer(int_persvio_per, type="text")
int_persvio_per_matrix <- as.matrix(int_persvio_per)
write_xlsx(as.data.frame(int_persvio_per_matrix), path="Results.xlsx")

#Count_econ recode interactions (with bad completes)
int_countecon_mex <- list(int_countecon_index1_mex_nc, int_countecon_index1_mex, int_countecon_index2_mex_nc, int_countecon_index2_mex, int_countecon_index3_mex_nc, int_countecon_index3_mex, type="text")
int_countecon_mex <- stargazer(int_countecon_mex, type="text")
int_countecon_mex_matrix <- as.matrix(int_countecon_mex)
write_xlsx(as.data.frame(int_countecon_mex_matrix), path="Results.xlsx")

int_countecon_per <- list(int_countecon_index1_per_nc, int_countecon_index1_per, int_countecon_index2_per_nc, int_countecon_index2_per, int_countecon_index3_per_nc, int_countecon_index3_per, type="text")
int_countecon_per <- stargazer(int_countecon_per, type="text")
int_countecon_per_matrix <- as.matrix(int_countecon_per)
write_xlsx(as.data.frame(int_countecon_per_matrix), path="Results.xlsx")

#Count_vio recode interactions (with bad completes)
int_countvio_mex <- list(int_countvio_index1_mex_nc, int_countvio_index1_mex, int_countvio_index2_mex_nc, int_countvio_index2_mex, int_countvio_index3_mex_nc, int_countvio_index3_mex, type="text")
int_countvio_mex <- stargazer(int_countvio_mex, type="text")
int_countvio_mex_matrix <- as.matrix(int_countvio_mex)
write_xlsx(as.data.frame(int_countvio_mex_matrix), path="Results.xlsx")

int_countvio_per <- list(int_countvio_index1_per_nc, int_countvio_index1_per, int_countvio_index2_per_nc, int_countvio_index2_per, int_countvio_index3_per_nc, int_countvio_index3_per, type="text")
int_countvio_per <- stargazer(int_countvio_per, type="text")
int_countvio_per_matrix <- as.matrix(int_countvio_per)
write_xlsx(as.data.frame(int_countvio_per_matrix), path="Results.xlsx")

#Politpart interactions (with bad completes)
int_politpart_mex <- list(int_polit_index1_mex_nc, int_polit_index1_mex, int_polit_index2_mex_nc, int_polit_index2_mex, int_polit_index3_mex_nc, int_polit_index3_mex, type="text")
int_politpart_mex <- stargazer(int_politpart_mex, type="text")
int_politpart_mex_matrix <- as.matrix(int_politpart_mex)
write_xlsx(as.data.frame(int_politpart_mex_matrix), path="Results.xlsx")

int_politpart_per <- list(int_polit_index1_per_nc, int_polit_index1_per, int_polit_index2_per_nc, int_polit_index2_per, int_polit_index3_per_nc, int_polit_index3_per, type="text")
int_politpart_per <- stargazer(int_politpart_per, type="text")
int_politpart_per_matrix <- as.matrix(int_politpart_per)
write_xlsx(as.data.frame(int_politpart_per_matrix), path="Results.xlsx")

#Elite recode interactions (with bad completes)
int_elite_mex <- list(int_elite_index1_mex_nc, int_elite_index1_mex, int_elite_index2_mex_nc, int_elite_index2_mex, int_elite_index3_mex_nc, int_elite_index3_mex, type="text")
int_elite_mex <- stargazer(int_elite_mex, type="text")
int_elite_mex_matrix <- as.matrix(int_elite_mex)
write_xlsx(as.data.frame(int_elite_mex_matrix), path="Results.xlsx")

int_elite_per <- list(int_elite_index1_per_nc, int_elite_index1_per, int_elite_index2_per_nc, int_elite_index2_per, int_elite_index3_per_nc, int_elite_index3_per, type="text")
int_elite_per <- stargazer(int_elite_per, type="text")
int_elite_per_matrix <- as.matrix(int_elite_per)
write_xlsx(as.data.frame(int_elite_per_matrix), path="Results.xlsx")

#Party recode 1 interactions (with bad completes)
int_party_mex <- list(int_party_index1_mex_nc, int_party_index1_mex, int_party_index2_mex_nc, int_party_index2_mex, int_party_index3_mex_nc, int_party_index3_mex, type="text")
int_party_mex <- stargazer(int_party_mex, type="text")
int_party_mex_matrix <- as.matrix(int_party_mex)
write_xlsx(as.data.frame(int_party_mex_matrix), path="Results.xlsx")

int_party_per <- list(int_party_index1_per_nc, int_party_index1_per, int_party_index2_per_nc, int_party_index2_per, int_party_index3_per_nc, int_party_index3_per, type="text")
int_party_per <- stargazer(int_party_per, type="text")
int_party_per_matrix <- as.matrix(int_party_per)
write_xlsx(as.data.frame(int_party_per_matrix), path="Results.xlsx")

#Elite recode 2 (marginal effects) interactions (with bad completes)
int_elite2_mex <- list(int_elite2_index1_mex_nc, int_elite2_index1_mex, int_elite2_index2_mex_nc, int_elite2_index2_mex, int_elite2_index3_mex_nc, int_elite2_index3_mex, type="text")
int_elite2_mex <- stargazer(int_elite2_mex, type="text")
int_elite2_mex_matrix <- as.matrix(int_elite2_mex)
write_xlsx(as.data.frame(int_elite2_mex_matrix), path="Results.xlsx")

int_elite2_per <- list(int_elite2_index1_per_nc, int_elite2_index1_per, int_elite2_index2_per_nc, int_elite2_index2_per, int_elite2_index3_per_nc, int_elite2_index3_per, type="text")
int_elite2_per <- stargazer(int_elite2_per, type="text")
int_elite2_per_matrix <- as.matrix(int_elite2_per)
write_xlsx(as.data.frame(int_elite2_per_matrix), path="Results.xlsx")


###Import Datasets (Without Bad Completes) ---------------------------------

m_dem <- read_excel("MX_Data_Excluding_Bad_Completes 7.26.23.xlsx")
p_dem <- read_excel("PE_Data_Excluding_Bad_Completes 7.26.23.xlsx")

m_dem <- subset(m_dem, treat == "Control" | treat == "Econ Treat")
p_dem <- subset(p_dem, treat == "Control" | treat == "Econ Treat")



###Coding the Indices without Bad Completes --------------------------------

#Recoded questions indices, Reverse Index (without bad completes)
m_dem$reverse_index <- (m_dem$frexp2_recode + m_dem$frassc1_recode + m_dem$frassc3_recode + m_dem$unisuff1_recode + m_dem$decelec1_recode + m_dem$frelect2_recode + m_dem$judcnstr2_recode + m_dem$legcnstr1_recode + m_dem$eqlaw1_recode)/9

p_dem$reverse_index <- (p_dem$frexp2_recode + p_dem$frassc1_recode + p_dem$frassc3_recode + p_dem$unisuff1_recode + p_dem$decelec1_recode + p_dem$frelect2_recode + p_dem$judcnstr2_recode + p_dem$legcnstr1_recode + p_dem$eqlaw1_recode)/9

#Not recoded questions indices, Forward Index (without bad completes)
m_dem$forward_index <- (m_dem$frexp1 + m_dem$frassc2 + m_dem$unisuff2 + m_dem$decelec2 + m_dem$frelect1 + m_dem$judcnstr1 + m_dem$legcnstr2 + m_dem$eqlaw2)/8

p_dem$forward_index <- (p_dem$frexp1 + p_dem$frassc2 + p_dem$unisuff2 + p_dem$decelec2 + p_dem$frelect1 + p_dem$judcnstr1 + p_dem$legcnstr2 + p_dem$eqlaw2)/8

#Claassen 7 question indices, Claassen Index (without bad completes)
m_dem$claassen_index <- (m_dem$frexp2_recode + m_dem$frassc1_recode + m_dem$unisuff2 + m_dem$frelect2_recode + m_dem$judcnstr2_recode + m_dem$legcnstr2 + m_dem$eqlaw1_recode)/7

p_dem$claassen_index <- (p_dem$frexp2_recode + p_dem$frassc1_recode + p_dem$unisuff2 + p_dem$frelect2_recode + p_dem$judcnstr2_recode + p_dem$legcnstr2 + p_dem$eqlaw1_recode)/7



###Regressions and Interactions without Bad Completes ----------------------

##Basic Regressions; controls: age, gender_n, income_n, ideo_1
#Mexico- reverse_index (without bad completes)
summary(reg_index1_mex_nc <- lm(reverse_index~treat, data=m_dem))
summary(reg_index1_mex <- lm(reverse_index ~ treat + age + gender_n + income_n+ ideo_1, data= m_dem))

#Peru- reverse_index (without bad completes)
summary(reg_index1_per_nc <- lm(reverse_index~treat, data=p_dem))
summary(reg_index1_per <- lm(reverse_index ~ treat + age + gender_n + income_n + ideo_1, data = p_dem))

#Mexico- forward_index (without bad completes)
summary(reg_index2_mex_nc <- lm(forward_index ~ treat, data = m_dem))
summary(reg_index2_mex <- lm(forward_index ~ treat + age + gender_n + income_n + ideo_1, data = m_dem))

#Peru- forward_index (without bad completes)
summary(reg_index2_per_nc <- lm(forward_index ~ treat, data = p_dem))
summary(reg_index2_per <- lm(forward_index ~ treat + age + gender_n + income_n + ideo_1, data = p_dem))

#Mexico- claassen_index (without bad completes)
summary(reg_index3_mex_nc <- lm(claassen_index ~ treat, data = m_dem))
summary(reg_index3_mex <- lm(claassen_index ~ treat + age + gender_n + income_n + ideo_1, data = m_dem))

#Peru- claassen_index (without bad completes)
summary(reg_index3_per_nc <- lm(claassen_index ~ treat, data = p_dem))
summary(reg_index3_per <- lm(claassen_index ~ treat + age + gender_n + income_n + ideo_1, data = p_dem))

#Interactions with moderators; controls: age, gender_n, income_n, ideo_1
#Mexico- reverse_index (without bad completes)
summary(int_pres_index1_mex_nc <- lm(reverse_index~treat*pres, data=m_dem))
summary(int_pres_index1_mex <- lm(reverse_index~treat*pres+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persecon_index1_mex_nc <- lm(reverse_index~treat*pers_econ, data=m_dem))
summary(int_persecon_index1_mex <- lm(reverse_index~treat*pers_econ+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persvio_index1_mex_nc <- lm(reverse_index~treat*pers_vio, data=m_dem))
summary(int_persvio_index1_mex <- lm(reverse_index~treat*pers_vio+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countecon_index1_mex_nc <- lm(reverse_index~treat*count_econ_recode, data=m_dem))
summary(int_countecon_index1_mex <- lm(reverse_index~treat*count_econ_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countvio_index1_mex_nc <- lm(reverse_index~treat*count_vio_recode, data=m_dem))
summary(int_countvio_index1_mex <- lm(reverse_index~treat*count_vio_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_polit_index1_mex_nc <- lm(reverse_index~treat*politpart, data=m_dem))
summary(int_polit_index1_mex <- lm(reverse_index~treat*politpart+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_elite_index1_mex_nc <- lm(reverse_index~treat*elite_recode, data=m_dem))
summary(int_elite_index1_mex <- lm(reverse_index~treat*elite_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_party_index1_mex_nc <- lm(reverse_index~treat*party_recode_1, data=m_dem))
summary(int_party_index1_mex <- lm(reverse_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_elite2_index1_mex_nc <- lm(reverse_index~treat*elite_recode_2, data=m_dem))
summary(int_elite2_index1_mex <- lm(reverse_index~treat*elite_recode_2+age+gender_n+income_n+ideo_1, data=m_dem))

#Peru- reverse_index (without bad completes)
summary(int_pres_index1_per_nc <- lm(reverse_index~treat*pres, data=p_dem))
summary(int_pres_index1_per <- lm(reverse_index~treat*pres+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persecon_index1_per_nc <- lm(reverse_index~treat*pers_econ, data=p_dem))
summary(int_persecon_index1_per <- lm(reverse_index~treat*pers_econ+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persvio_index1_per_nc <- lm(reverse_index~treat*pers_vio, data=p_dem))
summary(int_persvio_index1_per <- lm(reverse_index~treat*pers_vio+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countecon_index1_per_nc <- lm(reverse_index~treat*count_econ_recode, data=p_dem))
summary(int_countecon_index1_per <- lm(reverse_index~treat*count_econ_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countvio_index1_per_nc <- lm(reverse_index~treat*count_vio_recode, data=p_dem))
summary(int_countvio_index1_per <- lm(reverse_index~treat*count_vio_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_polit_index1_per_nc <- lm(reverse_index~treat*politpart, data=p_dem))
summary(int_polit_index1_per <- lm(reverse_index~treat*politpart+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_elite_index1_per_nc <- lm(reverse_index~treat*elite_recode, data=p_dem))
summary(int_elite_index1_per <- lm(reverse_index~treat*elite_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_party_index1_per_nc <- lm(reverse_index~treat*party_recode_1, data=p_dem))
summary(int_party_index1_per <- lm(reverse_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_elite2_index1_per_nc <- lm(reverse_index~treat*elite_recode_2, data=p_dem))
summary(int_elite2_index1_per <- lm(reverse_index~treat*elite_recode_2+age+gender_n+income_n+ideo_1, data=p_dem))

#Mexico- forward_index (without bad completes)
summary(int_pres_index2_mex_nc <- lm(forward_index~treat*pres, data=m_dem))
summary(int_pres_index2_mex <- lm(forward_index~treat*pres+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persecon_index2_mex_nc <- lm(forward_index~treat*pers_econ, data=m_dem))
summary(int_persecon_index2_mex <- lm(forward_index~treat*pers_econ+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persvio_index2_mex_nc <- lm(forward_index~treat*pers_vio, data=m_dem))
summary(int_persvio_index2_mex <- lm(forward_index~treat*pers_vio+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countecon_index2_mex_nc <- lm(forward_index~treat*count_econ_recode, data=m_dem))
summary(int_countecon_index2_mex <- lm(forward_index~treat*count_econ_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countvio_index2_mex_nc <- lm(forward_index~treat*count_vio_recode, data=m_dem))
summary(int_countvio_index2_mex <- lm(forward_index~treat*count_vio_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_polit_index2_mex_nc <- lm(forward_index~treat*politpart, data=m_dem))
summary(int_polit_index2_mex <- lm(forward_index~treat*politpart+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_elite_index2_mex_nc <- lm(forward_index~treat*elite_recode, data=m_dem))
summary(int_elite_index2_mex <- lm(forward_index~treat*elite_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_party_index2_mex_nc <- lm(forward_index~treat*party_recode_1, data=m_dem))
summary(int_party_index2_mex <- lm(forward_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_elite2_index2_mex_nc <- lm(forward_index~treat*elite_recode_2, data=m_dem))
summary(int_elite2_index2_mex <- lm(forward_index~treat*elite_recode_2+age+gender_n+income_n+ideo_1, data=m_dem))

#Peru- forward_index (without bad completes)
summary(int_pres_index2_per_nc <- lm(forward_index~treat*pres, data=p_dem))
summary(int_pres_index2_per <- lm(forward_index~treat*pres+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persecon_index2_per_nc <- lm(forward_index~treat*pers_econ, data=p_dem))
summary(int_persecon_index2_per <- lm(forward_index~treat*pers_econ+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persvio_index2_per_nc <- lm(forward_index~treat*pers_vio, data=p_dem))
summary(int_persvio_index2_per <- lm(forward_index~treat*pers_vio+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countecon_index2_per_nc <- lm(forward_index~treat*count_econ_recode, data=p_dem))
summary(int_countecon_index2_per <- lm(forward_index~treat*count_econ_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countvio_index2_per_nc <- lm(forward_index~treat*count_vio_recode, data=p_dem))
summary(int_countvio_index2_per <- lm(forward_index~treat*count_vio_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_polit_index2_per_nc <- lm(forward_index~treat*politpart, data=p_dem))
summary(int_polit_index2_per <- lm(forward_index~treat*politpart+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_elite_index2_per_nc <- lm(forward_index~treat*elite_recode, data=p_dem))
summary(int_elite_index2_per <- lm(forward_index~treat*elite_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_party_index2_per_nc <- lm(forward_index~treat*party_recode_1, data=p_dem))
summary(int_party_index2_per <- lm(forward_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_elite2_index2_per_nc <- lm(forward_index~treat*elite_recode_2, data=p_dem))
summary(int_elite2_index2_per <- lm(forward_index~treat*elite_recode_2+age+gender_n+income_n+ideo_1, data=p_dem))

#Mexico- claassen_index (without bad completes)
summary(int_pres_index3_mex_nc <- lm(claassen_index~treat*pres, data=m_dem))
summary(int_pres_index3_mex <- lm(claassen_index~treat*pres+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persecon_index3_mex_nc <- lm(claassen_index~treat*pers_econ, data=m_dem))
summary(int_persecon_index3_mex <- lm(claassen_index~treat*pers_econ+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persvio_index3_mex_nc <- lm(claassen_index~treat*pers_vio, data=m_dem))
summary(int_persvio_index3_mex <- lm(claassen_index~treat*pers_vio+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countecon_index3_mex_nc <- lm(claassen_index~treat*count_econ_recode, data=m_dem))
summary(int_countecon_index3_mex <- lm(claassen_index~treat*count_econ_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countvio_index3_mex_nc <- lm(claassen_index~treat*count_vio_recode, data=m_dem))
summary(int_countvio_index3_mex <- lm(claassen_index~treat*count_vio_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_polit_index3_mex_nc <- lm(claassen_index~treat*politpart, data=m_dem))
summary(int_polit_index3_mex <- lm(claassen_index~treat*politpart+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_elite_index3_mex_nc <- lm(claassen_index~treat*elite_recode, data=m_dem))
summary(int_elite_index3_mex <- lm(claassen_index~treat*elite_recode+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_party_index3_mex_nc <- lm(claassen_index~treat*party_recode_1, data=m_dem))
summary(int_party_index3_mex <- lm(claassen_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_elite2_index3_mex_nc <- lm(claassen_index~treat*elite_recode_2, data=m_dem))
summary(int_elite2_index3_mex <- lm(claassen_index~treat*elite_recode_2+age+gender_n+income_n+ideo_1, data=m_dem))

#Peru- claassen_index (without bad completes)
summary(int_pres_index3_per_nc <- lm(claassen_index~treat*pres, data=p_dem))
summary(int_pres_index3_per <- lm(claassen_index~treat*pres+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persecon_index3_per_nc <- lm(claassen_index~treat*pers_econ, data=p_dem))
summary(int_persecon_index3_per <- lm(claassen_index~treat*pers_econ+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persvio_index3_per_nc <- lm(claassen_index~treat*pers_vio, data=p_dem))
summary(int_persvio_index3_per <- lm(claassen_index~treat*pers_vio+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countecon_index3_per_nc <- lm(claassen_index~treat*count_econ_recode, data=p_dem))
summary(int_countecon_index3_per <- lm(claassen_index~treat*count_econ_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countvio_index3_per_nc <- lm(claassen_index~treat*count_vio_recode, data=p_dem))
summary(int_countvio_index3_per <- lm(claassen_index~treat*count_vio_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_polit_index3_per_nc <- lm(claassen_index~treat*politpart, data=p_dem))
summary(int_polit_index3_per <- lm(claassen_index~treat*politpart+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_elite_index3_per_nc <- lm(claassen_index~treat*elite_recode, data=p_dem))
summary(int_elite_index3_per <- lm(claassen_index~treat*elite_recode+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_party_index3_per_nc <- lm(claassen_index~treat*party_recode_1, data=p_dem))
summary(int_party_index3_per <- lm(claassen_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_elite2_index3_per_nc <- lm(claassen_index~treat*elite_recode_2, data=p_dem))
summary(int_elite2_index3_per <- lm(claassen_index~treat*elite_recode_2+age+gender_n+income_n+ideo_1, data=p_dem))



###Export the Regressions and Interactions ---------------------------------
#Regressions (without bad completes)
regressions_mex <- list(reg_index1_mex_nc, reg_index1_mex, reg_index2_mex_nc, reg_index2_mex, reg_index3_mex_nc, reg_index3_mex, type = "text")
regressions_mex <- stargazer(regressions_mex, type = "text")
regressions_mex_matrix <- as.matrix(regressions_mex)
write_xlsx(as.data.frame(regressions_mex_matrix), path = "Results.xlsx")

regressions_per <- list(reg_index1_per_nc, reg_index1_per, reg_index2_per_nc, reg_index2_per, reg_index3_per_nc, reg_index3_per, type="text")
regressions_per <- stargazer(regressions_per, type="text")
regressions_per_matrix <- as.matrix(regressions_per)
write_xlsx(as.data.frame(regressions_per_matrix), path="Results.xlsx")

#Pres interactions (without bad completes)
int_pres_mex <- list(int_pres_index1_mex_nc, int_pres_index1_mex, int_pres_index2_mex_nc, int_pres_index2_mex, int_pres_index3_mex_nc, int_pres_index3_mex, type="text")
int_pres_mex <- stargazer(int_pres_mex, type="text")
int_pres_mex_matrix <- as.matrix(int_pres_mex)
write_xlsx(as.data.frame(int_pres_mex_matrix), path="Results.xlsx")

int_pres_per <- list(int_pres_index1_per_nc, int_pres_index1_per, int_pres_index2_per_nc, int_pres_index2_per, int_pres_index3_per_nc, int_pres_index3_per, type="text")
int_pres_per <- stargazer(int_pres_per, type="text")
int_pres_per_matrix <- as.matrix(int_pres_per)
write_xlsx(as.data.frame(int_pres_per_matrix), path="Results.xlsx")

#Pers_econ interactions (without bad completes)
int_persecon_mex <- list(int_persecon_index1_mex_nc, int_persecon_index1_mex, int_persecon_index2_mex_nc, int_persecon_index2_mex, int_persecon_index3_mex_nc, int_persecon_index3_mex, type="text")
int_persecon_mex <- stargazer(int_persecon_mex, type="text")
int_persecon_mex_matrix <- as.matrix(int_persecon_mex)
write_xlsx(as.data.frame(int_persecon_mex_matrix), path="Results.xlsx")

int_persecon_per <- list(int_persecon_index1_per_nc, int_persecon_index1_per, int_persecon_index2_per_nc, int_persecon_index2_per, int_persecon_index3_per_nc, int_persecon_index3_per, type="text")
int_persecon_per <- stargazer(int_persecon_per, type="text")
int_persecon_per_matrix <- as.matrix(int_persecon_per)
write_xlsx(as.data.frame(int_persecon_per_matrix), path="Results.xlsx")

#Pers_vio interactions (without bad completes)
int_persvio_mex <- list(int_persvio_index1_mex_nc, int_persvio_index1_mex, int_persvio_index2_mex_nc, int_persvio_index2_mex, int_persvio_index3_mex_nc, int_persvio_index3_mex, type="text")
int_persvio_mex <- stargazer(int_persvio_mex, type="text")
int_persvio_mex_matrix <- as.matrix(int_persvio_mex)
write_xlsx(as.data.frame(int_persvio_mex_matrix), path="Results.xlsx")

int_persvio_per <- list(int_persvio_index1_per_nc, int_persvio_index1_per, int_persvio_index2_per_nc, int_persvio_index2_per, int_persvio_index3_per_nc, int_persvio_index3_per, type="text")
int_persvio_per <- stargazer(int_persvio_per, type="text")
int_persvio_per_matrix <- as.matrix(int_persvio_per)
write_xlsx(as.data.frame(int_persvio_per_matrix), path="Results.xlsx")

#Count_econ recode interactions (without bad completes)
int_countecon_mex <- list(int_countecon_index1_mex_nc, int_countecon_index1_mex, int_countecon_index2_mex_nc, int_countecon_index2_mex, int_countecon_index3_mex_nc, int_countecon_index3_mex, type="text")
int_countecon_mex <- stargazer(int_countecon_mex, type="text")
int_countecon_mex_matrix <- as.matrix(int_countecon_mex)
write_xlsx(as.data.frame(int_countecon_mex_matrix), path="Results.xlsx")

int_countecon_per <- list(int_countecon_index1_per_nc, int_countecon_index1_per, int_countecon_index2_per_nc, int_countecon_index2_per, int_countecon_index3_per_nc, int_countecon_index3_per, type="text")
int_countecon_per <- stargazer(int_countecon_per, type="text")
int_countecon_per_matrix <- as.matrix(int_countecon_per)
write_xlsx(as.data.frame(int_countecon_per_matrix), path="Results.xlsx")

#Count_vio recode interactions (without bad completes)
int_countvio_mex <- list(int_countvio_index1_mex_nc, int_countvio_index1_mex, int_countvio_index2_mex_nc, int_countvio_index2_mex, int_countvio_index3_mex_nc, int_countvio_index3_mex, type="text")
int_countvio_mex <- stargazer(int_countvio_mex, type="text")
int_countvio_mex_matrix <- as.matrix(int_countvio_mex)
write_xlsx(as.data.frame(int_countvio_mex_matrix), path="Results.xlsx")

int_countvio_per <- list(int_countvio_index1_per_nc, int_countvio_index1_per, int_countvio_index2_per_nc, int_countvio_index2_per, int_countvio_index3_per_nc, int_countvio_index3_per, type="text")
int_countvio_per <- stargazer(int_countvio_per, type="text")
int_countvio_per_matrix <- as.matrix(int_countvio_per)
write_xlsx(as.data.frame(int_countvio_per_matrix), path="Results.xlsx")

#Politpart interactions (without bad completes)
int_politpart_mex <- list(int_polit_index1_mex_nc, int_polit_index1_mex, int_polit_index2_mex_nc, int_polit_index2_mex, int_polit_index3_mex_nc, int_polit_index3_mex, type="text")
int_politpart_mex <- stargazer(int_politpart_mex, type="text")
int_politpart_mex_matrix <- as.matrix(int_politpart_mex)
write_xlsx(as.data.frame(int_politpart_mex_matrix), path="Results.xlsx")

int_politpart_per <- list(int_polit_index1_per_nc, int_polit_index1_per, int_polit_index2_per_nc, int_polit_index2_per, int_polit_index3_per_nc, int_polit_index3_per, type="text")
int_politpart_per <- stargazer(int_politpart_per, type="text")
int_politpart_per_matrix <- as.matrix(int_politpart_per)
write_xlsx(as.data.frame(int_politpart_per_matrix), path="Results.xlsx")

#Elite recode interactions (without bad completes)
int_elite_mex <- list(int_elite_index1_mex_nc, int_elite_index1_mex, int_elite_index2_mex_nc, int_elite_index2_mex, int_elite_index3_mex_nc, int_elite_index3_mex, type="text")
int_elite_mex <- stargazer(int_elite_mex, type="text")
int_elite_mex_matrix <- as.matrix(int_elite_mex)
write_xlsx(as.data.frame(int_elite_mex_matrix), path="Results.xlsx")

int_elite_per <- list(int_elite_index1_per_nc, int_elite_index1_per, int_elite_index2_per_nc, int_elite_index2_per, int_elite_index3_per_nc, int_elite_index3_per, type="text")
int_elite_per <- stargazer(int_elite_per, type="text")
int_elite_per_matrix <- as.matrix(int_elite_per)
write_xlsx(as.data.frame(int_elite_per_matrix), path="Results.xlsx")

#Party recode 1 interactions (without bad completes)
int_party_mex <- list(int_party_index1_mex_nc, int_party_index1_mex, int_party_index2_mex_nc, int_party_index2_mex, int_party_index3_mex_nc, int_party_index3_mex, type="text")
int_party_mex <- stargazer(int_party_mex, type="text")
int_party_mex_matrix <- as.matrix(int_party_mex)
write_xlsx(as.data.frame(int_party_mex_matrix), path="Results.xlsx")

int_party_per <- list(int_party_index1_per_nc, int_party_index1_per, int_party_index2_per_nc, int_party_index2_per, int_party_index3_per_nc, int_party_index3_per, type="text")
int_party_per <- stargazer(int_party_per, type="text")
int_party_per_matrix <- as.matrix(int_party_per)
write_xlsx(as.data.frame(int_party_per_matrix), path="Results.xlsx")

#Elite recode 2 (marginal effects) interactions (without bad completes)
int_elite2_mex <- list(int_elite2_index1_mex_nc, int_elite2_index1_mex, int_elite2_index2_mex_nc, int_elite2_index2_mex, int_elite2_index3_mex_nc, int_elite2_index3_mex, type="text")
int_elite2_mex <- stargazer(int_elite2_mex, type="text")
int_elite2_mex_matrix <- as.matrix(int_elite2_mex)
write_xlsx(as.data.frame(int_elite2_mex_matrix), path="Results.xlsx")

int_elite2_per <- list(int_elite2_index1_per_nc, int_elite2_index1_per, int_elite2_index2_per_nc, int_elite2_index2_per, int_elite2_index3_per_nc, int_elite2_index3_per, type="text")
int_elite2_per <- stargazer(int_elite2_per, type="text")
int_elite2_per_matrix <- as.matrix(int_elite2_per)
write_xlsx(as.data.frame(int_elite2_per_matrix), path="Results.xlsx")