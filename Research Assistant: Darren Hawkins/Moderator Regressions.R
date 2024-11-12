###Running Tests on Moderators without treatments
###September 7, 2023
###Last Updated: September 7, 2023


###Import Datasets (without bad completes) and Packages ------------------------------------------
library(readxl)
library(stargazer)
library(tidyverse)
library(writexl)

m_dem <- read_excel("MX_Data_Excluding_Bad_Completes 7.26.23.xlsx")
p_dem <- read_excel("PE_Data_Excluding_Bad_Completes 7.26.23.xlsx")


###Subset Control Group ----------------------------------------------------
m_dem <- subset(m_dem, treat == "Control")
p_dem <- subset(p_dem, treat == "Control")



###Broad Index -------------------------------------------------------------
m_dem$main_index <- rowMeans(m_dem[, c("frexp1", "frexp2_recode", "frassc1_recode", "frassc2",
                                  "frassc3_recode", "unisuff1_recode", "unisuff2",
                                  "decelec1_recode", "decelec2", "frelect1",
                                  "frelect2_recode", "judcnstr1", "legcnstr2", "eqlaw2",
                                  "judcnstr2_recode", "legcnstr1_recode", 
                                  "eqlaw1_recode")], na.rm = TRUE)

p_dem$main_index <- rowMeans(p_dem[, c("frexp1", "frexp2_recode", "frassc1_recode", "frassc2",
                                       "frassc3_recode", "unisuff1_recode", "unisuff2",
                                       "decelec1_recode", "decelec2", "frelect1",
                                       "frelect2_recode", "judcnstr1", "legcnstr2", "eqlaw2",
                                       "judcnstr2_recode", "legcnstr1_recode", 
                                       "eqlaw1_recode")], na.rm = TRUE)

###Reverse, Forward, and Claassen Indices ----------------------------------
#Reverse Index
m_dem$reverse_index <- rowMeans(m_dem[, c("frexp2_recode", "frassc1_recode", "frassc3_recode", "unisuff1_recode", "decelec1_recode", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

p_dem$reverse_index <- rowMeans(p_dem[, c("frexp2_recode", "frassc1_recode", "frassc3_recode", "unisuff1_recode", "decelec1_recode", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

#Forward Index
m_dem$forward_index <- rowMeans(m_dem[, c("frexp1", "frassc2", "unisuff2", "decelec2", "frelect1", "judcnstr1", "legcnstr2", "eqlaw2")], na.rm = TRUE)

p_dem$forward_index <- rowMeans(p_dem[, c("frexp1", "frassc2", "unisuff2", "decelec2", "frelect1", "judcnstr1", "legcnstr2", "eqlaw2")], na.rm = TRUE)

#Claassen Index
m_dem$claassen_index <- rowMeans(m_dem[, c("frexp2_recode", "frassc1_recode", "unisuff2", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

p_dem$claassen_index <- rowMeans(p_dem[, c("frexp2_recode", "frassc1_recode", "unisuff2", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

###Regressions -------------------------------------------------------------
##Mexico
##Broad Index
summary(reg_mex <- lm(main_index ~ pres + pers_vio + count_vio_recode + pers_econ + count_econ_recode + politpart + elite + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_pres_mex <- lm(main_index ~ pres + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_persvio_mex <- lm(main_index ~ pers_vio + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_countvio_mex <- lm(main_index ~ count_vio_recode + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_persecon_mex <- lm(main_index ~ pers_econ + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_countecon_mex <- lm(main_index ~ count_econ_recode + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_politpart_mex <- lm(main_index ~ politpart + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_elite_mex <- lm(main_index ~ elite + age + gender_n + income_n + ideo_1, data = m_dem))

##Reverse Index
summary(reg_forward_mex)
##Peru
summary(reg_per <- lm(main_index ~ pres + pers_vio + count_vio_recode + pers_econ + count_econ_recode + politpart + elite + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_pres_per <- lm(main_index ~ pres + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_persvio_per <- lm(main_index ~ pers_vio + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_countvio_per <- lm(main_index ~ count_vio_recode + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_persecon_per <- lm(main_index ~ pers_econ + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_countecon_per <- lm(main_index ~ count_econ_recode + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_politpart_per <- lm(main_index ~ politpart + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_elite_per <- lm(main_index ~ elite + age + gender_n + income_n + ideo_1, data = p_dem))


###Export Regression Results -----------------------------------------------
mex <- list(reg_pres_mex, reg_persvio_mex, reg_countvio_mex, reg_persecon_mex, reg_countecon_mex, reg_politpart_mex, reg_elite_mex, reg_mex, type = "text")
mex <- stargazer(mex, type = "text")
mex_matrix <- as.matrix(mex)
write_xlsx(as.data.frame(mex_matrix), path = "regressions.xlsx")

per <- list(reg_pres_per, reg_persvio_per, reg_countvio_per, reg_persecon_per, reg_countecon_per, reg_politpart_per, reg_elite_per, reg_per, type = "text")
per <- stargazer(per, type = "text")
per_matrix <- as.matrix(per)
write_xlsx(as.data.frame(per_matrix), path = "regression.xlsx")


###Robustness Checks (Other Indices) ---------------------------------------
#Reverse Index
m_dem$reverse_index <- rowMeans(m_dem[, c("frexp2_recode", "frassc1_recode", "frassc3_recode", "unisuff1_recode", "decelec1_recode", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

p_dem$reverse_index <- rowMeans(p_dem[, c("frexp2_recode", "frassc1_recode", "frassc3_recode", "unisuff1_recode", "decelec1_recode", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

#Forward Index
m_dem$forward_index <- rowMeans(m_dem[, c("frexp1", "frassc2", "unisuff2", "decelec2", "frelect1", "judcnstr1", "legcnstr2", "eqlaw2")], na.rm = TRUE)

p_dem$forward_index <- rowMeans(p_dem[, c("frexp1", "frassc2", "unisuff2", "decelec2", "frelect1", "judcnstr1", "legcnstr2", "eqlaw2")], na.rm = TRUE)

#Claassen Index
m_dem$claassen_index <- rowMeans(m_dem[, c("frexp2_recode", "frassc1_recode", "unisuff2", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

p_dem$claassen_index <- rowMeans(p_dem[, c("frexp2_recode", "frassc1_recode", "unisuff2", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)


###Regressions for Smaller Indices -----------------------------------------
##Mexico
summary(reg_mex <- lm(reverse_index ~ pres + pers_vio + count_vio_recode + pers_econ + count_econ_recode + politpart + elite + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_pres_mex <- lm(reverse_index ~ pres + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_persvio_mex <- lm(reverse_index ~ pers_vio + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_countvio_mex <- lm(reverse_index ~ count_vio_recode + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_persecon_mex <- lm(reverse_index ~ pers_econ + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_countecon_mex <- lm(reverse_index ~ count_econ_recode + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_politpart_mex <- lm(reverse_index ~ politpart + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_elite_mex <- lm(reverse_index ~ elite + age + gender_n + income_n + ideo_1, data = m_dem))

##Peru
summary(reg_per <- lm(reverse_index ~ pres + pers_vio + count_vio_recode + pers_econ + count_econ_recode + politpart + elite + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_pres_per <- lm(reverse_index ~ pres + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_persvio_per <- lm(reverse_index ~ pers_vio + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_countvio_per <- lm(reverse_index ~ count_vio_recode + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_persecon_per <- lm(reverse_index ~ pers_econ + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_countecon_per <- lm(reverse_index ~ count_econ_recode + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_politpart_per <- lm(reverse_index ~ politpart + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_elite_per <- lm(reverse_index ~ elite + age + gender_n + income_n + ideo_1, data = p_dem))

##Mexico
summary(reg_mex <- lm(forward_index ~ pres + pers_vio + count_vio_recode + pers_econ + count_econ_recode + politpart + elite + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_pres_mex <- lm(forward_index ~ pres + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_persvio_mex <- lm(forward_index ~ pers_vio + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_countvio_mex <- lm(forward_index ~ count_vio_recode + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_persecon_mex <- lm(forward_index ~ pers_econ + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_countecon_mex <- lm(forward_index ~ count_econ_recode + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_politpart_mex <- lm(forward_index ~ politpart + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_elite_mex <- lm(forward_index ~ elite + age + gender_n + income_n + ideo_1, data = m_dem))

##Peru
summary(reg_per <- lm(forward_index ~ pres + pers_vio + count_vio_recode + pers_econ + count_econ_recode + politpart + elite + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_pres_per <- lm(forward_index ~ pres + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_persvio_per <- lm(forward_index ~ pers_vio + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_countvio_per <- lm(forward_index ~ count_vio_recode + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_persecon_per <- lm(forward_index ~ pers_econ + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_countecon_per <- lm(forward_index ~ count_econ_recode + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_politpart_per <- lm(forward_index ~ politpart + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_elite_per <- lm(forward_index ~ elite + age + gender_n + income_n + ideo_1, data = p_dem))

##Mexico
summary(reg_mex <- lm(claassen_index ~ pres + pers_vio + count_vio_recode + pers_econ + count_econ_recode + politpart + elite + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_pres_mex <- lm(claassen_index ~ pres + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_persvio_mex <- lm(claassen_index ~ pers_vio + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_countvio_mex <- lm(claassen_index ~ count_vio_recode + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_persecon_mex <- lm(claassen_index ~ pers_econ + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_countecon_mex <- lm(claassen_index ~ count_econ_recode + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_politpart_mex <- lm(claassen_index ~ politpart + age + gender_n + income_n + ideo_1, data = m_dem))

summary(reg_elite_mex <- lm(claassen_index ~ elite + age + gender_n + income_n + ideo_1, data = m_dem))

##Peru
summary(reg_per <- lm(claassen_index ~ pres + pers_vio + count_vio_recode + pers_econ + count_econ_recode + politpart + elite + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_pres_per <- lm(claassen_index ~ pres + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_persvio_per <- lm(claassen_index ~ pers_vio + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_countvio_per <- lm(claassen_index ~ count_vio_recode + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_persecon_per <- lm(claassen_index ~ pers_econ + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_countecon_per <- lm(claassen_index ~ count_econ_recode + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_politpart_per <- lm(claassen_index ~ politpart + age + gender_n + income_n + ideo_1, data = p_dem))

summary(reg_elite_per <- lm(claassen_index ~ elite + age + gender_n + income_n + ideo_1, data = p_dem))

