###Creation of the Forward, Reverse, and Claassen Indices (both narrow and broad)
###July 13, 2023
###Last Updated: August 10, 2023


###Import Datasets (with Bad Completes) and Packages ---------------------------------
library(readxl)
m_dem <- read_excel("MX_Data_Including_Bad_Completes 7.26.23.xlsx")
p_dem <- read_excel("PE_Data_Including_Bad_Completes 7.26.23.xlsx")


###Creation of Indices -----------------------------------------------------
##Narrow Indices
#Reverse Index
m_dem$reverse_index <- (m_dem$frexp2_recode + m_dem$frassc1_recode + m_dem$frassc3_recode + m_dem$unisuff1_recode + m_dem$decelec1_recode + m_dem$frelect2_recode + m_dem$judcnstr2_recode + m_dem$legcnstr1_recode + m_dem$eqlaw1_recode)/9

p_dem$reverse_index <- (p_dem$frexp2_recode + p_dem$frassc1_recode + p_dem$frassc3_recode + p_dem$unisuff1_recode + p_dem$decelec1_recode + p_dem$frelect2_recode + p_dem$judcnstr2_recode + p_dem$legcnstr1_recode + p_dem$eqlaw1_recode)/9

#Forward Index
m_dem$forward_index <- (m_dem$frexp1 + m_dem$frassc2 + m_dem$unisuff2 + m_dem$decelec2 + m_dem$frelect1 + m_dem$judcnstr1 + m_dem$legcnstr2 + m_dem$eqlaw2)/8

p_dem$forward_index <- (p_dem$frexp1 + p_dem$frassc2 + p_dem$unisuff2 + p_dem$decelec2 + p_dem$frelect1 + p_dem$judcnstr1 + p_dem$legcnstr2 + p_dem$eqlaw2)/8

#Claassen Index
m_dem$claassen_index <- (m_dem$frexp2_recode + m_dem$frassc1_recode + m_dem$unisuff2 + m_dem$frelect2_recode + m_dem$judcnstr2_recode + m_dem$legcnstr2 + m_dem$eqlaw1_recode)/7

p_dem$claassen_index <- (p_dem$frexp2_recode + p_dem$frassc1_recode + p_dem$unisuff2 + p_dem$frelect2_recode + p_dem$judcnstr2_recode + p_dem$legcnstr2 + p_dem$eqlaw1_recode)/7

##Broad Indices
#Reverse Index
m_dem$reverse_index <- rowMeans(m_dem[, c("frexp2_recode", "frassc1_recode", "frassc3_recode", "unisuff1_recode", "decelec1_recode", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

p_dem$reverse_index <- rowMeans(p_dem[, c("frexp2_recode", "frassc1_recode", "frassc3_recode", "unisuff1_recode", "decelec1_recode", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

#Forward Index
m_dem$forward_index <- rowMeans(m_dem[, c("frexp1", "frassc2", "unisuff2", "decelec2", "frelect1", "judcnstr1", "legcnstr2", "eqlaw2")], na.rm = TRUE)

p_dem$forward_index <- rowMeans(p_dem[, c("frexp1", "frassc2", "unisuff2", "decelec2", "frelect1", "judcnstr1", "legcnstr2", "eqlaw2")], na.rm = TRUE)

#Claassen Index
m_dem$claassen_index <- rowMeans(m_dem[, c("frexp2_recode", "frassc1_recode", "unisuff2", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

p_dem$claassen_index <- rowMeans(p_dem[, c("frexp2_recode", "frassc1_recode", "unisuff2", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)


###Import Datasets (Without Bad Completes) ---------------------------------
#Note: Run this code and then rerun the index code if you want the indices produced without bad completes

m_dem <- read_excel("MX_Data_Excluding_Bad_Completes 7.26.23.xlsx")
p_dem <- read_excel("PE_Data_Excluding_Bad_Completes 7.26.23.xlsx")
