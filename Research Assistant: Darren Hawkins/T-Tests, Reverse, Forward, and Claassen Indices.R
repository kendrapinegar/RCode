###T-Tests for the Forward, Reverse, and Claassen Indices
###July 13, 2023
###Last Updated: July 26, 2023


###Importing Datasets (with Bad Completes) -----------------------------------
m_dem <- read_excel("MX_Data_Including_Bad_Completes 7.26.23.xlsx")
p_dem <- read_excel("PE_Data_Including_Bad_Completes 7.26.23.xlsx")


###Creating New Variables for the T-Test Code -------------------------------------
#Mexico
m_dem$treatecon[m_dem$treat == "Econ Treat"] = 1
m_dem$treatecon[m_dem$treat == "Vio Treat"] = 0
m_dem$treatecon[m_dem$treat == "Control"] = 0

table(m_dem$treatecon)

m_dem$treatvio[m_dem$treat == "Econ Treat"] = 0
m_dem$treatvio[m_dem$treat == "Vio Treat"] = 1
m_dem$treatvio[m_dem$treat == "Control"] = 0

table(m_dem$treatvio)

m_dem$control[m_dem$treat == "Econ Treat"] = 0
m_dem$control[m_dem$treat == "Vio Treat"] = 0
m_dem$control[m_dem$treat == "Control"] = 1

table(m_dem$control)

#Peru
p_dem$treatecon[p_dem$treat == "Econ Treat"] = 1
p_dem$treatecon[p_dem$treat == "Vio Treat"] = 0
p_dem$treatecon[p_dem$treat == "Control"] = 0

table(p_dem$treatecon)

p_dem$treatvio[p_dem$treat == "Econ Treat"] = 0
p_dem$treatvio[p_dem$treat == "Vio Treat"] = 1
p_dem$treatvio[p_dem$treat == "Control"] = 0

table(p_dem$treatvio)

p_dem$control[p_dem$treat == "Econ Treat"] = 0
p_dem$control[p_dem$treat == "Vio Treat"] = 0
p_dem$control[p_dem$treat == "Control"] = 1

table(p_dem$control)


###Creation of the Narrow Indices -------------------------------------------------
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

##Mexico
##Violence Treatment

#Reverse Index
m_dem$reverse_index_vio = ifelse(m_dem$treat == "Vio Treat" & !is.na(m_dem$reverse_index), m_dem$reverse_index, NA)
p_dem$reverse_index_vio = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$reverse_index), p_dem$reverse_index, NA)

#Forward Index
m_dem$forward_index_vio = ifelse(m_dem$treat == "Vio Treat" & !is.na(m_dem$forward_index), m_dem$forward_index, NA)
p_dem$forward_index_vio = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$forward_index), p_dem$forward_index, NA)

#Claassen Index
m_dem$claassen_index_vio = ifelse(m_dem$treat == "Vio Treat" & !is.na(m_dem$claassen_index), m_dem$claassen_index, NA)
p_dem$claassen_index_vio = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$claassen_index), p_dem$claassen_index, NA)

##Econ Treatment

#Reverse Index
m_dem$reverse_index_econ = ifelse(m_dem$treat == "Econ Treat" & !is.na(m_dem$reverse_index), m_dem$reverse_index, NA)
p_dem$reverse_index_econ = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$reverse_index), p_dem$reverse_index, NA)

#Forward Index
m_dem$forward_index_econ = ifelse(m_dem$treat == "Econ Treat" & !is.na(m_dem$forward_index), m_dem$forward_index, NA)
p_dem$forward_index_econ = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$forward_index), p_dem$forward_index, NA)

#Claassen Index
m_dem$claassen_index_econ = ifelse(m_dem$treat == "Econ Treat" & !is.na(m_dem$claassen_index), m_dem$claassen_index, NA)
p_dem$claassen_index_econ = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$claassen_index), p_dem$claassen_index, NA)

##Control

#Reverse Index
m_dem$reverse_index_con = ifelse(m_dem$treat == "Control" & !is.na(m_dem$reverse_index), m_dem$reverse_index, NA)
p_dem$reverse_index_con = ifelse(p_dem$treat == "Control" & !is.na(p_dem$reverse_index), p_dem$reverse_index, NA)

#Forward Index
m_dem$forward_index_con = ifelse(m_dem$treat == "Control" & !is.na(m_dem$forward_index), m_dem$forward_index, NA)
p_dem$forward_index_con = ifelse(p_dem$treat == "Control" & !is.na(p_dem$forward_index), p_dem$forward_index, NA)

#Claassen Index
m_dem$claassen_index_con = ifelse(m_dem$treat == "Control" & !is.na(m_dem$claassen_index), m_dem$claassen_index, NA)
p_dem$claassen_index_con = ifelse(p_dem$treat == "Control" & !is.na(p_dem$claassen_index), p_dem$claassen_index, NA)



### T-Tests -----------------------------------------------------------------

##T-tests
#Reverse Index

#Mexico
reverse_index_ve <- t.test(m_dem$reverse_index_vio, m_dem$reverse_index_econ, var.equal = TRUE)
reverse_index_ve

reverse_index_vc <- t.test(m_dem$reverse_index_vio, m_dem$reverse_index_con, var.equal = TRUE)
reverse_index_vc

reverse_index_ce <- t.test(m_dem$reverse_index_con, m_dem$reverse_index_econ, var.equal = TRUE)
reverse_index_ce

#Peru
reverse_index_ve <- t.test(p_dem$reverse_index_vio, p_dem$reverse_index_econ, var.equal = TRUE)
reverse_index_ve

reverse_index_vc <- t.test(p_dem$reverse_index_vio, p_dem$reverse_index_con, var.equal = TRUE)
reverse_index_vc

reverse_index_ce <- t.test(p_dem$reverse_index_con, p_dem$reverse_index_econ, var.equal = TRUE)
reverse_index_ce

#Forward Index

#Mexico
forward_index_ve <- t.test(m_dem$forward_index_vio, m_dem$forward_index_econ, var.equal = TRUE)
forward_index_ve

forward_index_vc <- t.test(m_dem$forward_index_vio, m_dem$forward_index_con, var.equal = TRUE)
forward_index_vc

forward_index_ce <- t.test(m_dem$forward_index_con, m_dem$forward_index_econ, var.equal = TRUE)
forward_index_ce

#Peru
forward_index_ve <- t.test(p_dem$forward_index_vio, p_dem$forward_index_econ, var.equal = TRUE)
forward_index_ve

forward_index_vc <- t.test(p_dem$forward_index_vio, p_dem$forward_index_con, var.equal = TRUE)
forward_index_vc

forward_index_ce <- t.test(p_dem$forward_index_con, p_dem$forward_index_econ, var.equal = TRUE)
forward_index_ce

#Claassen Index

#Mexico
claassen_index_ve <- t.test(m_dem$claassen_index_vio, m_dem$claassen_index_econ, var.equal = TRUE)
claassen_index_ve

claassen_index_vc <- t.test(m_dem$claassen_index_vio, m_dem$claassen_index_con, var.equal = TRUE)
claassen_index_vc

claassen_index_ce <- t.test(m_dem$claassen_index_con, m_dem$claassen_index_econ, var.equal = TRUE)
claassen_index_ce

#Peru
claassen_index_ve <- t.test(p_dem$claassen_index_vio, p_dem$claassen_index_econ, var.equal = TRUE)
claassen_index_ve

claassen_index_vc <- t.test(p_dem$claassen_index_vio, p_dem$claassen_index_con, var.equal = TRUE)
claassen_index_vc

claassen_index_ce <- t.test(p_dem$claassen_index_con, p_dem$claassen_index_econ, var.equal = TRUE)
claassen_index_ce



### Coding the Broad Indices ------------------------------------------------

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


##Mexico
##Violence Treatment

#Reverse Index
m_dem$reverse_index_vio = ifelse(m_dem$treat == "Vio Treat" & !is.na(m_dem$reverse_index), m_dem$reverse_index, NA)
p_dem$reverse_index_vio = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$reverse_index), p_dem$reverse_index, NA)

#Forward Index
m_dem$forward_index_vio = ifelse(m_dem$treat == "Vio Treat" & !is.na(m_dem$forward_index), m_dem$forward_index, NA)
p_dem$forward_index_vio = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$forward_index), p_dem$forward_index, NA)

#Claassen Index
m_dem$claassen_index_vio = ifelse(m_dem$treat == "Vio Treat" & !is.na(m_dem$claassen_index), m_dem$claassen_index, NA)
p_dem$claassen_index_vio = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$claassen_index), p_dem$claassen_index, NA)

##Econ Treatment

#Reverse Index
m_dem$reverse_index_econ = ifelse(m_dem$treat == "Econ Treat" & !is.na(m_dem$reverse_index), m_dem$reverse_index, NA)
p_dem$reverse_index_econ = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$reverse_index), p_dem$reverse_index, NA)

#Forward Index
m_dem$forward_index_econ = ifelse(m_dem$treat == "Econ Treat" & !is.na(m_dem$forward_index), m_dem$forward_index, NA)
p_dem$forward_index_econ = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$forward_index), p_dem$forward_index, NA)

#Claassen Index
m_dem$claassen_index_econ = ifelse(m_dem$treat == "Econ Treat" & !is.na(m_dem$claassen_index), m_dem$claassen_index, NA)
p_dem$claassen_index_econ = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$claassen_index), p_dem$claassen_index, NA)

##Control

#Reverse Index
m_dem$reverse_index_con = ifelse(m_dem$treat == "Control" & !is.na(m_dem$reverse_index), m_dem$reverse_index, NA)
p_dem$reverse_index_con = ifelse(p_dem$treat == "Control" & !is.na(p_dem$reverse_index), p_dem$reverse_index, NA)

#Forward Index
m_dem$forward_index_con = ifelse(m_dem$treat == "Control" & !is.na(m_dem$forward_index), m_dem$forward_index, NA)
p_dem$forward_index_con = ifelse(p_dem$treat == "Control" & !is.na(p_dem$forward_index), p_dem$forward_index, NA)

#Claassen Index
m_dem$claassen_index_con = ifelse(m_dem$treat == "Control" & !is.na(m_dem$claassen_index), m_dem$claassen_index, NA)
p_dem$claassen_index_con = ifelse(p_dem$treat == "Control" & !is.na(p_dem$claassen_index), p_dem$claassen_index, NA)



### T-Tests -----------------------------------------------------------------

#Reverse Index

#Mexico
reverse_index_ve <- t.test(m_dem$reverse_index_vio, m_dem$reverse_index_econ, var.equal = TRUE)
reverse_index_ve

reverse_index_vc <- t.test(m_dem$reverse_index_vio, m_dem$reverse_index_con, var.equal = TRUE)
reverse_index_vc

reverse_index_ce <- t.test(m_dem$reverse_index_con, m_dem$reverse_index_econ, var.equal = TRUE)
reverse_index_ce

#Peru
reverse_index_ve <- t.test(p_dem$reverse_index_vio, p_dem$reverse_index_econ, var.equal = TRUE)
reverse_index_ve

reverse_index_vc <- t.test(p_dem$reverse_index_vio, p_dem$reverse_index_con, var.equal = TRUE)
reverse_index_vc

reverse_index_ce <- t.test(p_dem$reverse_index_con, p_dem$reverse_index_econ, var.equal = TRUE)
reverse_index_ce

#Forward Index

#Mexico
forward_index_ve <- t.test(m_dem$forward_index_vio, m_dem$forward_index_econ, var.equal = TRUE)
forward_index_ve

forward_index_vc <- t.test(m_dem$forward_index_vio, m_dem$forward_index_con, var.equal = TRUE)
forward_index_vc

forward_index_ce <- t.test(m_dem$forward_index_con, m_dem$forward_index_econ, var.equal = TRUE)
forward_index_ce

#Peru
forward_index_ve <- t.test(p_dem$forward_index_vio, p_dem$forward_index_econ, var.equal = TRUE)
forward_index_ve

forward_index_vc <- t.test(p_dem$forward_index_vio, p_dem$forward_index_con, var.equal = TRUE)
forward_index_vc

forward_index_ce <- t.test(p_dem$forward_index_con, p_dem$forward_index_econ, var.equal = TRUE)
forward_index_ce

#Claassen Index

#Mexico
claassen_index_ve <- t.test(m_dem$claassen_index_vio, m_dem$claassen_index_econ, var.equal = TRUE)
claassen_index_ve

claassen_index_vc <- t.test(m_dem$claassen_index_vio, m_dem$claassen_index_con, var.equal = TRUE)
claassen_index_vc

claassen_index_ce <- t.test(m_dem$claassen_index_con, m_dem$claassen_index_econ, var.equal = TRUE)
claassen_index_ce

#Peru
claassen_index_ve <- t.test(p_dem$claassen_index_vio, p_dem$claassen_index_econ, var.equal = TRUE)
claassen_index_ve

claassen_index_vc <- t.test(p_dem$claassen_index_vio, p_dem$claassen_index_con, var.equal = TRUE)
claassen_index_vc

claassen_index_ce <- t.test(p_dem$claassen_index_con, p_dem$claassen_index_econ, var.equal = TRUE)
claassen_index_ce

###RUN THE BAD COMPLETES CODE AND RERUN THE INDEX COMMANDS###