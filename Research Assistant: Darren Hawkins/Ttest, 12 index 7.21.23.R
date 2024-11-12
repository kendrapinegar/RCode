###T-tests for the all 12 index
###July 14, 2023
###Last Updated: July 26, 2023

m_dem <- read_excel("MX_Data_Excluding_Bad_Completes 7.25.23.xlsx")
p_dem <- read_excel("PE_Data_Excluding_Bad_Completes 7.25.23.xlsx")

#Coding the indices WITH bad completes: RECODE THESE IF YOU HAVE RUN THE CODE FOR WITHOUT BAD COMPLETES

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

##Creation of the Indices
##Broad

m_dem$twelve <- rowMeans(m_dem[, c("frexp1", "frexp2_recode", "frassc2", 
                                  "unisuff1_recode", "unisuff2", 
                                  "decelec1_recode", "frelect1", 
                                  "frelect2_recode", "judcnstr1",
                                  "legcnstr2", "eqlaw1_recode", "eqlaw2")], na.rm = TRUE)

p_dem$twelve <- rowMeans(p_dem[, c("frexp1", "frexp2_recode", "frassc2", 
                                  "unisuff1_recode", "unisuff2", 
                                  "decelec1_recode", "frelect1", "frelect2_recode", 
                                  "judcnstr1", "legcnstr2", 
                                  "eqlaw1_recode", "eqlaw2")], na.rm = TRUE)

##Mexico
##Violence Treatment

m_dem$twelve_vio = ifelse(m_dem$treat == "Vio Treat" & !is.na(m_dem$twelve), m_dem$twelve, NA)
p_dem$twelve_vio = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$twelve), p_dem$twelve, NA)

##Econ Treatment

m_dem$twelve_econ = ifelse(m_dem$treat == "Econ Treat" & !is.na(m_dem$twelve), m_dem$twelve, NA)
p_dem$twelve_econ = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$twelve), p_dem$twelve, NA)

##Control

m_dem$twelve_con = ifelse(m_dem$treat == "Control" & !is.na(m_dem$twelve), m_dem$twelve, NA)
p_dem$twelve_con = ifelse(p_dem$treat == "Control" & !is.na(p_dem$twelve), p_dem$twelve, NA)

##T-tests

#Mexico
twelve_ve <- t.test(m_dem$twelve_vio, m_dem$twelve_econ, var.equal = TRUE)
twelve_ve

twelve_vc <- t.test(m_dem$twelve_vio, m_dem$twelve_con, var.equal = TRUE)
twelve_vc

twelve_ce <- t.test(m_dem$twelve_con, m_dem$twelve_econ, var.equal = TRUE)
twelve_ce

#Peru
twelve_ve <- t.test(p_dem$twelve_vio, p_dem$twelve_econ, var.equal = TRUE)
twelve_ve

twelve_vc <- t.test(p_dem$twelve_vio, p_dem$twelve_con, var.equal = TRUE)
twelve_vc

twelve_ce <- t.test(p_dem$twelve_con, p_dem$twelve_econ, var.equal = TRUE)
twelve_ce


##RUN BAD COMPLETES CODE AND THEN RERUN THE INDEX COMMANDS


#Coding the indices WITH bad completes: RECODE THESE IF YOU HAVE RUN THE CODE FOR WITHOUT BAD COMPLETES

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

##Creation of the Indices
##Broad

m_dem$twelve <- rowMeans(m_dem[, c("frexp1", "frexp2_recode", "frassc2", 
                                  "unisuff1_recode", "unisuff2", 
                                  "decelec1_recode", "frelect1", 
                                  "frelect2_recode", "judcnstr1",
                                  "legcnstr2", "eqlaw1_recode", "eqlaw2")], na.rm = TRUE)

p_dem$twelve <- rowMeans(p_dem[, c("frexp1", "frexp2_recode", "frassc2", 
                                  "unisuff1_recode", "unisuff2", 
                                  "decelec1_recode", "frelect1", "frelect2_recode", 
                                  "judcnstr1", "legcnstr2", 
                                  "eqlaw1_recode", "eqlaw2")], na.rm = TRUE)
##Mexico
##Violence Treatment

m_dem$twelve_vio = ifelse(m_dem$treat == "Vio Treat" & !is.na(m_dem$twelve), m_dem$twelve, NA)
p_dem$twelve_vio = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$twelve), p_dem$twelve, NA)

##Econ Treatment

m_dem$twelve_econ = ifelse(m_dem$treat == "Econ Treat" & !is.na(m_dem$twelve), m_dem$twelve, NA)
p_dem$twelve_econ = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$twelve), p_dem$twelve, NA)

##Control

m_dem$twelve_con = ifelse(m_dem$treat == "Control" & !is.na(m_dem$twelve), m_dem$twelve, NA)
p_dem$twelve_con = ifelse(p_dem$treat == "Control" & !is.na(p_dem$twelve), p_dem$twelve, NA)

##T-tests

#Mexico
twelve_ve <- t.test(m_dem$twelve_vio, m_dem$twelve_econ, var.equal = TRUE)
twelve_ve

twelve_vc <- t.test(m_dem$twelve_vio, m_dem$twelve_con, var.equal = TRUE)
twelve_vc

twelve_ce <- t.test(m_dem$twelve_con, m_dem$twelve_econ, var.equal = TRUE)
twelve_ce

#Peru
twelve_ve <- t.test(p_dem$twelve_vio, p_dem$twelve_econ, var.equal = TRUE)
twelve_ve

twelve_vc <- t.test(p_dem$twelve_vio, p_dem$twelve_con, var.equal = TRUE)
twelve_vc

twelve_ce <- t.test(p_dem$twelve_con, p_dem$twelve_econ, var.equal = TRUE)
twelve_ce

##NARROW
#Coding the indices WITH bad completes: RECODE THESE IF YOU HAVE RUN THE CODE FOR WITHOUT BAD COMPLETES

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

##Creation of the Indices
##Narrow

m_dem$narrow <- (m_dem$frexp1 + m_dem$frexp2_recode + m_dem$frassc2 + m_dem$unisuff1_recode + m_dem$unisuff2
                + m_dem$decelec1_recode + m_dem$frelect1 + m_dem$frelect2_recode +
                m_dem$judcnstr1 + m_dem$legcnstr2 + m_dem$eqlaw1_recode + m_dem$eqlaw2)/12

p_dem$narrow <- (p_dem$frexp1 + p_dem$frexp2_recode + p_dem$frassc2 + p_dem$unisuff1_recode + p_dem$unisuff2
                 + p_dem$decelec1_recode + p_dem$frelect1 + p_dem$frelect2_recode +
                   p_dem$judcnstr1 + p_dem$legcnstr2 + p_dem$eqlaw1_recode + p_dem$eqlaw2)/12
##Mexico
##Violence Treatment

m_dem$narrow_vio = ifelse(m_dem$treat == "Vio Treat" & !is.na(m_dem$narrow), m_dem$narrow, NA)
p_dem$narrow_vio = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$narrow), p_dem$narrow, NA)

##Econ Treatment

m_dem$narrow_econ = ifelse(m_dem$treat == "Econ Treat" & !is.na(m_dem$narrow), m_dem$narrow, NA)
p_dem$narrow_econ = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$narrow), p_dem$narrow, NA)

##Control

m_dem$narrow_con = ifelse(m_dem$treat == "Control" & !is.na(m_dem$narrow), m_dem$narrow, NA)
p_dem$narrow_con = ifelse(p_dem$treat == "Control" & !is.na(p_dem$narrow), p_dem$narrow, NA)

##T-tests

#Mexico
narrow_ve <- t.test(m_dem$narrow_vio, m_dem$narrow_econ, var.equal = TRUE)
narrow_ve

narrow_vc <- t.test(m_dem$narrow_vio, m_dem$narrow_con, var.equal = TRUE)
narrow_vc

narrow_ce <- t.test(m_dem$narrow_con, m_dem$narrow_econ, var.equal = TRUE)
narrow_ce

#Peru
narrow_ve <- t.test(p_dem$narrow_vio, p_dem$narrow_econ, var.equal = TRUE)
narrow_ve

narrow_vc <- t.test(p_dem$narrow_vio, p_dem$narrow_con, var.equal = TRUE)
narrow_vc

narrow_ce <- t.test(p_dem$narrow_con, p_dem$narrow_econ, var.equal = TRUE)
narrow_ce


##RUN BAD COMPLETES CODE AND THEN RERUN THE INDEX COMMANDS
#Coding the indices WITH bad completes: RECODE THESE IF YOU HAVE RUN THE CODE FOR WITHOUT BAD COMPLETES

m_dem <- subset(m_dem, nontake == "1")
p_dem <- subset(p_dem, nontake == "1")

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

##Creation of the Indices
##Narrow
m_dem$narrow <- (m_dem$frexp1 + m_dem$frexp2_recode + m_dem$frassc2 + m_dem$unisuff1_recode + m_dem$unisuff2
                 + m_dem$decelec1_recode + m_dem$frelect1 + m_dem$frelect2_recode +
                   m_dem$judcnstr1 + m_dem$legcnstr2 + m_dem$eqlaw1_recode + m_dem$eqlaw2)/12

p_dem$narrow <- (p_dem$frexp1 + p_dem$frexp2_recode + p_dem$frassc2 + p_dem$unisuff1_recode + p_dem$unisuff2
                 + p_dem$decelec1_recode + p_dem$frelect1 + p_dem$frelect2_recode +
                   p_dem$judcnstr1 + p_dem$legcnstr2 + p_dem$eqlaw1_recode + p_dem$eqlaw2)/12

##Mexico
##Violence Treatment

m_dem$narrow_vio = ifelse(m_dem$treat == "Vio Treat" & !is.na(m_dem$narrow), m_dem$narrow, NA)
p_dem$narrow_vio = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$narrow), p_dem$narrow, NA)

##Econ Treatment

m_dem$narrow_econ = ifelse(m_dem$treat == "Econ Treat" & !is.na(m_dem$narrow), m_dem$narrow, NA)
p_dem$narrow_econ = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$narrow), p_dem$narrow, NA)

##Control

m_dem$narrow_con = ifelse(m_dem$treat == "Control" & !is.na(m_dem$narrow), m_dem$narrow, NA)
p_dem$narrow_con = ifelse(p_dem$treat == "Control" & !is.na(p_dem$narrow), p_dem$narrow, NA)

##T-tests

#Mexico
narrow_ve <- t.test(m_dem$narrow_vio, m_dem$narrow_econ, var.equal = TRUE)
narrow_ve

narrow_vc <- t.test(m_dem$narrow_vio, m_dem$narrow_con, var.equal = TRUE)
narrow_vc

narrow_ce <- t.test(m_dem$narrow_con, m_dem$narrow_econ, var.equal = TRUE)
narrow_ce

#Peru
narrow_ve <- t.test(p_dem$narrow_vio, p_dem$narrow_econ, var.equal = TRUE)
narrow_ve

narrow_vc <- t.test(p_dem$narrow_vio, p_dem$narrow_con, var.equal = TRUE)
narrow_vc

narrow_ce <- t.test(p_dem$narrow_con, p_dem$narrow_econ, var.equal = TRUE)
narrow_ce
