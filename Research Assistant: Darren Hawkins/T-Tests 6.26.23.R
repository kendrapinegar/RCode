###T-tests Between Econ and Violence Treatments
###June 26, 2023
###Last Updated: August 10, 2023

###Import Datasets (without Bad Completes) -------------------------------------------
m_dem <- read_excel("MX_Data_Excluding_Bad_Completes 7.26.23.xlsx")
p_dem <- read_excel("PE_Data_Excluding_Bad_Completes 7.26.23.xlsx")


###Create different variables for the treatments and control -------------------------
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


###Create separate variables for each treatment/control and outcome variable ---------
#Violence Treatment
p_dem$viofrexp1 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$frexp1_recode), p_dem$frexp1_recode, NA)

p_dem$viofrexp2 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$frexp2), p_dem$frexp2, NA)

p_dem$viofrassc1 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$frassc1), p_dem$frassc1, NA)

p_dem$viofrassc2 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$frassc2_recode), p_dem$frassc2_recode, NA)

p_dem$viofrassc3 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$frassc3), p_dem$frassc3, NA)

p_dem$viounisuff1 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$unisuff1), p_dem$unisuff1, NA)

p_dem$viounisuff2 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$unisuff2_recode), p_dem$unisuff2_recode, NA)

p_dem$viodecelec1 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$decelec1), p_dem$decelec1, NA)

p_dem$viodecelec2 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$decelec2_recode), p_dem$decelec2_recode, NA)

p_dem$viofrelect1 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$frelect1_recode), p_dem$frelect1_recode, NA)

p_dem$viofrelect2 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$frelect2), p_dem$frelect2, NA)

p_dem$viojudcnstr1 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$judcnstr1_recode), p_dem$judcnstr1_recode, NA)

p_dem$viojudcnstr2 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$judcnstr2), p_dem$judcnstr2, NA)

p_dem$violegcnstr1 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$legcnstr1), p_dem$legcnstr1, NA)

p_dem$violegcnstr2 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$legcnstr2_recode), p_dem$legcnstr2_recode, NA)

p_dem$vioeqlaw1 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$eqlaw1), p_dem$eqlaw1, NA)

p_dem$vioeqlaw2 = ifelse(p_dem$treat == "Vio Treat" & !is.na(p_dem$eqlaw2_recode), p_dem$eqlaw2_recode, NA)

#Econ Treatment
p_dem$econfrexp1 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$frexp1_recode), p_dem$frexp1_recode, NA)

p_dem$econfrexp2 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$frexp2), p_dem$frexp2, NA)

p_dem$econfrassc1 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$frassc1), p_dem$frassc1, NA)

p_dem$econfrassc2 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$frassc2_recode), p_dem$frassc2_recode, NA)

p_dem$econfrassc3 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$frassc3), p_dem$frassc3, NA)

p_dem$econunisuff1 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$unisuff1), p_dem$unisuff1, NA)

p_dem$econunisuff2 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$unisuff2_recode), p_dem$unisuff2_recode, NA)

p_dem$econdecelec1 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$decelec1), p_dem$decelec1, NA)

p_dem$econdecelec2 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$decelec2_recode), p_dem$decelec2_recode, NA)

p_dem$econfrelect1 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$frelect1_recode), p_dem$frelect1_recode, NA)

p_dem$econfrelect2 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$frelect2), p_dem$frelect2, NA)

p_dem$econjudcnstr1 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$judcnstr1_recode), p_dem$judcnstr1_recode, NA)

p_dem$econjudcnstr2 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$judcnstr2), p_dem$judcnstr2, NA)

p_dem$econlegcnstr1 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$legcnstr1), p_dem$legcnstr1, NA)

p_dem$econlegcnstr2 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$legcnstr2_recode), p_dem$legcnstr2_recode, NA)

p_dem$econeqlaw1 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$eqlaw1), p_dem$eqlaw1, NA)

p_dem$econeqlaw2 = ifelse(p_dem$treat == "Econ Treat" & !is.na(p_dem$eqlaw2_recode), p_dem$eqlaw2_recode, NA)

#Control
p_dem$confrexp1 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$frexp1_recode), p_dem$frexp1_recode, NA)

p_dem$confrexp2 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$frexp2), p_dem$frexp2, NA)

p_dem$confrassc1 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$frassc1), p_dem$frassc1, NA)

p_dem$confrassc2 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$frassc2_recode), p_dem$frassc2_recode, NA)

p_dem$confrassc3 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$frassc3), p_dem$frassc3, NA)

p_dem$conunisuff1 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$unisuff1), p_dem$unisuff1, NA)

p_dem$conunisuff2 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$unisuff2_recode), p_dem$unisuff2_recode, NA)

p_dem$condecelec1 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$decelec1), p_dem$decelec1, NA)

p_dem$condecelec2 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$decelec2_recode), p_dem$decelec2_recode, NA)

p_dem$confrelect1 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$frelect1_recode), p_dem$frelect1_recode, NA)

p_dem$confrelect2 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$frelect2), p_dem$frelect2, NA)

p_dem$conjudcnstr1 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$judcnstr1_recode), p_dem$judcnstr1_recode, NA)

p_dem$conjudcnstr2 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$judcnstr2), p_dem$judcnstr2, NA)

p_dem$conlegcnstr1 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$legcnstr1), p_dem$legcnstr1, NA)

p_dem$conlegcnstr2 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$legcnstr2_recode), p_dem$legcnstr2_recode, NA)

p_dem$coneqlaw1 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$eqlaw1), p_dem$eqlaw1, NA)

p_dem$coneqlaw2 = ifelse(p_dem$treat == "Control" & !is.na(p_dem$eqlaw2_recode), p_dem$eqlaw2_recode, NA)


###T-tests -------------------------------------------------------------------------
#Frexp1
frexp1_ve <- t.test(p_dem$viofrexp1, p_dem$econfrexp1, var.equal = TRUE)
frexp1_ve

frexp1_vc <- t.test(p_dem$viofrexp1, p_dem$confrexp1, var.equal = TRUE)
frexp1_vc

frexp1_ce <- t.test(p_dem$confrexp1, p_dem$econfrexp1, var.equal = TRUE)
frexp1_ce

#Frexp2
frexp2_ve <- t.test(p_dem$viofrexp2, p_dem$econfrexp2, var.equal = TRUE)
frexp2_ve

frexp2_vc <- t.test(p_dem$viofrexp2, p_dem$confrexp2, var.equal = TRUE)
frexp2_vc

frexp2_ce <- t.test(p_dem$confrexp2, p_dem$econfrexp2, var.equal = TRUE)
frexp2_ce

#Frassc1
frassc1_ve <- t.test(p_dem$viofrassc1, p_dem$econfrassc1, var.equal = TRUE)
frassc1_ve

frassc2_vc <- t.test(p_dem$viofrassc2, p_dem$confrassc2, var.equal = TRUE)
frassc2_vc

frassc2_ce <- t.test(p_dem$confrassc2, p_dem$econfrassc2, var.equal = TRUE)
frassc2_ce

#Frassc3
frassc3_ve <- t.test(p_dem$viofrassc3, p_dem$econfrassc3, var.equal = TRUE)
frassc3_ve

frassc3_vc <- t.test(p_dem$viofrassc3, p_dem$confrassc3, var.equal = TRUE)
frassc3_vc

frassc3_ce <- t.test(p_dem$confrassc3, p_dem$econfrassc3, var.equal = TRUE)
frassc3_ce

#Unisuff1
unisuff1_ve <- t.test(p_dem$viounisuff1, p_dem$econunisuff1, var.equal = TRUE)
unisuff1_ve

unisuff1_vc <- t.test(p_dem$viounisuff1, p_dem$conunisuff1, var.equal = TRUE)
unisuff1_vc

unisuff1_ce <- t.test(p_dem$conunisuff1, p_dem$econunisuff1, var.equal = TRUE)
unisuff1_ce

#Unisuff2
unisuff2_ve <- t.test(p_dem$viounisuff2, p_dem$econunisuff2, var.equal = TRUE)
unisuff2_ve

unisuff2_vc <- t.test(p_dem$viounisuff2, p_dem$conunisuff2, var.equal = TRUE)
unisuff2_vc

unisuff2_ce <- t.test(p_dem$conunisuff2, p_dem$econunisuff2, var.equal = TRUE)
unisuff2_ce

#Decelec1
decelec1_ve <- t.test(p_dem$viodecelec1, p_dem$econdecelec1, var.equal = TRUE)
decelec1_ve

decelec1_vc <- t.test(p_dem$viodecelec1, p_dem$condecelec1, var.equal = TRUE)
decelec1_vc

decelec1_ce <- t.test(p_dem$condecelec1, p_dem$econdecelec1, var.equal = TRUE)
decelec1_ce

#Decelec2
decelec2_ve <- t.test(p_dem$viodecelec2, p_dem$econdecelec2, var.equal = TRUE)
decelec2_ve

decelec2_vc <- t.test(p_dem$viodecelec2, p_dem$condecelec2, var.equal = TRUE)
decelec2_vc

decelec2_ce <- t.test(p_dem$condecelec2, p_dem$econdecelec2, var.equal = TRUE)
decelec2_ce

#Frelect1
frelect1_ve <- t.test(p_dem$viofrelect1, p_dem$econfrelect1, var.equal = TRUE)
frelect1_ve

frelect1_vc <- t.test(p_dem$viofrelect1, p_dem$confrelect1, var.equal = TRUE)
frelect1_vc

frelect1_ce <- t.test(p_dem$confrelect1, p_dem$econfrelect1, var.equal = TRUE)
frelect1_ce

#Frelect2
frelect2_ve <- t.test(p_dem$viofrelect2, p_dem$econfrelect2, var.equal = TRUE)
frelect2_ve

frelect2_vc <- t.test(p_dem$viofrelect2, p_dem$confrelect2, var.equal = TRUE)
frelect2_vc

frelect2_ce <- t.test(p_dem$confrelect2, p_dem$econfrelect2, var.equal = TRUE)
frelect2_ce

#Judcnstr1
judcnstr1_ve <- t.test(p_dem$viojudcnstr1, p_dem$econjudcnstr1, var.equal = TRUE)
judcnstr1_ve

judcnstr1_vc <- t.test(p_dem$viojudcnstr1, p_dem$conjudcnstr1, var.equal = TRUE)
judcnstr1_vc

judcnstr1_ce <- t.test(p_dem$conjudcnstr1, p_dem$econjudcnstr1, var.equal = TRUE)
judcnstr1_ce

#Judcnstr2
judcnstr2_ve <- t.test(p_dem$viojudcnstr2, p_dem$econjudcnstr2, var.equal = TRUE)
judcnstr2_ve

judcnstr2_vc <- t.test(p_dem$viojudcnstr2, p_dem$conjudcnstr2, var.equal = TRUE)
judcnstr2_vc

judcnstr2_ce <- t.test(p_dem$conjudcnstr2, p_dem$econjudcnstr2, var.equal = TRUE)
judcnstr2_ce

#Legcnstr1
legcnstr1_ve <- t.test(p_dem$violegcnstr1, p_dem$econlegcnstr1, var.equal = TRUE)
legcnstr1_ve

legcnstr1_vc <- t.test(p_dem$violegcnstr1, p_dem$conlegcnstr1, var.equal = TRUE)
legcnstr1_vc

legcnstr1_ce <- t.test(p_dem$conlegcnstr1, p_dem$econlegcnstr1, var.equal = TRUE)
legcnstr1_ce

#Legcnstr2
legcnstr2_ve <- t.test(p_dem$violegcnstr2, p_dem$econlegcnstr2, var.equal = TRUE)
legcnstr2_ve

legcnstr2_vc <- t.test(p_dem$violegcnstr2, p_dem$conlegcnstr2, var.equal = TRUE)
legcnstr2_vc

legcnstr2_ce <- t.test(p_dem$conlegcnstr2, p_dem$econlegcnstr2, var.equal = TRUE)
legcnstr2_ce

#Eqlaw1
eqlaw1_ve <- t.test(p_dem$vioeqlaw1, p_dem$econeqlaw1, var.equal = TRUE)
eqlaw1_ve

eqlaw1_vc <- t.test(p_dem$vioeqlaw1, p_dem$coneqlaw1, var.equal = TRUE)
eqlaw1_vc

eqlaw1_ce <- t.test(p_dem$coneqlaw1, p_dem$econeqlaw1, var.equal = TRUE)
eqlaw1_ce

#Eqlaw2
eqlaw2_ve <- t.test(p_dem$vioeqlaw2, p_dem$econeqlaw2, var.equal = TRUE)
eqlaw2_ve

eqlaw2_vc <- t.test(p_dem$vioeqlaw2, p_dem$coneqlaw2, var.equal = TRUE)
eqlaw2_vc

eqlaw2_ce <- t.test(p_dem$coneqlaw2, p_dem$econeqlaw2, var.equal = TRUE)
eqlaw2_ce
