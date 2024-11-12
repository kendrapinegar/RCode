###Creation of Indices: Forward Index, Reverse Index, and Claassen Index; Narrow and Broad, With and Without Bad Completes
###July 14, 2023
###Last Updated: August 10, 2023

#Controls: age, gender (female, not-female), region, income (continuous), ideology

##NOTE: This drops responses "I don't know" and "Prefer not to respond" (coded as NA)

###Load the datasets and packages -----------------------------------------------
library(readxl)
library(stargazer)
library(writexl)
library(car)
m_dem <- read_excel("MX_Data_7.11.23.xlsx")
p_dem <- read_excel("PE_Data_6.26.23.xlsx")


###Cleaning Code -----------------------------------------------------------

#Treatment
m_dem$treat<- "Control"
m_dem$treat[m_dem$econ != ""] <- "Econ Treat"
m_dem$treat[m_dem$vio != ""] <- "Vio Treat"
m_dem$treat <- as.factor(m_dem$treat)
m_dem$treat <- relevel(m_dem$treat, ref="Control")
table(m_dem$treat)

p_dem$treat<- "Control"
p_dem$treat[p_dem$econ != ""] <- "Econ Treat"
p_dem$treat[p_dem$vio != ""] <- "Vio Treat"
p_dem$treat <- as.factor(p_dem$treat)
p_dem$treat <- relevel(p_dem$treat, ref="Control")
table(p_dem$treat)

#Age (no prefer not to respond answers)
class(m_dem$age)
class(p_dem$age)

p_dem$age = as.numeric(p_dem$age)

#Gender (should be coded female vs. non-female, including "prefer not to respond" and "other")
m_dem$gender_n <- 1
m_dem$gender_n[m_dem$gender == "Mujer/femenino"] <- 2

p_dem$gender_n <- 1
p_dem$gender_n[p_dem$gender == "Mujer/femenino"] <- 2

class(m_dem$gender_n)
class(p_dem$gender_n)

#Income (got rid of any prefer not to respond answers)
m_dem$income_n<-NA
m_dem$income_n[m_dem$income=="Entre $0 y $2,900 pesos"]<-1
m_dem$income_n[m_dem$income=="Entre $2,901 y $3,600 pesos"]<-2
m_dem$income_n[m_dem$income=="Entre $3,601 y $5,800 pesos"]<-3
m_dem$income_n[m_dem$income=="Entre $5,801 y $8,700 pesos"]<-4
m_dem$income_n[m_dem$income=="Más de $8,700 pesos"]<-5
class(m_dem$income_n)

p_dem$income_n <- NA
p_dem$income_n[p_dem$income == "Entre 0 y 350 soles"] <- 1
p_dem$income_n[p_dem$income == "Entre 351 y 840 soles"] <- 2
p_dem$income_n[p_dem$income == "Entre 841 y 1200 soles"] <- 3
p_dem$income_n[p_dem$income == "Entre 1201 y 1850 soles"] <- 4
p_dem$income_n[p_dem$income == "Más de 1850 soles"] <- 5
class(p_dem$income_n)

#Ideology (no prefer not to respond answers)
class(m_dem$ideo_1)
class(p_dem$ideo_1)
p_dem$ideo_1 = as.numeric(p_dem$ideo_1)

##Democracy outcomes

#Mexico
m_dem$frexp1 <-car::recode(m_dem$frexp1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="3";
                           "Prefiero no responder"="3"')
m_dem$frexp1<-as.numeric(m_dem$frexp1)

m_dem$frexp2 <-car::recode(m_dem$frexp2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$frexp2<-as.numeric(m_dem$frexp2)

m_dem$frassc1 <-car::recode(m_dem$frassc1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$frassc1<-as.numeric(m_dem$frassc1)

m_dem$frassc2 <-car::recode(m_dem$frassc2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$frassc2<-as.numeric(m_dem$frassc2)

m_dem$frassc3 <-car::recode(m_dem$frassc3, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$frassc3<-as.numeric(m_dem$frassc3)

m_dem$unisuff1 <-car::recode(m_dem$unisuff1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$unisuff1<-as.numeric(m_dem$unisuff1)

m_dem$unisuff2 <-car::recode(m_dem$unisuff2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$unisuff2<-as.numeric(m_dem$unisuff2)

m_dem$decelec1 <-car::recode(m_dem$decelec1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$decelec1<-as.numeric(m_dem$decelec1)

m_dem$decelec2 <-car::recode(m_dem$decelec2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$decelec2<-as.numeric(m_dem$decelec2)

m_dem$frelect1 <-car::recode(m_dem$frelect1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$frelect1<-as.numeric(m_dem$frelect1)

m_dem$frelect2 <-car::recode(m_dem$frelect2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$frelect2<-as.numeric(m_dem$frelect2)

m_dem$judcnstr1 <-car::recode(m_dem$judcnstr1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$judcnstr1<-as.numeric(m_dem$judcnstr1)

m_dem$judcnstr2 <-car::recode(m_dem$judcnstr2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$judcnstr2<-as.numeric(m_dem$judcnstr2)

m_dem$legcnstr1 <-car::recode(m_dem$legcnstr1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$legcnstr1<-as.numeric(m_dem$legcnstr1)

m_dem$legcnstr2 <-car::recode(m_dem$legcnstr2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$legcnstr2<-as.numeric(m_dem$legcnstr2)

m_dem$eqlaw1 <-car::recode(m_dem$eqlaw1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$eqlaw1<-as.numeric(m_dem$eqlaw1)

m_dem$eqlaw2 <-car::recode(m_dem$eqlaw2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
m_dem$eqlaw2<-as.numeric(m_dem$eqlaw2)

#Peru
p_dem$frexp1 <-car::recode(p_dem$frexp1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$frexp1<-as.numeric(p_dem$frexp1)

p_dem$frexp2 <-car::recode(p_dem$frexp2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$frexp2<-as.numeric(p_dem$frexp2)

p_dem$frassc1 <-car::recode(p_dem$frassc1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$frassc1<-as.numeric(p_dem$frassc1)

p_dem$frassc2 <-car::recode(p_dem$frassc2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$frassc2<-as.numeric(p_dem$frassc2)

p_dem$frassc3 <-car::recode(p_dem$frassc3, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$frassc3<-as.numeric(p_dem$frassc3)

p_dem$unisuff1 <-car::recode(p_dem$unisuff1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$unisuff1<-as.numeric(p_dem$unisuff1)

p_dem$unisuff2 <-car::recode(p_dem$unisuff2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$unisuff2<-as.numeric(p_dem$unisuff2)

p_dem$decelec1 <-car::recode(p_dem$decelec1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$decelec1<-as.numeric(p_dem$decelec1)

p_dem$decelec2 <-car::recode(p_dem$decelec2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$decelec2<-as.numeric(p_dem$decelec2)

p_dem$frelect1 <-car::recode(p_dem$frelect1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$frelect1<-as.numeric(p_dem$frelect1)

p_dem$frelect2 <-car::recode(p_dem$frelect2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$frelect2<-as.numeric(p_dem$frelect2)

p_dem$judcnstr1 <-car::recode(p_dem$judcnstr1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$judcnstr1<-as.numeric(p_dem$judcnstr1)

p_dem$judcnstr2 <-car::recode(p_dem$judcnstr2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$judcnstr2<-as.numeric(p_dem$judcnstr2)

p_dem$legcnstr1 <-car::recode(p_dem$legcnstr1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$legcnstr1<-as.numeric(p_dem$legcnstr1)

p_dem$legcnstr2 <-car::recode(p_dem$legcnstr2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$legcnstr2<-as.numeric(p_dem$legcnstr2)

p_dem$eqlaw1 <-car::recode(p_dem$eqlaw1, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$eqlaw1<-as.numeric(p_dem$eqlaw1)

p_dem$eqlaw2 <-car::recode(p_dem$eqlaw2, '
                           "Muy de acuerdo"="5";
                           "Algo de acuerdo"="4";
                           "Ni de acuerdo ni en desacuerdo"="3";
                           "Algo en desacuerdo"="2";
                           "Muy en desacuerdo"="1";
                           "No lo sé"="NA";
                           "Prefiero no responder"="NA"')
p_dem$eqlaw2<-as.numeric(p_dem$eqlaw2)

#Flip the outcome variables that need to be recoded

#Mexico
m_dem$frexp2_recode<- car::recode(m_dem$frexp2, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')
m_dem$frassc1_recode<- car::recode(m_dem$frassc1, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')
m_dem$frassc3_recode<- car::recode(m_dem$frassc3, '
                                   "5"="1";
                                   "4"="2";
                                   "3"="3";
                                   "2"="4";
                                   "1"="5"
                                   ')
m_dem$unisuff1_recode<- car::recode(m_dem$unisuff1, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')
m_dem$decelec1_recode<- car::recode(m_dem$decelec1, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')
m_dem$frelect2_recode<- car::recode(m_dem$frelect2, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')
m_dem$legcnstr1_recode<- car::recode(m_dem$legcnstr1, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')
m_dem$judcnstr2_recode<- car::recode(m_dem$judcnstr2, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')
m_dem$eqlaw1_recode<- car::recode(m_dem$eqlaw1, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')

#Peru
p_dem$frexp2_recode<- car::recode(p_dem$frexp2, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')
p_dem$frassc1_recode<- car::recode(p_dem$frassc1, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')
p_dem$frassc3_recode<- car::recode(p_dem$frassc3, '
                                   "5"="1";
                                   "4"="2";
                                   "3"="3";
                                   "2"="4";
                                   "1"="5"
                                   ')
p_dem$unisuff1_recode<- car::recode(p_dem$unisuff1, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')
p_dem$decelec1_recode<- car::recode(p_dem$decelec1, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')
p_dem$frelect2_recode<- car::recode(p_dem$frelect2, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')
p_dem$legcnstr1_recode<- car::recode(p_dem$legcnstr1, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')
p_dem$judcnstr2_recode<- car::recode(p_dem$judcnstr2, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')
p_dem$eqlaw1_recode<- car::recode(p_dem$eqlaw1, '
                                           "5"="1";
                                           "4"="2";
                                           "3"="3";
                                           "2"="4";
                                           "1"="5"
                                           ')

#Cleaning moderator variables

#Mexico
m_dem$pres <- car::recode(m_dem$pres, '
                          "Muy bien" = "5";
                          "Bien" = "4";
                          "Ni bien ni mal" = "3";
                          "Mal" = "2";
                          "Muy mal" = "1";
                          "No lo sé" = "NA";
                          "Prefiero no responder" = "NA"')
m_dem$pres <- as.numeric(m_dem$pres)
m_dem$pers_vio <- car::recode(m_dem$pers_vio, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
m_dem$pers_vio <- as.numeric(m_dem$pers_vio)
m_dem$count_vio <- car::recode(m_dem$count_vio, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
m_dem$count_vio <- as.numeric(m_dem$count_vio)
m_dem$pers_econ <- car::recode(m_dem$pers_econ, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
m_dem$pers_econ <- as.numeric(m_dem$pers_econ)
m_dem$count_econ <- car::recode(m_dem$count_econ, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
m_dem$count_econ <- as.numeric(m_dem$count_econ)
m_dem$politpart <- car::recode(m_dem$politpart, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
m_dem$politpart <- as.numeric(m_dem$politpart)
m_dem$elite <- car::recode(m_dem$elite, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
m_dem$elite <- as.numeric(m_dem$elite)
m_dem$party <- as.factor(m_dem$party)
#Peru
p_dem$pres <- car::recode(p_dem$pres, '
                          "Muy bien" = "5";
                          "Bien" = "4";
                          "Ni bien ni mal" = "3";
                          "Mal" = "2";
                          "Muy mal" = "1";
                          "No lo sé" = "NA";
                          "Prefiero no responder" = "NA"')
p_dem$pres <- as.numeric(p_dem$pres)
p_dem$pers_vio <- car::recode(p_dem$pers_vio, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
p_dem$pers_vio <- as.numeric(p_dem$pers_vio)
p_dem$count_vio <- car::recode(p_dem$count_vio, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
p_dem$count_vio <- as.numeric(p_dem$count_vio)
p_dem$pers_econ <- car::recode(p_dem$pers_econ, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
p_dem$pers_econ <- as.numeric(p_dem$pers_econ)
p_dem$count_econ <- car::recode(p_dem$count_econ, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
p_dem$count_econ <- as.numeric(p_dem$count_econ)
p_dem$politpart <- car::recode(p_dem$politpart, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
p_dem$politpart <- as.numeric(p_dem$politpart)
p_dem$elite <- car::recode(p_dem$elite, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
p_dem$elite <- as.numeric(p_dem$elite)
p_dem$party <- as.factor(p_dem$party)
#Recoding moderator variables

#Mexico
m_dem$count_vio_recode <- car::recode(m_dem$count_vio, '
                                     "5" = "1";
                                     "4" = "2";
                                     "3" = "3";
                                     "2" = "4";
                                     "1" = "5"')
m_dem$count_econ_recode <- car::recode(m_dem$count_econ, '
                               "5" = "1";
                               "4" = "2";
                               "3" = "3";
                               "2" = "4";
                               "1" = "5"')
m_dem$elite_recode <- car::recode(m_dem$elite, '
                                      "5" = "1";
                                      "4" = "2";
                                      "3" = "3";
                                      "2" = "4";
                                      "1" = "5"')
m_dem$party_recode_1 <- car::recode(m_dem$party, '
                                    "MORENA" = "1";
                                    "PRI" = "0";
                                    "PRD" = "0";
                                    "PAN" = "0";
                                    "Otro (¿Cuál?)" = "0";
                                    "No sé" = "0";
                                    "Prefiero no responder" = "0"',
                                    as.factor = TRUE)
m_dem$party_recode_2 <- car::recode(m_dem$party, '
                                    "MORENA" = "2";
                                    "PRI" = "1";
                                    "PRD" = "1";
                                    "PAN" = "1";
                                    "Otro (¿Cuál?)" = "1";
                                    "No sé" = "0";
                                    "Prefiero no responder" = "0"',
                                    as.factor = TRUE)
m_dem$elite_recode_2 <- m_dem$elite_recode
m_dem$elite_recode_2 <- as.factor(m_dem$elite_recode_2)

#Peru
p_dem$count_vio_recode <- car::recode(p_dem$count_vio, '
                                     "5" = "1";
                                     "4" = "2";
                                     "3" = "3";
                                     "2" = "4";
                                     "1" = "5"')
p_dem$count_econ_recode <- car::recode(p_dem$count_econ, '
                               "5" = "1";
                               "4" = "2";
                               "3" = "3";
                               "2" = "4";
                               "1" = "5"')
p_dem$elite_recode <- car::recode(p_dem$elite, '
                                      "5" = "1";
                                      "4" = "2";
                                      "3" = "3";
                                      "2" = "4";
                                      "1" = "5"')
p_dem$party_recode_1 <- car::recode(p_dem$party, '
                                    "Fuerza Popular" = "1";
                                    "Renovación Popular" = "1";
                                    "Avanza País" = "1";
                                    "Acción Popular" = "0";
                                    "Alianza Para El Progreso" = "0";
                                    "Juntos Por El Perú" = "0";
                                    "Perú Libre" = "0";
                                    "Otro (¿Cuál?)" = "0";
                                    "No sé" = "0";
                                    "Prefiero no responder" = "0"',
                                    as.factor = TRUE)
p_dem$party_recode_2 <- car::recode(p_dem$party, '
                                    "Fuerza Popular" = "2";
                                    "Renovación Popular" = "2";
                                    "Avanza País" = "2";
                                    "Acción Popular" = "1";
                                    "Alianza Para El Progreso" = "1";
                                    "Juntos Por El Perú" = "1";
                                    "Perú Libre" = "1";
                                    "Otro (¿Cuál?)" = "1";
                                    "No sé" = "0";
                                    "Prefiero no responder" = "0"',
                                    as.factor = TRUE)
p_dem$elite_recode_2 <- p_dem$elite_recode
p_dem$elite_recode_2 <- as.factor(p_dem$elite_recode_2)


### Coding Narrow Indices WITH Bad Completes ---------------------------------------

#Recoded questions indices, Reverse Index (with bad completes)
m_dem$reverse_index <- (m_dem$frexp2_recode + m_dem$frassc1_recode + m_dem$frassc3_recode + m_dem$unisuff1_recode + m_dem$decelec1_recode + m_dem$frelect2_recode + m_dem$judcnstr2_recode + m_dem$legcnstr1_recode + m_dem$eqlaw1_recode)/9

p_dem$reverse_index <- (p_dem$frexp2_recode + p_dem$frassc1_recode + p_dem$frassc3_recode + p_dem$unisuff1_recode + p_dem$decelec1_recode + p_dem$frelect2_recode + p_dem$judcnstr2_recode + p_dem$legcnstr1_recode + p_dem$eqlaw1_recode)/9

#Not recoded questions indices, Forward Index (with bad completes)
m_dem$forward_index <- (m_dem$frexp1 + m_dem$frassc2 + m_dem$unisuff2 + m_dem$decelec2 + m_dem$frelect1 + m_dem$judcnstr1 + m_dem$legcnstr2 + m_dem$eqlaw2)/8

p_dem$forward_index <- (p_dem$frexp1 + p_dem$frassc2 + p_dem$unisuff2 + p_dem$decelec2 + p_dem$frelect1 + p_dem$judcnstr1 + p_dem$legcnstr2 + p_dem$eqlaw2)/8

#Claassen 7 question indices, Claassen Index (with bad completes)
m_dem$claassen_index <- (m_dem$frexp2_recode + m_dem$frassc1_recode + m_dem$unisuff2 + m_dem$frelect2_recode + m_dem$judcnstr2_recode + m_dem$legcnstr2 + m_dem$eqlaw1_recode)/7

p_dem$claassen_index <- (p_dem$frexp2_recode + p_dem$frassc1_recode + p_dem$unisuff2 + p_dem$frelect2_recode + p_dem$judcnstr2_recode + p_dem$legcnstr2 + p_dem$eqlaw1_recode)/7



### Narrow Regressions and Interactions WITH Bad Completes -------------------------

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



### Exporting the Narrow Results WITH Bad Completes --------------------------------

#regressions (with bad completes)
regressions_mex <- list(reg_index1_mex_nc, reg_index1_mex, reg_index2_mex_nc, reg_index2_mex, reg_index3_mex_nc, reg_index3_mex, type = "text")
regressions_mex <- stargazer(regressions_mex, type = "text")
regressions_mex_matrix <- as.matrix(regressions_mex)
write_xlsx(as.data.frame(regressions_mex_matrix), path = "Results.xlsx")

regressions_per <- list(reg_index1_per_nc, reg_index1_per, reg_index2_per_nc, reg_index2_per, reg_index3_per_nc, reg_index3_per, type="text")
regressions_per <- stargazer(regressions_per, type="text")
regressions_per_matrix <- as.matrix(regressions_per)
write_xlsx(as.data.frame(regressions_per_matrix), path="Results.xlsx")

#pres interactions (with bad completes)
int_pres_mex <- list(int_pres_index1_mex_nc, int_pres_index1_mex, int_pres_index2_mex_nc, int_pres_index2_mex, int_pres_index3_mex_nc, int_pres_index3_mex, type="text")
int_pres_mex <- stargazer(int_pres_mex, type="text")
int_pres_mex_matrix <- as.matrix(int_pres_mex)
write_xlsx(as.data.frame(int_pres_mex_matrix), path="Results.xlsx")

int_pres_per <- list(int_pres_index1_per_nc, int_pres_index1_per, int_pres_index2_per_nc, int_pres_index2_per, int_pres_index3_per_nc, int_pres_index3_per, type="text")
int_pres_per <- stargazer(int_pres_per, type="text")
int_pres_per_matrix <- as.matrix(int_pres_per)
write_xlsx(as.data.frame(int_pres_per_matrix), path="Results.xlsx")

#persecon interactions (with bad completes)
int_persecon_mex <- list(int_persecon_index1_mex_nc, int_persecon_index1_mex, int_persecon_index2_mex_nc, int_persecon_index2_mex, int_persecon_index3_mex_nc, int_persecon_index3_mex, type="text")
int_persecon_mex <- stargazer(int_persecon_mex, type="text")
int_persecon_mex_matrix <- as.matrix(int_persecon_mex)
write_xlsx(as.data.frame(int_persecon_mex_matrix), path="Results.xlsx")

int_persecon_per <- list(int_persecon_index1_per_nc, int_persecon_index1_per, int_persecon_index2_per_nc, int_persecon_index2_per, int_persecon_index3_per_nc, int_persecon_index3_per, type="text")
int_persecon_per <- stargazer(int_persecon_per, type="text")
int_persecon_per_matrix <- as.matrix(int_persecon_per)
write_xlsx(as.data.frame(int_persecon_per_matrix), path="Results.xlsx")

#persvio interactions (with bad completes)
int_persvio_mex <- list(int_persvio_index1_mex_nc, int_persvio_index1_mex, int_persvio_index2_mex_nc, int_persvio_index2_mex, int_persvio_index3_mex_nc, int_persvio_index3_mex, type="text")
int_persvio_mex <- stargazer(int_persvio_mex, type="text")
int_persvio_mex_matrix <- as.matrix(int_persvio_mex)
write_xlsx(as.data.frame(int_persvio_mex_matrix), path="Results.xlsx")

int_persvio_per <- list(int_persvio_index1_per_nc, int_persvio_index1_per, int_persvio_index2_per_nc, int_persvio_index2_per, int_persvio_index3_per_nc, int_persvio_index3_per, type="text")
int_persvio_per <- stargazer(int_persvio_per, type="text")
int_persvio_per_matrix <- as.matrix(int_persvio_per)
write_xlsx(as.data.frame(int_persvio_per_matrix), path="Results.xlsx")

#countecon recode interactions (with bad completes)
int_countecon_mex <- list(int_countecon_index1_mex_nc, int_countecon_index1_mex, int_countecon_index2_mex_nc, int_countecon_index2_mex, int_countecon_index3_mex_nc, int_countecon_index3_mex, type="text")
int_countecon_mex <- stargazer(int_countecon_mex, type="text")
int_countecon_mex_matrix <- as.matrix(int_countecon_mex)
write_xlsx(as.data.frame(int_countecon_mex_matrix), path="Results.xlsx")

int_countecon_per <- list(int_countecon_index1_per_nc, int_countecon_index1_per, int_countecon_index2_per_nc, int_countecon_index2_per, int_countecon_index3_per_nc, int_countecon_index3_per, type="text")
int_countecon_per <- stargazer(int_countecon_per, type="text")
int_countecon_per_matrix <- as.matrix(int_countecon_per)
write_xlsx(as.data.frame(int_countecon_per_matrix), path="Results.xlsx")

#countvio recode interactions (with bad completes)
int_countvio_mex <- list(int_countvio_index1_mex_nc, int_countvio_index1_mex, int_countvio_index2_mex_nc, int_countvio_index2_mex, int_countvio_index3_mex_nc, int_countvio_index3_mex, type="text")
int_countvio_mex <- stargazer(int_countvio_mex, type="text")
int_countvio_mex_matrix <- as.matrix(int_countvio_mex)
write_xlsx(as.data.frame(int_countvio_mex_matrix), path="Results.xlsx")

int_countvio_per <- list(int_countvio_index1_per_nc, int_countvio_index1_per, int_countvio_index2_per_nc, int_countvio_index2_per, int_countvio_index3_per_nc, int_countvio_index3_per, type="text")
int_countvio_per <- stargazer(int_countvio_per, type="text")
int_countvio_per_matrix <- as.matrix(int_countvio_per)
write_xlsx(as.data.frame(int_countvio_per_matrix), path="Results.xlsx")

#politpart interactions (with bad completes)
int_politpart_mex <- list(int_polit_index1_mex_nc, int_polit_index1_mex, int_polit_index2_mex_nc, int_polit_index2_mex, int_polit_index3_mex_nc, int_polit_index3_mex, type="text")
int_politpart_mex <- stargazer(int_politpart_mex, type="text")
int_politpart_mex_matrix <- as.matrix(int_politpart_mex)
write_xlsx(as.data.frame(int_politpart_mex_matrix), path="Results.xlsx")

int_politpart_per <- list(int_polit_index1_per_nc, int_polit_index1_per, int_polit_index2_per_nc, int_polit_index2_per, int_polit_index3_per_nc, int_polit_index3_per, type="text")
int_politpart_per <- stargazer(int_politpart_per, type="text")
int_politpart_per_matrix <- as.matrix(int_politpart_per)
write_xlsx(as.data.frame(int_politpart_per_matrix), path="Results.xlsx")

#elite recode interactions (with bad completes)
int_elite_mex <- list(int_elite_index1_mex_nc, int_elite_index1_mex, int_elite_index2_mex_nc, int_elite_index2_mex, int_elite_index3_mex_nc, int_elite_index3_mex, type="text")
int_elite_mex <- stargazer(int_elite_mex, type="text")
int_elite_mex_matrix <- as.matrix(int_elite_mex)
write_xlsx(as.data.frame(int_elite_mex_matrix), path="Results.xlsx")

int_elite_per <- list(int_elite_index1_per_nc, int_elite_index1_per, int_elite_index2_per_nc, int_elite_index2_per, int_elite_index3_per_nc, int_elite_index3_per, type="text")
int_elite_per <- stargazer(int_elite_per, type="text")
int_elite_per_matrix <- as.matrix(int_elite_per)
write_xlsx(as.data.frame(int_elite_per_matrix), path="Results.xlsx")

#party recode 1 interactions (with bad completes)
int_party_mex <- list(int_party_index1_mex_nc, int_party_index1_mex, int_party_index2_mex_nc, int_party_index2_mex, int_party_index3_mex_nc, int_party_index3_mex, type="text")
int_party_mex <- stargazer(int_party_mex, type="text")
int_party_mex_matrix <- as.matrix(int_party_mex)
write_xlsx(as.data.frame(int_party_mex_matrix), path="Results.xlsx")

int_party_per <- list(int_party_index1_per_nc, int_party_index1_per, int_party_index2_per_nc, int_party_index2_per, int_party_index3_per_nc, int_party_index3_per, type="text")
int_party_per <- stargazer(int_party_per, type="text")
int_party_per_matrix <- as.matrix(int_party_per)
write_xlsx(as.data.frame(int_party_per_matrix), path="Results.xlsx")

#elite recode 2 (marginal effects) interactions (with bad completes)
int_elite2_mex <- list(int_elite2_index1_mex_nc, int_elite2_index1_mex, int_elite2_index2_mex_nc, int_elite2_index2_mex, int_elite2_index3_mex_nc, int_elite2_index3_mex, type="text")
int_elite2_mex <- stargazer(int_elite2_mex, type="text")
int_elite2_mex_matrix <- as.matrix(int_elite2_mex)
write_xlsx(as.data.frame(int_elite2_mex_matrix), path="Results.xlsx")

int_elite2_per <- list(int_elite2_index1_per_nc, int_elite2_index1_per, int_elite2_index2_per_nc, int_elite2_index2_per, int_elite2_index3_per_nc, int_elite2_index3_per, type="text")
int_elite2_per <- stargazer(int_elite2_per, type="text")
int_elite2_per_matrix <- as.matrix(int_elite2_per)
write_xlsx(as.data.frame(int_elite2_per_matrix), path="Results.xlsx")



### Dropping Bad Completes --------------------------------------------------

###CODING TO DROP THE BAD COMPLETES AND RERUN THE INDICES
#1.   They offer a gibberish answer to the open-ended response question. This is defined as words that make no sense.
#2.   Those who spend less than 4 minutes on the survey, if control, and less than 5 minutes, if treatment 
#3.   Those who put the same answer on all 17 democracy outcome questions, or all the same answer on 16 of the 17 questions. 
#4.   Those who choose one of the two “no answer” options in the democracy questions at least 75 percent of the time. 

#Mexico
m_dem$gib <- as.numeric(m_dem$gib)
m_dem$nonresp <- as.numeric(m_dem$nonresp)
m_dem$outincons <- as.numeric(m_dem$outincons)

m_dem$bad=0
m_dem$bad[m_dem$treat == "Control" & m_dem$`Duration (in seconds)` < 240]=1
m_dem$bad[m_dem$treat == "Econ Treat" & m_dem$`Duration (in seconds)` < 300]=1
m_dem$bad[m_dem$treat == "Vio Treat" & m_dem$`Duration (in seconds)` < 300]=1
m_dem$bad[m_dem$gib == 1]=1
m_dem$bad[m_dem$nonresp == 1]=1
m_dem$bad[m_dem$outincons == 1]=1
table(m_dem$bad)
m_dem <- subset(m_dem, m_dem$bad == 0)

#Peru
p_dem$gib <- as.numeric(p_dem$gib)
p_dem$nonresp <- as.numeric(p_dem$nonresp)
p_dem$outincons <- as.numeric(p_dem$outincons)
p_dem$`Duration (in seconds)` <- as.numeric(p_dem$`Duration (in seconds)`)

p_dem$bad=0
p_dem$bad[p_dem$treat == "Control" & p_dem$`Duration (in seconds)` < 240]=1
p_dem$bad[p_dem$treat == "Econ Treat" & p_dem$`Duration (in seconds)` < 300]=1
p_dem$bad[p_dem$treat == "Vio Treat" & p_dem$`Duration (in seconds)` < 300]=1
p_dem$bad[p_dem$gib == 1]=1
p_dem$bad[p_dem$nonresp == 1]=1
p_dem$bad[p_dem$outincons == 1]=1
table(p_dem$bad)
p_dem <- subset(p_dem, p_dem$bad == 0)



### Coding Narrow Indices WITHOUT Bad Completes ------------------------------------

#Recoded questions indices, Reverse Index (with bad completes)
m_dem$reverse_index <- rowMeans(m_dem[, c("frexp2_recode", "frassc1_recode", "frassc3_recode", "unisuff1_recode", "decelec1_recode", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

p_dem$reverse_index <- rowMeans(p_dem[, c("frexp2_recode", "frassc1_recode", "frassc3_recode", "unisuff1_recode", "decelec1_recode", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

#Not recoded questions indices, Forward Index (with bad completes)
m_dem$forward_index <- rowMeans(m_dem[, c("frexp1", "frassc2", "unisuff2", "decelec2", "frelect1", "judcnstr1", "legcnstr2", "eqlaw2")], na.rm = TRUE)

p_dem$forward_index <- rowMeans(p_dem[, c("frexp1", "frassc2", "unisuff2", "decelec2", "frelect1", "judcnstr1", "legcnstr2", "eqlaw2")], na.rm = TRUE)

#Claassen 7 question indices, Claassen Index (with bad completes)
m_dem$claassen_index <- rowMeans(m_dem[, c("frexp2_recode", "frassc1_recode", "unisuff2", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

p_dem$claassen_index <- rowMeans(p_dem[, c("frexp2_recode", "frassc1_recode", "unisuff2", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)



### Narrow Regressions and Interactions WITHOUT Bad Completes -----------

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



### Exporting the Narrow Results WITHOUT Bad Completes ---------------------

#regressions (without bad completes)
regressions_mex <- list(reg_index1_mex_nc, reg_index1_mex, reg_index2_mex_nc, reg_index2_mex, reg_index3_mex_nc, reg_index3_mex, type = "text")
regressions_mex <- stargazer(regressions_mex, type = "text")
regressions_mex_matrix <- as.matrix(regressions_mex)
write_xlsx(as.data.frame(regressions_mex_matrix), path = "Results.xlsx")

regressions_per <- list(reg_index1_per_nc, reg_index1_per, reg_index2_per_nc, reg_index2_per, reg_index3_per_nc, reg_index3_per, type="text")
regressions_per <- stargazer(regressions_per, type="text")
regressions_per_matrix <- as.matrix(regressions_per)
write_xlsx(as.data.frame(regressions_per_matrix), path="Results.xlsx")

#pres interactions (without bad completes)
int_pres_mex <- list(int_pres_index1_mex_nc, int_pres_index1_mex, int_pres_index2_mex_nc, int_pres_index2_mex, int_pres_index3_mex_nc, int_pres_index3_mex, type="text")
int_pres_mex <- stargazer(int_pres_mex, type="text")
int_pres_mex_matrix <- as.matrix(int_pres_mex)
write_xlsx(as.data.frame(int_pres_mex_matrix), path="Results.xlsx")

int_pres_per <- list(int_pres_index1_per_nc, int_pres_index1_per, int_pres_index2_per_nc, int_pres_index2_per, int_pres_index3_per_nc, int_pres_index3_per, type="text")
int_pres_per <- stargazer(int_pres_per, type="text")
int_pres_per_matrix <- as.matrix(int_pres_per)
write_xlsx(as.data.frame(int_pres_per_matrix), path="Results.xlsx")

#persecon interactions (without bad completes)
int_persecon_mex <- list(int_persecon_index1_mex_nc, int_persecon_index1_mex, int_persecon_index2_mex_nc, int_persecon_index2_mex, int_persecon_index3_mex_nc, int_persecon_index3_mex, type="text")
int_persecon_mex <- stargazer(int_persecon_mex, type="text")
int_persecon_mex_matrix <- as.matrix(int_persecon_mex)
write_xlsx(as.data.frame(int_persecon_mex_matrix), path="Results.xlsx")

int_persecon_per <- list(int_persecon_index1_per_nc, int_persecon_index1_per, int_persecon_index2_per_nc, int_persecon_index2_per, int_persecon_index3_per_nc, int_persecon_index3_per, type="text")
int_persecon_per <- stargazer(int_persecon_per, type="text")
int_persecon_per_matrix <- as.matrix(int_persecon_per)
write_xlsx(as.data.frame(int_persecon_per_matrix), path="Results.xlsx")

#persvio interactions (without bad completes)
int_persvio_mex <- list(int_persvio_index1_mex_nc, int_persvio_index1_mex, int_persvio_index2_mex_nc, int_persvio_index2_mex, int_persvio_index3_mex_nc, int_persvio_index3_mex, type="text")
int_persvio_mex <- stargazer(int_persvio_mex, type="text")
int_persvio_mex_matrix <- as.matrix(int_persvio_mex)
write_xlsx(as.data.frame(int_persvio_mex_matrix), path="Results.xlsx")

int_persvio_per <- list(int_persvio_index1_per_nc, int_persvio_index1_per, int_persvio_index2_per_nc, int_persvio_index2_per, int_persvio_index3_per_nc, int_persvio_index3_per, type="text")
int_persvio_per <- stargazer(int_persvio_per, type="text")
int_persvio_per_matrix <- as.matrix(int_persvio_per)
write_xlsx(as.data.frame(int_persvio_per_matrix), path="Results.xlsx")

#countecon recode interactions (without bad completes)
int_countecon_mex <- list(int_countecon_index1_mex_nc, int_countecon_index1_mex, int_countecon_index2_mex_nc, int_countecon_index2_mex, int_countecon_index3_mex_nc, int_countecon_index3_mex, type="text")
int_countecon_mex <- stargazer(int_countecon_mex, type="text")
int_countecon_mex_matrix <- as.matrix(int_countecon_mex)
write_xlsx(as.data.frame(int_countecon_mex_matrix), path="Results.xlsx")

int_countecon_per <- list(int_countecon_index1_per_nc, int_countecon_index1_per, int_countecon_index2_per_nc, int_countecon_index2_per, int_countecon_index3_per_nc, int_countecon_index3_per, type="text")
int_countecon_per <- stargazer(int_countecon_per, type="text")
int_countecon_per_matrix <- as.matrix(int_countecon_per)
write_xlsx(as.data.frame(int_countecon_per_matrix), path="Results.xlsx")

#countvio recode interactions (without bad completes)
int_countvio_mex <- list(int_countvio_index1_mex_nc, int_countvio_index1_mex, int_countvio_index2_mex_nc, int_countvio_index2_mex, int_countvio_index3_mex_nc, int_countvio_index3_mex, type="text")
int_countvio_mex <- stargazer(int_countvio_mex, type="text")
int_countvio_mex_matrix <- as.matrix(int_countvio_mex)
write_xlsx(as.data.frame(int_countvio_mex_matrix), path="Results.xlsx")

int_countvio_per <- list(int_countvio_index1_per_nc, int_countvio_index1_per, int_countvio_index2_per_nc, int_countvio_index2_per, int_countvio_index3_per_nc, int_countvio_index3_per, type="text")
int_countvio_per <- stargazer(int_countvio_per, type="text")
int_countvio_per_matrix <- as.matrix(int_countvio_per)
write_xlsx(as.data.frame(int_countvio_per_matrix), path="Results.xlsx")

#politpart interactions (without bad completes)
int_politpart_mex <- list(int_polit_index1_mex_nc, int_polit_index1_mex, int_polit_index2_mex_nc, int_polit_index2_mex, int_polit_index3_mex_nc, int_polit_index3_mex, type="text")
int_politpart_mex <- stargazer(int_politpart_mex, type="text")
int_politpart_mex_matrix <- as.matrix(int_politpart_mex)
write_xlsx(as.data.frame(int_politpart_mex_matrix), path="Results.xlsx")

int_politpart_per <- list(int_polit_index1_per_nc, int_polit_index1_per, int_polit_index2_per_nc, int_polit_index2_per, int_polit_index3_per_nc, int_polit_index3_per, type="text")
int_politpart_per <- stargazer(int_politpart_per, type="text")
int_politpart_per_matrix <- as.matrix(int_politpart_per)
write_xlsx(as.data.frame(int_politpart_per_matrix), path="Results.xlsx")

#elite recode interactions (without bad completes)
int_elite_mex <- list(int_elite_index1_mex_nc, int_elite_index1_mex, int_elite_index2_mex_nc, int_elite_index2_mex, int_elite_index3_mex_nc, int_elite_index3_mex, type="text")
int_elite_mex <- stargazer(int_elite_mex, type="text")
int_elite_mex_matrix <- as.matrix(int_elite_mex)
write_xlsx(as.data.frame(int_elite_mex_matrix), path="Results.xlsx")

int_elite_per <- list(int_elite_index1_per_nc, int_elite_index1_per, int_elite_index2_per_nc, int_elite_index2_per, int_elite_index3_per_nc, int_elite_index3_per, type="text")
int_elite_per <- stargazer(int_elite_per, type="text")
int_elite_per_matrix <- as.matrix(int_elite_per)
write_xlsx(as.data.frame(int_elite_per_matrix), path="Results.xlsx")

#party recode 1 interactions (without bad completes)
int_party_mex <- list(int_party_index1_mex_nc, int_party_index1_mex, int_party_index2_mex_nc, int_party_index2_mex, int_party_index3_mex_nc, int_party_index3_mex, type="text")
int_party_mex <- stargazer(int_party_mex, type="text")
int_party_mex_matrix <- as.matrix(int_party_mex)
write_xlsx(as.data.frame(int_party_mex_matrix), path="Results.xlsx")

int_party_per <- list(int_party_index1_per_nc, int_party_index1_per, int_party_index2_per_nc, int_party_index2_per, int_party_index3_per_nc, int_party_index3_per, type="text")
int_party_per <- stargazer(int_party_per, type="text")
int_party_per_matrix <- as.matrix(int_party_per)
write_xlsx(as.data.frame(int_party_per_matrix), path="Results.xlsx")

#elite recode 2 (marginal effects) interactions (without bad completes)
int_elite2_mex <- list(int_elite2_index1_mex_nc, int_elite2_index1_mex, int_elite2_index2_mex_nc, int_elite2_index2_mex, int_elite2_index3_mex_nc, int_elite2_index3_mex, type="text")
int_elite2_mex <- stargazer(int_elite2_mex, type="text")
int_elite2_mex_matrix <- as.matrix(int_elite2_mex)
write_xlsx(as.data.frame(int_elite2_mex_matrix), path="Results.xlsx")

int_elite2_per <- list(int_elite2_index1_per_nc, int_elite2_index1_per, int_elite2_index2_per_nc, int_elite2_index2_per, int_elite2_index3_per_nc, int_elite2_index3_per, type="text")
int_elite2_per <- stargazer(int_elite2_per, type="text")
int_elite2_per_matrix <- as.matrix(int_elite2_per)
write_xlsx(as.data.frame(int_elite2_per_matrix), path="Results.xlsx")



### Coding Broad Indices WITH Bad Completes ---------------------------------

# RERUN THE CLEANING CODE (INCLUDING THE NON-TAKERS CODE) AND THEN RUN THE BROAD INDICES CODE #

#Coding the indices WITH  bad completes: RECODE THESE IF YOU HAVE RUN THE CODE FOR WITHOUT BAD COMPLETES

#Recoded questions indices, Reverse Index (with bad completes)
m_dem$reverse_index <- rowMeans(m_dem[, c("frexp2_recode", "frassc1_recode", "frassc3_recode", "unisuff1_recode", "decelec1_recode", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

p_dem$reverse_index <- rowMeans(p_dem[, c("frexp2_recode", "frassc1_recode", "frassc3_recode", "unisuff1_recode", "decelec1_recode", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

#Not recoded questions indices, Forward Index (with bad completes)
m_dem$forward_index <- rowMeans(m_dem[, c("frexp1", "frassc2", "unisuff2", "decelec2", "frelect1", "judcnstr1", "legcnstr2", "eqlaw2")], na.rm = TRUE)

p_dem$forward_index <- rowMeans(p_dem[, c("frexp1", "frassc2", "unisuff2", "decelec2", "frelect1", "judcnstr1", "legcnstr2", "eqlaw2")], na.rm = TRUE)

#Claassen 7 question indices, Claassen Index (with bad completes)
m_dem$claassen_index <- rowMeans(m_dem[, c("frexp2_recode", "frassc1_recode", "unisuff2", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

p_dem$claassen_index <- rowMeans(p_dem[, c("frexp2_recode", "frassc1_recode", "unisuff2", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)




### Broad Regressions and Interactions WITH Bad Completes -------------------

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



### Exporting the Broad Results WITH Bad Completes --------------------------

#Export the regressions and interactions
#regressions (with bad completes)
regressions_mex <- list(reg_index1_mex_nc, reg_index1_mex, reg_index2_mex_nc, reg_index2_mex, reg_index3_mex_nc, reg_index3_mex, type = "text")
regressions_mex <- stargazer(regressions_mex, type = "text")
regressions_mex_matrix <- as.matrix(regressions_mex)
write_xlsx(as.data.frame(regressions_mex_matrix), path = "Results.xlsx")

regressions_per <- list(reg_index1_per_nc, reg_index1_per, reg_index2_per_nc, reg_index2_per, reg_index3_per_nc, reg_index3_per, type="text")
regressions_per <- stargazer(regressions_per, type="text")
regressions_per_matrix <- as.matrix(regressions_per)
write_xlsx(as.data.frame(regressions_per_matrix), path="Results.xlsx")

#pres interactions (with bad completes)
int_pres_mex <- list(int_pres_index1_mex_nc, int_pres_index1_mex, int_pres_index2_mex_nc, int_pres_index2_mex, int_pres_index3_mex_nc, int_pres_index3_mex, type="text")
int_pres_mex <- stargazer(int_pres_mex, type="text")
int_pres_mex_matrix <- as.matrix(int_pres_mex)
write_xlsx(as.data.frame(int_pres_mex_matrix), path="Results.xlsx")

int_pres_per <- list(int_pres_index1_per_nc, int_pres_index1_per, int_pres_index2_per_nc, int_pres_index2_per, int_pres_index3_per_nc, int_pres_index3_per, type="text")
int_pres_per <- stargazer(int_pres_per, type="text")
int_pres_per_matrix <- as.matrix(int_pres_per)
write_xlsx(as.data.frame(int_pres_per_matrix), path="Results.xlsx")

#persecon interactions (with bad completes)
int_persecon_mex <- list(int_persecon_index1_mex_nc, int_persecon_index1_mex, int_persecon_index2_mex_nc, int_persecon_index2_mex, int_persecon_index3_mex_nc, int_persecon_index3_mex, type="text")
int_persecon_mex <- stargazer(int_persecon_mex, type="text")
int_persecon_mex_matrix <- as.matrix(int_persecon_mex)
write_xlsx(as.data.frame(int_persecon_mex_matrix), path="Results.xlsx")

int_persecon_per <- list(int_persecon_index1_per_nc, int_persecon_index1_per, int_persecon_index2_per_nc, int_persecon_index2_per, int_persecon_index3_per_nc, int_persecon_index3_per, type="text")
int_persecon_per <- stargazer(int_persecon_per, type="text")
int_persecon_per_matrix <- as.matrix(int_persecon_per)
write_xlsx(as.data.frame(int_persecon_per_matrix), path="Results.xlsx")

#persvio interactions (with bad completes)
int_persvio_mex <- list(int_persvio_index1_mex_nc, int_persvio_index1_mex, int_persvio_index2_mex_nc, int_persvio_index2_mex, int_persvio_index3_mex_nc, int_persvio_index3_mex, type="text")
int_persvio_mex <- stargazer(int_persvio_mex, type="text")
int_persvio_mex_matrix <- as.matrix(int_persvio_mex)
write_xlsx(as.data.frame(int_persvio_mex_matrix), path="Results.xlsx")

int_persvio_per <- list(int_persvio_index1_per_nc, int_persvio_index1_per, int_persvio_index2_per_nc, int_persvio_index2_per, int_persvio_index3_per_nc, int_persvio_index3_per, type="text")
int_persvio_per <- stargazer(int_persvio_per, type="text")
int_persvio_per_matrix <- as.matrix(int_persvio_per)
write_xlsx(as.data.frame(int_persvio_per_matrix), path="Results.xlsx")

#countecon recode interactions (with bad completes)
int_countecon_mex <- list(int_countecon_index1_mex_nc, int_countecon_index1_mex, int_countecon_index2_mex_nc, int_countecon_index2_mex, int_countecon_index3_mex_nc, int_countecon_index3_mex, type="text")
int_countecon_mex <- stargazer(int_countecon_mex, type="text")
int_countecon_mex_matrix <- as.matrix(int_countecon_mex)
write_xlsx(as.data.frame(int_countecon_mex_matrix), path="Results.xlsx")

int_countecon_per <- list(int_countecon_index1_per_nc, int_countecon_index1_per, int_countecon_index2_per_nc, int_countecon_index2_per, int_countecon_index3_per_nc, int_countecon_index3_per, type="text")
int_countecon_per <- stargazer(int_countecon_per, type="text")
int_countecon_per_matrix <- as.matrix(int_countecon_per)
write_xlsx(as.data.frame(int_countecon_per_matrix), path="Results.xlsx")

#countvio recode interactions (with bad completes)
int_countvio_mex <- list(int_countvio_index1_mex_nc, int_countvio_index1_mex, int_countvio_index2_mex_nc, int_countvio_index2_mex, int_countvio_index3_mex_nc, int_countvio_index3_mex, type="text")
int_countvio_mex <- stargazer(int_countvio_mex, type="text")
int_countvio_mex_matrix <- as.matrix(int_countvio_mex)
write_xlsx(as.data.frame(int_countvio_mex_matrix), path="Results.xlsx")

int_countvio_per <- list(int_countvio_index1_per_nc, int_countvio_index1_per, int_countvio_index2_per_nc, int_countvio_index2_per, int_countvio_index3_per_nc, int_countvio_index3_per, type="text")
int_countvio_per <- stargazer(int_countvio_per, type="text")
int_countvio_per_matrix <- as.matrix(int_countvio_per)
write_xlsx(as.data.frame(int_countvio_per_matrix), path="Results.xlsx")

#politpart interactions (with bad completes)
int_politpart_mex <- list(int_polit_index1_mex_nc, int_polit_index1_mex, int_polit_index2_mex_nc, int_polit_index2_mex, int_polit_index3_mex_nc, int_polit_index3_mex, type="text")
int_politpart_mex <- stargazer(int_politpart_mex, type="text")
int_politpart_mex_matrix <- as.matrix(int_politpart_mex)
write_xlsx(as.data.frame(int_politpart_mex_matrix), path="Results.xlsx")

int_politpart_per <- list(int_polit_index1_per_nc, int_polit_index1_per, int_polit_index2_per_nc, int_polit_index2_per, int_polit_index3_per_nc, int_polit_index3_per, type="text")
int_politpart_per <- stargazer(int_politpart_per, type="text")
int_politpart_per_matrix <- as.matrix(int_politpart_per)
write_xlsx(as.data.frame(int_politpart_per_matrix), path="Results.xlsx")

#elite recode interactions (with bad completes)
int_elite_mex <- list(int_elite_index1_mex_nc, int_elite_index1_mex, int_elite_index2_mex_nc, int_elite_index2_mex, int_elite_index3_mex_nc, int_elite_index3_mex, type="text")
int_elite_mex <- stargazer(int_elite_mex, type="text")
int_elite_mex_matrix <- as.matrix(int_elite_mex)
write_xlsx(as.data.frame(int_elite_mex_matrix), path="Results.xlsx")

int_elite_per <- list(int_elite_index1_per_nc, int_elite_index1_per, int_elite_index2_per_nc, int_elite_index2_per, int_elite_index3_per_nc, int_elite_index3_per, type="text")
int_elite_per <- stargazer(int_elite_per, type="text")
int_elite_per_matrix <- as.matrix(int_elite_per)
write_xlsx(as.data.frame(int_elite_per_matrix), path="Results.xlsx")

#party recode 1 interactions (with bad completes)
int_party_mex <- list(int_party_index1_mex_nc, int_party_index1_mex, int_party_index2_mex_nc, int_party_index2_mex, int_party_index3_mex_nc, int_party_index3_mex, type="text")
int_party_mex <- stargazer(int_party_mex, type="text")
int_party_mex_matrix <- as.matrix(int_party_mex)
write_xlsx(as.data.frame(int_party_mex_matrix), path="Results.xlsx")

int_party_per <- list(int_party_index1_per_nc, int_party_index1_per, int_party_index2_per_nc, int_party_index2_per, int_party_index3_per_nc, int_party_index3_per, type="text")
int_party_per <- stargazer(int_party_per, type="text")
int_party_per_matrix <- as.matrix(int_party_per)
write_xlsx(as.data.frame(int_party_per_matrix), path="Results.xlsx")

#elite recode 2 (marginal effects) interactions (with bad completes)
int_elite2_mex <- list(int_elite2_index1_mex_nc, int_elite2_index1_mex, int_elite2_index2_mex_nc, int_elite2_index2_mex, int_elite2_index3_mex_nc, int_elite2_index3_mex, type="text")
int_elite2_mex <- stargazer(int_elite2_mex, type="text")
int_elite2_mex_matrix <- as.matrix(int_elite2_mex)
write_xlsx(as.data.frame(int_elite2_mex_matrix), path="Results.xlsx")

int_elite2_per <- list(int_elite2_index1_per_nc, int_elite2_index1_per, int_elite2_index2_per_nc, int_elite2_index2_per, int_elite2_index3_per_nc, int_elite2_index3_per, type="text")
int_elite2_per <- stargazer(int_elite2_per, type="text")
int_elite2_per_matrix <- as.matrix(int_elite2_per)
write_xlsx(as.data.frame(int_elite2_per_matrix), path="Results.xlsx")



### Dropping Bad Completes --------------------------------------------------

###CODING TO DROP THE BAD COMPLETES AND RERUN THE INDICES
#1.   They offer a gibberish answer to the open-ended response question. This is defined as words that make no sense.
#2.   Those who spend less than 4 minutes on the survey, if control, and less than 5 minutes, if treatment 
#3.   Those who put the same answer on all 17 democracy outcome questions, or all the same answer on 16 of the 17 questions. 
#4.   Those who choose one of the two “no answer” options in the democracy questions at least 75 percent of the time. 

#Mexico
m_dem$gib <- as.numeric(m_dem$gib)
m_dem$nonresp <- as.numeric(m_dem$nonresp)
m_dem$outincons <- as.numeric(m_dem$outincons)

m_dem$bad=0
m_dem$bad[m_dem$treat == "Control" & m_dem$`Duration (in seconds)` < 240]=1
m_dem$bad[m_dem$treat == "Econ Treat" & m_dem$`Duration (in seconds)` < 300]=1
m_dem$bad[m_dem$treat == "Vio Treat" & m_dem$`Duration (in seconds)` < 300]=1
m_dem$bad[m_dem$gib == 1]=1
m_dem$bad[m_dem$nonresp == 1]=1
m_dem$bad[m_dem$outincons == 1]=1
table(m_dem$bad)
m_dem <- subset(m_dem, m_dem$bad == 0)

#Peru
p_dem$gib <- as.numeric(p_dem$gib)
p_dem$nonresp <- as.numeric(p_dem$nonresp)
p_dem$outincons <- as.numeric(p_dem$outincons)
p_dem$`Duration (in seconds)` <- as.numeric(p_dem$`Duration (in seconds)`)

p_dem$bad=0
p_dem$bad[p_dem$treat == "Control" & p_dem$`Duration (in seconds)` < 240]=1
p_dem$bad[p_dem$treat == "Econ Treat" & p_dem$`Duration (in seconds)` < 300]=1
p_dem$bad[p_dem$treat == "Vio Treat" & p_dem$`Duration (in seconds)` < 300]=1
p_dem$bad[p_dem$gib == 1]=1
p_dem$bad[p_dem$nonresp == 1]=1
p_dem$bad[p_dem$outincons == 1]=1
table(p_dem$bad)
p_dem <- subset(p_dem, p_dem$bad == 0)



### Coding Broad Indices WITHOUT Bad Completes ------------------------------

#Coding the indices WITHOUT bad completes: RECODE THESE IF YOU HAVE RUN THE CODE FOR WITH BAD COMPLETES

#Recoded questions indices, Reverse Index (with bad completes)
m_dem$reverse_index <- rowMeans(m_dem[, c("frexp2_recode", "frassc1_recode", "frassc3_recode", "unisuff1_recode", "decelec1_recode", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

p_dem$reverse_index <- rowMeans(p_dem[, c("frexp2_recode", "frassc1_recode", "frassc3_recode", "unisuff1_recode", "decelec1_recode", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

#Not recoded questions indices, Forward Index (with bad completes)
m_dem$forward_index <- rowMeans(m_dem[, c("frexp1", "frassc2", "unisuff2", "decelec2", "frelect1", "judcnstr1", "legcnstr2", "eqlaw2")], na.rm = TRUE)

p_dem$forward_index <- rowMeans(p_dem[, c("frexp1", "frassc2", "unisuff2", "decelec2", "frelect1", "judcnstr1", "legcnstr2", "eqlaw2")], na.rm = TRUE)

#Claassen 7 question indices, Claassen Index (with bad completes)
m_dem$claassen_index <- rowMeans(m_dem[, c("frexp2_recode", "frassc1_recode", "unisuff2", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)

p_dem$claassen_index <- rowMeans(p_dem[, c("frexp2_recode", "frassc1_recode", "unisuff2", "frelect2_recode", "judcnstr2_recode", "legcnstr1_recode", "eqlaw1_recode")], na.rm = TRUE)



### Broad Regressions and Interactions WITHOUT Bad Completes --------

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



### Exporting the Broad Results WITHOUT Bad Completes -----------------------

#Export the regressions and interactions
#regressions (without bad completes)
regressions_mex <- list(reg_index1_mex_nc, reg_index1_mex, reg_index2_mex_nc, reg_index2_mex, reg_index3_mex_nc, reg_index3_mex, type = "text")
regressions_mex <- stargazer(regressions_mex, type = "text")
regressions_mex_matrix <- as.matrix(regressions_mex)
write_xlsx(as.data.frame(regressions_mex_matrix), path = "Results.xlsx")

regressions_per <- list(reg_index1_per_nc, reg_index1_per, reg_index2_per_nc, reg_index2_per, reg_index3_per_nc, reg_index3_per, type="text")
regressions_per <- stargazer(regressions_per, type="text")
regressions_per_matrix <- as.matrix(regressions_per)
write_xlsx(as.data.frame(regressions_per_matrix), path="Results.xlsx")

#pres interactions (without bad completes)
int_pres_mex <- list(int_pres_index1_mex_nc, int_pres_index1_mex, int_pres_index2_mex_nc, int_pres_index2_mex, int_pres_index3_mex_nc, int_pres_index3_mex, type="text")
int_pres_mex <- stargazer(int_pres_mex, type="text")
int_pres_mex_matrix <- as.matrix(int_pres_mex)
write_xlsx(as.data.frame(int_pres_mex_matrix), path="Results.xlsx")

int_pres_per <- list(int_pres_index1_per_nc, int_pres_index1_per, int_pres_index2_per_nc, int_pres_index2_per, int_pres_index3_per_nc, int_pres_index3_per, type="text")
int_pres_per <- stargazer(int_pres_per, type="text")
int_pres_per_matrix <- as.matrix(int_pres_per)
write_xlsx(as.data.frame(int_pres_per_matrix), path="Results.xlsx")

#persecon interactions (without bad completes)
int_persecon_mex <- list(int_persecon_index1_mex_nc, int_persecon_index1_mex, int_persecon_index2_mex_nc, int_persecon_index2_mex, int_persecon_index3_mex_nc, int_persecon_index3_mex, type="text")
int_persecon_mex <- stargazer(int_persecon_mex, type="text")
int_persecon_mex_matrix <- as.matrix(int_persecon_mex)
write_xlsx(as.data.frame(int_persecon_mex_matrix), path="Results.xlsx")

int_persecon_per <- list(int_persecon_index1_per_nc, int_persecon_index1_per, int_persecon_index2_per_nc, int_persecon_index2_per, int_persecon_index3_per_nc, int_persecon_index3_per, type="text")
int_persecon_per <- stargazer(int_persecon_per, type="text")
int_persecon_per_matrix <- as.matrix(int_persecon_per)
write_xlsx(as.data.frame(int_persecon_per_matrix), path="Results.xlsx")

#persvio interactions (without bad completes)
int_persvio_mex <- list(int_persvio_index1_mex_nc, int_persvio_index1_mex, int_persvio_index2_mex_nc, int_persvio_index2_mex, int_persvio_index3_mex_nc, int_persvio_index3_mex, type="text")
int_persvio_mex <- stargazer(int_persvio_mex, type="text")
int_persvio_mex_matrix <- as.matrix(int_persvio_mex)
write_xlsx(as.data.frame(int_persvio_mex_matrix), path="Results.xlsx")

int_persvio_per <- list(int_persvio_index1_per_nc, int_persvio_index1_per, int_persvio_index2_per_nc, int_persvio_index2_per, int_persvio_index3_per_nc, int_persvio_index3_per, type="text")
int_persvio_per <- stargazer(int_persvio_per, type="text")
int_persvio_per_matrix <- as.matrix(int_persvio_per)
write_xlsx(as.data.frame(int_persvio_per_matrix), path="Results.xlsx")

#countecon recode interactions (without bad completes)
int_countecon_mex <- list(int_countecon_index1_mex_nc, int_countecon_index1_mex, int_countecon_index2_mex_nc, int_countecon_index2_mex, int_countecon_index3_mex_nc, int_countecon_index3_mex, type="text")
int_countecon_mex <- stargazer(int_countecon_mex, type="text")
int_countecon_mex_matrix <- as.matrix(int_countecon_mex)
write_xlsx(as.data.frame(int_countecon_mex_matrix), path="Results.xlsx")

int_countecon_per <- list(int_countecon_index1_per_nc, int_countecon_index1_per, int_countecon_index2_per_nc, int_countecon_index2_per, int_countecon_index3_per_nc, int_countecon_index3_per, type="text")
int_countecon_per <- stargazer(int_countecon_per, type="text")
int_countecon_per_matrix <- as.matrix(int_countecon_per)
write_xlsx(as.data.frame(int_countecon_per_matrix), path="Results.xlsx")

#countvio recode interactions (without bad completes)
int_countvio_mex <- list(int_countvio_index1_mex_nc, int_countvio_index1_mex, int_countvio_index2_mex_nc, int_countvio_index2_mex, int_countvio_index3_mex_nc, int_countvio_index3_mex, type="text")
int_countvio_mex <- stargazer(int_countvio_mex, type="text")
int_countvio_mex_matrix <- as.matrix(int_countvio_mex)
write_xlsx(as.data.frame(int_countvio_mex_matrix), path="Results.xlsx")

int_countvio_per <- list(int_countvio_index1_per_nc, int_countvio_index1_per, int_countvio_index2_per_nc, int_countvio_index2_per, int_countvio_index3_per_nc, int_countvio_index3_per, type="text")
int_countvio_per <- stargazer(int_countvio_per, type="text")
int_countvio_per_matrix <- as.matrix(int_countvio_per)
write_xlsx(as.data.frame(int_countvio_per_matrix), path="Results.xlsx")

#politpart interactions (without bad completes)
int_politpart_mex <- list(int_polit_index1_mex_nc, int_polit_index1_mex, int_polit_index2_mex_nc, int_polit_index2_mex, int_polit_index3_mex_nc, int_polit_index3_mex, type="text")
int_politpart_mex <- stargazer(int_politpart_mex, type="text")
int_politpart_mex_matrix <- as.matrix(int_politpart_mex)
write_xlsx(as.data.frame(int_politpart_mex_matrix), path="Results.xlsx")

int_politpart_per <- list(int_polit_index1_per_nc, int_polit_index1_per, int_polit_index2_per_nc, int_polit_index2_per, int_polit_index3_per_nc, int_polit_index3_per, type="text")
int_politpart_per <- stargazer(int_politpart_per, type="text")
int_politpart_per_matrix <- as.matrix(int_politpart_per)
write_xlsx(as.data.frame(int_politpart_per_matrix), path="Results.xlsx")

#elite recode interactions (without bad completes)
int_elite_mex <- list(int_elite_index1_mex_nc, int_elite_index1_mex, int_elite_index2_mex_nc, int_elite_index2_mex, int_elite_index3_mex_nc, int_elite_index3_mex, type="text")
int_elite_mex <- stargazer(int_elite_mex, type="text")
int_elite_mex_matrix <- as.matrix(int_elite_mex)
write_xlsx(as.data.frame(int_elite_mex_matrix), path="Results.xlsx")

int_elite_per <- list(int_elite_index1_per_nc, int_elite_index1_per, int_elite_index2_per_nc, int_elite_index2_per, int_elite_index3_per_nc, int_elite_index3_per, type="text")
int_elite_per <- stargazer(int_elite_per, type="text")
int_elite_per_matrix <- as.matrix(int_elite_per)
write_xlsx(as.data.frame(int_elite_per_matrix), path="Results.xlsx")

#party recode 1 interactions (without bad completes)
int_party_mex <- list(int_party_index1_mex_nc, int_party_index1_mex, int_party_index2_mex_nc, int_party_index2_mex, int_party_index3_mex_nc, int_party_index3_mex, type="text")
int_party_mex <- stargazer(int_party_mex, type="text")
int_party_mex_matrix <- as.matrix(int_party_mex)
write_xlsx(as.data.frame(int_party_mex_matrix), path="Results.xlsx")

int_party_per <- list(int_party_index1_per_nc, int_party_index1_per, int_party_index2_per_nc, int_party_index2_per, int_party_index3_per_nc, int_party_index3_per, type="text")
int_party_per <- stargazer(int_party_per, type="text")
int_party_per_matrix <- as.matrix(int_party_per)
write_xlsx(as.data.frame(int_party_per_matrix), path="Results.xlsx")

#elite recode 2 (marginal effects) interactions (without bad completes)
int_elite2_mex <- list(int_elite2_index1_mex_nc, int_elite2_index1_mex, int_elite2_index2_mex_nc, int_elite2_index2_mex, int_elite2_index3_mex_nc, int_elite2_index3_mex, type="text")
int_elite2_mex <- stargazer(int_elite2_mex, type="text")
int_elite2_mex_matrix <- as.matrix(int_elite2_mex)
write_xlsx(as.data.frame(int_elite2_mex_matrix), path="Results.xlsx")

int_elite2_per <- list(int_elite2_index1_per_nc, int_elite2_index1_per, int_elite2_index2_per_nc, int_elite2_index2_per, int_elite2_index3_per_nc, int_elite2_index3_per, type="text")
int_elite2_per <- stargazer(int_elite2_per, type="text")
int_elite2_per_matrix <- as.matrix(int_elite2_per)
write_xlsx(as.data.frame(int_elite2_per_matrix), path="Results.xlsx")


