#Initial Peru Democracy Cleaning and Analysis
#Last Updated: August 10, 2023

###Run Packages and Import Dataset ---------------------------------------------------
library(dplyr)
library(car)
library(readxl)

p_dem <- read_excel("PE_Data_Excluding_Bad_Completes 7.26.23.xlsx")


###Cleaning Code -----------------------------------------------------------
#Drop first row
p_dem <- subset(p_dem, Status == "IP Address")


#Create treatment variable
p_dem$treat <- "Control"
p_dem$treat[p_dem$econ != ""] <- "Econ Treat"
p_dem$treat[p_dem$vio != ""] <- "Vio Treat"
p_dem$treat <- as.factor(p_dem$treat)
p_dem$treat <- relevel(p_dem$treat, ref = "Control")
table(p_dem$treat)

#Cleaning up the control variables

##Age
summary(p_dem$age)
p_dem$age <- as.numeric(p_dem$age)
table(p_dem$age)
hist(p_dem$age)

##Gender
summary(p_dem$gender)
p_dem$gender_n <- 0
p_dem$gender_n[p_dem$gender == "Mujer/femenino"] <- 2
p_dem$gender_n[p_dem$gender == "Hombre/masculino"] <- 1
table(p_dem$gender_n)

##State
p_dem$state <- car::recode(p_dem$state, '
                           "1" = "Amazonas";
                           "2" = "Ancash";
                           "3" = "Apurímac";
                           "4" = "Arequipa";
                           "5" = "Ayacucho";
                           "6" = "Cajamarca";
                           "7" = "Callao";
                           "8" = "Cusco";
                           "9" = "Huancavelica";
                           "10" = "Huánuco";
                           "11" = "Ica";
                           "12" = "Junín";
                           "13" = "La Libertad";
                           "14" = "Lambayeque";
                           "15" = "Lima";
                           "16" = "Loreto";
                           "17" = "Madre de Dios";
                           "18" = "Moquegua";
                           "19" = "Pasco";
                           "20" = "Piura";
                           "21" = "Puno";
                           "22" = "San Martín";
                           "23" = "Tacna";
                           "24" = "Tumbes";
                           "25" = "Ucacyali";
                           "26" = "Prefiero no responder"
                           ', as.factor = TRUE)
table(p_dem$state)

##Living
p_dem$living <- car::recode(p_dem$living, '
                            "1" = "Barrios fuera del area/los suburbios";
                            "2" = "Ciudad";
                            "3" = "En un pueblo cerca de una área rural/zona rural";
                            "4" = "En una área rural/zona rural";
                            "5" = "No sé";
                            "6" = "Prefiero no responder"
                            ', as.factor = TRUE)
table(p_dem$living)

##Education
p_dem$edu_n <- NA
p_dem$edu_n[p_dem$edu == "Ningun"] <- 1
p_dem$edu_n[p_dem$edu == "Primaria (incompleta o completa)"] <- 2
p_dem$edu_n[p_dem$edu == "Secundaria (incompleta o completa)"] <- 3
p_dem$edu_n[p_dem$edu == "Superior, técnica o universitaria (incompleta o completa)"] <- 4

table(p_dem$edu_n)

##Income
p_dem$income_n <- NA
p_dem$income_n[p_dem$income == "Entre 0 y 350 soles"] <- 1
p_dem$income_n[p_dem$income == "Entre 351 y 840 soles"] <- 2
p_dem$income_n[p_dem$income == "Entre 841 y 1200 soles"] <- 3
p_dem$income_n[p_dem$income == "Entre 1201 y 1850 soles"] <- 4
p_dem$income_n[p_dem$income == "Más de 1850 soles"] <- 5

table(p_dem$income_n)

##Ideo_1
class(p_dem$ideo_1)
p_dem$ideo_1 <- as.numeric(p_dem$ideo_1)
table(p_dem$ideo_1)

##Party
p_dem$party <- car::recode(p_dem$party, '
                           "1" = "Acción Popular";
                           "2" = "Alianza Para El Progreso";
                           "3" = "Avanza País";
                           "4" = "Fuerza Popular";
                           "5" = "Juntos Por El Perú";
                           "6" = "Perú Libre";
                           "7" = "Renovación Popular";
                           "8" = "No sé";
                           "9" = "Prefiero no responder";
                           "10" = "Otro (¿Cuál?)"
                           ', as.factor = TRUE)
table(p_dem$party)


###Cleaning outcome variables -------------------------------------------------------
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


##Flip the outcome variables that are reverse coded
###"FREXP2", "FRASSC1", "FRASSC3", "UNISUFF1", "DECELEC1", "FRELECT2", "JUDCNSTR2", "LEGCNSTR1", "EQLAW1")
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
p_dem$pres <- car::recode(p_dem$pres, '
                          "Muy bien" = "5";
                          "Bien" = "4";
                          "Ni bien ni mal" = "3";
                          "Mal" = "2";
                          "Muy mal" = "1";
                          "No lo sé" = "NA";
                          "Prefiero no responder" = "NA"')
p_dem$pers_vio <- car::recode(p_dem$pers_vio, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
p_dem$count_vio <- car::recode(p_dem$count_vio, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
p_dem$pers_econ <- car::recode(p_dem$pers_econ, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
p_dem$count_econ <- car::recode(p_dem$count_econ, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
p_dem$politpart <- car::recode(p_dem$politpart, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')
p_dem$elite <- car::recode(p_dem$elite, '
                              "Muy de acuerdo" = "5";
                              "Algo de acuerdo" = "4";
                              "Ni de acuerdo ni en desacuerdo" = "3";
                              "Algo en desacuerdo" = "2";
                              "Muy en desacuerdo" = "1";
                              "No lo sé" = "NA";
                              "Prefiero no responder" = "NA"')

##Recoding moderator variables
p_dem$count_econ_recode <- car::recode(p_dem$count_econ, '
                                     "5" = "1";
                                     "4" = "2";
                                     "3" = "3";
                                     "2" = "4";
                                     "1" = "5"')
p_dem$count_vio_recode <- car::recode(p_dem$count_vio, '
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

###Initial analysis -----------------------------------------------------------------
p_dem$index <- (p_dem$frexp1 + p_dem$frexp2_recode + p_dem$frassc1_recode + p_dem$frassc2 + p_dem$frassc3_recode + p_dem$unisuff1_recode + p_dem$unisuff2 + p_dem$decelec1_recode + p_dem$decelec2 + p_dem$frelect1 + p_dem$frelect2_recode + p_dem$judcnstr1 + p_dem$judcnstr2_recode + p_dem$legcnstr1_recode + p_dem$legcnstr2 + p_dem$eqlaw1_recode + p_dem$eqlaw2)/17


summary(index.p.lm  <- lm(index ~ treat, data = p_dem))
summary(index_c.p.lm <- lm(index ~ treat + age + gender + state + living + edu_n + income_n + ideo_1+ party, data = p_dem))

summary(dem1<-lm(frexp1 ~ treat, data=p_dem))
summary(dem1<-lm(frexp2_recode ~ treat, data=p_dem))
summary(dem1<-lm(frassc1_recode ~ treat, data=p_dem))
summary(dem1<-lm(frassc2 ~ treat, data=p_dem))
summary(dem1<-lm(frassc3_recode ~ treat, data=p_dem))
summary(dem1<-lm(unisuff1_recode ~ treat, data=p_dem))
summary(dem1<-lm(unisuff2 ~ treat, data=p_dem))
summary(dem1<-lm(decelec1_recode ~ treat, data=p_dem))
summary(dem1<-lm(decelec2 ~ treat, data=p_dem))
summary(dem1<-lm(frelect1 ~ treat, data=p_dem))
summary(dem1<-lm(frelect2_recode ~ treat, data=p_dem))
summary(dem1<-lm(judcnstr1 ~ treat, data=p_dem))
summary(dem1<-lm(judcnstr2_recode ~ treat, data=p_dem))
summary(dem1<-lm(legcnstr1_recode ~ treat, data=p_dem))
summary(dem1<-lm(legcnstr2 ~ treat, data=p_dem))
summary(dem1<-lm(eqlaw1_recode ~ treat, data=p_dem))
summary(dem1<-lm(eqlaw2 ~ treat, data=p_dem))