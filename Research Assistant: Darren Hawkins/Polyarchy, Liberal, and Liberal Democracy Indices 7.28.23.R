###V-Dem Indices
###July 28, 2023
###Last Updated: July 28, 2023


#Import Datasets Excluding Bad Completes ---------------------------------
library(readxl)
library(stargazer)
library(writexl)
m_dem <- read_excel("MX_Data_Excluding_Bad_Completes 7.26.23.xlsx")
p_dem <- read_excel("PE_Data_Excluding_Bad_Completes 7.26.23.xlsx")


#Re-scale Variables -------------------------------------------------------
##Turn this into a scale from 0 to 1
#Mexico
m_dem$frexp1_n <- car::recode(m_dem$frexp1, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$frexp2_recode_n <- car::recode(m_dem$frexp2_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$frassc1_recode_n <- car::recode(m_dem$frassc1_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$frassc2_n <- car::recode(m_dem$frassc2, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$frassc3_recode_n <- car::recode(m_dem$frassc3_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$unisuff1_recode_n <- car::recode(m_dem$unisuff1_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$unisuff2_n <- car::recode(m_dem$unisuff2, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$decelec1_recode_n <- car::recode(m_dem$decelec1_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$decelec2_n <- car::recode(m_dem$decelec2, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$frelect1_n <- car::recode(m_dem$frelect1, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$frelect2_recode_n <- car::recode(m_dem$frelect2_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$legcnstr1_recode_n <- car::recode(m_dem$legcnstr1_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$legcnstr2_n <- car::recode(m_dem$legcnstr2, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$judcnstr1_n <- car::recode(m_dem$judcnstr1, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$judcnstr2_recode_n <- car::recode(m_dem$judcnstr2_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$eqlaw1_recode_n <- car::recode(m_dem$eqlaw1_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
m_dem$eqlaw2_n <- car::recode(m_dem$eqlaw2, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
#Peru
p_dem$frexp1_n <- car::recode(p_dem$frexp1, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$frexp2_recode_n <- car::recode(p_dem$frexp2_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$frassc1_recode_n <- car::recode(p_dem$frassc1_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$frassc2_n <- car::recode(p_dem$frassc2, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$frassc3_recode_n <- car::recode(p_dem$frassc3_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$unisuff1_recode_n <- car::recode(p_dem$unisuff1_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$unisuff2_n <- car::recode(p_dem$unisuff2, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$decelec1_recode_n <- car::recode(p_dem$decelec1_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$decelec2_n <- car::recode(p_dem$decelec2, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$frelect1_n <- car::recode(p_dem$frelect1, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$frelect2_recode_n <- car::recode(p_dem$frelect2_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$legcnstr1_recode_n <- car::recode(p_dem$legcnstr1_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$legcnstr2_n <- car::recode(p_dem$legcnstr2, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$judcnstr1_n <- car::recode(p_dem$judcnstr1, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$judcnstr2_recode_n <- car::recode(p_dem$judcnstr2_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$eqlaw1_recode_n <- car::recode(p_dem$eqlaw1_recode, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')
p_dem$eqlaw2_n <- car::recode(p_dem$eqlaw2, '
                              "1"="0";
                              "2"="0.25";
                              "3"="0.5";
                              "4"="0.75";
                              "5"="1"')

#Mexico
m_dem$pres_n <- car::recode(m_dem$pres, '
                            "1"="0";
                            "2"="0.25";
                            "3"="0.5";
                            "4"="0.75";
                            "5"="1"')
m_dem$pers_vio_n <- car::recode(m_dem$pers_vio, '
                                "1"="0";
                                "2"="0.25";
                                "3"="0.5";
                                "4"="0.75";
                                "5"="1"')
m_dem$pers_econ_n <- car::recode(m_dem$pers_econ, '
                                "1"="0";
                                "2"="0.25";
                                "3"="0.5";
                                "4"="0.75";
                                "5"="1"')
m_dem$count_vio_recode_n <- car::recode(m_dem$count_vio_recode, '
                                "1"="0";
                                "2"="0.25";
                                "3"="0.5";
                                "4"="0.75";
                                "5"="1"')
m_dem$count_econ_recode_n <- car::recode(m_dem$count_econ_recode, '
                                "1"="0";
                                "2"="0.25";
                                "3"="0.5";
                                "4"="0.75";
                                "5"="1"')
m_dem$politpart_n <- car::recode(m_dem$politpart, '
                                "1"="0";
                                "2"="0.25";
                                "3"="0.5";
                                "4"="0.75";
                                "5"="1"')
m_dem$elite_recode_n <- car::recode(m_dem$elite_recode, '
                                "1"="0";
                                "2"="0.25";
                                "3"="0.5";
                                "4"="0.75";
                                "5"="1"')
#Peru
p_dem$pres_n <- car::recode(p_dem$pres, '
                            "1"="0";
                            "2"="0.25";
                            "3"="0.5";
                            "4"="0.75";
                            "5"="1"')
p_dem$pers_vio_n <- car::recode(p_dem$pers_vio, '
                                "1"="0";
                                "2"="0.25";
                                "3"="0.5";
                                "4"="0.75";
                                "5"="1"')
p_dem$pers_econ_n <- car::recode(p_dem$pers_econ, '
                                "1"="0";
                                "2"="0.25";
                                "3"="0.5";
                                "4"="0.75";
                                "5"="1"')
p_dem$count_vio_recode_n <- car::recode(p_dem$count_vio_recode, '
                                "1"="0";
                                "2"="0.25";
                                "3"="0.5";
                                "4"="0.75";
                                "5"="1"')
p_dem$count_econ_recode_n <- car::recode(p_dem$count_econ_recode, '
                                "1"="0";
                                "2"="0.25";
                                "3"="0.5";
                                "4"="0.75";
                                "5"="1"')
p_dem$politpart_n <- car::recode(p_dem$politpart, '
                                "1"="0";
                                "2"="0.25";
                                "3"="0.5";
                                "4"="0.75";
                                "5"="1"')
p_dem$elite_recode_n <- car::recode(p_dem$elite_recode, '
                                "1"="0";
                                "2"="0.25";
                                "3"="0.5";
                                "4"="0.75";
                                "5"="1"')

#Polyarchy Index ---------------------------------------------------------
#polyarchy_index = .5(electedofficials*clean elections*freeassociation*suffrage*alternativeinfo) + .5 (1/8*electedofficials + ¼*cleanelections + ¼*freeassociation + 1/8*suffrage  + ¼*altinfo)

#Mexico
m_dem$polyarchy_index = 0.5 * (
  (rowMeans(m_dem[, c("decelec1_recode_n", "decelec2_n")], na.rm = TRUE)) *
    (rowMeans(m_dem[, c("frelect1_n", "frelect2_recode_n")], na.rm = TRUE)) *
    (rowMeans(m_dem[, c("frassc1_recode_n", "frassc2_n", "frassc3_recode_n")], na.rm = TRUE)) *
    (rowMeans(m_dem[, c("unisuff1_recode_n", "unisuff2_n")], na.rm = TRUE)) *
    (rowMeans(m_dem[, c("frexp1_n", "frexp2_recode_n")], na.rm = TRUE))
) +
  0.5 * (
    (1/8) * (rowMeans(m_dem[, c("decelec1_recode_n", "decelec2_n")], na.rm = TRUE)) +
      (1/4) * (rowMeans(m_dem[, c("frelect1_n", "frelect2_recode_n")], na.rm = TRUE)) +
      (1/4) * (rowMeans(m_dem[, c("frassc1_recode_n", "frassc2_n", "frassc3_recode_n")], na.rm = TRUE)) +
      (1/8) * (rowMeans(m_dem[, c("unisuff1_recode_n", "unisuff2_n")], na.rm = TRUE)) +
      (1/4) * (rowMeans(m_dem[, c("frexp1_n", "frexp2_recode_n")], na.rm = TRUE))
  )

#Peru
p_dem$polyarchy_index = 0.5 * (
  (rowMeans(p_dem[, c("decelec1_recode_n", "decelec2_n")], na.rm = TRUE)) *
    (rowMeans(p_dem[, c("frelect1_n", "frelect2_recode_n")], na.rm = TRUE)) *
    (rowMeans(p_dem[, c("frassc1_recode_n", "frassc2_n", "frassc3_recode_n")], na.rm = TRUE)) *
    (rowMeans(p_dem[, c("unisuff1_recode_n", "unisuff2_n")], na.rm = TRUE)) *
    (rowMeans(p_dem[, c("frexp1_n", "frexp2_recode_n")], na.rm = TRUE))
) +
  0.5 * (
    (1/8) * (rowMeans(p_dem[, c("decelec1_recode_n", "decelec2_n")], na.rm = TRUE)) +
      (1/4) * (rowMeans(p_dem[, c("frelect1_n", "frelect2_recode_n")], na.rm = TRUE)) +
      (1/4) * (rowMeans(p_dem[, c("frassc1_recode_n", "frassc2_n", "frassc3_recode_n")], na.rm = TRUE)) +
      (1/8) * (rowMeans(p_dem[, c("unisuff1_recode_n", "unisuff2_n")], na.rm = TRUE)) +
      (1/4) * (rowMeans(p_dem[, c("frexp1_n", "frexp2_recode_n")], na.rm = TRUE))
  )


#Liberal Index -----------------------------------------------------------
#liberal_index: rule of law + judicial constraints + legislative constraints average

#Mexico
m_dem$liberal_index = rowMeans(m_dem[, c("legcnstr1_recode_n", "legcnstr2_n", "judcnstr1_n", "judcnstr2_recode_n", "eqlaw1_recode_n", "eqlaw2_n")], na.rm = TRUE)

#Peru
p_dem$liberal_index = rowMeans(p_dem[, c("legcnstr1_recode_n", "legcnstr2_n", "judcnstr1_n", "judcnstr2_recode_n", "eqlaw1_recode_n", "eqlaw2_n")], na.rm = TRUE)


#Liberal Democracy Index -------------------------------------------------
#libdem_index= .25*polyarchy^1.585 + .25*liberal + .5*polyarchy^1.585 * liberal

#Mexico
m_dem$libdem_index = 0.25 * (m_dem$polyarchy_index ^ 1.585) +
  0.25 * (m_dem$liberal_index) +
  0.5 * (m_dem$polyarchy_index ^ 1.585) * (m_dem$liberal_index)

p_dem$libdem_index = 0.25 * (p_dem$polyarchy_index ^ 1.585) +
  0.25 * (p_dem$liberal_index) +
  0.5 * (p_dem$polyarchy_index ^ 1.585) * (p_dem$liberal_index)



#Regressions and Interactions --------------------------------------------
#Mexico- polyarchy_index
summary(reg_poly_mex_nc <- lm(polyarchy_index~treat, data=m_dem))
summary(reg_poly_mex <- lm(polyarchy_index ~ treat + age + gender_n + income_n+ ideo_1, data= m_dem))

#Peru- polyarchy_index
summary(reg_poly_per_nc <- lm(polyarchy_index~treat, data=p_dem))
summary(reg_poly_per <- lm(polyarchy_index ~ treat + age + gender_n + income_n + ideo_1, data = p_dem))

#Mexico- libdem_index
summary(reg_libdem_mex_nc <- lm(libdem_index ~ treat, data = m_dem))
summary(reg_libdem_mex <- lm(libdem_index ~ treat + age + gender_n + income_n + ideo_1, data = m_dem))

#Peru- libdem_index
summary(reg_libdem_per_nc <- lm(libdem_index ~ treat, data = p_dem))
summary(reg_libdem_per <- lm(libdem_index ~ treat + age + gender_n + income_n + ideo_1, data = p_dem))

#Mexico- liberal_index
summary(reg_liberal_mex_nc <- lm(libdem_index ~ treat, data = m_dem))
summary(reg_liberal_mex <- lm(liberal_index ~ treat + age + gender_n + income_n + ideo_1, data = m_dem))

#Peru- liberal_index
summary(reg_liberal_per_nc <- lm(liberal_index ~ treat, data = p_dem))
summary(reg_liberal_per <- lm(liberal_index ~ treat + age + gender_n + income_n + ideo_1, data = p_dem))


#Interactions with moderators; controls: age, gender_n, income_n, ideo_1
#Mexico- polyarchy_index
summary(int_pres_poly_mex_nc <- lm(polyarchy_index~treat*pres_n, data=m_dem))
summary(int_pres_poly_mex <- lm(polyarchy_index~treat*pres_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persecon_poly_mex_nc <- lm(polyarchy_index~treat*pers_econ_n, data=m_dem))
summary(int_persecon_poly_mex <- lm(polyarchy_index~treat*pers_econ_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persvio_poly_mex_nc <- lm(polyarchy_index~treat*pers_vio_n, data=m_dem))
summary(int_persvio_poly_mex <- lm(polyarchy_index~treat*pers_vio_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countecon_poly_mex_nc <- lm(polyarchy_index~treat*count_econ_recode_n, data=m_dem))
summary(int_countecon_poly_mex <- lm(polyarchy_index~treat*count_econ_recode_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countvio_poly_mex_nc <- lm(polyarchy_index~treat*count_vio_recode_n, data=m_dem))
summary(int_countvio_poly_mex <- lm(polyarchy_index~treat*count_vio_recode_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_polit_poly_mex_nc <- lm(polyarchy_index~treat*politpart_n, data=m_dem))
summary(int_polit_poly_mex <- lm(polyarchy_index~treat*politpart_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_elite_poly_mex_nc <- lm(polyarchy_index~treat*elite_recode_n, data=m_dem))
summary(int_elite_poly_mex <- lm(polyarchy_index~treat*elite_recode_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_party_poly_mex_nc <- lm(polyarchy_index~treat*party_recode_1, data=m_dem))
summary(int_party_poly_mex <- lm(polyarchy_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=m_dem))

#Peru- polyarchy_index
summary(int_pres_poly_per_nc <- lm(polyarchy_index~treat*pres_n, data=p_dem))
summary(int_pres_poly_per <- lm(polyarchy_index~treat*pres_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persecon_poly_per_nc <- lm(polyarchy_index~treat*pers_econ_n, data=p_dem))
summary(int_persecon_poly_per <- lm(polyarchy_index~treat*pers_econ_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persvio_poly_per_nc <- lm(polyarchy_index~treat*pers_vio_n, data=p_dem))
summary(int_persvio_poly_per <- lm(polyarchy_index~treat*pers_vio_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countecon_poly_per_nc <- lm(polyarchy_index~treat*count_econ_recode_n, data=p_dem))
summary(int_countecon_poly_per <- lm(polyarchy_index~treat*count_econ_recode_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countvio_poly_per_nc <- lm(polyarchy_index~treat*count_vio_recode_n, data=p_dem))
summary(int_countvio_poly_per <- lm(polyarchy_index~treat*count_vio_recode_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_polit_poly_per_nc <- lm(polyarchy_index~treat*politpart_n, data=p_dem))
summary(int_polit_poly_per <- lm(polyarchy_index~treat*politpart_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_elite_poly_per_nc <- lm(polyarchy_index~treat*elite_recode_n, data=p_dem))
summary(int_elite_poly_per <- lm(polyarchy_index~treat*elite_recode_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_party_poly_per_nc <- lm(polyarchy_index~treat*party_recode_1, data=p_dem))
summary(int_party_poly_per <- lm(polyarchy_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=p_dem))

#Mexico- libdem_index
summary(int_pres_libdem_mex_nc <- lm(libdem_index~treat*pres_n, data=m_dem))
summary(int_pres_libdem_mex <- lm(libdem_index~treat*pres_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persecon_libdem_mex_nc <- lm(libdem_index~treat*pers_econ_n, data=m_dem))
summary(int_persecon_libdem_mex <- lm(libdem_index~treat*pers_econ_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persvio_libdem_mex_nc <- lm(libdem_index~treat*pers_vio_n, data=m_dem))
summary(int_persvio_libdem_mex <- lm(libdem_index~treat*pers_vio_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countecon_libdem_mex_nc <- lm(libdem_index~treat*count_econ_recode_n, data=m_dem))
summary(int_countecon_libdem_mex <- lm(libdem_index~treat*count_econ_recode_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countvio_libdem_mex_nc <- lm(libdem_index~treat*count_vio_recode_n, data=m_dem))
summary(int_countvio_libdem_mex <- lm(libdem_index~treat*count_vio_recode_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_polit_libdem_mex_nc <- lm(libdem_index~treat*politpart_n, data=m_dem))
summary(int_polit_libdem_mex <- lm(libdem_index~treat*politpart_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_elite_libdem_mex_nc <- lm(libdem_index~treat*elite_recode_n, data=m_dem))
summary(int_elite_libdem_mex <- lm(libdem_index~treat*elite_recode_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_party_libdem_mex_nc <- lm(libdem_index~treat*party_recode_1, data=m_dem))
summary(int_party_libdem_mex <- lm(libdem_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=m_dem))

#Peru- libdem_index
summary(int_pres_libdem_per_nc <- lm(libdem_index~treat*pres_n, data=p_dem))
summary(int_pres_libdem_per <- lm(libdem_index~treat*pres_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persecon_libdem_per_nc <- lm(libdem_index~treat*pers_econ_n, data=p_dem))
summary(int_persecon_libdem_per <- lm(libdem_index~treat*pers_econ_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persvio_libdem_per_nc <- lm(libdem_index~treat*pers_vio_n, data=p_dem))
summary(int_persvio_libdem_per <- lm(libdem_index~treat*pers_vio_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countecon_libdem_per_nc <- lm(libdem_index~treat*count_econ_recode_n, data=p_dem))
summary(int_countecon_libdem_per <- lm(libdem_index~treat*count_econ_recode_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countvio_libdem_per_nc <- lm(libdem_index~treat*count_vio_recode_n, data=p_dem))
summary(int_countvio_libdem_per <- lm(libdem_index~treat*count_vio_recode_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_polit_libdem_per_nc <- lm(libdem_index~treat*politpart_n, data=p_dem))
summary(int_polit_libdem_per <- lm(libdem_index~treat*politpart_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_elite_libdem_per_nc <- lm(libdem_index~treat*elite_recode_n, data=p_dem))
summary(int_elite_libdem_per <- lm(libdem_index~treat*elite_recode_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_party_libdem_per_nc <- lm(libdem_index~treat*party_recode_1, data=p_dem))
summary(int_party_libdem_per <- lm(libdem_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=p_dem))

#Mexico- liberal_index
summary(int_pres_liberal_mex_nc <- lm(liberal_index~treat*pres_n, data=m_dem))
summary(int_pres_liberal_mex <- lm(liberal_index~treat*pres_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persecon_liberal_mex_nc <- lm(liberal_index~treat*pers_econ_n, data=m_dem))
summary(int_persecon_liberal_mex <- lm(liberal_index~treat*pers_econ_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_persvio_liberal_mex_nc <- lm(liberal_index~treat*pers_vio_n, data=m_dem))
summary(int_persvio_liberal_mex <- lm(liberal_index~treat*pers_vio_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countecon_liberal_mex_nc <- lm(liberal_index~treat*count_econ_recode_n, data=m_dem))
summary(int_countecon_liberal_mex <- lm(liberal_index~treat*count_econ_recode_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_countvio_liberal_mex_nc <- lm(liberal_index~treat*count_vio_recode_n, data=m_dem))
summary(int_countvio_liberal_mex <- lm(liberal_index~treat*count_vio_recode_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_polit_liberal_mex_nc <- lm(liberal_index~treat*politpart_n, data=m_dem))
summary(int_polit_liberal_mex <- lm(liberal_index~treat*politpart_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_elite_liberal_mex_nc <- lm(liberal_index~treat*elite_recode_n, data=m_dem))
summary(int_elite_liberal_mex <- lm(liberal_index~treat*elite_recode_n+age+gender_n+income_n+ideo_1, data=m_dem))
summary(int_party_liberal_mex_nc <- lm(liberal_index~treat*party_recode_1, data=m_dem))
summary(int_party_liberal_mex <- lm(liberal_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=m_dem))

#Peru- liberal_index
summary(int_pres_liberal_per_nc <- lm(liberal_index~treat*pres_n, data=p_dem))
summary(int_pres_liberal_per <- lm(liberal_index~treat*pres_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persecon_liberal_per_nc <- lm(liberal_index~treat*pers_econ_n, data=p_dem))
summary(int_persecon_liberal_per <- lm(liberal_index~treat*pers_econ_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_persvio_liberal_per_nc <- lm(liberal_index~treat*pers_vio_n, data=p_dem))
summary(int_persvio_liberal_per <- lm(liberal_index~treat*pers_vio_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countecon_liberal_per_nc <- lm(liberal_index~treat*count_econ_recode_n, data=p_dem))
summary(int_countecon_liberal_per <- lm(liberal_index~treat*count_econ_recode_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_countvio_liberal_per_nc <- lm(liberal_index~treat*count_vio_recode_n, data=p_dem))
summary(int_countvio_liberal_per <- lm(liberal_index~treat*count_vio_recode_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_polit_liberal_per_nc <- lm(liberal_index~treat*politpart_n, data=p_dem))
summary(int_polit_liberal_per <- lm(liberal_index~treat*politpart_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_elite_liberal_per_nc <- lm(liberal_index~treat*elite_recode_n, data=p_dem))
summary(int_elite_liberal_per <- lm(liberal_index~treat*elite_recode_n+age+gender_n+income_n+ideo_1, data=p_dem))
summary(int_party_liberal_per_nc <- lm(liberal_index~treat*party_recode_1, data=p_dem))
summary(int_party_liberal_per <- lm(liberal_index~treat*party_recode_1+age+gender_n+income_n+ideo_1, data=p_dem))

#Export Results ----------------------------------------------------------

##Regressions
#Polyarchy
regressions_poly <- list(reg_poly_mex_nc, reg_poly_per_nc, reg_poly_mex, reg_poly_per, type = "text")
regressions_poly <- stargazer(regressions_poly, type = "text")
regressions_poly_matrix <- as.matrix(regressions_poly)
write_xlsx(as.data.frame(regressions_poly_matrix), path = "Results.xlsx")

#Liberal Democracy
regressions_libdem <- list(reg_libdem_mex_nc, reg_libdem_per_nc, reg_libdem_mex, reg_libdem_per, type = "text")
regressions_libdem <- stargazer(regressions_libdem, type = "text")
regressions_libdem_matrix <- as.matrix(regressions_libdem)
write_xlsx(as.data.frame(regressions_libdem_matrix), path = "Results.xlsx")

#Liberal
regressions_liberal <- list(reg_liberal_mex_nc, reg_liberal_per_nc, reg_liberal_mex, reg_liberal_per, type = "text")
regressions_liberal <- stargazer(regressions_liberal, type = "text")
regressions_liberal_matrix <- as.matrix(regressions_liberal)
write_xlsx(as.data.frame(regressions_liberal_matrix), path = "Results.xlsx")

##Pres Interactions
#Polyarchy
int_pres_poly <- list(int_pres_poly_mex_nc, int_pres_poly_per_nc, int_pres_poly_mex, int_pres_poly_per, type = "text")
int_pres_poly <- stargazer(int_pres_poly, type = "text")
int_pres_poly_matrix <- as.matrix(int_pres_poly)
write_xlsx(as.data.frame(int_pres_poly_matrix), path = "Results.xlsx")

#Liberal Democracy
int_pres_libdem <- list(int_pres_libdem_mex_nc, int_pres_libdem_per_nc, int_pres_libdem_mex, int_pres_libdem_per, type = "text")
int_pres_libdem <- stargazer(int_pres_libdem, type = "text")
int_pres_libdem_matrix <- as.matrix(int_pres_libdem)
write_xlsx(as.data.frame(int_pres_libdem_matrix), path = "Results.xlsx")

#Liberal
int_pres_liberal <- list(int_pres_liberal_mex_nc, int_pres_liberal_per_nc, int_pres_liberal_mex, int_pres_liberal_per, type = "text")
int_pres_liberal <- stargazer(int_pres_liberal, type = "text")
int_pres_liberal_matrix <- as.matrix(int_pres_liberal)
write_xlsx(as.data.frame(int_pres_liberal_matrix), path = "Results.xlsx")

##Pers_Econ Interactions
#Polyarchy
int_persecon_poly <- list(int_persecon_poly_mex_nc, int_persecon_poly_per_nc, int_persecon_poly_mex, int_persecon_poly_per, type = "text")
int_persecon_poly <- stargazer(int_persecon_poly, type = "text")
int_persecon_poly_matrix <- as.matrix(int_persecon_poly)
write_xlsx(as.data.frame(int_persecon_poly_matrix), path = "Results.xlsx")

#Liberal Democracy
int_persecon_libdem <- list(int_persecon_libdem_mex_nc, int_persecon_libdem_per_nc, int_persecon_libdem_mex, int_persecon_libdem_per, type = "text")
int_persecon_libdem <- stargazer(int_persecon_libdem, type = "text")
int_persecon_libdem_matrix <- as.matrix(int_persecon_libdem)
write_xlsx(as.data.frame(int_persecon_libdem_matrix), path = "Results.xlsx")

#Liberal
int_persecon_liberal <- list(int_persecon_liberal_mex_nc, int_persecon_liberal_per_nc, int_persecon_liberal_mex, int_persecon_liberal_per, type = "text")
int_persecon_liberal <- stargazer(int_persecon_liberal, type = "text")
int_persecon_liberal_matrix <- as.matrix(int_persecon_liberal)
write_xlsx(as.data.frame(int_persecon_liberal_matrix), path = "Results.xlsx")

##Pers_Vio Interactions
#Polyarchy
int_persvio_poly <- list(int_persvio_poly_mex_nc, int_persvio_poly_per_nc, int_persvio_poly_mex, int_persvio_poly_per, type = "text")
int_persvio_poly <- stargazer(int_persvio_poly, type = "text")
int_persvio_poly_matrix <- as.matrix(int_persvio_poly)
write_xlsx(as.data.frame(int_persvio_poly_matrix), path = "Results.xlsx")

#Liberal Democracy
int_persvio_libdem <- list(int_persvio_libdem_mex_nc, int_persvio_libdem_per_nc, int_persvio_libdem_mex, int_persvio_libdem_per, type = "text")
int_persvio_libdem <- stargazer(int_persvio_libdem, type = "text")
int_persvio_libdem_matrix <- as.matrix(int_persvio_libdem)
write_xlsx(as.data.frame(int_persvio_libdem_matrix), path = "Results.xlsx")

#Liberal
int_persvio_liberal <- list(int_persvio_liberal_mex_nc, int_persvio_liberal_per_nc, int_persvio_liberal_mex, int_persvio_liberal_per, type = "text")
int_persvio_liberal <- stargazer(int_persvio_liberal, type = "text")
int_persvio_liberal_matrix <- as.matrix(int_persvio_liberal)
write_xlsx(as.data.frame(int_persvio_liberal_matrix), path = "Results.xlsx")

##Count_Econ Interactions
#Polyarchy
int_countecon_poly <- list(int_countecon_poly_mex_nc, int_countecon_poly_per_nc, int_countecon_poly_mex, int_countecon_poly_per, type = "text")
int_countecon_poly <- stargazer(int_countecon_poly, type = "text")
int_countecon_poly_matrix <- as.matrix(int_countecon_poly)
write_xlsx(as.data.frame(int_countecon_poly_matrix), path = "Results.xlsx")

#Liberal Democracy
int_countecon_libdem <- list(int_countecon_libdem_mex_nc, int_countecon_libdem_per_nc, int_countecon_libdem_mex, int_countecon_libdem_per, type = "text")
int_countecon_libdem <- stargazer(int_countecon_libdem, type = "text")
int_countecon_libdem_matrix <- as.matrix(int_countecon_libdem)
write_xlsx(as.data.frame(int_countecon_libdem_matrix), path = "Results.xlsx")

#Liberal
int_countecon_liberal <- list(int_countecon_liberal_mex_nc, int_countecon_liberal_per_nc, int_countecon_liberal_mex, int_countecon_liberal_per, type = "text")
int_countecon_liberal <- stargazer(int_countecon_liberal, type = "text")
int_countecon_liberal_matrix <- as.matrix(int_countecon_liberal)
write_xlsx(as.data.frame(int_countecon_liberal_matrix), path = "Results.xlsx")

##Count_Vio Interactions
#Polyarchy
int_countvio_poly <- list(int_countvio_poly_mex_nc, int_countvio_poly_per_nc, int_countvio_poly_mex, int_countvio_poly_per, type = "text")
int_countvio_poly <- stargazer(int_countvio_poly, type = "text")
int_countvio_poly_matrix <- as.matrix(int_countvio_poly)
write_xlsx(as.data.frame(int_countvio_poly_matrix), path = "Results.xlsx")

#Liberal Democracy
int_countvio_libdem <- list(int_countvio_libdem_mex_nc, int_countvio_libdem_per_nc, int_countvio_libdem_mex, int_countvio_libdem_per, type = "text")
int_countvio_libdem <- stargazer(int_countvio_libdem, type = "text")
int_countvio_libdem_matrix <- as.matrix(int_countvio_libdem)
write_xlsx(as.data.frame(int_countvio_libdem_matrix), path = "Results.xlsx")

#Liberal
int_countvio_liberal <- list(int_countvio_liberal_mex_nc, int_countvio_liberal_per_nc, int_countvio_liberal_mex, int_countvio_liberal_per, type = "text")
int_countvio_liberal <- stargazer(int_countvio_liberal, type = "text")
int_countvio_liberal_matrix <- as.matrix(int_countvio_liberal)
write_xlsx(as.data.frame(int_countvio_liberal_matrix), path = "Results.xlsx")

##Politpart Interactions
#Polyarchy
int_polit_poly <- list(int_polit_poly_mex_nc, int_polit_poly_per_nc, int_polit_poly_mex, int_polit_poly_per, type = "text")
int_polit_poly <- stargazer(int_polit_poly, type = "text")
int_polit_poly_matrix <- as.matrix(int_polit_poly)
write_xlsx(as.data.frame(int_polit_poly_matrix), path = "Results.xlsx")

#Liberal Democracy
int_polit_libdem <- list(int_polit_libdem_mex_nc, int_polit_libdem_per_nc, int_polit_libdem_mex, int_polit_libdem_per, type = "text")
int_polit_libdem <- stargazer(int_polit_libdem, type = "text")
int_polit_libdem_matrix <- as.matrix(int_polit_libdem)
write_xlsx(as.data.frame(int_polit_libdem_matrix), path = "Results.xlsx")

#Liberal
int_polit_liberal <- list(int_polit_liberal_mex_nc, int_polit_liberal_per_nc, int_polit_liberal_mex, int_polit_liberal_per, type = "text")
int_polit_liberal <- stargazer(int_polit_liberal, type = "text")
int_polit_liberal_matrix <- as.matrix(int_polit_liberal)
write_xlsx(as.data.frame(int_polit_liberal_matrix), path = "Results.xlsx")

##Elite Interactions
#Polyarchy
int_elite_poly <- list(int_elite_poly_mex_nc, int_elite_poly_per_nc, int_elite_poly_mex, int_elite_poly_per, type = "text")
int_elite_poly <- stargazer(int_elite_poly, type = "text")
int_elite_poly_matrix <- as.matrix(int_elite_poly)
write_xlsx(as.data.frame(int_elite_poly_matrix), path = "Results.xlsx")

#Liberal Democracy
int_elite_libdem <- list(int_elite_libdem_mex_nc, int_elite_libdem_per_nc, int_elite_libdem_mex, int_elite_libdem_per, type = "text")
int_elite_libdem <- stargazer(int_elite_libdem, type = "text")
int_elite_libdem_matrix <- as.matrix(int_elite_libdem)
write_xlsx(as.data.frame(int_elite_libdem_matrix), path = "Results.xlsx")

#Liberal
int_elite_liberal <- list(int_elite_liberal_mex_nc, int_elite_liberal_per_nc, int_elite_liberal_mex, int_elite_liberal_per, type = "text")
int_elite_liberal <- stargazer(int_elite_liberal, type = "text")
int_elite_liberal_matrix <- as.matrix(int_elite_liberal)
write_xlsx(as.data.frame(int_elite_liberal_matrix), path = "Results.xlsx")