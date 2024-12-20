###POLI 370 Paper Analysis

#load packages
library(tidyverse)
library(dplyr)
library(plm)
library(stargazer)
library(writexl)
library(readxl)
library(stringr)
library(haven)
library(loo)
library(MASS)
library(rstanarm)
library(lme4)
library(fixest)
library(margins)

#load data sets
#This is for the global democracy scores (V-Dem Dataset)
vdem <- read.csv("/Users/kap237/Box/PERSONAL WORK/POLI 340/V-Dem-CY-Core-v14.csv")

#This is for the democracy protests (Mass Mobilizations of Autocracies Dataset)
reports <- read_csv("/Users/kap237/Box/PERSONAL WORK/POLI 370/Reports-Level Data (MMAD).csv")


#Turning reports data into event-level variables
max.new <- function(v) {
  if (all(is.na(v))) {
    return(NA) }
  else {
    return(max(v, na.rm = TRUE))
    }
}

mean.new <- function(v) {
  if (all(is.na(v))) {
    return(NA) }
  else {
    return(mean(v, na.rm = TRUE))
  }
}

mmad <- reports %>%
  group_by(cowcode, location, latitude, longitude, asciiname, event_date, side) %>%
  summarise(numreports = n(),
            maxscope = max.new(scope),
            maxpartyviolence = max.new(part_violence),
            max_secengagement = max.new(sec_engagement),
            avg_numparticipants = mean.new(avg_numparticipants),
            issues = paste(issue, collapse = "; "), .groups = "drop")

#Democracy protests variable
mmad <- mmad %>%
  mutate(dem_protests = ifelse(str_detect(mmad$issues, regex("democra", ignore_case = TRUE)), 1, 0))

#Assigning country names to the COW codes
mmad$country[mmad$cowcode == 40] = "Cuba"
mmad$country[mmad$cowcode == 41] = "Haiti"
mmad$country[mmad$cowcode == 91] = "Honduras"
mmad$country[mmad$cowcode == 93] = "Nicaragua"
mmad$country[mmad$cowcode == 101] = "Venezuela"
mmad$country[mmad$cowcode == 145] = "Bolivia"
mmad$country[mmad$cowcode == 310] = "Hungary"
mmad$country[mmad$cowcode == 343] = "Macedonia"
mmad$country[mmad$cowcode == 345] = "Yugoslavia"
mmad$country[mmad$cowcode == 365] = "Russia"
mmad$country[mmad$cowcode == 369] = "Ukraine"
mmad$country[mmad$cowcode == 370] = "Belarus"
mmad$country[mmad$cowcode == 371] = "Armenia"
mmad$country[mmad$cowcode == 372] = "Georgia"
mmad$country[mmad$cowcode == 373] = "Azerbaijan"
mmad$country[mmad$cowcode == 404] = "Guinea-Bissau"
mmad$country[mmad$cowcode == 411] = "Equatorial Guinea"
mmad$country[mmad$cowcode == 420] = "Gambia"
mmad$country[mmad$cowcode == 432] = "Mali"
mmad$country[mmad$cowcode == 434] = "Benin"
mmad$country[mmad$cowcode == 435] = "Mauritania"
mmad$country[mmad$cowcode == 436] = "Niger"
mmad$country[mmad$cowcode == 437] = "Ivory Coast"
mmad$country[mmad$cowcode == 438] = "Guinea"
mmad$country[mmad$cowcode == 439] = "Burkina Faso"
mmad$country[mmad$cowcode == 450] = "Liberia"
mmad$country[mmad$cowcode == 461] = "Togo"
mmad$country[mmad$cowcode == 471] = "Cameroon"
mmad$country[mmad$cowcode == 475] = "Nigeria"
mmad$country[mmad$cowcode == 481] = "Gabon"
mmad$country[mmad$cowcode == 482] = "Central African Republic"
mmad$country[mmad$cowcode == 483] = "Chad"
mmad$country[mmad$cowcode == 484] = "Congo"
mmad$country[mmad$cowcode == 490] = "Democratic Republic of the Congo"
mmad$country[mmad$cowcode == 500] = "Uganda"
mmad$country[mmad$cowcode == 501] = "Kenya"
mmad$country[mmad$cowcode == 510] = "Tanzania"
mmad$country[mmad$cowcode == 516] = "Burundi"
mmad$country[mmad$cowcode == 517] = "Rwanda"
mmad$country[mmad$cowcode == 520] = "Somalia"
mmad$country[mmad$cowcode == 530] = "Ethiopia"
mmad$country[mmad$cowcode == 531] = "Eritrea"
mmad$country[mmad$cowcode == 540] = "Angola"
mmad$country[mmad$cowcode == 541] = "Mozambique"
mmad$country[mmad$cowcode == 551] = "Zambia"
mmad$country[mmad$cowcode == 552] = "Zimbabwe"
mmad$country[mmad$cowcode == 553] = "Malawi"
mmad$country[mmad$cowcode == 565] = "Namibia"
mmad$country[mmad$cowcode == 570] = "Lesotho"
mmad$country[mmad$cowcode == 571] = "Botswana"
mmad$country[mmad$cowcode == 572] = "Eswatini"
mmad$country[mmad$cowcode == 580] = "Madagascar"
mmad$country[mmad$cowcode == 600] = "Morocco"
mmad$country[mmad$cowcode == 615] = "Algeria"
mmad$country[mmad$cowcode == 616] = "Tunisia"
mmad$country[mmad$cowcode == 620] = "Libya"
mmad$country[mmad$cowcode == 625] = "Sudan"
mmad$country[mmad$cowcode == 626] = "South Sudan"
mmad$country[mmad$cowcode == 630] = "Iran"
mmad$country[mmad$cowcode == 640] = "Turkey"
mmad$country[mmad$cowcode == 645] = "Iraq"
mmad$country[mmad$cowcode == 651] = "Egypt"
mmad$country[mmad$cowcode == 652] = "Syria"
mmad$country[mmad$cowcode == 660] = "Lebanon"
mmad$country[mmad$cowcode == 663] = "Jordan"
mmad$country[mmad$cowcode == 670] = "Saudi Arabia"
mmad$country[mmad$cowcode == 679] = "Yemen"
mmad$country[mmad$cowcode == 690] = "Kuwait"
mmad$country[mmad$cowcode == 692] = "Bahrain"
mmad$country[mmad$cowcode == 694] = "Qatar"
mmad$country[mmad$cowcode == 696] = "United Arab Emirates"
mmad$country[mmad$cowcode == 698] = "Oman"
mmad$country[mmad$cowcode == 700] = "Afghanistan"
mmad$country[mmad$cowcode == 701] = "Turkmenistan"
mmad$country[mmad$cowcode == 702] = "Tajikistan"
mmad$country[mmad$cowcode == 703] = "Kyrgyzstan"
mmad$country[mmad$cowcode == 704] = "Uzbekistan"
mmad$country[mmad$cowcode == 705] = "Kazakhstan"
mmad$country[mmad$cowcode == 710] = "China"
mmad$country[mmad$cowcode == 731] = "North Korea"
mmad$country[mmad$cowcode == 770] = "Pakistan"
mmad$country[mmad$cowcode == 771] = "Bangladesh"
mmad$country[mmad$cowcode == 775] = "Myanmar"
mmad$country[mmad$cowcode == 780] = "Sri Lanka"
mmad$country[mmad$cowcode == 790] = "Nepal"
mmad$country[mmad$cowcode == 800] = "Thailand"
mmad$country[mmad$cowcode == 811] = "Cambodia"
mmad$country[mmad$cowcode == 812] = "Laos"
mmad$country[mmad$cowcode == 816] = "Vietnam"
mmad$country[mmad$cowcode == 820] = "Malaysia"
mmad$country[mmad$cowcode == 830] = "Singapore"
mmad$country[mmad$cowcode == 840] = "Philippines"
mmad$country[mmad$cowcode == 910] = "Papua New Guinea"

#Sorting through non-anti-government protests that were marked as democracy protests
#Side: 0 removals remove from dem_protests = 1
mmad$dem_protests[mmad$issues == "for:celebrating Chavez's election victory; for:Chavez in presidential election; for:Chavez in presidential election; for:Chavez in presidential election; for:celebrating Chavez's election victory;for:expansion of revolution; for:celebrating Chavez's election victory;for:expansion of revolution;against:US imperialism; for:celebrating Chavez's election victory;for:socialist democracy; for:celebrating Chavez's election victory; for:Chavez in presidential election"] = 0
mmad$dem_protests[mmad$issues == "for:commemorating anniversary of Venezuela's democracy;against:claims that Chavez is becoming authoritarian"] = 0
mmad$dem_protests[mmad$issues == "against:democratic protest"] = 0
mmad$dem_protests[mmad$issues == "against:Liberal Democratic Party Congress"] = 0
mmad$dem_protests[mmad$issues == "against:Movement for Democratic Change; against:Movement for Democratic Change"] = 0
mmad$dem_protests[mmad$issues == "for:anniversary of pro-democracy movement; for:celebration of the first anniversary of the uprising"] = 0
mmad$dem_protests[mmad$issues == "for:removing pro democracy protesters; against:Occupy Central pro democracy protests; against:Occupy Central pro democracy protests; against:pro democracy protests; against:pro democracy Occupy movement; for:driving away Occupy Central demonstrators"] = 0
mmad$dem_protests[mmad$issues == "against:pro democracy protests; against:Occupy Central pro democracy protests; for:end of pro democracy protests; against:pro democracy protests; against:Occupy Central pro democracy protests"] = 0
mmad$dem_protests[mmad$issues == "for:end of pro democracy protests; against:occupation by pro democracy activists;against:protestors; for:supporting the Hong Kong police force;against:intervention of People's Liberation Army in Hong Kong;against:illegal acts by protesting students"] = 0
mmad$dem_protests[mmad$issues == "against:pro democracy protest"] = 0
mmad$dem_protests[mmad$issues == "against:planned civil disobedience campaign by pro democracy Occupy Central; for:universal suffrage for Chief Executive election in 2017;against:Occupy Central movement; for:universal suffrage for Chief Executive election in 2017;against:Occupy Central movement; against:Occupy Central;against:paralyzing the central business district for real democracy;for:universal suffrage following rules set by Beijing; against:Occupy Central;against:paralyzing the central business district for real democracy;for:universal suffrage following rules set by Beijing; against:planned pro democracy Occupy Central protest campaign;against:violent protests;for:supporting the Communist Party"] = 0
mmad$dem_protests[mmad$issues == "for:limit democracy; for:limit democracy"] = 0
mmad$dem_protests[mmad$issues == "for:loyalty to Hong Kong's and central Chinese government;against:pro democracy protests"] = 0
mmad$dem_protests[mmad$issues == "against:pro democracy protesters; against:Occupy Central pro democracy protests"] = 0
mmad$dem_protests[mmad$issues == "against:pro democracy protests; against:pro democracy protests"] = 0
mmad$dem_protests[mmad$issues == "against:democracy protesters; against:democracy protesters; against:democracy protesters"] = 0
mmad$dem_protests[mmad$issues == "for:law against illegal pro democracy protests; for:jailing pro democracy protesters; for:law against illegal pro democracy protests"] = 0
mmad$dem_protests[mmad$issues == "against:protest by pro democracy camp; against:pro democracy protest camp"] = 0
mmad$dem_protests[mmad$issues == "against:democracy activists disrupting daily life in Hong Kong"] = 0
mmad$dem_protests[mmad$issues == "for:supporting seven police officers charged with assaulting a pro democracy protester;against:pro democracy protests"] = 0
mmad$dem_protests[mmad$issues == "for:election of pro Beijing leader; against:pro democracy protests"] = 0
mmad$dem_protests[mmad$issues == "against:pro democracy protests"] = 0
mmad$dem_protests[mmad$issues == "against:pro democracy protests; for:China's communist party; against:pro democracy protest"] = 0
mmad$dem_protests[mmad$issues == "for:support of police;against:pro democracy protests; for:support of city police;against:traitors"] = 0
mmad$dem_protests[mmad$issues == "for:support of city police;for:safe Hong Kong;against:pro democracy movement; for:support of pro Beijing city leadership;for:support of city police"] = 0
mmad$dem_protests[mmad$issues == "for:peace;for:stability;against:violent protests;for:support of police; against:violent protests; for:defense of police;against:violent protests; against:democracy protests;for:support of police"] = 0
mmad$dem_protests[mmad$issues == "for:support for police; for:support for police;against:democracy movement; for:support of police;against:democracy protests; for:support for police; NA"] = 0
mmad$dem_protests[mmad$issues == "against:democracy in Hong Kong"] = 0
mmad$dem_protests[mmad$issues == "for:pro democracy demonstrators to leave;for:removing barricades by pro democracy groups; for:removing pro democracy barricades;against:illegal Occupy movement"] = 0
mmad$dem_protests[mmad$issues == "against:pro democracy group against corruption scandal surrounding Rajabhakti Park;against:politicization of a sacred royal venue"] = 0

#Side: 3 removals from dem_protest = 1
mmad$dem_protests[mmad$issues == "against:leader of opposition People's Democratic Union"] = 0
mmad$dem_protests[mmad$issues == "for:Ouyahia's resignation;policy change within the Democratic National Rally"] = 0
mmad$dem_protests[mmad$issues == "against:democracy within university; against:democracy within university; against:democracy within university; against:democracy within university; against:lack of democracy within university"] = 0
mmad$dem_protests[mmad$issues == "against:democracy within university" & mmad$location == 128226] = 0
mmad$dem_protests[mmad$issues == "for:withdrawal of the Syrian Democratic Forces from region;against:massacre committed by SDF and US forces in local town;against:SDF abuse against civilians"] = 0
mmad$dem_protests[mmad$issues == "against:policies of Syrian Democratic Forces;against:lack of services that civilians are entitled to"] = 0
mmad$dem_protests[mmad$issues == "against:detention of activists;for:release of detainees;against:Kurdish Democratic Union Party"] = 0
mmad$dem_protests[mmad$issues == "against:firing of Cathay employees linked to protests; against:dismissal of Cathay employees for pro democracy protests; against:dismissal of Cathay staff supporting anti government protests"] = 0

#Additional Removals
mmad$dem_protests[mmad$event_date == "2019-05-31" & mmad$country == "Malawi"] = 0
mmad$dem_protests[mmad$event_date == "2014-10-25" & mmad$country == "Afghanistan"] = 0

#Creation of democracy institution protests variable
#Free and fair elections
mmad <- mmad %>%
  mutate(proelections = ifelse(str_detect(mmad$issues, regex("free elections", ignore_case = TRUE)), 1,
                               ifelse(str_detect(mmad$issues, regex("fair elections", ignore_case = TRUE)), 1,
                                      ifelse(str_detect(mmad$issues, regex("elections without opposition", ignore_case = TRUE)), 1, 0))))

#Freedom of assembly
mmad <- mmad %>%
  mutate(proassembly = ifelse(str_detect(mmad$issues, regex("freedom of assembly", ignore_case = TRUE)), 1, 0))

#Freedom of speech
mmad <- mmad %>%
  mutate(prospeech = ifelse(str_detect(mmad$issues, regex("freedom of speech", ignore_case = TRUE)), 1, 0))
mmad$prospeech[mmad$prospeech == 1 & mmad$side == 0] = 0
mmad$prospeech[mmad$prospeech == 1 & mmad$side == 3] = 0

#Freedom of expression
mmad <- mmad %>%
  mutate(proexpression = ifelse(str_detect(mmad$issues, regex("freedom of expression", ignore_case = TRUE)), 1, 0))
mmad$proexpression[mmad$proexpression == 1 & mmad$side == 3] = 0

#Freedom of Press
mmad <- mmad %>%
  mutate(propress = ifelse(str_detect(mmad$issues, regex("freedom of press", ignore_case = TRUE)), 1, 0))
mmad$propress[mmad$propress == 1 & mmad$side == 3] = 0

#Universal suffrage
mmad <- mmad %>%
  mutate(prosuffrage = ifelse(str_detect(mmad$issues, regex("universal suffrage", ignore_case = TRUE)), 1, 0))
mmad$prosuffrage[mmad$prosuffrage == 1 & mmad$side == 0] = 0

#Right to vote
mmad <- mmad %>%
  mutate(provote = ifelse(str_detect(mmad$issues, regex("right to vote", ignore_case = TRUE)), 1, 0))

#Independent judiciary
mmad <- mmad %>%
  mutate(projudiciary = ifelse(str_detect(mmad$issues, regex("independent judiciary", ignore_case = TRUE)), 1, 0))
mmad$projudiciary[mmad$projudiciary == 1 & mmad$side == 3] = 0


#Democracy institution protests variable: binary
mmad <- mmad %>%
  mutate(democracy_protest = ifelse(dem_protests == 1, 1,
                                    ifelse(proelections == 1, 1,
                                           ifelse(proassembly == 1, 1,
                                                  ifelse(prospeech == 1, 1,
                                                         ifelse(proexpression == 1, 1,
                                                                ifelse(propress == 1, 1,
                                                                       ifelse(prosuffrage == 1, 1,
                                                                              ifelse(provote == 1, 1,
                                                                                     ifelse(projudiciary == 1, 1, 0))))))))))


#Create an aggregate yearly global democracy score (polyarchy and liberal democracy)
vdem <- vdem %>%
  select(country_name, year, v2x_libdem, v2x_polyarchy) %>%
  filter(year >= 1990 & year <= 2020) %>%
  rename(country = country_name)

global_democracy <- vdem %>%
  group_by(year) %>%
  summarise(global_democracy = mean(v2x_libdem, na.rm = TRUE),
            global_polyarchy = mean(v2x_polyarchy, na.rm = TRUE)) %>%
  mutate(global_democracy_lag = dplyr::lag(global_democracy)) %>%
  mutate(global_polyarchy_lag = dplyr::lag(global_polyarchy))

mmad$year <- format(mmad$event_date, "%Y")
mmad$year <- as.integer(mmad$year)

mmad <- mmad %>%
  left_join(global_democracy, by = "year")

#Internet variable (plus lag)
internet <- read_csv("/Users/kap237/Box/PERSONAL WORK/POLI 370/internet_users.csv")
internet <- internet %>%
  pivot_longer(names_to = "year", cols = -country, values_to = "internet100") %>%
  mutate(internet100_lag = dplyr::lag(internet100))

internet$year <- as.integer(internet$year)

mmad <- mmad %>%
  left_join(internet, by = c("year", "country"))


#Control: GDP per capita (plus lag)
econ <- read.csv("/Users/kap237/Box/PERSONAL WORK/POLI 340/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_77536.csv")
econ <- econ %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               values_to = "gdpcap") %>%
  mutate(year = sub("^X", "", year)) %>%
  mutate(gdpcap1000 = gdpcap / 1000) %>%
  mutate(gdpcap1000_lag = dplyr::lag(gdpcap1000))

econ$year <- as.integer(econ$year)

mmad <- mmad %>%
  left_join(econ, by = c("year", "country"))

#Control: GDP per capita growth (plus lag)
growth <- read.csv("/Users/kap237/Box/PERSONAL WORK/POLI 370/GDPpercap_growth.csv")
growth <- growth %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               values_to = "gdpcapgrowth") %>%
  mutate(year = sub("^X", "", year)) %>%
  mutate(gdpcapgrowth_lag = dplyr::lag(gdpcapgrowth))

growth$year <- as.integer(growth$year)

mmad <- mmad %>%
  left_join(growth, by = c("year", "country"))

#Controls: Armed forces in 1000s (plus lag)
forces <- read.csv("/Users/kap237/Box/PERSONAL WORK/POLI 370/armedforces.csv")
forces <- forces %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               values_to = "forces") %>%
  mutate(year = sub("^X", "", year)) %>%
  mutate(forces1000 = forces/1000) %>%
  mutate(forces1000_lag = dplyr::lag(forces1000))

forces$year <- as.integer(forces$year)

mmad <- mmad %>%
  left_join(forces, by = c("year", "country"))

#Total democracy protests: democracy protests (without democracy institutions)
democracy <- mmad %>%
  group_by(country, year, cowcode, internet100, internet100_lag, global_democracy, global_polyarchy, global_democracy_lag, global_polyarchy_lag, gdpcapgrowth, gdpcapgrowth_lag, gdpcap1000, gdpcap1000_lag, forces1000, forces1000_lag) %>%
  summarise(total_protests = sum(dem_protests, na.rm = TRUE),
            avg_partyviolence = mean.new(maxpartyviolence),
            avg_secengagement = mean.new(max_secengagement),
            avg_numparticipants = mean.new(avg_numparticipants),
            issues = paste(issues, collapse = "|| "),
            total_proelections = sum(proelections, na.rm = TRUE),
            total_proassembly = sum(proassembly, na.rm = TRUE),
            total_prospeech = sum(prospeech, na.rm = TRUE),
            total_proexpression = sum(proexpression, na.rm = TRUE),
            total_propress = sum(propress, na.rm = TRUE), .groups = "drop")

democracy <- democracy %>%
  mutate(binary_democracy = ifelse(total_protests >= 1, 1, 0))

#Total democracy institutions protests: democracy protests (with democracy institutions)
mmad <- mmad %>%
  group_by(country, year, cowcode, internet100, internet100_lag, global_democracy, global_democracy_lag, global_polyarchy, global_polyarchy_lag, gdpcap1000, gdpcap1000_lag, gdpcapgrowth, gdpcapgrowth_lag, forces1000, forces1000_lag) %>%
  summarise(democracy_protests = sum(democracy_protest, na.rm = TRUE),
            avg_partyviolence = mean.new(maxpartyviolence),
            avg_secengagement = mean.new(max_secengagement),
            avg_numparticipants = mean.new(avg_numparticipants),
            issues = paste(issues, collapse = "|| "), .groups = "drop")

mmad <- mmad %>%
  mutate(binary_democracy = ifelse(democracy_protests >= 1, 1, 0))

#Eight Approaches: need to eliminate like 3 of them (probably first 2 and perhaps binary variable)
#1: using global liberal democracy score and count democracy protest variable (explicit mentions of democracy motivations for coding)
#2: using global polyarchy score and count democracy protest variable (explicit mentions of democracy motivations for coding)
#3: using global liberal democracy score and binary democracy protest variable (explicit mentions of democracy motivations for coding)
#4: using global polyarchy score and binary democracy protest variable (explicit mentions of democracy motivations for coding)
#5: using global liberal democracy score and count democracy protest variable (mentions of any elements of democracy for coding)
#6: using global polyarchy score and count democracy protest variable (mentions of any elements of democracy for coding)
#7: using global liberal democracy score and binary democracy protest variable (mentions of any elements of democracy for coding)
#8: using global polyarchy score and binary democracy protest variable (mentions of any elements of democracy for coding)

#Model: total democracy protests; global democracy; count protests
reg1 <- lm(total_protests ~ internet100*global_democracy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, data = democracy)
summary(reg1)

reg2 <- plm(total_protests ~ internet100*global_democracy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, index = "country", model = "within", data = democracy)
summary(reg2)

reg3 <- glm.nb(total_protests ~ internet100*global_democracy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, data = democracy)
summary(reg3)
#Significant interaction (p<0.05)

reg4 <- fenegbin(total_protests ~ internet100*global_democracy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000 | country, data = democracy)
summary(reg4)

#Model: total democracy protests; global democracy; logged protests
reg7 <- lm(log(1 + total_protests) ~ internet100*global_democracy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, data = democracy)
summary(reg7)
#Significant interaction (p<0.05)

reg8 <- plm(log(1 + total_protests) ~ internet100*global_democracy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, level = "country", model = "within", data = democracy)
summary(reg8)

#Model: total democracy protests; global polyarchy; count protests
reg9 <- lm(total_protests ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, data = democracy)
summary(reg9)

reg10 <- plm(total_protests ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, index = "country", model = "within", data = democracy)
summary(reg10)

reg11 <- glm.nb(total_protests ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, data = democracy)
summary(reg11)
#Significant interaction (p<0.05)

reg12 <- fenegbin(total_protests ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000 | country, data = democracy)
summary(reg12)

#Model: total democracy protests; global polyarchy; logged protests
reg15 <- lm(log(1 + total_protests) ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, data = democracy)
summary(reg15)
#Significant interaction (p<0.05)

reg16 <- plm(log(1 + total_protests) ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, level = "country", model = "within", data = democracy)
summary(reg16)
#Significant interaction (p<0.05)

#Model: total democracy protests; global democracy; count protests; lagged variables
reg17 <- lm(total_protests ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, data = democracy)
summary(reg17)

reg18 <- plm(total_protests ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, index = "country", model = "within", data = democracy)
summary(reg18)

reg19 <- glm.nb(total_protests ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, data = democracy)
summary(reg19)
#Significant interaction (p<0.05)

reg20 <- fenegbin(total_protests ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag | country, data = democracy)
summary(reg20)
#Somewhat significant interaction (p<0.10)

#Model: total democracy protests; global democracy; logged protests; lagged variables
reg23 <- lm(log(1 + total_protests) ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, data = democracy)
summary(reg23)
#Significant interaction (p<0.05)

reg24 <- plm(log(1 + total_protests) ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, level = "country", model = "within", data = democracy)
summary(reg24)
#Significant interaction (p<0.05)

#Model: total democracy protests; global polyarchy; count protests; lagged variables
reg25 <- lm(total_protests ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, data = democracy)
summary(reg25)

reg26 <- plm(total_protests ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, index = "country", model = "within", data = democracy)
summary(reg26)

reg27 <- glm.nb(total_protests ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, data = democracy)
summary(reg27)
#Significant interaction (p<0.05)

reg28 <- fenegbin(total_protests ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag | country, data = democracy)
summary(reg28)
#Somewhat significant interaction (p<0.10)

#Model: total democracy protests; global polyarchy; logged protests; lagged variables
reg31 <- lm(log(1 + total_protests) ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, data = democracy)
summary(reg31)
#Significant interaction (p<0.05)

reg32 <- plm(log(1 + total_protests) ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, level = "country", model = "within", data = democracy)
summary(reg32)
#Significant interaction (p<0.05)

#Model: total democracy institutions protests; global democracy; logged protests
reg39 <- lm(log(1 + democracy_protests) ~ internet100*global_democracy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, data = mmad)
summary(reg39)
#Significant interaction (p<0.05)

reg40 <- plm(log(1 + democracy_protests) ~ internet100*global_democracy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, level = "country", model = "within", data = mmad)
summary(reg40)

#Model: total democracy institutions protests; global polyarchy; logged protests
reg47 <- lm(log(1 + democracy_protests) ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, data = mmad)
summary(reg47)
#Significant interaction (p<0.05)

reg48 <- plm(log(1 + democracy_protests) ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, level = "country", model = "within", data = mmad)
summary(reg48)

#Model: total democracy institutions protests; global democracy; count protests; lagged variables
reg49 <- lm(democracy_protests ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, data = mmad)
summary(reg49)

reg50 <- plm(democracy_protests ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, index = "country", model = "within", data = mmad)
summary(reg50)

reg51 <- glm.nb(democracy_protests ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, data = mmad)
summary(reg51)
#Significant interaction (p<0.05)

reg52 <- fenegbin(democracy_protests ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag | country, data = mmad)
summary(reg52)

#Model: total democracy institutions protests; global democracy; binary; lagged variables
reg53 <- glm(binary_democracy ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, family = binomial(link = "logit"), data = mmad)
summary(reg53)
#Significant interaction (p<0.05)

reg54 <- femlm(binary_democracy ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag | country, data = mmad)
summary(reg54)

#Model: total democracy institutions protests; global democracy; logged protests; lagged variables
reg55 <- lm(log(1 + democracy_protests) ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, data = mmad)
summary(reg55)
#significant

reg56 <- plm(log(1 + democracy_protests) ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, level = "country", model = "within", data = mmad)
summary(reg56)
#Somewhat significant interaction (p<0.10)

#Model: total democracy institutions protests; global polyarchy; count protests; lagged variables
reg57 <- lm(democracy_protests ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, data = mmad)
summary(reg57)

reg58 <- plm(democracy_protests ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, index = "country", model = "within", data = mmad)
summary(reg58)

reg59 <- glm.nb(democracy_protests ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, data = mmad)
summary(reg59)
#Significant interaction (p<0.05)

reg60 <- fenegbin(democracy_protests ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag | country, data = mmad)
summary(reg60)
#Somewhat significant interaction (p<0.10)

#Model: total democracy institutions protests; global polyarchy; binary; lagged variables
reg61 <- glm(binary_democracy ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, family = binomial(link = "logit"), data = mmad)
summary(reg61)
#Significant interaction (p<0.05)

reg62 <- femlm(binary_democracy ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag | country, data = mmad)
summary(reg62)

#Model: total democracy institutions protests; global polyarchy; logged protests; lagged variables
reg63 <- lm(log(1 + democracy_protests) ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, data = mmad)
summary(reg63)
#Significant interaction (p<0.05)

reg64 <- plm(log(1 + democracy_protests) ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, level = "country", model = "within", data = mmad)
summary(reg64)
#Somewhat significant interaction (p<0.010)

#TWO DIFFERENT VERSION OF THE ABOVE FOR EACH: DEMOCRACY/POLYARCHY, LAG/NO LAG, BINARY/NOT BINARY
#Tables:
#Table 1: total democracy protests; logged protests; no lag; democracy and polyarchy
###### In paper already
table1 <- list(reg7, reg8, reg15, reg16, type = "text")
table1 <- stargazer(table1, type = "html", out = "table1.doc")

#Table 2: total democracy protests; logged protests; one-year lag; democracy and polyarchy
##### In paper already
table2 <- list(reg23, reg24, reg31, reg32, type = "text")
table2 <- stargazer(table2, type = "html", out = "table2.doc")

#Table 3: total democracy protests; count protests; no lag; democracy and polyarchy
##### In paper already
table3 <- list(reg3, reg11)
etable(reg4, reg12) #Have to add these manually because they don't work with the stargazer package
table3 <- stargazer(table3, type = "html", out = "table3.doc")

#Table 4: total democracy protests; count protests; one-year lag; democracy and polyarchy
##### In paper already
table4 <- list(reg19, reg27, type = "text")
etable(reg20, reg28) #Have to add these manually because they don't work with the stargazer package
table4 <- stargazer(table4, type = "html", out = "table4.doc")

#Table A1: total democracy institutions protests; logged protests; no lag; democracy and polyarchy
##### In paper already
tablea1 <- list(reg39, reg40, reg47, reg48, type = "text")
tablea1 <- stargazer(tablea1, type = "html", out = "tablea1.doc")

#Table A2: total democracy institutions protests; logged variables; one-year lag; democracy and polyarchy
##### In paper already
tablea2 <- list(reg55, reg56, reg63, reg64, type = "text")
tablea2 <- stargazer(tablea2, type = "html", out = "tablea2.doc")

#Table A3: total democracy institutions protests; count protests; one-year lag; democracy and polyarchy
##### In paper already
tablea3 <- list(reg51, reg59, type = "text")
etable(reg52, reg60) #Have to add these manually because they don't work with the stargazer package
tablea3 <- stargazer(tablea3, type = "html", out = "tablea3.doc")

#Table A4: total democracy institutions protests; binary marker; one-year lag; democracy and polyarchy
##### Did not include in paper (I forgot)
tablea4 <- list(reg53, reg61, type = "text")
etable(reg54, reg62) #Have to add these manually because they don't work with the stargazer package
tablea4 <- stargazer(tablea4, type = "html", out = "tablea4.doc")


##### Not included in the paper

#Model: total democracy protests; global polyarchy; binary marker
reg13 <- glm(binary_democracy ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, family = binomial(link = "logit"), data = democracy)
summary(reg13)
#Significant interaction (p<0.05)

reg14 <- femlm(binary_democracy ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000 | country, data = democracy)
summary(reg14)

#Model: total democracy protests; global democracy; binary marker; lagged variables
reg21 <- glm(binary_democracy ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, family = binomial(link = "logit"), data = democracy)
summary(reg21)
#Significant interaction (p<0.05)

reg22 <- femlm(binary_democracy ~ internet100_lag*global_democracy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag | country, data = democracy)
summary(reg22)

#Model: total democracy protests; global poyarchy; binary marker; lagged variables
reg29 <- glm(binary_democracy ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag, family = binomial(link = "logit"), data = democracy)
summary(reg29)
#Significant interaction (p<0.05)

reg30 <- femlm(binary_democracy ~ internet100_lag*global_polyarchy_lag + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000_lag + gdpcapgrowth_lag + forces1000_lag | country, data = democracy)
summary(reg30)

#Model: total democracy institutions protests; global democracy; count protests
reg33 <- lm(democracy_protests ~ internet100*global_democracy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, data = mmad)
summary(reg33)

reg34 <- plm(democracy_protests ~ internet100*global_democracy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, index = "country", model = "within", data = mmad)
summary(reg2)

reg35 <- glm.nb(democracy_protests ~ internet100*global_democracy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, data = mmad)
summary(reg35)
#Significant interaction (p<0.05)

reg36 <- fenegbin(democracy_protests ~ internet100*global_democracy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000 | country, data = mmad)
summary(reg36)

#Model: total democracy institutions protests; global democracy; binary marker
reg37 <- glm(binary_democracy ~ internet100*global_democracy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, family = binomial(link = "logit"), data = mmad)
summary(reg37)
#Significant interaction (p<0.05)

reg38 <- femlm(binary_democracy ~ internet100*global_democracy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000 | country, data = mmad)
summary(reg38)

#Model: total democracy institutions protests; global polyarchy; count protests
reg41 <- lm(democracy_protests ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, data = mmad)
summary(reg41)

reg42 <- plm(democracy_protests ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, index = "country", model = "within", data = mmad)
summary(reg42)

reg43 <- glm.nb(democracy_protests ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, data = mmad)
summary(reg43)
#Significant interaction (p<0.05)

reg44 <- fenegbin(democracy_protests ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000 | country, data = mmad)
summary(reg44)

#Model: total democracy institutions protests; global polyarchy; binary marker
reg45 <- glm(binary_democracy ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000, family = binomial(link = "logit"), data = mmad)
summary(reg45)
#Significant interaction (p<0.05)

reg46 <- femlm(binary_democracy ~ internet100*global_polyarchy + avg_partyviolence + avg_secengagement + avg_numparticipants + gdpcap1000 + gdpcapgrowth + forces1000 | country, data = mmad)
summary(reg46)

#Histogram plot
ggplot(democracy, aes(x = total_protests)) + 
  geom_histogram(binwidth = 1, fill = "#CAA5D4", color = "black", alpha = 0.7) + 
  labs(x = "Total Protests", 
       y = "Frequency") + 
  theme_bw()
