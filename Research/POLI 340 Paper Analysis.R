###POLI 340 Paper Analysis

#load packages
library(tidyverse)
library(dplyr)
library(plm)
library(lme4)
library(stargazer)
library(writexl)

#upload data sets
satis <- read.csv("/Users/kap237/Box/PERSONAL WORK/POLI 340/Democratic Satisfaction, Claassen.csv")
protests <-  read.csv("/Users/kap237/Box/PERSONAL WORK/POLI 340/mmALL_073120_csv.csv")
vdem <- read.csv("/Users/kap237/Box/PERSONAL WORK/POLI 340/V-Dem-CY-Core-v14.csv")
econ <- read.csv("/Users/kap237/Box/PERSONAL WORK/POLI 340/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_77536.csv")

#clean up the data sets

#econ data set: transform to long data, drop X in the years, create GDP in 1000s variable
econ <- econ %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               values_to = "gdppercap") %>%
  mutate(year = sub("^X", "", year)) %>%
  select(country, year, gdppercap) %>%
  mutate(gdppercap_1000 = gdppercap / 1000)

#vdem data set: drop miscellaneous variables, filter data to 1990-2020, rename country variable
vdem <- vdem %>%
  select(country_name, year, v2x_libdem) %>%
  filter(year >= 1990 & year <= 2020) %>%
  rename(country = country_name)

#protests data set: sum total protests by country-year term
protests <- protests %>%
  group_by(country, year) %>%
  summarise(total_protests = sum(protest, na.rm = TRUE), .groups = "drop")

#create state retaliation variables
protests_2 <- read.csv("/Users/kap237/Box/PERSONAL WORK/POLI 340/mmALL_073120_csv.csv")

protests_2 <- protests_2 %>%
  select(year, country, stateresponse1, stateresponse2, stateresponse3, stateresponse4, stateresponse5, stateresponse6, stateresponse7)

#binary state retaliation variable (1 if retaliation, 0 if no)
protests_2$retaliation <- 0
protests_2$retaliation[protests_2$stateresponse1 %in% c("arrests", "beatings", "killings", "shootings", "crowd dispersal")] <- 1
protests_2$retaliation[protests_2$stateresponse2 %in% c("arrests", "beatings", "killings", "shootings", "crowd dispersal")] <- 1
protests_2$retaliation[protests_2$stateresponse3 %in% c("arrests", "beatings", "killings", "shootings", "crowd dispersal")] <- 1
protests_2$retaliation[protests_2$stateresponse4 %in% c("arrests", "beatings", "killings", "shootings", "crowd dispersal")] <- 1
protests_2$retaliation[protests_2$stateresponse5 %in% c("arrests", "beatings", "killings", "shootings", "crowd dispersal")] <- 1
protests_2$retaliation[protests_2$stateresponse6 %in% c("arrests", "beatings", "killings", "crowd dispersal")] <- 1
protests_2$retaliation[protests_2$stateresponse7 %in% c("arrests", "beatings", "killings", "crowd dispersal")] <- 1

#for some reason, R won't let me perform the next three commands together, so I separate them
protests_2 <- protests_2 %>%
  group_by(country, year)

protests_2 <- protests_2 %>%
  summarise(total_retaliation = sum(retaliation, na.rm = TRUE), .groups = "drop")

protests_2 <- protests_2 %>%
  mutate(retaliation = ifelse(total_retaliation >= 1, 1, 0))

#index values for retaliation index variable: crowd dispersal: 1/5; arrests: 2/5; beatings: 3/5; shootings: 4/5; killings: 1

#average the state retaliation index
protests_3 <- read.csv("/Users/kap237/Box/PERSONAL WORK/POLI 340/mmALL_073120_csv.csv")

#create binary variables for each state retaliation action
protests_3$retaliation_dispersal <- 0
protests_3$retaliation_dispersal[protests_3$stateresponse1 == "crowd dispersal" |
                                   protests_3$stateresponse2 == "crowd dispersal" |
                                   protests_3$stateresponse3 == "crowd dispersal" |
                                   protests_3$stateresponse4 == "crowd dispersal" |
                                   protests_3$stateresponse5 == "crowd dispersal" |
                                   protests_3$stateresponse6 == "crowd dispersal" |
                                   protests_3$stateresponse7 == "crowd dispersal"] <- 1/5
protests_3$retaliation_arrests <- 0
protests_3$retaliation_arrests[protests_3$stateresponse1 == "arrests" |
                                 protests_3$stateresponse2 == "arrests" |
                                 protests_3$stateresponse3 == "arrests" |
                                 protests_3$stateresponse4 == "arrests" |
                                 protests_3$stateresponse5 == "arrests" |
                                 protests_3$stateresponse6 == "arrests" |
                                 protests_3$stateresponse7 == "arrests"] <- 2/5
protests_3$retaliation_beatings <- 0
protests_3$retaliation_beatings[protests_3$stateresponse1 == "beatings" |
                                  protests_3$stateresponse2 == "beatings" |
                                  protests_3$stateresponse3 == "beatings" |
                                  protests_3$stateresponse4 == "beatings" |
                                  protests_3$stateresponse5 == "beatings" |
                                  protests_3$stateresponse6 == "beatings" |
                                  protests_3$stateresponse7 == "beatings"] <- 3/5
protests_3$retaliation_shootings <- 0
protests_3$retaliation_shootings[protests_3$stateresponse1 == "shootings" |
                                   protests_3$stateresponse2 == "shootings" |
                                   protests_3$stateresponse3 == "shootings" |
                                   protests_3$stateresponse4 == "shootings" |
                                   protests_3$stateresponse5 == "shootings" |
                                   protests_3$stateresponse6 == "shootings" |
                                   protests_3$stateresponse7 == "shootings"] <- 4/5
protests_3$retaliation_killings <- 0
protests_3$retaliation_killings[protests_3$stateresponse1 == "killings" |
                                  protests_3$stateresponse2 == "killings" |
                                  protests_3$stateresponse3 == "killings" |
                                  protests_3$stateresponse4 == "killings" |
                                  protests_3$stateresponse5 == "killings" |
                                  protests_3$stateresponse6 == "killings" |
                                  protests_3$stateresponse7 == "killings"] <- 1

#sum binary variables and average by number of protests in a country-year
protests_3 <- protests_3 %>%
  group_by(country, year) %>%
  summarise(retaliation_index = sum(retaliation_dispersal, retaliation_arrests, retaliation_beatings, retaliation_shootings, retaliation_killings, na.rm = TRUE), .groups = "drop")

#merge retaliation variables into one data set
protests_2 <- merge(protests_2, protests_3, by = c("country", "year"))

protests_2 <- protests_2 %>%
  mutate(retaliation_index = ifelse(total_retaliation == 0, 0, retaliation_index/total_retaliation))

protests_2 <- protests_2 %>%
  select(country, year, total_retaliation, retaliation, retaliation_index)

#protester violence
protests_4 <- read.csv("/Users/kap237/Box/PERSONAL WORK/POLI 340/mmALL_073120_csv.csv")

protests_4 <- protests_4 %>%
  select(country, year, protesterviolence) %>%
  group_by(country, year) %>%
  summarise(violence_sum = sum(protesterviolence, na.rm = TRUE), .groups = "drop") %>%
  mutate(violence = ifelse(violence_sum >= 1, 1, 0))

#merging data sets
satis$country <- satis$Country
satis$year <- satis$Year

demsatis <- merge(satis, protests, by = c("year", "country"))
demsatis$Country <- NULL
demsatis$First_yr <- NULL
demsatis$Year <- NULL
demsatis <- merge(demsatis, vdem, by = c("year", "country"))
demsatis <- merge(demsatis, econ, by = c("year", "country"))
demsatis <- merge(demsatis, protests_2, by = c("year", "country"))
demsatis <- merge(demsatis, protests_4, by = c("year", "country"))

#clean fully merged data set
demsatis <- demsatis %>%
  rename(satis_u95 = Satis_u95) %>%
  rename(satis_sd = Satis_sd) %>%
  rename(satis_l95 = Satis_l95) %>%
  rename(satis = Satis)

#create one-year satisfaction lag variable
demsatis <- demsatis %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(satis_lag = dplyr::lag(satis)) %>%
  ungroup()

#create two-year satisfaction lag variable
demsatis <- demsatis %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(satis_lag2 = dplyr::lag(satis, n = 2)) %>%
  ungroup()

#DATA ANALYSIS

#Table 1: regression with no lag satisfaction variable (country/year FEs, controls)
#no FE
reg1 <- lm(satis ~ total_protests + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, data = demsatis)
summary(reg1)

#Country FE
reg2 <- plm(satis ~ total_protests + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, index = "country", model = "within", data = demsatis)
summary(reg2)

#Year FE
reg3 <- plm(satis ~ total_protests + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, index = "year", model = "within", data = demsatis)
summary(reg3)

#Full model: no lag satisfaction regression, country and year FE
reg4 <- plm(satis ~ total_protests + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, index = c("country", "year"), model = "within", data = demsatis)
summary(reg4)

#Export Table 1
table1 <- list(reg1, reg2, reg3, reg4, type = "text")
table1 <- stargazer(table1, type = "html", out = "Regression_table_1.doc")

#Table 2: regression with one-year lag satisfaction variable (country/year FEs, controls)
#No FE
reg5 <- lm(satis_lag ~ total_protests + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, data = demsatis)
summary(reg5)

#Country FE
reg6 <- plm(satis_lag ~ total_protests + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, index = "country", model = "within", data = demsatis)
summary(reg6)

#Year FE
reg7 <- plm(satis_lag ~ total_protests + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, index = "year", model = "within", data = demsatis)
summary(reg7)

#Full model: one-year lag satisfaction regression, country and year FE
reg8 <- plm(satis_lag ~ total_protests + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, index = c("country", "year"), model = "within", data = demsatis)
summary(reg8)

#Export Table 2
table2 <- list(reg5, reg6, reg7, reg8, type = "text")
table2 <- stargazer(table2, type = "html", out = "Regression_table_2.doc")

#Table 3: lag variable comparison (no vs. one year vs. two year)
#Full model: two-year lag satisfaction regression, country and year FE
reg9 <- plm(satis_lag2 ~ total_protests + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, index = c("country", "year"), model = "within", data = demsatis)
summary(reg9)

#Extra two-year lag regressions (not included in the paper)
reg10 <- lm(satis_lag2 ~ total_protests + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, data = demsatis)
summary(reg10)

reg11 <- plm(satis_lag2 ~ total_protests + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, index = "country", model = "within", data = demsatis)
summary(reg11)

reg12 <- plm(satis_lag2 ~ total_protests + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, index = "year", model = "within", data = demsatis)
summary(reg12)

#Export Table 3
table3 <- list(reg4, reg8, reg9, type = "text")
table3 <- stargazer(table3, type = "html", out = "Main_regressions.doc")

#Table 4: binary protest regression (country/year FE, controls)
#Create binary protest variable
demsatis <- demsatis %>%
  mutate(protest_binary = ifelse(total_protests >= 1, 1, 0))

#No lag, country and year FE
reg13 <- plm(satis ~ protest_binary + v2x_libdem + gdppercap_1000 + retaliation_index + violence, index = c("country", "year"), model = "within", data = demsatis)
summary(reg13)

#One-year lag, country and year FE
reg14 <- plm(satis_lag ~ protest_binary + v2x_libdem + gdppercap_1000 + retaliation_index + violence, index = c("country", "year"), model = "within", data = demsatis)
summary(reg14)

#Two-year lag, country and year FE
reg15 <- plm(satis_lag2 ~ protest_binary + v2x_libdem + gdppercap_1000 + retaliation_index + violence, index = c("country", "year"), model = "within", data = demsatis)
summary(reg15)

#Extra binary regressions (not included in the paper)
#No lag, no FE
reg16 <- lm(satis ~ protest_binary + v2x_libdem + gdppercap_1000 + retaliation_index + violence, data = demsatis)
summary(reg16)

#One-year lag, no FE
reg17 <- lm(satis_lag ~ protest_binary + v2x_libdem + gdppercap_1000 + retaliation_index + violence, data = demsatis)
summary(reg17)

#Export Table 4
table4 <- list(reg13, reg14, reg15, type = "text")
table4 <- stargazer(table4, type = "html", out = "Binary_regressions.doc")

#Linear model plot
ggplot(data = demsatis, aes(x = total_protests, y = satis)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#CAA5D4", se = TRUE)

#Log plot: log total_protests + 1
ggplot(data = demsatis, aes(x = log(total_protests +1), y = satis)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "#CAA5D4", se = TRUE)

#Boxplot: dividing protests into low, medium, high categories
#Create the new data set with divisions
boxplot <- demsatis %>%
  mutate(protest_category = cut(total_protests,
                                breaks = c(-Inf, 5, 20, Inf),
                                labels = c("Low", "Medium", "High")))

#Create the division as predicted by the squared term coefficients (-(beta1)/2*beta2)
boxplot <- boxplot %>%
  mutate(protest_inflection = cut(total_protests,
                                  breaks = c(-Inf, 30, Inf),
                                  labels = c("Low", "High")))


#Plot
ggplot(data = boxplot, aes(x = protest_category, y = satis)) +
  geom_boxplot() +
  labs(x = "Protest Category", y = "Democratic Satisfaction", 
       title = "Protest Categories vs. Democratic Satisfaction")

ggplot(data = boxplot, aes(x = protest_inflection, y = satis)) +
  geom_boxplot() +
  labs(x = "Protest Inflection", y = "Democratic Satisfaction")
#Nonlinear model plot
plot1 <- ggplot(data = demsatis, aes(x = total_protests, y = satis)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "#CAA5D4", se = TRUE) +
  labs(x = "Total Protests", y = "Democratic Satisfaction") +
  theme_bw()

#Nonlinear relationship plot (protests vs. liberal democracy)
demsatis_subset <- demsatis %>%
  subset(!is.na(gdppercap_1000))

libdem_model <- plm(v2x_libdem ~ total_protests + I(total_protests^2) + gdppercap_1000 + retaliation_index + violence_sum, index = c("country", "year"), model = "within", data = demsatis_subset, na.rm = TRUE)

demsatis_subset$libdem_pred <- predict(libdem_model, newdata = demsatis_subset)

ggplot(data = demsatis_subset, aes(x = total_protests, y = libdem_pred)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "#B9B7E7", se = TRUE) +
  labs(x = "Total Protests", y = "Liberal Democracy Index") +
  theme_bw()

#Table 5: nonlinear model with squared term
#No lag, no FE
nlm1 <- lm(satis ~ total_protests + I(total_protests^2) + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, data = demsatis)
summary(nlm1)

#No lag, country FE
nlm2 <- plm(satis ~ total_protests + I(total_protests^2) + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, index = "country", data = demsatis)
summary(nlm2)

#No lag, year FE
nlm3 <- plm(satis ~ total_protests + I(total_protests^2) + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, index = "year", model = "within", data = demsatis)
summary(nlm3)

#Full model: no lag satisfaction regression, squared protest term, country and year FE
nlm4 <- plm(satis ~ total_protests + I(total_protests^2) + v2x_libdem + gdppercap_1000 + retaliation_index + violence_sum, index = c("country", "year"), model = "within", data = demsatis)
summary(nlm4)

#Export Table 5
table5 <- list(nlm1, nlm2, nlm3, nlm4, type = "text")
table5 <- stargazer(table5, type = "html", out = "Nonlinear_models.doc")

#Nonlinear model plot: no lag satisfaction regression, squared protest term, country and year FE
demsatis_subset$nlm_pred <- predict(nlm4, newdata = demsatis_subset)

plot2 <- ggplot(data = demsatis_subset, aes(x = total_protests, y = nlm_pred)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "#CAA5D4", se = TRUE) +
  labs(x = "Total Protests", y = "Democratic Satisfaction") +
  theme_bw()

plot2
