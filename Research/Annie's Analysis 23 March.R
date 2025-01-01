library(foreign)
library(ivreg)
library(haven)
library(tidyverse)
library(stargazer)
library(sjPlot)
terror = read_dta("replicationdatajpr-oldstata.dta")
View(terror)

#working on index creation
summary(terror$propertycount)
summary(terror$nkill)

#Generate per capita outcome
terror <- terror %>%
  mutate(percap = (loggdp/logpop))


#NOTE HERE PLEASE READ IT IS VERY IMPORTANT
#The index creation is wonky. For some reason, it will continue to compound infinitely if you 
#run the creation code more than once. Also, the first dam gets destroyed after you make damage
#I have no idea why. In sum, the first time you run it will be the right time and it will compound for
#forever if you try to create the code again or run the outlier code after you make the outlier code
#To fix clear the global environment and run the code again.

terror_sub = subset(terror, !is.na(terror$loggdp))

#outliers version
terror_sub <- terror_sub %>% 
  mutate(dam = ((nkill + propertycount)/2), na.rm=T)

#IV Reg not by hand WITH OUTLIERS
fit_2sls_ivreg_nocontrols = ivreg(dam ~ loggdp | pgood, data = terror_sub)
summary(fit_2sls_ivreg_nocontrols)

fit_2sls_ivreg_fixed = ivreg(dam ~ loggdp + factor(year) | pgood + factor(year), data = terror_sub)
summary(fit_2sls_ivreg_fixed)

fit_2sls_ivreg = ivreg(dam ~ loggdp + politics + logmil + factor(year) | pgood + politics + logmil+ factor(year), data = terror_sub)
summary(fit_2sls_ivreg)

#export
stargazer(fit_2sls_ivreg_nocontrols, fit_2sls_ivreg_fixed, fit_2sls_ivreg, type = "html", out = "fit_outliers.htm", star.cutoffs = 0.05, omit = "year")

#Dropping outliers using IQR method
summary(terror_sub$dam)

terror_sub2 <- terror_sub %>% 
  mutate(damage = ((nkill + propertycount)/2), na.rm=T)

Q1 <- quantile(terror_sub2$damage, 0.25)
Q3 <- quantile(terror_sub2$damage, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filter out outliers
terror_sub2 <- terror_sub2 %>% 
  filter(damage >= lower_bound, damage <= upper_bound)

#IV Reg not by hand WITHOUT OUTLIERS
m1 = ivreg(damage ~ loggdp | pgood, data = terror_sub2)
summary(m1)

m2 = ivreg(damage ~ loggdp + factor(year) | pgood + factor(year), data = terror_sub2)
summary(m2)

m3 = ivreg(damage ~ loggdp + politics + logmil + factor(year) | pgood + politics + logmil + factor(year), data = terror_sub2)
summary(m3)

#export
stargazer(fit_2sls_ivreg_nocontrols, fit_2sls_ivreg_fixed, fit_2sls_ivreg, type="html", out="gdp1.doc", omit = "year")

#per capita outcome WITHOUT OUTLIERS
fit_2sls_ivreg_nocontrols = ivreg(damage ~ percap | pgood, data = terror_sub2)
summary(fit_2sls_ivreg_nocontrols)

fit_2sls_ivreg_fixed = ivreg(damage ~ percap + factor(year) | pgood + factor(year), data = terror_sub2)
summary(fit_2sls_ivreg_fixed)

fit_2sls_ivreg = ivreg(damage ~ percap + politics + logmil + factor(year) | pgood + politics + logmil + factor(year), data = terror_sub2)
summary(fit_2sls_ivreg, diagnostics=T)


#export
stargazer(fit_2sls_ivreg_nocontrols, fit_2sls_ivreg_fixed, fit_2sls_ivreg, type="html", out="percap.doc", omit = "year")

###Percap classic OLS

plm = lm(damage ~ loggdp, data = terror_sub2)
summary(plm)

plm2 = lm(damage ~ pgood, data = terror_sub2)
summary(plm2)

plm3 = lm(damage ~ loggdp + pgood + factor(year), data = terror_sub2)
summary(plm3)

plm4 = lm(damage ~ loggdp + pgood + politics + logmil + factor(year), data = terror_sub2)
summary(plm4)

#export OLS
stargazer(plm, plm2, plm3, plm4, type="html", out="ols.doc", omit = "year")


#Matching IVs in the models for the figure
fit_2sls_ivreg = ivreg(damage ~ loggdp + politics + logmil + factor(year) | pgood + politics + logmil + factor(year), data = terror_sub2)
summary(fit_2sls_ivreg, diagnostics=T)
plm4 = lm(damage ~ loggdp + politics + logmil + pgood + factor(year), data = terror_sub2)
summary(plm4)


#Attempting a coefficient plot (OLS vs Instrumental Comparison)
attempt1 <- plot_models(
  plm4, fit_2sls_ivreg,
  rm.terms = c("factor(year)1981", "factor(year)1982", "factor(year)1983", "factor(year)1984", "factor(year)1985", "factor(year)1986", "factor(year)1987", "factor(year)1988",
               "factor(year)1989", "factor(year)1990", "factor(year)1990", "factor(year)1991", "factor(year)1992", "factor(year)1993", "factor(year)1994", "factor(year)1995", "factor(year)1996", "factor(year)1997", "factor(year)1998", "factor(year)1999",
               "factor(year)2000", "factor(year)2001", "factor(year)2002", "factor(year)2003", "factor(year)2004", "factor(year)2005", "factor(year)2006", "factor(year)2007", "factor(year)2008", "factor(year)2009", "factor(year)2010", "factor(year)2011"),
  axis.labels=c( "Public Good Provision", "Military Personnel (Logged)", "Group Has a Political Arm", "GDP (Logged)"),
 # title= "OLS vs Instrumental Model Damage Estimates",
  legend.title = "Model", 
  axis.title= "Damage Estimates",
  m.labels = c("OLS", "Instrumental"),
  colors = c("#073763", "#42a0ba"),
  wrap.labels = 15,
  show.values = T,
  show.p=T,
  digits=3,
  spacing = .8,
  value.size=3,
  show.legend = T,
  vline.color="black"
)

attempt1 + legend_style(pos = "bottom", base.theme = theme_minimal()) +
  theme(text = element_text(family = "serif"))

re1 <- lm(loggdp ~ pgood, data = terror_sub2)
summary(re1)
re2 <- lm(percap ~ pgood, data = terror_sub2)
summary(re2)

stargazer(re1, re2, type="html", out="re.doc", omit = "year")


#Attempting a coefficient plot (Table 2 Visualization)
#IV Reg not by hand WITHOUT OUTLIERS
m1 = ivreg(damage ~ loggdp | pgood, data = terror_sub2)
summary(m1)

m2 = ivreg(damage ~ loggdp + factor(year) | pgood + factor(year), data = terror_sub2)
summary(m2)

m3 = ivreg(damage ~ loggdp + politics + logmil + factor(year) | pgood + politics + logmil + factor(year), data = terror_sub2)
summary(m3)

plot1 <- plot_models(
  m1, m2, m3,
  rm.terms = c("factor(year)1981", "factor(year)1982", "factor(year)1983", "factor(year)1984", "factor(year)1985", "factor(year)1986", "factor(year)1987", "factor(year)1988",
               "factor(year)1989", "factor(year)1990", "factor(year)1990", "factor(year)1991", "factor(year)1992", "factor(year)1993", "factor(year)1994", "factor(year)1995", "factor(year)1996", "factor(year)1997", "factor(year)1998", "factor(year)1999",
               "factor(year)2000", "factor(year)2001", "factor(year)2002", "factor(year)2003", "factor(year)2004", "factor(year)2005", "factor(year)2006", "factor(year)2007", "factor(year)2008", "factor(year)2009", "factor(year)2010", "factor(year)2011"),
  axis.labels=c( "Military Personnel (Logged)", "Group Has a Political Arm", "GDP (Logged)"),
  axis.title= "Damage Estimates",
 # title= "Figure 1: Visualization of Table 2",
  legend.title = "Table 2 Model", 
  m.labels = c("Model 1", "Model 2", "Model 3"),
  colors = c("#073763", "#42a0ba", "#004c00"),
  wrap.labels = 15,
  show.values = T,
  show.p=T,
  digits=3,
  spacing = .9,
  value.size= 3,
  show.legend = T,
  vline.color="black"
)

plot1 + legend_style(pos = "bottom", base.theme = theme_minimal()) +
  theme(text = element_text(family = "serif"))


