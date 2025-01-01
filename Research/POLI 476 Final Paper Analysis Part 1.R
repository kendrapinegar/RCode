library(foreign)
library(ivreg)
terror = read.dta("/Users/BYU Rental/Box/PERSONAL WORK/POLI 476/replicationdatajpr-oldstata.dta")
View(terror)

###govenrment deprivation index
#loggdp, politics, nmbrtrr, rel
terror$loggdp_c2sd = (terror$loggdp - mean(terror$loggdp))/(2*sd(terror$loggdp))

fit = glm(pgood ~ loggdp + politics + nmbrtrr + rel + factor(year), family = binomial(link = "logit"), data = terror)
summary(fit)

predict = posterior_epred(fit, newdata = new_scm, type = "response")
subeff_scm = data.frame(predict_scm)

                    # Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       0.7909838  0.0956684   8.268 2.27e-16 ***
# loggdp           -0.0782029  0.0079988  -9.777  < 2e-16 ***
# politics          0.3401731  0.0200317  16.982  < 2e-16 ***
# nmbrtrr          -0.0018064  0.0020637  -0.875   0.3815    
# rel               0.0563069  0.0242896   2.318   0.0205 *

#With time fixed effects, no entity fixed effects
                    # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       1.2159428  0.4965302   2.449   0.0143 *
# loggdp           -0.3858179  0.0412980  -9.342   <2e-16 ***
# politics          1.8793713  0.1252407  15.006   <2e-16 ***
# nmbrtrr          -0.0074090  0.0103108  -0.719   0.4724  
# rel               0.3090809  0.1218412   2.537   0.0112 *

#With time fixed effects, no entity fixed effects (invlogit)
                    # Estimate 
# (Intercept)       0.7713488  
# loggdp            0.6963813  
# politics          0.956699   
# nmbrtrr           0.7700394    
# rel               0.8212771  

#2sls
#x: loggdp (continuous)
#y: propertycount (count variable)
#z: pgood (binary, 0 to 1)
#stage 1

terror_sub = subset(terror, !is.na(terror$loggdp))
terror_sub2 = subset(terror_sub2, !is.na(terror_sub2$logmil))
stage1 = lm(damage ~ loggdp + politics + logmil + factor(year), data = terror_sub2)
summary(stage1, digit = 3)
#predict x-hat
terror_sub2$loggdp_hat = predict(stage1, na.rm = TRUE)
#stage 2
fit_2sls = lm(propertycount ~ loggdp_hat, data = terror_sub2)
summary(fit_2sls, digit = 3)

ok = lm(propertycount ~ pgood + factor(year), data = terror_sub)
summary(ok)

exclusion = lm(damage ~ pgood, data = terror_sub2)
summary(exclusion)

stargazer(exclusion, type="html", out="exclusion.doc")

fit_2sls_pcount = ivreg(propertycount ~ loggdp + nmbrtrr + factor(year) | pgood + nmbrtrr + factor(year), data = terror_sub)
summary(fit_2sls_pcount)

fit_2sls_nkill = ivreg(nkill ~ loggdp + nmbrtrr + factor(year) | pgood + nmbrtrr + factor(year), data = terror_sub)
summary(fit_2sls_nkill)

m3 = ivreg(damage ~ loggdp + politics + logmil + factor(year) | pgood + politics + logmil + factor(year), data = terror_sub2)

glmer = ()
