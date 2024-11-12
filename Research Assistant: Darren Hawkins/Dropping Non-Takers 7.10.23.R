###Dropping Non-Takers
###Last Updated: August 10, 2023

###Create a variable for the "non-takers"--people who didn't finish the outcomes
##MEXICO
summary(m_dem$partout)
m_dem$nontake <-0
m_dem$nontake[m_dem$partout=="0"]<- 1
table(m_dem$nontake)

#binary variable is now "non-take." 1 means they DID finish the survey
#0 means they didn't. 

###Create a new variable indicating missing responses
m_dem$opt_outs <- ifelse(is.na(m_dem$frexp1) | is.na(m_dem$frexp2) |is.na(m_dem$frassc1) |is.na(m_dem$frassc3) |is.na(m_dem$unisuff1) |is.na(m_dem$unisuff2) |is.na(m_dem$decelec1) |is.na(m_dem$decelec2) |is.na(m_dem$frelect1) |is.na(m_dem$frelect2) |is.na(m_dem$judcnstr1) |is.na(m_dem$judcnstr2) |is.na(m_dem$legcnstr1) |is.na(m_dem$legcnstr2) |is.na(m_dem$eqlaw1) |is.na(m_dem$eqlaw2), 0, 1)
table(m_dem$opt_outs) 
#Binary variable for opt-out/opt-in. 1 means they put a number value, and 0 means at least one question was black, prefer not to answer, or I don't know

##PERU
summary(p_dem$partout)
p_dem$nontake <-0
p_dem$nontake[p_dem$partout=="0"]<- 1
table(p_dem$nontake)
#Binary variable is now "non-take." 1 means they DID finish the survey
#0 means they didn't. 

# Create a new variable indicating missing responses
p_dem$opt_outs <- ifelse(is.na(p_dem$frexp1) | is.na(p_dem$frexp2) |is.na(p_dem$frassc1) |is.na(p_dem$frassc3) |is.na(p_dem$unisuff1) |is.na(p_dem$unisuff2) |is.na(p_dem$decelec1) |is.na(p_dem$decelec2) |is.na(p_dem$frelect1) |is.na(p_dem$frelect2) |is.na(p_dem$judcnstr1) |is.na(p_dem$judcnstr2) |is.na(p_dem$legcnstr1) |is.na(p_dem$legcnstr2) |is.na(p_dem$eqlaw1) |is.na(p_dem$eqlaw2), 0, 1)
table(p_dem$opt_outs) 
#Binary variable for opt-out/opt-in. 1 means they put a number value, and 0 means at least one question was black, prefer not to answer, or I don't know
