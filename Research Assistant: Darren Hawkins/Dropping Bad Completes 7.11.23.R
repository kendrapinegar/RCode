###Code to drop Bad Completes
###Last Updated: August 10, 2023


###CODING TO DROP THE BAD COMPLETES
#1. They offer a gibberish answer to the open-ended response question. This is defined as words that make no sense.
#2. Those who spend less than 4 minutes on the survey, if control, and less than 5 minutes, if treatment 
#3. Those who put the same answer on all 17 democracy outcome questions, or all the same answer on 16 of the 17 questions. 
#4. Those who choose one of the two “no answer” options in the democracy questions at least 75 percent of the time. 

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