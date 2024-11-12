m_dem <- read_excel("MX_Data_Excluding_Bad_Completes_FA 7.26.23.xlsx")

m_dem <- m_dem %>%
  filter(!is.na(frexp1)) %>%
  filter(!is.na(frexp2_recode)) %>%
  filter(!is.na(frassc1_recode)) %>%
  filter(!is.na(frassc2)) %>%
  filter(!is.na(frassc3_recode)) %>%
  filter(!is.na(unisuff1_recode)) %>%
  filter(!is.na(unisuff2)) %>%
  filter(!is.na(frelect1)) %>%
  filter(!is.na(frelect2_recode)) %>%
  filter(!is.na(decelec1_recode)) %>%
  filter(!is.na(decelec2)) %>%
  filter(!is.na(legcnstr1_recode)) %>%
  filter(!is.na(legcnstr2)) %>%
  filter(!is.na(judcnstr1)) %>%
  filter(!is.na(judcnstr2_recode)) %>%
  filter(!is.na(eqlaw1_recode)) %>%
  filter(!is.na(eqlaw2))

var(m_dem[, 29:45], na.rm = TRUE)
corrplot(cor(m_dem[,29:45]), method = "ellipse")
fa1 <- factanal(m_dem[,29:45], factors = 4, scores = "regression")
print(fa1, digits = 2, cutoff = .25, sort = TRUE)
fa1$scores
fa1$loadings

fa2 <- factanal(m_dem[,29:45], factors = 5)
print(fa2, digits = 2, cutoff = .5, sort = TRUE)

ev <- eigen(cor(m_dem[,29:45]))
ap <- parallel(subject=nrow(m_dem[,29:45]), var=ncol(m_dem[,29:45]), rep=100, cent=0.5)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


load <- fa1$loadings[,1:2]
plot(load, type="n")
text(load, labels=names(m_dem[,29:45]), cex=0.7)


