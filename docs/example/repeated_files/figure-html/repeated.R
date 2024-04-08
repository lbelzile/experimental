## -----------------------------------------------------------------------------
data(HOSM22_E3, package = "hecedsm")
str(HOSM22_E3)
with(HOSM22_E3, table(waiting)/2)


## -----------------------------------------------------------------------------
options(contrasts = c("contr.sum", "contr.poly"))
fmod <- afex::aov_ez(id = "id", 
                     dv = "imscore", 
                     between = "waiting",
                     within = "ratingtype",
                     data = HOSM22_E3)
anova(fmod)
# MANOVA tests
fmod$Anova


## -----------------------------------------------------------------------------
data(HB22_S5, package = "hecedsm")
xtabs(~ curstate + predout, data = HB22_S5)


## -----------------------------------------------------------------------------
mmod <- afex::aov_ez(
  id = "id",
  dv = "likelihood",
  between = c("curstate","predout"),
  within = "tempdist",
  data = HB22_S5)
summary(mmod)


## -----------------------------------------------------------------------------
library(emmeans)
library(ggplot2)
emm <- emmeans(mmod, 
               specs = c("curstate","predout"),
               by = "tempdist")
emmip(emm, 
      formula = curstate ~ predout | tempdist,
      CIs = TRUE) +
  theme_classic() + 
  theme(legend.position = "bottom") +
  labs(x = "predicted output",
       color = "current state")

