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

# LEO'S RAMBLING
# Fit a compound symmetry model
# and compare to the output of MANOVA and Mauchly's test
modGLS_CS <- nlme::gls(model = imscore ~ waiting*ratingtype,
          data = HOSM22_E3,
          correlation = nlme::corCompSymm(form = ~ 1 | id))
car::Anova(modGLS, type = 2, test.statistic = "F")
modGLS_UN <- nlme::gls(model = imscore ~ waiting*ratingtype,
                 data = HOSM22_E3,
                 correlation = nlme::corSymm(form = ~ 1 | id),
                 weights = nlme::varIdent(form = ~ 1 | ratingtype))
# Here, there is a single correlation so the difference comes from the change in variance
anova(modGLS_UN, modGLS_CS)
emmeans::emmeans(modGLS_UN, specs = "ratingtype")
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

