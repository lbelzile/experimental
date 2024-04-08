# Repeated measures ANOVA as mixed model
library(lmerTest) # Mixed models, using 'lme4'
# Force sum-to-zero constraints globally for linear models
options(contrasts = c("contr.sum", "contr.poly"))

data(HOSM22_E3, package = "hecedsm")
mixmod <- lmer(
  imscore ~ waiting*ratingtype +
    (1 | id), # random intercept per id
  data = HOSM22_E3)
anova(mixmod, type = 2)
# Output includes estimation of variance
# Total variance is 0.8999 + 0.6895, correlation
# of within-subject measurements is 0.8999/(0.8999 + 0.6895)
summary(mixmod)

data(HB22_S5, package = "hecedsm")
# Look at the repartition of the observations
xtabs(~ curstate + predout + tempdist, data = HB22_S5)
# Look at the database
# 'tempdist' is within-subject factors, `curstate` and `predout` are betwen-subject
repmod <- afex::aov_ez(
  id = "id",
  dv = "likelihood",
  between = c("curstate","predout"),
  within = "tempdist",
  data = HB22_S5)
summary(repmod)

mixmod <- lmer(likelihood ~ curstate*predout*tempdist + (1 | id),
     data = HB22_S5)
summary(mixmod)
car::Anova(mixmod, test = "F")
# Is the correlation between observations statistically different from zero?
# The function forces positive correlation between observations
ranova(mixmod)
# Clearly yes!
anova(mixmod, type = "3", ddf = "Kenward")
# The three-way interaction is significant, so we need to fix one variable
# and look at two-way interaction conditional on either, or
# consider simple effect constraining two variables
