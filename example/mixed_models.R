library(lmerTest)
# TV data from Bang & Olufsen
data(TVbo, package = "lmerTest")
# Crossed factors (Picture and TVset)
# 15 response variables (characteristics of product)
# these are nested within assessors (8)
fm <- lmer(Coloursaturation ~ TVset * Picture + 
       (1 | Assessor) + 
       (1 | Assessor:TVset) + 
       (1 | Assessor:Picture), data = TVbo)
ranova(fm)
anova(fm, ddf = "Kenward-Roger")

fm <- aov(Coloursaturation ~ TVset * Picture + Error(Assessor/(Picture*TVset)), data = TVbo)


library(afex) # analysis of factorial experiments
library(emmeans) # estimated marginal means
# Set up sum-to-zero constraint for factors
options(contrasts = c("contr.sum", "contr.poly"))
data(C22, package = "hecedsm") # Load data
str(C22)
# How are conditions manipulated?
with(C22, xtabs(~ anchor + verdictsyst))
# Thus for each individual we see only two pairs of anchor + verdict system
# a so-called incomplete design.
# With only two measurements, no interaction is possible within participants
# Likewise, we cannot have interaction between subject-effect
# and with anchor and verdict system (why?)
model <- mixed(guilt ~ anchor*verdictsyst + 
                 vignette + pjaq + (1 | id),
      data = C22)
model <- lme4::lmer(guilt ~ anchor*verdictsyst + 
                      vignette + pjaq + (1 | id),
                    data = C22)
car::Anova(model, test.statistic = "F")
emmeans(model, spec = "anchor") |> contrast(method = "pairwise")
