# Analysis of covariance
# Teaching to read example
# Data from Baumann, Seifert-Kessell and Jones (1992)
# Journal of Reading Behavior

## Load packages and set global options
suppressPackageStartupMessages(library(tidyverse))
library(emmeans)
options(contrasts = c("contr.sum", "contr.poly"))

## Load data
data(BSJ92, package = "hecedsm")
# Check whether data are balanced
with(reading, table(group))
# Model the scores on tests 2
model1 <- lm(posttest2 ~ group + pretest2, data = BSJ92)
# Model assumes parallel slopes with equal differences
anova(model1)
# Only look for the difference between groups
model2 <- lm(posttest2 ~ group, data = BSJ92)
anova(model2)
# Compare the mean squared residuals
# and the p-value for group

# Testing model assumptions
# Linearity: are the slopes the same?
model3 <- lm(posttest2 ~ group*pretest2, data = BSJ92)
anova(model1, model3)
# No evidence that the slopes are not parallel

# Test for equality of variance
# For Levene's test, use residuals
car::leveneTest(resid(model1) ~ group,
                data = BSJ92)
model1b <- nlme::gls(posttest2 ~ group + pretest2,
                     data = BSJ92)
model4 <- nlme::gls(posttest2 ~ group + pretest2,
                    data = BSJ92,
                    weights = nlme::varIdent(form =  ~ 1 | group))
anova(model1b, model4)
# Weak evidence of heterogeneity
car::qqPlot(model1, id = FALSE)
# Acceptable: no large deviation from line
# one large residual

# Residual plot: should be mean zero
# Flat line around zero
# Adding local smoothing to see how this unravels

# Create data frame with
# (1) ordinary residuals (residuals)
# (2) fitted values yhat
# (3) externally studentized residuals
model_diag <- data.frame(residuals = resid(model1),
                         fitted = fitted(model1),
                         rstudent = rstudent(model1))
ggplot(data = model_diag,
       aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth() +
  theme_classic()

# Marginal means and planned constrasts

planned_contrasts <-
  list(c1 = c(2, -1, -1),
       c2 = c(0, 1, -1))
contrast(emmeans(model1, specs = "group"),
         method = planned_contrasts,
         p.adjust = "holm")
# Because lines are parallel, the difference between group
# is the same regardless of the score on the pre-test
# The two contrasts are orthogonal so
# use disjoint bits of information
# Thanks to the conditioning

# Cohen's d for the ANCOVA
emmeans::eff_size(contrast(emmeans(model1, specs = "group"),
                           method = planned_contrasts),
                  method = "identity",
                  sigma = sigma(model1),
                  edf = df.residual(model1))
