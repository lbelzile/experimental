library(emmeans)
library(hecedsm)
library(ggplot2)


## Blocking factor
# Note that this is fundamentally repeated measures
url <- "https://edsm.rbind.io/files/data/resting_metabolic_rate.txt"
# transform integers to factors (categorical)
resting <- read.table(url, header = TRUE) |>
  dplyr::mutate(
    subject = factor(subject), #blocking factor
    protocol = factor(protocol), #experimental factor
    rate = rate/1000)
# Force sum-to-zero parametrization for unordered factors
options(contrasts = c("contr.sum", "contr.poly"))
# Fit model with blocking factor
model_block <- aov(rate ~ subject + protocol, data = resting)
# One-way ANOVA (no blocking)
model_raw <- aov(rate ~ protocol, data = resting)

# ANOVA tables with and without blocking factor
anova(model_block)
anova(model_raw)
# Use ANOVA table to compute etasquared effect size
etasq <- 0.0359/(23.1175 + 1.2355 + 0.0359)
effectsize::eta_squared(model_block, partial = TRUE, generalized = "subject")

# Interaction plot
ggplot(data = resting,
       aes(x = subject,
           y = rate,
           group = protocol,
           color = protocol)) +
  geom_line(linewidth = 1.5) +
  labs(subtitle = "mean resting metabolic rate",
       y = "",
       x = "subject identifier") +
  scale_color_grey()+
  theme_classic() +
  theme(legend.position = "bottom")



##################################################################################
###########          Example 1 - Hosano et al. (2022)                  ###########
###########       Equivalence repeated measures and mixed models       ###########
##################################################################################
data(HOSM22_E3, package = 'hecedsm')
# Pivot from long to wide for MANOVA
HOSM22_E3w <- HOSM22_E3 |> tidyr::pivot_wider(
  names_from = ratingtype,
  values_from = imscore
)

library(afex)
mod <- aov_ez(id = "id",
              dv = "imscore",
              between = "waiting",
              within = "ratingtype",
              data = HOSM22_E3)
xtabs(~ratingtype + waiting, HOSM22_E3) # crossed factors
xtabs(~id + waiting, HOSM22_E3) # id nested in waiting
xtabs(~id + ratingtype, HOSM22_E3)
# Obtain MANOVA table
mod$Anova

# Fit instead the model with MANOVA using multivariate linear regression
manova_mod <- lm(cbind(prediction, experience) ~ waiting,
                 data = HOSM22_E3w)
# For repeated measures, we need to reconstruct the missing factor(s)
# corresponding to the repeated measures via a data frame (idata)
# that contains the factor levels and the variable name
# and idesign that includes the additional factors to our models
car::Anova(manova_mod,
           idata = data.frame(ratingtype = factor(c("prediction","experience"))),
           idesign =~ratingtype, type = 3)

# Fitting the same 2x2 model (with interaction), including a random intercept per subject
library(lmerTest)
mixmod <- lmer(
  imscore ~ waiting*ratingtype + (1 | id),
  data = HOSM22_E3)
# We get the same table if we set type III
anova(mixmod, type = 2)

##################################################################################
#################     Example 2 - Curley et al.               ####################
##################################################################################
data(C22, package = "hecedsm")
head(C22)
options(contrasts = c("contr.sum", "contr.poly"))
# balanced!
xtabs(~ anchor + vignette + verdictsyst, data = C22)
# Incomplete block design: there are two (counterbalanced) combo of anchor, vignette, verdictsyst
# but participants see 2 out of 8 combinations. So no interaction between these and ID is possible.
xtabs(~  id + interaction(anchor, vignette, verdictsyst), data = C22)
model <- lmer(
  guilt ~ anchor*vignette*verdictsyst + pjaq + (1|id),
  data = C22)
# pjaq is a covariate (so used to reduce error, plus the slope is of interest on it's own
# Cannot have interaction pjaq * id, because we get a single pjaq score per person

# No ambiguity for sum of square decomposition
anova(model)
# No three-way interaction
# A two-way interaction between vignette:verdictsyst
library(emmeans)
# Computing differences between anchors
emmeans(model, specs = "anchor") |> pairs()
# Computing differences in verdict separately for each vignette
emmeans(model, specs = "verdictsyst", by = "vignette") |> pairs()

##################################################################################
#################     Example 3 - Chocolate rating!             ##################
##################################################################################
book.url <- "http://stat.ethz.ch/~meier/teaching/book-anova"
chocolate <- read.table(file.path(book.url, "data/chocolate.dat"),
                        header = TRUE)
chocolate[,"rater"]      <- factor(chocolate[,"rater"])
chocolate[,"background"] <- factor(chocolate[,"background"])
str(chocolate)
# Fit the model (note that rater recycles the id 1:10, so we need to be careful here!
chocolate <- chocolate |> dplyr::mutate(id = factor(paste(background, rater)))
# This model is correct
model <- lmer(y ~ background*choc +
                (1 | rater:background) + (1 | rater:choc:background),
              data = chocolate)
# This is fine too (because of the distinct IDs)
model <- lmer(y ~ background*choc +
                (1 | id) + (1 | choc:id),
              data = chocolate)
# This is WRONG (compare degrees of freedom)
# model <- lmer(y ~ background*choc +
#                 (1 | rater) + (1 | choc:rater),
#               data = chocolate)
# Data are again balanced
anova(model)
# There is a no evidence of interaction
# The model output includes variance coefficients
summary(model)
# Look at best chocolate type overall
(emm <- emmeans(model, specs = "choc"))
emm |> contrast("pairwise")
# C has the highest rating, but indistinguishable from A
# B is worst

# Compare variability
vars <- c(unlist(VarCorr(model)), sigma(model)^2)
# Correlation between same chocolate/rater
sum(vars[1:2])/sum(vars)
# Correlation between measurements from same rater, different chocolates
vars[2]/sum(vars)
