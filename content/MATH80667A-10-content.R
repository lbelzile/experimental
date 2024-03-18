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
model <- lmer(
  guilt ~ anchor*vignette*verdictsyst + prior + (1|id),
  data = C22)
# prior is a covariate (so used to reduce error, plus the slope is of interest on it's own
# Cannot have interaction prior * id, because we get a single prior score per person

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
#################     Example 3 - Chocolat rating!             ###################
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
summary(model)
# Look at best chocolate type overall
(emm <- emmeans(model, specs = "choc"))
emm |> contrast("pairwise")
# C has the highest rating, but indistinguishable from A
# B is worst

# Compare variability
vars <- c(unlist(VarCorr(model)), sigma(model))
# Correlation between same chocolate/rater
sum(vars[1:2])/sum(vars)
# Correlation between measurements from same rater, different chocolates
vars[2]/sum(vars)
