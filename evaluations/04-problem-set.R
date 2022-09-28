## Helper code for Problem set 4
# Install packages
# 'dplyr', 'broom', 'emmeans' and 'hecedsm'

library(hecedsm)
library(dplyr)
data("GK14_S3", package = "hecedsm")
?GK14_S3

alpha <- 0.05 # unadjusted level
m <- 32 # size of family

# Fit ANOVA model
# You will need to assign
mod_old_limits <-
  aov(formula = limits ~ condition,
     data = GK14_S3,
     subset = (age == "old"))
# Specify the model with pairwise differences
mod_emm_old_limits <-
  emmeans::emmeans(mod_old_limits,
                   specs = 'condition')
# The same thing for limits, this time with young people
mod_young_limits <-
  aov(formula = limits ~ condition,
     data = GK14_S3,
     subset = (age == "young"))
mod_emm_young_limits <-
  emmeans::emmeans(mod_young_limits,
                   specs = 'condition')

# TODO repeat this for the 6 other models
# with other response variables (replace 'limits' with 'compr', etc.) -
# and the other age category (replace "old" by "young")
# Check that the naming convention matches that below

# Check order to specify your contrasts correctly
# Print the order of the factor levels
levels(GK14_S3$condition)

contrast_specif <- list(
  selfdistvsselfimmersed = c(-1, 1, 0, 0)
  # TODO Add the other three contrasts here
  # See slides for examples
  # Take 'other' as the average of the two
)
# The contrasts are the same for each response and subgroup,
# so the above block of code only appears once
# no need to respecify 'contrast_specif'


# Compute the contrasts
# TODO use the naming convention to match
# names in allcontrasts below
# repeat this for the 7 others
contrasts_res_old_limits <-
  emmeans::contrast(object = mod_emm_old_limits,
                    method = contrast_specif,
                    adjust = "none")
# By default there is no adjustment for custom contrasts
# Since the tests are based on different data (subsamples)
# and responses, we cannot use ANOVA-specific methods like
# Scheffé correction here...

# In the following code, we create data tables with all the
# results and stack them on top of one another
# The result is a data frame with 32 rows
# Once you have defined each model and contrasts
# pool all tests together
# TODO uncomment lines below after having defined the objects
allcontrasts <- rbind(
  # broom::tidy(contrasts_res_old_limits),
  # broom::tidy(contrasts_res_old_compr),
  # broom::tidy(contrasts_res_old_persp),
  # broom::tidy(contrasts_res_old_change),
  # broom::tidy(contrasts_res_young_limits),
  # broom::tidy(contrasts_res_young_compr),
  # broom::tidy(contrasts_res_young_persp),
  # broom::tidy(contrasts_res_young_change)
)

# Compare confidence intervals
# with and without Bonferroni adjustment
# Compute unadjusted confidence intervals
allcontrasts <- allcontrasts |>
  dplyr::mutate(lower = estimate + std.error*qt(p = alpha/2, df = df),
                upper = estimate + std.error*qt(p = 1-alpha/2, df = df))
# TODO change the above by putting alphastar instead, where alphastar = alpha/m



# Consider multiplicity adjustment
# See ?p.adjust
pval_raw <- allcontrasts$p.value
pval_holmbonf <- p.adjust(p = allcontrasts$p.value, method = "holm", n = m)
pval_bonf <- p.adjust(p = allcontrasts$p.value, method = "bonferroni", n = m)


# TODO Report the number of tests
# for which we get a rejection of H0
sum(pval_raw <= alpha)
# Repeat this with the adjusted p-values with Holm-Bonferroni and Bonferroni

