## Example of two-way ANOVA
library(emmeans)
# Grossmann and Kross (2014)
data(GK14_S3, package = "hecedsm")
# Modify the data to get two categorical variables
GK14_S3 <- GK14_S3 |>
  tidyr::drop_na() |> # remove rows with missing values
  dplyr::mutate(target = factor(
                  ifelse(condition %in% c("self immersed", "self distanced"),
                         "self",  # if true, assign 'self'
                         "other")),  # else assign 'other
                distance = factor(
                  ifelse(condition %in% c("self immersed", "other immersed"),
                         "immersed",
                         "distanced")))
# Check that we get the same repartition
with(GK14_S3, table(target, distance))
with(GK14_S3, table(condition))
# Fit a linear regression model with 'change' as response variable,
# as a function of four subgroups
# First, we globally force the parametrization to be mean-zero
options(contrasts = c("contr.sum", "contr.poly"))
# The R notation A*B mean A + B + A:B, where the latter denotes interaction
mod1 <- lm(change ~ distance*target, data = GK14_S3)
# For those familiar with linear models, the 'sum to zero' constraint amounts to
# coding baseline with -1, and 1 if you are in level k (sum of effects is zero)
# default comparison uses 0 baseline, 1 if you are in level k (difference to baseline)


# Compute mean of each cell and standard errors
# Differences in SE are due to differences in sample sizes
(mmeans <- emmeans::emmeans(mod1, specs = ~ target * distance))
# Plot means with confidence intervals
plot(mmeans)
# Specify contrasts
cont <- list(other_vs_selfdistanced = c(0.5, -1, 0.5, 0),
             other_vs_selfimmersed  = c(0.5, 0, 0.5, -1),
             otherdistanced_vs_otherimmersed = c(1, 0, -1, 0),
             selfdistanced_vs_selfimmersed = c(0, 1, 0, -1))
# Set up contrasts
mmeans |> emmeans::contrast(cont)

# Compute analysis of variance table
car::Anova(mod1, type = 3) # need type 3 b/c unequal sample size
# Read table bottom to top, only consider the last line for interaction
# p-value is 0.98, so we proceed with marginal effects
emmeans::test(mmeans, joint = TRUE)
# Side remark: the above test compares two models, one with an interaction (alternative)
# and the null model without the interaction
mod0 <- lm(change ~ distance + target, data = GK14_S3)
# Comparison of nested models, there are 1 additional parameter for the interaction
# Four groups, minus 1 global mean - 1 row difference to mean - 1 row difference to column
anova(mod0, mod1)

# Marginal effects
margmeans <- emmeans::emmeans(mod1, specs = ~ distance) # specs indicate factors to keep
# We can ignore the warning here, since interaction not significant
margmeans
# Note that these are averages of both other means, NOT the sample mean of the other/self
# To see this, compare with the summary statistics ignoring distance
GK14_S3 |> dplyr::group_by(distance) |>
  dplyr::summarize(mean_change = mean(change))
# Not the same, because no equiweighting
margmeans |> emmeans::contrast("pairwise")
# Other marginal effect can be obtained directly
emmeans::emmeans(mod1, specs = ~ target, contr = "pairwise")
# use 'contr' to obtain directly contrasts

# Two of the contrasts of interest were simple effects,
# comparing self vs other within each 'target'
emmeans::emmeans(mod1, pairwise ~ distance | target)

############################################################################
################# Example 2 - fake balanced data ###########################
############################################################################
library(ggplot2)
theme_set(theme_classic())
data(teller, package = "hecedsm")
?teller
mod1 <- lm(error ~ nweeks * course, data = teller)

# Interaction plot
emmeans::emmip(mod1, course ~ nweeks, CIs = TRUE)
# Strong evidence of non-parallel lines!
# There is some improvement

car::Anova(mod1, type = 3)
anova(mod1)
# same as "anova(mod1)" for balanced data
# differences are due to rounding when printed

# Clear evidence of interaction, test also suggests there is one

emm1 <- emmeans::emmeans(mod1, ~ nweeks * course)
# Compute all pairwise differences with multiplicity adjustment
emm1 |> emmeans::contrast("pairwise", adjust = "tukey")
# Simple effects - the software won't allow for correction
# as these are done separately for each combo

# At which point are there no more discernable difference
# between training and no training?
emm1 |> emmeans::contrast(method = "pairwise", by = "nweeks")
# No difference from 8 weeks onwards
