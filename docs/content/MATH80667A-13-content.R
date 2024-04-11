library(coin)
library(dplyr)

###################################################
## Example 1 - Duke and Amir (2023), experiment 2
###################################################
data(DA23_E2, package = "hecedsm")
tabs <- with(DA23_E2, table(purchased, format))
# Chi-square test for independence
chisq <- chisq.test(tabs)

###################################################
## Example 2 - Elliot (2021) multilab
###################################################

data(MULTI21_D1, package = "hecedsm")
contingency <- xtabs( #pool data
  count ~ age + frequency,
  data = MULTI21_D1)
# No correction to get same result as Poisson regression model
chisqtest <- chisq.test(contingency, correct = FALSE)

# Two-way table with (I,J) categories
# Here, I=4 (age group) and J=3 (frequency)
MULTI21_D1_long <- MULTI21_D1 |> # pool data by age freq
  dplyr::group_by(age, frequency) |>
  dplyr::summarize(total = sum(count)) # aggregate counts

# Fit Poisson regression
mod_main <- glm(total ~ age + frequency, # null model, no interaction
    family = poisson, data = MULTI21_D1_long)
mod_satur <- glm(total ~ age * frequency, # saturated model
    family = poisson, data = MULTI21_D1_long)
# The saturated model returns the observed counts
isTRUE(all.equal(
    target = predict(mod_satur, type = "response"),
    current = MULTI21_D1_long$total,
    check.attributes = FALSE))

# Likelihood ratio test and Pearson chi-square
anova(mod_main, mod_satur, test = "LRT")  # deviance
anova(mod_main, mod_satur, test = "Rao")  # score test

# We can also compute them manually without fitting the saturated model
# The Pearson chi-square stat is the sum of squared Pearson residuals
PearsonX2 <- sum(resid(mod_main, type = "pearson")^2)
pchisq(q = PearsonX2, df = mod_main$df.residual, lower.tail = FALSE)
# The likelihood ratio test is obtained from the deviance
pchisq(q = deviance(mod_main),
       df = mod_main$df.residual, lower.tail = FALSE)

###################################################
## Example 3 - Bertrand and Mullainathan (2004)
###################################################

data(BM04_T2, package = "hecedsm")
# Symmetric model with 6 parameters (3 diag + 3 upper triangular)
mod_null <- glm(count ~ gnm::Symm(black, white),
                data = BM04_T2,
                family = poisson)
# Compare the two nested models using a likelihood ratio test
pchisq(deviance(mod_null), lower.tail = FALSE,
       df = mod_null$df.residual) # 9 cells - 6 parameters = 3
PearsonX2 <- sum(residuals(mod_null, type = "pearson")^2)
pchisq(PearsonX2, df = mod_null$df.residual, lower.tail = FALSE)


###################################################
## Example 1 - Brucks and Levav (2022)
###################################################


data(BL22_E, package = "hecedsm")
# Two-sample comparison using Mann-Whitney or Wilcoxon rank-sum test
mww <- coin::wilcox_test(
  partner_time ~ cond,
  data = BL22_E,
  conf.int = TRUE)
# The point estimate is Welch's average
# Values and bounds for confidence intervals are times in seconds

# Compare results with two sample t-test
welch <- t.test(partner_time ~ cond,
  data = BL22_E,
  conf.int = TRUE)


###################################################
## Example 2 - Brodeur et al. (2021)
###################################################
data(BRLS21_T3, package = "hecedsm")
# Friedman test (more popular, but less powerful than Quade)
friedman <- coin::friedman_test(
  nviolation ~ task | id,
  data = BRLS21_T3)
# Quade test
quade <- coin::quade_test(
  nviolation ~ task | id,
  data = BRLS21_T3)
# Effect size for rank-based tests
eff_size <- effectsize::kendalls_w(
  x = "nviolation",
  groups = "task",
  blocks = "id",
  data = BRLS21_T3)

# Compute ranks separately for each person
BRLS21_T3_rank <- BRLS21_T3 |>
  group_by(id) |>
  mutate(rank = rank(nviolation)) |>
  ungroup()
# Which violation type has the highest rank?
BRLS21_T3_rank |>
  group_by(task) |>
  summarize(mrank = mean(rank))
# So texting leads to more violations


# Transform to wide format - one line per id
smartwatch <- tidyr::pivot_wider(
  data = BRLS21_T3,
  names_from = task,
  values_from = nviolation)

# Wilcoxon signed-rank test for all pairwise differences
tests <- list(
  wilcoxsign_test(phone ~ watch,
                  data = smartwatch),
  wilcoxsign_test(speaker ~ watch,
                  data = smartwatch),
  wilcoxsign_test(texting ~ watch,
                  data = smartwatch),
  wilcoxsign_test(phone ~ speaker,
                  data = smartwatch),
  wilcoxsign_test(phone ~ texting,
                  data = smartwatch),
  wilcoxsign_test(texting ~ speaker,
                  data = smartwatch))
# Extract p-values of tests
pvals <- sapply(tests, pvalue)
# Adjust for multiple testing using Holm-Bonferroni
p.adjust(pvals, method = "holm")
# Texting is the only significantly different pair (more distracting than alternatives)
