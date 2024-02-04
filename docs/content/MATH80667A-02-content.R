
library(hecedsm)
library(dplyr) # data manipulation
library(knitr) # data formatting
library(ggplot2) # grammar of graphics
library(emmeans) # marginal means and contrasts

# Load arithmetic data
data(arithmetic, package = "hecedsm")
# categorical variable = factor
str(arithmetic) # Look up data

# Compute summary statistics
summary_stat <-
arithmetic |>
  group_by(group) |>
  summarize(mean = mean(score),
            sd = sd(score))
knitr::kable(summary_stat,
             digits = 2)
# Boxplot with jittered data
ggplot(data = arithmetic,
       aes(x = group,
           y = score)) +
  geom_boxplot() + # box and whiskers plot
  # scatterplot, with horizontal jittering
  geom_jitter(width = 0.3,
              height = 0) +
  theme_bw()

# Analysis of variance model
model <- aov(score ~ group, data = arithmetic)
# Compute mean for each group with pooled standard error
margmeans <- emmeans(model, specs = "group")
# Compute pairwise differences
contrast(margmeans,
         method = "pairwise",
         adjust = 'none',
         infer = TRUE) |>
  as_tibble() |> # transform to data frame
  filter(contrast == "praise - reprove") |>
  # extract the sole pairwise difference
  knitr::kable(digits = 3)
# Manual calculation of p-value
2*pt(q = 2.467, df = 45 - 5, lower.tail = FALSE)


## Three examples discussed in the course notes and
## in class

## Example 1 - Rosen and Jerdee (1974)
# Pearson chi-square test
RJtab <- matrix(c(32L,12L,19L,30L), nrow = 2, ncol = 2)
chisq.test(RJ,
           correct = FALSE)

# We can get this test from a Poisson regression model
Rosen_Jerdee74 <- data.frame(
  action = factor(x = rep(c("promote", "hold file"),
                          length.out = 4L)),
  sex = factor(x = rep(c("male", "female"), each = 2)),
  n = c(32L,12L,19L,30L))
# Fit the saturated model with a mean for each subcategory
full <- glm(data = Rosen_Jerdee74,
    n ~ action*sex,
    family = poisson)
# Compute score test for removing the interaction
anova(full, test = "Rao")

# Second test
fisher.test(RJtab,
            conf.int = FALSE)
library(infer)
# Calculate the odds ratio for the sample
obs_stat <- dat_exper1_long %>%
  specify(response = action, explanatory = sex, success = "promote") %>%
  calculate(stat = "odds ratio", order = c("male", "female"))
# Approximate the null distribution using a permutation test
set.seed(2021) # set random seed
null_dist <- dat_exper1_long %>%
  specify(response = action, explanatory = sex, success = "promote") %>%
  hypothesize(null = "independence") %>% # sex doesn't impact decision
  generate(reps = 9999, type = "permute") %>% # shuffle sex
  calculate(stat = "odds ratio", order = c("male", "female"))
# Visualize the null distribution
ggplot(data = null_dist, # a tibble with a single variable, 'stat'
       mapping = aes(x = stat)) + # map 'stat' to the x-axis
  geom_bar() + # bar plot b/c data are discrete (few combinations)
  labs(x = "odds ratio") + # give meaningful label
  geom_vline(data = obs_stat, # add vertical line
             mapping = aes(xintercept = stat), # position on x-axis of line
             color = "red")  +
  theme_classic() # color
# Obtain the p-value
pval <- null_dist %>%
  get_p_value(obs_stat = obs_stat,
              direction = "two-sided")


## Example 2 Liu et al.
##
data(LRMM23_S1, package = 'hecedsm')
# Summary statistics per gender and role
tab1 <- LRMM23_S1 |>
  dplyr::group_by(gender) |>
  dplyr::summarize(min = min(age),
                   max = max(age),
                   mean = mean(age),
                   n = dplyr::n())
tab2 <- LRMM23_S1  |>
  dplyr::group_by(role) |>
  dplyr::summarize(mean = mean(appreciation),
                   sd = sd(appreciation),
                   n = dplyr::n())
# Welch t-test
ttest <- t.test(appreciation ~ role,
                data = LRMM23_S1,
                var.equal = FALSE)
# Standardize output in a table
broom::tidy(ttest)
# Two-sample t-test
t.test(appreciation ~ role,
       data = LRMM23_S1, var.equal = TRUE) |>
  broom::tidy()
# Wilcoxon signed rank test
coin::wilcox_test(appreciation ~ role,
                  data = LRMM23_S1) |>
  broom::tidy()

## Example 3
data(BL22_L, package = "hecedsm")
# Negative binomial regression
negbin <- MASS::glm.nb(ncreative ~ cond,
                       data = BL22_L)
# Obtain likelihood ratio test
car::Anova(negbin, type = 3) |>
  broom::tidy()
# A different test, not as appropriate for count data
t.test(ncreative ~ cond,
       data = BL22_L, var.equal = FALSE) |>
  broom::tidy()
