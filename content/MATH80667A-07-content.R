#### MULTIWAY ANOVA  ####


library(ggplot2) # grammar of graphics
library(dplyr) # data manipulation
library(car) # regresion tools
library(emmeans) # estimated marginal means
# Load data
data(popcorn, package = 'hecedsm')
# Fit model with three-way interaction
model <- aov(percentage ~ brand*power*time,
             data = popcorn)
# ANOVA table - 'anova' function is ONLY for balanced designs
anova_table <- anova(model)
car::Anova(model, type = 3)
# Because of balancing, can analyze all effects
# No three-way interaction
# Only two-way interaction is brand*time combo (and weak evidence)

# Quantile-quantile plot
car::qqPlot(model)
# Interaction plot
popcorn |>
  group_by(brand, time, power) |>
  summarize(meanp = mean(percentage)) |>
  ggplot(mapping = aes(x = power,
                       y = meanp,
                       col = time,
                       group = time)) +
  geom_line() +
  facet_wrap(~brand)

## Questions
# Q1: Which combo (brand, power, time) gives highest popping rate? (pairwise comparisons of all combos)
cellmeans <- emmeans(model, specs = c("brand","power","time"))
# There are 153 pairs!
choose(n = 18, k =  2)
pairs(cellmeans)
# Q2: Best brand overall (marginal means marginalizing over power and time, assuming no interaction)
margmean_bt <- emmeans(model, specs = c("brand","time"))

emmeans(model, specs = "brand") |>
  contrast(list(c(0.5,0.5,-1)))
margmean_bt |>
  contrast(list(natvslocal = c(0.5,0.5,-1,0.5,0.5,-1,0.5,0.5,-1)/3))

contr <- list(contrast4vs4p5 = c(0.5,0.5,-1, -0.5, -0.5, 1, 0,0,0),
     contrast4vs5 = c(0.5,0.5,-1, 0,0,0, -0.5, -0.5, 1),
     contrast4.5vs5 = c(0,0,0, 0.5,0.5,-1,  -0.5, -0.5, 1))
margmean_bt |> contrast(contr) |> test(joint = TRUE)
# National 1 has the highest average, but not significantly difference from others
# The main culprit is the high standard error, due to r=2 replications.
# We only have 12 observations per group, but standard dev has 18 degrees of freedom.

# Q3: Effect of time and power on percentage of popped kernels
# pairwise comparison of time x power
emmeans(model, specs = c("time", "power")) |> # specs is what we keep.
  # 4m30s 600W is highest mean
  pairs()


# Q4: main effect of power
emmeans(model, specs = "power") |>
  pairs()  # shortcut for contrast("pairwise")
# Note how this differs from the ANOVA table (because of averaging)
# The reason for this discrepancy is that we compare different models!


# Q5: main effect of time
# Rather than global, we show how to look at this separately for each brand
emmeans(model, specs = "time", by = "brand") |>
  pairs() |>
  test(joint = TRUE)
# Q6: Interaction contrasts for two-way ANOVA
# Compare difference between percentage of popped kernels
#  for 4.5 versus 5 minutes, for brands 1 and 2
emm_popcorn_AC <- emmeans(model,
                          specs = c("brand","time"))
contrast_list <- list(
    brand12with4.5vs5min = c(0, 0, 0, 1, -1, 0, -1, 1,0))
contrast(emm_popcorn_AC,  # marginal mean (no time)
         method = contrast_list) # list of contrasts


#### MANOVA ####

library(hecedsm)
xtabs(~stimulus + id, data = AA21)
# Set sum-to-zero constraint for factors
options(contrasts = c("contr.sum", "contr.poly"))
data(AA21, package = "hecedsm")
# Compute mean
AA21_m <- AA21 |>
  dplyr::group_by(id, stimulus) |>
  dplyr::summarize(latency = mean(latency))
anova(lm(data = AA21_m, latency ~ id + stimulus))

# Compute analysis of variance - here treated as blocking factor
mod <- aov(latency ~ stimulus + Error(id/stimulus), data = AA21_m)
# Same with afex package
model <- afex::aov_ez(
  id = "id",           # subject id
  dv = "latency",      # response variable
  within = "stimulus", # within-subject factor
  data = hecedsm::AA21,
  fun_aggregate = mean) # How to average to get a single measurement
anova(model, # mixed ANOVA model
      correction = "none", # sphericity correction
      es = "none") # effect size
summary(model) # Prints the ANOVA table
model$Anova # Prints the result of the MANOVA test


## Multivariate analysis of variance for credit rating data
# Bind columns (cbind) for multiple responses on the left of the tilde sign ~
# See the full vignette at https://lbelzile.github.io/experimental/example/manova.html
data(AVC02, package = "hecedsm")

# Results for the test of equality of means
model <- manova(cbind(prime, debt, profitability) ~ format, data = AVC02)
summary(model, test = "Wilks") # change test to get different statistics
summary(model) # change test to get different statistics
car::Manova(model) #MANOVA table (as above, with Pillai's trace as default stat)
# Results for univariate analysis of variance (as follow-up)
summary.aov(model)
# Compute marginal means - for each subcombination of response and format
# Note that the variable is named by default 'rep.meas' for MANOVA models
emmeans::emmeans(model, specs = c("format", "rep.meas"))
