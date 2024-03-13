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
