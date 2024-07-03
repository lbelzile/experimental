library(WebPower)   # power and sample size calculations
library(effectsize) # calculation of effect sizes from fitted models
library(emmeans) # estimated marginal means

# Power calculation for a one-way ANOVA
# Only value of interest here is the main effect
data(arithmetic, package = "hecedsm")
model <- lm(score ~ group, data = arithmetic)
summary(model)$r.squared
# Extract summary statistics from ANOVA tables (degrees of freedom and F stat)
anova_table <- anova(model)
nobs <- nrow(arithmetic)
nu1 <- 4
ng <- 5
nu2 <- 40
Fstat <- broom::tidy(anova_table)$statistic[1]
# Compute eta_sq and omega_sq using formula
etasq <- Fstat*nu1/(Fstat*nu1 + nu2)
omegasq <- nu1*(Fstat-1)/(nu1*(Fstat-1) + nrow(arithmetic))
# Omega_sq is always smaller - convert to Cohen's f2
f2_om <- omegasq/(1-omegasq)

# Solve equation to get the value of n such that noncentrality parameter
# gives a probability of rejecting close to power
power <- numeric(50)
for(i in seq_along(power)){
   n <- ng + i
  power[n] <- pf(
     q = qf(0.95, df1 = nu1, df2 = n - ng), # cutoff value based on null
     df1 = nu1,
     df2 = n - ng,
     ncp = n*f2_om,
     lower.tail = FALSE)
}
nsamp <- ng + min(which(power > 0.95))
ggplot(data = data.frame(power = power, n = ng + 1:length(power)),
       aes(x = n, y = power)) +
   geom_line() +
   geom_hline(yintercept = c(0.8, 0.9, 0.95), alpha = 0.5, linetype = "dashed") +
   theme_minimal()

# Using software packages
f <- effectsize::cohens_f(model = model, method = "omega", partial = FALSE, squared = FALSE)$Cohens_f
nsamp <- WebPower::wp.anova(k = 5, f = f, power = 0.95)

# Effect size for contrasts
contr <- emmeans::emmeans(model, specs = "group") |>
   emmeans::contrast(list("effect vs control" = c(1/2, 1/2, -1/3, -1/3, -1/3),
                          "praise vs reprove" = c(0, 0, 1, -1, 0)))
# 'emmeans' returns confidence intervals, but they don't account for the
# uncertainty in the estimated variance so are too narrow
eff <- emmeans::eff_size(
   object = emmeans::emmeans(model, specs = "group"),
   edf = n - ng,
   sigma = summary(model)$sigma,
   method = list(
      "effect vs control" = c(1/2, 1/2, -1/3, -1/3, -1/3),
      "praise vs reprove" = c(0,0,1,-1,0)))
# What is the sample needed to replicate the smallest of the two contrast effects?
Cohen_d <- min(abs(summary(eff)$effect.size))
WebPower::wp.kanova(ndf = 1, ng = 5, f = Cohen_d/2, power = 0.95)

# Two-way ANOVA model with two factors, unbalanced
data("JZBJG22_E2", package = "hecedsm")
xtabs(~ condition + order, data = JZBJG22_E2)
model <- lm(conf_dying ~ condition * order, data = JZBJG22_E2)


# Compute effect sizes, these are tiny - essentially no effect
effectsize::omega_squared(model, partial = TRUE)
# You can compare the residual sum of square with that of the effect
car::Anova(model, type = 2)
# Effect sizes converted to Cohen's f
eff <- effectsize::cohens_f(model,
                     partial = TRUE,
                     squared = FALSE,
                     method = "omega")
# Sample size for the largest effect
WebPower::wp.kanova(ndf = 1, f = eff$Cohens_f_partial[1], ng = 4, power = 0.8)

