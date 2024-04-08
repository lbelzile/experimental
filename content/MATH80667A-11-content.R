# Download the PROCESS macro from the website
# (cannot be shared/distributed)
url <- "https://www.afhayes.com/public/processv43.zip"
temp <- tempfile()
download.file(url, temp)
# Source set
source(unz(temp, "PROCESS v4.3 for R/process.R"))
# Remove temp directory
unlink(temp)
library(ggplot2)
library(emmeans)
library(mediation)

# Example 1: mediation
data(L22_E4, package = "hecedsm")
L22_E4 <- L22_E4 |> # Create a binary 0/1
  dplyr::mutate(socialbin = as.integer(I(social != "alone")))
# This is a 2x3 mixed ANOVA (score: cutfreq/cutintensity as within-factors, social as between)


# Model with social behaviour as binary (alone or not) versus three categories
mod1 <- lm(data = L22_E4, cutfreq ~ socialbin + enjoyother + enjoyamount)
mod2 <- lm(data = L22_E4, cutfreq ~ social + enjoyother + enjoyamount)
anova(mod1, mod2) # No evidence we can't collapse categories

mod1 <- lm(data = L22_E4, cutintensity ~ socialbin + enjoyother + enjoyamount)
mod2 <- lm(data = L22_E4, cutintensity ~ social + enjoyother + enjoyamount)
anova(mod1, mod2) # idem

# Function to calculate Sobel's statistic for binary treatment
# With social, we would have to look at pairwise differences as effects
sobel <- function(alpha, gamma, alpha_var, gamma_var){
  # Compute point estimate (product of alpha and gamma)
  pe_sob <- as.numeric(alpha*gamma)
  # Obtain standard error of alpha_hat*gamma_hat
  se_sob <- as.numeric(sqrt(gamma^2 * alpha_var + alpha^2 * gamma_var  + gamma_var*alpha_var))
  # Wald statistic
  stat <- pe_sob/se_sob
  data.frame(estimate = pe_sob,
             se = se_sob,
             stat = stat, # Sobel test statistic
             pvalue = 2*pnorm(abs(stat), lower.tail = FALSE), # two-sided p-value
             lowerCI = pe_sob + qnorm(p = 0.025)*se_sob,
             upperCI = pe_sob + qnorm(p = 0.975)*se_sob
  )
}

# Create a function that returns the statistic
stat <- function(data, peOnly = TRUE){
  # Fit response models
  modResp1 <- lm(data = data,
                 cutfreq ~ socialbin + enjoyother + enjoyamount)
  modResp2 <- lm(data = data,
                 cutintensity ~ socialbin + enjoyother + enjoyamount)
  # There are no mention of confounders...
  #  and the mediators are not experimentally manipulated
  modMediator1 <- lm(enjoyamount ~ socialbin, data = data)
  modMediator2 <- lm(enjoyother ~ socialbin, data = data)
  # Compute Sobel's statistic
  S11 <- sobel(alpha = coef(modMediator1)['socialbin'],
               alpha_var = vcov(modMediator1)['socialbin','socialbin'],
               gamma = coef(modResp1)['enjoyamount'],
               gamma_var = vcov(modResp1)['enjoyamount','enjoyamount'])
  S12 <- sobel(alpha = coef(modMediator2)['socialbin'],
               alpha_var = vcov(modMediator2)['socialbin','socialbin'],
               gamma = coef(modResp1)['enjoyother'],
               gamma_var = vcov(modResp1)['enjoyother','enjoyother'])
  S21 <- sobel(alpha = coef(modMediator1)['socialbin'],
               alpha_var = vcov(modMediator1)['socialbin','socialbin'],
               gamma = coef(modResp2)['enjoyamount'],
               gamma_var = vcov(modResp2)['enjoyamount','enjoyamount'])
  S22 <- sobel(alpha = coef(modMediator2)['socialbin'],
               alpha_var = vcov(modMediator2)['socialbin','socialbin'],
               gamma = coef(modResp2)['enjoyother'],
               gamma_var = vcov(modResp2)['enjoyother','enjoyother'])
  if(!isTRUE(peOnly)){
    return(rbind(S11, S12, S21, S22))
  } else{
    return(as.vector(rbind(S11, S12, S21, S22)[,'estimate']))
  }
}

# Values for the paper - using large-sample approximation
stat(data = L22_E4, peOnly = FALSE)

# Using the PROCESS macro
process(
  data = L22_E4, # dataset
  y = 'cutfreq', # response variable
  x = 'socialbin', # experimental factor
  m = c('enjoyamount','enjoyother'), # mediator
  boot = 1e4L, # number of bootstrap replications
  model = 4, # parallel mediation
  seed = 80667,
  total = TRUE, # Also fit model with only X -> Y
  normal = TRUE)  # Return large-sample approximation
# Sobel test stat and results are reported in "Indirect effects"


modResp1 <- lm(data = L22_E4,
               cutfreq ~ socialbin + enjoyother + enjoyamount)
modMediator1 <- lm(enjoyamount ~ socialbin, data = L22_E4)

# Using the 'mediation' package
# to get the same syntax as PROCESS, use the 'bruceR' package
med11 <- mediation::mediate(
  model.m = modMediator1, # mediator model
  model.y = modResp1, # response model
  sims = 1e4L, # number of bootstrap simulations
  boot = TRUE, # whether to use bootstrap or not
  treat = "socialbin", # name of treatment variable
  mediator = "enjoyamount") # name of mediator
summary(med11)
# Sensitivity diagnostics - effect would vanish if there was a confounder giving 20% correlation
sensitivity <- mediation::medsens(med11)
summary(sensitivity)
# Plot the robustness check
plot(sensitivity)



# Manual coding (because it's dead simple!)

#' Bootstrap estimation of statistic
#' @param data data frame with observations
#' @param statfn function that returns a vectir of summary statistics
#' @param B number of bootstrap replications
#' @param alpha level of the test
#' @return a data frame with three columns: the bootstrap p-value \code{pval} and the \eqn{1-\alpha}
#' percentile confidence interval (\code{lower} and \code{upper} bounds)
bootFn <- function(data, statfn, B = 1e4L, alpha = 0.05){
  # Sample observations with replacement
  stopifnot(alpha < 0.5, alpha > 0, length(alpha) == 1L, B > 20L, length(B) == 1L)
  B <- as.integer(B)
  stat_original <- statfn(data)
  n <- nrow(data)
  results <- matrix(nrow = B, ncol = length(stat_original))
  # Warning: somewhat computationally intensive
  for(b in seq_len(B-1)){
    # Recompute the statistic with bootstrap sample (sampling with replacement)
    results[b,] <- statfn(data[sample.int(n, n, replace = TRUE),])
  }
  # Include the original data among the cases
  results[B,] <- stat_original
  # Apply will compute this separately for each column - postulated value is zero, so this is what we used below
  M <- apply(results, 2, function(x){sum(x < 0)})
  pval <- 2*pmin(M/B, 1-M/B)
  # Percentile intervals
  confints <- apply(results, 2, function(x){quantile(x, probs = c(alpha/2, 1-alpha/2))})
  # alternatively, just use the 'quantile' function
  data.frame(pval = pval, lower = confints[1,], upper = confints[2,])
}

boot <- bootFn(data = L22_E4, statfn = stat)
boot

#########################################################################
# Example 2 - moderation and mediated moderation

data(GSBE10, package = "hecedsm")
lin_moder <- lm(respeval ~ protest*sexism,
                data = GSBE10)
summary(lin_moder) # coefficients
car::Anova(lin_moder, type = 3)


ggplot(data = GSBE10,
       aes(x = sexism,
           y = respeval,
           color = protest,
           group = protest,
           fill = protest)) +
  geom_point() +
  geom_smooth(se = TRUE, method = "lm", formula = y ~ x) +
  labs(subtitle = "evaluation of response",
       y = "",
       color = "experimental condition") +
  theme_classic() +
  theme(legend.position = "bottom")

quart <-  quantile(
  GSBE10$sexism,
  probs = c(0.25, 0.5, 0.75))
emm <- emmeans(lin_moder,
        specs = "protest",
        by = "sexism",
        at = list("sexism" = quart))
emm
emm |> contrast("pairwise")


lin_moder2 <- lm(
  respeval ~ protest*sexism,
  data = GSBE10 |>
    # We dichotomize the manipulation, pooling protests together
    dplyr::mutate(protest = as.integer(protest != "no protest")))
# Test for equality of slopes/intercept for two protest groups
anova(lin_moder2, lin_moder)
# p-value of 0.18: fail to reject individual = collective.

# Johnson-Neyman plot
jn <- interactions::johnson_neyman(
  model = lin_moder2, # linear model
  pred = protest, # binary experimental factor
  modx = sexism, # moderator
  control.fdr = TRUE, # control for false discovery rate
  mod.range = range(GSBE10$sexism)) # range of values for sexism
jn$plot

# Using the process macro
process(data = GSBE10 |>
          dplyr::mutate(protestind = as.integer(protest)),
        y = "respeval",  # response variable
        w = "sexism", # postulated moderator (continuous)
        x = "protestind", # experimental factor
        model = 1, # number of model in Hayes (simple mediation)
        plot = TRUE, # add plot
        moments = TRUE, # probe at mean +/- std. error;
        # for different values, use argument "wmodval"
        jn = TRUE)


# Example of mediated moderation
# Model 8 assumes that W impacts both
# X -> M, so this model now reads M ~ X + W + X*W
# and X -> Y, so the model Y ~ X + W + X*W + M
process(data = GSBE10 |>
          dplyr::mutate(protestind = as.integer(protest)),
        y = 'likeability', # response
        x = 'protestind', # experimental factor
        w = 'sexism', # moderator
        m = 'respeval',
        model = 8, # interaction between W and X for both model for M and Y
        moments = TRUE, # probe average at mean +/- 1 std error
        boot = 1e4L,
        seed = 80667)
# Since the postulated mediator 'm=respeval' is continuous, we get the effect of 'x'
# (and thus the mediation effect) depends on the value of 'sexism' ('w')
# To get the average effect, we would need to marginalize (i.e., average over) sexism


# Third example, with PROCESS macro, of moderation analysis
# This time, the factor of interest is continuous
# and the moderator is categorical (M=3)
data(LWSH23_S3, package = "hecedsm")
mod <- lm(data = LWSH23_S3, needsatis ~ needclosure * cond)
anova(mod)
emm <- emmeans(mod,
               specs = "needclosure",
               by = "cond")
# 4.75 is mean of need for closure

# Process only understand numeric values for factors...
process(data = LWSH23_S3 |>
          dplyr::mutate(condind = as.integer(cond)),
        y = "needsatis",  # response variable
        x = "needclosure", # explanatory variable (not manipulated)
        w = "condind", # postulated moderator
        mcw = 1, # dummy coding for moderator w (so compare to base level, here 'included')
        model = 1, # number of model in Hayes (simple)
        plot = TRUE) # add plot, doesn't seem to work...
