# Download the PROCESS macro from the website
# (cannot be shared/distributed)
url <- "https://www.afhayes.com/public/processv43.zip"
temp <- tempfile()
download.file(url, temp)
# Source set
source(unz(temp, "PROCESS v4.3 for R/process.R"))
# Remove temp directory
unlink(temp)

set.seed(80667) # fix seed to ensure reproducibility


library(ggplot2)
library(emmeans)
library(mediation)

# Example 1: mediation
data(L22_E4, package = "hecedsm")
L22_E4 <- L22_E4 |>
  # dichotomize social
  dplyr::mutate(socialbin = as.integer(I(social != "alone")))

# Compare response model with collapsed social categories (3 vs 2)
L22_E4_long <- L22_E4 |>
  dplyr::mutate(id = dplyr::row_number()) |> # add unique identifier
  tidyr::pivot_longer(cols = c(cutfreq, cutintensity),
                       values_to = "cut",
                       names_prefix = "cut",
                       names_to = "type")
# Model fits suggest there is zero correlation between observations (or negative correlation)
# Since we compare different models for the mean, the models must be fitted via maximum likelihood
modY1 <- nlme::gls(cut ~ type*social,
                   correlation = nlme::corCompSymm(form = ~ id),
                   data = L22_E4_long,
                   method = "ML")
modY0 <- nlme::gls(cut ~ type*socialbin,
                   correlation = nlme::corCompSymm(form = ~ id),
                   data = L22_E4_long,
                   method = "ML")
# Comparing the two nested models
anova(modY1, modY0)
# Alternatively, run two regressions
anova(lm(data = L22_E4, cutfreq ~ socialbin + enjoyother + enjoyamount),
      lm(data = L22_E4, cutfreq ~ social + enjoyother + enjoyamount))
anova(lm(data = L22_E4, cutintensity ~ socialbin + enjoyother + enjoyamount),
      lm(data = L22_E4, cutintensity ~ social + enjoyother + enjoyamount))



# Function to calculate Sobel's statistic for binary treatment
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
  m = c('enjoyamount','enjoyother'), # mediators
  boot = 1e4L, # number of bootstrap replications
  model = 4, # parallel mediation
  seed = 80667,
  total = TRUE, # Also fit model with only X -> Y
  normal = TRUE)  # Return large-sample approximation
# Sobel test stat and results are reported in "Indirect effects"


B <- 1000
n <- nrow(L22_E4)
results <- matrix(nrow = B, ncol = 4)
for(i in seq_len(B)){
  # Sample row indices with replacement at random uniformly
  bootdata <- L22_E4[sample.int(n, n, replace = TRUE),]
  # Compute the test statistic on the bootstrap sample
  results[i, ] <- stat(data = bootdata, peOnly = TRUE)
}
# Confidence intervals
apply(results, 2,  quantile, prob = c(0.025, 0.975))
# Bootstrap p-value for test alpha*gamma=0
M <- apply(results, 2, function(x){ sum(x < 0) })
2*min(c(M/B, 1-M/B))

# Using the 'mediation' package
# to get the same syntax as PROCESS, use the 'bruceR' package
#
# 'mediation' is more general in that you can decide what is in the model
modResp1 <- lm(data = L22_E4,
               cutfreq ~ socialbin + enjoyother + enjoyamount)
modMediator1 <- lm(enjoyamount ~ socialbin, data = L22_E4)

med11 <- mediation::mediate(
  model.m = modMediator1, # mediator model
  model.y = modResp1, # response model
  sims = 1e4L, # number of bootstrap simulations
  boot = TRUE, # whether to use bootstrap or not
  treat = "socialbin", # name of treatment variable
  mediator = "enjoyamount") # name of mediator
summary(med11)
# Sensitivity diagnostics - effect would vanish if there was a confounder giving +20% correlation
sensitivity <- mediation::medsens(med11)
summary(sensitivity)
# Add plot of sensitivity diagnostic, varying the correlation
plot(sensitivity)



# Manual coding (because it's dead simple!)

#' Bootstrap estimation of statistic
#' @param data data frame with observations
#' @param statfn function that returns a vector of summary statistics
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

###############################################################################
# Example 2 -  Example of mediated moderation
# Hayes' process macro Model 8 assumes that W interacts with X
# in both mediation model and outcome model
# so the mediation equation reads M ~ X + W + X*W
# and the outcome model reads Y ~ X + W + X*W + M
data(GSBE10, package = "hecedsm")
GSBE10 <- GSBE10 |>
  dplyr::mutate(protestind = as.integer(protest))
process(data = GSBE10,
        y = 'likeability', # response
        x = 'protestind', # experimental factor
        w = 'sexism', # moderator
        m = 'respeval',
        model = 8,
        moments = TRUE,
        boot = 1e4L,
        seed = 80667)
# These are the regression models fitted by the software
modY <- lm(likeability ~ protestind*sexism + respeval, data = GSBE10)
modM <- lm(respeval ~ protestind*sexism, data = GSBE10)

################################################################################


# Example with moderated mediation and a logistic regression for the response
data(jobs, package = "mediation")

Mmod <- lm(job_seek ~ treat + econ_hard +  sex +
             age + occp + marital + nonwhite + educ + income, data = jobs)
Ymod <- glm(work1 ~ treat + job_seek + treat:job_seek + sex +
              age + occp + marital + nonwhite + educ + income,
            family = binomial, data = jobs)
# The software will accomodate directly the different models
medmod <- mediation::mediate(
  model.m = Mmod,
  model.y = Ymod,
  sims = 1000,
  boot = TRUE,
  mediator = "job_seek",
  treat = "treat")
# There are now different versions of ACME and ADE since these depend on X
summary(medmod)
# Plot effects with confidence intervals
plot(medmod)

