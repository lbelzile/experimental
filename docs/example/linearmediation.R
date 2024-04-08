
library(mediation, quietly = TRUE)
data(BJF14_S1, package = "hecedsm")

# 'threat' is the mediator variable
# 'condition' is the experimental manipulation
# 'groupsize', 'age', 'gender' are control variates
MsX <- lm(threat ~ condition + gender + groupsize + age,
          data = BJF14_S1) # mediator model
YsXM <- lm(bonding ~ condition + threat + gender + groupsize + age,
           data = BJF14_S1) # response model
summary(MsX) # coefficients for the mediation model
summary(YsXM) # coefficients for the response model

# Use 'coef' to extract estimated coefficients
# and 'vcov' to get the estimated covariance matrix of coefficients
coef_alpha <- coef(MsX)['conditionPain']
coef_gamma <- coef(YsXM)['threat']
coef_beta <- coef(YsXM)['conditionPain']
var_alpha <- vcov(MsX)['conditionPain','conditionPain']
var_gamma <- vcov(YsXM)['threat','threat']

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

# Compute Sobel statistic
sobelStat <- sobel(
  alpha = coef_alpha,
  gamma = coef_gamma,
  alpha_var = var_alpha,
  gamma_var = var_gamma)
# Compute two-sided p-value for test alpha*gamma=0
sobelStat

## Run bootstrap
set.seed(80667) # set seed for reproducibility
linmed <- mediate(
  model.m = MsX, # mediation model
  model.y = YsXM, # response model
  sims = 1000L, # number of bootstrap simulations
  boot = TRUE, # use bootstrap
  boot.ci.type = "perc", # type of bootstrap: percentile
  mediator = "threat", # name of mediator
  treat = "condition", # name of treatment
  control.value = "Control", # name of control (level of 'condition')
  treat.value = "Pain") # name of treatment (level of 'condition')
summary(linmed)

# Run sensitivity analysis
linmed_sensitivity <- medsens(linmed)
summary(linmed_sensitivity)
plot(linmed_sensitivity)

