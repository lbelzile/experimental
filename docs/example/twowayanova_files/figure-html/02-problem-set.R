## R code for Problem set 2

# Any line that starts with a hash symbol (#) is a comment
# The first step is loading the database
# from the **R** package. If this step fails,
# uncomment the following two lines and run once
# install.packages("remotes")
# remotes::install_github("lbelzile/hecedsm")

library(tidyverse)
# Data from Bastian, Jetten and Ferris (2014)
# https://doi.org/10.1177/0956797614545886
data(BJF14_S1, package = "hecedsm")
?BJF14_S1
## Summary statistics
BJF14_S1 |>
  dplyr::group_by(condition) |>
  dplyr::summarize(mean = mean(bonding),
                   sd = sd(bonding),
                   nobs = n(),
                   se = sd/sqrt(nobs),
                   lower = mean - qt(0.975,
                                     df = nobs - 1)*se,
                   upper = mean + qt(0.975,
                                     df = nobs - 1)*se)
# This returns a table with summary statistics
# per experimental condition
# 1. the sample mean
#	2. the standard deviation of observations
#	3. the sample size per group
#	4. the standard error of the mean
#	5. the lower bound of the 95% confidence interval
#	6. the upper bound of the 95% confidence interval

## Hypothesis tests
# These are functions with a set of arguments
# In R, most models either take arguments
# or a formula of the form
# "response ~ experimental factor"
test1 <- t.test(pain ~ condition,
                var.equal = TRUE,
                data = BJF14_S1,
                conf.level = 0.95)
# t.test performs a two-sample t-test
# if argument 'var.equal' is TRUE
# or Welch's two-sample t-test
# if 'var.equal' is FALSE (default)
test2 <- t.test(unpleasant ~ condition,
                 var.equal = TRUE,
                 data = BJF14_S1)
# Each test function outputs slightly different
# objects (lists) with different names
# the 'broom' package 'tidy' package cleans it
# and exports the output as a data table
broom::tidy(test1)
# The papaja package also has user-friendly
# functions to automatically report summary
# statistics in APA-compliant format for Rmarkdown
# papaja::apa_print(test1)


# Analysis of variance table
test3a <- anova(aov(formula = bonding ~ condition,
                    data = BJF14_S1))
broom::tidy(test3a)
# Equivalent test is two-sample t-test (with equal variance)
# one statistic is the square of the other!
# Thus same p-value
test3b <- t.test(bonding ~ condition,
                 data = BJF14_S1,
                 var.equal = TRUE)
broom::tidy(test3b)

# Experiment 1 of Duke and Amir (2022+)
# https://doi.org/10.1287/mksc.2022.1364
# Perform a two-sample t-test
data(DA22_E1, package = "hecedsm")
# Check documentation for the list and
# description of the variables
?DA22_E1

# To filter only observations of people who bought
# use the subset "DA22_E1s"
DA22_E1s <- DA22_E1 |> dplyr::filter(purchased == 1L)
