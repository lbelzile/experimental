
# Load packages
library(dplyr)   # data manipulation
library(ggplot2) # plots
library(emmeans) # marginal means and contrasts
data(arithmetic, package = "hecedsm")
# categorical variable = factor

# Look up data
str(arithmetic)
# Summray statistics per group
summary_stat <-
  arithmetic |> # the pipe |> indicates take the left hand side and send it as argument to rhs
  group_by(group) |> # gather observations by factor
  summarize(mean = mean(score), # compute summary statistics for each subgroup
            sd = sd(score))
# Print in a nice HTML table
knitr::kable(summary_stat,
             digits = 2)

set.seed(1234) # fix random seed for reproducibility, since jittering that comes next is random
# this ensures we get the same plot if we re-run the code
# Boxplot with jittered data
ggplot(data = arithmetic,   # dataset
       mapping = aes(x = group,  # mapping gives aesthetics (what variables for which dimension)
                     y = score)) +
  geom_boxplot() + # box and whiskers plot
  geom_point(position = position_jitter(width = 0.3, height = 0)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(subtitle = "score on arithmetic test",
       title = "Impact of encouragement on learning outcomes",
       y = "") # empty label

#Fit one way analysis of variance
test <- aov(data = arithmetic,
            formula = score ~ group)
# "formula" takes the form response variable ~ explanatory factors

# Compute test statistic and associated p-value
anova(test)
# Format output so that we get a data frame, rather than a printed object
out <- broom::tidy(test)
# Compute p-value by hand
pf(out$statistic,
   df1 = 4, # number of groups minus one
   df2 = 40, # number of observations minus nb of groups
   lower.tail = FALSE) #look at tail (probability of above)

# Compute pairwise differences and extract the sole one of interest
# for now - the code is overly complicated for now, but we will use
# this method later.

# Compute the group mean for each "group", i.e., factor level
margmeans <- emmeans(test, specs = "group")
# Compute pairwise differences and associated standard errors
contrast(margmeans,
         method = "pairwise",
         adjust = 'none',
         infer = TRUE) |>
  tibble::as_tibble() |> # convert to data frame
  filter(contrast == "praise - reprove") |> # extract a single line
  knitr::kable(digits = 3) # format to keep three significant digits
