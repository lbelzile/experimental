
# Load packages
library(dplyr)   # data manipulation
library(ggplot2) # plots
library(emmeans) # marginal means and contrasts
data(arithmetic,
     package = "hecedsm")
# categorical variable = factor

# Look up data
str(arithmetic)
# Summray statistics per group
summary_stat <-
  arithmetic |>
  group_by(group) |>
  summarize(mean = mean(score),
            sd = sd(score))
# Print in a nice HTML table
knitr::kable(summary_stat,
             digits = 2)

# Boxplot with jittered data
ggplot(data = arithmetic,
       mapping = aes(x = group,
                     y = score,
                     color = group)) +
  geom_boxplot() +
  geom_jitter(width = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(subtitle = "score on arithmetic test",
       title = "Impact of encouragement on learning outcomes",
       y = "") # empty label

#Fit one way analysis of variance
test <- aov(data = arithmetic,
            formula = score ~ group)
anova(test)
out <- broom::tidy(test)
# Compute p-value by hand
pf(out$statistic,
   df1 = 4,
   df2 = 40,
   lower.tail = FALSE)


margmeans <- emmeans(test, specs = "group")
contrast(margmeans,
         method = "pairwise",
         adjust = 'none',
         infer = TRUE) |>
  tibble::as_tibble() |>
  filter(contrast == "praise - reprove") |>
  knitr::kable(digits = 3)
