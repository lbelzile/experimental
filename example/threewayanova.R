# Load packages
library(dplyr)
library(ggplot2)
library(emmeans)
# Load data
data(words, package = 'hecedsm')
# unordered factors, ordered factors
options(contrasts = c("contr.sum","contr.poly"))
# are the data balanced
words |>
  group_by(feedback, material, age) |>
  summarize(nsamp = n())
# This is a 3x3x2 factorial design with r=5 replicates
model <- lm(words ~ feedback*material*age,
            data = words)
anova(model)

# Interaction plot - group average (for each subgroup)
words |>
  group_by(feedback, material, age) |>
  summarize(meanwords = mean(words)) |>
ggplot(mapping = aes(x = feedback,
                     y = meanwords,
                     group = material,
                     color = material)) +
  geom_line() + # connect the dots
  facet_wrap(~age) +
  theme(legend.position = "bottom") +
  labs(subtitle = "mean number of words remembered",
       y = "")

# Plot raw data
ggplot(data = words,
         mapping = aes(x = age,
                       y = words,
                       group = material,
                       color = material)) +
  geom_point(position = position_jitterdodge(jitter.height = 0.1,
                                             dodge.width = 0.3)) +
  facet_wrap(~feedback) + # wrap by third variable
  theme_classic() + # change default theme
  theme(legend.position = "bottom") + # move caption
  labs(subtitle = "mean number of words remembered",
       y = "") # add more meaningful axis labels

# Post-hoc effect estimation
# Keppel and Wickens (Chapter 22)
# Note that all degrees of freedom in this chapter are off by 2
# Main effects and contrasts
# Estimated marginal mean (EMM)
# Marginal estimate (averaged over levels of material and age)
margA <- emmeans(model, specs = "feedback")
# Check that this is indeed the mean of each word count by feedback
words |>
    group_by(feedback) |>
    summarize(mean = mean(words))
# Compute a marginal contrast:
# comparison between no-feedback and the average of pos + neg
contrast(object = margA,
         method = list(interaction = c(1, -0.5, -0.5)))
# H0: MU_none = 0.5*(MU_positive + MU_negative)
# 1MU_none - 0.5MU_positive - 0.5MU_negative = 0

# Simple effects: B x C (material x age) is significant,
# so can test for difference between levels of B
# within each value of C (average over all feedback)
simpleBpC <-
  emmeans(model,
          specs = c("material", "age")) |>
  joint_tests(by = "age")
# Simple contrasts
margBC <- emmeans(model,
                  specs = c("material", "age"),
                  by = "age") |>
  contrast(method = list(contrast = c(-0.5, -0.5, 1)))

# Interaction components based on marginal means
# Compare b2 and b3: is there difference between groups of C?
emmeans(model,
        specs = c("material", "age"),
        by = "age") |>
  contrast(method = list(b2vsb3 = c(0, 1, -1))) |>
  joint_tests()

# Compute differences between feedback and material
emmeans(model, specs = c("feedback", "material")) |>
  contrast(method = list(nonevsaverageforb2vsb3 =
  c(0, 0, 0, -1, 0.5, 0.5, 1, -0.5, -0.5)))
# Three factor interaction
# A: no feedback vs feedback (none vs average of pos & neg)
# B: low vs high emotional feedback (b2 vs b3)
# C: fifth graders vs seniors
threewaycontrast <- emmeans(model, specs = c("feedback", "material","age")) |>
  contrast(method = list(contrast =
                           c(0, 0, 0, -1, 0.5, 0.5, 1, -0.5, -0.5,
                               0, 0, 0, 1, -0.5, -0.5, -1, 0.5, 0.5)))
summary(threewaycontrast, adjust = "scheffe", scheffe.rank = 17)

