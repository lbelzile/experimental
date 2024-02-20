# Load packages
library(dplyr)   # data manipulation
library(ggplot2) # graphics
library(emmeans) # contrasts, etc.
library(car) # companion to applied regression
# Example of two-way ANOVA with balanced design
data(C16, package = "hecedsm")
# Check for balance
xtabs(formula = ~ anchor + magnitude,
      data = C16)

# Fit two-way ANOVA model
mod <- aov(madjust ~ anchor * magnitude,
           data = C16)
# Analysis of variance table
summary(mod)
anova(mod)
# Interaction plot with jittered observations, superimposed
C16 |>
 ggplot(mapping = aes(x = anchor,
                      y = madjust,
                      color = magnitude)) +
  geom_jitter(width = 0.1,
              alpha = 0.1) +
  stat_summary(aes(group = magnitude),
               fun = mean,
               geom = "line") +
  # Change position of labels
  labs(y = "",
       subtitle = "Mean adjustment") +
  theme_classic() + # change theme
  theme(legend.position = "bottom")

# Interaction plot
emmeans::emmip(mod,
               magnitude ~ anchor,
               CIs = TRUE) +
  theme_minimal()

# Get grand mean, cell means, etc.
model.tables(mod, type = "means")

# Cell means
emmeans(object = mod,
        specs = c("anchor", "magnitude"),
        type = "response")
# Marginal means
emmeans(object = mod,
        specs = "anchor",
        type = "response")
emmeans(object = mod,
        specs = "anchor",
        type = "response")
# These match summary statistics because the dataset is balanced
C16 |>
  group_by(magnitude) |>
  summarize(margmean = mean(madjust))
C16 |>
  group_by(anchor) |>
  summarize(margmean = mean(madjust))

## Quantile-quantile plot
car::qqPlot(mod, id = FALSE)


## Plot of residuals against fitted values
# Evidence of unequal variance
ggplot(data = data.frame(residuals = resid(mod),
                         fitted = fitted(mod)),
       mapping = aes(x = fitted,
                     y = residuals)) +
   geom_jitter(width = 0.03, height = 0) +
  theme_classic()
# Equality of variance - Brown-Forsythe
car::leveneTest(mod)

## Model with heteroscedasticity
# Fit a variance per group
mod2 <- nlme::gls(
  model = madjust ~ anchor * magnitude,
  data = C16,
  weights = nlme::varIdent(
    form = ~ 1 | anchor * magnitude))

# Different ANOVA - we use type II here
car::Anova(mod2, type = 2)


## Estimated marginal means - the software will account for the unequal variances
emmeans(object = mod2,
        specs = c("anchor", "magnitude"))
# Compute pairwise difference for anchor
marg_effect <- emmeans(object = mod2,
        specs = "anchor") |>
  pairs()
marg_effect
# To get a data frame tidy
tab <- broom::tidy(marg_effect)
# To print in a Quarto/Rmarkdown report
# knitr::kable(tab, digits = 2)

## Estimated marginal mean with unequal variance model
emm_marg <- emmeans::emmeans(
  object = mod2,
  specs = "anchor"
)

# Simple effects for anchor
emm_simple <- emmeans::emmeans(
  object = mod,
  specs = "anchor",
  by = "magnitude"
)
# Compute pairwise differences within each magnitude
pairs(emm_simple)


## Pairwise differences
emmeans(object = mod2,
        specs = c("magnitude", "anchor"),
        contr = "pairwise")


## -----------------------------------------------------------------------------
## Second dataset, with exotic contrasts!
data(MP14_S1, package = "hecedsm")
xtabs(~ direction + station, data = MP14_S1)

str(MP14_S1) # look at data

# Set up contrasts
options(contrasts = c("contr.sum", "contr.poly"))
model <- lm(distance ~ station*direction,
            data = MP14_S1)
summary(model) # the coefficients are global mean

# Test only the interaction
car::Anova(model, type = 3)


## Automatic interaction plot from emmeans
emmeans::emmip(object = model,
               # trace.factor ~ x.factor
               formula = direction ~ station,
               CIs = TRUE) +
  theme_minimal()



# Interaction plot
# average of each subgroup, with +/- 1 std. error

MP14_S1 |>
  group_by(direction, station) |>
  summarize(mean = mean(distance),
            se = sigma(model) / sqrt(n()),
            lower = mean - se,
            upper = mean + se) |>
  ggplot(mapping = aes(x = station,
                       y = mean,
                       group = direction,
                       col = direction)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = 0.2) +
  geom_point() +
  scale_colour_grey() +
  labs(title = "subjective distance",
       subtitle = "mean (1 std. error)",
       x = "subway station",
       y = "",
       caption = "stations are ordered from west to east") +
  theme_classic() +
  theme(legend.position = "top")


## Joint test of symmetry
emm <- emmeans(model,
               specs = c("direction", "station"))
levels(emm)
contrast_list <- list(
 "2 station (opposite)" = c(1, 0, 0, 0, 0, 0, 0, -1),
 "1 station (opposite)" = c(0, 0, 1, 0, 0, -1, 0, 0),
 "2 station (travel)" = c(0, 1, 0, 0, 0, 0, -1, 0),
 "1 station (travel)" = c(0, 0, 0, 1, -1, 0, 0, 0)
)
# Hypothesis test (symmetry)
emm |>
  contrast(method = contrast_list) |>
  test(joint = TRUE)


## Contrasts
custom_contrasts <- list(
 "2 station, opposite vs same" =
   c(1, -1, 0, 0, 0, 0, -1, 1),
 "1 station, opposite vs same" =
   c(0, 0, 1, -1, -1, 1, 0, 0),
 "1 vs 2 station (opposite)" =
   c(-1, 0, 2, 0, 0, 2, 0, -1),
 "1 vs 2 station (same)" =
   c(0, -1, 0, 2, 2, 0, -1, 0)
)
# Set up contrasts with correction
cont_emm <- contrast(
  object = emm,
  method = custom_contrasts,
  adjust = "scheffe")
# Tests and p-values
cont_emm |>
  test(scheffe.rank = 7)
# Tests with confidence intervals
cont_emm |>
  confint(scheffe.rank = 7,
          level = 0.99)

