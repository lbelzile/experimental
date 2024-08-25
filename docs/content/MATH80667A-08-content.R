library(knitr)
library(ggplot2)
library(patchwork)
library(emmeans)
options(contrasts = c("contr.sum", "contr.poly"))
data(SSVB21_S2, package = "hecedsm")
# Check balance
with(SSVB21_S2, table(condition))

# Plot the data
ggplot(data = SSVB21_S2,
       aes(x = prior, y = post)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE)
# Fit model with and without covariate prior
model1 <- lm(post ~ condition + prior, data = SSVB21_S2)
model2 <- lm(post ~ condition, data = SSVB21_S2)


# Check that the data are well randomized
car::Anova(lm(prior ~ condition, data = SSVB21_S2), type = 3)
# Fit linear model with continuous covariate
model1 <- lm(post ~ condition + prior, data = SSVB21_S2)
# Fit model without for comparison
model2 <- lm(post ~ condition, data = SSVB21_S2)
# Global test for differences - of NO INTEREST
car::Anova(model1, type = 3)
car::Anova(model2, type = 3)

# Estimated marginal means
emm1 <- emmeans(model1, specs = "condition")
# Note order: Boost, BoostPlus, consensus
emm2 <- emmeans(model2, specs = "condition")
# Not comparable: since one is detrended and the other isn't
contrast_list <- list(
   "boost vs control" = c(0.5,  0.5, -1),
   #av. boosts vs consensus
   "Boost vs BoostPlus" = c(1, -1,  0))
# Compute contrast
contrast(emm1,
         method = contrast_list,
         p.adjust = "holm")


# Print pretty table
kableExtra::kable(c1,
                  col.names = c("contrast",
                                "estimate",
                                "se",
                                "df",
                                "t stat",
                                "p-value"),
                  digits = c(2,2,2,0,2,2))

# Same, with second model
c2 <- contrast(emm2,
         method = contrast_list,
         p.adjust = "holm")
c2

# Test equality of variance
levene <- car::leveneTest(
   resid(model1) ~ condition,
   data = SSVB21_S2,
   center = 'mean')
# Equality of slopes (interaction)
car::Anova(lm(post ~ condition * prior,
           data = SSVB21_S2),
           model1, type = 3)

#-------------------------------------------------


data(GSBE10, package = "hecedsm")
lin_moder <- lm(respeval ~ protest*sexism,
                data = GSBE10)
summary(lin_moder) # coefficients
car::Anova(lin_moder, type = 3) #if significant, look at simple effects

# Plot response as a function of sexism (the continuous variable)
ggplot(data = GSBE10,
       aes(x = sexism,
           y = respeval,
           color = protest)) +
  geom_point() +
  geom_smooth(se = TRUE, method = "lm", formula = y ~ x) +
  labs(subtitle = "evaluation of response",
       y = "",
       color = "experimental condition") +
  theme_classic() +
  theme(legend.position = "bottom")

# Compute quartiles of sexism and return the estimated marginal means
# for each subgroup
quart <-  quantile(GSBE10$sexism, probs = c(0.25, 0.5, 0.75))
emmeans(lin_moder,
        specs = "protest",
        by = "sexism",
        at = list("sexism" = quart))

# Consider again a simpler version with both pooled
lin_moder2 <- lm(
  respeval ~ protest*sexism,
  data = GSBE10 |>
    # We dichotomize the manipulation, pooling protests together
    dplyr::mutate(protest = as.integer(protest != "no protest")))
# Test for equality of slopes/intercept for two protest groups
anova(lin_moder, lin_moder2)
# p-value of 0.18: fail to reject individual = collective.

# Johnson-Neyman plot
jn <- interactions::johnson_neyman(
  model = lin_moder2, # linear model
  pred = protest, # binary experimental factor
  modx = sexism, # moderator
  control.fdr = TRUE, # control for false discovery rate
  mod.range = range(GSBE10$sexism)) # range of values for sexism
jn$plot
