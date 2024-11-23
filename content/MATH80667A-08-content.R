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
car::Anova(model1)
car::Anova(model2)

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
c1 <- contrast(emm1,
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
   rstudent(model1) ~ condition,
   data = SSVB21_S2,
   center = 'mean')
# Equality of slopes (interaction)
car::Anova(lm(post ~ condition * prior,
           data = SSVB21_S2),
           model1)
# Linearity of effect
car::residualPlots(model = model1, terms = ~ prior)
# Here, while the straight line isn't quite perfect,
# it's a good enough approximation.
# There are however several subgroups/clusters
# firm disbelievers (100 to 100), people who have no more opinion (move to 0)
# people who have the same posterio and prior score, etc.
# and the weird scale /selection mechanisms caps the effect, so the larger
# fitted values have more dispersion

#-------------------------------------------------

# Moderation analysis
data(GSBE10, package = "hecedsm")
lin_moder <- lm(respeval ~ protest*sexism,
                data = GSBE10)
summary(lin_moder) # coefficients
car::Anova(lin_moder) #if significant, look at simple effects

# Plot response as a function of sexism (the continuous variable)
ggplot(data = GSBE10,
       aes(x = sexism,
           y = respeval,
           color = protest,
           fill = protest)) +
  geom_point() +
  geom_smooth(se = TRUE, method = "lm", formula = y ~ x) +
  labs(subtitle = "evaluation of response",
       y = "",
       color = "experimental condition",
       fill = "experimental condition") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
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

# With a single binary covariate, we can try to check the zone of
# significance on the x-axis (covariate) at which the difference
# between averages of groups becomes significant
# Johnson-Neyman plot
jn <- interactions::johnson_neyman(
  model = lin_moder2, # linear model
  pred = protest, # binary experimental factor
  modx = sexism, # moderator
  control.fdr = TRUE, # control for false discovery rate
  mod.range = range(GSBE10$sexism)) # range of values for sexism
jn$plot

# Using Hayes' PROCESS macro
process(data = GSBE10 |>
          dplyr::mutate(protestind = as.integer(protest)),
        y = "respeval",  # response variable
        w = "sexism", # postulated moderator (continuous)
        x = "protestind", # experimental factor
        model = 1, # number of model in Hayes (simple moderation)
        plot = TRUE, # add plot
        moments = TRUE, # probe at mean +/- std. error;
        # for different values, use argument "wmodval"
        jn = TRUE)


#--------------------------------------------------------------------
# Moderation analysis
# This time, the factor of interest is continuous
# and the moderator is categorical (K=3 levels)
data(LWSH23_S3, package = "hecedsm")
mod <- lm(data = LWSH23_S3, needsatis ~ needclosure * cond)
anova(mod) # interaction is significant
# Compute estimated marginal means, but with global weights equal to relative weight of each variable
emmeans(mod, specs = "needclosure", by = "cond", weights = "prop")
# All values are reported for the average of needclosure

# With PROCESS macro, which  only understand numeric values for factors...
process(data = LWSH23_S3 |>
          dplyr::mutate(condind = as.integer(cond)), # cast factor to numeric integer levels
        y = "needsatis",  # response variable
        x = "needclosure", # explanatory variable (not manipulated)
        w = "condind", # postulated moderator
        mcw = 1, # dummy coding for moderator w (so compare to base level, here 'included')
        model = 1, # number of model in Hayes (simple moderation)
        plot = TRUE) # add plot, doesn't seem to work...
