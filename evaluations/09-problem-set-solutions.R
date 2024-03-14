data(BMH19_S2, package = "hecedsm")
library(afex)
library(emmeans)

rm_mod <- afex::aov_ez(
  id = "id",
  dv = "pcorr",
  within = "color",
  data = BMH19_S2)
summary(rm_mod)
emmeans(rm_mod, specs = "color") |>
  contrast("pairwise", adjust = "tukey") |>
  confint()
# MANOVA results
# The above fits a different mean to each response variable
# Yet we want to compare this to the output with a common mean for all three variables...
# which "afex" does magically
rm_mod$Anova

# Can't use Box's M test here because there is a single group (no between-subject factors)

# Using the MANOVA model via 'lm'
# Pivot data from long to wide, so that each row correspond to an individual
BMH19_S2w <- BMH19_S2 |> tidyr::pivot_wider(names_from = "color", values_from = "pcorr")
multmod <- lm(cbind(col, mix, mon) ~ 1, data = BMH19_S2w)
# We have to set up a model with a common average for each response, which we fake here
colors <- factor(c("col","mix","mon"))
car::Anova(multmod, idata = data.frame(colors),idesign =~colors, type = 3)
# If we wanted to compute follow-up pairwise tests, we need to account for dependence
# All pairwise comparisons are performed using t-tests
ttest1 <- t.test(BMH19_S2w$col, BMH19_S2w$mix, paired = TRUE)
ttest2 <- t.test(BMH19_S2w$mon, BMH19_S2w$mix, paired = TRUE)
ttest3 <- t.test(BMH19_S2w$mon, BMH19_S2w$col, paired = TRUE)
p.adjust(c(ttest1$p.value, ttest2$p.value, ttest3$p.value), method = "bonferroni")
