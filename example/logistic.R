data("DA23_E2", package = "hecedsm")
mod <- glm(purchased ~ meanval, data = DA23_E2, family=binomial)
summary(mod)
car::Anova(mod, type = 3)



mod <- glm(purchased ~ format, data = DA23_E2, family=binomial)
# Compute odds ratios
emm <- emmeans::emmeans(mod, specs = "format", type = "response")
# Extract output for point estimates
probs <- summary(emm)$prob # probability of success for both groups
odds <- probs/(1-probs) # Compute odds
odds[1]/odds[2] # odds ratio
probs[1]/probs[2] # risk ratio
probs[1] - probs[2]# prob difference
# Pairwise differences with risk ratios p1/p2
log.emm <- emmeans::regrid(emm, "log")
pairs(log.emm, type = "response")
# Odds ratios
emm |>  emmeans::contrast("pairwise")
# Difference in estimated probabilities of success
emmeans::regrid(emm, "response") |> pairs()

