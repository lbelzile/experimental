library(ggplot2)
library(patchwork)
library(afex)
library(emmeans)

# Set sum-to-zero constraint for factors
options(contrasts = c("contr.sum", "contr.poly"))
data(AA21, package = "hecedsm")
# Compute mean
AA21_m <- AA21 |>
  dplyr::group_by(id, stimulus) |>
  dplyr::summarize(latency = mean(latency))

# Plot repeated observations
ggplot(data = AA21_m,
       aes(x = id,
           colour = stimulus,
           y = latency)) +
  geom_point() +
  theme_classic() +
  labs(subtitle = "latency measure",
       y = "",
       x = "participant identifier") +
  theme(legend.position = "bottom")

# Fit model using `afex`
model <- afex::aov_ez(
  id = "id",           # subject id
  dv = "latency",      # response
  within = "stimulus", # within-subject
  data = hecedsm::AA21,
  fun_aggregate = mean)
anova(model, # mixed ANOVA model
      correction = "none", # sphericity
      es = "none") # effect size
summary(model) #truncated output

# Set up contrast vector
cont_vec <- list("real vs GAN" = c(1, -0.5, -0.5))
# Compute t-statistic for contrast
model |> emmeans::emmeans(spec = "stimulus", contr = cont_vec)

##################################################################
## Second example, from course notes

data(HOSM22_E3, package = "hecedsm")
str(HOSM22_E3)
# Check balance - half the measurements since there are two for each
with(HOSM22_E3, table(waiting))
# Compute simple effects for the plot
emm <- emmeans(mmod,
               specs = "waiting",
               by = "ratingtype")
# Interaction plot - lines seem parallel
# Indicative of no interaction
emmip(emm,
      formula =  ~ waiting | ratingtype,
      CIs = TRUE) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "predicted output",
       color = "current state")

# Check intuition with formal tests
mmod <- afex::aov_ez(
  id = "id", # subject identifier
  dv = "imscore", # dependent variable, aka response
  between = "waiting",
  within = "ratingtype",
  data = HOSM22_E3)
# Since this is a two-by-two, no sphericity assumption
summary(mmod)
# Interaction is not significant, proceed with marginal effects
emmeans(mmod,
        specs = "waiting") |>
  pairs()
# For rating type, we should proceed with caution
emmeans(mmod,
        specs = "ratingtype") |>
  pairs()
# Note that the degrees of freedom are based on number of individual (not measurements!)
