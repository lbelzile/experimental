# Repeated measure ANOVA
library(ggplot2) # grammar of graphics
library(afex) # analysis of factorial experiments
library(emmeans) # estimated marginal means
library(hecedsm)
library(dplyr) # data manipulation

##############################################################################
## One-way within-subject ANOVA
##############################################################################

# Load data and query the first observations
data(AA21, package = "hecedsm")
head(AA21)

# Plot repeated observations to check for learning effects
ggplot(data = AA21, 
       mapping = aes(x = epoch,
                     y = latency,
                     color = stimulus)
       ) +
  geom_point() +
  facet_wrap(~ id, nrow = 3, ncol = 4) +
  theme_classic()

# Compute the mean to get a single measurement per person
AA21_m <- AA21 |>
  dplyr::group_by(id, stimulus) |>
  dplyr::summarize(latency = mean(latency))

# Analysis of variance model for within-subject
aov_mod <- afex::aov_ez(
  id = "id",  # variable for grouping factor
  within = "stimulus", # within-factor (all individuals get these)
  dv = "latency", # response
  data = AA21, # dataset
  fun_aggregate = mean) # aggregation
# Generic anova for afex_aov 
anova(aov_mod, es = "none", correction = "none")

# Compute marginal means and pairwise differences
emm <- emmeans::emmeans(aov_mod, specs = "stimulus")
emm |> contrast(method = "pairwise")

# Make an interaction plot
with(AA21_m,
     interaction.plot(x.factor = id,
                      trace.factor = stimulus,
                      response = latency))


##############################################################################
## Example 1 - two-way mixed ANOVA
##############################################################################

data("HOSM22_E3", package = "hecedsm")
str(HOSM22_E3)
xtabs(~ waiting + ratingtype, data = HOSM22_E3) 

# Fit an ANOVA model

fmod <- afex::aov_ez(id = "id",
                     dv = "imscore",
                     between = "waiting",
                     within = "ratingtype",
                     data = HOSM22_E3)
# Type 3 ANOVA table
anova(fmod)
# MANOVA tests
fmod$Anova

# To get type 2, use the following
rstatix::anova_test(wid = "id",
                    dv = "imscore",
                    between = "waiting",
                    within = "ratingtype",
                    data = HOSM22_E3,
                    type = 2)

# Interaction is not significant
# Compute estimated marginal means (main effects for waiting or ratingtype)
(emm_rating <- emmeans(fmod, specs = "ratingtype"))
(emm_waiting <- emmeans(fmod, specs = "waiting"))

# Compute pairwise differences
emm_rating |> pairs()
emm_waiting |> pairs()
# Interaction plot
emmip(object = emmeans(fmod, specs = c("ratingtype","waiting")),
      formula = ratingtype ~ waiting,
      CIs = TRUE)  +
  scale_color_viridis_d() + # colorblind-friendly color palette
  theme_classic() + # white background theme
  theme(legend.position = "bottom") # move legend position



##############################################################################
## Example 2 - three-way mixed ANOVA (2x2x2)
##############################################################################
# Halevy and Berson, Study 5
data(HB22_S5, package = "hecedsm")
# Check sample size and balance
xtabs(~ curstate + predout, data = HB22_S5)
# Summary statistics
summary(HB22_S5)
str(HB22_S5)
# Fit ANOVA model
mmod <- afex::aov_ez(
  id = "id",
  dv = "likelihood",
  between = c("curstate", "predout"),
  within = "tempdist",
  data = HB22_S5)
# Type 3 ANOVA table
anova(mmod)
# Since the three-way interaction is significant, either
# get a single dimension conditioning on two others
# or compute a 4 way ANOVA separately for each predout (simple effects)

# Get sub tables of marginal means
emm_rc <- emmeans(mmod,
                  specs = c("curstate","predout"),
                  by = "tempdist")
# Custom contrasts as usual
emm_rc |> contrast(method = list("same vs diff" = c(0.5, -0.5, -0.5, 0.5)))
# Interaction plot 
# You can play around with variables to get a better ordering
emmip(emm_rc, 
      formula =  predout ~ curstate | tempdist,
      CIs = TRUE)   +
  scale_color_viridis_d() +
  # Change the labels to get more meaningful text
  labs(color = "predicted outcome", 
       x = "current state") + 
  theme_classic() +
  theme(legend.position = "bottom")

