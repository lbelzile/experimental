# Load packages
library(ggplot2)
# Change theme for plots
theme_set(theme_minimal())
# Load data
data("GK14_S3", package = "hecedsm")
? GK14_S3
# Fit model
model <- aov(persp ~ condition,
             data = GK14_S3,
             subset = age == "young")
# Analysis of variance table
anova(model)


# Diagnostic plots
db <- data.frame(
  residuals = resid(model),
  rstudent = rstudent(model),
  fitted = fitted(model)
)

ggplot(
  data = na.omit(GK14_S3),
  mapping = aes(x = condition,
                y = persp,
                color = condition)
) +
  geom_boxplot() +
  geom_jitter()

# Install package 'car' - do this ONCE
# install.packages("car")
library(ggplot2)
# Fitted values vs ordinary residuals
ggplot(data = db,
       mapping = aes(x = fitted,
                     y = residuals)) +
  geom_point()
# Test of equality of variance (Brown-Forsythe)
car::leveneTest(model)
