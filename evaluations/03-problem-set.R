# Install packages
# 'dplyr', 'ggplot2', 'car' and 'hecedsm'

library(hecedsm)
data("GK14_S3", package = "hecedsm")
?GK14_S3
model <- aov(persp ~ condition, data = GK14_S3)
anova(model)

library(ggplot2)
db <- data.frame(residuals = resid(model),
                 rstudent = rstudent(model),
                 fitted = fitted(model))

ggplot(data = na.omit(GK14_S3),
       mapping = aes(x = condition,
                     y = persp,
                     color = condition)) +
  geom_boxplot() +
  geom_jitter()

# Install package 'car'
# install.packages("car")
library(ggplot2)
# Fitted values vs ordinary residuals
ggplot(data = db,
       mapping = aes(x = fitted,
                     y = residuals)) +
  geom_point()
# Test of equality of variance (Brown-Forsythe)
car::leveneTest(model)
# Check normality
car::qqPlot(model)


# Fit the ANOVA model and test equality of mean at level 5%.
# - Report the test statistic, the null distribution and the p-value.
# - Provide a conclusion in the context of the study. Check with the
# -There are missing values (use `summary(GK14_S3)`: do you think it will impact the conclusions or not?
# - How many observations are there in each group (excluding values)? Is the number sufficient to reliably estimate the sample mean of each experimental condition?
GK14_S3 |>
  dplyr::filter(!is.na(persp)) |>
  dplyr::group_by(condition) |>
  dplyr::summarize(count = dplyr::n())
# -
