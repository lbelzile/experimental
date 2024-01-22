# Install packages
# Remove hash sign (#) to uncomment a line
# install.packages(c("dplyr", "ggplot2", "car", "remotes"))
# remotes::install_github("lbelzile/hecedsm")
library(hecedsm)
data("GK14_S3", package = "hecedsm")
?GK14_S3 # Get documentation

# Fit one-way analysis of variance model
model <- aov(persp ~ condition, data = GK14_S3)
# Output table with results and p-values
anova(model)
# Format results in a table

broom::tidy(anova(model))
library(ggplot2)
db <- data.frame(residuals = resid(model), # ordinary residuals
                 rstudent = rstudent(model), # studentized residuals
                 fitted = fitted(model)) #fitted values, aka group averages
# Box and whiskers plot with data superimposed
ggplot(data = na.omit(GK14_S3),
       mapping = aes(x = condition,
                     y = persp,
                     color = condition)) +
  geom_boxplot() +
  geom_jitter(height = 0)

library(ggplot2)
# Fitted values vs ordinary residuals
ggplot(data = db,
       mapping = aes(x = fitted,
                     y = residuals)) +
  geom_point()
# Test of equality of variance (Brown-Forsythe)
car::leveneTest(model)
# Check normality via quantile-quantile plot
car::qqPlot(model)


# Fit the ANOVA model and test equality of mean at level 5%.
# - Report the test statistic, the null distribution and the p-value.
# - Provide a conclusion in the context of the study. Check with the
# - There are missing values (use `summary(GK14_S3)`: do you think it will impact the conclusions or not?
# - How many observations are there in each group (excluding missing values)? Is the number sufficient to reliably estimate the sample mean of each experimental condition?
GK14_S3 |>
  dplyr::filter(!is.na(persp)) |>
  dplyr::group_by(condition) |>
  dplyr::summarize(count = dplyr::n())
# Subset the data for young people
data_young  <- GK14_S3 |>
  dplyr::filter(is.na(persp),
                age == "young")
