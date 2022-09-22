data(arithmetic, package = "hecedsm")
## #Fit one way analysis of variance
test <- aov(data = arithmetic,
            formula = score ~ group)
anova(test) #print anova table


# Compute p-value
pf(15.27,
   df1 = 4,
   df2 = 40,
   lower.tail = FALSE)

data(arithmetic, package = "hecedsm")
model <- aov(score ~ group, data = arithmetic)
car::leveneTest(model) #Brown-Forsythe by default
car::qqPlot(model) # quantile-quantile plot
