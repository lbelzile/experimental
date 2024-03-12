# Power calculations
power <- c(0.8, 0.9, 0.95)
# Example 1 - https://osf.io/ehjdm
f <- effectsize::eta2_to_f(0.55)
ng <- 4
ceiling(WebPower::wp.kanova(ndf = 1, ng = ng, f = f, power = power[1])$n)
ceiling(WebPower::wp.kanova(ndf = 1, ng = ng, f = f, power = power[2])$n)
ceiling(WebPower::wp.kanova(ndf = 1, ng = ng, f = f, power = power[3])$n)

f <- effectsize::eta2_to_f(0.11)
ceiling(WebPower::wp.kanova(ndf = 1, ng = ng, f = f, power = power[1])$n)
ceiling(WebPower::wp.kanova(ndf = 1, ng = ng, f = f, power = power[2])$n)
ceiling(WebPower::wp.kanova(ndf = 1, ng = ng, f = f, power = power[3])$n)


# Example 2
# Note that we need to tweak the effect size
# The latter in WebPower (for an interaction effect) should be sigma_effect/sigma_resid * C
# where C = sqrt(K/(1-rho)), for K the number of groups and rho the correlation of measurements within subjects
WebPower::wp.rmanova(type = 2, nm = 2, ng = 2, f = effectsize::eta2_to_f(0.05)*sqrt(2), power = 0.8)
WebPower::wp.rmanova(type = 2, nm = 2, ng = 2, f = effectsize::eta2_to_f(0.05)*sqrt(2), power = 0.9)
WebPower::wp.rmanova(type = 2, nm = 2, ng = 2, f = effectsize::eta2_to_f(0.05)*sqrt(2), power = 0.95)


# Example 3
WebPower::wp.kanova(f = 0.87/2, ndf = 1, ng = 3, power = 0.99)

# Example 4
WebPower::wp.anova(f = 0.61/2, k = 2, power = 0.99)

# Example 5
f <- effectsize::eta2_to_f(0.389) * sqrt(6/(1-0.5))
# Error message because the sample size is smaller than the number of measurements
# if we search - but we can still compute the power for a given sample size ...
WebPower::wp.rmanova(f = f, ng = 1, nm = 6, n = 3:4, type = 1)
WebPower::wp.rmanova(f =  sqrt(0.427/(1-0.427)) * sqrt(6/(1-0.5)), ng = 1, nm = 6, n = 3:4, type = 1)
# You can use the effect size calculator: https://webpower.psychstat.org/models/means05/effectsize.php

# Example 6
WebPower::wp.t(type = "one.sample", d = 0.93, power = 0.8)
WebPower::wp.t(type = "one.sample", d = 0.93, power = 0.9)
WebPower::wp.t(type = "one.sample", d = 0.93, power = 0.95)

# Example 7
2*ceiling(WebPower::wp.t(type = "two.sample", alternative = "two.sided", d = 0.451, power = 0.8)$n)
WebPower::wp.t(type = "two.sample", alternative = "two.sided", d = 0.451, power = 0.9)
WebPower::wp.t(type = "two.sample", alternative = "two.sided", d = 0.451, power = 0.95)
# Note that number reported is sample size per group, so double for a pairwise comparison
