# Power analysis for population swapping project

# We have three treatment groups - self, unrelated, and other population. So we will do an ANOVA power analysis
library(pwr)
pwr.anova.test(k = 3, f = .25, sig.level = .05, power = .8)
# Where k = number of groups, f = effect size (and 0.25 is generally accepted as a medium effect size)
# Used medium effect size as we probably don't really care about a "small" effect of pedestal swapping between populations, and chasing after that would be difficult.
# For comparison sakes, the effect size of intra-population swapping from other project was definitely large (>= .8 for a t-test)

# Can also do power analysis for a planned linear model analysis. Will do here for analysis with 3 predictors (Sex + mother + treatment)
pwr.f2.test(u = 3, v = NULL, f2 = .15, sig.level = .05, power = .8)
# Where u = is numerator degrees of freedom (k - 1, k = number of continuous variables + number of dummy codes for categorical variables)
# V = sample size, f2 = effect size (.02 is small, .15 medium, .35 large by Cohen).

# Can also use both together. If we meet the sample size target suggested by ANOVA calculation above, what would be the power of our linear model?

pwr.f2.test(u = 3, v = 52, f2 = NULL, sig.level = .05, power = .8)
# .21, or, relatively high