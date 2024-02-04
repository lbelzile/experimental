# -*- coding: utf-8 -*-

import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
import scipy.stats as stats
url = 'https://raw.githubusercontent.com/lbelzile/hecedsm/main/data-raw/csv/RJ74.csv'
df = pd.read_csv(url)

# Cross-tabulate counts
crosstab = pd.crosstab(df["gender"], df["decision"],  values=df["count"], aggfunc = "sum")
print(crosstab)
stats.chi2_contingency(crosstab)

# Run alternatively a Poisson model - we want the score test
df_agg = df.groupby(['gender', 'decision']).agg({'count':'sum'}).reset_index()
print(df_agg)
model = smf.glm(formula = "count ~ gender + decision", data = df_agg, family=sm.families.Poisson())
# Fit the model
result = model.fit()
# Display and interpret results
print(result.summary())

# Second example: negative binomial counts
url = 'https://raw.githubusercontent.com/lbelzile/hecedsm/main/data-raw/csv/BL22_L.csv'
df2 = pd.read_csv(url)
print(df2, 5)
# This function does not estimate the dispersion parameter 'alpha'
# Fix to the value obtained by R (alpha is reciprocal of dispersion)
model = smf.glm(formula = "ncreative ~ cond", data = df2, family=sm.families.NegativeBinomial(alpha = 1/15.52077809))
# Fit the model
result = model.fit()
# Display and interpret results
print(result.summary())

# Third example: Liu et al.
url = 'https://raw.githubusercontent.com/lbelzile/hecedsm/main/data-raw/csv/LRMM23_S1.csv'
df3 = pd.read_csv(url)

stats.ttest_ind(df3['appreciation'][df3["role"] == "responder"], df3['appreciation'][df3["role"] == "initiator"], equal_var= False)
