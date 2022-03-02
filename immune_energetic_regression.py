import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm
import numpy as np


df = pd.read_csv("merged dataset.csv", header=0)


df = df.filter(items = ['group',
       'subj', 'date', 'time', 'sample_number', 'Cr', 'SG', 'Cr.plate.number',
       'Cr.date', 'cp_dilution', 'cp_run', 'cp_assay_date', 'pg.ml_ucp',
       'cp_batch', 'stdcr_CP', 'stdsg_CP', 'cp_note', 'month', 'year', 'sex',
       'age', 'month.name', 'mum', 'bday', 'weaning', 'mid', 'neo_value', 'CV',
       'neo_plate_number', 'neo_dilution', 'neo_date_assayed', 'neo_note',
       'SG_corr_fac', 'neo_sg', 'month_name', 'pl', 'gmd', 'gm', 'r',
       'n_agg_g', 'n_agg_r', 'n_obs', 'date_order'])

#checks which columns have the na
#print(df.isna().sum())

#remove na from neo_sg
df = df.dropna(subset = ['neo_sg', 'stdsg_CP'])



#separates males and females df
sexes_groups = df.groupby('sex')
male_df = sexes_groups.get_group('M')
female_df = sexes_groups.get_group('F')

#tinker with statsmodel


#energy balance and cellular immunity
neo_cp_model = sm.OLS(df['stdsg_CP'], df['neo_sg']).fit()
print(neo_cp_model.summary())

fig = sm.graphics.plot_regress_exog(neo_cp_model, 'neo_sg')


#creatine/sg resid
cr_resid_model = sm

#

"""
#my attempt at multivariate

X = df[['stdsg_CP', 'neo_sg']]
X = sm.add_constant(X)
model = sm.OLS( df['gm'], X)
fit = model.fit()
print(fit.summary())

fig = sm.graphics.plot_regress_exog(fit, 'gm')
"""
plt.show()
#plot that works...
"""
fig, axes = plt.subplots(2,2)
#add padding between plots
fig.tight_layout(h_pad=2)

#energy balance and cellular immunity
axes[0,0].scatter(female_df['stdsg_CP'], female_df['neo_sg'], color = 'red', alpha = 0.3, s = 10)
axes[0,0].scatter(male_df['stdsg_CP'], male_df['neo_sg'], color = 'blue', alpha = 0.3, s =10)
sns.regplot(x = male_df['stdsg_CP'], y = male_df['neo_sg'],
             color = 'blue', ax = axes[0,0], scatter = False)
sns.regplot(x = female_df['stdsg_CP'], y = female_df['neo_sg'],color = 'red', ax = axes[0,0], scatter = False)

#Immune function and growth tradeoff
axes[0,1].scatter(female_df['stdsg_CP'], female_df['neo_sg'], color = 'red', alpha = 0.3, s = 10)
axes[0,1].scatter(male_df['stdsg_CP'], male_df['neo_sg'], color = 'blue', alpha = 0.3, s =10)
sns.residplot(x = male_df['stdsg_CP'], y = male_df['neo_sg'],
             color = 'blue', ax = axes[0,1])
sns.residplot(x = female_df['stdsg_CP'], y = female_df['neo_sg'],color = 'red', ax = axes[0,1])

#affiliative behavior and immunity
axes[1,0].scatter(female_df['neo_sg'], female_df['Cr'] / female_df['SG'], color = 'red', alpha = 0.3, s = 10)
axes[1,0].scatter(male_df['neo_sg'], male_df['Cr'] / male_df['SG'], color = 'blue', alpha = 0.3, s =10)
sns.regplot(x = male_df['neo_sg'], y = male_df['Cr'] / male_df['SG'],
             color = 'blue', ax = axes[1,0], scatter = False)
sns.regplot(x = female_df['neo_sg'], y = female_df['Cr'] / female_df['SG'], color = 'red', ax = axes[1,0], scatter = False)

#affiliative behavior and immunity
axes[1,1].scatter(female_df['neo_sg'], female_df['gm'], color = 'red', alpha = 0.3, s = 10)
axes[1,1].scatter(male_df['neo_sg'], male_df['pl'], color = 'blue', alpha = 0.3, s =10)
sns.regplot(x = male_df['neo_sg'], y = male_df['pl'],
             color = 'blue', ax = axes[1,1], scatter = False)
sns.regplot(x = female_df['neo_sg'], y = female_df['gm'] , color = 'red', ax = axes[1,1], scatter = False)



plt.show()

"""