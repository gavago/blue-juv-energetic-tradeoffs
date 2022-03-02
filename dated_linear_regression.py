import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from datetime import datetime as dt
import statsmodels.api as sm
import seaborn as sns
import numpy as np



df = pd.read_csv("merged dataset.csv", header=0)
#convert to datetime
df['date'] = pd.to_datetime(df['date'])
df = df.sort_values(by = ['date'])
#df.set_index('date', inplace=True)


unique_dates = df['date'].unique()

#creates a list of choices for date order
choices = list(range(0,134))
print(choices)

#this creates a string of conditions, which I took into google docs and parsed so I could use for np select
conditions = []
for i in range(len(unique_dates)):
    #print(unique_dates[i])
    condition_str = f"(df['date'].eq('{unique_dates[i]}')),"
    conditions.append(condition_str)

cond_ = [
(df['date'].eq('2015-08-04T00:00:00.000000000')), (df['date'].eq('2015-08-05T00:00:00.000000000')), (df['date'].eq('2015-08-06T00:00:00.000000000')), (df['date'].eq('2015-08-10T00:00:00.000000000')), (df['date'].eq('2015-08-11T00:00:00.000000000')), (df['date'].eq('2015-08-12T00:00:00.000000000')), (df['date'].eq('2015-08-13T00:00:00.000000000')), (df['date'].eq('2015-08-14T00:00:00.000000000')), (df['date'].eq('2015-08-17T00:00:00.000000000')), (df['date'].eq('2015-08-18T00:00:00.000000000')), (df['date'].eq('2015-08-19T00:00:00.000000000')), (df['date'].eq('2015-08-20T00:00:00.000000000')), (df['date'].eq('2015-08-21T00:00:00.000000000')), (df['date'].eq('2015-08-24T00:00:00.000000000')), (df['date'].eq('2015-08-25T00:00:00.000000000')), (df['date'].eq('2015-08-26T00:00:00.000000000')), (df['date'].eq('2015-08-27T00:00:00.000000000')), (df['date'].eq('2015-08-28T00:00:00.000000000')), (df['date'].eq('2015-08-31T00:00:00.000000000')), (df['date'].eq('2015-09-01T00:00:00.000000000')), (df['date'].eq('2015-09-02T00:00:00.000000000')), (df['date'].eq('2015-09-03T00:00:00.000000000')), (df['date'].eq('2015-09-04T00:00:00.000000000')), (df['date'].eq('2015-09-07T00:00:00.000000000')), (df['date'].eq('2015-09-08T00:00:00.000000000')), (df['date'].eq('2015-09-09T00:00:00.000000000')), (df['date'].eq('2015-09-10T00:00:00.000000000')), (df['date'].eq('2015-09-11T00:00:00.000000000')), (df['date'].eq('2015-09-14T00:00:00.000000000')), (df['date'].eq('2015-09-15T00:00:00.000000000')), (df['date'].eq('2015-09-16T00:00:00.000000000')), (df['date'].eq('2015-09-17T00:00:00.000000000')), (df['date'].eq('2015-09-18T00:00:00.000000000')), (df['date'].eq('2015-09-22T00:00:00.000000000')), (df['date'].eq('2015-09-24T00:00:00.000000000')), (df['date'].eq('2015-09-25T00:00:00.000000000')), (df['date'].eq('2015-09-28T00:00:00.000000000')), (df['date'].eq('2015-09-29T00:00:00.000000000')), (df['date'].eq('2015-09-30T00:00:00.000000000')), (df['date'].eq('2015-10-01T00:00:00.000000000')), (df['date'].eq('2015-10-02T00:00:00.000000000')), (df['date'].eq('2015-10-05T00:00:00.000000000')), (df['date'].eq('2015-10-06T00:00:00.000000000')), (df['date'].eq('2015-10-07T00:00:00.000000000')), (df['date'].eq('2015-10-08T00:00:00.000000000')), (df['date'].eq('2015-10-09T00:00:00.000000000')), (df['date'].eq('2015-10-12T00:00:00.000000000')), (df['date'].eq('2015-10-13T00:00:00.000000000')), (df['date'].eq('2015-10-15T00:00:00.000000000')), (df['date'].eq('2015-10-16T00:00:00.000000000')), (df['date'].eq('2015-10-19T00:00:00.000000000')), (df['date'].eq('2015-10-20T00:00:00.000000000')), (df['date'].eq('2015-10-21T00:00:00.000000000')), (df['date'].eq('2015-10-22T00:00:00.000000000')), (df['date'].eq('2015-10-23T00:00:00.000000000')), (df['date'].eq('2015-10-26T00:00:00.000000000')), (df['date'].eq('2015-10-27T00:00:00.000000000')), (df['date'].eq('2015-10-28T00:00:00.000000000')), (df['date'].eq('2015-10-29T00:00:00.000000000')), (df['date'].eq('2015-11-03T00:00:00.000000000')), (df['date'].eq('2015-11-04T00:00:00.000000000')), (df['date'].eq('2015-11-09T00:00:00.000000000')), (df['date'].eq('2015-11-11T00:00:00.000000000')), (df['date'].eq('2015-11-12T00:00:00.000000000')), (df['date'].eq('2015-11-13T00:00:00.000000000')), (df['date'].eq('2015-11-18T00:00:00.000000000')), (df['date'].eq('2015-11-19T00:00:00.000000000')), (df['date'].eq('2015-11-23T00:00:00.000000000')), (df['date'].eq('2015-11-24T00:00:00.000000000')), (df['date'].eq('2015-11-25T00:00:00.000000000')), (df['date'].eq('2015-11-26T00:00:00.000000000')), (df['date'].eq('2015-11-27T00:00:00.000000000')), (df['date'].eq('2015-12-01T00:00:00.000000000')), (df['date'].eq('2015-12-03T00:00:00.000000000')), (df['date'].eq('2015-12-04T00:00:00.000000000')), (df['date'].eq('2015-12-07T00:00:00.000000000')), (df['date'].eq('2015-12-18T00:00:00.000000000')), (df['date'].eq('2015-12-19T00:00:00.000000000')), (df['date'].eq('2015-12-21T00:00:00.000000000')), (df['date'].eq('2015-12-22T00:00:00.000000000')), (df['date'].eq('2015-12-23T00:00:00.000000000')), (df['date'].eq('2015-12-28T00:00:00.000000000')), (df['date'].eq('2015-12-29T00:00:00.000000000')), (df['date'].eq('2015-12-30T00:00:00.000000000')), (df['date'].eq('2015-12-31T00:00:00.000000000')), (df['date'].eq('2016-01-04T00:00:00.000000000')), (df['date'].eq('2016-01-06T00:00:00.000000000')), (df['date'].eq('2016-01-07T00:00:00.000000000')), (df['date'].eq('2016-01-08T00:00:00.000000000')), (df['date'].eq('2016-01-11T00:00:00.000000000')), (df['date'].eq('2016-01-12T00:00:00.000000000')), (df['date'].eq('2016-01-13T00:00:00.000000000')), (df['date'].eq('2016-01-14T00:00:00.000000000')), (df['date'].eq('2016-01-15T00:00:00.000000000')), (df['date'].eq('2016-01-20T00:00:00.000000000')), (df['date'].eq('2016-01-21T00:00:00.000000000')), (df['date'].eq('2016-01-22T00:00:00.000000000')), (df['date'].eq('2016-01-25T00:00:00.000000000')), (df['date'].eq('2016-01-26T00:00:00.000000000')), (df['date'].eq('2016-01-27T00:00:00.000000000')), (df['date'].eq('2016-01-28T00:00:00.000000000')), (df['date'].eq('2016-02-01T00:00:00.000000000')), (df['date'].eq('2016-02-02T00:00:00.000000000')), (df['date'].eq('2016-02-03T00:00:00.000000000')), (df['date'].eq('2016-02-04T00:00:00.000000000')), (df['date'].eq('2016-02-05T00:00:00.000000000')), (df['date'].eq('2016-02-08T00:00:00.000000000')), (df['date'].eq('2016-02-09T00:00:00.000000000')), (df['date'].eq('2016-02-10T00:00:00.000000000')), (df['date'].eq('2016-02-11T00:00:00.000000000')), (df['date'].eq('2016-02-15T00:00:00.000000000')), (df['date'].eq('2016-02-16T00:00:00.000000000')), (df['date'].eq('2016-02-17T00:00:00.000000000')), (df['date'].eq('2016-02-18T00:00:00.000000000')), (df['date'].eq('2016-02-19T00:00:00.000000000')), (df['date'].eq('2016-02-22T00:00:00.000000000')), (df['date'].eq('2016-02-23T00:00:00.000000000')), (df['date'].eq('2016-02-24T00:00:00.000000000')), (df['date'].eq('2016-03-08T00:00:00.000000000')), (df['date'].eq('2016-03-10T00:00:00.000000000')), (df['date'].eq('2016-03-11T00:00:00.000000000')), (df['date'].eq('2016-03-12T00:00:00.000000000')), (df['date'].eq('2016-03-14T00:00:00.000000000')), (df['date'].eq('2016-03-15T00:00:00.000000000')), (df['date'].eq('2016-03-16T00:00:00.000000000')), (df['date'].eq('2016-03-17T00:00:00.000000000')), (df['date'].eq('2016-03-18T00:00:00.000000000')), (df['date'].eq('2016-03-21T00:00:00.000000000')), (df['date'].eq('2016-03-22T00:00:00.000000000')), (df['date'].eq('2016-03-23T00:00:00.000000000')), (df['date'].eq('2016-03-24T00:00:00.000000000')), (df['date'].eq('2016-03-25T00:00:00.000000000')), (df['date'].eq('2016-03-28T00:00:00.000000000')), (df['date'].eq('2016-03-29T00:00:00.000000000'))
    ]

df['date_order'] = np.select(cond_,choices)

print(df['date_order'])

#add ordered dates to csv
df.to_csv("merged dataset.csv")

#find the minimum date of each month
month_groups = df.groupby('month')
#print grouped['Points'].agg(np.mean)

print(month_groups['date_order'].agg('min'))

""" attempt to plot"""
X = df['date_order']
Y = df['pl']

X = sm.add_constant(X) # adding a constant

model = sm.OLS(Y, X).fit()
predictions = model.predict(X)

print_model = model.summary()
print(print_model)

#smoothed = sm.nonparametric.lowess(exog=X, endog=Y, frac=0.2)

#plt.plot(smoothed[:, 0], smoothed[:, 1], c="k")


#plt.plot(X, model.fittedvalues, 'r--.', label="OLS")

#plt.show()


#tinker
fig, axes = plt.subplots(2,2)
plt.setp(axes, xticks=[0,19,39,59,72,85,101,118], xticklabels = ['Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar'])

#axes[0,0].scatter(df['date_order'], df['pl'], color = 'red')
sns.regplot(x = df['date_order'], y = df['pl'], lowess = True, color = 'blue', ax = axes[0,0])

plt.show()

