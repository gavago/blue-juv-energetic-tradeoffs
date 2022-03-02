import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

from datetime import datetime

import numpy as np
plt.style.use('ggplot')

df = pd.read_csv("merged dataset.csv", header=0)

#df['date'] = df['date'].strftime('%Y-%m-%d')

#convert to datetime
df['date'] = pd.to_datetime(df['date'])

#converts months to category
df['month'] = df['month'].astype('str')
df['month'] = df['month'].astype('category')
print(df['month'])

df['month'] = df['month'].cat.reorder_categories(['8', '9', '10', '11', '12', '1', '2' ,'3'], ordered = True)
#df = df.sort_values(by='month', ascending=True)
print(df['month'])

conditions = [
    (df["month"].eq('8')),
    (df["month"].eq('9')),
    (df["month"].eq('10')),
    (df["month"].eq('11')),
    (df["month"].eq('12')),
    (df["month"].eq('1')),
    (df["month"].eq('2')),
    (df["month"].eq('3'))
]
choices = [0, 1, 2, 3, 4, 5, 6, 7]

df["month_order"] = np.select(conditions, choices)

#separates males and females
sexes_groups = df.groupby('sex')
male_df = sexes_groups.get_group('M')
female_df = sexes_groups.get_group('F')

#groups by month and gets mean of stdsg_CP
#it is key to reset the index!!! otherwise we have issues plotting
male_months = male_df.groupby('month').mean(['stdsg_CP', 'neo_sg', 'pl', 'gm'])
male_months.reset_index(inplace = True)

female_months = female_df.groupby('month').mean(['stdsg_CP', 'neo_sg','pl', 'gm'])
female_months.reset_index(inplace=True)

all_months = df.groupby('month').mean(['stdsg_CP', 'neo_sg', 'pl', 'gm'])
all_months.reset_index(inplace=True)

print(female_months)
print(female_months.index)



#demonstrates plotting from multiple df on same graph
#plt.scatter(female_months['month_order'], female_months['pl'], color = 'red')

#sns.regplot(x=male_months.index, y=male_months['gm'])
#trying to plot from every datapoint, not mean
"""
sns.regplot(x = df['month_order'], y = df['gm'], lowess=True)
plt.clf()
plt.scatter(female_months['month_order'], female_months['pl'], color = 'red', ax = axes[0,0])
plt.scatter(male_months['month_order'], male_months['pl'], color = 'blue', ax = axes[0,0])
sns.regplot(x = male_months['month_order'], y = male_months['pl'], lowess = True, color = 'blue', ax = axes[0,0])
sns.regplot(x = female_months['month_order'], y = female_months['pl'], lowess = True, color = 'red', ax = axes[0,0])
"""
#makes the xticks months

plt.xticks([0,1,2,3,4,5,6,7], ['Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar'])


#tinker
fig, axes = plt.subplots(2,2)
# Set the ticks and ticklabels for all axes
plt.setp(axes, xticks=[0,1,2,3,4,5,6,7], xticklabels = ['Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar'])

#play data
axes[0,0].scatter(female_months['month_order'], female_months['pl'], color = 'red')
axes[0,0].scatter(male_months['month_order'], male_months['pl'], color = 'blue')
sns.regplot(x = male_months['month_order'], y = male_months['pl'],
            lowess = True, color = 'blue', ax = axes[0,0])
sns.regplot(x = female_months['month_order'], y = female_months['pl'], lowess = True, color = 'red', ax = axes[0,0])
#axes.xticks([0,1,2,3,4,5,6,7], ['Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar'])


#groom data
axes[1,0].scatter(female_months['month_order'], female_months['gm'], color = 'red')
axes[1,0].scatter(male_months['month_order'], male_months['gm'], color = 'blue')
sns.regplot(x = male_months['month_order'], y = male_months['gm'], lowess = True, color = 'blue', ax = axes[1,0])
sns.regplot(x = female_months['month_order'], y = female_months['gm'], lowess = True, color = 'red', ax = axes[1,0])

#neo data
axes[0,1].scatter(female_months['month_order'], female_months['neo_sg'], color = 'red')
axes[0,1].scatter(male_months['month_order'], male_months['neo_sg'], color = 'blue')
sns.regplot(x = male_months['month_order'], y = male_months['neo_sg'], lowess = True, color = 'blue', ax = axes[0,1])
sns.regplot(x = female_months['month_order'], y = female_months['neo_sg'], lowess = True, color = 'red', ax = axes[0,1])




#cpep data
axes[1,1].scatter(female_months['month_order'], female_months['stdsg_CP'], color = 'red')
axes[1,1].scatter(male_months['month_order'], male_months['stdsg_CP'], color = 'blue')
sns.regplot(x = male_months['month_order'], y = male_months['stdsg_CP'], lowess = True, color = 'blue', ax = axes[1,1])
sns.regplot(x = female_months['month_order'], y = female_months['stdsg_CP'], lowess = True, color = 'red', ax = axes[1,1])


axes[0,0].set(xlabel=None,
       title='Play')

plt.show()


