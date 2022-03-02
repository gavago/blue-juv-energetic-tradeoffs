import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import statsmodels.api as sm

plt.style.use('ggplot')

df = pd.read_csv("cpep_mon_avg_dataset.csv", header=0)

#convert to datetime
df['date'] = pd.to_datetime(df['date'])

fig,ax = plt.subplots(nrows=1, ncols=1)

#scatter plot of month and cpep_avg
plt.scatter(df['month_order'], df['cpep_avg'], color = 'blue')

plt.scatter(df['month_order'], df['stdsg_CP'], color = 'blue', s = 1)

#best fit line operations
df = df.dropna(subset = ['stdsg_CP'])
theta = np.polyfit(df['month_order'], df['stdsg_CP'], 1)
y_line = theta[1] + theta[0] * df['month_order']

plt.plot(df['month_order'],y_line, color = 'red')

"""
Best fit lines for male / female
"""

print(df.groupby(['sex', 'month']).mean('stdsg_CP'))

#Create male/female DF
sex_groups =  df.groupby('sex')
male_df = sex_groups.get_group('M')
female_df = sex_groups.get_group('F')

#best fit using statsmodel
sm_x = male_df['month_order']
sm_x = sm_x.astype('float')
sm_y = male_df['stdsg_CP']
sm_y = sm_y.astype('float')

model = sm.OLS(sm_x, sm_y).fit()
print(model.summary())

"""
#male best fit (green/solid)
male_theta = np.polyfit(male_df['month_order'], male_df['stdsg_CP'], 1)
male_y_line = male_theta[1] + male_theta[0] * male_df['month_order']
plt.plot(male_df['month_order'], male_y_line, color = 'green')

#female best fit (green/dashed)
female_theta = np.polyfit(female_df['month_order'], female_df['stdsg_CP'], 1)
female_y_line = female_theta[1] + female_theta[0] * female_df['month_order']
plt.plot(female_df['month_order'], female_y_line, color = 'green', linestyle = 'dashed')
"""
#label y axiz
plt.ylabel("ng/mL", labelpad = 4)

#create a key for the x axis
plt.xticks([1,2,3,4,5,6,7,8], ['Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar'])

plt.title("Monthly Cpeptide")

plt.show()




