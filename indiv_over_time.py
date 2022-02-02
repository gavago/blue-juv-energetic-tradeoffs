import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

"""
Task:
1. Visualize a single individual over time
	filter og DF to one alloy (create a definition so that you can just input monkey name) (which is name of monkey)
	y axis = stdsg_cp
	x axis the date
	line plot of the different cpeptide  values
"""


df = pd.read_csv("merged dataset.csv", header=0)

#convert to datetime
df['date'] = pd.to_datetime(df['date'])

#function to plot cpeptide values over time
#parameter subj is the subj you would like to see the data for
def plot_indiv_cpep(subj):
    filtered_group = df.groupby('subj', as_index=False)
    indiv_df = filtered_group.get_group((f'{subj}'))

    indiv_df.plot(kind='line',
                  x='date',
                  y='stdsg_CP',
                  xlabel="Date",
                  ylabel='Cpeptide',
                  title=f'{subj}',
                  color='red',
                  # lw is line width
                  lw=6,
                  figsize=(12, 6)
                  )
    plt.show()
    return

plot_indiv_cpep('allo')

