import pandas as pd
import numpy as np
import os

""""#the os.listdir command tells you what files are in the available directory
cwd = os.getcwd()
files = os.listdir(cwd)
print(files)
"""

#converst csv files to pd DataFrames
#header=0 ensures the first row is column names
#na_values turns a value into NAN
pd_cp_dataset = pd.read_csv("cp dataset full.csv", header=0, na_values='NA')
pd_behav_dataset = pd.read_csv("behav dataset month.csv", header=0, na_values='NA')
pd_neo_dataset = pd.read_csv("neo dataset full.csv", header=0, na_values='NA')

#creates a list of things we want to merge on between cp and neo datasets
cp_neo_merge_list = ['group', 'subj', 'date', 'month', 'year', 'time', "sample_number", 'Cr', 'SG']

#merges cp and neo
biomarker_df = pd_cp_dataset.merge(pd_neo_dataset, on=cp_neo_merge_list)

#merge biomarker and behavior based on month,year, and subject id
final_merge_list = ['month', 'year', 'subj']
final_df = pd.merge(biomarker_df, pd_behav_dataset, how='left', on=final_merge_list)

final_df.to_csv("merged dataset.csv")

#filter the final DF to subject, sample number, and month
#count how many samples per subject per month
filter_list = ['subj', 'sample_number', 'month']
final_df_checkker = final_df.filter(items=filter_list)
final_df_checkker = final_df_checkker.sort_values(by=['month', 'subj'], ascending=True)
#converts string to date
final_df["date"] = pd.to_datetime(final_df['date'])

#groups the data by month and subj and then counts sample_number
graph_df = final_df.groupby(['month','subj'],as_index = False)["sample_number"].count()
graph_df = graph_df.rename(columns={"sample_number":"count"})

#sets index to month
graph_df.set_index('month', inplace= True)
#reshapes df
graph_df = graph_df.pivot(columns='subj', values='count')
#converts NAN to 0
graph_df = graph_df.fillna(0)

print(graph_df)




"""
problems:
"""



"""
#loc allows you to see specific rows/columns. 
#in this case rows 0-5 and column "sample_number"
print(pd_cp_dataset.loc[0:5, "sample_number"])
"""

