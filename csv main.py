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
merge_list = ['month', 'year', 'subj']
bio_behav_merge_df = pd.merge(biomarker_df, pd_behav_dataset, how='left', on=merge_list)
bio_behav_merge_df["date"] = pd.to_datetime(bio_behav_merge_df['date'])
print(bio_behav_merge_df)
bio_behav_merge_df.to_csv("merged dataset.csv")



#create a behavior key dataframe
key_df = pd_behav_dataset.filter(['subj', 'month', 'year'])


#create a filtered DF from all the data
#groups the data by month and subj and then counts sample_number
subj_mo_yr_df = bio_behav_merge_df.groupby(['subj','month','year'], as_index = False)['sample_number'].count()
subj_mo_yr_df = subj_mo_yr_df.rename(columns={"sample_number":"count"})

freq_subj_mo_yr_df = pd.merge(key_df, subj_mo_yr_df, how = 'left')
print('hello')
#fill na's with zeros
freq_subj_mo_yr_df =freq_subj_mo_yr_df.fillna(0)
print(freq_subj_mo_yr_df)

#average frequency per month
avg_subj_mo = freq_subj_mo_yr_df.groupby([ 'month'])['count'].mean()
print(avg_subj_mo.describe())



"""
#filter the final DF to subject, sample number, and month
#count how many samples per subject per month
filter_list = ['subj', 'sample_number', 'month']
bio_behav_merge_df_checkker = bio_behav_merge_df.filter(items=filter_list)
bio_behav_merge_df_checkker = bio_behav_merge_df_checkker.sort_values(by=['month', 'subj'], ascending=True)
#converts string to date
"""


"""
problems:
"""



"""
#loc allows you to see specific rows/columns. 
#in this case rows 0-5 and column "sample_number"
print(pd_cp_dataset.loc[0:5, "sample_number"])
"""

