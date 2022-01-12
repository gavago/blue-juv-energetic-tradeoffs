import pandas as pd
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

print(biomarker_df.columns)
print(biomarker_df.loc[0:5, 'year'])




"""
problems:
"""



"""
#loc allows you to see specific rows/columns. 
#in this case rows 0-5 and column "sample_number"
print(pd_cp_dataset.loc[0:5, "sample_number"])
"""

