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

#creates smaller sample DF to test on
cp_mini= pd_cp_dataset.head()
behav_mini = pd_behav_dataset.head()
neo_mini = pd_neo_dataset.head()

print("neo mini\n", neo_mini.loc[:, 'sample_number'])
print("cp mini\n", cp_mini.loc[:, 'sample_number'])

#merge on inner should find same columns between the 2
inner = pd.merge(neo_mini, cp_mini, how='inner')
print(inner)

df1 = cp_mini.merge(neo_mini, on="sample_number")
print(df1)



"""
problems:
the merge is return an empty dataframe
"""



"""
#loc allows you to see specific rows/columns. 
#in this case rows 0-5 and column "sample_number"
print(pd_cp_dataset.loc[0:5, "sample_number"])
"""

