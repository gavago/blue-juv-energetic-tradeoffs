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

#filter the final DF to subject, sample number, and month
#count how many samples per subject per month
filter_list = ['subj', 'sample_number', 'month']
final_df_checkker = final_df.filter(items=filter_list)
final_df_checkker = final_df_checkker.sort_values(by=['month', 'subj'], ascending=True)
#make a list of all different subj values
unique_values = final_df_checkker.subj.unique()
print(type(unique_values))
#iterate over each unique values

#creates empty dataframe with each month and subject type
df = pd.DataFrame(columns=[unique_values], index=[1,2,3,4,5,6,7,8,9,10,11,12])
#appends data
df.loc['1','allo'] = [1]
print(df)

#change range to (1,13) for each month
for month in range(1,13):
    for x in unique_values:
        # creates custom query for specific value and month
        query1 = "subj == '" + x + "' and month == " + str(month)
        #queries DF for specific subject and month
        #essentially final_df_checkker.query("subj == 'amos' and month ==1")
        z = final_df_checkker.query(query1)


        print(z, end=' \n')
        #counts number of rows and therefore samples in each sub DF

        print(z.shape[0])



#print(final_df_checkker)






"""
problems:
"""



"""
#loc allows you to see specific rows/columns. 
#in this case rows 0-5 and column "sample_number"
print(pd_cp_dataset.loc[0:5, "sample_number"])
"""

