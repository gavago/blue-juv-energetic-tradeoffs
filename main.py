
#https://www.youtube.com/watch?v=a8MckiothGc&ab_channel=TheoSuciu
#https://stackoverflow.com/questions/21288133/loading-rdata-files-into-python

#import pyreadr
#result = pyreadr.read_r("neo dataset full.Rdata")
# print(result.keys())
import pandas as pd
import os
print(os.getcwd())

import rpy2.robjects as robjects

robjects.r['load']("cp dataset full.Rdata")
robjects.r['load']("neo dataset full.Rdata")
robjects.r['load']("behav dataset month.Rdata")

cp_raw = robjects.r['cp_raw']
neo_raw = robjects.r['neo_data_full']
behav_raw = robjects.r['behav_data_month']

print(cp_raw.head())

# dimension of a dataframe
# inspecting/filtering rows
# merge by relevant key / observation level

# Goal is to merge these 3 datasets together (behav observations will be repeated over different samples)
# draft of data dictionary:
# cp_raw, neo_raw is biomarker data: are both at subject sample level
# every row is a unique sample number, which is an subject sampled on a given date and time
# behav dataset, is a unique subject-month of behavioral observation, and variables
# in here are proportion of observed time in a given behavioral activity (giving grooming, playing, etc.)
# or number of counts in a given month that a subject gave or received aggression.



# Ish to figure out:
# function to get objects in working environment?
# run single line