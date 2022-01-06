
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

# want to be able to:
# check dimension of a dataframe
# inspect dataframe in various ways, summarize, filter rows
# merge by relevant key / observation level
# Goal is to merge these 3 datasets together (behav observations will be repeated over different samples)
# an observation will be an subject's urine sample value of neopterin and c-peptide paired with its rate of play/grooming that month

# Ish to figure out:
# function to get objects in working environment?
# run single line
