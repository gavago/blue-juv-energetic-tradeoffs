#https://www.youtube.com/watch?v=a8MckiothGc&ab_channel=TheoSuciu
#https://stackoverflow.com/questions/21288133/loading-rdata-files-into-python

#import pyreadr
#result = pyreadr.read_r("neo dataset full.Rdata")
# print(result.keys())
import pandas as pd
import os
print('the final push')
import numpy
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri

from rpy2.robjects.conversion import localconverter

robjects.r['load']("cp dataset full.Rdata")
robjects.r['load']("neo dataset full.Rdata")
robjects.r['load']("behav dataset month.Rdata")

cp_raw = robjects.r['cp_raw']
neo_raw = robjects.r['neo_data_full']
behav_raw = robjects.r['behav_data_month']

#converts cp_raw into a DF
cp_raw_df = robjects.DataFrame(cp_raw)
#the type is a <class 'rpy2.robjects.vectors.DataFrame'>
print(type(cp_raw_df))



#attempts to convert <class 'rpy2.robjects.vectors.DataFrame'> into a pd.DataFrame
pd_cp_raw_df = pd.DataFrame(cp_raw_df)
#I also tried with just the raw Robject
#pd_cp_raw_df = pd.DataFrame(cp_raw)

print(type(pd_cp_raw_df))




# want to be able to:
# check dimension of a dataframe
# inspect dataframe in various ways, summarize, filter rows
# merge by relevant key / observation level
# Goal is to merge these 3 datasets together (behav observations will be repeated over different samples)
# an observation will be an subject's urine sample value of neopterin and c-peptide paired with its rate of play/grooming that month

# Ish to figure out:
# function to get objects in working environment?
# run single line