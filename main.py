
#https://www.youtube.com/watch?v=a8MckiothGc&ab_channel=TheoSuciu

#import pyreadr
#result = pyreadr.read_r("neo dataset full.Rdata")
# print(result.keys())
import pandas as pd
import os
print(os.getcwd())

import rpy2.robjects as robjects

robjects.r['load']("cp dataset full.Rdata")

cp_raw = robjects.r['cp_raw']

print(cp_raw.head())