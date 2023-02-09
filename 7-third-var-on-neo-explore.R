library(tidyverse)
library(lmerTest)

source("functions/vif.mer function.R") # vif.mer
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)

# would need to break down point sample + partner raw data
# and summarize total number of partners per month, have done before



names(full_data_month)

# total time overtly socializing not associated w neo
neo_time_soc_month <- full_data_month %>%
  mutate(time_soc = pl + gmd + gm) %>%
  lmer(log2_avg_neo ~  sex +
                       age +
                       mrank +
                       time_soc +
                       log2_avg_cp_tar +
                       (1|subj), data = .)
summary(neo_time_soc_month)


#lbm not associated w pl or gm social behavior
lbm_soc_month <- full_data_month %>%
  mutate(time_soc = pl + gmd + gm) %>%
  lmer(avg_cr_resid ~  sex +
         age +
         mrank +
         pl + gmd + gm +
         log2_avg_cp_tar +
         (1|subj), data = .)
summary(lbm_soc_month)
