library(tidyverse)
library(lmerTest)

select <- dplyr::select # interplot also has select

source("functions/vif.mer function.R") # function is "vif.mer"


# create dataset 
load("data/urine_sample_dataset_juv_immune_energetics.Rdata", verbose = T)
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)

#add sample lbm change
full_data_short_term_lbm_change <- full_udata %>% 
  group_by(subj) %>% 
  mutate(sample_lbm_change = lead(cr_resid) - cr_resid) %>%
  mutate(sample_interval = as.numeric(difftime(lead(date), date, units = "days"))) %>%
  # mutate(sample_interval = case_when(
  #   is.na(sample_interval) ~ NA_real_,
  #   TRUE ~ sample_interval)) %>% 
  ungroup() %>%
  mutate(log2_neo = log2(neo_sg)) %>%
  mutate(log2_cp = log2(stdsg_CP))

#check alignment of data within rows
full_data_short_term_lbm_change %>%
    select(subj, date, sample_interval, sample_lbm_change, cr_resid, neo_sg) %>%
    arrange(subj, date) %>%
    View()

#save(full_data_short_term_lbm_change, file="data/full_data_short_term_lbm_change.Rdata")
