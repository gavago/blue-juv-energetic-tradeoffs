library(tidyverse)
library(lmerTest)

select <- dplyr::select # interplot also has select

source("functions/vif.mer function.R") # function is "vif.mer"


# create dataset 
full_data <- read.csv("data/full_dataset_juv_immune_energetics.csv", header = T)
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)

#add sample lbm change
full_data_short_term_lbm_change <- full_data %>% 
  group_by(subj) %>% 
  mutate(sample_lbm_change = lead(cr_resid) - cr_resid) %>%
  mutate(sample_interval = as.numeric(difftime(lead(date), date, units = "days"))) %>%
  # mutate(sample_interval = case_when(
  #   is.na(sample_interval) ~ NA_real_,
  #   TRUE ~ sample_interval)) %>% 
  ungroup() %>%
  mutate(log2_neo = log2(neo_sg))

#check alignment of data within rows
# full_data_short_term_lbm_change %>%
#     select(subj, date, sample_interval, sample_lbm_change, cr_resid, neo_sg) %>%
#     arrange(subj, date) %>%
#     view()

#save(full_data_short_term_lbm_change, file="data/full_data_short_term_lbm_change.Rdata")
