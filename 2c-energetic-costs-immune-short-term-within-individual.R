library(tidyverse)
library(lmerTest)
library(interplot)
select <- dplyr::select

source("functions/vif.mer function.R") # function is "vif.mer"
load("data/full_data_short_term_lbm_change.Rdata", verbose = T)
names(full_data_short_term_lbm_change) 


neo_stcost_fun <- function(x) { lm(sample_lbm_change ~ 
                                     age +
                                     log2_neo +
                                     log2_cp_tar +
                                     sample_interval +
                                     log2_neo*sample_interval +
                                     log2_cp_tar*sample_interval, data = x) %>%
    summary() %>%
    coef()}


wi_lbm_neo_cp <- full_data_short_term_lbm_change %>%
  group_by(subj) %>%
  group_map(~ neo_stcost_fun(.x)) %>%
  do.call("rbind", .) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  filter(grepl("log2_neo|log2_neo.sample", rowname))


# 26 of 41 slopes of lbm change ~ neo are negative, increase neo decrease change lbm
wi_lbm_neo_cp %>%
  filter(!grepl("sample", rowname)) %>%
  mutate(is_neg = Estimate<0) %>%
  count(is_neg)

# mean estimate of within individual slope lbm change ~ neo is negative, -0.15 ± 0.27 (mean std error)
wi_lbm_neo_cp %>%
  filter(!grepl("sample", rowname)) %>%
  summarise(avg_lbm_neo = mean(Estimate), avg_SE = mean(`Std. Error`, na.rm = T))

# less variation in interval length within individuals
# thus only 20 of 41 interactions of within indivdual slopes of neo*interval are postiive
  wi_lbm_neo_cp %>%
  filter(grepl("sample", rowname)) %>%
    mutate(is_pos = Estimate>0) %>%
    count(is_pos)
    
# nevertheless mean estimate of slope of interaction is positive, however small. 0.004 ± 0.018 (mean std error)
  wi_lbm_neo_cp %>%
  filter(grepl("sample", rowname)) %>%
  summarise(avg_int = mean(Estimate), sd_int = mean(`Std. Error`, na.rm = T))
  