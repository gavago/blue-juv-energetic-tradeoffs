library(tidyverse)
library(lmerTest)

source("functions/vif.mer function.R") # vif.mer
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)




names(full_data_month)

# within ind - neopterin by energy balance and lean body mass -------

neo_cons_fun1 <- function(x) { lm(log2_avg_neo ~ 
       age +
       avg_cr_resid +
       log2_avg_cp_tar, data = x) %>%
    summary() %>%
    coef()}

full_data_month %>%
  group_by(subj) %>%
  group_map(~ neo_cons_fun1(.x)) %>%
  do.call("rbind", .) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  filter(grepl("log2_avg_cp_tar", rowname))

# wihin sex - neopterin by energy balance and lean body mass ------

neo_cons_fun2 <- function(x) { 
  lmer(log2_avg_neo ~ 
         age +
         mrank +
         avg_cr_resid +
         log2_avg_cp_tar + 
         (1|subj), data = x) %>%
    summary() %>%
    coef()}

full_data_month %>%
  group_by(sex) %>%
  group_map(~ neo_cons_fun2(.x)) %>%
  do.call("rbind", .) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  filter(grepl("log2_avg_cp_tar", rowname))
  
full_data_month$sex
