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

wi_neo_cp_cr <- full_data_month %>%
  group_by(subj) %>%
  group_map(~ neo_cons_fun1(.x)) %>%
  do.call("rbind", .) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  filter(grepl("log2_avg_cp_tar|cr_resid", rowname))


# 30 of 41 within individual slopes neo ~ cp positive
wi_neo_cp_cr %>%
  filter(grepl("log2_avg_cp_tar", rowname)) %>%
  mutate(is_pos = Estimate>0) %>%
  count(is_pos)

# mean estimate and mean std error of within individual slope neo ~ cp 0.16 ± 0.33
wi_neo_cp_cr %>%
  filter(grepl("log2_avg_cp_tar", rowname)) %>%
  summarise(avg_neo_cp_slope = mean(Estimate), avg_SE = mean(`Std. Error`))


# 33 of 41 within individual slopes neo ~ cr positive
wi_neo_cp_cr %>%
  filter(grepl("cr_resid", rowname)) %>%
  mutate(is_pos = Estimate>0) %>%
  count(is_pos)

# mean estimate and mean std error of within individual slope neo ~ cr 1.8 ± 1.8
wi_neo_cp_cr %>%
  filter(grepl("cr_resid", rowname)) %>%
  summarise(avg_neo_cp_slope = mean(Estimate), avg_SE = mean(`Std. Error`))


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
