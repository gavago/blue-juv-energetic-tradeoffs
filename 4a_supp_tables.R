library(tidyverse)

#create S1 table for uData

load("data/full_data_short_term_lbm_change.Rdata", verbose = T)
s1_udata <- full_data_short_term_lbm_change %>% 
  group_by(sex) %>% 
  summarize(avg_neo = mean(neo_sg, na.rm = T), 
           sd_neo = sd(neo_sg, na.rm = T), 
           avg_cp = mean(cp_tar, na.rm = T),
           sd_cp = sd(cp_tar, na.rm = T),
           avg_cr = mean(cr_resid, na.rm = T),
           sd_cr = sd(cr_resid, na.rm = T)) %>% 
  ungroup()

load("data/fgc_data_by_sample.Rdata", verbose = T)

# add sex to gc_raw

gc_raw <-gc_raw %>% 
  inner_join(full_data_short_term_lbm_change %>% 
  select(subj, sex) %>% 
    distinct())

# create S1 table for fgc
s1_fgc <- gc_raw %>% 
  group_by(sex) %>% 
  summarize(avg_fgc = mean(fgc.ng_g.feces, na.rm = T), 
             sd_fgc = sd(fgc.ng_g.feces, na.rm = T)) %>% 
  ungroup()

# bind urine and fecal tables

s1_table <- bind_cols(s1_udata, s1_fgc)[,-8]

write.table(s1_table, file = "results/tables/s1_table.txt", 
            quote = FALSE, sep = "/", row.names = F)

