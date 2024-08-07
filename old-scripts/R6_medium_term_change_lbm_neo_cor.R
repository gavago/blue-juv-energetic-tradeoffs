library(lmerTest)
library(mgcv)
library(tidyverse)
library(dplyr)
# load datasets
full_data <- read.csv("data/full_dataset_juv_immune_energetics.csv", header = T)
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)

# viz: look at fluctuations in cr_resid ----
full_data_rownum <- full_data %>% 
  arrange(subj, date) %>% 
  group_by(subj, date) %>% 
  rowid_to_column(var = "rowid") %>% 
  ungroup()

full_data_month_rownum <- full_data_month %>% 
  arrange(subj, year, month) %>% 
  group_by(subj, year, month) %>% 
  rowid_to_column(var = "rowid") %>% 
  ungroup()

full_data_rownum %>% filter(subj == "bike") %>% 
  ggplot() + 
  geom_line(aes(x = rowid, y = cr_resid, group = subj, color = subj))

full_data_rownum %>% filter(rowid < 102) %>% 
  ggplot() + 
  geom_line(aes(x = rowid, y = cr_resid, color = subj))

full_data_month_rownum %>% filter(rowid < 102) %>% 
  ggplot() + 
  geom_line(aes(x = rowid, y = avg_cr_resid, color = subj))

full_data_rownum %>% filter(rowid > 101, rowid < 205) %>% 
  ggplot() + 
  geom_smooth(aes(x = rowid, y = cr_resid, color = subj))

full_data_rownum %>% filter(rowid > 200, rowid < 300) %>% 
  ggplot() + 
  geom_line(aes(x = rowid, y = cr_resid, color = subj))

full_data_month_rownum %>% filter(rowid > 200, rowid < 300) %>% 
  ggplot() + 
  geom_line(aes(x = rowid, y = avg_cr_resid, color = subj))

# viz: fluctuations in neo

full_data_rownum %>% filter(rowid > 200, rowid < 300) %>% 
  ggplot() + 
  geom_line(aes(x = rowid, y = avg_neo_sg, color = subj))

full_data_month_rownum %>% filter(rowid > 200, rowid < 300) %>% 
  ggplot() + 
  geom_line(aes(x = rowid, y = avg_neo_sg, color = subj))

# make new dataset for regression ----

full_data_chng_lbm <- full_data_month_rownum %>% 
  drop_na(avg_cr_resid) %>% 
  group_by(subj) %>% 
  summarize(change_lbm = first(avg_cr_resid) - last(avg_cr_resid),
                                                    sex = sex, 
            initial_age = first(age)) %>% 
  ungroup() %>% 
  distinct()
# used initial age

full_data_avg_neo <- full_data %>% 
  drop_na(neo_sg) %>% 
  group_by(subj) %>% 
  summarize(avg_neo_all_months = mean(neo_sg)) %>% 
  ungroup()

full_data_growth_neo <- full_join(full_data_avg_neo, full_data_chng_lbm,  
                                  by = intersect(names(full_data_avg_neo), names(full_data_chng_lbm)))

# regression ----
# had to use glm bc there is no change over time

change_lbm_avg_neo_glm <- glm(change_lbm ~ log2(avg_neo_all_months) + 
        initial_age + 
        sex,
        data = full_data_growth_neo)

qqnorm(residuals(change_lbm_avg_neo_glm))
qqline(residuals(change_lbm_avg_neo_glm))

summary(change_lbm_avg_neo_glm)

#cor.test(change_lbm, avg_neo) ----

cor.test(full_data_growth_neo$change_lbm, 
         full_data_growth_neo$avg_neo_all_months,
        alternative = c("two.sided"),
        method = c("pearson"),
        conf.level = 0.95,
        continuity = FALSE )
  