library(tidyverse)
library(janitor)
library(lmerTest)


# load datasets
load("data/urine_sample_dataset_juv_immune_energetics.Rdata", verbose = T)
load("data/fgc_data_by_sample.Rdata", verbose = T)
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)
load("data/full_data_short_term_lbm_change.Rdata", verbose = T)

# Ns and samples per subject month -----

# total urine
nrow(full_udata)
# total fecal - fgc
gc_raw %>%
  filter(!is.na(fgc.ng_g.feces)) %>% nrow()


# neopterin
full_udata %>%
  filter(!is.na(neo_sg)) %>% nrow()

# could replace followig w LoD, could possibly also replace high CV samples bc so low to LoD with LoD, 
# but seems questionable practice
# full_udata %>%
#   filter(sample_number == 94) %>% select(SG) 


# cpeptide
full_udata %>%
  filter(!is.na(stdsg_CP)) %>% nrow()


nrow(gc_raw)

# samples per subj/month ------

# total urine
full_udata %>% 
  count(subj, month) %>%
  summarise(mean = mean(n, na.rm = T), sd = sd(n, na.rm = T)) 
# total fecal
gc_raw %>%
  count(subj, month) %>%
  summarise(mean = mean(n, na.rm = T), sd = sd(n, na.rm = T))



#neo
neo_subj_month <- full_udata %>%
  filter(!is.na(neo_sg)) %>%
  count(subj, month) %>%
  summarise(mean = mean(n, na.rm = T), sd = sd(n, na.rm = T)) %>%
  mutate(biomarker = "neopterin")

#cp
cp_subj_month <-full_udata %>%
  filter(!is.na(stdsg_CP)) %>%
  count(subj, month) %>%
  summarise(mean = mean(n, na.rm = T), sd = sd(n, na.rm = T))  %>%
  mutate(biomarker = "C-peptide")


#lbm
lbm_subj_month <-full_udata %>%
  filter(!is.na(cr_resid)) %>%
  count(subj, month) %>%
  summarise(mean = mean(n, na.rm = T), sd = sd(n, na.rm = T))  %>%
  mutate(biomarker = "LBM")


#gc
gc_subj_month <- gc_raw %>%
  #filter(!is.na(fgc.ng_g.feces)) %>%
  count(subj, month) %>%
  summarise(mean = mean(n, na.rm = T), sd = sd(n, na.rm = T))  %>%
  mutate(biomarker = "fGCs")


all <- list(neo_subj_month, cp_subj_month, lbm_subj_month, gc_subj_month)
monthly_biomarker_tab <- do.call("rbind", all) %>%
  dplyr::select(biomarker, mean, sd) %>%
  rename(mean_samples = mean)

#write.table(monthly_biomarker_tab, file = "results/tables/month_biomarker_counts.txt", sep = "/", quote = F)
# average biomarker values per subject ------

avg_sd <- list(
  avg = ~mean(.x, na.rm = T),
  sd = ~sd(.x, na.rm = T))

full_udata %>%
  summarise(across(c(neo_sg, stdsg_CP, cr_resid), avg_sd)) #~ mean(.x, na.rm = T)
#pivot longer so Neo, Cp, cr resid each a row

gc_raw %>%
  summarise(avg = mean(fgc.ng_g.feces, na.rm =T), sd = sd(fgc.ng_g.feces, na.rm =T)) %>%
  mutate(marker = "fGCs") %>%
  dplyr::select(marker, everything())
  


# intra-assay CV neo ----
mean(full_udata$neo_CV, na.rm = T)
sd(full_udata$neo_CV, na.rm = T)


# inter-assay CV neo ----
neo_qchl <- read_csv("Neo_shared_w_Josh_C/QCH-QCL-assays.csv") %>%
  clean_names() %>% 
  mutate(control = case_when(
    control == "QCH" ~ "QCL", #labels qch and qcl are reversed
    control == "QCL" ~ "QCH"
  )) %>%
  filter(cv < 20)
names(neo_qchl)

neo_qchl %>%
  filter(control == "QCH") %>%
  pull(mean_result) %>% hist()

neo_qchl %>%
  filter(control == "QCL") %>%
  pull(mean_result) %>% hist()

neo_qchl_wide <- neo_qchl %>%
  pivot_wider(id_cols = c("plate","group"),names_from = "control", values_from = "mean_result") 

neo_qchl_wide

neo_qchl_wide %>%
  filter(QCL < 7, QCH <= 20) %>%
  pivot_longer(cols = c("QCL","QCH"), names_to = "control",values_to = "mean_result") %>%
  group_by(control) %>%
  summarise(mean_qc = mean(mean_result, na.rm = T),
            sd_qc = sd(mean_result, na.rm = T),
            cv_qc = sd_qc/mean_qc)

# how many plates determined QCs
neo_qchl %>%
  filter(!is.na(mean_result)) %>%
  distinct(plate) %>%
  nrow()

# how many wells determined QCL, each mean result is 2 wells
neo_qchl %>%
  filter(control == "QCL", !is.na(mean_result)) %>%
  nrow()*2
# how many wells determined QCH
neo_qchl %>%
  filter(control == "QCH", !is.na(mean_result)) %>%
  nrow()*2

# group effects: -------
lmer(log2_avg_neo ~ 
       sex +
       age +
       mrank +
       group +
       (1|subj),
     data = full_data_month) %>%
  summary()

lmer(avg_cr_resid ~ 
       sex +
       age +
       mrank +
       group +
       (1|subj),
     data = full_data_month) %>%
  summary() # tws higher than twn

lmer(log2_avg_cp_tar ~ 
       sex +
       age +
       mrank +
       group +
       (1|subj),
     data = full_data_month) %>%
  summary()


# viz: look at within individ fluctuations in cr_resid ----
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

# viz: fluctuations in neo ----

full_data_month_rownum %>% filter(rowid > 200, rowid < 300) %>% 
  ggplot() + 
  geom_line(aes(x = rowid, y = avg_neo_sg, color = subj))

full_data_month_rownum %>% filter(rowid > 200, rowid < 300) %>% 
  ggplot() + 
  geom_line(aes(x = rowid, y = avg_neo_sg, color = subj))





# Distributions of c peptide tar, elbm, monthly change elbm -------


full_udata %>% # dist of all cp data
  pull(cp_tar) %>% hist()

full_data_month %>% #monthly cp data
  pull(avg_cp_sg_tar) %>% hist()

# - cp age
full_data_month %>%
  ggplot(aes(x = age, y = avg_cp_sg_tar)) +
  geom_point(aes(color = sex))
# could negative relationship between cp and growth be
# because cp higher among older individuals?

# - elbm age
full_data_month %>%
  ggplot(aes(x = age, y = avg_cr_resid)) +
  geom_point(aes(color = sex))

# - cp and elbm
full_data_month %>%
  ggplot(aes(x = avg_cp_sg_tar, y = avg_cr_resid)) +
  geom_point(aes(color = sex))

# - distribution monthly growth
full_data_month %>%
  group_by(subj) %>%
  mutate(month_lbm_change = lead(avg_cr_resid) - avg_cr_resid) %>%
  ungroup() %>%
  pull(month_lbm_change) %>%
  hist()

# - monthly growth by age
full_data_month %>%
  group_by(subj) %>%
  mutate(month_lbm_change = lead(avg_cr_resid) - avg_cr_resid) %>%
  ungroup() %>%
  ggplot(aes(x = age, y = month_lbm_change)) +
  geom_point(aes(color = sex))
# monthly growth isn't increasing with age.


# distribution of urine sampling intervals -----

full_data_short_term_lbm_change %>%
  pull(sample_interval) %>%
  hist()
  



# graveyard -------
lmer(m ~ r + (1|subj), data = full_data_month) %>% summary()
# make new dataset for regression ----

full_data_chng_lbm <- full_data_month_rownum %>% 
  drop_na(avg_cr_resid) %>% 
  group_by(subj) %>% 
  summarize(change_lbm = first(avg_cr_resid) - last(avg_cr_resid),
                                                    sex = sex, 
            age = first(age)) %>% 
  ungroup() %>% 
  distinct()
# used initial age

full_data_avg_neo <- full_data %>% 
  drop_na(neo_sg) %>% 
  group_by(subj) %>% 
  summarize(avg_neo_all_months = mean(neo_sg)) %>% 
  ungroup()

full_data_growth_neo <- full_join(full_data_avg_neo, full_data_chng_lbm,  
                                  by = "subj")
nrow(full_data_avg_neo)
nrow(full_data_chng_lbm)
nrow(full_data_growth_neo)
# regression ----
# had to use glm bc there is no change over time

change_lbm_avg_neo_glm <- lm(change_lbm ~ log2(avg_neo_all_months) + 
        age + 
        sex,
        data = full_data_growth_neo)

qqnorm(residuals(change_lbm_avg_neo_glm))
qqline(residuals(change_lbm_avg_neo_glm))

summary(change_lbm_avg_neo_glm)

# regressions not controlling for age and sex
change_lbm_avg_neo_glm_nocontrols <- glm(change_lbm ~ log2(avg_neo_all_months),
                              data = full_data_growth_neo)

qqnorm(residuals(change_lbm_avg_neo_glm_nocontrols))
qqline(residuals(change_lbm_avg_neo_glm_nocontrols))

summary(change_lbm_avg_neo_glm_nocontrols)

# regression neo ~ cp

avg_neo_change_lbm_glm <- glm(avg_neo_all_months ~ change_lbm + 
  age + sex, data = full_data_growth_neo)

qqnorm(residuals(avg_neo_change_lbm_glm))
qqline(residuals(avg_neo_change_lbm_glm))
# a mess

summary(avg_neo_change_lbm_glm)
# still significant negative corr

#cor.test(change_lbm, avg_neo) ----

cor.test(full_data_growth_neo$change_lbm, 
         full_data_growth_neo$avg_neo_all_months,
        alternative = c("two.sided"),
        method = c("pearson"),
        conf.level = 0.95,
        continuity = FALSE)
full_data_growth_neo %>% ggplot(aes(x = avg_neo_all_months, y = change_lbm, 
                                    color = sex, size = age)) + 
  geom_jitter() 
# effect of log2 -----

beta = .2

x = 16 

y = beta*log2(x)

x2 = 32 # difference is a 1 unit increase in log2(x)

y2 = beta*log2(x2)

y
y2 # a doubling of x yields a ß change in Y 

y2-y

# when Y is also log2, what is change in raw change in Y? a doubling * ß?

2^y2 - 2^y