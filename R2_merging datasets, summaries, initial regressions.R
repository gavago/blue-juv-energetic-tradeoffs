library(tidyverse)
# library(readxl)
# library(magrittr)
# library(lubridate)
# library(mgcv)
library(lmerTest)

load("data/neo-dataset-full.Rdata", verbose = T)
load("data/cp-dataset-full.Rdata", verbose = T)
load("data/behav-dataset-month.Rdata", verbose = T)
load("data/fgc_data_by_sample.Rdata", verbose = T)


# save each Rdata dataset to CV ----
#write.csv(neo_data_full, file = "neo-dataset-full.csv", row.names = F)
# write.csv(cp_raw, file = "cp-dataset-full.csv", row.names = F)
# write.csv(behav_data_month, file = "behav-dataset-month.csv", row.names = F)



# Merging urine sample data (neo, cp, cr_resid)  -----
merged_udata <- left_join( neo_data_full, cp_raw, by = intersect(names(neo_data_full), names(cp_raw))) %>%
  select(group, subj, date, age, year, month, time, sample_number, stdsg_CP, neo_sg, everything())


# Add lean body mass - calculating cr-sg resids ------

merged_udata %>%
  ggplot(., aes(y = Cr, x = SG/1000, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm")


m <- merged_udata %>%
  lm(Cr ~ I(SG/1000) + I((SG/1000)^2), data = ., na.action = na.exclude)
resids <- as.numeric(resid(m))

full_udata <- merged_udata %>%
  mutate(cr_resid = resids)

str(full_udata)

#write_csv(full_udata, file = "data/urine_sample_dataset_juv_immune_energetics.csv")

# Summarize udata and fgc data by month ------

### summarize & save udata -----

full_udata <- read_csv("data/urine_sample_dataset_juv_immune_energetics.csv")
View(full_udata)
dim(full_udata)

udata_month_avg <- full_udata %>%
  group_by(subj, month, year) %>% 
  summarize(avg_neo_sg = mean(neo_sg, na.rm = T), 
            avg_cr_resid = mean(cr_resid, na.rm = T), 
            avg_stdsg_CP = mean(stdsg_CP, na.rm = T),
            med_neo_sg = median(neo_sg, na.rm = T), 
            med_cr_resid = median(cr_resid, na.rm = T), 
            med_stdsg_CP = median(stdsg_CP, na.rm = T),
            weaning = weaning, sex = sex,
            mum = mum, age = age) %>% 
  ungroup() %>% 
  distinct()
dim(udata_month_avg)

save(udata_month_avg, file = "data/udata_month_avg.Rdata", row.names = F)


### summarize & save fgcs ------

load("data/fgc_data_by_sample.Rdata", verbose = T)
view(gc_raw)
dim(gc_raw)

fgc_month_avg <- gc_raw %>% 
  group_by(subj, month, year) %>% 
  summarize(avg_fgc = mean(fgc.ng_g.feces, na.rm = T)) %>% 
  ungroup()
dim(fgc_month_avg)

save(fgc_month_avg, file = "data/fgc_month_avg.Rdata", row.names = F)

view(fgc_month_avg)

# Merge monthly data - urine, feces, behavior -----
load("data/behav-dataset-month.Rdata", verbose = T)

udata_fgc_month_avg <- full_join(udata_month_avg, fgc_month_avg, by = intersect(names(udata_month_avg), names(fgc_month_avg)))
dim(udata_fgc_month_avg) # 317, 5 urine subj-months where no fgc data

view(udata_gc_month_avg)

full_data_month <- full_join(udata_fgc_month_avg, behav_data_month, by = intersect(names(udata_fgc_month_avg), names(behav_data_month)))
dim(full_data_month) # 323

view(full_data_month)

# save r data
save(full_data_month, file = "data/full_data_month_udata_fgc_behav.RData")

load("data/full_data_month_udata_fgc_behav.RData", verbose = T)

# save csv
write.csv(full_data_month,  file = "data/full_data_month_udata_fgc_behav.csv", row.names = F)

# Validation of cr_resid by relationship with age, sex, and c-peptide ------

#sex == "M"
  ggplot() +
  # geom_point(data = full_data %>% filter(subj == "allo"), 
  #            aes(x = age, y = cr_resid), color = "red") +
  # geom_smooth(data = full_data %>% filter(subj == "allo"), 
  #             aes(x = age, y = cr_resid), color = "red",
  #             method = "lm") +
  # geom_point(data = full_data %>% filter(subj == "brog"), 
  #            aes(x = age, y = cr_resid), color = "green") +
  # geom_smooth(data = full_data %>% filter(subj == "brog"), 
  #             aes(x = age, y = cr_resid),color = "green",
  #             method = "lm") +
  geom_point(data = full_data, 
             aes(x = age, y = cr_resid), alpha = 0.1) +
  geom_smooth(data = full_data, 
              aes(x = age, y = cr_resid),
              method = "lm")

full_data %>% 
  ggplot() +
  geom_point(aes(x = age, y = cr_resid, color = sex), alpha = 0.3) +
  geom_smooth(aes(x = age, y = cr_resid, color = sex), method = "lm") +
  theme_minimal()

df_cr_resid %>%
  ggplot(., aes(x = sex, y = cr_resid)) +
  geom_point() +
  geom_boxplot()

full_data %>%
  ggplot(., aes(y = cr_resid, x = stdsg_CP, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm")

crsg_cp_lm <- lmer(cr_resid ~ sex + scale(stdsg_CP) + scale(age) + (1|subj), data = full_data)
qqnorm(residuals(crsg_cp_lm))
summary(crsg_cp_lm)

library(lme4)
lmer(neo_sg ~ sex + scale(age) + scale(gm) + scale(stdsg_CP) + +  (1|subj), data = full_data)



# graveyard -----
merged_data <- read.csv("data/merged_data_no_lean_body_mass.csv", header = T)


# check that time format is same
# x <- left_join(cp_raw, neo_data_full, by = "sample_number")
# x %>%
#   filter(time.x != time.y) %>%
#   select(starts_with("time"))

#check intersection names across all data frames
# intersect(names(neo_data_full), names(cp_raw)) %>%
#   intersect(names(behav_data_month))

# x <- neo_data_full %>% arrange(sample_number)
# y <- cp_raw %>% arrange(sample_number)
# all(x$time == y$time) # amazing, if cp_raw and neo aren't saved on the same day then they're times aren't the same


merged_data <- left_join( neo_data_full, cp_raw, by = intersect(names(neo_data_full), names(cp_raw))) %>%
  left_join(., behav_data_month) %>%
  select(group, subj, date, age, year, month, time, sample_number, stdsg_CP, neo_sg, everything())
nrow(merged_data) # 620

View(merged_data)

# check out how many values NA per variable, should def be zero for age and sex
apply(merged_data, 2, function(x) sum(is.na(x)))

#write.csv(merged_data,  file = "data/merged_data_no_lean_body_mass.csv", row.names = F)

# graveyard ----


