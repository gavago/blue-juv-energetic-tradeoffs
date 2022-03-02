library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)
library(mgcv)
library(lmerTest)

load("neo dataset full.Rdata", verbose = T)
load("cp dataset full.Rdata", verbose = T)
load("behav dataset month.Rdata", verbose = T)

# save each Rdata dataset to CV ----
# write.csv(neo_data_full, file = "neo dataset full.csv", row.names = F)
# write.csv(cp_raw, file = "cp dataset full.csv", row.names = F)
# write.csv(behav_data_month, file = "behav dataset month.csv", row.names = F)


# merging neo, cp, and behavior ----

# check that time format is same
# x <- left_join(cp_raw, neo_data_full, by = "sample_number")
# x %>%
#   filter(time.x != time.y) %>%
#   select(starts_with("time"))

#check intersection names across all data frames
# intersect(names(neo_data_full), names(cp_raw)) %>%
#   intersect(names(behav_data_month))

merged_data <- left_join(cp_raw, neo_data_full, by = intersect(names(neo_data_full), names(cp_raw))) %>%
  left_join(., behav_data_month) %>%
  select(group, subj, date, age, year, month, time, sample_number, stdsg_CP, neo_sg, everything())
nrow(full_data) # 620


# check out how many values NA per variable, should def be zero for age and sex
apply(merged_data, 2, function(x) sum(is.na(x)))
#write.csv(merged_data,  file = "merged_data_no_lean_body_mass.csv", row.names = F)

# Add lean body mass - calculating cr-sg resids ------
merged_data <- read.csv("merged_data_no_lean_body_mass.csv", header = T)

merged_data %>%
  filter(sex == "M") %>%
  #filter(subj == "dame") %>%
  ggplot(., aes(y = Cr, x = SG/100, color = age)) +
  geom_point() +
  geom_smooth(method = "lm")

m <- merged_data %>%
  lm(Cr ~ I(SG/100) + I((SG/100)^2), data = .)
slope_crsg <- coef(m)[[2]]

full_data <- merged_data %>%
  mutate(expected_cr = (SG/100) * slope_crsg, cr_resid = Cr - expected_cr) %>%
  select(-expected_cr)

str(full_data)

#write.csv(full_data, file = "full_dataset_juv_immune_energetics.csv", row.names = F)


# Validation of cr-sg by relationship with age, sex, and c-peptide ------

full_data %>% #sex == "M"
  ggplot() +
  geom_point(data = full_data %>% filter(subj == "amos"), 
             aes(x = age, y = cr_resid, color = "red")) +
  geom_smooth(data = full_data %>% filter(subj == "amos"), 
              aes(x = age, y = cr_resid, color = "red"),
              method = "lm") +
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



# graveyard ----


