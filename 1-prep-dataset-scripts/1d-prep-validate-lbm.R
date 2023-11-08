library(tidyverse)
library(lmerTest)

load("data/urine_sample_dataset_juv_immune_energetics.Rdata", verbose = T)
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)

# Validation - Cr influenced by monthly physical activity?
names(full_data_month)

cr_activity <- lmer(avg_cr_resid ~ log2(avg_cp_sg_tar) + age + m + f + r + (1|subj), data = full_data_month)
summary(cr_activity)


# Validation of cr_resid by relationship with age, sex, and c-peptide ------

#sex == "M"
# ggplot() +
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
# geom_point(data = full_data, 
#            aes(x = age, y = cr_resid), alpha = 0.1) +
# geom_smooth(data = full_data, 
#             aes(x = age, y = cr_resid),
#             method = "lm")

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

# by sample
lbm_by_age_samp <- lmer(cr_resid ~ sex + log2(cp_tar) + age + (1|subj), data = full_udata)
qqnorm(residuals(lbm_by_age_samp))
qqline(residuals(lbm_by_age_samp))
summary(lbm_by_age_samp)

# by monthly avg
lbm_by_age_mo <- lmer(avg_cr_resid ~ sex + log2_avg_cp_tar + age + (1|subj), data = full_data_month)
summary(lbm_by_age_mo)

# does monthly moving and resting predict monthly avg cr resid? no.
# lbm_by_age_mo1 <- lmer(avg_cr_resid ~ sex + log2_avg_cp_tar + age + r + m + (1|subj), data = full_data_month)


