library(tidyverse)
library(lmerTest)


load("data/urine_sample_dataset_juv_immune_energetics.Rdata", verbose = T)
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)

# Validation - Cr resid -----

names(full_data_month)

# influenced by monthly physical activity? no
cr_activity <- lmer(avg_cr_resid ~ log2(avg_cp_sg_tar) + age + m + f + r + (1|subj), data = full_data_month)
summary(cr_activity)

# by sample - # increase w cp and age? yes
lbm_by_age_samp <- lmer(cr_resid ~ sex + log2(cp_tar) + age + (1|subj), data = full_udata)
qqnorm(residuals(lbm_by_age_samp))
qqline(residuals(lbm_by_age_samp))
summary(lbm_by_age_samp)

# by monthly avg - # increase w cp and age? yes
lbm_by_age_mo <- lmer(avg_cr_resid ~ sex + log2_avg_cp_tar + age + (1|subj), data = full_data_month)
summary(lbm_by_age_mo)

# does monthly moving and resting predict monthly avg cr resid? no.
# lbm_by_age_mo1 <- lmer(avg_cr_resid ~ sex + log2_avg_cp_tar + age + r + m + (1|subj), data = full_data_month)


# Visualize - explore monthly cr_resid - paper figures in script 5 viz marginal effects -----
full_data_month%>% 
  ggplot() +
  geom_point(aes(x = age, y =avg_cr_resid, color = sex), alpha = 0.5) +
  geom_smooth(aes(x = age, y =avg_cr_resid, color = sex), method = "lm") +
  theme_minimal()

df_cr_resid %>%
  ggplot(., aes(x = sex, y = avg_cr_resid)) +
  geom_point() +
  geom_boxplot()

full_data_month %>%
  ggplot(., aes(y = avg_cr_resid, x = avg_cp_sg_tar_viz, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm")

]

# gravevyard ----
sex == "M"
ggplot() +
geom_point(data = full_data_month %>% filter(subj == "allo"),
           aes(x = age, y = avg_ cr_resid), color = "red") +
geom_smooth(data = full_data_month %>% filter(subj == "allo"),
            aes(x = age, y = avg_ cr_resid), color = "red",
            method = "lm") +
geom_point(data = full_data_month %>% filter(subj == "brog"),
           aes(x = age, y = avg_ cr_resid), color = "green") +
geom_smooth(data = full_data_month %>% filter(subj == "brog"),
            aes(x = age, y = avg_ cr_resid),color = "green",
            method = "lm") +
geom_point(data = full_data_month,
           aes(x = age, y = avg_ cr_resid), alpha = 0.1) +
geom_smooth(data = full_data_month,
            aes(x = age, y = avg_ cr_resid),
            method = "lm")

