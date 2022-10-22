library(tidyverse)
library(lmerTest)

load("data/full_data_month_udata_fgc_behav.RData", verbose = T)


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

crsg_cp_lm <- lmer(cr_resid ~ sex + scale(stdsg_CP) + scale(age) + (1|subj), data = full_data)
qqnorm(residuals(crsg_cp_lm))
summary(crsg_cp_lm)

library(lme4)
lmer(neo_sg ~ sex + scale(age) + scale(gm) + scale(stdsg_CP) + +  (1|subj), data = full_data)


