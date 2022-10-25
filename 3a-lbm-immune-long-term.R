library(tidyverse)
library(lmerTest)
library(gtools)

source("functions/vif.mer function.R") # function is "vif.mer"

# data summarized by subj year month
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)
apply(full_data_month, 2, function(x) sum(is.na(x)))


# A2- body condition and cellular immunity - neo cr_resid - viz and regression ----
# - A2a neo/immunity prioritized over growth --- cr_resid ~ neo + age + sex  ----
# iffy q-q plot
# non-positive values can't be used with gamma and cant use log
# pos corr btw med cr and med neo sg

full_data_month %>% 
  filter(avg_neo_sg < 2000) %>% 
  ggplot(aes(x = avg_neo_sg, y = avg_cr_resid, color = sex)) +
  geom_smooth(method = "lm") + 
  geom_point() +
  theme_minimal() + 
  labs(x = "Median Neopterin by Month",
       y =  "Median Creatinine Residuals by Month",
       title = "Neopterin Relationship with Lean Tissue")

cr_neo_lm_month <- lmer(avg_cr_resid ~ sex + mrank +
                           log2(avg_neo_sg) + 
                           age +
                           (1|subj),
                         data = full_data_month)
qqnorm(residuals(cr_neo_lm_month))
qqline(residuals(cr_neo_lm_month))
summary(cr_neo_lm_month)


# - A2b neo/immunity constrained by body condition --- neo ~ cr_resid + age + sex ----
# iffy q-q plot
# results - pos corr btw neo and cr: estimate = .273630 +- 1.96*0 .032827, p score = <2e-16

neo_cr_glm_month <- glmer(avg_neo_sg ~ sex +
                           log2(avg_cr_resid) + 
                           age +
                           (1|group/subj), 
                         family = Gamma("log"), 
                         data = full_data_month)
qqnorm(residuals(neo_cr_glm_month))
qqline(residuals(neo_cr_glm_month))
summary(neo_cr_glm_month)

full_data_month %>% 
  filter(avg_neo_sg < 2000, avg_cr_resid < .75) %>% 
  ggplot(aes(y = avg_neo_sg, x = avg_cr_resid, color = sex)) +
  geom_smooth(method = "lm") + 
  geom_point() +
  theme_minimal() + 
  labs(y = "Median Neopterin by Month",
       x =  "Median Lean Body Mass by Month",
       title = "Body Condition Constraint on Neopterin")



full_data_month %>% 
  #filter(avg_neo_sg < 2000) %>% 
  ggplot(aes(x = log(avg_neo_sg), y = r, color = sex)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  theme_minimal() + 
  labs(x = "Monthly Log Median Neopterin",
       y =  "Proportion of Time Resting", 
       title = "Immunity and Rest")

r_neo_glm_month <- glmer(r ~ sex +
                           log2(avg_neo_sg) + 
                           age +
                           (1|group/subj), 
                         family = Gamma("log"),
                         data = full_data_month)

qqnorm(residuals(r_neo_glm_month))
qqline(residuals(r_neo_glm_month))
summary(r_neo_glm_month)

# viz
full_data_month %>% filter(avg_neo_sg < 2000) %>% 
  ggplot(aes(x = avg_neo_sg, y = f, color = sex)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  theme_minimal() + 
  labs(x = "Median Neopterin by Month",
       x =  "Proportion of Time Feeding", 
       title = "Immunity and Feeding")

#regression
f_neo_lm_month_lmer <- lmer(f ~ sex +
                          log2(avg_neo_sg) + 
                          age +
                          (1|group/subj), 
                        data = full_data_month)
qqnorm(residuals(f_neo_lm_month_lmer))
qqline(residuals(f_neo_lm_month_lmer))
summary(f_neo_lm_month_lmer)

hist(residuals(f_neo_lm_month_lmer))

neo_f_glm_month <- glmer(avg_neo_sg ~ sex +
                          log2(f) + 
                          age +
                          (1|group/subj), 
                        family = Gamma("log"),
                        data = full_data_month)
qqnorm(residuals(neo_f_glm_month))
qqline(residuals(neo_f_glm_month))
summary(neo_f_glm_month)

# graveyard ----

hist(full_data_month$age)
hist(scale(full_data_month$age))
hist(log2(full_data_month$age))

