library(tidyverse)
library(lmerTest)

source("functions/vif.mer function.R") # vif.mer
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)
names(full_data_month)

hist(full_data_month$log2_avg_cp_tar, binwidth = 0.2)
hist(log2(full_data_month$avg_cp_sg))

# is neo energetically constrained? ------
# - by lbm and energy balance -----
neo_cp_cr_lm_month <- full_data_month %>%
  #mutate(month = as.character(month)) %>%
  #mutate(month = factor(month, ordered = F)) %>%
  #mutate(month = factor(month, ordered = T, levels = c(8,9,10,11,12,1,2,3))) %>%
  lmer(log2_avg_neo ~ 
         sex +
         age +
         mrank +
         avg_cr_resid +
         log2_avg_cp_tar +
         avg_rain +
         (1|subj) + (1|month),
       data = .)

#avg_rain +
#n_partners +
#avg_f_part +


qqnorm(residuals(neo_cp_cr_lm_month))
qqline(residuals(neo_cp_cr_lm_month))
summary(neo_cp_cr_lm_month)
vif.mer(neo_cp_cr_lm_month)# all < 1.17

# --- plot data points of neo against cp and lbm (2 plots) where lm is marginal effect of each ------



# save model -----
#save(neo_cp_cr_lm_month, file = "models/energetic-constraints-immune-broad.Rdata") 

# graveyard ----
# -- energy balance alone ----
neo_cp_cr_glm_month <- glmer(avg_neo_sg ~ 
                               sex +
                               age +
                               log2(avg_cr_resid) +
                               log2(avg_stdsg_CP) +
                               (1|subj), 
                             family = Gamma("log"),
                             data = full_data_month, 
                             control = glmerControl(optimizer ="Nelder_Mead"))
qqnorm(residuals(neo_cp_glm_month))
qqline(residuals(neo_cp_glm_month))
summary(neo_cp_glm_month)
vif.mer(neo_cp_glm_month)# all < 1.02

# visualization (mean and median look very similar in viz)

full_data_month %>% filter(avg_stdsg_CP < 4000, avg_neo_sg < 2000) %>%
  ggplot(aes(x = avg_stdsg_CP, y = avg_neo_sg, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "C-peptide ng/ml",
       y = "Neopterin ng/ml") #title = "Energetic Constraints on Neopterin"

# -- lbm alone -----
neo_lbm_glm_month <- glmer(avg_neo_sg ~ 
                             sex +
                             age +
                             avg_cr_resid + 
                             (1|subj), 
                           family = Gamma("log"), 
                           data = full_data_month)
qqnorm(residuals(neo_lbm_glm_month))
qqline(residuals(neo_lbm_glm_month))
summary(neo_lbm_glm_month)
vif.mer(neo_lbm_glm_month) # all < 1.000002

full_data_month %>% 
  filter(avg_neo_sg < 2000, avg_cr_resid < .75) %>% 
  ggplot(aes(y = avg_neo_sg, x = avg_cr_resid, color = sex)) +
  geom_smooth(method = "lm") + 
  geom_point() +
  theme_minimal() + 
  labs(y = "avg. Neopterin ng/ml",
       x =  "avg. Estimated Lean Body Mass") # title = "Body Condition Constraint on Neopterin"


hist(full_data_month$age)
hist(scale(full_data_month$age))
hist(log2(full_data_month$age))

# explore seasonality of all markers: neo, cp, lbm, rain -----
full_data_month %>%
  mutate(month = as.factor(month)) %>%
  ggplot(aes(x = month, y = avg_cp_sg)) +
  geom_boxplot()
full_data_month %>%
  mutate(month = as.factor(month)) %>%
  ggplot(aes(x = month, y = avg_neo_sg)) +
  geom_boxplot()
full_data_month %>%
  mutate(month = as.factor(month)) %>%
  ggplot(aes(x = month, y = avg_cr_resid)) +
  geom_boxplot()
full_data_month %>%
  mutate(month = as.factor(month)) %>%
  ggplot(aes(x = month, y = avg_rain)) +
  geom_boxplot()



