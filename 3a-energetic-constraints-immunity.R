library(tidyverse)
library(lmerTest)

source("functions/vif.mer function.R") # vif.mer
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)


# is neo energetically constrained? ------
# -- energy balance ----
neo_cp_glm_month <- glmer(avg_neo_sg ~ 
                            sex +
                            age +
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

# -- lbm -----
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


# save models -----
save(neo_cp_glm_month , neo_lbm_glm_month, file = "models/energetic-constraints-immune-broad.Rdata") 
# graveyard ----

hist(full_data_month$age)
hist(scale(full_data_month$age))
hist(log2(full_data_month$age))

