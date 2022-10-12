library(lmerTest)
library(mgcv)
library(tidyverse)
library(dplyr)

#load datasets
full_data <- read.csv("data/full_dataset_juv_immune_energetics.csv", header = T)
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)

#add sample lbm change
full_data_short_term_lbm_change <- full_data %>% 
  group_by(subj) %>% 
  mutate(sample_lbm_change = lead(cr_resid) - cr_resid) %>%
  ungroup()

view(full_data_short_term_lbm_change)
hist(full_data_short_term_lbm_change$sample_lbm_change)
# looks like normal dist

dim(full_data_short_term_lbm_change)
dim(full_data)

full_data_short_term_lbm_change %>% 
  filter(neo_sg < 2000) %>% 
  ggplot(aes(x = neo_sg, 
             y = sample_lbm_change, 
             color = sex)) +
  geom_smooth(method = "lm") + 
  geom_point()

# regression
sample_lbm_change_neo_sg_lmer <-  lmer(sample_lbm_change ~ neo_sg + 
                                         age + 
                                         sex +
                                         (1|subj),
                                       #control = glmerControl(optimizer ="Nelder_Mead"),
                                       data = full_data_short_term_lbm_change)

hist(residuals(sample_lbm_change_neo_sg_lmer))
plot(residuals(sample_lbm_change_neo_sg_lmer))

summary(sample_lbm_change_neo_sg)
# boundary (singular) fit: see help('isSingular') --> extreme collinearity?

cor.test(full_data_short_term_lbm_change$sample_lbm_change, 
         full_data_short_term_lbm_change$neo_sg,
         alternative = c("two.sided"),
         method = c("pearson"),
         conf.level = 0.95,
         continuity = FALSE)
# negative correlation - not controlling for subject, age, or sex

full_data_short_term_lbm_change %>% 
  filter(neo_sg < 2000) %>% 
  ggplot(aes(x = neo_sg, 
             y = sample_lbm_change, 
             color = sex)) +
  geom_smooth(method = "lm") 
#geom_point()

