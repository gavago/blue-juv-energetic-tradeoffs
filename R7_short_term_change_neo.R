library(lmerTest)
library(interplot)
library(tidyverse)

select <- dplyr::select

#load datasets
full_data <- read.csv("data/full_dataset_juv_immune_energetics.csv", header = T)
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)

# add sample neo change
full_data_short_term_neo_change <- full_data %>% 
  group_by(subj) %>% 
  mutate(sample_neo_change = lead(neo_sg) - neo_sg) %>%
  mutate(sample_interval = as.numeric(difftime(lead(date), date, units = "days"))) %>%
  ungroup()

view(full_data_short_term_neo_change)
hist(full_data_short_term_neo_change$sample_neo_change)

# w/o outliers
sample_neo_change_no_outliers <- full_data_short_term_neo_change %>% 
  filter(sample_neo_change > -1000, sample_neo_change < 1000)

hist(sample_neo_change_no_outliers$sample_neo_change)


# viz

full_data_short_term_neo_change %>% 
  filter(sample_neo_change > -2000, 
         sample_neo_change < 2000,
         stdsg_CP < 40000) %>% 
  ggplot(aes(x = stdsg_CP,
             y = sample_neo_change,
             color = sex)) +
  geom_smooth(method = "lm") + 
  geom_jitter()

# regression

sample_neo_change_cp_lmer <-  full_data_short_term_neo_change %>%
  mutate(log2_stdsg_CP = log2(neo_sg)) %>%
  lmer(sample_neo_change ~
         log2_stdsg_CP + 
         age + 
         sex +
         sample_interval +
         log2_stdsg_CP*sample_interval +
         (1|subj),
       data = .)
# add sample 1 neo_sg to predictors to control for neo levels dropping
# following infection?

qqnorm(residuals(full_data_short_term_neo_change_lmer))
qqline(residuals(full_data_short_term_neo_change_lmer))
plot(residuals(sample_lbm_change_neo_sg_lmer))

summary(full_data_short_term_neo_change_lmer)
# model is too poorly fit to see anything, outliers throwing things off ?

# interaction viz 
interplot(m = sample_neo_change_cp_lmer, var1 = "log2_stdsg_CP", var2 = "sample_interval")

# correlation test
cor.test(full_data_short_term_neo_change$sample_neo_change, 
         full_data_short_term_neo_change$stdsg_CP,
         alternative = c("two.sided"),
         method = c("pearson"),
         conf.level = 0.95,
         continuity = FALSE)

# negative corr not controlling for anything
