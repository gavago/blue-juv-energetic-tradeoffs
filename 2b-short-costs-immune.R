library(tidyverse)
library(lmerTest)
library(interplot)

select <- dplyr::select # interplot also has select

source("functions/vif.mer function.R") # function is "vif.mer"

# load dataset ---
load("data/full_data_short_term_lbm_change.Rdata", verbose = T)
 
# explore dataset ------

# relationship lbm change and sample interval
# no bias, similar changes at all intervals
# hist(full_data_short_term_lbm_change$sample_lbm_change)
full_data_short_term_lbm_change %>%
  ggplot(aes(sample_interval, sample_lbm_change)) +
  geom_point() +
  geom_smooth()

full_data_short_term_lbm_change %>% 
  filter(neo_sg < 2000) %>% 
  ggplot(aes(x = neo_sg, 
             y = sample_lbm_change, 
             color = sex)) +
  geom_smooth(method = "lm") + 
  geom_point()

# change lbm t2-t1 ~ neo t1, regression ------
lbm_change_neo_lm_sample <-
  lmer(sample_lbm_change ~ 
         age + 
         sex +
         log2_neo +
         sample_interval +
  log2_neo*sample_interval +
  (1|subj),
  data = full_data_short_term_lbm_change)
qqnorm(residuals(lbm_change_neo_lm_sample))
qqline(residuals(lbm_change_neo_lm_sample))
hist(residuals(lbm_change_neo_lm_sample))
plot(residuals(lbm_change_neo_lm_sample))

summary(lbm_change_neo_lm_sample)

vif.mer(lbm_change_neo_lm_sample) 
# all < 90.6 with interaction and < 1.02 without it

# viz - relationship neo and subsequent change in elbm -----
full_data_short_term_lbm_change %>% 
  filter(neo_sg < 2000) %>% 
  ggplot(aes(x = log2(neo_sg), 
             y = sample_lbm_change, 
             color = sex)) +
  geom_jitter() +
  geom_smooth(method = "lm")

# viz interaction - neo by interval ----
# change in relationship neo & change elbm according to change interval
interplot(m = sample_lbm_change_neo_sg_lmer, var1 = "log2_neo", var2 = "sample_interval")


# boundary (singular) fit: see help('isSingular') --> extreme collinearity?

cor.test(full_data_short_term_lbm_change$sample_lbm_change, 
         full_data_short_term_lbm_change$neo_sg,
         alternative = c("two.sided"),
         method = c("pearson"),
         conf.level = 0.95,
         continuity = FALSE)
# negative correlation - not controlling for subject, age, or sex



