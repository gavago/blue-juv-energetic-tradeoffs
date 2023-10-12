library(tidyverse)
library(lmerTest)
library(interplot)
select <- dplyr::select

source("functions/vif.mer function.R") # function is "vif.mer"
load("data/full_data_short_term_lbm_change.Rdata", verbose = T)
names(full_data_short_term_lbm_change) 


# change lbm t2-t1 ~ neo t1, regression ------
change_lbm_neo_lm_sample <-
  lmer(sample_lbm_change ~ 
         age + 
         sex +
         log2_neo +
         sample_interval +
  log2_neo*sample_interval +
  (1|subj) + (1|month),
  data = full_data_short_term_lbm_change)
qqnorm(residuals(change_lbm_neo_lm_sample))
qqline(residuals(change_lbm_neo_lm_sample))
hist(residuals(change_lbm_neo_lm_sample))
plot(residuals(change_lbm_neo_lm_sample))

summary(change_lbm_neo_lm_sample)

vif.mer(change_lbm_neo_lm_sample) 
# all < 90.5 with interaction and < 1.03 in terms not included

# viz - relationship neo and subsequent change in elbm -----
full_data_short_term_lbm_change %>% 
  filter(neo_sg < 2000) %>% 
  ggplot(aes(x = log2_neo, 
             y = sample_lbm_change, 
             color = sex)) +
  geom_jitter() +
  geom_smooth(method = "lm")

# viz interaction - neo by interval ----
# change in relationship neo & change elbm according to change interval
interplot(m = change_lbm_neo_lm_sample, var1 = "log2_neo", var2 = "sample_interval")



# save model -----
#save(change_lbm_neo_lm_sample, file = "models/energetic-costs-immune-short.Rdata")



# relationship lbm change and sample interval -----
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

# graveyard --------------------------
# boundary (singular) fit: see help('isSingular') --> extreme collinearity?

cor.test(full_data_short_term_lbm_change$sample_lbm_change, 
         full_data_short_term_lbm_change$neo_sg,
         alternative = c("two.sided"),
         method = c("pearson"),
         conf.level = 0.95,
         continuity = FALSE)
# negative correlation - not controlling for subject, age, or sex


# alternative models for better fits - actually worse
full_data_short_term_lbm_change %<>%
  mutate(pos_lbm_change = sample_lbm_change+abs(min(sample_lbm_change, na.rm = T)) + 0.001) %>%
  mutate(log2_pos_lbm_change = log2(pos_lbm_change))

change_lbm_neo_glm_sample <-
  glmer(pos_lbm_change ~ 
          age + 
          sex +
          log2_neo +
          sample_interval +
          log2_neo*sample_interval +
          (1|subj),
        family = Gamma(link = "log"),
        data = full_data_short_term_lbm_change)
summary(change_lbm_neo_glm_sample)
qqnorm(residuals(change_lbm_neo_glm_sample))
qqline(residuals(change_lbm_neo_glm_sample))

change_lbm_neo_lm_sample1 <-
  lmer(log2_pos_lbm_change ~ 
         age + 
         sex +
         log2_neo +
         sample_interval +
         log2_neo*sample_interval +
         (1|subj),
       data = full_data_short_term_lbm_change)
qqnorm(residuals(change_lbm_neo_lm_sample1))
qqline(residuals(change_lbm_neo_lm_sample1))
summary(change_lbm_neo_lm_sample1)
