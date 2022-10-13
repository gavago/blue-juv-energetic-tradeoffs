library(lmerTest)
library(interplot)
library(tidyverse)

select <- dplyr::select

#load datasets
full_data <- read.csv("data/full_dataset_juv_immune_energetics.csv", header = T)
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)

#add sample lbm change
full_data_short_term_lbm_change <- full_data %>% 
  group_by(subj) %>% 
  mutate(sample_lbm_change = lead(cr_resid) - cr_resid) %>%
  mutate(sample_interval = as.numeric(difftime(lead(date), date, units = "days"))) %>%
  # mutate(sample_interval = case_when(
  #   is.na(sample_interval) ~ NA_real_,
  #   TRUE ~ sample_interval)) %>% 
  ungroup()

full_data_short_term_lbm_change %>%
    select(subj, date, sample_interval, sample_lbm_change, cr_resid, neo_sg) %>% 
    arrange(subj, date) %>%
    view()
  
view(full_data_short_term_lbm_change)
hist(full_data_short_term_lbm_change$sample_lbm_change)
# looks like normal dist

# relationship lbm change and sample interval
full_data_short_term_lbm_change %>%
  ggplot(aes(sample_interval, sample_lbm_change)) +
  geom_point() +
  geom_smooth()


dim(full_data_short_term_lbm_change)
dim(full_data)

full_data_short_term_lbm_change %>% 
  filter(neo_sg < 2000) %>% 
  ggplot(aes(x = neo_sg, 
             y = sample_lbm_change, 
             color = sex)) +
  geom_smooth(method = "lm") + 
  geom_point()

z.<- function(x) log2(x)

# regression
sample_lbm_change_neo_sg_lmer <-  full_data_short_term_lbm_change %>%
  mutate(log2_neo = log2(neo_sg)) %>%
  lmer(sample_lbm_change ~
         log2_neo + 
         age + 
         sex +
         sample_interval +
  log2_neo*sample_interval +
  (1|subj),
  data = .)
qqnorm(residuals(sample_lbm_change_neo_sg_lmer))
qqline(residuals(sample_lbm_change_neo_sg_lmer))
hist(residuals(sample_lbm_change_neo_sg_lmer))
plot(residuals(sample_lbm_change_neo_sg_lmer))

summary(sample_lbm_change_neo_sg_lmer)

# viz - relationship neo and subsequent change in elbm
full_data_short_term_lbm_change %>% 
  filter(neo_sg < 2000) %>% 
  ggplot(aes(x = log2(neo_sg), 
             y = sample_lbm_change, 
             color = sex)) +
  geom_jitter() +
  geom_smooth(method = "lm")

# viz - change in relationship neo & change elbm according to change interval
interplot(m = sample_lbm_change_neo_sg_lmer, var1 = "log2_neo", var2 = "sample_interval")


# boundary (singular) fit: see help('isSingular') --> extreme collinearity?

cor.test(full_data_short_term_lbm_change$sample_lbm_change, 
         full_data_short_term_lbm_change$neo_sg,
         alternative = c("two.sided"),
         method = c("pearson"),
         conf.level = 0.95,
         continuity = FALSE)
# negative correlation - not controlling for subject, age, or sex



