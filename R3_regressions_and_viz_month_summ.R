library(tidyverse)
library(lmerTest)
library(dplyr)
library(gtools)

# data includes fgcs and overall is summarized by subj year month
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)
view(full_data_month)

# regressions to be run here will test relationships in the concept map

# H1a & b - cp neo - viz and regression -----

# - H1a is neo energetically constrained? --- neo ~ cp + age + sex ------

neo_cp_lm_month <- glmer(med_neo_sg ~ sex +
                           scale(age) +
                           scale(med_stdsg_CP) +
                           (1|subj), 
                         family = Gamma("log"),
                         data = full_data_month)
qqnorm(residuals(neo_cp_lm_month))
qqline(residuals(neo_cp_lm_month))
summary(neo_cp_lm_month)

full_data_month %>% ggplot(aes(x = med_stdsg_CP)) + geom_histogram()

# non-positive values not allowed for the 'Gamma' family,had to unscale med_neo_sg

# visualization (mean and median look very similar in viz)

full_data_month %>% filter(med_stdsg_CP < 4000, med_neo_sg < 2000) %>%
  ggplot(aes(x = med_stdsg_CP, y = med_neo_sg, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "C-peptide",
       y = "Neopterin",
       title = "Energetic Constraints on Neopterin ")

# - H1b does neo eat into available energy? --- cp ~ neo + age + sex ----
# positive correlation btw med cp and med neo, coefficient = .16085 +- .05783
cp_neo_lm_month <- glmer(med_stdsg_CP ~ sex +
                           scale(med_neo_sg) +
                           scale(age) +
                           (1|subj), 
                         family = Gamma("log"), 
                         data = full_data_month)
qqnorm(residuals(cp_neo_lm_month))
qqline(residuals(cp_neo_lm_month))
summary(cp_neo_lm_month)

full_data_month %>% filter(med_stdsg_CP < 4000, med_neo_sg < 2000) %>%
  ggplot(aes(y = med_stdsg_CP, x = med_neo_sg, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(y = "C-peptide",
       x = "Neopterin",
       title = "Neopterin's Effect on Energy Balance")

# ---- if so (if neo coef < 0) is this because increase neo corresponds with lower feeding? 
# ---- f ~ neo + age + sex 

f_neo_lm_month <- glmer(f ~ sex + 
                          scale(med_neo_sg) + 
                          scale(age) + (1|subj), 
                        family = Gamma("log"), 
                        data = full_data_month)
qqnorm(residuals(f_neo_lm_month))
qqline(residuals(f_neo_lm_month))
summary(f_neo_lm_month)

# nope - large P value, std error crosses 0

# - additional exploration cp neo -----
# LF says high neo outliers have lower cp, worth checking somehow
# NATG suggestion: bin neopterin using quantcut into 6 even bins,
full_data_month %>% ggplot(aes(y = med_stdsg_CP, x = med_neo_sg, color = sex)) + geom_point()

neo_bin <- full_data_month %>% 
  select(med_neo_sg) %>% 
  pull() %>% 
  quantcut(quant = 6)

full_data_month %>% 
  mutate(neo_bin = quantcut(med_neo_sg, q = 6)) %>% 
  ggplot(aes(x = neo_bin, y = med_stdsg_CP))+
  geom_boxplot() + 
  labs(x =  "Neopterin", 
       y = "C-Peptide",
       title = "Median C-Peptide and Neopterin by Month") 


# visualize with boxplot with y = cp and x = neo_bin,
# test with anova to see if cp differs by neo bin

# H2ab - neo cr_resid - viz and regression ----
# - H2a neo/immunity prioritized over growth --- cr_resid ~ neo + age + sex  ---
cr_neo_lm_month <- glmer(med_cr_resid ~ sex + 
                           scale(med_neo_sg) + 
                           scale(age) +
                           (1|subj),
                         family = Gamma("log"), 
                         data = full_data_month)
qqnorm(residuals(cr_neo_lm_month))
qqline(residuals(cr_neo_lm_month))
summary(cr_neo_lm_month)
# positive relationship btw cr and neo
# non-positive values can't be used with gamma - might need to use new family. 

full_data_month %>% 
  filter(med_neo_sg < 2000) %>% 
  ggplot(aes(x = med_neo_sg, y = med_cr_resid, color = sex)) +
  geom_smooth(method = "lm") + 
  geom_point() +
  theme_minimal() + 
  labs(x = "Median Neopterin by Month",
       y =  "Median Creatinine Residuals by Month",
       title = "Neopterin's Constraint on Growth")

# - H2b neo/immunity constrained by body condition --- neo ~ cr_resid + age + sex ---
cr_neo_lm_month <- glmer(med_neo_sg ~ sex +
                           scale(med_cr_resid) + 
                           scale(age) +
                           (1|subj), 
                         family = Gamma("log"), 
                         data = full_data_month)
qqnorm(residuals(cr_neo_lm_month))
qqline(residuals(cr_neo_lm_month))
summary(cp_neo_lm_month)

full_data_month %>% 
  filter(med_neo_sg < 2000, med_cr_resid < .75) %>% 
  ggplot(aes(y = med_neo_sg, x = med_cr_resid, color = sex)) +
  geom_smooth(method = "lm") + 
  geom_point() +
  theme_minimal() + 
  labs(y = "Median Neopterin by Month",
       x =  "Creatinine Residuals by Month",
       title = "Body Condition Constraint on Neopterin")

# H3 investment in affiliative behavior vs immunity ----


full_data_month %>% mutate(social_behav = mean(c(pl, gm, f))) %>% 
  ggplot(aes(x = social_behav, y = med_neo_sg, color = sex)) +
  geom_point() + 
  geom_smooth(method = lm)
# working on it

full_data_month %>% filter(pl < .1) %>% 
  ggplot(aes(x = pl, y = med_neo_sg, color = sex)) +
  geom_point() + 
  geom_smooth(method = lm) +
  theme_minimal() + labs(x = "Proportion of Time Playing",
                         y =  "Median Neopterin by Month", 
                         title = "Immunity and Play")

neo_pl_lm_month <- glmer(med_neo_sg ~ sex +
                           scale(pl) + 
                           scale(age) +
                           (1|subj), 
                         family = Gamma("log"), 
                         data = full_data_month)
qqnorm(residuals(neo_pl_lm_month))
qqline(residuals(neo_pl_lm_month))
summary(neo_pl_lm_month)

# significant negative relationship but iffy q-q plot 

full_data_month %>% filter(gm < .10) %>% 
  ggplot(aes(x = gm, y = med_neo_sg, color = sex)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
    theme_minimal() + 
    labs(x = "Proportion of Time Spent Grooming",
                           y =  "Median Neopterin by Month", 
                           title = "Immunity and Grooming")

neo_gm_lm_month <- glmer(med_neo_sg ~ sex +
                           scale(gm) + 
                           scale(age) +
                           (1|subj), 
                         family = Gamma("log"), 
                         data = full_data_month)
qqnorm(residuals(neo_gm_lm_month))
qqline(residuals(neo_gm_lm_month))
summary(neo_gm_lm_month)

# Warning message : Model failed to converge with max|grad| = 0.0359666 (tol = 0.002, component 1)

  
