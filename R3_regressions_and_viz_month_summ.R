library(tidyverse)
library(lmerTest)
library(dplyr)
library(gtools)
install.packages("AICcmodavg")
library(AICcmodavg)


# data includes fgcs and overall is summarized by subj year month
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)
view(full_data_month)

apply(full_data_month, 2, function(x) sum(is.na(x)))


# regressions to be run here will test relationships in the concept map

# H1a & b - cp neo - viz and regression -----

# - H1a is neo energetically constrained? --- neo ~ cp + age + sex ------
# results - pos corr: estimate = .17546 +- 1.96 * .03850
neo_cp_glm_month <- glmer(med_neo_sg ~ sex +
                           scale(med_stdsg_CP) +
                           scale(age) +
                           (1|subj), 
                         family = Gamma("log"),
                         data = full_data_month)
qqnorm(residuals(neo_cp_glm_month))
qqline(residuals(neo_cp_glm_month))
summary(neo_cp_glm_month)

#neo ~ cp controlling for fgc
neo_cp_fgc_glm_month <- glmer(med_neo_sg ~ sex +
                            scale(med_stdsg_CP) +
                            scale(avg_fgc) +
                            scale(age) +
                            (1|subj), 
                          family = Gamma("log"),
                          data = full_data_month)
qqnorm(residuals(neo_cp_fgc_glm_month))
qqline(residuals(neo_cp_fgc_glm_month))
summary(neo_cp_fgc_glm_month)

hist(residuals(neo_cp_glm_month))

hist(residuals(neo_cp_glm_month))

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
# result: pos corr btw med cp and med neo, coefficient = .16085 +- 1.96 * .05783 -- 
# does not eat into available energy -- as more energy becomes available, more used for immunity

cp_neo_glm_month <- glmer(med_stdsg_CP ~ sex +
                           scale(med_neo_sg) +
                           scale(age) +
                           (1|subj), 
                         family = Gamma("log"), 
                         data = full_data_month)
qqnorm(residuals(cp_neo_glm_month))
qqline(residuals(cp_neo_glm_month))
summary(cp_neo_glm_month)

full_data_month %>% filter(med_stdsg_CP < 4000, med_neo_sg < 2000) %>%
  ggplot(aes(y = med_stdsg_CP, x = med_neo_sg, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(y = "C-peptide",
       x = "Neopterin",
       title = "Neopterin and Energy Balance")

# ---- if so (if neo coef < 0) is this because increase neo corresponds with lower feeding? 
# neo coef > 0 
# ---- f ~ neo + age + sex 
# results: no significant relationship. large P value, std error crosses 0
# but iffy q-q plot
f_neo_lm_month <- lmer(f ~ sex + 
                          scale(med_neo_sg) + 
                          scale(age) + (1|subj), 
                        data = full_data_month)
qqnorm(residuals(f_neo_lm_month))
qqline(residuals(f_neo_lm_month))
summary(f_neo_lm_month)


# - additional exploration cp neo -----
# LF says high neo outliers have lower cp, worth checking somehow
# NATG suggestion: bin neopterin using quantcut into 6 even bins,
# visualize with boxplot with y = cp and x = neo_bin
full_data_month %>% ggplot(aes(y = med_stdsg_CP, x = med_neo_sg, color = sex)) + geom_point()

neo_bin <- full_data_month %>% 
  select(med_neo_sg) %>% 
  pull() %>% 
  quantcut(q = 6)

full_data_month %>% 
  mutate(neo_bin = quantcut(med_neo_sg, q = 6)) %>% 
  ggplot(aes(x = neo_bin, y = med_stdsg_CP)) +
  geom_boxplot() + 
  labs(x =  "Neopterin", 
       y = "C-Peptide",
       title = "Median C-Peptide and Neopterin by Month") 

cp_neo_bin_anova <- aov(med_stdsg_CP ~ neo_bin*age*sex, data = full_data_month)

# test with anova to see if cp differs by neo bin
# need to find equivalent of (1 | subj)

# H2ab - neo cr_resid - viz and regression ----
# - H2a neo/immunity prioritized over growth --- cr_resid ~ neo + age + sex  ---
# iffy q-q plot
# non-positive values can't be used with gamma and cant use log
# pos corr btw med cr and med neo sg

full_data_month %>% 
  filter(med_neo_sg < 2000) %>% 
  ggplot(aes(x = med_neo_sg, y = med_cr_resid, color = sex)) +
  geom_smooth(method = "lm") + 
  geom_point() +
  theme_minimal() + 
  labs(x = "Median Neopterin by Month",
       y =  "Median Creatinine Residuals by Month",
       title = "Neopterin Relationship with Lean Tissue")

cr_neo_lm_month <- lmer(med_cr_resid ~ sex + 
                           scale(med_neo_sg) + 
                           scale(age) +
                           (1|subj),
                         data = full_data_month)
qqnorm(residuals(cr_neo_lm_month))
qqline(residuals(cr_neo_lm_month))
summary(cr_neo_lm_month)


# - H2b neo/immunity constrained by body condition --- neo ~ cr_resid + age + sex ---
# iffy q-q plot
# results - pos corr btw neo and cr: estimate = .273630 +- 1.96*0 .032827, p score = <2e-16

neo_cr_glm_month <- glmer(med_neo_sg ~ sex +
                           scale(med_cr_resid) + 
                           scale(age) +
                           (1|subj), 
                         family = Gamma("log"), 
                         data = full_data_month)
qqnorm(residuals(neo_cr_glm_month))
qqline(residuals(neo_cr_glm_month))
summary(neo_cr_glm_month)

full_data_month %>% 
  filter(med_neo_sg < 2000, med_cr_resid < .75) %>% 
  ggplot(aes(y = med_neo_sg, x = med_cr_resid, color = sex)) +
  geom_smooth(method = "lm") + 
  geom_point() +
  theme_minimal() + 
  labs(y = "Median Neopterin by Month",
       x =  "Median Lean Body Mass by Month",
       title = "Body Condition Constraint on Neopterin")

# H3 investment in affiliative behavior vs immunity ----

# all affil behav viz (boring)

full_data_month %>% 
  group_by(subj, month, year) %>% 
  mutate(affil_behav = median(c(pl, gm, f))) %>% 
  ungroup() %>% 
  filter (med_neo_sg < 2000, affil_behav < .2) %>% 
  ggplot(aes(x = affil_behav, y = med_neo_sg, color = sex)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  labs(x = "Affiliative Behavior (Play, Giving and Recieving Grooming)", 
       y = "Median Neopterin by Month",
       title = "Neopterin and Affiliative Behavior") 
  
# play and neo
# results: neg corr w large std error (estimate = -.07584 +- 1.96* .03781, p-score = .0448)

full_data_month %>% filter(med_neo_sg < 2000, pl < .1) %>% 
  ggplot(aes(x = pl, y = med_neo_sg, color = sex)) +
  geom_point() + 
  geom_smooth(method = lm) +
  theme_minimal() + labs(x = "Proportion of Time Playing",
                         y =  "Monthly Median Neopterin", 
                         title = "Immunity and Play")


neo_pl_glm_month <- glmer(med_neo_sg ~ sex +
                           scale(pl) + 
                           scale(age) +
                           (1|subj), 
                         family = Gamma("log"),
                         data = full_data_month)
qqnorm(residuals(neo_pl_glm_month))
qqline(residuals(neo_pl_glm_month))
summary(neo_pl_glm_month)

# neo ~ pl with low cp

full_data_month %>%  filter(med_neo_sg < 2000) %>% 
  mutate(pl_bin = quantcut(pl, q = 6)) %>%
  ggplot(aes(x = pl_bin, y = med_neo_sg)) + geom_boxplot() 

full_data_month_cp_bin <- full_data_month %>% 
  mutate(cp_bin = quantcut(med_stdsg_CP, q = 6))

lo_cp <- full_data_month_cp_bin %>%
  filter(med_stdsg_CP < 6190) %>% 
  as.data.frame()

two.way <- aov(med_neo_sg ~ scale(med_stdsg_CP) + scale(pl), data = full_data_month)
summary(two.way)

neo_pl_glm_month_lo_cp <- glmer(med_neo_sg ~ sex +
                            scale(pl) + 
                            scale(age) +
                            (1|subj), 
                          family = Gamma("log"),
                          data = lo_cp)
summary(neo_pl_glm_month_lo_cp)

hist(neo_pl_glm_month_lo_cp)
plot(residuals(neo_pl_glm_month_lo_cp))
qqplot(residuals(neo_pl_glm_month_lo_cp))

sum(full_data_month$pl == 0, na.rm = TRUE)

#exploratory
full_data_month %>%  filter(med_neo_sg < 2000) %>% 
  mutate(pl_bin = quantcut(pl, q = 6)) %>%
  ggplot(aes(x = pl_bin, y = med_neo_sg)) + geom_boxplot() 

full_data_month %>%
  ggplot(aes(x = neo_bin, y = pl)) + geom_boxplot()

full_data_month %>%
  ggplot(aes(x = neo_bin, y = pl, color = sex))  + geom_boxplot()

full_data_month %>% filter(med_stdsg_CP < 20000) %>% 
  ggplot(aes(x = med_stdsg_CP, y = pl, color = sex)) + 
  geom_point() + 
  geom_smooth(method = "lm")
# stronger relationship when controlling for fgc (estimate = -.09055 +- 1.96*.03773, p = .01639)

neo_pl_fgc_glm_month <- glmer(med_neo_sg ~ sex +
                           scale(pl) + 
                           scale(age) + 
                           scale(avg_fgc) +
                           (1|subj), 
                         family = Gamma("log"),
                         data = full_data_month)
qqnorm(residuals(neo_pl_fgc_glm_month))
qqline(residuals(neo_pl_fgc_glm_month))
summary(neo_pl_fgc_glm_month)

# grooming given and neo
# bad q-q plot
# Warning message : Model failed to converge with max|grad| = 0.0359666 (tol = 0.002, component 1)
# results: neg corr (estimate = -.125426 +- 1.96* .001956, <p score = 2e-16)
  
full_data_month %>% filter(gm < .10) %>% 
  ggplot(aes(x = gm, y = med_neo_sg, color = sex)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
    theme_minimal() + 
    labs(x = "Proportion of Time Spent Grooming",
                           y =  "Monthly Median Neopterin", 
                           title = "Immunity and Grooming")

neo_gm_glm_month <- glmer(med_neo_sg ~ sex +
                           scale(gm) + 
                           scale(age) +
                           (1|subj), 
                         family = Gamma("log"), 
                         data = full_data_month)
qqnorm(residuals(neo_gm_glm_month))
qqline(residuals(neo_gm_glm_month))
summary(neo_gm_glm_month)

# grooming recieved and neo 

# non-positive values not allowed for gamma and cant take log of y bc of 0s
# Error in mkRespMod(fr, REML = REMLpass) : NA/NaN/Inf in 'y'

full_data_month %>% filter(gmd < .08) %>% 
  ggplot(aes(x = gmd, y = med_neo_sg, color = sex)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  theme_minimal() + 
  labs(x = "Proportion of Time Spent Recieving Grooming",
       y =  "Monthly Median Neopterin", 
       title = "Immunity and Grooming Recieved")

neo_gmd_glm_month <- glmer(med_neo_sg ~ sex +
                           scale(gmd) + 
                           scale(age) +
                           (1|subj), 
                         family = Gamma("log"),
                         data = full_data_month)
qqnorm(residuals(neo_gmd_glm_month))
qqline(residuals(neo_gmd_glm_month))
summary(neo_gm_glm_month)

# Resting and feeding compensating for neo ----

# resting
# good q-q plot
# results: pos corr (estimate = 0.02366 +- 1.96*.01011, p = .01926)
# suggests animals rest to compensate for energy expend. from immunity

full_data_month %>% 
  #filter(med_neo_sg < 2000) %>% 
  ggplot(aes(x = log(med_neo_sg), y = r, color = sex)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  theme_minimal() + 
  labs(x = "Monthly Log Median Neopterin",
       y =  "Proportion of Time Resting", 
       title = "Immunity and Rest")

r_neo_glm_month <- glmer(r ~ sex +
                           scale(med_neo_sg) + 
                           scale(age) +
                           (1|subj), 
                         family = Gamma("log"),
                         data = full_data_month)
qqnorm(residuals(r_neo_glm_month))
qqline(residuals(r_neo_glm_month))
summary(r_neo_glm_month)

# feeding
# iffy q-q plot
# results: no significant relationship w both med and avg
# (estimate crosses 0, hi p score)

full_data_month %>% filter(med_neo_sg < 2000) %>% 
  ggplot(aes(x = med_neo_sg, y = f, color = sex)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  theme_minimal() + 
  labs(x = "Median Neopterin by Month",
       x =  "Proportion of Time Feeding", 
       title = "Immunity and Feeding")

f_neo_lm_month_lmer <- lmer(f ~ sex +
                          scale(avg_neo_sg) + 
                          scale(age) +
                          (1|subj), 
                        data = full_data_month)
qqnorm(residuals(f_neo_lm_month_lmer))
qqline(residuals(f_neo_lm_month_lmer))
summary(f_neo_lm_month_lmer)

hist(residuals(f_neo_lm_month_lmer))

neo_f_glm_month <- glmer(med_neo_sg ~ sex +
                          scale(f) + 
                          scale(age) +
                          (1|subj), 
                        family = Gamma("log"),
                        data = full_data_month)
qqnorm(residuals(neo_f_glm_month))
qqline(residuals(neo_f_glm_month))
summary(neo_f_glm_month)

# significant relationship BUT 
# failed to converge with max|grad| = 0.0348692 (tol = 0.002, component 1)

# how to address non normality in linear mixed models

# fgc ----

# neo and fgc
# results: negative corr (estimate = -0.11286 +- 1.96* .03389, p = .000868)

full_data_month %>% filter(med_neo_sg < 1000) %>% 
  ggplot(aes(x = med_neo_sg, y = avg_fgc, color = sex)) + 
  geom_point() + 
  geom_smooth(method = "lm")

neo_fgc_glm_month <- glmer(med_neo_sg ~ sex +
                          scale(avg_fgc) + 
                          scale(age) +
                          (1|subj), 
                        family = Gamma("log"),
                        data = full_data_month)
qqnorm(residuals(neo_fgc_glm_month))
qqline(residuals(neo_fgc_glm_month))
summary(neo_fgc_glm_month)

# cp and fgc
# neg corr (estimate = -.11694 +- 1.96* .0457, p = .0150)
cp_fgc_glm_month <- glmer(med_stdsg_CP ~ sex +
                            scale(avg_fgc) + 
                            scale(age) +
                            (1|subj), 
                          family = Gamma("log"),
                          data = full_data_month)

qqnorm(residuals(cp_fgc_glm_month))
qqline(residuals(cp_fgc_glm_month))

hist(residuals(cp_fgc_glm_month))
plot(cp_fgc_glm_month)

summary(cp_fgc_glm_month)

# need to fix model, right skewed

#fgc and pl, no significant relationship
fgc_pl_glm_month <- glmer(avg_fgc ~ sex +
                            scale(pl) + 
                            scale(age) +
                            (1|subj), 
                          family = Gamma("log"),
                          data = full_data_month)
qqnorm(residuals(fgc_pl_glm_month))
qqline(residuals(fgc_pl_glm_month))
summary(fgc_pl_glm_month)


# grooming given and fgc
# results: pos corr (estimate = .07164 +- 1.96*.032108, p = .0257)
fgc_gm_glm_month <- glmer(avg_fgc ~ sex +
                           scale(gm) + 
                           scale(age) +
                           (1|subj), 
                         family = Gamma("log"),
                         data = full_data_month)
qqnorm(residuals(fgc_gm_glm_month))
qqline(residuals(fgc_gm_glm_month))

hist(residuals(fgc_gm_glm_month))
plot(fgc_gm_glm_month)

summary(fgc_gm_glm_month)

# grooming recieved and fgc
# no significant correlation

fgc_gmd_glm_month <- glmer(avg_fgc ~ sex +
                           scale(gmd) + 
                           scale(age) +
                           (1|subj), 
                         family = Gamma("log"),
                         data = full_data_month)
qqnorm(residuals(fgc_gmd_glm_month))
qqline(residuals(fgc_gmd_glm_month))

hist(residuals(fgc_gmd_glm_month))
plot(fgc_gmd_glm_month)

summary(fgc_gmd_glm_month)


