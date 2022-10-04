library(tidyverse)
library(lmerTest)
library(gtools)
library(AICcmodavg) #NTG to LF - what are are you using this package for?
library(mediation)

# data includes fgcs and overall is summarized by subj year month
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)
view(full_data_month)

apply(full_data_month, 2, function(x) sum(is.na(x)))


# H1 - energetics of cellular immunity - cp neo - viz and regression -----

# - H1a is neo energetically constrained? --- neo ~ cp + age + sex ------
# results - pos corr: estimate = .17546 +- 1.96 * .03850

neo_cp_glm_month <- glmer(avg_neo_sg ~ sex +
                           age +
                           log2(avg_stdsg_CP) +
                           (1|subj), 
                         family = Gamma("log"),
                         data = full_data_month,
                         control = glmerControl(optimizer ="Nelder_Mead"))
qqnorm(residuals(neo_cp_glm_month))
qqline(residuals(neo_cp_glm_month))
summary(neo_cp_glm_month)

# neo ~ cp controlling for mrank

neo_cp_mrank_glm_month <- glmer(avg_neo_sg ~ sex +
                            age + mrank +
                            log2(avg_stdsg_CP) +
                            (1|subj), 
                          family = Gamma("log"),
                          data = full_data_month)
                          #control = glmerControl(optimizer ="Nelder_Mead"))
qqnorm(residuals(neo_cp_mrank_glm_month))
qqline(residuals(neo_cp_mrank_glm_month))
summary(neo_cp_mrank_glm_month)


#neo ~ cp controlling for fgc
neo_cp_fgc_glm_month <- glmer(avg_neo_sg ~ sex +
                            age +
                            log2(avg_stdsg_CP) +
                            log2(avg_fgc) +
                            (1|subj), 
                          family = Gamma("log"),
                          data = full_data_month,
                          control = glmerControl(optimizer ="Nelder_Mead"))
qqnorm(residuals(neo_cp_fgc_glm_month))
qqline(residuals(neo_cp_fgc_glm_month))
summary(neo_cp_fgc_glm_month)

hist(residuals(neo_cp_glm_month))

hist(residuals(neo_cp_glm_month))

# visualization (mean and median look very similar in viz)

full_data_month %>% filter(avg_stdsg_CP < 4000, avg_neo_sg < 2000) %>%
  ggplot(aes(x = avg_stdsg_CP, y = avg_neo_sg, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "C-peptide",
       y = "Neopterin",
       title = "Energetic Constraints on Neopterin ")



# - H1b does neo eat into available energy? --- cp ~ neo + age + sex ----

# result: pos corr btw med cp and med neo, coefficient = .16085 +- 1.96 * .05783 -- 
# does not eat into available energy -- as more energy becomes available, more used for immunity
# mod
cp_neo_glm_month <- glmer(avg_stdsg_CP ~ sex +
                            age +
                            log2(avg_neo_sg) +
                           (1|subj), 
                         family = Gamma("log"), 
                         data = full_data_month)
                         control = glmerControl(optimizer ="Nelder_Mead"))
qqnorm(residuals(cp_neo_glm_month))
qqline(residuals(cp_neo_glm_month))
summary(cp_neo_glm_month)

# does mrank correlate with energy balance?
cp_neo_mrank_glm_month <- glmer(avg_stdsg_CP ~ sex +
                            age +
                            log2(avg_neo_sg) + 
                            mrank +
                            (1|subj), 
                          family = Gamma("log"), 
                          data = full_data_month)
                        # control = glmerControl(optimizer ="Nelder_Mead"))
qqnorm(residuals(cp_neo_mrank_glm_month))
qqline(residuals(cp_neo_mrank_glm_month))
summary(cp_neo_mrank_glm_month)

# viz
full_data_month %>% filter(avg_stdsg_CP < 4000, avg_neo_sg < 2000) %>%
  ggplot(aes(y = avg_stdsg_CP, x = avg_neo_sg, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(y = "C-peptide",
       x = "Neopterin",
       title = "Neopterin and Energy Balance")

# - H1c is food availability responsible for relationship bw cp and neo
cp_fai_glm_month <- glmer(avg_stdsg_CP ~ sex +
                            age + log2(fai) +
                            (1|subj), 
                          family = Gamma("log"), 
                          data = full_data_month,
                          control = glmerControl(optimizer ="Nelder_Mead"))
qqnorm(residuals(cp_fai_glm_month))
qqline(residuals(cp_fai_glm_month))
summary(cp_fai_glm_month)
# pos corr btw fai and cp as expected

neo_fai_glm_month <- glmer(avg_neo_sg ~ sex +
                                scale(age) + scale(log2(fai)) +
                                (1|subj), 
                              family = Gamma("log"), 
                              data = full_data_month,
                              control = glmerControl(optimizer ="Nelder_Mead"))
summary(neo_fai_glm_month)

neo_fai_cp_glm_month <- glmer(avg_neo_sg ~ sex +
                            scale(age) + scale(log2(fai)) +
                             scale(log2(avg_stdsg_CP)) +
                            (1|subj), 
                          family = Gamma("log"), 
                          data = full_data_month,
                          control = glmerControl(optimizer ="Nelder_Mead"))
qqnorm(residuals(neo_fai_cp_glm_month))
qqline(residuals(neo_fai_cp_glm_month))
summary(neo_fai_cp_glm_month)

neo_fai_cp_glm_month_nolog <- glmer(avg_neo_sg ~ sex +
                                scale(age) + scale(fai) +
                                scale(avg_stdsg_CP) +
                                (1|subj), 
                              family = Gamma("log"), 
                              data = full_data_month,
control = glmerControl(optimizer ="Nelder_Mead"))

qqnorm(residuals(neo_fai_cp_glm_month_nolog))
qqline(residuals(neo_fai_cp_glm_month_nolog))
summary(neo_fai_cp_glm_month_nolog)


# stronger relationship btw fai and neo than cp and neo (ind of each other)
# suggests fruit availability has stronger influence on neo than energy
# stores released/other food in environment


# - Does feeding, moving, or resting compensate for the cost of neo? ----
# resting:
# good q-q plot
# results: pos corr (estimate = 0.02366 +- 1.96*.01011, p = .01926)
# suggests animals rest to compensate for energy expend. from immunity
# feeding
# iffy q-q plot
# results: no significant relationship w both med and avg
# (estimate crosses 0, hi p score)

# feeding ~ neo 
f_neo_lm_month <- lmer(f ~ sex + 
                          age +
                          log2(avg_neo_sg) + 
                          log2(avg_stdsg_CP) +
                           (1|subj), 
                        data = full_data_month)
qqnorm(residuals(f_neo_lm_month))
qqline(residuals(f_neo_lm_month))
summary(f_neo_lm_month)

# resting ~ neo - see increased resting when neo is higher
r_neo_lm_month <- lmer(r ~ sex + 
                         age +
                         log2(avg_neo_sg) + 
                         log2(avg_stdsg_CP) +
                         (1|subj), 
                       data = full_data_month)
qqnorm(residuals(r_neo_lm_month))
qqline(residuals(r_neo_lm_month))
summary(r_neo_lm_month)

# moving ~ neo 
m_neo_lm_month <- lmer(m ~ sex + 
                         age +
                         log2(avg_neo_sg) + 
                         log2(avg_stdsg_CP) +
                         (1|subj), 
                       data = full_data_month)
qqnorm(residuals(m_neo_lm_month))
qqline(residuals(m_neo_lm_month))
summary(m_neo_lm_month)

# - H1c - cp effect on neo mediated by GCs ----

# examine mediation effect of fgc in relationship of 

full_data_month %>% filter(avg_neo_sg < 1000) %>% 
  ggplot(aes(y = log(avg_neo_sg), x = log(avg_fgc), color = sex)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# DV and IV: neo ~ cp
summary(neo_cp_glm_month) 
# mediator and IV: fgc ~ cp
fgc_cp_lm_month <- lmer(avg_fgc ~ sex +
                            age +
                            log2(avg_stdsg_CP) +
                            (1|subj),
                          data = full_data_month)
qqnorm(residuals(fgc_cp_lm_month))
qqline(residuals(fgc_cp_lm_month))
summary(fgc_cp_lm_month)
# mediationr and DV: neo ~ fgc
neo_fgc_glm_month <- glmer(avg_neo_sg ~ sex +
                             age +
                             log2(avg_fgc) +
                             (1|subj),
                           family = Gamma("log"),
                           data = full_data_month)
qqnorm(residuals(neo_fgc_glm_month))
qqline(residuals(neo_fgc_glm_month))
summary(neo_fgc_glm_month)
# DV mediator and IV: neo ~ fgc + cp
neo_fgc_cp_glm_month <- glmer(avg_neo_sg ~ sex +
                                age +
                                log2(avg_fgc) + 
                                log2(avg_stdsg_CP)
                                (1|subj), 
                              family = Gamma("log"),
                              data = full_data_month,
                              control = glmerControl(optimizer ="Nelder_Mead"))
qqnorm(residuals(neo_fgc_cp_glm_month))
qqline(residuals(neo_fgc_cp_glm_month))
summary(neo_fgc_cp_glm_month)
# compare cp coefficient when fgc present vs absent, no big change in effect of cp on neo.

# -- causal mediation analysis with "mediation" pkg ----
# see R4 script - mediation analysis

# - additional exploration cp neo -----
# LF says high neo outliers have lower cp, worth checking somehow
# NATG suggestion: bin neopterin using quantcut into 6 even bins,
# visualize with boxplot with y = cp and x = neo_bin
full_data_month %>% ggplot(aes(y = avg_stdsg_CP, x = avg_neo_sg, color = sex)) + geom_point()

neo_bin <- full_data_month %>% 
  select(avg_neo_sg) %>% 
  pull() %>% 
  quantcut(q = 6)

full_data_month %>% 
  mutate(neo_bin = quantcut(avg_neo_sg, q = 6)) %>% 
  ggplot(aes(x = neo_bin, y = avg_stdsg_CP)) +
  geom_boxplot() + 
  labs(x =  "Neopterin", 
       y = "C-Peptide",
       title = "Median C-Peptide and Neopterin by Month") 

cp_neo_bin_anova <- aov(avg_stdsg_CP ~ neo_bin*age*sex, data = full_data_month)

# test with anova to see if cp differs by neo bin
# need to find equivalent of (1 | subj)

# H2- body condition and cellular immunity - neo cr_resid - viz and regression ----
# - H2a neo/immunity prioritized over growth --- cr_resid ~ neo + age + sex  ----
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

cr_neo_lm_month <- lmer(avg_cr_resid ~ sex + 
                           log2(avg_neo_sg) + 
                           age +
                           (1|subj),
                         data = full_data_month)
qqnorm(residuals(cr_neo_lm_month))
qqline(residuals(cr_neo_lm_month))
summary(cr_neo_lm_month)


# - H2b neo/immunity constrained by body condition --- neo ~ cr_resid + age + sex ----
# iffy q-q plot
# results - pos corr btw neo and cr: estimate = .273630 +- 1.96*0 .032827, p score = <2e-16

neo_cr_glm_month <- glmer(avg_neo_sg ~ sex +
                           log2(avg_cr_resid) + 
                           age +
                           (1|subj), 
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
                           (1|subj), 
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
                          (1|subj), 
                        data = full_data_month)
qqnorm(residuals(f_neo_lm_month_lmer))
qqline(residuals(f_neo_lm_month_lmer))
summary(f_neo_lm_month_lmer)

hist(residuals(f_neo_lm_month_lmer))

neo_f_glm_month <- glmer(avg_neo_sg ~ sex +
                          log2(f) + 
                          age +
                          (1|subj), 
                        family = Gamma("log"),
                        data = full_data_month)
qqnorm(residuals(neo_f_glm_month))
qqline(residuals(neo_f_glm_month))
summary(neo_f_glm_month)

# graveyard ----
# fgc ~ CP
# neg corr (estimate = -.11694 +- 1.96* .0457, p = .0150)
cp_fgc_glm_month <- glmer(avg_stdsg_CP ~ sex +
                            age +
                            log2(avg_fgc) + 
                            (1|subj), 
                          family = Gamma("log"),
                          data = full_data_month) 

qqnorm(residuals(cp_fgc_glm_month))
qqline(residuals(cp_fgc_glm_month))

# mediation analysis in progress
set.seed(288)

full_data_month_mediate <- full_data_month %>% 
  drop_na(avg_fgc, avg_stdsg_CP, avg_neo_sg)

colSums(!is.na(full_data_month_mediate))

# total effect iv has on dv plus covariates
fit.totaleffect <- lmer(avg_neo_sg ~ log2(avg_stdsg_CP) + 
                          age + 
                          sex + 
                          (1|subj), 
                        #family = Gamma("log")
                        full_data_month_mediate)

summary(fit.totaleffect)

# effect of iv on mediator

fit.mediator <- lmer(avg_fgc ~ log2(avg_stdsg_CP) + 
                       age +
                       sex +
                       (1|subj),
                     #family = Gamma("log"),
                     data = full_data_month_mediate)
qqnorm(residuals(fit.mediator))
qqline(residuals(fit.mediator))
hist(residuals(fit.mediator))
summary(fit.mediator)

# no significant relationship, strange given negative relationship when cp is dependent

# effect of mediator on dv controlling for iv

fit.dv <- lmer(avg_neo_sg ~ log2(avg_fgc) + 
                 log2(avg_stdsg_CP) +
                 age +
                 sex +
                 (1|subj),
               #family = Gamma("log"),
               data = full_data_month_mediate)

summary(fit.dv)
summary(fit.totaleffect)

results = mediate(fit.mediator, fit.dv, treat='avg_stdsg_CP', mediator='avg_fgc', boot =T)


# cannot use glmer and number of observations dont match btw mediator 
# and outcome models

hist(full_data_month$age)
hist(scale(full_data_month$age))
hist(log2(full_data_month$age))

hist(full_data_month$avg_neo_sg)
hist(full_data_month$med_neo_sg)
hist(scale(full_data_month$avg_neo_sg))
hist(log2(full_data_month$avg_neo_sg))

hist(full_data_month$avg_stdsg_CP)
hist(full_data_month$med_stdsg_CP)
hist(scale(full_data_month$avg_stdsg_CP))
hist(log2(full_data_month$avg_stdsg_CP))

hist(full_data_month$avg_fgc)
hist(full_data_month$med_fgc)
hist(scale(full_data_month$avg_fgc))
hist(log2(full_data_month$avg_fgc))