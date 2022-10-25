library(tidyverse)

library(lmerTest)
library(mediation)
library(lavaan)


load("data/full_data_month_udata_fgc_behav.RData", verbose = T)
full_data_month_mediate <- full_data_month %>% 
  drop_na(avg_fgc, avg_stdsg_CP, avg_neo_sg)
colSums(is.na(full_data_month_mediate))


# is energy constraint on neo mediated by GCs?
# lmer - mediation package approach ----

detach("package:lmerTest", unload = T)
library(lme4)

# total effect iv has on dv plus covariates
fit.totaleffect <- lmer(log2(avg_neo_sg) ~ 
                          log2(avg_stdsg_CP) + 
                          age + 
                          sex + 
                          (1|subj),
                        full_data_month_mediate)


# effect of iv on mediator
fit.mediator <- lmer(log2(avg_fgc) ~ 
                       log2(avg_stdsg_CP) + 
                       age +
                       sex +
                       (1|subj),
                     data = full_data_month_mediate)

# effect of mediator on dv controlling for iv

fit.dv <- lmer(log2(avg_neo_sg) ~ 
                 log2(avg_fgc) + 
                 log2(avg_stdsg_CP) +
                 age +
                 sex +
                 (1|subj),
               data = full_data_month_mediate)

summary(fit.dv)
summary(fit.totaleffect)

set.seed(288)
results = mediate(fit.mediator, fit.dv, treat='log2(avg_stdsg_CP)', mediator='log2(avg_fgc)')

summary(results)
# Total Effect = effect of CP on Neo broadly, no controls
# ADE = Average Direct Effect = effect of CP on Neo independent of/controlling for FGC
# ACME = Average Causal Mediated Effect = effect of CP on Neo via mediator FGC
# ---- ACME also = indirect effect = total effect - direct effect

plot(results)
summary(results, output = "b")





# glmer - examine mediation effect of fgc in relationship of neo ~ cp -----

full_data_month %>% filter(avg_neo_sg < 1000) %>% 
  ggplot(aes(y = log(avg_neo_sg), x = log(avg_fgc), color = sex)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# DV and IV: neo ~ cp
neo_cp_glm_month <- glmer(avg_neo_sg ~ 
                            log2(avg_stdsg_CP) + 
                            age + 
                            sex + 
                            (1|subj),
                          family = Gamma("log"),
                          data = full_data_month,
                          control = glmerControl(optimizer ="Nelder_Mead"))
neo_cp_lm_month <- lmer(log2(avg_neo_sg) ~ 
                          log2(avg_stdsg_CP) + 
                          age + 
                          sex + 
                          (1|subj),
                        data = full_data_month)

summary(neo_cp_glm_month) 
summary(neo_cp_lm_month) 

# mediator and IV: fgc ~ cp
fgc_cp_lm_month <- lmer(avg_fgc ~ sex +
                          age +
                          log2(avg_stdsg_CP) +
                          (1|subj),
                        data = full_data_month)
qqnorm(residuals(fgc_cp_lm_month))
qqline(residuals(fgc_cp_lm_month))
summary(fgc_cp_lm_month)

# mediatior and DV: neo ~ fgc
neo_fgc_glm_month <- glmer(avg_neo_sg ~ sex +
                             age +
                             log2(avg_fgc) +
                             (1|subj),
                           family = Gamma("log"),
                           data = full_data_month)



neo_fgc_lm_month <- lmer(log2(avg_neo_sg) ~ sex +
                           age +
                           log2(avg_fgc) +
                           (1|subj),
                         data = full_data_month)

qqnorm(residuals(neo_fgc_glm_month))
qqline(residuals(neo_fgc_glm_month))
qqnorm(residuals(neo_fgc_lm_month))
qqline(residuals(neo_fgc_lm_month))
summary(neo_fgc_glm_month)
summary(neo_fgc_lm_month)


# DV mediator and IV: neo ~ fgc + cp
neo_fgc_cp_glm_month <- glmer(avg_neo_sg ~ sex +
                                age +
                                log2(avg_fgc) + 
                                log2(avg_stdsg_CP) +
                                (1|subj),
                              family = Gamma("log"),
                              data = full_data_month,
                              control = glmerControl(optimizer ="Nelder_Mead"))

neo_fgc_cp_lm_month <- lmer(log2(avg_neo_sg) ~ sex +
                              age +
                              log2(avg_fgc) + 
                              log2(avg_stdsg_CP) +
                              (1|subj),
                            data = full_data_month)

qqnorm(residuals(neo_fgc_cp_glm_month))
qqline(residuals(neo_fgc_cp_glm_month))
qqnorm(residuals(neo_fgc_cp_lm_month))
qqline(residuals(neo_fgc_cp_lm_month))
summary(neo_fgc_cp_glm_month)
summary(neo_fgc_cp_lm_month)

# compare cp coefficient when fgc present vs absent, no big change in effect of cp on neo.


summary(neo_fgc_cp_lm_month)

# lavaan - sobel & permutation approach -----





# graveyard -----

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
