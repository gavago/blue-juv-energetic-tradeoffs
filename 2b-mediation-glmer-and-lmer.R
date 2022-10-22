library(tidyverse)
detach("package:lmerTest", unload = T)
library(lme4)
library(mediation)


load("data/full_data_month_udata_fgc_behav.RData", verbose = T)
full_data_month_mediate <- full_data_month %>% 
  drop_na(avg_fgc, avg_stdsg_CP, avg_neo_sg)
colSums(!is.na(full_data_month_mediate))
dim(full_data_month)

# is energy constraint on neo mediated by GCs?
# examine mediation effect of fgc in relationship of neo ~ cp -----

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
                          (1|group/subj),
                        data = full_data_month)
qqnorm(residuals(fgc_cp_lm_month))
qqline(residuals(fgc_cp_lm_month))
summary(fgc_cp_lm_month)
# mediatior and DV: neo ~ fgc
neo_fgc_glm_month <- glmer(avg_neo_sg ~ sex +
                             age +
                             log2(avg_fgc) +
                             (1|group/subj),
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
                              (1|group/subj), 
                              family = Gamma("log"),
                              data = full_data_month,
                              control = glmerControl(optimizer ="Nelder_Mead"))
qqnorm(residuals(neo_fgc_cp_glm_month))
qqline(residuals(neo_fgc_cp_glm_month))
summary(neo_fgc_cp_glm_month)
# compare cp coefficient when fgc present vs absent, no big change in effect of cp on neo.

# mediation package approach ----

# total effect iv has on dv plus covariates
fit.totaleffect <- lmer(avg_neo_sg ~ 
                          log2(avg_stdsg_CP) + 
                          age + 
                          sex + 
                          (1|subj),
                        full_data_month_mediate)


# effect of iv on mediator
fit.mediator <- lmer(avg_fgc ~ 
                       log2(avg_stdsg_CP) + 
                       age +
                       sex +
                       (1|subj),
                     data = full_data_month_mediate)

# effect of mediator on dv controlling for iv

fit.dv <- lmer(avg_neo_sg ~ 
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
# Total Effect = effect of CP on Neo
# ADE = effect of CP on Neo independent of FGC
# ACME = effect of CP on Neo via mediator FGC

plot(results)
summary(results, output = "b")

######
# fgc ~ CP
# neg corr (estimate = -.11694 +- 1.96* .0457, p = .0150)
cp_fgc_glm_month <- glmer(avg_stdsg_CP ~ sex +
                            age +
                            log2(avg_fgc) + 
                            (1|group/subj), 
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
                          (1|group/subj), 
                        #family = Gamma("log")
                        full_data_month_mediate)

summary(fit.totaleffect)

# effect of iv on mediator

fit.mediator <- lmer(avg_fgc ~ log2(avg_stdsg_CP) + 
                       age +
                       sex +
                       (1|group/subj),
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
                 (1|group/subj),
               #family = Gamma("log"),
               data = full_data_month_mediate)

summary(fit.dv)
summary(fit.totaleffect)

results = mediate(fit.mediator, fit.dv, treat='avg_stdsg_CP', mediator='avg_fgc', boot =T)


# cannot use glmer and number of observations dont match btw mediator 
# and outcome models




# graveyard -----