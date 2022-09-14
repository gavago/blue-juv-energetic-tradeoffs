library(tidyverse)
detach("package:lmerTest", unload = T)
library(lme4)
library(mediation)


load("data/full_data_month_udata_fgc_behav.RData", verbose = T)
full_data_month_mediate <- full_data_month %>% 
  drop_na(avg_fgc, avg_stdsg_CP, avg_neo_sg)
colSums(!is.na(full_data_month_mediate))
dim(full_data_month)

# total effect iv has on dv plus covariates
fit.totaleffect <- lmer(avg_neo_sg ~ 
                          log2(avg_stdsg_CP) + 
                          age + 
                          sex + 
                          (1|subj), 
                        #family = Gamma("log")
                        full_data_month_mediate)


# effect of iv on mediator
fit.mediator <- lmer(avg_fgc ~ 
                       log2(avg_stdsg_CP) + 
                       age +
                       sex +
                       (1|subj),
                     #family = Gamma("log"),
                     data = full_data_month_mediate)

# effect of mediator on dv controlling for iv

fit.dv <- lmer(avg_neo_sg ~ 
                 log2(avg_fgc) + 
                 log2(avg_stdsg_CP) +
                 age +
                 sex +
                 (1|subj),
               #family = Gamma("log"),
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


