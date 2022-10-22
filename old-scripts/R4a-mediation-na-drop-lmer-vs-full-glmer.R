library(tidyverse)
library(lmerTest)

load("data/full_data_month_udata_fgc_behav.RData", verbose = T)

# for causal mediation analysis, need to look at mediation of FGCs in direct relation 
# to time equivalent CP and NEO, so all rows w NAs of any are dropped
full_data_month_mediate <- full_data_month %>% 
  drop_na(avg_fgc, avg_stdsg_CP, avg_neo_sg)

# drop na vs full - keep lmer ------
fit.totaleffect <- lmer(avg_neo_sg ~ 
                          log2(avg_stdsg_CP) + 
                          age + 
                          sex + 
                          (1|subj),
                        data = full_data_month_mediate)
fit.mediator <- lmer(avg_fgc ~ 
                       log2(avg_stdsg_CP) + 
                       age +
                       sex +
                       (1|subj),
                     data = full_data_month_mediate)
fit.dv <- lmer(avg_neo_sg ~ 
                 log2(avg_fgc) + 
                 log2(avg_stdsg_CP) +
                 age +
                 sex +
                 (1|subj),
               data = full_data_month_mediate)

fit.totaleffect_full <- lmer(avg_neo_sg ~ 
                          log2(avg_stdsg_CP) + 
                          age + 
                          sex + 
                          (1|subj),
                        data = full_data_month)
fit.mediator_full <- lmer(avg_fgc ~ 
                       log2(avg_stdsg_CP) + 
                       age +
                       sex +
                       (1|subj),
                     data = full_data_month)
fit.dv_full <- lmer(avg_neo_sg ~ 
                 log2(avg_fgc) + 
                 log2(avg_stdsg_CP) +
                 age +
                 sex +
                 (1|subj),
               data = full_data_month)

summary(fit.totaleffect)
summary(fit.totaleffect_full) # slightly stronger relationship CP on NEO with all FGC NAs included
summary(fit.mediator)
summary(fit.mediator_full) # stronger relationship CP on FGC with all NEO NAs included
summary(fit.dv)
summary(fit.dv_full) # no diff, bc all NAs dropped here too.


# glmer vs lmer, using mediation dataset ---------
fit.totaleffect_glmer <- glmer(avg_neo_sg ~
                            log2(avg_stdsg_CP) +
                              age + sex +
                            (1|subj), 
                          family = Gamma("log"),
                          data = full_data_month_mediate,
                          control = glmerControl(optimizer ="Nelder_Mead"))
# no fit.mediator_glmer - uses lmer
fit.dv_glmer <- glmer(avg_neo_sg ~ 
                                log2(avg_fgc) + 
                                log2(avg_stdsg_CP) +
                                age + sex +
                                (1|subj), 
                              family = Gamma("log"),
                              data = full_data_month_mediate,
                              control = glmerControl(optimizer ="Nelder_Mead"))

summary(fit.totaleffect)
summary(fit.totaleffect_glmer)
summary(fit.dv)
summary(fit.dv_glmer) 
# much stronger direct effect of CP on NEO (i.e. direct = controlling for FGCs)
# when using gamma error distribution vs gaussian
# gah. can't use mediate package.
# suppose must do deep dive/consultation about how to conduct appropriate 
# mediation analysis
# mediation package results are perhaps ok for tentative reporting in AABA abstract



# inspect model fits ---------
qqnorm(residuals(fit.mediator))
qqline(residuals(fit.mediator))
hist(residuals(fit.mediator))
qqnorm(residuals(fit.mediator))
qqline(residuals(fit.mediator))
hist(residuals(fit.mediator))