library(tidyverse)
library(ggeffects)
library(splines)
library(lmerTest)

load("data/full_data_short_term_lbm_change.Rdata", verbose = T)
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)

# marginal effects cp ~ neo ----
cp_neo_glm_month <- glmer(avg_stdsg_CP ~ sex +
                            age +
                            mrank +
                          log2(avg_neo_sg) + (1|subj),
                          family = Gamma("log"), 
                          data = full_data_month)

cp_neo_glm_marg_effect <- ggemmeans(cp_neo_glm_month, terms = "avg_neo_sg")

cp_neo_glm_marg_effect

ggplot(cp_neo_glm_marg_effect, aes(x, predicted)) +
  geom_line(color = "deepskyblue4") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = .3, 
              fill = "deepskyblue4") +
  ylab("Predicted C-peptide")+
  xlab(bquote(Neopterin(log[2])))
 
# marginal effects lbm ~ neo ----
lbm_neo_lm_month <- lmer(avg_cr_resid ~ 
                           age +
                           sex + 
                           mrank +
                           log2(avg_neo_sg) +
                           (1|subj),
                         data = full_data_month)

lbm_neo_marg_effect <- ggemmeans(lbm_neo_lm_month, terms = "avg_neo_sg")

ggplot(lbm_neo_marg_effect, aes(x, predicted)) +
  geom_line(color = "coral3") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = .3, 
              fill = "coral3") +
  ylab("Predicted Estimated Lean Body Mass")+
  xlab(bquote(Neopterin(log[2])))

# marginal effects short term lbm change ~ neo ----
change_lbm_neo_lm_sample <-
  lmer(sample_lbm_change ~ 
         age + 
         sex +
         log2_neo +
         sample_interval +
         log2_neo*sample_interval +
         (1|subj),
       data = full_data_short_term_lbm_change)

change_lbm_neo_marg_effect <- ggemmeans(change_lbm_neo_lm_sample, terms = "log2_neo")
# NOTE: Results may be misleading due to involvement in interactions 

ggplot(change_lbm_neo_marg_effect, aes(x, predicted)) +
  geom_line(color = "deeppink3") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = .3, 
              fill = "deeppink3") +
  ylab("Predicted Change in Lean Body Mass")+
  xlab(bquote(Neopterin(log[2])))
# not logging predictor
# no difference with interaction term removed

# marginal effects f ~ neo ----
f_neo_lm_month <- lmer(f ~ sex + 
                         age +
                         log2(avg_neo_sg) + 
                         log2(avg_stdsg_CP) +
                         (1|subj), 
                       data = full_data_month)

f_neo_lm_month_marg_effect <- ggemmeans(f_neo_lm_month, terms = "avg_neo_sg")

ggplot(f_neo_lm_month_marg_effect, aes(x, predicted)) +
  geom_line(color = "deeppink3") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = .3, 
              fill = "deeppink3") +
  ylab("Predicted Feeding")+
  xlab(bquote(Neopterin(log[2])))
