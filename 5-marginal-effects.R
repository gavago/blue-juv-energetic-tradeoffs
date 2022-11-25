library(tidyverse)
library(ggeffects)
library(splines)
library(lmerTest)

load("data/full_data_short_term_lbm_change.Rdata", verbose = T)
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)
load("models/energetic-costs-immune-broad.Rdata", verbose = T)
load("models/energetic-costs-immune-short.Rdata", verbose = T)

# raw data points w marginal effects smooth, cp ~ neo ----

summary(cp_neo_lm_month)
cp_neo_lm_marg_effect <- ggemmeans(cp_neo_lm_month, terms = "log2_avg_neo")

cp_neo_lm_marg_effect

# both axes logged
ggplot() + 
  geom_point(data = full_data_month, aes(x = log2_avg_neo, y = log2_avg_cp_tar)) +
  geom_line(data = cp_neo_lm_marg_effect, aes(x, predicted), color = "deepskyblue4") +
  geom_ribbon(data = cp_neo_lm_marg_effect, aes(x= x, y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = .3, 
              fill = "deepskyblue4") +
  ylab(expression(log[2]~"C-peptide" ~ "ng/ml")) +
  xlab(expression(log[2]~Neopterin~"ng/ml")) +
  theme_minimal()
# example of what each viz should do ^
# color theme outcomes w points, minimal theme etc

# neo logged
ggplot() + 
  geom_point(data = full_data_month, aes(x = log2_avg_neo, y = avg_cp_sg_tar)) +
  geom_line(data = cp_neo_lm_marg_effect, aes(x, 2^predicted), color = "deepskyblue4") +
  geom_ribbon(data = cp_neo_lm_marg_effect, aes(x= x, y = 2^predicted, ymin = conf.low, ymax = conf.high), 
              alpha = .3, 
              fill = "deepskyblue4") +
  ylab("C-peptide ng/ml (time-adjusted)") +
  xlab(expression(log[2]~Neopterin~"ng/ml")) +
  theme_minimal()

# reminder why logging...
# ggplot(full_data_month, aes(x = avg_neo_sg, y = avg_cp_sg)) +
#   geom_point() +
#   geom_smooth(method = "lm")

# marginal effects lbm ~ neo ----
lbm_neo_marg_effect <- ggemmeans(lbm_neo_lm_month, terms = "log2_avg_neo")

ggplot(lbm_neo_marg_effect, aes(x, predicted)) +
  geom_line(color = "coral3") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = .3, 
              fill = "coral3") +
  ylab("Predicted Estimated Lean Body Mass")+
  xlab(bquote(Neopterin(log[2])))

# marginal effects short term lbm change ~ neo ----

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


# Supplemental exploratory ---------
# marginal effects f ~ neo ----
r_neo_lm_month <- lmer(r ~ sex + 
                         age +
                         log2_avg_neo+ 
                         log2_avg_stdsg_CP +
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
