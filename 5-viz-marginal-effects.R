library(tidyverse)
library(ggeffects)
library(splines)
library(lmerTest)

load("data/full_data_short_term_lbm_change.Rdata", verbose = T)
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)
load("models/energetic-costs-immune-broad.Rdata", verbose = T)
load("models/energetic-costs-immune-short.Rdata", verbose = T)

# raw data points w marginal effects smooth, cp ~ neo ----

cp_neo_lm_marg_effect <- ggemmeans(cp_neo_lm_month, terms = "log2_avg_neo")

cp_neo_lm_marg_effect

# both axes logged
ggplot() + 
  geom_point(data = full_data_month, aes(x = log2_avg_neo, y = log2_avg_cp_tar), alpha = .3) +
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
  geom_point(data = full_data_month, aes(x = log2_avg_neo, y = avg_cp_sg_tar), alpha = .3) +
  geom_line(data = cp_neo_lm_marg_effect, aes(x, 2^predicted), color = "deepskyblue4") +
  geom_ribbon(data = cp_neo_lm_marg_effect, aes(x= x, y = 2^predicted, ymin = 2^conf.low, ymax = 2^conf.high), 
              alpha = .4, 
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

ggplot() +
  geom_point(data = full_data_month, aes(x = log2_avg_neo, y = avg_cr_resid), alpha = .3) +
  geom_line(data = lbm_neo_marg_effect, aes(x, predicted), color = "coral3") +
  geom_ribbon(data = lbm_neo_marg_effect, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
              alpha = .4,
              fill = "coral3") +
  ylab("Estimated Lean Body Mass") +
  xlab(expression(log[2]~Neopterin~"ng/ml")) +
  theme_minimal()


# PAPER/POSTER FIGURES --------
# fig 1 - validation - elbm by age -------
# fig 2a - costs - short term lbm change ~ neo ----

change_lbm_neo_marg_effect <- ggemmeans(change_lbm_neo_lm_sample, terms = "log2_neo")
# NOTE: Results may be misleading due to involvement in interactions 

ggplot() +
  geom_point(data = full_data_short_term_lbm_change, aes(x = log2_neo, y = sample_lbm_change), alpha = .3) +
  geom_line(data = change_lbm_neo_marg_effect, aes(x, predicted), color = "deeppink3") +
  geom_ribbon(data = change_lbm_neo_marg_effect, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = .3, 
              fill = "deeppink2") +
  ylab(expression("∆t"[2]-t[1]~"in"~Estimated~Lean~Body~Mass)) +
  xlab(expression(log[2]~Neopterin[t1]~"ng/ml")) +
  theme_minimal()
  

# not logging predictor
# no difference with interaction term removed

# fig 2b - costs - interaction lbm change ~ sample interval*neo ------
# fig 3a - constraints - neo ~ cp marg effect --------
# fig 3b - constraints - neo ~ lbm marg effect --------

# Supplemental exploratory ---------
# marginal effects r ~ neo ----
r_neo_lm_month <- lmer(r ~ sex + 
                         age +
                         log2_avg_neo+ 
                         log2_avg_cp_tar +
                         (1|subj), 
                       data = full_data_month)

r_neo_lm_month_marg_effect <- ggemmeans(r_neo_lm_month, terms = "log2_avg_neo")

ggplot() +
  geom_point(data = full_data_month, aes(x = log2_avg_neo, y = r), alpha = .3) +
  geom_line(data = r_neo_lm_month_marg_effect, aes(x, predicted), color = "dark green") +
  geom_ribbon(data = r_neo_lm_month_marg_effect, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = .3, 
              fill = "dark green") +
  ylab("Time resting")+
  xlab(bquote(Neopterin(log[2])))

