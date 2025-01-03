library(tidyverse)
library(ggeffects)
library(splines)
library(lmerTest)

load("data/full_data_short_term_lbm_change.Rdata", verbose = T)
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)
load("models/energetic-costs-immune-broad.Rdata", verbose = T)
load("models/energetic-costs-immune-short.Rdata", verbose = T)
load("data/urine_sample_dataset_juv_immune_energetics.Rdata", verbose = T)

# actually chose to make the viz var one that includes negative time adjusted residuals...
# the positive transformation was to be able to take the log of the response var, not for viz.

#  actual marginal effects are visualized...?


# PAPER/POSTER FIGURES --------
# fig 1A - validation - monthly elbm by age -------

# geom_smooth version

age_lbm_plot <- ggplot(data = full_data_month, aes(x = age, y = avg_cr_resid)) +
  geom_point(aes(color = sex), alpha = .5) + 
  scale_shape_manual(values = c(15, 16)) +
  geom_smooth(method = lm, color = "darkgrey", alpha = .3) + 
  theme_minimal() +
  labs(x = "Age (yrs)", y = "Residual creatinine (mg/ml)\nrelative to expected by specific gravity\n(Estimated lean body mass)")+
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = 10)) +
  theme(legend.position = "none")

age_lbm_plot
# put together w labels A and B, export as figure

ggplot(data = full_data_month, aes(x = age, y = avg_cr_resid)) +
  geom_point(aes(color = sex), alpha = .5) + 
  scale_shape_manual(values = c(15, 16)) +
  geom_smooth(method = lm, color = "darkgrey", alpha = .3) + 
  labs(x = "Age (yrs)", y = "") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave(file = "results/fig-1a-age-lbm-plot.png", height = 5, width = 5)



# age_lbm_sample_plot <- ggplot(data = full_udata, aes(x = age, y = cr_resid)) +
#   geom_point(aes(color = sex), alpha = .5) + 
#   scale_shape_manual(values = c(15, 16)) +
#   geom_smooth(method = lm, color = "darkgray", fill = "darkgray", alpha = .3) + 
#   theme_minimal() 
# age_lbm_sample_plot



# fig 1B - validation - monthly elbm by monthly energy balance -------

ggplot(data = full_data_month, aes(x = log2_avg_cp_tar, y = avg_cr_resid)) +
  geom_point(aes(color = sex), alpha = .5) + 
  scale_shape_manual(values = c(15, 16)) +
  geom_smooth(method = lm, color = "darkgray", fill = "darkgray", alpha = .3) + 
  theme_minimal() +
  labs(x = expression(log[2]~"C-peptide" ~ "pg/ml (Energy balance)"), y = "") +
  theme(legend.position = "none")
ggsave(file = "results/fig-1b-lbm-plot.png", height = 5, width = 5)




# ucp_lbm_sample_plot <- ggplot(data = full_udata, aes(x = cp_tar, y = cr_resid)) +
#   geom_point(aes(color = sex), alpha = .5) + 
#   scale_shape_manual(values = c(15, 16)) +
#   geom_smooth(method = lm, color = "darkgray", fill = "darkgray", alpha = .3) + 
#   theme_minimal() 
# ucp_lbm_sample_plot

  
# fig 2 - costs - short term lbm change ~ neo ----


sample_lbm_chng_neo_plot <- 
  ggplot(data = full_data_short_term_lbm_change, 
         aes(x = log2_neo, y = sample_lbm_change, color = sex)) +
  geom_point(alpha = .5) + 
  geom_smooth(method = lm, color = "green4", fill = "green4", alpha = .3) + 
  ylab(expression("∆t"[2]-t[1]~"in"~Estimated~Lean~Body~Mass)) +
  xlab(expression(t[1]~log[2]~Neopterin~"ng/ml")) + 
  theme_classic() + 
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 10))

sample_lbm_chng_neo_plot

# no difference with interaction term removed

# fig 2b - costs - interaction lbm change ~ sample interval*neo ------


# find each quartile
quantile(full_data_short_term_lbm_change$sample_interval, na.rm = T, 
         probs=c(.25,.5,.75,1))

#create new column with sample quartiles
full_interaction_data <- full_data_short_term_lbm_change %>% 
  mutate(sample_int_quart = case_when(
    sample_interval <= 6 ~ "q1",
    sample_interval <= 12 ~"q2",
    sample_interval <= 22 ~ "q3",
    sample_interval <= 73 ~ "q4"))

#plot
change_lbm_sample_int_neo_plot <- full_interaction_data %>% 
  ggplot(aes(x = neo_sg, y = sample_lbm_change, color = sample_int_quart)) +
           geom_smooth(method = lm, alpha = .3) + xlim(0,2000) + 
  ylab(expression("∆t"[2]-t[1]~"in"~Estimated~Lean~Body~Mass)) +
  xlab(expression(t[1]~log[2]~Neopterin~"ng/ml")) +
  scale_color_discrete(name="Sample Interval Quartile",
                      labels=c("Q1 (0-6 days)", 
                               "Q2 (7-12 days)", 
                               "Q3 (13 - 22 days)", 
                               "Q4 (23 - 73 days)"))
change_lbm_sample_int_neo_plot 
  

# fig 3a - constraints - neo ~ cp marg effect --------

# w cp on x axis
cp_neo_plot <- 
  ggplot(data = full_data_month, aes(x = log2_avg_cp_tar, y = log2_avg_neo, color = sex),
         alpha = .3) +
  geom_point(alpha = .5) + 
  geom_smooth(method = lm, color = "steelblue3", fill = "steelblue3", alpha = .5) + 
  xlab(expression(log[2]~"C-peptide" ~ "pg/ml")) +
  ylab(expression(log[2]~Neopterin~"ng/ml")) +
  theme_classic() + 
  theme(legend.position = "none")  +
  theme(axis.title = element_text(size = 10))
cp_neo_plot

# export pdf
pdf("cp_neo_plot")
print(cp_neo_plot)
dev.off()


# fig 3b - constraints - neo ~ lbm marg effect --------

lbm_neo_plot <- 
  ggplot(data = full_data_month, aes(x = avg_cr_resid, y = log2_avg_neo, color = sex)) +
  geom_point(alpha = .5) +
  geom_smooth(method = lm, color = "steelblue4", fill = "steelblue4", alpha = .5) + 
  xlab("Estimated Lean Body Mass") +
  ylab(expression(log[2]~Neopterin~"ng/ml")) +
    theme_classic() + 
    theme(legend.position = "none")  +
    theme(axis.title = element_text(size = 10))
lbm_neo_plot

# export pdf
pdf("lbm_neo_plot")
print(lbm_neo_plot)
dev.off()

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
  geom_line(data = r_neo_lm_month_marg_effect, aes(x, predicted), color = "coral2") +
  geom_ribbon(data = r_neo_lm_month_marg_effect, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = .3, 
              fill = "coral2") +
  ylab("Time resting")+
  xlab(bquote(Neopterin(log[2])))



# graveyard -----
# raw data points w marginal effects smooth, cp ~ neo ----
# marginal effects: change lbm ~ neo ----
change_lbm_neo_marg_effect <- ggemmeans(change_lbm_neo_lm_sample, terms = "log2_neo")

change_lbm_neo_marg_effect_plot <- ggplot() +
  geom_point(data = full_data_short_term_lbm_change, 
             aes(x = log2_neo, y = sample_lbm_change), 
             alpha = .4) +
  geom_line(data = change_lbm_neo_marg_effect, 
            aes(x, predicted), 
            color = "deepskyblue4") +
  geom_ribbon(data = change_lbm_neo_marg_effect, 
              aes(x = x, y = predicted, 
                  ymin = conf.low, ymax = conf.high), 
              alpha = .4, 
              fill = "deepskyblue4") +
  ylab(expression("∆t"[2]-t[1]~"in"~Estimated~Lean~Body~Mass)) +
  xlab(expression(log[2]~Neopterin[t1]~"ng/ml")) + 
  theme_minimal ()

change_lbm_neo_marg_effect_plot

pdf("change_lbm_neo_marg_effect_plot")
print(change_lbm_neo_marg_effect_plot)
dev.off()

# fig 3b - constraints - neo ~ lbm marg effect --------
lbm_neo_marg_effect <- ggemmeans(lbm_neo_lm_month, terms = "log2_avg_neo")

lbm_neo_marg_effect_plot <- ggplot() +
  geom_point(data = full_data_month, 
             aes(x = log2_avg_neo, y = avg_cr_resid), alpha = .3) +
  geom_line(data = lbm_neo_marg_effect, 
            aes(x, predicted), color = "deeppink2") +
  geom_ribbon(data = lbm_neo_marg_effect, 
              aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = .2,fill = "deeppink2") +
  #geom_smooth(data = full_data_month, 
  #aes(x = log2_avg_neo, y = avg_cr_resid), alpha = .3, method = lm) +
  ylab("Creatinine residuals") +
  xlab(expression(log[2]~Neopterin~"ng/ml")) +
  theme_minimal()
# neo ~ cp marg effect ----
  cp_neo_lm_marg_effect <- ggemmeans(cp_neo_lm_month, terms = "log2_avg_neo")

cp_neo_lm_marg_effect_plot <- ggplot() + 
  geom_point(data = full_data_month, 
             aes(x = log2_avg_neo, y = log2_avg_cp_tar), alpha = .3) +
  geom_line(data = cp_neo_lm_marg_effect, 
            aes(x, predicted), color = "deeppink4") +
  geom_ribbon(data = cp_neo_lm_marg_effect, 
              aes(x, predicted, ymin = conf.low, ymax = conf.high), 
              alpha = .3, 
              fill = "deeppink4") +
  #geom_smooth(data = full_data_month, 
  #aes(x = log2_avg_neo, y = log2_avg_cp_tar), 
  #alpha = .3, color = "blue", method = lm) +
  ylab(expression(log[2]~"C-peptide" ~ "ng/ml")) +
  xlab(expression(log[2]~Neopterin~"ng/ml")) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 15))
# cr resid & age marginal effects ---- 
age_cr_lm_month <- lmer(avg_cr_resid ~ age + 
                          sex +  
                          mrank +
                          avg_cp_sg +
                          (1|subj),
                        data = full_data_month)

qqnorm(residuals(age_cr_lm_month))
qqline(residuals(age_cr_lm_month))

age_lbm_marg_effect <- ggemmeans(age_cr_lm_month, terms = "age")

age_lbm_plot <- ggplot() + 
  geom_point(data = full_data_month,
             aes(x = age, y = avg_cr_resid),
             alpha = .3) + 
  geom_line(data = age_lbm_marg_effect, 
            aes(x, predicted), 
            alpha = .3, color = "gray") +
  geom_ribbon(data = age_lbm_marg_effect, 
              aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = .3, fill = "gray") + 
  ylab(expression("Estimated Lean Body Mass")) +
  xlab(expression("Age"))
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
  geom_line(data = lbm_neo_marg_effect, aes(x, predicted), color = "tomato3") +
  geom_ribbon(data = lbm_neo_marg_effect, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
              alpha = .4,
              fill = "tomato3") +
  ylab("Estimated Lean Body Mass") +
  xlab(expression(log[2]~Neopterin~"ng/ml")) +
  theme_minimal()

