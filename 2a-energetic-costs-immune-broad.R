library(tidyverse)
library(lmerTest)

source("functions/vif.mer function.R") # vif.mer

load("data/full_data_month_udata_fgc_behav.RData", verbose = T)

# H1 - energetic costs cellular immunity -----

# - does neo eat into energy balance? ----
cp_neo_glm_month <- glmer(avg_stdsg_CP ~ sex +
                            age +
                            mrank +
                            log2(avg_neo_sg) +
                            (1|subj), 
                          family = Gamma("log"), 
                          data = full_data_month)

qqnorm(residuals(cp_neo_glm_month))
qqline(residuals(cp_neo_glm_month))
summary(cp_neo_glm_month)
vif.mer(cp_neo_glm_month) # all < 1.04


# viz
full_data_month %>% filter(avg_stdsg_CP < 4000, avg_neo_sg < 2000) %>%
  ggplot(aes(y = avg_stdsg_CP, x = avg_neo_sg, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(y = "C-peptide",
       x = "Neopterin",
       title = "Neopterin and Energy Balance")



# - - -  does feeding, moving, or resting compensate for the cost of neo? ----

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
vif.mer(f_neo_lm_month) # all < 1.17

#hist(full_data_month$f)
full_data_month %>%
  ggplot(aes(x = log2(avg_neo_sg) , y = f, color = sex)) +
  geom_point()

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
vif.mer(r_neo_lm_month) # all < 1.16

#hist(full_data_month$r)
full_data_month %>%
  ggplot(aes(x = log2(avg_neo_sg) , y = r, color = sex)) +
  geom_point()

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
vif.mer(m_neo_lm_month) # all < 1.16

#hist(full_data_month$m)
full_data_month %>%
  ggplot(aes(x = log2(avg_neo_sg) , y = m, color = sex)) +
  geom_point()


# - is neo/immunity prioritized over body mass/growth?  ----

lbm_neo_lm_month <- lmer(avg_cr_resid ~ 
                           age +
                           sex + 
                           mrank +
                           log2(avg_neo_sg) +
                           (1|subj),
                         data = full_data_month)
qqnorm(residuals(lbm_neo_lm_month))
qqline(residuals(lbm_neo_lm_month))
summary(lbm_neo_lm_month)
vif.mer(lbm_neo_lm_month) # all < 1.05

full_data_month %>% 
  filter(avg_neo_sg < 2000) %>% 
  ggplot(aes(x = avg_neo_sg, y = avg_cr_resid, color = sex)) +
  geom_smooth(method = "lm") + 
  geom_point() +
  theme_minimal() + 
  labs(x = "Median Neopterin by Month",
       y =  "Median Creatinine Residuals by Month",
       title = "Neopterin Relationship with Lean Tissue")


# save models ----
save(cp_neo_glm_month, f_neo_lm_month, r_neo_lm_month, m_neo_lm_month,
     lbm_neo_lm_month, file = "models/energetic-costs-immune.Rdata")

# H2b mechanism of limitation
# mediation


# graveyard ------
neo_fai_cp_glm_month_nolog <- glmer(avg_neo_sg ~ sex +
                                      scale(age) + scale(fai) +
                                      scale(avg_stdsg_CP) +
                                      (1|group/subj), 
                                    family = Gamma("log"), 
                                    data = full_data_month,
                                    control = glmerControl(optimizer ="Nelder_Mead"))

qqnorm(residuals(neo_fai_cp_glm_month_nolog))
qqline(residuals(neo_fai_cp_glm_month_nolog))
# - neo ~ cp controlling for fgc ----
# neo_cp_fgc_glm_month <- glmer(avg_neo_sg ~ 
#                                 sex +
#                                 age +
#                                 log2(avg_stdsg_CP) +
#                                 log2(avg_fgc) +
#                                 (1|subj), 
#                               family = Gamma("log"),
#                               data = full_data_month,
#                               control = glmerControl(optimizer ="Nelder_Mead"))
# qqnorm(residuals(neo_cp_fgc_glm_month))
# qqline(residuals(neo_cp_fgc_glm_month))
# hist(residuals(neo_cp_glm_month))
# summary(neo_cp_fgc_glm_month)
# 
# vif.mer(neo_cp_fgc_glm_month) # all < 1.03summary(neo_fai_cp_glm_month_nolog)


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


#  is food availability responsible for relationship bw cp and neo ----
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
vif.mer(cp_fai_glm_month) # all < 1.009

neo_fai_glm_month <- glmer(avg_neo_sg ~ sex +
                             age + scale(log2(fai)) +
                             (1|subj), 
                           family = Gamma("log"), 
                           data = full_data_month,
                           control = glmerControl(optimizer ="Nelder_Mead"))
summary(neo_fai_glm_month)

neo_fai_cp_glm_month <- glmer(avg_neo_sg ~ sex +
                                scale(age) + scale(log2(fai)) +
                                scale(log2(avg_stdsg_CP)) +
                                (1|group/subj), 
                              family = Gamma("log"), 
                              data = full_data_month,
                              control = glmerControl(optimizer ="Nelder_Mead"))
qqnorm(residuals(neo_fai_cp_glm_month))
qqline(residuals(neo_fai_cp_glm_month))
summary(neo_fai_cp_glm_month)
vif.mer(neo_fai_cp_glm_month) # all < 1.08

# stronger relationship btw fai and neo than cp and neo (ind of each other)
# suggests fruit availability has stronger influence on neo than energy
# stores released/other food in environment
