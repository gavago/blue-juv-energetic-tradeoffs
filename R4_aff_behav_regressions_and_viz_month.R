library(tidyverse)
library(lmerTest)


# data includes fgcs and overall is summarized by subj year month
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)
view(full_data_month)


# H3 investment in affiliative behavior vs immunity ----

# all affil behav viz (boring)

full_data_month %>% 
  group_by(subj, month, year) %>% 
  mutate(affil_behav = median(c(pl, gm, f))) %>% 
  ungroup() %>% 
  filter (med_neo_sg < 2000, affil_behav < .2) %>% 
  ggplot(aes(x = affil_behav, y = med_neo_sg, color = sex)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  labs(x = "Affiliative Behavior (Play, Giving and Recieving Grooming)", 
       y = "Median Neopterin by Month",
       title = "Neopterin and Affiliative Behavior") 

# Male only play model ----
# results: neg corr w large std error (estimate = -.07584 +- 1.96* .03781, p-score = .0448)

full_data_month %>% 
  #filter(med_neo_sg < 2000, pl < .1) %>% 
  filter(sex == "M") %>%
  ggplot(aes(x = pl, y = med_neo_sg, color = sex)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal() + labs(x = "Proportion of Time Playing",
                         y =  "Monthly Median Neopterin", 
                         title = "Immunity and Play")

neo_pl_glm_month_male <- full_data_month %>%
  filter(sex == "M") %>%
  glmer(med_neo_sg ~ scale(pl) + scale(age) + scale(med_stdsg_CP) + (1|subj), 
        family = Gamma("log"), data = .)

neo_pl_glm_month_male_int <- full_data_month %>%
  filter(sex == "M") %>%
  glmer(med_neo_sg ~ scale(pl) + scale(age) + scale(med_stdsg_CP) + 
          scale(med_stdsg_CP)*scale(pl) + (1|subj), 
        family = Gamma("log"), data = .)

qqnorm(residuals(neo_pl_glm_month_male))
qqline(residuals(neo_pl_glm_month_male))
summary(neo_pl_glm_month_male_int)

# Female only grooming model ----
# grooming given
full_data_month %>% filter(sex == "F") %>%
  ggplot(aes(x = gm, y = med_neo_sg, color = sex)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal() + labs(x = "Proportion of Time Grooming",
                         y =  "Monthly Median Neopterin", 
                         title = "Immunity and Grooming")

neo_gm_glm_month_female <- full_data_month %>%
  filter(sex == "F") %>%
  glmer(med_neo_sg ~ scale(gm) + scale(age) + scale(med_stdsg_CP) + (1|subj), 
        family = Gamma("log"), data = .)

qqnorm(residuals(neo_gm_glm_month_female))
qqline(residuals(neo_gm_glm_month_female))
summary(neo_gm_glm_month_female)
# negative estimate but high P value, no significant relationship

neo_gm_glm_month_female_int <- full_data_month %>%
  filter(sex == "F") %>%
  glmer(med_neo_sg ~ scale(gm) + scale(age) + scale(med_stdsg_CP) + 
          scale(med_stdsg_CP)*scale(gm) + (1|subj), 
        family = Gamma("log"), data = .)

qqnorm(residuals(neo_gm_glm_month_female_int))
qqline(residuals(neo_gm_glm_month_female_int))
summary(neo_gm_glm_month_female_int)

# grooming recieved 
full_data_month %>% filter(sex == "F") %>%
  ggplot(aes(x = gmd, y = med_neo_sg, color = sex)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal() + labs(x = "Proportion of Time Receiving Grooming",
                         y =  "Monthly Median Neopterin", 
                         title = "Immunity and Grooming Received")

neo_gmd_glm_month_female <- full_data_month %>%
  filter(sex == "F") %>%
  glmer(med_neo_sg ~ scale(gmd) + scale(age) + scale(med_stdsg_CP) + (1|subj), 
        family = Gamma("log"), data = .)

qqnorm(residuals(neo_gmd_glm_month_female))
qqline(residuals(neo_gmd_glm_month_female))
summary(neo_gmd_glm_month_female)

# negative estimate but high P value, no significant relationship

neo_gmd_glm_month_female_int <- full_data_month %>%
  filter(sex == "F") %>%
  glmer(med_neo_sg ~ scale(gmd) + scale(age) + scale(med_stdsg_CP) + 
          scale(med_stdsg_CP)*scale(gmd) + (1|subj), 
        family = Gamma("log"), data = .)

qqnorm(residuals(neo_gmd_glm_month_female_int))
qqline(residuals(neo_gmd_glm_month_female_int))
summary(neo_gmd_glm_month_female_int)

# Male only grooming model ----
full_data_month %>% filter(sex == "M") %>%
  ggplot(aes(x = gm, y = med_neo_sg, color = sex)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal() + labs(x = "Proportion of Time Grooming",
                         y =  "Monthly Median Neopterin", 
                         title = "Immunity and Grooming")

neo_gm_glm_month_male <- full_data_month %>%
  filter(sex == "M") %>%
  glmer(med_neo_sg ~ scale(gm) + scale(age) + scale(med_stdsg_CP) + (1|subj), 
        family = Gamma("log"), data = .)

summary(neo_gm_glm_month_male)

neo_gm_glm_month_male_int <- full_data_month %>%
  filter(sex == "M") %>%
  glmer(med_neo_sg ~ scale(gm) + scale(age) + scale(med_stdsg_CP) + 
          scale(med_stdsg_CP)*scale(gm) + (1|subj), 
        family = Gamma("log"), data = .)

summary(neo_gm_glm_month_male_int)
# negative relationship btw gm given and neo

# grooming received 

full_data_month %>% filter(sex == "M") %>%
  ggplot(aes(x = gmd, y = med_neo_sg, color = sex)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal() + labs(x = "Proportion of Time Receiving Grooming",
                         y =  "Monthly Median Neopterin", 
                         title = "Immunity and Recieving Grooming")

neo_gmd_glm_month_male <- full_data_month %>%
  filter(sex == "M") %>%
  glmer(med_neo_sg ~ scale(gmd) + scale(age) + scale(med_stdsg_CP) + (1|subj), 
        family = Gamma("log"), data = .)

summary(neo_gmd_glm_month_male)

neo_gmd_glm_month_male_int <- full_data_month %>%
  filter(sex == "M") %>%
  glmer(med_neo_sg ~ scale(gmd) + scale(age) + scale(med_stdsg_CP) + 
          scale(med_stdsg_CP)*scale(gmd) + (1|subj), 
        family = Gamma("log"), data = .)

summary(neo_gmd_glm_month_male_int)

full_data_month_1_subj <- full_data_month %>% 
  group_by(subj) %>% summarise(sex = sex, subj = subj) %>% distinct()
full_data_month_1_subj %>% filter(sex == "F") %>% nrow()
# same amt of male and female subjects

# neo ~ pl with low cp ----
full_data_month %>% 
  filter(med_neo_sg < 2000, sex == "M") %>% 
  mutate(cp_bin = quantcut(med_stdsg_CP, q = 3)) %>%
  ggplot(aes(x = pl, y = med_neo_sg, color = cp_bin)) +
  geom_smooth(method = "lm")

full_data_month %>%  filter(med_neo_sg < 2000, sex == "M") %>% 
  mutate(pl_bin = quantcut(pl, q = 6)) %>%
  ggplot(aes(x = pl_bin, y = med_neo_sg)) + geom_boxplot() 

full_data_month_cp_bin <- full_data_month %>% 
  mutate(cp_bin = quantcut(med_stdsg_CP, q = 6))

lo_cp <- full_data_month_cp_bin %>%
  filter(med_stdsg_CP < 6190) %>% 
  as.data.frame()

two.way <- aov(med_neo_sg ~ scale(med_stdsg_CP) + scale(pl), data = full_data_month)
summary(two.way)

neo_pl_glm_month_lo_cp <- glmer(med_neo_sg ~ sex +
                                  scale(pl) + 
                                  scale(age) +
                                  (1|subj), 
                                family = Gamma("log"),
                                data = lo_cp)
summary(neo_pl_glm_month_lo_cp)

hist(neo_pl_glm_month_lo_cp)
plot(residuals(neo_pl_glm_month_lo_cp))
qqplot(residuals(neo_pl_glm_month_lo_cp))

sum(full_data_month$pl == 0, na.rm = TRUE)

#exploratory
full_data_month %>%  filter(med_neo_sg < 2000) %>% 
  mutate(pl_bin = quantcut(pl, q = 6)) %>%
  ggplot(aes(x = pl_bin, y = med_neo_sg)) + geom_boxplot() 

full_data_month %>%
  ggplot(aes(x = neo_bin, y = pl)) + geom_boxplot()

full_data_month %>%
  ggplot(aes(x = neo_bin, y = pl, color = sex))  + geom_boxplot()

full_data_month %>% filter(med_stdsg_CP < 20000) %>% 
  ggplot(aes(x = med_stdsg_CP, y = pl, color = sex)) + 
  geom_point() + 
  geom_smooth(method = "lm")
# stronger relationship when controlling for fgc (estimate = -.09055 +- 1.96*.03773, p = .01639)

neo_pl_fgc_glm_month <- glmer(med_neo_sg ~ sex +
                                scale(pl) + 
                                scale(age) + 
                                scale(avg_fgc) +
                                (1|subj), 
                              family = Gamma("log"),
                              data = full_data_month)
qqnorm(residuals(neo_pl_fgc_glm_month))
qqline(residuals(neo_pl_fgc_glm_month))
summary(neo_pl_fgc_glm_month)

# grooming given and neo
# bad q-q plot
# Warning message : Model failed to converge with max|grad| = 0.0359666 (tol = 0.002, component 1)
# results: neg corr (estimate = -.125426 +- 1.96* .001956, <p score = 2e-16)

full_data_month %>% filter(gm < .10) %>% 
  ggplot(aes(x = gm, y = med_neo_sg, color = sex)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  theme_minimal() + 
  labs(x = "Proportion of Time Spent Grooming",
       y =  "Monthly Median Neopterin", 
       title = "Immunity and Grooming")

neo_gm_glm_month <- glmer(med_neo_sg ~ sex +
                            scale(gm) + 
                            scale(age) +
                            (1|subj), 
                          family = Gamma("log"), 
                          data = full_data_month)
qqnorm(residuals(neo_gm_glm_month))
qqline(residuals(neo_gm_glm_month))
summary(neo_gm_glm_month)

# grooming recieved and neo 

# non-positive values not allowed for gamma and cant take log of y bc of 0s
# Error in mkRespMod(fr, REML = REMLpass) : NA/NaN/Inf in 'y'

full_data_month %>% filter(gmd < .08) %>% 
  ggplot(aes(x = gmd, y = med_neo_sg, color = sex)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  theme_minimal() + 
  labs(x = "Proportion of Time Spent Recieving Grooming",
       y =  "Monthly Median Neopterin", 
       title = "Immunity and Grooming Recieved")

neo_gmd_glm_month <- glmer(med_neo_sg ~ sex +
                             scale(gmd) + 
                             scale(age) +
                             (1|subj), 
                           family = Gamma("log"),
                           data = full_data_month)
qqnorm(residuals(neo_gmd_glm_month))
qqline(residuals(neo_gmd_glm_month))
summary(neo_gm_glm_month)
