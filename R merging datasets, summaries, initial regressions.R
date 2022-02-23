library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)

# all bind together well? -----

load("neo dataset full.Rdata", verbose = T)
load("cp dataset full.Rdata", verbose = T)
load("behav dataset month.Rdata", verbose = T)

#write.csv(neo_data_full, file = "neo dataset full.csv", row.names = F)
#write.csv(cp_raw, file = "cp dataset full.csv", row.names = F)
#write.csv(behav_data_month, file = "behav dataset month.csv", row.names = F)

x <- left_join(cp_raw, neo_data_full, by = "sample_number")
x %>%
  filter(time.x != time.y) %>%
  select(starts_with("time"))

intersect(names(neo_data_full), names(cp_raw)) %>%
  intersect(names(behav_data_month))

full_data <- left_join(cp_raw, neo_data_full, by = intersect(names(neo_data_full), names(cp_raw))) %>%
  left_join(., behav_data_month) %>%
  select(group, subj, date, age, year, month, time, sample_number, stdsg_CP, neo_sg, everything())
names(full_data)

apply(neo_data_full, 2, function(x) sum(is.na(x)))

# modeling neo cp ------
library(mgcv)
library(lme4)

for_gam <- full_data %>% 
  select(neo_sg, stdsg_CP, age, sex, subj) %>%
  mutate(subj = as.factor(subj)) %>%
  na.omit()
neo_cp_gam <- gam(neo_sg ~ sex + s(stdsg_CP, k = 5) + s(age, k = 5) + s(subj, bs = "re"), family = gaussian("log"), data = for_gam)
gam.check(neo_cp_gam)
plot.gam(neo_cp_gam, pages = 1)
summary(neo_cp_gam)

neo_cp_lm <- glmer(neo_sg ~ sex + scale(stdsg_CP) + scale(age) + (1|subj), family = Gamma("log"), data = for_gam)
qqnorm(residuals(neo_cp_lm))
summary(neo_cp_lm)

ggplot(full_data, aes(y = neo_sg, x = stdsg_CP, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") #highest cp values are low neo

# modeling lean body mass - cr sg resids ------

ggplot(full_data, aes(y = Cr, x = SG/100, color = age)) +
  geom_point() +
  geom_smooth(method = "lm")

m <- lm(Cr ~ I(SG/100), data = full_data)
slope_crsg <- coef(m)[[2]]

df_cr_resid <- full_data %>%
  select(subj, age, sex, neo_sg, stdsg_CP, Cr, SG) %>%
  mutate(expected_cr = SG * slope_crsg, cr_resid = Cr - expected_cr) 


df_cr_resid %>%
  ggplot(., aes(x = age, y = cr_resid)) +
  geom_point() +
  geom_smooth(method = "lm")

df_cr_resid %>%
  ggplot(., aes(x = sex, y = cr_resid)) +
  geom_point() +
  geom_boxplot()


#time of day effects model ------
library(lmerTest)
lmer(neo_sg ~ time + (1|subj), data = jet_data) %>% summary()
lmer(stdsg_CP ~ time + (1|subj), data = jet_data) %>% summary()
plot(jet_data$time, jet_data$neo_sg) # no time of day effects...
plot(jet_data$time, jet_data$stdsg_CP) 


lmer(neo_sg ~ scale(stdsg_CP) + (1|subj), data = jet_data) %>% summary()



