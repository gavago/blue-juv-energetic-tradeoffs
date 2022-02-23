library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)
library(mgcv)
library(lmerTest)

# all bind together well? -----

load("neo dataset full.Rdata", verbose = T)
load("cp dataset full.Rdata", verbose = T)
load("behav dataset month.Rdata", verbose = T)

# save each Rdata dataset to CV ----
# write.csv(neo_data_full, file = "neo dataset full.csv", row.names = F)
# write.csv(cp_raw, file = "cp dataset full.csv", row.names = F)
# write.csv(behav_data_month, file = "behav dataset month.csv", row.names = F)


# merging full_data ----

# check that time format is same
# x <- left_join(cp_raw, neo_data_full, by = "sample_number")
# x %>%
#   filter(time.x != time.y) %>%
#   select(starts_with("time"))

#check intersection names across all data frames
# intersect(names(neo_data_full), names(cp_raw)) %>%
#   intersect(names(behav_data_month))

full_data <- left_join(cp_raw, neo_data_full, by = intersect(names(neo_data_full), names(cp_raw))) %>%
  left_join(., behav_data_month) %>%
  select(group, subj, date, age, year, month, time, sample_number, stdsg_CP, neo_sg, everything())
nrow(full_data) # 620


# check out how many values NA per variable, should def be zero for age and sex
apply(full_data, 2, function(x) sum(is.na(x)))

# regression and viz neo cp ------
#viz

full_data %>%
  filter(neo_sg < 3000) %>%
ggplot(., aes(y = neo_sg, x = stdsg_CP, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") #highest cp values are low neo

# non linear relationship...
for_gam <- full_data %>% 
  select(neo_sg, stdsg_CP, age, sex, subj) %>%
  mutate(subj = as.factor(subj)) %>%
  na.omit()
neo_cp_gam <- gam(neo_sg ~ sex + s(stdsg_CP, k = 5) + s(age, k = 5) + s(subj, bs = "re"), family = gaussian("log"), data = for_gam)
gam.check(neo_cp_gam)
plot.gam(neo_cp_gam, pages = 1)
summary(neo_cp_gam)


# expected linear relationship
neo_cp_lm <- glmer(neo_sg ~ sex + scale(stdsg_CP) + scale(age) + (1|subj), family = Gamma("log"), data = full_data)
qqnorm(residuals(neo_cp_lm))
summary(neo_cp_lm)



# lean body mass - calculating cr sg resids ------
full_data %>%
  filter(SG > 1.003) %>%
  ggplot(., aes(y = Cr, x = SG/100, color = age)) +
  geom_point() +
  geom_smooth(method = "lm")

m <- full_data %>%
  lm(Cr ~ I(SG/100) + I((SG/100)^2), data = .)
slope_crsg <- coef(m)[[2]]

df_cr_resid <- full_data %>%
  select(subj, age, sex, neo_sg, stdsg_CP, Cr, SG) %>%
  mutate(expected_cr = SG * slope_crsg, cr_resid = Cr - expected_cr) 

# no clear relationship w age
df_cr_resid %>%
  filter(sex == "M") %>%
  ggplot(., aes(x = age, y = cr_resid)) +
  geom_point() +
  geom_smooth(method = "lm")

df_cr_resid %>%
  ggplot(., aes(x = sex, y = cr_resid)) +
  geom_point() +
  geom_boxplot()

df_cr_resid %>%
  filter(is.na(sex))


# time of day effects ------
lmer(neo_sg ~ time + (1|subj), data = full_data) %>% summary()
lmer(stdsg_CP ~ time + (1|subj), data = full_data) %>% summary()
plot(full_data$time, full_data$neo_sg) # no time of day effects...
plot(full_data$time, full_data$stdsg_CP) 


lmer(neo_sg ~ scale(stdsg_CP) + (1|subj), data = full_data) %>% summary()



