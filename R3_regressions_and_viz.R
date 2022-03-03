library(lmerTest)
library(tidyverse)
library(mgcv)

full_data <- read.csv("full_dataset_juv_immune_energetics.csv", header = T)

full_data$time <- unclass(as.POSIXct(full_data$time))


full_data %>%
  filter()

# Descriptives -------
# --- intra-assay CVs -----

# --- neo, cp, cr_resid by age ----
for_gam <- full_data %>% 
  select(neo_sg, stdsg_CP, age, sex, subj) %>%
  mutate(subj = as.factor(subj)) %>%
  na.omit()

neo_age_lm <- glmer(neo_sg ~ sex + scale(age) + sex*scale(age) + (1|subj), family = Gamma("log"), data = full_data)
qqnorm(residuals(neo_age_lm))
summary(neo_age_lm) # no relationship neo and age

cp_age_lm <- glmer(stdsg_CP ~ sex + scale(age) + sex*scale(age) + (1|subj), family = Gamma("log"), data = full_data)
qqnorm(residuals(cp_age_lm))
summary(cp_age_lm) # cp higher males than females, and increase w age, same as previously reported Thompson HB

lbm_age_lm <- lmer(cr_resid ~ sex + scale(age) + sex*scale(age) + (1|subj), data = full_data)
qqnorm(residuals(lbm_age_lm))
summary(lbm_age_lm) # strong increase lbm with age



# H1 - neo cp - regression and viz ------

# neo by age
full_data %>%
  filter(neo_sg < 3000) %>%
  ggplot(., aes(y = neo_sg, x = age, color = sex)) +
  geom_point() +
  geom_smooth(method = "loess")

# neo by CP
full_data %>%
  filter(neo_sg < 3000) %>%
  ggplot(., aes(y = neo_sg, x = stdsg_CP, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") #highest cp values are low neo


# expected linear relationship
neo_cp_lm <- glmer(neo_sg ~ sex + scale(stdsg_CP) + scale(age) + (1|subj), family = Gamma("log"), data = full_data)
qqnorm(residuals(neo_cp_lm))
summary(neo_cp_lm)
hist(full_data$neo_sg)

# H2 - cr_sg neo - regression and viz  ------

# now neo and cr_Sg
#viz
full_data %>%
  filter(neo_sg < 3000) %>%
  ggplot(., aes(y = cr_resid, x = neo_sg, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm")

crsg_neo_lm <- lmer(cr_resid ~ sex + scale(neo_sg) + scale(age) + (1|subj), data = full_data)
qqnorm(residuals(crsg_neo_lm))
summary(crsg_neo_lm)



# time of day effects ------
str(full_data)

lmer(neo_sg ~ time + (1|subj), data = full_data) %>% summary()
lmer(stdsg_CP ~ time + (1|subj), data = full_data) %>% summary()
plot(full_data$time, full_data$neo_sg) # no time of day effects...
plot(full_data$time, full_data$stdsg_CP) 


