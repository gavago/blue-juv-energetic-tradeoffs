library(lmerTest)
library(tidyverse)
library(mgcv)

full_data <- read.csv("data/full_dataset_juv_immune_energetics.csv", header = T)

full_data$time <- unclass(as.POSIXct(full_data$time))


# Descriptives -------
# --- CP CVs -----
cp_cv <- read.csv("data/CP_IntraassayCV.csv", header = T)
str(cp_cv)

#average high and low control CVs
cp_cv %>%
  filter(grepl("Control", sample)) %>%
  group_by(sample) %>%
  summarise(inter_cv = mean(CV)) %>%
  ungroup()

# isolate samples CVs
cv_num <- cp_cv %>%
  filter(!grepl("Control", sample)) %>%
  filter(!grepl("Standard", sample)) %>%
  filter(!grepl("x", sample)) %>%
  filter(!grepl("redo", notes)) %>%
  mutate(sample_number = as.numeric(sample)) %>%
  select(sample_number, CV, notes)

# find intra-assay CV of samples in full dataset
full_data %>%
  select(sample_number, stdsg_CP) %>%
  left_join(., cv_num) %>%
  summarise(mean = mean(CV, na.rm = T))

nrow(full_data)


cv_num$sample

# --- Neo CVs -----
full_data %>% 
  summarise(mean = mean(neo_CV, na.rm = T))

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
  geom_smooth(method = "lm")



# neo by CP
neo_cp <- full_data %>%
  filter(neo_sg < 3000) %>%
  ggplot(., aes(y = neo_sg, x = stdsg_CP, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") + #highest cp values are low neo
  theme_minimal() +
  theme(legend.position = "none") 
neo_cp  
ggsave(file = "results/neo by cp viz.jpg", plot = neo_cp, height = 4, width = 4)

# expected linear relationship
neo_cp_lm <- glmer(neo_sg ~ sex + scale(stdsg_CP) + scale(age) + (1|subj), family = Gamma("log"), data = full_data)
qqnorm(residuals(neo_cp_lm))
summary(neo_cp_lm)
hist(full_data$neo_sg)

# H2 - cr_sg neo - regression and viz  ------

# now neo and cr_Sg
#viz
neo_crsg <- full_data %>%
  filter(neo_sg < 3000) %>%
  ggplot(., aes(x = cr_resid, y = neo_sg, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(legend.position = "none") 
neo_crsg
ggsave(file = "results/neo by creatinine resid viz.jpg", plot = neo_crsg, height = 4, width = 4)


neo_crsg_lm <- lmer(neo_sg ~ sex + scale(cr_resid) + scale(age) + (1|subj), data = full_data)
qqnorm(residuals(neo_crsg_lm))
summary(neo_crsg_lm)



# time of day effects ------
str(full_datax)

full_datax <- full_data %>%
  mutate(time = strptime(time, format = "%H:%M"))

lmer(neo_sg ~ time + (1|subj), data = full_data) %>% summary()
lmer(stdsg_CP ~ time + (1|subj), data = full_data) %>% summary()
plot(full_data$time, full_data$neo_sg) # no time of day effects...
plot(full_data$time, full_data$stdsg_CP) 


