library(tidyverse)
library(lmerTest)

load("data/neo-dataset-full.Rdata", verbose = T)
load("data/cp-dataset-full.Rdata", verbose = T)
load("data/fgc_data_by_sample.Rdata", verbose = T)

# check for SG inflation -----

neo_data_full %>%
  mutate(blas_samp = (sample_number == 198)) %>% 
  ggplot(aes(x = SG, y = neo_sg, color = blas_samp)) +
  geom_point() +
  geom_smooth()

cp_raw %>%
  mutate(blas_samp = (sample_number == 198)) %>% 
  ggplot(aes(x = SG, y = stdsg_CP, color = blas_samp)) +
  geom_point() +
  geom_smooth()

neo_data_full1 <- neo_data_full %>%
  filter(SG >= 3)
cp_raw1 <- cp_raw %>%
  filter(SG >= 3)

# create hrs since midnight var ------

neo_data_full1$hrs_midnight <- format(as.POSIXct(neo_data_full1$time), format = "%H:%M:%S") %>%
  lubridate::hms(.) %>%
  as.numeric()/60/60 #as seconds, then divide into minutes and hours.
head(neo_data_full1$hrs_midnight)
head(neo_data_full1$time)

cp_raw1$hrs_midnight <- format(as.POSIXct(cp_raw1$time), format = "%H:%M:%S") %>%
  lubridate::hms(.) %>%
  as.numeric()/60/60 #as seconds, then divide into minutes and hours.
head(cp_raw1$hrs_midnight)
head(cp_raw1$time)

gc_raw$hrs_midnight <- format(as.POSIXct(gc_raw$time), format = "%H:%M:%S") %>%
lubridate::hms(.) %>%
as.numeric()/60/60 #as seconds, then divide into minutes and hours.
head(gc_raw$hrs_midnight)
head(gc_raw$time)

# time effects regressions -----
neo_time <- neo_data_full1 %>%
  glmer(neo_sg ~ hrs_midnight + (hrs_midnight|subj), family = Gamma("log"), data = .)
summary(neo_time)
cp_time <- cp_raw1 %>%
  glmer(stdsg_CP ~ hrs_midnight + (hrs_midnight|subj), family = Gamma("log"), data = .)
summary(cp_time)
gc_time <- glmer(fgc.ng_g.feces ~ hrs_midnight + (hrs_midnight|subj), family = Gamma("log"), data = gc_raw)
summary(gc_time)

# sig time effects for cp ^


# viz time effects for cp ----- 

plot(cp_raw1$hrs_midnight, cp_raw1$stdsg_CP)
abline(lm(cp_raw1$stdsg_CP ~ cp_raw1$hrs_midnight))


#individ slopes cp ~ time
cp_raw1 %>%
  ggplot( aes(x=hrs_midnight, y=stdsg_CP, color = subj)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm", se = F, alpha = 0.5) +
  theme(legend.position = "none")

# calculate predictions from cp-time -----

cp_time_intercept <- colMeans(coef(cp_time)$subj)[1]
cp_time_slope <- colMeans(coef(cp_time)$subj)[2]
cp_time_pred <- exp(cp_time_intercept + cp_time_slope * cp_raw1$hrs_midnight)

# compare ran slop and intercept pred to other pred values ------

tar <- cp_raw1 %>% # create time adjusted residuals
  filter(SG >= 3) %>%
  mutate(cp_tar1 = stdsg_CP - cp_time_pred) %>%
  mutate(cp_tar = cp_tar1 + abs(min(cp_tar1, na.rm = T)) + 0.0001)
pred2 <- predict(cp_time, type = "response") # incorporates individ rand slope


head(pred2)
head(cp_raw1$stdsg_CP)
head(tar$cp_tar)

par(mfrow = c(1,2))
hist(cp_raw1$stdsg_CP)
hist(tar$cp_tar)

tar_test <- glmer(cp_tar ~ hrs_midnight + (hrs_midnight|subj), family = Gamma("log"), data = tar)
summary(tar_test) # no time effect now.

plot(tar$hrs_midnight, tar$cp_tar)
abline(lm(tar$cp_tar ~ tar$hrs_midnight))

# save cp time effects ----- 
#to create time adjusted residual in 1b merge
save(cp_time, cp_time_intercept, cp_time_slope, cp_time_pred,
     file = "data/cp-time-of-day-model.Rdata")

