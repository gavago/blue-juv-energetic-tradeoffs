library(tidyverse)
library(lmerTest)

load("data/neo-dataset-full.Rdata", verbose = T)
load("data/cp-dataset-full.Rdata", verbose = T)
load("data/fgc_data_by_sample.Rdata", verbose = T)


# create hrs since midnight var ------
neo_data_full$hrs_midnight <- format(as.POSIXct(neo_data_full$time), format = "%H:%M:%S") %>%
  lubridate::hms(.) %>%
  as.numeric()/60/60 #as seconds, then divide into minutes and hours.
head(neo_data_full$hrs_midnight)
head(neo_data_full$time)

cp_raw$hrs_midnight <- format(as.POSIXct(cp_raw$time), format = "%H:%M:%S") %>%
  lubridate::hms(.) %>%
  as.numeric()/60/60 #as seconds, then divide into minutes and hours.
head(cp_raw$hrs_midnight)
head(cp_raw$time)

gc_raw$hrs_midnight <- format(as.POSIXct(gc_raw$time), format = "%H:%M:%S") %>%
lubridate::hms(.) %>%
as.numeric()/60/60 #as seconds, then divide into minutes and hours.
head(gc_raw$hrs_midnight)
head(gc_raw$time)

# time effects regressions -----
neo_time <- glmer(neo_sg ~ hrs_midnight + (hrs_midnight|subj), family = Gamma("log"), data = neo_data_full)
summary(neo_time)
cp_time <- glmer(stdsg_CP ~ hrs_midnight + (hrs_midnight|subj), family = Gamma("log"), data = cp_raw)
summary(cp_time)
gc_time <- glmer(fgc.ng_g.feces ~ hrs_midnight + (hrs_midnight|subj), family = Gamma("log"), data = gc_raw)
summary(gc_time)

# sig time effects for cp


# viz time effects for cp ----- 
plot(full_udata$time, full_udata$neo_sg)
plot(full_udata$time, full_udata$stdsg_CP)
abline(lm(full_udata$stdsg_CP ~ full_udata$time))
plot(tar$time, tar$cp_tar)
abline(lm(tar$cp_tar ~ tar$time))

#individ slopes cp ~ time
full_udata %>%
  ggplot( aes(x=hrs_midnight, y=stdsg_CP, color = subj)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm", se = F, alpha = 0.5) +
  theme(legend.position = "none")

# calculate predictions from cp-time -----

cp_time_intercept <- colMeans(coef(cp_time)$subj)[1]
cp_time_slope <- colMeans(coef(cp_time)$subj)[2]
cp_time_pred <- exp(cp_time_intercept + cp_time_slope * cp_raw$hrs_midnight)

# compare ran slop and intercept pred to other pred values ------

tar <- full_udata %>% # create time adjusted residuals
  mutate(cp_tar1 = stdsg_CP - pred1) %>%
  mutate(cp_tar = cp_tar1 + abs(min(cp_tar1, na.rm = T)) + 0.0001)
pred2 <- predict(cp_time, type = "response") # incorporates individ rand slope

head(pred1)
head(pred2)
head(full_udata$stdsg_CP)
head(tar$cp_tar)


tar_test <- glmer(cp_tar ~ hrs_midnight + (hrs_midnight|subj), family = Gamma("log"), data = tar)
summary(tar_test) # no time effect now.

# save cp time effects ----- 
#to create time adjusted residual in 1b merge
save(cp_time, cp_time_intercept, cp_time_slope, cp_time_pred,
     file = "data/cp-time-of-day-model.Rdata")

