library(tidyverse)
library(lmerTest)

load("data/urine_sample_dataset_juv_immune_energetics.Rdata", verbose = T)
load("data/fgc_data_by_sample.Rdata", verbose = T)

as.numeric(full_udata$time)
max(gc_raw$time)
str(gc_raw)
#full_udata$time
#full_data$time <- unclass(as.POSIXct(full_udata$time))

full_udata$hrs_midnight <- format(as.POSIXct(full_udata$time), format = "%H:%M:%S") %>%
lubridate::hms(.) %>%
as.numeric()/60/60 #as seconds, then divide into minutes and hours.


full_udatax <- full_udata %>%
  mutate(time = strptime(time, format = "%H:%M"))

# time effects -----
neo_time <- glmer(neo_sg ~ hrs_midnight + (hrs_midnight|subj), family = Gamma("log"), data = full_udata)
summary(neo_time)
cp_time <- glmer(stdsg_CP ~ hrs_midnight + (hrs_midnight|subj), family = Gamma("log"), data = full_udata)
summary(cp_time)

# viz time effects ----- 
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

# 


intercept <- colMeans(coef(cp_time)$subj)[1]
slope <- colMeans(coef(cp_time)$subj)[2]
pred1 <- exp(intercept + slope * full_udata$hrs_midnight)

tar <- full_udata %>%
  mutate(cp_tar1 = stdsg_CP - pred1) %>%
  mutate(cp_tar = cp_tar1 + abs(min(cp_tar1, na.rm = T)) + 0.0001)

# compare to other predicted values ------

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

# save cp time effects model ----- 
#to create time adjusted residual in 1b merge
save(cp_time, file = "data/cp-time-of-day-model.Rdata")

