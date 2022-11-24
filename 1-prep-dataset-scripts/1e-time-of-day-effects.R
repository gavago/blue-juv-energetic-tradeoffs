library(tidyverse)

full_udata <- read_csv("data/urine_sample_dataset_juv_immune_energetics.csv")
load("data/fgc_data_by_sample.Rdata", verbose = T)


#full_udata$time
#full_data$time <- unclass(as.POSIXct(full_udata$time))

full_udatax <- full_udata %>%
  mutate(time = strptime(time, format = "%H:%M"))

lmer(neo_sg ~ time + (1|subj), data = full_data) %>% summary()
lmer(stdsg_CP ~ time + (1|subj), data = full_data) %>% summary()
plot(full_data$time, full_data$neo_sg) # no time of day effects...
plot(full_data$time, full_data$stdsg_CP) 