library(tidyverse)

load("data/neo-dataset-full.Rdata", verbose = T)
load("data/cp-dataset-full.Rdata", verbose = T)
load("data/behav-dataset-month.Rdata", verbose = T)
load("data/fgc_data_by_sample.Rdata", verbose = T)
load("data/cp-time-of-day-model.Rdata", verbose = T)


# Merging urine sample data (neo, cp, cr_resid)  -----
merged_udata <- left_join( neo_data_full, cp_raw, by = intersect(names(neo_data_full), names(cp_raw))) %>%
  select(group, subj, date, age, year, month, time, sample_number, stdsg_CP, neo_sg, everything())
dim(merged_udata) # 620 rows

# Add lean body mass and cp time adjust residual (script 1b) ------

# calculating cr-sg resids
merged_udata %>%
  ggplot(., aes(y = Cr, x = SG/1000, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm")


m <- merged_udata %>%
  lm(Cr ~ I(SG/1000) + I((SG/1000)^2), data = ., na.action = na.exclude)
resids <- as.numeric(resid(m))


# add vars
full_udata <- merged_udata %>%
  mutate(cr_resid = resids) %>%
  mutate(cp_tar1 = stdsg_CP - cp_time_pred) %>%
  mutate(cp_tar = cp_tar1 + abs(min(cp_tar1, na.rm = T)) + 0.0001) %>%
  select(-cp_tar1)

str(full_udata)

#save(full_udata, file = "data/urine_sample_dataset_juv_immune_energetics.Rdata")

# Summarize udata and fgc data by month ------

### summarize & save month udata -----

load("data/urine_sample_dataset_juv_immune_energetics.Rdata")
View(full_udata)
dim(full_udata)
str(full_udata)

udata_month_avg <- full_udata %>%
  group_by(subj, month, year) %>% 
  summarize(avg_neo_sg = mean(neo_sg, na.rm = T), 
            avg_cr_resid = mean(cr_resid, na.rm = T), 
            avg_cp_sg_tar = mean(cp_tar, na.rm = T),
            avg_cp_sg = mean(stdsg_CP, na.rm = T),
            med_neo_sg = median(neo_sg, na.rm = T), 
            med_cr_resid = median(cr_resid, na.rm = T), 
            med_cp_sg_tar = median(cp_tar, na.rm = T)) %>% 
  ungroup() 
dim(udata_month_avg) # 299 rows

#save(udata_month_avg, file = "data/udata_month_avg.Rdata")


### summarize & save fgcs ------

load("data/fgc_data_by_sample.Rdata", verbose = T)
view(gc_raw)
dim(gc_raw) # 627 rows

fgc_month_avg <- gc_raw %>% 
  group_by(subj, month, year) %>% 
  summarize(avg_fgc = mean(fgc.ng_g.feces, na.rm = T), med_fgc = median(fgc.ng_g.feces, na.rm = T)) %>% 
  ungroup()
dim(fgc_month_avg) 

#save(fgc_month_avg, file = "data/fgc_month_avg.Rdata")

view(fgc_month_avg)

# Merge monthly data - urine, feces, behavior, lh bday -----
load("data/udata_month_avg.Rdata", verbose = T)
load("data/fgc_month_avg.Rdata", verbose = T)
load("data/behav-dataset-month.Rdata", verbose = T)
load("/Users/nicolethompsongonzalez/Dropbox/2_R-projects/Juv-blues-diss/Juvenile data and field/Data/3. Behavior data by month/Rdata files month/Juv LH month.Rdata", verbose = T)
lh.mo_merge <- lh.mo %>%
  mutate(year = lubridate::year(month), month = lubridate::month(month)) 

names(behav_data_month)

udata_fgc_month_avg <- full_join(udata_month_avg, fgc_month_avg, by = intersect(names(udata_month_avg), names(fgc_month_avg)))
dim(udata_fgc_month_avg) # 317, 5 urine subj-months where no fgc data

str(udata_fgc_month_avg)

view(udata_gc_month_avg)

full_data_month <- full_join(udata_fgc_month_avg, behav_data_month, by = intersect(names(udata_fgc_month_avg), names(behav_data_month))) %>%
  left_join(., lh.mo_merge) %>%
  mutate(age = as.numeric(mid - bday)/365.25) %>%
  mutate(log2_avg_neo = log2(avg_neo_sg), log2_avg_cp_tar = log2(avg_cp_sg_tar))
dim(full_data_month) # 323


apply(full_data_month, 2, function(x) sum(is.na(x)))

view(full_data_month)

# save full month data -----
# save(full_data_month, file = "data/full_data_month_udata_fgc_behav.RData")
# load("data/full_data_month_udata_fgc_behav.RData", verbose = T)

# save csv
# write.csv(full_data_month,  file = "data/full_data_month_udata_fgc_behav.csv", row.names = F)


# Sample counts for methods ------
full_udata <- read_csv("data/urine_sample_dataset_juv_immune_energetics.csv")
load("data/fgc_data_by_sample.Rdata", verbose = T)

nrow(full_udata) #620
nrow(gc_raw) #627

full_udata %>%
  distinct(subj, .keep_all = T) %>%
  count(sex)


# graveyard -----
# save each Rdata dataset to CV ----
# write.csv(neo_data_full, file = "neo-dataset-full.csv", row.names = F)
# write.csv(cp_raw, file = "cp-dataset-full.csv", row.names = F)
# write.csv(behav_data_month, file = "behav-dataset-month.csv", row.names = F)


# check that time format is same
# x <- left_join(cp_raw, neo_data_full, by = "sample_number")
# x %>%
#   filter(time.x != time.y) %>%
#   select(starts_with("time"))

#check intersection names across all data frames
# intersect(names(neo_data_full), names(cp_raw)) %>%
#   intersect(names(behav_data_month))

# x <- neo_data_full %>% arrange(sample_number)
# y <- cp_raw %>% arrange(sample_number)
# all(x$time == y$time) # amazing, if cp_raw and neo aren't saved on the same day then they're times aren't the same


merged_data <- left_join( neo_data_full, cp_raw, by = intersect(names(neo_data_full), names(cp_raw))) %>%
  left_join(., behav_data_month) %>%
  select(group, subj, date, age, year, month, time, sample_number, stdsg_CP, neo_sg, everything())
nrow(merged_data) # 620

View(merged_data)

# check out how many values NA per variable, should def be zero for age and sex
apply(merged_data, 2, function(x) sum(is.na(x)))

#write.csv(merged_data,  file = "data/merged_data_no_lean_body_mass.csv", row.names = F)


