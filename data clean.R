library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)

# energetic condition moderate a relationship between play and immunity
# high condition, no relationship
# low condition, neg relationship
# play and cp, immunity and cp
# play immunity by cp


# NEO
#### -----
# 1A. TWN TWS ------
#these samples tested a second time on plate 11, to see if dilution in freezer left an effect
# mark them as test for filtering
test_samp <- c(7, 21, 35, 82, 87, 99, 140, 151, 165)

twn_twsx <- read.csv("Neo shared w Josh C/updated draft assign neo value.csv") %>%
  filter(!is.na(Sample_number)) %>%
  mutate(OG_Data_Set = as.numeric(gsub("A", "", OG_Data_Set))) %>%
  mutate(plate_number = OG_Data_Set + 6) %>%
  rename(sample_number = Sample_number, neo_value = NEO_value) %>%
  select(-OG_Data_Set) %>%
  mutate(note = case_when(
    sample_number %in% test_samp & plate_number == 11 ~ "Test to remove",
    TRUE ~ note
  ))
nrow(twn_twsx)

str(twn_twsx)
twn_twsx[!is.na(twn_twsx) & twn_twsx == "Range?"] <- NA

twn_twsx %<>%
  mutate(neo_value = as.numeric(neo_value),
         date_assayed = as.Date(date, format = "%m/%d/%y"),
         CV = as.numeric(CV)) %>%
  select(-date)

# --- examine 2x repeats ####
twns_dups <- twn_twsx %>%
  count(sample_number) %>%
  filter(n == 2) %>%
  pull(sample_number)

# any dups where both CVs too high?
high_twns_dups <- twn_twsx %>%
  filter(sample_number %in% twns_dups) %>%
  arrange(sample_number) %>%
  filter(CV > 20) %>%
  count(sample_number) %>%
  filter(n > 1) %>%
  pull(Sample_number)

twn_twsx %>%
  filter(sample_number %in% high_dups) %>%
  arrange(sample_number)
  
# --- examine 3x repeats ----
 # remove all w filter CV
                
twns_trips <- twn_twsx %>%
  count(sample_number) %>%
  filter(n == 3) %>%
  pull(sample_number)

# any where all CVs > 20   
high_twns_trips <- twn_twsx %>%
  filter(sample_number %in% twns_trips) %>%
  filter(CV > 20) %>%
  arrange(sample_number) %>%
  count(sample_number) %>%
  filter(n > 2) %>%
  pull(sample_number)

twn_twsx %>%
  filter(sample_number %in% high_twns_trips) %>%
  arrange(sample_number)

twn_twsx %>%
  mutate(final_conc =  dilution * NEO_value) %>%
  summarise(min = min(final_conc, na.rm = T) , max = max(final_conc, na.rm = T))

range(twn_twsx$NEO_value, na.rm = T)
# 1B. GN data ----
gnx <- read.csv("Neo shared w Josh C/Updated GN results.csv") %>%
  filter(!is.na(sample_number)) %>%
  select(- Taken_From, - notes.for.josh, - SD) %>%
  rename(dilution = neo_dilution, plate_number = plate, note = notes) %>%
  mutate(date_assayed = mdy(date_assayed))

# ---- examine 2x repeats -----
gn_dups <- gnx %>%
  count(sample_number) %>%
  filter(n > 1) %>%
  pull(sample_number)

hi_cv_dups <- gnx %>%
  filter(sample_number %in% gn_dups) %>%
  filter(CV > 20) %>%
  count(sample_number) %>%
  filter(n > 1) %>%
  pull(sample_number)

# 1C. clean up n bind -----

leftover_dups_twns <- twn_twsx %>%
  filter(CV < 21) %>%
  filter(!grepl("Test", note)) %>%
  count(sample_number) %>%
  filter(n > 1) %>%
  pull(sample_number)

twn_tws <- twn_twsx %>%
  filter(CV < 21) %>%
  filter(!grepl("Test", note)) %>% # remove samples that were tested for storage at dilution
  filter(!(sample_number %in% leftover_dups_twns & CV > 14)) %>%
  filter(!(sample_number == 307 & plate_number == 19)) %>% # redo where first was better
  arrange(sample_number, plate_number)
twns_samp_before <- unique(twn_twsx$sample_number)
twns_samp_after <- unique(twn_tws$sample_number) 
length(twns_samp_before) - length(twns_samp_after) # removed  21 samples
twns_samp_removed <- setdiff(twns_samp_before,twns_samp_after)
nrow(twn_tws) #376

gn <- gnx %>%
  filter(CV < 21) %>%
  filter(!grepl("poor dup", note) & !grepl("range", note)) %>%
  arrange(sample_number, plate_number)
gn_samp_before <- unique(gnx$sample_number)
gn_samp_after <- unique(gn$sample_number) 
length(gn_samp_before) - length(gn_samp_after) # removed  15 samples
gn_samp_removed <- setdiff(gn_samp_before, gn_samp_after)
nrow(gn) #185

neo_data_no_info <- rbind(gn, twn_tws)
nrow(neo_data_no_info) #561
names(neo_data_no_info)

# how many samples used assayed multiple times ----
twns_twice <- twn_tws %>%
  filter(sample_number %in% twns_dups) %>%
  nrow() 
twns_twice #57
twns_thrice <- twn_tws %>%
  filter(sample_number %in% twns_trips) %>%
  nrow() 
twns_thrice #13

# how many samples assayed twice (no trips)
gn_twice <- gn %>%
  filter(sample_number %in% gn_dups) %>%
  nrow() 
gn_twice #25

(gn_twice + twns_twice)/ nrow(neo_data_no_info) # 14.6% assayed twice
twns_thrice/ nrow(neo_data_no_info) # 2.3% assayed thrice


# --- save to data merge ----

#save(twn_tws, gn, neo_data_no_info, file = "cleaned neo values to merge.Rdata")

# 1D. merge sample info with neo data ------

load("cleaned neo values to merge.Rdata", verbose = T)
load("/Users/nicolethompsongonzalez/Dropbox/2. R projects/Juvenile blues (diss)/Juvenile data and field/Data/3. Behavior data by month/Rdata files month/Juv LH month.Rdata", verbose = T)

sample_info <- read_xlsx("Neo shared w Josh C/Neo values complete to fill in.xlsx") %>%
  select(group, subj, date, time, sample_number, Cr, SG) %>%
  mutate(SG = as.numeric(SG), Cr = as.numeric(Cr), 
         date = date(date)) %>%
  mutate(year = year(date), month = month(date)) %>%
  mutate(time_char = as.character(time)) %>%
  separate(time_char, into = c("xdate", "time"), sep = " ") %>%
  select(-xdate) %>%
  mutate(time = strptime(time, format = "%H:%M")) %>%
  distinct() # removes one duplicate line, sample 18 burn 

lh.mo_merge <- lh.mo %>%
  mutate(year = year(month), month = month(month)) 
head(lh.mo_merge)


neo_data_full <- 
  left_join(sample_info, lh.mo_merge, by = c("group", "subj", "year", "month")) %>%
  left_join(., neo_data_no_info) %>%
  mutate(SG_corr_fac = mean(SG, na.rm = T)/SG, neo_sg = neo_value*SG_corr_fac*dilution) %>%
  mutate(month = month(date), year = year(date), Cr = as.numeric(Cr)) %>%
  rename(neo_dilution = dilution, neo_date_assayed = date_assayed, neo_plate_number = plate_number, neo_note = note) %>%
    select(group, subj, sex, age, year, month, sample_number, date, time, everything())

View(neo_data_full)

# how many subj represened w different numbers of samples per month, 
# majority are at 2 samples
neo_data_full %>%
  filter(!is.na(neo_value)) %>% 
  count(subj, month, year) %>% # nrow() #300 subj months
  count(n) 

#save(neo_data_full, file = "neo dataset full.Rdata")

# see why 50 samples don't have neo value ----
anti_join(sample_info %>% filter(group != "twn" & group != "tws"), gn, by = "sample_number") %>% View()

anti_join(sample_info, neo_data_no_info, by = "sample_number") %>% 
  filter(!(sample_number %in% twns_samp_removed | sample_number %in% gn_samp_removed)) %>%
  View()




# 2A. Format CP data # ----

cp_raw1 <- read.csv("/Users/nicolethompsongonzalez/Dropbox/2. R projects/Juvenile blues (diss)/Juv analysis/Lab work/UCPs/Std_UCP.csv", stringsAsFactors = F)
cp_raw <- cp_raw1 %>%
  select(-X, -period) %>%
  mutate(date = mdy(date), Cr.date = mdy(Cr.date),
         month = month(date), year = year(date),
         pg.ml_ucp = as.numeric(pg.ml_ucp), SG = as.numeric(SG),
         time = strptime(time, format = "%H:%M")) %>%
  rename(cp_assay_date = run.date, cp_run = run, cp_dilution = dilution,
         sample_number = NUMBER, cp_batch = batch, cp_note = notes)
names(cp_raw)
nrow(cp_raw)

str(cp_raw)
# cp_raw1 %>%
#   mutate(pg.ml_ucp_num = as.numeric(pg.ml_ucp)) %>%
#   select(pg.ml_ucp, pg.ml_ucp_num) %>% 
#   filter(pg.ml_ucp == "-")

#going to check how stdsg CP  calculated, by comparing to newly calculated
# sg_std_check <- cp_raw %>%
#   #value is already calculated with dilution, think that was the case of what needed to be entered in wizard/gamma counter.
#   mutate(sg.corr.fac = mean(SG, na.rm = T)/SG) %>%
#   mutate(stdsg_CP1 = sg.corr.fac * pg.ml_ucp) %>%
#   select(SG, cp_dilution, pg.ml_ucp, stdsg_CP, stdsg_CP1, sg.corr.fac) 
# sg_std_check %>%
#   filter(ceiling(stdsg_CP) != ceiling(stdsg_CP1)) # round bc diff in small binary computation

nrow(cp_raw)
nrow(neo_data_full)
names(cp_raw)
names(neo_data_full)

#save(cp_raw, file = "cp dataset full.Rdata")


# 3a. Monthly rate of play -----
load("/Users/nicolethompsongonzalez/Dropbox/2. R projects/Juvenile blues (diss)/Rewrite chapter 3 2021/scripts of Juv social preferences for rewrite/data/juv monthly activity budgets.Rdata", verbose = T)
nrow(act_budget)
head(act_budget)
names(act_budget)
str(act_budget)

behav_data_month <- act_budget %>%
  rename(subj = ID1) %>%
  mutate(year = year(month), month = month(month)) %>%
  select(subj, month, year, month_name, pl, gmd, gm, r, n_agg_g, n_agg_r, n_obs)

#save(behav_data_month, file = "behav dataset month.Rdata")

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

jet_data <- left_join(cp_raw, neo_data_full, by = intersect(names(neo_data_full), names(cp_raw))) %>%
  left_join(., behav_data_month) %>%
  select(group, subj, date, age, year, month, time, sample_number, stdsg_CP, neo_sg, everything())
names(jet_data)


# avg monthly neo
neo_month <- neo_data_full %>%
  group_by(month, subj) %>%
  summarise(avg_neo_mo = mean(neo_sg, na.rm = T)) %>%
  ungroup()


#time of day effects model ------
library(lmerTest)
lmer(neo_sg ~ time + (1|subj), data = jet_data) %>% summary()
lmer(stdsg_CP ~ time + (1|subj), data = jet_data) %>% summary()
plot(jet_data$time, jet_data$neo_sg) # no time of day effects...
plot(jet_data$time, jet_data$stdsg_CP) 


lmer(neo_sg ~ scale(stdsg_CP) + (1|subj), data = jet_data) %>% summary()



