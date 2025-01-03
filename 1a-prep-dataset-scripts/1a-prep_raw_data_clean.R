library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)

# energetic condition moderate a relationship between play and immunity
# high condition, no relationship
# low condition, neg relationship
# play and cp, immunity and cp
# play immunity by cp


# 1.NEO
#### -----
# 1A. TWN TWS ------
#these samples tested a second time on plate 11, to see if dilution in freezer left an effect
# mark them as test for filtering
test_samp <- c(7, 21, 35, 82, 87, 99, 140, 151, 165)

twn_twsx <- read.csv("Neo_shared_w_Josh_C/updated draft assign neo value.csv") %>%
  filter(!is.na(Sample_number)) %>%
  mutate(OG_Data_Set = as.numeric(gsub("A", "", OG_Data_Set))) %>%
  mutate(plate_number = OG_Data_Set + 6) %>%
  rename(sample_number = Sample_number, neo_value = NEO_value) %>%
  select(-OG_Data_Set) %>%
  mutate(note = case_when(
    sample_number %in% test_samp & plate_number == 11 ~ "Test to remove",
    TRUE ~ note
  ))
nrow(twn_twsx) #498


#values out of range are NA
twn_twsx[!is.na(twn_twsx) & twn_twsx == "Range?"] <- NA

twn_twsx %<>%
  mutate(neo_value = as.numeric(neo_value),
         date_assayed = as.Date(date, format = "%m/%d/%y"),
         CV = as.numeric(CV)) %>%
  select(-date)
nrow(twn_twsx) #498

# 1B. GN data ----
gnx <- read.csv("Neo_shared_w_Josh_C/Updated GN results.csv") %>%
  filter(!is.na(sample_number)) %>%
  select(- Taken_From, - notes.for.josh, - SD) %>%
  rename(dilution = neo_dilution, plate_number = plate, note = notes) %>%
  mutate(date_assayed = mdy(date_assayed))

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


# --- save to data merge ----

#save(twn_tws, gn, neo_data_no_info, file = "data/cleaned-neo-values-to-merge.Rdata")

# 1D. merge sample info with neo data ------

load("data/cleaned-neo-values-to-merge.Rdata", verbose = T)
load("/Users/nicolethompsongonzalez/Dropbox/2_R-projects/Juv-blues-diss/Juvenile data and field/Data/3. Behavior data by month/Rdata files month/Juv LH month.Rdata", verbose = T)
lh.mo_merge <- lh.mo %>%
  mutate(year = lubridate::year(month), month = lubridate::month(month)) 

apply(neo_data_no_info, 2, function(x) sum(is.na(x)))
apply(lh.mo_merge, 2, function(x) sum(is.na(x)))

# only use sample info from template - not any miscellaneous values.
sample_info <- read_xlsx("data/Neo sample info template.xlsx") %>%
  select(group, subj, date, time, sample_number, Cr, SG) %>%
  mutate(SG = as.numeric(SG), Cr = as.numeric(Cr), 
         date = lubridate::date(date)) %>%
  mutate(year = lubridate::year(date), month = lubridate::month(date)) %>%
  mutate(time_char = as.character(time)) %>%
  separate(time_char, into = c("xdate", "time"), sep = " ") %>%
  select(-xdate) %>%
  mutate(time = strptime(time, format = "%H:%M")) %>%
  distinct() # removes one duplicate line, sample 18 burn 
#NAs introduced by coercion is ok

neo_data_full <- 
  left_join(sample_info, lh.mo_merge, by = c("group", "subj", "year", "month")) %>%
  left_join(., neo_data_no_info) %>%
  mutate(SG_corr_fac = mean(SG, na.rm = T)/SG, neo_sg = neo_value*SG_corr_fac*dilution) %>%
  mutate(month = lubridate::month(date), year = lubridate::year(date), Cr = as.numeric(Cr)) %>%
  rename(neo_dilution = dilution, neo_date_assayed = date_assayed, neo_plate_number = plate_number, neo_note = note) %>%
    select(group, subj, sex, age, year, month, sample_number, date, time, everything()) %>%
  rename(neo_CV = CV)
nrow(neo_data_full) #620
names(neo_data_full)

#save(neo_data_full, file = "data/neo-dataset-full.Rdata")

# -- see why 50 samples don't have neo value ----
anti_join(sample_info %>% filter(group != "twn" & group != "tws"), gn, by = "sample_number") %>% View()

anti_join(sample_info, neo_data_no_info, by = "sample_number") %>% 
  filter(!(sample_number %in% twns_samp_removed | sample_number %in% gn_samp_removed)) %>%
  View()




# -- how many subj represented w different numbers of samples per month ----
# majority are at 2 samples
neo_data_full %>%
  filter(!is.na(neo_value)) %>% 
  count(subj, month, year) %>% # nrow() #300 subj months
  count(n) 
# 2. CP ----

cp_raw1 <- read.csv("/Users/nicolethompsongonzalez/Dropbox/2_R projects/Juvenile blues diss/Juv analysis/Lab work/UCPs/Std_UCP.csv", stringsAsFactors = F)

cp_raw <- cp_raw1 %>%
  select(-X, -period) %>%
  mutate(date = mdy(date), Cr.date = mdy(Cr.date),
         month = month(date), year = year(date),
         pg.ml_ucp = as.numeric(pg.ml_ucp), SG = as.numeric(SG),
         time = strptime(time, format = "%H:%M")) %>%
  rename(cp_assay_date = run.date, cp_run = run, cp_dilution = dilution,
         sample_number = NUMBER, cp_batch = batch, cp_note = notes)
names(cp_raw)
nrow(cp_raw)#620

str(cp_raw)

#checking how stdsg CP  calculated, by comparing to newly calculated
# sg_std_check <- cp_raw %>%
#   #value is already calculated with dilution, think that was the case of what needed to be entered in wizard/gamma counter.
#   mutate(sg.corr.fac = mean(SG, na.rm = T)/SG) %>%
#   mutate(stdsg_CP1 = sg.corr.fac * pg.ml_ucp) %>%
#   select(SG, cp_dilution, pg.ml_ucp, stdsg_CP, stdsg_CP1, sg.corr.fac) 
# sg_std_check %>%
#   filter(ceiling(stdsg_CP) != ceiling(stdsg_CP1)) # round bc diff in small binary computation
# calculated correctly

nrow(cp_raw)
nrow(neo_data_full)
names(cp_raw)
names(neo_data_full)

# check that cp and neo data have same data for SG and Cr
cp_sample_sgcr <- cp_raw %>% select(sample_number, SG, Cr)
neo_sample_sgcr <- neo_data_full %>% select(sample_number, SG, Cr)
left_join(cp_sample_sgcr, neo_sample_sgcr, by = "sample_number") %>%
  filter(SG.x != SG.y | Cr.x != Cr.y)


#save(cp_raw, file = "data/cp-dataset-full.Rdata")


# 3. BEHAVIOR - activity -----
load("/Users/nicolethompsongonzalez/Dropbox/2_R-projects/Juv-blues-diss/Rewrite chapter 3 2021/scripts of Juv social preferences for rewrite/data/juv monthly activity budgets.Rdata", verbose = T)
nrow(act_budget) #323
head(act_budget)
names(act_budget)
str(act_budget)

behav_data_month <- act_budget %>%
  rename(subj = ID1, mrank = rank_ID1, fai = prod.ripe.mid) %>%
  mutate(year = lubridate::year(month), month = lubridate::month(month)) %>%
  select(subj, month, year, month_name, pl, gmd, gm, r, f, m, sl, n_agg_g, n_agg_r, n_obs, mrank, group, fai, fai_cat, avg_rain)

#save(behav_data_month, file = "data/behav-dataset-month.Rdata")

# 4. BEHAVIOR - partner number -----
psp <- read_csv("/Users/nicolethompsongonzalez/Dropbox/2_R-projects/Juv-blues-diss/Rewrite chapter 3 2021/scripts of Juv social preferences for rewrite/data/3. Behavior data by month/all month PS for partners.csv") %>%
  mutate(month = lubridate::mdy(month))

# fix: in psp there are two rows where focal is NA
rows <- psp %>%
 filter(is.na(Focal)) %>%
  pull(X)
#see focals
# psp %>%
#   filter(X %in% (rows[1]-5):(rows[1]+5) | X %in% (rows[2]-5):(rows[2]+5)) %>%
#   View()
psp[rows[1],"Focal"] <- "stre"


# feeding partners
avg_f_partners <- psp %>%
  filter(!is.na(Focal)) %>% #one line Focal empty
  group_by(month, Focal) %>%
  summarise(avg_f_part = mean(X.7m, na.rm = T)) %>%
  ungroup() %>%
  mutate(month = lubridate::month(month))

# distinct grooming, play, contact, and resting partners total

total_partners <- psp %>%
  select(ag, se, everything()) %>% # move ag & sexual partners out of the way
  pivot_longer(cols = gm.gmd:X1m, names_to = "partner_type", values_to = "partner") %>%
  select(month, Focal, partner_type, partner) %>%
  filter(!is.na(Focal)) %>%
  group_by(month, Focal) %>%
  summarise(n_partners = n_distinct(partner, na.rm = T)) %>%
  ungroup() %>%
  mutate(month = lubridate::month(month))

beh_partners <- psp %>%
  select(ag, se, everything()) %>%
  pivot_longer(cols = gm.gmd:X1m, names_to = "partner_type", values_to = "partner") %>%
  select(month, Focal, partner_type, partner) %>%
  filter(!is.na(Focal)) %>% 
  group_by(month, Focal, partner_type) %>%
  summarise(n = n_distinct(partner, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = partner_type, values_from = n) %>%
  rename(n_contact = X0.m, n_rest = X1m, n_grooming = gm.gmd, n_play = pl) %>%
  mutate(month = lubridate::month(month))

dim(avg_f_partners) #323
dim(total_partners) # 323
dim(beh_partners) #323


# #check typos
# head(psp)
# sort(unique(psp$gm.gmd))
# sort(unique(psp$pl))
# sort(unique(psp$X0.m))
# sort(unique(psp$X1m))


partner_df <- left_join(avg_f_partners, total_partners) %>%
  left_join(., beh_partners) %>%
  rename(subj = Focal)

#save(partner_df, avg_f_partners, total_partners, beh_partners, file = "data/monthly-partners-data.Rdata")

# 5. fGCs -------
gc_raw <- read_csv("/Users/nicolethompsongonzalez/Dropbox/2_R projects/Juvenile blues diss/Juv analysis/Lab work/DPZ - fgcs/Final GC concentrations per sample.csv") %>%
  select(-type) %>%
  mutate(date = mdy(date),
         month = month(date), year = year(date))

#save(gc_raw, file = "data/fgc_data_by_sample.Rdata")

# exploration ----
# 
# neo_data_no_info_unfiltered
rbind(gnx, twn_twsx) %>%
  count(sample_number) %>%
  filter(n > 1)

# --- examine twn tws 2x neo repeats ####
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
  pull(sample_number)

twn_twsx %>%
  filter(sample_number %in% high_twns_dups) %>%
  arrange(sample_number)

# --- examine twn tws 3x neo repeats ----
# remove all later w filter CV

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

names(twn_twsx)
twn_twsx %>%
  mutate(final_conc =  dilution * neo_value) %>%
  summarise(min = min(final_conc, na.rm = T) , max = max(final_conc, na.rm = T))

range(twn_twsx$NEO_value, na.rm = T)

# --- examine gn 2x neo repeats -----
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

# --- for methods - how many samples used were assayed for neo multiple times ----
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
