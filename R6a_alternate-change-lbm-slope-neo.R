library(tidyverse)


sample_data1 <- read.csv("data/full_dataset_juv_immune_energetics.csv", header = T)

# Evaluate slope change of subj cr_resid by their total neo

# create sample_day var -------
# group_by subject, create sample interval
# replace NAs in sample interval with 0 (first subject sample date)
# create sample day with cumulative sum of interval

sample_data <- sample_data1 %>%
  arrange(subj, date) %>%
  group_by(subj) %>%
  mutate(sample_interval = as.numeric(difftime(date, lag(date), units = "days"))) %>%
  mutate(sample_interval = case_when(
    is.na(sample_interval) ~ 0,
    TRUE ~ sample_interval)) %>% 
  mutate(sample_day = cumsum(sample_interval)) %>%
  ungroup()

# sample_data %>%
#   select(subj, date, sample_interval, sample_day) 


# function to extract lbm slope from cr_resid ~ sample day per subject ------ 
#   extracts coefficient of sample day from Cr_resid ~ sample day

# test set:
# data <- sample_data %>%
#   filter(subj == "allo")

lbm_slope_fun <- function(data){
  coefs <- lm(cr_resid ~ sample_day, data = data) %>% 
    coef(.)
  slope <- coefs[[2]]
  return(slope)
  }


# create lbm slope df for analysis ---------
# calc total_avg_neo (ng/ml) summarized df 
lbm_slope_df <- sample_data %>%
  group_by(subj, sex) %>%
  summarise(total_avg_neo = mean(neo_sg, na.rm = T),
            total_avg_cp = mean(stdsg_CP, na.rm = T),
            initial_age = first(age)) %>%
  ungroup()

# add lbm_slope
lbm_slope_df$lbm_slope <- sample_data %>%
  group_by(subj) %>%
  group_map(~ lbm_slope_fun(data = .x)) %>%
  unlist()

# regression -------

hist(log(lbm_slope_df$lbm_slope))

lm(log(lbm_slope) ~ total_avg_neo + sex + initial_age, data = lbm_slope_df) %>%
  summary()




