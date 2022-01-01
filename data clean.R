library(tidyverse)
library(magrittr)

# clean up TWN TWS ------
x <- read.csv("Neo shared w Josh C/updated draft assign neo value.csv") %>%
  filter(!is.na(Sample_number))
names(x)
str(x)
x[!is.na(x) & x == "Range?"] <- NA

head(x$date)
x %<>%
  mutate(NEO_value = as.numeric(NEO_value),
         CV = as.numeric(CV),
         date = as.Date(date, format = "%m/%d/%y"))
head(x)

# --- examine 2x repeats ####
dups <- x %>%
  count(Sample_number) %>%
  filter(n == 2) %>%
  pull(Sample_number)

# any dups where both CVs too high?
high_dups <- x %>%
  filter(Sample_number %in% dups) %>%
  arrange(Sample_number) %>%
  filter(CV > 20) %>%
  count(Sample_number) %>%
  filter(n > 1) %>%
  pull(Sample_number)

x %>%
  filter(Sample_number %in% high_dups) %>%
  arrange(Sample_number)
  
# --- examine 3x repeats ----
                        
trips <- x %>%
  count(Sample_number) %>%
  filter(n == 3) %>%
  pull(Sample_number)

# any where all CVs > 20   
high_trips <- x %>%
  filter(Sample_number %in% trips) %>%
  filter(CV > 20) %>%
  arrange(Sample_number) %>%
  count(Sample_number) %>%
  filter(n > 2) %>%
  pull(Sample_number)

x %>%
  filter(Sample_number %in% high_trips) %>%
  arrange(Sample_number)

x %>%
  mutate(final_conc =  dilution * NEO_value) %>%
  summarise(min = min(final_conc, na.rm = T) , max = max(final_conc, na.rm = T))

range(x$NEO_value, na.rm = T)

# clean up GN data ----

