library(tidyverse)
source("functions/function-mod-info-extract.R")
source("functions/function-prettify-result-table.R")
load("models/energetic-costs-immune-broad.Rdata", verbose = T)
load("models/energetic-costs-immune-short.Rdata", verbose = T)
load("models/energetic-constraints-immune-broad.Rdata", verbose = T)
load("models/mechanism-constraints-on-immune.Rdata", verbose = T)


# A1 - energetic costs neo
costs1 <- mod_info_extract(cp_neo_lm_month) %>% prettify_table()
costs2 <- mod_info_extract(lbm_neo_lm_month) %>% prettify_table()

# A1 - energetic costs neo short term
costs3 <- mod_info_extract(change_lbm_neo_lm_sample) %>% prettify_table()

# A1 - modify behavior in light of costs? (Supplementary) 
sup1 <- mod_info_extract(f_neo_lm_month) %>% prettify_table()
sup2 <- mod_info_extract(r_neo_lm_month) %>% prettify_table()
sup3 <- mod_info_extract(m_neo_lm_month) %>% prettify_table()

# A2 - energetic limitations neo
constraints <- mod_info_extract(neo_cp_cr_lm_month) %>% prettify_table()

# A2 - mechanism constraints
med_results


tab1_costs <- do.call("rbind", list(costs1, costs2, costs3))
write.table(tab1_costs, file = "results/tables/tab1-costs.txt", quote = FALSE, sep = "/", row.names = F)
