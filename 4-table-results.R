library(tidyverse)
library(mediation)
library(broom)
source("functions/function-mod-info-extract.R")
source("functions/function-prettify-result-table.R")
load("models/energetic-costs-immune-broad.Rdata", verbose = T)
load("models/energetic-costs-immune-short.Rdata", verbose = T)
load("models/energetic-constraints-immune-broad.Rdata", verbose = T)
load("models/mechanism-constraints-on-immune.Rdata", verbose = T)


# A1 - energetic costs neo -----
costs1 <- mod_info_extract(cp_neo_lm_month) %>% prettify_table()
costs2 <- mod_info_extract(change_lbm_neo_lm_month) %>% prettify_table()

summary(cp_neo_lm_month)
summary(change_lbm_neo_lm_month)


# A1 - energetic costs neo short term -----
costs3 <- mod_info_extract(change_lbm_neo_lm_sample) %>% prettify_table()

summary(change_lbm_neo_lm_sample)

# A1 - compensatory behavior in light of costs? (Supplementary)  -----
sup1 <- mod_info_extract(f_neo_lm_month) %>% prettify_table()
sup2 <- mod_info_extract(r_neo_lm_month) %>% prettify_table()
sup3 <- mod_info_extract(m_neo_lm_month) %>% prettify_table()
summary(f_neo_lm_month)
summary(r_neo_lm_month)
summary(m_neo_lm_month)


# A2 - energetic limitations neo -----
constraints <- mod_info_extract(neo_cp_cr_lm_month) %>% prettify_table()
summary(neo_cp_cr_lm_month)

# A2 - mechanism of constraint ----
set.seed(288)
med_results = mediate(fit.mediator, fit.dv, treat='log2_avg_cp_tar', mediator='log2(avg_fgc)')
med_summ<- summary(med_results)

#tidy doesn't work with mer / lmm


# save tabs ---------
tab1_costs <- do.call("rbind", list(costs1, costs2, costs3))
#write.table(tab1_costs, file = "results/tables/tab1-costs.txt", quote = FALSE, sep = "/", row.names = F)

compensatory_tabs <- do.call("rbind", list(sup1, sup2, sup3))
#write.table(compensatory_tabs, file = "results/tables/supp_tables.txt", quote = FALSE, sep = "/", row.names = F)

#write.table(constraints, file = "results/tables/tab2-constraint_table.txt", quote = FALSE, sep = "/", row.names = F)


