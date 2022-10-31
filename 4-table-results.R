
source("functions/function-mod-info-extract.R")
load("models/energetic-costs-immune.Rdata", verbose = T)
load("models/short-energetic-costs-immune.Rdata", verbose = T)
load("models/energetic-constraints-immune.Rdata", verbose = T)
load("models/mechanism-constraints-on-immune.Rdata", verbose = T)

# A1 - energetic costs neo
mod_info_extract(cp_neo_glm_month)
mod_info_extract(lbm_neo_lm_month)

# A1 - mod behavior in light of costs? 
mod_info_extract(f_neo_lm_month)
mod_info_extract(r_neo_lm_month)
mod_info_extract(m_neo_lm_month)

# A1 - energetic costs neo short term
mod_info_extract(change_lbm_neo_lm_sample)

# A2 - energetic limitations neo
mod_info_extract(neo_cp_glm_month)

# A2 - mechanism constraints




