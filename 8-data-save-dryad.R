load("data/full_data_month_udata_fgc_behav.RData", verbose = T)

load("data/full_data_short_term_lbm_change.Rdata", verbose = T)


names(full_data_month)
names(full_data_short_term_lbm_change)


full_data_month %>%
  select(subj, group, year, month, sex, age, mrank, log2_avg_neo, log2_avg_cp_tar, avg_cr_resid, avg_rain, n_partners, r, f, m)

full_data_short_term_lbm_change %>%
  select(subj, group, year, month, sex, age, mrank, log2_neo, log2_cp_tar, cr_resid, sample_interval)





# gyard -------
# select(-c( "avg_neo_sg",  "avg_cp_sg_tar", "avg_cp_sg_tar_viz",
#            "avg_cp_sg","med_neo_sg", "med_cr_resid", "med_cp_sg_tar", "avg_fgc","med_fgc",
#            "month_name", "pl", "gmd","gm","sl", "n_agg_g", "n_agg_r", "n_obs", "mrank",
#            "fai","fai_cat","month.name","mum", "sex","bday","weaning", "mid",
#            "n_contact", "n_rest", "n_grooming","n_play", "avg_f_part")) %>%