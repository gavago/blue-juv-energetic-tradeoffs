mod_info_extract <- function(mod){
  mod_name <- deparse(substitute(mod))
  if(grepl("^neo", mod_name)) {response <- "Neopterin"}
  if(grepl("^cp", mod_name)) {response <- "C-peptide"}
  if(grepl("^cp", mod_name)) {response <- "C-peptide"}
  if(grepl("^r", mod_name)) {response <- "Time resting"}
  if(grepl("^f", mod_name)) {response <- "Time feeding"}
  if(grepl("^m", mod_name)) {response <- "Time moving"}
  if(grepl("^change.*sample$", mod_name)) {response <- "∆ ELBM sample"}
  if(grepl("^change.*month$", mod_name)) {response <- "∆ ELBM month"}
  
  if(grepl("_lm_", mod_name)) {mod_type <- "lmer"}
  if(grepl("_glm_", mod_name)) {mod_type <- "glmer"}

  if(mod_type == "lmer"){
    tab1 <- coef(summary(mod)) %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      mutate(Estimate = round(Estimate, 3)) %>%
      mutate(SE = round(`Std. Error`, 3)) %>%
      mutate(p_value = round(`Pr(>|t|)`, 3)) %>%
      mutate(CI_hi = round(Estimate + 1.96*`Std. Error`, 2), CI_lo = round(Estimate - 1.96*`Std. Error`, 2), CI = paste0("[", CI_lo, ",", CI_hi, "]"))
  }
  
  if(mod_type == "glmer"){
    tab1 <- coef(summary(mod)) %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      mutate(Estimate = round(Estimate, 3)) %>%
      mutate(SE = round(`Std. Error`, 3)) %>%
      mutate(p_value = round(`Pr(>|z|)`, 3)) %>%
      mutate(CI_hi = round(Estimate + 1.96*`Std. Error`, 2), CI_lo = round(Estimate - 1.96*`Std. Error`, 2), CI = paste0("[", CI_lo, ",", CI_hi, "]"))
  }
    
    df <- tab1 %>%
      mutate(Predictor = case_when(
        rowname == "(Intercept)" ~ "Intercept", 
        rowname == "age" ~ "Age", 
        rowname == "sexM" ~ "Sex",
        rowname == "mrank" ~ "Maternal Rank",
        rowname == "avg_rain" ~ "Monthly rainfall",
        rowname == "n_partners" ~ "N social partners",
        rowname == "log2_avg_cp_tar" ~ "log2 C-peptide", 
        rowname == "log2_avg_neo" ~ "log2 Neopterin",
        rowname == "log2_neo" ~ "log2 Neopterin",
        rowname == "log2_cp_tar" ~ "log2 C-peptide",
        rowname == "avg_cr_resid" ~ "ELBM",
        rowname == "sample_interval" ~ "sample interval",
        rowname == "log2_neo:sample_interval" ~ "log2 Neo x sample interval",
        rowname == "log2_cp_tar:sample_interval" ~ "log2 CP x sample interval",
        TRUE ~ rowname
      )) 
    
    df2 <- data.frame(Response = response, df) %>%
      mutate(p_value= ifelse(p_value < 0.05, paste0(p_value, "*"),
                          p_value)) %>%
      dplyr::select(Response, Predictor, Estimate, SE, CI, p_value)
  
  return(df2)
}


glmm_ext_coefplot <- function(mod){
  mod_name <- deparse(substitute(mod))
  
  df <- coef(summary(mod)) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(Estimate = round(Estimate, 2)) %>%
    mutate(CI_hi = round(Estimate + 1.96*`Std. Error`, 2), CI_lo = round(Estimate - 1.96*`Std. Error`, 2)) %>%
    select(rowname, Estimate, CI_hi, CI_lo, `Pr(>|z|)`)
  
  colnames(df) <- paste(mod_name, colnames(df), sep = "_")
  
  return(df)
}

# #glmm_extract_CI_adjust1 <- function(mod, behavior){
#   mod_name <- deparse(substitute(mod))
#   if(grepl("_f", mod_name)) {sex <- "Female"}
#   if(grepl("_m", mod_name)) {sex <- "Male"}
#   if(grepl("_old", mod_name)) {age_class <- "Old"}
#   if(grepl("_yng", mod_name)) {age_class <- "Young"}
#   
#   tab1 <- coef(summary(mod)) %>%
#     as.data.frame() %>%
#     rownames_to_column() %>%
#     mutate(Estimate = round(Estimate, 2)) %>%
#     mutate(`Pr(>|z|)`= round(`Pr(>|z|)`, 3)) %>%
#     #mutate_if(is.numeric, round, 3) %>%
#     mutate(CI_hi = round(Estimate + 1.96*`Std. Error`, 2), CI_lo = round(Estimate - 1.96*`Std. Error`, 2))
#   
#   if(grepl("received|given", behavior)){
#     df <- tab1 %>%
#       mutate(Predictor = case_when(
#         rowname == "(Intercept)" ~ "Intercept", 
#         rowname == "age_diff" ~ "Age diff", 
#         rowname == "rank_diff" ~ "Rank diff", 
#         rowname == "kin.L" ~ "Kinship", 
#         rowname == "kin.Q" ~ "Kinship^2",
#         rowname == "same_sexTRUE" ~ "Same sex" 
#       )) %>%
#       select(Predictor, Estimate, CI_hi, CI_lo)
#   } else {
#     df <- tab1 %>%
#       mutate(Predictor = case_when(
#         rowname == "(Intercept)" ~ "Intercept", 
#         rowname == "age_diff" ~ "Age diff", 
#         rowname == "rank_diff" ~ "Rank diff", 
#         rowname == "kin.L" ~ "Kinship", 
#         rowname == "kin.Q" ~ "Kinship^2",
#         rowname == "same_sexTRUE" ~ "Same sex" 
#       )) %>%
#       select(Predictor, Estimate, CI_hi, CI_lo)
#   }
#   
#   if(exists("age_class")){
#     df2 <- data.frame(`Dyadic Behavior` = behavior, Sex = sex, Age = age_class,  df)
#   } else {
#     df2 <- data.frame(`Dyadic Behavior` = behavior, Sex = sex,  df)
#   }
#   
#   
#   return(df2)
# }

