source("functions/function - glmm extract.R")

glmm_table <- function(data, type = c("table", "plot")){

  if(type == "table"){
    extract_function <- glmm_extract
  }  
  if(type == "plot"){
    extract_function <- glmm_ext_coefplot
  }  
  
  beh_order <- c("Aggression received", "Aggression given", "Groomed by", "Rest social", "Groom", "Play", "Contact")
  
  table <- lapply(data, extract_function) %>%
    do.call("rbind",.) %>%
    rownames_to_column() %>%
    rename(model = rowname, predictor = `X[[i]]_rowname`, Beta = `X[[i]]_Estimate`,
           p = `X[[i]]_Pr(>|z|)`) %>%
    mutate(sig = ifelse(p < 0.05, "*", "")) %>%
    mutate(predictor = case_when(
      predictor == "(Intercept)" ~ "Intercept",
      predictor == "z.(age_diff)" ~ "Age difference",
      predictor == "z.(rank_diff)" ~ "Rank difference",
      predictor == "kin0.125" ~ "Distant kin",
      predictor == "kin0.25" ~ "Close kin",
      predictor == "same_sexTRUE" ~ "Same sex",
      predictor == "z.(prod.ripe.mid)" ~ "Fruit availability"
    )) %>%
    mutate(model = case_when(
      grepl("mod_yn_agg_r_f.", model) ~ "Female Aggression received",
      grepl("mod_yn_agg_r_m.", model) ~ "Male Aggression received",
      grepl("mod_yn_gmd_f.", model) ~ "Female Groomed by",
      grepl("mod_yn_gmd_m.", model) ~ "Male Groomed by",
      grepl("mod_yn_gm_f.", model) ~ "Female Groom",
      grepl("mod_yn_gm_m.", model) ~ "Male Groom",
      grepl("mod_yn_agg_g_f.", model) ~ "Female Aggression given",
      grepl("mod_yn_agg_g_m.", model) ~ "Male Aggression given",
      grepl("mod_yn_c_f.", model) ~ "Female Contact",
      grepl("mod_yn_c_m.", model) ~ "Male Contact",
      grepl("mod_yn_r_f.", model) ~ "Female Rest social",
      grepl("mod_yn_r_m.", model) ~ "Male Rest social",
      grepl("mod_yn_pl_f.", model) ~ "Female Play",
      grepl("mod_yn_pl_m.", model) ~ "Male Play",
      TRUE ~ model
    )) %>%
    separate(model, into = c("Sex", "Behavior"), sep = "\\s", extra = "merge") %>%
    mutate(Behavior = factor(Behavior, levels = beh_order)) %>%
    arrange(Behavior)

  if(type == "table"){
    
    table1 <- table %>%
      mutate(Behavior = as.character(Behavior)) %>%
      rename(CI = `X[[i]]_CI`)
    table1[duplicated(table1[,c("Sex","Behavior")]),c("Sex","Behavior")] <- c("","")
    
    return(table1)
  }  
  
  if(type == "plot"){
   
    table2 <-  table  %>%
     filter(predictor != "Intercept") %>%
     rename(CI_hi = `X[[i]]_CI_hi`, CI_lo = `X[[i]]_CI_lo`)
   
    return(table2)
  }  
  
  
  }
  