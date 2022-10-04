glmm_extract <- function(mod, behavior){
  mod_name <- deparse(substitute(mod))
  if(grepl("_f", mod_name)) {sex <- "Female"}
  if(grepl("_m", mod_name)) {sex <- "Male"}
  if(grepl("_old", mod_name)) {age_class <- "Old"}
  if(grepl("_yng", mod_name)) {age_class <- "Young"}
  
  tab1 <- coef(summary(mod)) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(Estimate = round(Estimate, 2)) %>%
    mutate(`Pr(>|z|)`= round(`Pr(>|z|)`, 3)) %>%
    #mutate_if(is.numeric, round, 3) %>%
    mutate(CI_hi = round(Estimate + 1.96*`Std. Error`, 2), CI_lo = round(Estimate - 1.96*`Std. Error`, 2), CI = paste0("[", CI_lo, ",", CI_hi, "]"))
  
  if(grepl("received|given", behavior)){
    df <- tab1 %>%
      mutate(Predictor = case_when(
        rowname == "(Intercept)" ~ "Intercept", 
        rowname == "age_diff" ~ "Age diff", 
        rowname == "rank_diff" ~ "Rank diff", 
        rowname == "kin.L" ~ "Kinship", 
        rowname == "kin.Q" ~ "Kinship^2",
        rowname == "same_sexTRUE" ~ "Same sex" 
      )) %>%
      select(Predictor, Estimate, CI, `Pr(>|z|)`)
  } else {
      df <- tab1 %>%
        mutate(Predictor = case_when(
          rowname == "(Intercept)" ~ "Intercept", 
          rowname == "age_diff" ~ "Age diff", 
          rowname == "rank_diff" ~ "Rank diff", 
          rowname == "kin.L" ~ "Kinship", 
          rowname == "kin.Q" ~ "Kinship^2",
          rowname == "same_sexTRUE" ~ "Same sex" 
        )) %>%
        select(Predictor, Estimate, CI, `Pr(>|z|)`)
    }
    
  if(exists("age_class")){
    df2 <- data.frame(`Dyadic Behavior` = behavior, Sex = sex, Age = age_class,  df) %>%
      mutate(p.value = ifelse(`Pr...z..` < 0.05, paste0(`Pr...z..`, "*"), `Pr...z..`)) %>%
      select(-Pr...z..)
    } else {
    df2 <- data.frame(`Dyadic Behavior` = behavior, Sex = sex,  df) %>%
      mutate(p.value = ifelse(`Pr...z..` < 0.05, paste0(`Pr...z..`, "*"), `Pr...z..`)) %>%
      select(-Pr...z..)
  }

    
  return(df2)
}


glmm_extract_bonf <- function(mod, behavior){
  mod_name <- deparse(substitute(mod))
  if(grepl("_f", mod_name)) {sex <- "Female"}
  if(grepl("_m", mod_name)) {sex <- "Male"}
  if(grepl("_old", mod_name)) {age_class <- "Old"}
  if(grepl("_yng", mod_name)) {age_class <- "Young"}
  
  tab1 <- coef(summary(mod)) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(Estimate = round(Estimate, 2)) %>%
    mutate(`Pr(>|z|)`= round(`Pr(>|z|)`, 3)) %>%
    #mutate_if(is.numeric, round, 3) %>%
    mutate(CI_hi = round(Estimate + 1.96*`Std. Error`, 2), CI_lo = round(Estimate - 1.96*`Std. Error`, 2), CI = paste0("[", CI_lo, ",", CI_hi, "]"))
  
  if(grepl("received|given", behavior)){
    df <- tab1 %>%
      mutate(Predictor = case_when(
        rowname == "(Intercept)" ~ "Intercept", 
        rowname == "age_diff" ~ "Age diff", 
        rowname == "rank_diff" ~ "Rank diff", 
        rowname == "kin.L" ~ "Kinship", 
        rowname == "kin.Q" ~ "Kinship^2",
        rowname == "same_sexTRUE" ~ "Same sex" 
      )) %>%
      select(Predictor, Estimate, CI, `Pr(>|z|)`)
  } else {
    df <- tab1 %>%
      mutate(Predictor = case_when(
        rowname == "(Intercept)" ~ "Intercept", 
        rowname == "age_diff" ~ "Age diff", 
        rowname == "rank_diff" ~ "Rank diff", 
        rowname == "kin.L" ~ "Kinship", 
        rowname == "kin.Q" ~ "Kinship^2",
        rowname == "same_sexTRUE" ~ "Same sex" 
      )) %>%
      select(Predictor, Estimate, CI, `Pr(>|z|)`)
  }
  
  if(exists("age_class")){
    df2 <- data.frame(`Dyadic Behavior` = behavior, Sex = sex, Age = age_class,  df) %>%
      mutate(p.value = ifelse(`Pr...z..` < 0.05/6, paste0(`Pr...z..`, "*"), `Pr...z..`)) %>%
      select(-Pr...z..) # 3 hypos agg r, pl, gm x 2 male female (not x 2 old young)
  } else {
    df2 <- data.frame(`Dyadic Behavior` = behavior, Sex = sex,  df) %>%
      mutate(p.value = ifelse(`Pr...z..` < 0.05/6, paste0(`Pr...z..`, "*"), `Pr...z..`)) %>%
      select(-Pr...z..)
  }
  
  
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

print("functions: glmm_extract, glmm_extract_bonf, & glmm_ext_coefplot")

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

