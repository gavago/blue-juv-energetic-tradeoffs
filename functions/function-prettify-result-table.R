

prettify_table <- function(x) {
  
  dup_resp <- which(duplicated(x[,"Response"]))
  
  x[dup_resp, c("Response")] <- ""  
  
  return(x)
}
