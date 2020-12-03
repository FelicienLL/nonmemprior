#t_val output of get_t_val
text_tp <- function(t_val){
  paste0("\n$THETAP; (Prior on THETA values)\n", paste0(t_val, " FIXED ;  ", seq_len(length(t_val)), " ", names(t_val), collapse = "\n"), "\n", collapse = "\n")
}

#t_var output of get_t_var
text_tpv <- function(t_var){
  ntvar <- ""
  if(!is.null(names(t_var))) ntvar <- names(t_var)
  mtx <- diag(t_var)
  df <- mutate(as.data.frame(mtx), across(everything(), as.character))
  txttvar <- replace(df, upper.tri(df),"") %>%
    mutate(fixed = ifelse(row_number()== 1, "FIXED", "")) %>%
    mutate(name = paste(";", row_number(), ntvar)) %>%
    unite(everything(), col = "tpv", sep = " ") %>%
    pull() %>%
    paste0(collapse = "\n")
  paste0("\n$THETAPV; (Variance-covariance of THETA)\n", txttvar, "\n")
}

text_op <- function(o_val){
  paste0("\n$OMEGAP; (Prior on OMEGA values)\n", o_val %>% paste0(" FIXED ;  ",names(o_val),collapse = "\n"), "\n")
}

text_opd <- function(o_var, o_val){
 df <- round(1+2*(o_val^2)/o_var)
 ndf <- ""
 if(!is.null(names(df))) ndf <- names(df)
 paste0( "\n$OMEGAPD; (Degree of freedom on OMEGA) \n", paste0(df, " FIXED;  ", ndf, collapse = "\n"), "\n")
}
