#' @param t_val output of get_t_val
text_tp <- function(t_val){
  paste0("\n$THETAP (Prior on THETA values)\n", paste0(t_val, " FIXED ;  ", seq_len(length(t_val)), " ", names(t_val), collapse = "\n"), "\n", collapse = "\n")
}

#' @param t_var output of get_t_var
text_tpv <- function(t_var){
  tvar <- get_t_var(c(KA = .1, CL = .2, V = .3), "SE")
  ntvar <- ""
  if(!is.null(names(tvar))) ntvar <- names(tvar)
  mtx <- diag(tvar)
  df <- mutate(as.data.frame(mtx), across(everything(), as.character))
  txttvar <- replace(df, upper.tri(df),"") %>%
    mutate(fixed = ifelse(row_number()== 1, "FIXED", "")) %>%
    mutate(name = paste(";", row_number(), ntvar)) %>%
    unite(everything(), col = "tpv", sep = " ") %>%
    pull() %>%
    paste0(collapse = "\n")
  paste0("\n$THETAPV (Variance-covariance of THETA)\n", txttvar, "\n")
}
