#' @param theta_val a named vector or list of theta values
get_t_val <- function(theta_val){
  unlist(theta_val)
}

#' @param theta_prec a named vector or list of theta precision
#' @param type is precision set as SE, RSE or CI95
#' @param theta_val a named vector or list of theta values (optional for SE)
get_t_var <- function(theta_prec, type, theta_val = NULL){
  if(!type %in% c("RSE", "SE", "CI95", "CI90")) stop("Type of precision on theta value is not supported")
  if(type %in% c("RSE", "CI95", "CI90") & is.null(theta_val)) stop("Please provide theta values to compute variance")
  if(type == "RSE"){
    v <- (unlist(theta_prec) * get_t_val(theta_val))^2
  }
  if(type == "SE"){
    v <- (unlist(theta_prec))^2
  }
  if(type == "CI95"){
    v <- ((unlist(theta_prec) / 1.96) / get_t_val(theta_val))^2
  }
  if(type == "CI90"){
    v <- ((unlist(theta_prec) / 1.645) / get_t_val(theta_val))^2
  }
  return(v)
}

