#theta_val a named vector or list of theta values
get_t_val <- function(theta_val){
  unlist(theta_val)
}

# theta_prec a named vector or list of theta precision
# type is precision set as SE, RSE or CI95
# theta_val a named vector or list of theta values (optional for SE)
get_t_var <- function(theta_prec, type, theta_val = NULL){
  if(!type %in% c("RSE", "SE", "CI95", "CI90")) stop("Type of precision on theta value is not supported")

  if(type == "RSE"){
    if(is.null(theta_val)) stop("Please provide theta values to compute variance")
    v <- (unlist(theta_prec) * get_t_val(theta_val))^2
  }
  if(type == "SE"){
    v <- (unlist(theta_prec))^2
  }
  if(type %in% c("CI95", "CI90")){
    if(!all((theta_prec %>% map(length) %>% as.double) == 2)) stop("Please provide the 2 bounds of the confidence interval for every param.")
    gap <- (as.double(map(theta_prec, 2)) - as.double(map(theta_prec, 1)))/2

    z <- switch(type,
                "CI95" = 1.96,
                "CI90" = 1.645
    )
    v <- (unlist(gap) / z)^2
  }
  return(v)
}

get_o_val <- function(omega_val, omega_type){
  if(!omega_type %in% c("VAR", "OMEGA2", "SE", "CV", "logN_CV")) stop("Do you report interindividual variability as omega2, se, or CV% ?")
  if(omega_type %in% c("VAR", "OMEGA2")){
    v <- unlist(omega_val)
  }

  if(omega_type %in% c("SE", "CV")){
    v <- (unlist(omega_val)) ^ 2
  }

  if(omega_type %in% "logN_CV"){
    v <- log(1+ unlist(omega_val)^2)
  }

  return(v)
}


get_o_var <- function(omega_prec, type, omega_val = NULL, omega_type = NULL){
  if(omega_type %in% c("VAR", "OMEGA2") & type == "CV"){
    se <- unlist(omega_prec) * get_o_val(omega_val, omega_type)
  }
  if(omega_type %in% c("VAR", "OMEGA2") & type == "SE"){
    se <- unlist(omega_prec)
  }
  if(omega_type %in% c("CV", "SE", "logN_CV") & type == "CV"){
    se <- 2*unlist(omega_prec) * get_o_val(omega_val, omega_type)
  }
  if(omega_type %in% c("VAR", "OMEGA2") & type %in% c("CI95", 'CI90')){

    if(!all((omega_prec %>% map(length) %>% as.double) == 2)) stop("Please provide the 2 bounds of the confidence interval for every param.")
    gap <- (as.double(map(omega_prec, 2)) - as.double(map(omega_prec, 1)))/2

    z <- switch(type,
                "CI95" = 1.96,
                "CI90" = 1.645
    )
    se <- unlist(gap) / z
  }
  if(!exists('se')) stop("Please mind the way you entered precison on OMEGA.")

  v <- se^2
  return(v)
}






