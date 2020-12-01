#' Render character vector to paste into NM control stream
#'
#' @param model_name Name and reference of the model
#' @param THETA_VALUE a named list of the values of theta
#' @param THETA_PRECISION a list of precision on theta
#' @param OMEGA_VALUE a named list of the values of omega
#' @param OMEGA_PRECISION a list of precision on theta
#' @param OMEGA2_or_CV are the omega value sd, variance or CV
#' @param PRECISION is precision reported as SE or RSE
#'
#' @return dummy result
#' @export
nmprior <- function(model_name,
                    THETA_VALUE,
                    THETA_PRECISION,
                    OMEGA_VALUE,
                    OMEGA_PRECISION,
                    OMEGA2_or_CV = "CV",
                    PRECISION = "RSE"){

  ### OMEGA CODE IS STILL "ROUGH" ; NEEDS REFACTORING SUCH AS THETAS

  omega_value <- switch (OMEGA2_or_CV,
                         "CV" = (unlist(OMEGA_VALUE)) ^ 2,
                         "OMEGA2" = OMEGA_VALUE
  )


  omega_rse <- switch (PRECISION,
                       "SE" = unlist(OMEGA_PRECISION) / unlist(OMEGA_VALUE),
                       "RSE" = unlist(OMEGA_PRECISION),
                       "CI95" = (unlist(OMEGA_PRECISION) / 1.96) / unlist(OMEGA_VALUE)
  )

  omegap <- omega_value


  omegapd <- (1/unlist(omega_rse)) %>% #(omega/ (RSE x omega))= 1/omega
    raise_to_power(2) %>%
    multiply_by(2) %>%
    add(1) %>%
    round(0)

  op_text <- paste0("\n$OMEGAP \n", omegap %>% paste0(" FIXED ;  ",names(omegap),collapse = "\n"), "\n")
  opd_text <- paste0( "\n$OMEGAPD \n", omegapd %>% paste0(" FIXED;  ",names(omegap), collapse = "\n"), "\n")


  #Print
  print_blocks(i = intro_prior(name = model_name),
               tp = text_tp(get_t_val(THETA_VALUE)),
               tpv = text_tpv(get_t_var(THETA_PRECISION, type = PRECISION, theta_val = THETA_VALUE)),
               op = op_text,
               opd = opd_text,
               e = end_prior()
               ) %>%
    cat(sep = "")

  message("/!\\ Reminder : \n
  $PRIOR NWPRI ;(between $SUB and $PK)
  $COV MATRIX = R")

}
