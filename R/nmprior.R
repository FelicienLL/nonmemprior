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
  #browser()
  omega_value <- switch (OMEGA2_or_CV,
                         "CV" = (unlist(OMEGA_VALUE)) ^ 2,
                         "OMEGA2" = OMEGA_VALUE
  )

  theta_rse <- switch (PRECISION,
                       "SE" = unlist(THETA_PRECISION) / unlist(THETA_VALUE),
                       "RSE" = unlist(THETA_PRECISION),
                       "CI95" = (unlist(THETA_PRECISION) / 1.96) / unlist(THETA_VALUE)
  )

  omega_rse <- switch (PRECISION,
                       "SE" = unlist(OMEGA_PRECISION) / unlist(OMEGA_VALUE),
                       "RSE" = unlist(OMEGA_PRECISION),
                       "CI95" = (unlist(OMEGA_PRECISION) / 1.96) / unlist(OMEGA_VALUE)
  )


  # THETAP

  thetap <- unlist(THETA_VALUE)

  # THETAPV
#browser()
  thetapv0 <- map2(thetap,
                  theta_rse,
                  function(.x, .y){
                    (.x * .y)^2
                  }) %>%
    unlist() %>%
    as.double() %>%
    dmat() %>%
    as_tibble() %>%
    mutate(across(everything(), as.character))

  thetapv <- replace(thetapv0, upper.tri(thetapv0),"") %>%
    unite(everything(), col = "THETAPV", sep = " ") %>%
    mutate(THETAPV = ifelse(row_number() == 1,
                            paste0(.data$THETAPV, " FIXED"),
                            .data$THETAPV)
    ) %>%
    pull


  #OMEGAP

  omegap <- omega_value

  #OMEGAPD

  omegapd <- (1/unlist(omega_rse)) %>% #(omega/ (RSE x omega))= 1/omega
    raise_to_power(2) %>%
    multiply_by(2) %>%
    add(1) %>%
    round(0)

  #   browser()
  #PRINT EVERYTHING

  cat(
    "\n\n",
    ";===== PRIOR BLOCKS ======\n",
    ";===== from ",model_name," ======\n",

    "$THETAP \n",
    thetap %>% paste0(" FIXED ;  ",names(thetap),collapse = "\n"),
    "\n\n",

    "$THETAPV BLOCK(", length(thetapv),") \n",
    thetapv %>% paste0( paste0(" ; ", names(thetap)), collapse = "\n"),
    "\n\n",

    "$OMEGAP \n",
    omegap %>% paste0(" FIXED ;  ",names(omegap),collapse = "\n"),
    "\n\n",

    "$OMEGAPD \n",
    omegapd %>% paste0(" FIXED;  ",names(omegap), collapse = "\n"),
    "\n\n",

    ";===== end of prior blocks =====\n\n\n",

    sep = ""
  )

  message("/!\\ Reminder : \n
  $PRIOR NWPRI ;(between $SUB and $PK)
  $COV MATRIX = R")

}
