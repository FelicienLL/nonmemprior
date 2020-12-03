#' Render character vector to paste into NM control stream
#'
#' @param model_name Name and reference of the model
#' @param THETA_VALUE a named list of the values of theta
#' @param THETA_PRECISION a list of precision on theta
#' @param OMEGA_VALUE a named list of the values of omega
#' @param OMEGA_TYPE how was inter-individual variabilty reported ?
#' @param OMEGA_PRECISION a list of precision on theta
#' @param PRECISION is precision reported as SE or RSE
#' @param .message display an additional reminder about other blocks.
#'
#' @return dummy result
#' @export
nmprior <- function(model_name,
                    THETA_VALUE,
                    THETA_PRECISION,
                    OMEGA_VALUE,
                    OMEGA_TYPE = c("OMEGA2","VAR", "CV", "SE", "logN_CV"),
                    OMEGA_PRECISION,
                    PRECISION = c("RSE", "SE", "CI90", "CI95"),
                    .message = TRUE){

  print_blocks(
    i = intro_prior(name = model_name),
    tp = text_tp(get_t_val(THETA_VALUE)),
    tpv = text_tpv(get_t_var(THETA_PRECISION, type = PRECISION, theta_val = THETA_VALUE)),
    op = text_op(get_o_val(OMEGA_VALUE, OMEGA_TYPE)),
    opd = text_opd(get_o_var(OMEGA_PRECISION, PRECISION, OMEGA_VALUE, OMEGA_TYPE), get_o_val(OMEGA_VALUE, OMEGA_TYPE)),
    e = end_prior()
  ) %>%
    cat(sep = "")

  if(.message){
    message("/!\\ Reminder : \n
  $PRIOR NWPRI ;(between $SUB and $PK)
  $COV MATRIX = R")
  }
}
