intro_prior <- function(name){
  paste0("\n\n",
  ";===== PRIOR BLOCKS ======\n",
  ";===== from ",name," ======\n")
}

end_prior <- function()';===== End of PRIOR BLOCKS ====='

print_blocks <- function(i, tp, tpv, op, opd, e){
  paste0(i, tp, tpv, op, opd, e, collapse = "")
}

