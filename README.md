
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nonmemprior

<!-- badges: start -->

<!-- badges: end -->

The goal of nonmemprior is to automatically return the code text that
you need to include in order to use $PRIOR routine in NONMEM. Automatic
computation of variance and degree of freedom to pass to NWPRI.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("FelicienLL/nonmemprior")
```

## Features

What does nonmemprior doesn’t do:

  - Dealing with omega blocks (I should look into it)
  - Dealing with log transformed parameters and mu-referencing (don’t
    know if necessary ?)
  - Taking information from a NMXML file (should it ?)
  - Taking information from a xpose file (I should look into it)

## Ressources

  - [Gislekog *et al*, J Pharmacokinet Pharmacodyn,
    2002](https://link.springer.com/article/10.1023/A:1022972420004).
    Princeps article about the $PRIOR routine implemented in NONMEM.
  - [Chan Kwong *et al*, J Pharmacokinet Pharmacodyn,
    2020](https://link.springer.com/article/10.1007/s10928-020-09695-z).
    Recent review and guidance about the subroutine.
