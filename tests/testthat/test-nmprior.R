test_that("nmprior works", {
  expect_message(nmprior(model_name = "Urien,British Journal of Clinical Pharmacology, 2004",
                              THETA_VALUE = list(35.5,23.4,22.2,12),
                              THETA_PRECISION = list(1.4, 1, 1.44, 5.33),
                              OMEGA_VALUE = list(0.149, 0.234),
                              OMEGA_TYPE = "CV",
                              OMEGA_PRECISION = list(0.076, 0.075),
                              PRECISION = "SE"
  ))
})
