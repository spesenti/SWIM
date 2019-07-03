if(requireNamespace('spelling', quietly = TRUE))
  spelling::spell_check_test(vignettes = "~/R package/SWIM", error = FALSE,
                             skip_on_cran = TRUE)
