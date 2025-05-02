withr::with_dir("vignettes", {
  knitr::knit("III_individual_phenometrics.Rmd.orig", "III_individual_phenometrics.Rmd")
})
