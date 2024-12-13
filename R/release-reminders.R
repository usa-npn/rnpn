
#to add additional checks to release issues created with usethis::use_release_issue()
#https://usethis.r-lib.org/reference/use_release_issue.html
release_bullets <- function() {
c(
  "Update CITATION.cff with `cffr::cff_write(cffr::cff_create(dependencies = FALSE))`",
  "Update inst/CITATION with `cffr::cff_write_citation(cffr::cff_create(), file = 'inst/CITATION')`",
  "Update codemeta.json with `codemetar::write_codemeta()`"
  #TODO add reminder to pre-compute vignettes
)
}
