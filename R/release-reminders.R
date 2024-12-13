
#to add additional checks to release issues created with usethis::use_release_issue()
#https://usethis.r-lib.org/reference/use_release_issue.html
release_bullets <- function() {
c(
  "Update CITATION.cff with `cffr::cff_create(dependencies = FALSE) |> cff::cff_write()`",
  "Update codemeta.json with `codemetar::write_codemeta()`"
  #TODO add reminder to pre-compute vignettes
)
}
