library(cffr)
library(codemeta)

#run this after running usethis::use_version()

#update inst/CITATION (for better results from `citation("rnpn")`)
#run these in order to prevent cff_create() from reading inst/CITATION and including it as a `preferred-citation`
unlink("inst/CITATION")
cff <- cff_create(keys = list(`date-released` = Sys.Date()), dependencies = FALSE)
cff_write(cff)
cff_write_citation(cff, file = "inst/CITATION")

#update codemeta.json
write_codemeta()
