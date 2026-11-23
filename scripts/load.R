root <- normalizePath(".", winslash = "/", mustWork = TRUE)
stopifnot(file.exists(file.path(root, "DESCRIPTION")))
options(undepRcate.root = root)
# scripts/setup.R restores renv.lock to this project-local library.
if (dir.exists(".local-library")) .libPaths(c(normalizePath(".local-library"), .libPaths()))
suppressPackageStartupMessages(library(shiny))
for (file in list.files("R", pattern = "[.]R$", full.names = TRUE)) {
  source(file, local = environment(), encoding = "UTF-8")
}
