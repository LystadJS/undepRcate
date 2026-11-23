root <- normalizePath(".", winslash = "/", mustWork = TRUE)
stopifnot(file.exists(file.path(root, "DESCRIPTION")))
options(undepRcate.root = root)
# scripts/setup.R restores renv.lock to this project-local library.
if (dir.exists(".local-library")) .libPaths(c(normalizePath(".local-library"), .libPaths()))
suppressPackageStartupMessages(library(shiny))
source_module <- function(path, scope) {
  # A binary connection lets the R parser read UTF-8 notation even when the
  # workstation's native text connection would convert it to a legacy encoding.
  connection <- file(path, open = "rb")
  on.exit(close(connection))
  source(connection, local = scope, encoding = "UTF-8")
}
for (path in list.files("R", pattern = "[.]R$", full.names = TRUE)) {
  source_module(path, environment())
}
