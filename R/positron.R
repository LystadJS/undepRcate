practice_folder <- function(s) {
  file.path(getOption("undepRcate.practice", getOption("rreps.practice", "practice")), sprintf("session-%06d", s$id),
            sprintf("rep-%03d-%s-%d", s$position, s$template_id, s$seed))
}

prepare_files <- function(s, ex) {
  path <- practice_folder(s)
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  answer <- file.path(path, "answer.R")
  if (!file.exists(answer)) writeLines(s$draft, answer, useBytes = TRUE)
  if (!file.exists(file.path(path, "inputs.rds"))) saveRDS(ex$inputs, file.path(path, "inputs.rds"))
  context <- file.path(path, "context.R")
  # The documented source() command runs from the project root. Relative paths
  # keep exported exercises usable when the checkout moves.
  data_path <- encodeString(file.path(path, "inputs.rds"), quote = '"')
  library_line <- '.libPaths(c(".local-library", .libPaths()))'
  input_line <- paste0("list2env(readRDS(", data_path, "), envir = .GlobalEnv)")
  if (!file.exists(context)) {
    writeLines(c(paste("#", strwrap(ex$instructions)), library_line, "library(tidyverse)",
      if (identical(ex$category, "Missing Data")) 'invisible(loadNamespace("mice"))', input_line), context)
  } else relocate_context(context)
  answer
}

relocate_context <- function(context) {
  # Repair only the generated paths. The learner may have added their own
  # setup code or notes to this file, and their answer lives separately.
  original <- readLines(context, warn = FALSE, encoding = "UTF-8")
  updated <- original
  old_root <- file.path(dirname(getOption("undepRcate.root")), "RReps")
  input_file <- file.path(dirname(context), "inputs.rds")
  old_library <- sprintf('.libPaths(c("%s/.local-library", .libPaths()))', old_root)
  input_line <- function(path) sprintf('list2env(readRDS(%s), envir = .GlobalEnv)', encodeString(path, quote = '"'))
  updated[original == old_library] <- '.libPaths(c(".local-library", .libPaths()))'
  updated[original == input_line(file.path(old_root, input_file))] <- input_line(input_file)
  if (!identical(updated, original)) writeLines(enc2utf8(updated), context, useBytes = TRUE)
  invisible(!identical(updated, original))
}

read_practice_file <- function(s) {
  path <- file.path(practice_folder(s), "answer.R")
  size <- file.info(path)$size
  if (is.na(size)) stop("Open the exercise folder and save answer.R first.")
  if (size > 1024^2) stop("The answer file exceeds the 1 MB editor limit.")
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
