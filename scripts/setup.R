# Run from the repository root with Rscript --vanilla scripts/setup.R.
stopifnot(file.exists("renv.lock"), file.exists("DESCRIPTION"))
project <- normalizePath(".", winslash = "/")
project_library <- file.path(project, ".local-library")
dir.create(project_library, showWarnings = FALSE)
.libPaths(c(project_library, .libPaths()))
options(repos = c(CRAN = "https://cloud.r-project.org"))
restore_repos <- getOption("repos")
bootstrap_repo <- restore_repos
if (.Platform$OS.type == "windows" && R.version$arch == "x86_64" &&
    getRversion() >= "4.5" && getRversion() < "4.6") {
  # No single Windows binary snapshot contains every exact version in this lock.
  dates <- c(RSPM = "2026-04-30", summer = "2026-08-10",
             autumn = "2026-09-30", winter = "2026-02-28")
  snapshots <- setNames(paste0("https://packagemanager.posit.co/cran/", dates), names(dates))
  restore_repos <- c(snapshots, restore_repos)
  bootstrap_repo <- snapshots["summer"]
}

# Keep renv's downloaded packages and installation metadata inside the project.
Sys.setenv(RENV_PATHS_ROOT = file.path(project, "renv", "local"),
           RENV_CONFIG_CACHE_ENABLED = "FALSE")
if (!dir.exists(file.path(project_library, "renv"))) {
  previous <- find.package("renv", quiet = TRUE)
  if (length(previous)) {
    stopifnot(file.copy(previous, project_library, recursive = TRUE))
  } else {
    install.packages("renv", lib = project_library, repos = bootstrap_repo)
  }
}
lock <- renv::lockfile_read("renv.lock")
if (as.character(getRversion()) != lock$R$Version)
  message("The tested R version is ", lock$R$Version, ". This R is ", getRversion(), ".")

# A workstation with all locked builds already installed can set up offline.
matches_lock <- function(record, libraries) {
  location <- find.package(record$Package, lib.loc = libraries, quiet = TRUE)
  if (!length(location)) return(FALSE)
  version <- read.dcf(file.path(location, "DESCRIPTION"), fields = "Version")[[1L]]
  identical(version, record$Version)
}
present <- vapply(lock$Packages, matches_lock, logical(1), libraries = project_library)
if (!all(present) && all(vapply(lock$Packages, matches_lock, logical(1), libraries = .libPaths()))) {
  renv::hydrate(project = project, library = project_library, packages = names(lock$Packages),
                sources = .libPaths(), prompt = FALSE)
} else if (!all(present)) {
  # restore() can leave exact-version recommended packages in R's own library.
  # Materialize those builds so the project-local verification also covers them.
  bundled <- Filter(function(record) identical(record$Priority, "recommended") &&
                      matches_lock(record, .Library), lock$Packages[!present])
  if (length(bundled)) {
    renv::hydrate(project = project, library = project_library, packages = names(bundled),
                  sources = .Library, prompt = FALSE)
  }
}
present <- vapply(lock$Packages, matches_lock, logical(1), libraries = project_library)
if (!all(present)) {
  renv::restore(project = project, library = project_library,
                packages = names(present)[!present], repos = restore_repos, prompt = FALSE)
}
verified <- vapply(lock$Packages, matches_lock, logical(1), libraries = project_library)
if (!all(verified)) stop("Locked versions unavailable: ", paste(names(verified)[!verified], collapse = ", "))
message("Verified ", length(verified), " locked packages in ", project_library, ".")
