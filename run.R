source("scripts/load.R", local = TRUE)
port <- as.integer(Sys.getenv("UNDEPRCATE_PORT", Sys.getenv("RREPS_PORT", "3838")))
open_browser <- !identical(Sys.getenv("UNDEPRCATE_BROWSER", Sys.getenv("RREPS_BROWSER")), "0")
shiny::runApp(".", host = "127.0.0.1", port = port, launch.browser = open_browser)
