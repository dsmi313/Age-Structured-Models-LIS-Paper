# Benchmark the current length.R implementation against the previous commit
# using a small, deterministic simulation run inside shiny::testServer.
#
# Usage:
#   Rscript tests/benchmark_length.R
#
# Note: Requires git access (to read the prior version) and the full set of
# package dependencies used by length.R.

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
})

load_server_from_ref <- function(ref_path) {
  env <- new.env(parent = emptyenv())
  sys.source(ref_path, envir = env)
  if (!exists("server", envir = env, inherits = FALSE)) {
    stop("No `server` object found when sourcing ", ref_path)
  }
  env$server
}

materialize_ref <- function(ref) {
  tf <- tempfile(fileext = ".R")
  cmd <- c("show", sprintf("%s:length.R", ref))
  status <- system2("git", cmd, stdout = tf, stderr = FALSE)
  if (!is.null(attr(status, "status")) && attr(status, "status") != 0) {
    stop("Unable to read length.R from ", ref)
  }
  tf
}

run_single_sim <- function(server_fn, nsim = 50, seed = 123) {
  set.seed(seed)
  testServer(server_fn, {
    session$setInputs(
      species = "white_crappie",
      growth_preset = "moderate",
      wl_a = 2.40991e-6,
      wl_b = 3.38,
      mat_size = 200,
      memorable_size = 305,
      nat_mort = 0.35,
      rec_cv = 0.8,
      enable_ddr = FALSE,
      enable_depensation = FALSE,
      steepness = 0.7,
      amax = 8,
      linf = 353,
      vbk = 0.374,
      t0 = 0.197,
      growth_cv = 0.15,
      exploitation = 0.34,
      capsize = 204,
      harvlim = 254,
      enable_slot = FALSE,
      slot_type = "traditional",
      slot_upper = 406,
      enable_max_limit = FALSE,
      max_harvest_size = 500,
      dismort = 0.09,
      nsim = nsim,
      ymax = 120,
      run_sim = 1
    )
    session$flushReact()
    sim_results()
  })
}

benchmark_pair <- function(nsim = 50) {
  current_server <- load_server_from_ref("length.R")
  old_path <- materialize_ref("HEAD^")
  on.exit(unlink(old_path), add = TRUE)
  old_server <- load_server_from_ref(old_path)

  message("Running current version (nsim = ", nsim, ")...")
  current_time <- system.time({ run_single_sim(current_server, nsim = nsim) })["elapsed"]

  message("Running previous version (nsim = ", nsim, ")...")
  old_time <- system.time({ run_single_sim(old_server, nsim = nsim) })["elapsed"]

  tibble::tibble(
    implementation = c("current", "previous"),
    seconds = c(current_time, old_time)
  ) %>%
    mutate(speedup = seconds[2] / seconds[1])
}

if (identical(Sys.getenv("TESTTHAT"), "true")) {
  invisible(benchmark_pair(nsim = 5))
} else {
  results <- benchmark_pair(nsim = 50)
  print(results)
}
