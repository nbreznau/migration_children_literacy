# =============================================================================
# run_pipeline.R
#
# Runs the analysis scripts in order and writes a complete, readable log of
# each run to logs/<date_time>/. Open this file in RStudio and click "Source"
# (or run source("run_pipeline.R") from the console).
#
# What you get in logs/<date_time>/ :
#   <script>.md        the knitted script as plain Markdown: code, printed
#                      output and tables, in order (figures in <script>_files/)
#   <script>.log       everything R printed to the console while knitting,
#                      including warnings and messages hidden from the .md
#   run_summary.txt    one line per script: OK / FAILED, run time, error text
#   sessionInfo.txt    R version, OS and package versions
# logs/LATEST.txt points to the most recent run folder.
#
# The .md and .log files are plain text, so they can be read and diffed
# against earlier runs without opening R.
# =============================================================================

library(here)
library(rmarkdown)

# -----------------------------------------------------------------------------
# Settings
# -----------------------------------------------------------------------------

# Scripts to run, in order.
# 00_Import.R is skipped automatically when its outputs already exist
# (see below); delete Data/piaac_cycle2.rds to force a fresh import.
scripts <- c(
  "00_Import.R",
  "01_Data_Setup.Rmd",
  "02_Descriptives.Rmd",
  "03_Analyses.Rmd",
  "04_Analyses_Educ.Rmd",
  "05_Robustness_Subsample.Rmd",
  "06_Simulations.Rmd"
)

# Scripts to leave out of this run, e.g. skip <- c("05_Robustness_Subsample.Rmd")
skip <- c()
scripts <- setdiff(scripts, skip)

# Every script loads what it needs from Data/ (04 reads the models saved by
# 03 in Data/models_03.rds), so each runs in a fresh environment, as when
# knitted on its own in RStudio. Set to TRUE to share one environment.
shared_env <- FALSE

# -----------------------------------------------------------------------------
# Set up the log folder for this run
# -----------------------------------------------------------------------------

run_id  <- format(Sys.time(), "%Y-%m-%d_%H%M")
log_dir <- here("logs", run_id)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
writeLines(run_id, here("logs", "LATEST.txt"))

summary_file <- file.path(log_dir, "run_summary.txt")
cat("Pipeline run", run_id, "\n", file = summary_file)

# The original scripts call pacman::p_load(packages, character.only = TRUE),
# which looks the `packages` vector up in the global environment. So the
# shared run uses the global environment itself, exactly as when a script is
# run by hand in RStudio. (Clean up with rm(list = ls()) afterwards if needed.)
run_env <- if (shared_env) globalenv() else new.env(parent = globalenv())

# -----------------------------------------------------------------------------
# Knit one script to Markdown, capturing all console output in a .log file
# -----------------------------------------------------------------------------

run_one <- function(script) {
  name     <- tools::file_path_sans_ext(script)
  log_file <- file.path(log_dir, paste0(name, ".log"))
  env      <- if (shared_env) run_env else new.env(parent = globalenv())

  con <- file(log_file, open = "wt")
  sink(con, type = "output")
  sink(con, type = "message")

  t0     <- Sys.time()
  status <- "OK"
  err    <- ""

  cat("==== ", script, " started ", format(t0), " ====\n", sep = "")

  tryCatch(
    withCallingHandlers(
      rmarkdown::render(
        input             = here(script),
        # Markdown output overrides the word/html formats in the YAML header
        output_format     = rmarkdown::md_document(variant = "gfm"),
        output_file       = paste0(name, ".md"),
        output_dir        = log_dir,
        intermediates_dir = tempdir(),
        knit_root_dir     = here(),
        envir             = env,
        quiet             = FALSE
      ),
      # Write every warning to the log as it happens, then carry on
      warning = function(w) {
        cat("WARNING: ", conditionMessage(w), "\n", sep = "")
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      status <<- "FAILED"
      err    <<- conditionMessage(e)
      cat("ERROR: ", err, "\n", sep = "")
    }
  )

  mins <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  cat("==== ", script, " finished: ", status, " (", mins, " min) ====\n", sep = "")

  sink(type = "message")
  sink(type = "output")
  close(con)

  line <- sprintf("%-32s %-7s %6.1f min  %s", script, status, mins, err)
  cat(line, "\n", file = summary_file, append = TRUE)
  message(line)

  status
}

# -----------------------------------------------------------------------------
# Run the scripts. Stop at the first failure, because later scripts read the
# files earlier ones write.
# -----------------------------------------------------------------------------

# Cycle 1 is only needed for Figure S2; without piaac_combined.csv its
# output is not expected.
import_files <- here("Data", "piaac_cycle2.rds")
if (file.exists(here("Data", "piaac_combined.csv")))
  import_files <- c(import_files, here("Data", "piaac_cycle1_lit.rds"))
if ("00_Import.R" %in% scripts && all(file.exists(import_files))) {
  scripts <- setdiff(scripts, "00_Import.R")
  line <- sprintf("%-32s %-7s %s", "00_Import.R", "SKIPPED",
                  "imported data files already exist")
  cat(line, "\n", file = summary_file, append = TRUE)
  message(line)
}

for (s in scripts) {
  if (run_one(s) == "FAILED") {
    message("Stopped after ", s, " failed. See ", log_dir)
    break
  }
}

writeLines(capture.output(sessionInfo()), file.path(log_dir, "sessionInfo.txt"))
message("Logs written to ", log_dir)
