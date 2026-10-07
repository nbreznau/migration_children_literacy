# Archive: original code, 2026-10-06

Snapshot of the analysis code and Results/ exactly as they were before the
2026-10-06 clean-up: the working copy of the local folder on that day
(git HEAD fd02a7d "Updated for ESREA talk", plus uncommitted edits in
03_Analyses.Rmd and 04_Analyses_Educ.Rmd).

Nothing in this folder is run by the current pipeline. It is kept so that any
number in an earlier draft of the paper can be traced back to the code that
produced it. Data files are NOT copied (too large); they live in Data/.

Contents
- 01_Data_Setup.Rmd ... 05_Robustness_Subsample.Rmd : original workflow
- Sub/      : original helper scripts sourced by 02_Descriptives.Rmd
- Shiny/    : original app.R / app_not_blind.R (data .rds files not copied)
- Results/  : all tables and figures as produced by the original code

Version history (what replaced these files)
- 2026-10-06: 01_Data_Setup.Rmd and 02_Descriptives.Rmd rewritten. Data now come
  from the OECD country files via the new 00_Import.R; Cycle 1 removed from 01;
  02 rebuilt on functions in R/functions_descriptives.R. The Sub/ helpers
  (IRT_helper_groups*.R, combine_g_objects*.R, country_loop.R) are no longer used
  by the pipeline. 03-05 are unchanged so far.
