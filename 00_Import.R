#' ---
#' title: "00 Import: combine the PIAAC Cycle 2 public-use files"
#' output: html_document
#' ---
#'
#' **What this script does.** It reads the OECD public-use files (PUFs) of
#' PIAAC Cycle 2 (Survey of Adult Skills 2023), one CSV per country, stacks
#' them into one table and saves it as `Data/piaac_cycle2.rds`.
#' `01_Data_Setup.Rmd` starts from that file.
#'
#' **Input.** `Data/cycle2csvs/prg<cnt>p2.csv`, downloaded from
#' <https://www.oecd.org/en/data/datasets/piaac-2nd-cycle-database.html>.
#' The files are semicolon-separated and all have the same 2,482 columns.
#'
#' **Output.** `Data/piaac_cycle2.rds` (one row per respondent).
#'
#' **How the files are combined.** Following the OECD's guidance for
#' cross-country analysis with the PUFs, the country files are simply
#' appended. Nothing is re-weighted or re-scaled here: every file already
#' carries its own final weight (`SPFWT0`), its 80 replicate weights
#' (`SPFWT1`–`SPFWT80`), the replication method (`VEMETHOD`, `VEFAYFAC`,
#' `VENREPS`) and ten plausible values per skill domain. These must be kept
#' together so that later scripts can compute correct standard errors.
#'
#' **Missing values.** The CSVs use SAS-style missing codes stored as text:
#' `.v` valid skip, `.d` don't know, `.r` refused, `.n` not stated or not
#' released for that country, and a bare `.` for other missing. All are set
#' to `NA` here. A count of each code for the analysis variables is printed
#' below so that the recodes in 01 can be checked against it.
#'
#' **Cycle 1, for Supplementary Figure S2 only.** Part 2 at the end reads
#' the few PIAAC Cycle 1 (2012–17) variables needed to compare mean literacy
#' between the two cycles and saves them as `Data/piaac_cycle1_lit.rds`.
#' Nothing else in the analysis uses Cycle 1.
#'
#' This script only needs to run once. `run_pipeline.R` skips it when
#' `Data/piaac_cycle2.rds` and `Data/piaac_cycle1_lit.rds` both exist;
#' delete either file to re-import.

#+ setup, message = FALSE
library(here)
library(data.table)

raw_dir  <- here("Data", "cycle2csvs")
out_file <- here("Data", "piaac_cycle2.rds")

c1_file     <- here("Data", "piaac_combined.csv")      # Cycle 1, all countries
c1_out_file <- here("Data", "piaac_cycle1_lit.rds")

#' # Part 1. Cycle 2
#'
#' ## Variables to keep
#'
#' Keeping all 2,482 columns would make a multi-gigabyte file. Instead we
#' keep the variables the analysis uses, plus the design variables. Add a
#' name here and re-run the script if another variable is needed later.

keep_vars <- c(
  # identifiers
  "SEQID", "CNTRYID", "CNTRYID_E",
  # sex and age
  "GENDER_R", "A2_N02_T", "AGE_R", "AGEG5LFS", "AGEG10LFS",
  # birth country and language
  "A2_Q03a",      # born in this country? (1 yes, 2 no)
  "BORNLANG",     # 1-4: native/foreign-born x native/foreign language
  "NATIVELANG", "HOMLANG", "IMPARC2", "IMGENC2",
  # children
  "J2_Q03a",      # has children? (1 yes, 2 no)
  "J2_Q03b",      # number of children
  # education
  "EDCAT6_TC1", "EDCAT7_TC1", "EDCAT8_TC1",
  "B2_Q05a",      # currently studying for a formal qualification? (1 yes, 2 no)
  # adult learning (ALE) in the last 12 months
  "B2_Q08a", "NFE12C2", "NFE12NR", "NFE12JRC2", "NFE12NJRC2", "FAET12C2",
  "B2_Q22", "B2_Q23",
  # skills match
  "H2_Q19a",
  # plausible values
  paste0("PVLIT", 1:10), paste0("PVNUM", 1:10), paste0("PVAPS", 1:10),
  # weights and variance estimation
  "SPFWT0", paste0("SPFWT", 1:80),
  "VEMETHOD", "VEFAYFAC", "VENREPS", "VARSTRAT", "VARUNIT"
)

#' Columns that stay text after import (everything else becomes numeric).
text_vars <- c("VEMETHOD")

sas_missing <- c("", ".", ".a", ".b", ".c", ".d", ".n", ".r", ".u", ".v")

#' ## Read and stack the country files

files <- list.files(raw_dir, pattern = "^prg[a-z]{3}p2\\.csv$", full.names = TRUE)
cat(length(files), "country files found in", raw_dir, "\n")
stopifnot(length(files) > 0)

read_country <- function(f) {
  header  <- names(fread(f, sep = ";", nrows = 0))
  missing <- setdiff(keep_vars, header)
  if (length(missing) > 0) {
    cat("  ", basename(f), "lacks:", paste(missing, collapse = ", "), "\n")
  }
  d <- fread(f, sep = ";", colClasses = "character", encoding = "UTF-8",
             select = intersect(keep_vars, header), showProgress = FALSE)
  d[, source_file := basename(f)]
  d
}

raw <- rbindlist(lapply(files, read_country), use.names = TRUE, fill = TRUE)
cat("Stacked:", nrow(raw), "respondents,", ncol(raw), "columns\n")

#' ## Tally the missing codes before they are set to NA

code_tally <- rbindlist(lapply(setdiff(keep_vars, c(paste0("SPFWT", 1:80),
                                                   grep("^PV", keep_vars, value = TRUE))),
  function(v) {
    if (!v %in% names(raw)) return(NULL)
    x <- raw[[v]]
    data.table(variable = v,
               valid = sum(!x %in% sas_missing),
               `.v` = sum(x == ".v"), `.n` = sum(x == ".n"),
               `.d` = sum(x == ".d"), `.r` = sum(x == ".r"),
               `.`  = sum(x == "." | x == ""),
               other = sum(x %in% c(".a", ".b", ".c", ".u")))
  }))
print(code_tally)

#' ## Convert codes to NA and text to numbers

for (v in setdiff(names(raw), "source_file")) {
  x <- raw[[v]]
  x[x %in% sas_missing] <- NA
  if (!v %in% text_vars) x <- as.numeric(x)
  set(raw, j = v, value = x)
}

#' ## Country codes
#'
#' `CNTRYID` is the ISO 3166 numeric code. Canada and Switzerland have
#' sub-national codes in `CNTRYID_E` (language regions), which we ignore.
#' The lookup is written out instead of using a package so that the mapping
#' is visible to readers.

country_lookup <- data.table(
  CNTRYID = c(40, 56, 124, 152, 191, 203, 208, 233, 246, 250, 276, 348, 372,
              376, 380, 392, 410, 428, 440, 528, 554, 578, 616, 620, 702, 703,
              724, 752, 756, 826, 840),
  iso3c   = c("AUT", "BEL", "CAN", "CHL", "HRV", "CZE", "DNK", "EST", "FIN",
              "FRA", "DEU", "HUN", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA",
              "LTU", "NLD", "NZL", "NOR", "POL", "PRT", "SGP", "SVK", "ESP",
              "SWE", "CHE", "GBR", "USA")
)
raw <- merge(raw, country_lookup, by = "CNTRYID", all.x = TRUE, sort = FALSE)
stopifnot(!anyNA(raw$iso3c))

#' Respondents and replication settings by country. Fay's method with
#' factor 0.3 is used everywhere except France (paired jackknife, JK2);
#' Chile, Portugal and the United States have fewer than 80 replicates.

print(raw[, .(n = .N, VEMETHOD = first(VEMETHOD), VEFAYFAC = first(VEFAYFAC),
              VENREPS = first(VENREPS)), by = iso3c][order(iso3c)], nrows = 50)

#' ## Save

setcolorder(raw, c("iso3c", "CNTRYID", "SEQID"))
saveRDS(as.data.frame(raw), out_file)
cat("Saved", out_file, "(", round(file.size(out_file) / 1e6, 1), "MB )\n")

#' # Part 2. Cycle 1 literacy, for Supplementary Figure S2
#'
#' Figure S2 compares each country's mean literacy in Cycle 1 (2012–17)
#' with Cycle 2 (2022–23), for all adults and for the foreign-born. It needs
#' only the country, birth country, the ten literacy plausible values and the
#' weights, so only those columns are read from the 1 GB Cycle 1 file.
#'
#' **Input.** `Data/piaac_combined.csv`, the Cycle 1 public-use files of all
#' participating countries appended into one comma-separated file. Its
#' missing codes are letters (`V` valid skip, `N` not stated, `D` don't know,
#' `R` refused) or `NA`.
#'
#' **Variables.**
#'
#' - `J_Q04a` born in this country? (1 yes, 2 no). `FORBORN` = 1 if no.
#'   Cycle 1 has no single variable that mixes birth country and language,
#'   so this is the direct counterpart of `A2_Q03a` in Cycle 2.
#' - `PVLIT1`–`PVLIT10` literacy plausible values.
#' - `SPFWT0`–`SPFWT80` and `VEMETHOD`, `VEFAYFAC`, `VENREPS`. In Cycle 1
#'   countries use the jackknife (JK1 or JK2), not Fay's method.
#'
#' Only countries that also took part in Cycle 2 are kept (same country
#' lookup as Part 1). Respondents without literacy scores are dropped: one
#' block of 3,660 rows in the combined file has no country or scores at all.

#+ cycle1
c1_vars <- c("CNTRYID", "J_Q04a", paste0("PVLIT", 1:10),
             "SPFWT0", paste0("SPFWT", 1:80),
             "VEMETHOD", "VEFAYFAC", "VENREPS")

if (file.exists(c1_file)) {
  c1 <- fread(c1_file, select = c1_vars, colClasses = "character",
              na.strings = c("", "NA"))
  cat("Read", nrow(c1), "rows and", ncol(c1), "columns from", basename(c1_file), "\n")

  for (v in setdiff(c1_vars, "VEMETHOD")) {
    x <- c1[[v]]
    x[x %in% c("V", "N", "D", "R")] <- NA
    set(c1, j = v, value = as.numeric(x))
  }

  c1 <- c1[!is.na(CNTRYID) & !is.na(PVLIT1)]
  c1 <- merge(c1, country_lookup, by = "CNTRYID", sort = FALSE)   # Cycle 2 countries only
  c1[, FORBORN := fcase(J_Q04a == 1, 0, J_Q04a == 2, 1)]
  c1[, J_Q04a := NULL]

  # respondents, share foreign-born (unweighted) and replication settings
  print(c1[, .(n = .N, pct_forborn = round(100 * mean(FORBORN, na.rm = TRUE), 1),
               missing_forborn = sum(is.na(FORBORN)),
               VEMETHOD = first(VEMETHOD), VENREPS = first(VENREPS)),
           by = iso3c][order(iso3c)], nrows = 50)

  setcolorder(c1, c("iso3c", "CNTRYID", "FORBORN"))
  saveRDS(as.data.frame(c1), c1_out_file)
  cat("Saved", c1_out_file, "(", round(file.size(c1_out_file) / 1e6, 1), "MB )\n")
} else {
  cat("Data/piaac_combined.csv not found: Cycle 1 file not made,",
      "so 02_Descriptives.Rmd will skip Figure S2.\n")
}
