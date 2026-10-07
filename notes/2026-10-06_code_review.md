# Code review and clean-up plan (2026-10-06)

Scope: `01_Data_Setup.Rmd` to `05_Robustness_Subsample.Rmd`, `Sub/*.R`, `Shiny/app.R`,
checked against the raw data in `Data/piaac_combined*.csv` and against the manuscript
(Jul 15 draft) and supplement (May 15 draft); paper-vs-code findings are in section E.

The original code and `Results/` are archived untouched in
`archive/2026-10-06_original/`.

---

## A. Problems that change reported numbers or their uncertainty

**A1. The confidence intervals on every ATE figure are not sampling uncertainty.**
(03 Fig 1/Fig1_dotmatrix/Fig1_sCI, 04 Fig 3 and Fig3_compare, 05 Fig S6, S6_middle,
Shiny `SE/lower/upper`.)
Each "ATE" is `predict(model, profile with ALE=1) - predict(model, profile with ALE=0)`.
Because every row gets the same profile values, that difference is the same number
for every row: it is a fixed linear combination of model coefficients. The CI is then
taken from `t.test(pred1, pred0)` across ~100k rows, so its width reflects the spread of
the *other* covariates (country between-terms, GDP) and the row count, not the
standard error of the coefficients. The later "scaling" to n = 5,700 / 6,000 or to
group-size `n_expected` (04) rescales that same non-SE.
*Fix:* compute each ATE as a linear contrast of fixed effects with its SE from
`vcov(model)` (delta method; `marginaleffects::avg_comparisons()` or
`emmeans::contrast()` do this for lmer), then pool over plausible values (A2).

**A2. Literacy is the mean of the 10 plausible values, used as one outcome.**
(`skill_reading = rowMeans(PVLIT1:10)` in 01; every model in 03–05.)
Point estimates of linear models are roughly fine, but SEs are understated and any
non-linear quantity (levels, % below Level 2) is biased. The descriptive helpers in
`Sub/IRT_helper_groups_se_pop.R` already do this correctly (one estimate per PV,
Rubin's rules); the models do not.
*Fix:* fit each model once per PV and combine with Rubin's rules (`mitml::testEstimates`
or a 15-line helper). 10x run time; m5 is the slow one.

**A3. Replicate-weight settings are wrong for both cycles.** (02 `svrepdesign`.)
Code uses `type = "Fay", rho = 0.5` everywhere. The data say:
Cycle 2: Fay with factor 0.3 for all countries except France (JK2), and Chile,
Portugal and the US have only 28, 52 and 44 replicates. Cycle 1: JK1 or JK2, not Fay.
Means are unaffected; every SE in Fig A1/Fig 2 descriptives is off.
*Fix:* build the design per country from `VEMETHOD`, `VEFAYFAC`, `VENREPS`
(or use `EdSurvey`/`intsvy`, which read these automatically).

**A4. Models are unweighted, justified by a test that cannot show it.**
(03 "Sample Bias Test": correlation of residuals with `aweight`.)
A near-zero residual–weight correlation does not show that weights are ignorable for
the interaction terms. Reviewers at Soc of Ed / AEQ will ask. Also `aweight = SPFWT0 /
min(SPFWT0)` makes each country's total weight depend on its single smallest weight,
so it is neither population- nor sample-proportional in pooled models.
*Options:* (a) keep unweighted multilevel models and add a weighted country-fixed-effects
OLS (senate weights, replicate SEs) as the robustness check; (b) make the weighted FE
model the main one. I'd recommend (a) because the multilevel story is in the paper.

**A5. "GDP?" check uses one respondent per country.** (03)
`summarise(lit = first(skill_reading))` takes the *first person's* literacy, not the
country mean, so the r and p printed there, and the conclusion that GDP is not a
confounder, are meaningless. Same for `first(gdppc_k)` (fine, constant) but not lit.

**A6. Missing-value codes in the raw CSV are SAS-style strings** (`.`, `.n`, `.v`,
`.d`, `.r`). Several recodes in 01 turn them into substantive 0s:
- `children`: ~3,000 respondents with missing `J2_Q03a` are coded childless.
- `ed_currently`: ~7,000 with missing `B2_Q05a` are coded "not in education" and kept.
- `underskilled`, `NFE_barrier`: valid skips coded 0 (not used in the paper models).
- `Sub/IRT_helper_groups_se_pop.R::svy_pct_yes()` counts NA as "no", so ALE % and
  tertiary % in the Fig 2 trees are biased down wherever NFE12/ed3 are missing.
Each is defensible only if stated; I'd recode to NA and report the listwise N.

**A7. Education backup recode is inconsistent.** (01)
`ed3_backup` maps EDCAT7 = 2 (lower secondary) to "secondary", but EDCAT6 = 1
("lower secondary or less") maps to "primary". Only used when EDCAT6 is missing, so
small, but it should be EDCAT7 1–2 → 1, 3–4 → 2, 5–7 → 3.

**A8. Rich vs "middle-income" split.** (05; paper title.)
The Cycle 2 file has 29 countries (no NLD, no NZL). The "middle-income" group is
CHL, CZE, EST, HRV, HUN, ISR, KOR, LTU, LVA, POL, SGP, SVK; all are World Bank
high-income. The 05 text also says "10 country sample". "Upper- and Middle-Income"
in the title needs a different label (e.g. "Western vs. Central-Eastern Europe and
other OECD") or a cited classification.

**A9. Robustness models are rank-deficient.** (05)
In 05, `ed3_secondary = ed3 %in% 1:2`, so with ed3 non-missing it is exactly
`1 - ed3_tertiary`; m4_sub, m5_sub and m5_mid include both, so lmer silently drops
one. In 03/04 `ed3_secondary = ed3 == 2`. Same name, two definitions.

**A10. Two ATE methods give different "profiles".**
Fig 1 / Fig 3 set within-values to `1 - grand mean` for everyone; the Shiny/
population functions set `target - that person's country mean`. Both are defensible,
but they answer different questions and the paper should use one.

---

## B. Reproducibility bugs (code would not re-create the current Results/)

**B1. 04 cannot be knitted.** It uses `M2.3`, `M2.4`, `M2.5` from 03's session.
Knit always starts a clean session, so 04 fails at Table A4 unless run by hand.

**B2. Several figures in Results/ were made interactively.** `.Rhistory` shows
`Fig1_dotmatrix.png` produced with "newly adjusted lower/upper" code that is not in
03, and `Fig1_sCI.png` in 03 plots `lower/upper`, not the adjusted `lower_sCI/upper_sCI`
it computes ("Makes no difference" is because the wrong columns are plotted).

**B3. Output files overwrite each other.**
- `table_S1.html` is written three times in 02 (all, women, men): men's table survives.
- `TS2.docx`: written by 03 (m4) and then by 04 (m6).
- `TS3.*`: TS3.csv (omitted intercategories), TS3.html (16 groups), TS3.docx (rich m4).
- `TS4.*`: TS4.csv (UN migration), TS4.docx (rich m5).

**B4. m4 and m5 differ between scripts.** 03 m4 has `gdppc_k_log` and REML; 04 m4
has no GDP and ML. 04's m5 (Table A3, Fig 3) includes `ed3_secondary_*`; 04's
m5_compare (Table A4) does not. So Table A3 and Table A4 "Model 5" are different
models, and Fig 3's "Non-Tertiary" profile is specifically "upper secondary".

**B5. `plot_df.RData` (Fig A1) is loaded, not rebuilt.** `country_loop.R` also reads
`sd_lit_*` from a helper that now returns `se_lit_*` (silently dropped).

**B6. Smaller items.** `predict(m5, re.form = NA)` without `newdata` in the Shiny
precompute relies on m5's rows equalling `df2r`'s; Table S2 mixes a literacy mean
from the full sample with N from the analysis sample; DE literacy tree includes
current students but the numeracy tree drops them; `forlang_within` is not re-centred
for the immigrant/native split models while the others are; Fig 2 colours are scaled
within each panel, so the same colour means different values across panels;
TS3 `ratio_*` is 1 / (ALE rate), not a ratio of shares; "Europe" foreign-born share
is "Western Europe (UN)"; comment says 26 countries, data have 28 after dropping DNK;
Cycle 1 vs 2 literacy comparison (Fig A1) should use the OECD rescaled Cycle 1 PVs.

---

## C. Proposed structure

```
run_pipeline.R            # sources everything in order, writes logs/ (already added)
R/
  00_packages.R           # one package list, versions printed to the log
  functions_data.R        # recodes, within/between centring (one definition)
  functions_pv.R          # fit-per-PV + Rubin pooling, replicate designs per country
  functions_ate.R         # profile ATEs with delta-method SEs, pooled over PVs
  functions_plots.R       # dot-matrix + bar figure (now copy-pasted 6 times)
01_data_setup.Rmd         # cleaned; explicit missing-code handling; N table per step
02_descriptives.Rmd
03_main_models.Rmd        # saves models to Data/models/*.rds
04_education_models.Rmd   # loads those models, so it knits on its own
05_robustness.Rmd
Results/                  # file names = paper numbering (Table1.docx, FigS3.png, ...)
Results/output_map.csv    # file -> paper table/figure -> script -> chunk
archive/2026-10-06_original/
```

Each Rmd gets a short "What this script does / inputs / outputs" header and a
sentence before every chunk saying what it does and why, written for replicators.
Every script ends with sample sizes at each filtering step and `sessionInfo()`.

## D. Logging system (ready now)

`run_pipeline.R` in the project root. Open it in RStudio and click **Source**. It knits
each script to plain Markdown into `logs/<date_time>/`:
- `<script>.md`: code + printed output and tables in order (figures in a subfolder)
- `<script>.log`: everything printed to the console, including warnings hidden by
  `warning = FALSE` in the chunks
- `run_summary.txt`: OK/FAILED and minutes per script; `sessionInfo.txt`
- `logs/LATEST.txt` names the newest run

It stops at the first failing script. Logs are git-ignored. Once the scripts are
clean, each will also write its key numbers (model coefficients, ATEs with SEs, Ns)
to small CSVs in the log folder so runs can be diffed number by number.

A first run on the *original* code is useful as a baseline: it shows which numbers
in the current draft are reproducible before anything changes.

---

## E. Paper vs. code (manuscript Jul 15, supplement May 15)

**E1. The paper's tables come from an older version of the code.** The current
`Results/*.docx` do not reproduce them:
| Paper | Paper spec | Current code / Results |
|---|---|---|
| Table A2 (OLS) | `factor(ed3)`: secondary + tertiary; NFE12 M1.1 = 20.97 | `I(ed3==3)` only; NFE12 = 22.81 (`TA2.docx`) |
| Table A3 | M2.3a/M2.3b with GDP; `ed3_sec` terms | no GDP variants in 03; secondary commented out |
| Table A4 (M4) | secondary + GDP | 03 m4: tertiary + GDP (ALE within 17.86), then `TS2.docx` overwritten by 04's m6 |
| Supp Table S1 (M5) | GDP + secondary; intercept 217.98 | 04 m5 has no GDP; intercept 259.04 (`TA3.docx`) |
*Decision needed:* which specification is canonical (secondary in or out; GDP in or out).
The clean code will then produce exactly that set, under the paper's names.

**E2. Model naming.** The paper's M2.4 (four ALE interactions) is the code's M2.5; the
text says Mx.4 is ALE × foreign-born. The text says ATEs come from M2.4; the code
computes them from m4.

**E3. AIC comparison is invalid as reported.** Paper: M4 1,269,259.63 vs M5
1,269,054.29. Those are REML fits with different fixed effects, which are not
comparable. The ML refit in current TA4 gives 1,274,929.71 vs 1,274,701.31 (same
conclusion, different numbers).

**E4. Methods text vs code.**
- "Country-specific random slope": models have random intercepts only (a slope exists
  only in the Shiny context function).
- Education from EDCAT7_TC1: code uses EDCAT6_TC1 first, EDCAT7 only as backup.
- GDP per capita "2023": code uses the 2019–2022 mean, capped at 60k, logged.
- N: text 122,816; tables 122,641.
- Fig 3 note "average country of 5,000": code scales to 6,000 (04) / 5,700 (03),
  and the scaling itself is not a valid SE (A1).
- "GLM" → linear mixed models. OLS models are described as having random intercepts;
  they use country dummies (fixed effects).
- Rabe-Hesketh & Skrondal (2006) is cited for dropping weights. *Corrected 2026-10-06:*
  the citation fits if the text states its condition. The paper shows unweighted
  estimates are fine (and more efficient) when selection is non-informative given the
  model covariates, and weights are needed when it is not. PIAAC weights are calibrated
  on age, sex, education and region (and in some countries birth country); the models
  include sex, education and birth country but not age or region. Suggested: add age
  group (`age10`) as a control and report a weighted vs unweighted comparison (or the
  DuMouchel–Duncan test) instead of the residual–weight correlation.

**E5. Supplement Note S1.** The equation leaves female out of the interaction and
miscounts the sub-interactions. It says ATEs use "actual values" and are non-linear;
the code fixes the profile values for every row, so each ATE is a linear combination
of coefficients (see A1).

**E6. Figures and tables without code, or code without a place in the paper.**
- Supp Fig S4 (Europe-only ATEs) and S5 (ATE by course type): no code found.
- Code's FigS6, FigS6_middle, FigS7: not in the supplement.
- Supp Tables S2 and S3 are empty. Table S4 "Ratio" is 1 / ALE rate (B6).
- Supp Fig S3 B lists AUS, NZL and DNK, which are not in the analysis data.
- Figure 4 is captioned "Figure 3".
- Table A1 shows artifact rows ("Listwise Missing Cases" 1/1/0, `aweight`) and uses the
  full sample (n 126,621), not the analysis sample.

**E7. Framing.**
- Title/footnote "Upper- and Middle-Income" and "OECD members": all 28 countries are
  high-income; SGP and HRV are not OECD members (A8).
- "24 or 28 countries declined": Cycle 1 overlaps with fewer countries; needs checking
  against the data once Fig A1 is rebuilt.
- Results vs conclusion: results say non-tertiary groups have higher returns to ALE;
  the conclusion says tertiary-educated foreign-born non-native speakers have the most
  to gain. One of them needs to change once the ATEs have proper SEs.

---

## F. 2026-10-06 rebuild of the data and descriptives (decisions)

- Data now come straight from the 30 OECD Cycle 2 country files via `00_Import.R`
  (157,525 respondents). NZL is new. AUS took no part in Cycle 2. NLD has no file in
  `Data/cycle2csvs`. DNK's public file withholds language, education and current
  studies, so DNK cannot enter the analysis sample (29 countries, 126,629 cases).
- Education: tertiary only (Nate, 2026-10-06). EDCAT6 and EDCAT7 give identical
  tertiary splits; EDCAT7 is missing for AUT, CHE, NOR, SWE.
- Missing codes are kept as NA (children, current study, ALE) instead of 0.
- Descriptives describe the analysis sample, weighted with SPFWT0; SEs use each
  country's replication method and Rubin's rules over the 10 PVs (checked against
  the survey package for DEU, FRA, CHL: identical).
- Cycle 1 is used only for Supp Fig S2 and the "24 of 28 countries declined" sentence.
- Fig S2 rebuilt (Nate, 2026-10-06: keep it, read only the needed Cycle 1 columns).
  `00_Import.R` Part 2 reads 96 of 1,200+ columns from `Data/piaac_combined.csv`
  (CNTRYID, J_Q04a, PVLIT1-10, SPFWT0-80, VE*) and saves `Data/piaac_cycle1_lit.rds`;
  02 computes the change with replicate SEs and writes `Fig_S2.png` + `Fig_S2_data.csv`.
  Countries in both cycles: 26 (old figure had 25; NZL now added; NLD has no Cycle 2
  file). All adults declined in 22 of 26 (18 significantly); foreign-born declined
  in 21 of 26 (14 significantly). The paper's "24 of 28" needs updating. Means match
  the old plot_df (all adults identical; foreign-born within ~1 point in Cycle 2).
  Caveat: original Cycle 1 PVs, not OECD's re-estimated trend values.
