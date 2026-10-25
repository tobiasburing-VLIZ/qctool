# QC Tool (Beta)

Quality control of Darwin Core datasets for **EMODnet Biology / EurOBIS**. One set of
rules, the strictest of GBIF, OBIS and EMODnet. A dataset that does not meet them has to
be fixed by the provider before it can be harvested.

Internal test version. Not for distribution.

## How it is meant to be used

One R package, two ways in; both run the same checks with the same rules workbook.

| Who | How | For what |
|---|---|---|
| DM team, in R | `qc_check()`, `qc_issues()`, `qc_export_*()` | one dataset, or many in a loop (see `dev/run_test_datasets.R`); only some checks with `checks = ...`; later in the harvest pipeline |
| DM team and data providers | the app, `qc_app()` | look at one dataset: what blocks the harvest, which records, where on the map |

Now: internal tests by the DM team (e.g. on the VLIZ RStudio Server). Later: the app on a VLIZ
server for providers (needs a machine with enough memory for big datasets), and the package on
GitHub / r-universe; CRAN is possible but not needed (see `docs/design.md`, section 11).

## Install

Installing the tool installs every package it needs; nothing to install by hand.

```r
install.packages("remotes")
remotes::install_github("tobiasburing-VLIZ/qctool")
# While the repository is private, GitHub first needs a token:
# usethis::create_github_token(), then gitcreds::gitcreds_set()
```

From a downloaded zip (*Code > Download ZIP*, e.g. on the RStudio Server): unzip, open
`qctool.Rproj`, then `remotes::install_local(".", upgrade = "never")` (or `devtools::install()`).
On a Linux server a package that fails to compile needs system libraries: `libcurl4-openssl-dev`,
`libxml2-dev`, `libssl-dev` (curl, xml2, openxlsx2) and GDAL, GEOS, PROJ (sf and terra, for leaflet).

## Use it: the app

```r
library(qctool)
qc_app()          # opens in the browser
```

For development, `devtools::load_all("qctool")` from the QC Tool folder loads the code without installing.

1. **Load**: a file (DwC-A zip, CSV, TXT, Excel) or a web address: an IPT / GBIF / OBIS address,
   or a link to an archive or a table (http(s) only). For a table, the app proposes a Darwin Core
   term for every column; check and change them. Options: online
   services on/off, your own rules workbook, only some checks (groups or single checks).
2. **Overview**: the verdict (*Not ready for harvest*, *Not complete* when checks that can block
   the harvest did not run, *No errors*, *No errors and no warnings*), right under it the checks
   that did not run (and why), errors / warnings / info, the **essentials** (share of occurrences
   without errors in taxonomy, position, date, occurrenceStatus), the **links between tables**
   (records pointing to missing events or occurrences, missing parents, doubled IDs), the **values and their IDs**
   (one scientificName with two scientificNameIDs, one measurementType with two typeIDs, ...), the data checks **per table**
   (errors shown first: they block the harvest; tick warnings / info to see more), one line on
   the metadata.
3. **Data issues**, one tab per table: problems (identical ones grouped) → click → their records,
   as they are in the data, with the problem's columns highlighted → click → everything about one
   record: its issues and those of its events, its values, the events above it, its measurements.
   Every table has a filter box above each column.
4. **Metadata**: the FAIR letters, the metadata issues, and what the metadata says.
5. **Map**: positions with a problem in colour (errors first), the others in grey, the metadata
   area, and optionally tracks: positions in date order per cruise, animal, vessel ...
6. **Explore**: records over time against the metadata time span; the taxa with WoRMS details;
   the measurements per type and unit; histograms or box plots (per taxon) of any number; two measurements against each
   other with a crude model (power for length-weight, exponential, linear) that marks the odd
   records; males and females per taxon; the event levels.
7. **Download**: Excel report, CSV files (zip; comma or semicolon), JSON.

**Help** (top right): *Checks* (find a check by words and read everything about it), the
*Manual* (`inst/manual/manual.md`: getting started, every screen, R, maintenance, adding a
check, troubleshooting) and *About the tool* (version, disclaimer, contact, links; edit in
`inst/reference/about_the_tool.csv`).

If something goes wrong on a screen, that screen shows the message and the rest of the app keeps
working. The check runs inside the app with a progress bar; a dataset like FIFD takes
about 5 minutes and 13 GB of memory.

## Use it: in R

```r
# After installing (see Install). For development: devtools::load_all("qctool")
library(qctool)

# What can be checked: an archive (zip or folder), an IPT, OBIS or GBIF address, a link to a
# zip or a table, a table
result <- qc_check("Datasets/dwca-biofun_2009-v1.9.zip")
result <- qc_check("https://ipt.vliz.be/upload/resource?r=satellitetracking-turtles-mediterranean")
result <- qc_check("https://obis.org/dataset/bbc947ad-48c0-4235-8398-3c92f0fc994a")
result <- qc_check("https://example.org/data/survey.csv", mapping = c(Station = "locality"))
result <- qc_check("Datasets/DataMunida.csv")
# a table with your own column mapping (NA: leave the column out)
result <- qc_check("Datasets/DataMunida.csv", mapping = c(Day = NA, Station = "locality"))

# Results, from overview to detail
result                     # summary in the console
result$checks              # one row per check: status, issues, records affected
result$issues_grouped      # one row per identical problem, with example records
qc_issues(result)          # every issue with record ID, file and line
qc_issues(result, check = "no_placeholder_values", limit = 100)

# Save: Excel (Summary, Checks, Issues grouped, Issues, About the checks), or all as CSV
qc_export_xlsx(result, "biofun_qc.xlsx")
qc_export_csv(result, "biofun_qc")    # sep = ";" for Belgian or German Excel

# More
qc_suggest_mapping(c("Date", "Latitude", "species"))  # which term would each column get?
qc_check(..., checks = c("no_placeholder_values"))    # only some checks (+ the ones they need)
qc_check(..., progress = TRUE)                        # progress bar
qc_check(..., online = FALSE)                         # no internet: stored answers only
qc_clear_cache()                                      # forget the stored WoRMS/NERC/OBIS answers

# The summaries the app shows (also at the top of the Excel report)
qc_essentials(result)        # % occurrences OK: taxonomy, position, date, occurrenceStatus
qc_fair(result)              # metadata checks per FAIR letter
qc_record(result, "occurrence", 12)   # one record: values, issues, events, measurements
qc_map_points(result)        # positions for a map
qc_time_summary(result)      # records over time vs the metadata time span
qc_taxa_summary(result)      # records per taxon, WoRMS details, issues
qc_emof_summary(result)      # measurements per type and unit
qc_event_levels(result)      # the event hierarchy per level
qc_relationship(result, "emof:1", "emof:2", "power")   # a crude fit, the odd records
qc_sex_ratio(result)         # sexes per taxon
qc_tracks(result, "organismID")       # tracks in date order
qc_links(result)             # broken links: missing events, occurrences, parents; doubled IDs
qc_one_to_one(result)        # values and their IDs: one name with two IDs, one typeID with two texts, ...
qc_tables(result)            # the tables and how they are linked
qc_checks("NERC label")      # find a check by words
qc_map(result)               # map of the positions with problems (Viewer)
qc_map(result, "map.png")    # ... as a picture (.png / .pdf) or web page (.html)
```

**Change how strict a check is**: column *severity* in the sheet *Checks* of
`inst/rules/qc_rules.xlsx` (error / warning / info / off). The app and `qc_check()` read the
workbook at every check; manual section 7.1. (Help > About the tool shows the path and offers
the workbook for download.)

**Online lookups (WoRMS, NERC, OBIS).** 16 checks look things up: AphiaIDs and names in
WoRMS, measurement terms and units in the NERC Vocabulary Server, distance to the coast and
sea floor depth at OBIS (xylookup). Each different value is asked once, in batches, and the
answer is stored on disk for 30 days (`tools::R_user_dir("qctool", "cache")`;
`options(qctool.cache_dir = , qctool.cache_days = )` to change). Without internet, or with
`online = FALSE`, the stored answers are used; a check that needs more is *not performed*
(never an error in the data, never a pass either): the verdict then says *Not complete* when
that check could block the harvest, the checks that did not run are listed right under it, and
an essential whose checks did not run is grey. A service that does not answer
is tried 3 times; the answers that did come are kept, so the next run only asks for the rest.
Without internet, `points_on_land` uses the coarse coastline of the `maps` package and says
"approximate" (then `marine_taxon_on_land` gives warnings instead of errors). A first run of a
DNA dataset with thousands of names without scientificNameID takes a few minutes (WoRMS name
matching is slow); the next runs take seconds.

**About big datasets.** Identical problems are stored once with the row numbers of the records
(FIFD: 40 million issues = 8 problems, 0.3 GB). The detail per record is built only when asked
for. The Excel sheets 'Issues grouped' and 'Issues' show at most the 100,000 most important rows
(`max_issue_rows` to change; the Summary says when rows were left out); the CSV export always
has all of them, compressed as `issues.csv.gz` when Excel could not open it anyway.

**Writing checks that stay fast on 34 million rows.** Work on the *different* values of a
column, not on every value: `per_value(x, f)`, `rows_where()`, `as_number()`, `lower_trim()`,
`nerc_code()` all do this. One `trimws()` or `tolower()` over a whole eMoF column takes
minutes; over its few different values, a second. Group by numbers (`value_codes()`), not by text.

## Change the rules: `inst/rules/qc_rules.xlsx`

Open the workbook in Excel, change, save. The next `qc_check()` uses the new rules.

| Sheet | Column | Values |
|---|---|---|
| Checks | `severity` | error / warning / info / off |
| Checks | `settings` | e.g. `buffer_m = 3000; min_year = 1600` |
| Fields | `requirement` | required / recommended / optional |

Do not change `check_id` (it links the row to the code). If the workbook contains something
the tool does not understand, `qc_check()` stops and names the sheet, row and column.
`qc_read_rules()` shows what the tool reads.

Other lists the tool uses are CSV files in `inst/reference/`: `dwc_terms.csv` (the EurOBIS
term table: types, database column sizes, tables), `placeholders.csv`, `term_synonyms.csv`
(column names like "Latitude" or "species" and the term they stand for), `unit_synonyms.csv`
(other accepted ways to write a NERC unit, e.g. "individuals" for Dimensionless, "mg/g" for
Grams per kilogram).

## Add a check

The manual (section 7.3) explains it step by step. In short:

0. `source("qctool/dev/new_check.R"); new_check("my_check", group = "dates", ...)` prints the
   three pieces below, ready to copy.
1. Add a row to the sheet *Checks* of `qc_rules.xlsx` (check_id, title, what, why, severity, depends_on).
2. Write `check_<check_id>()` in the file of its group, `R/checks_NN_<group>.R`.
   Copy a similar check. A check gets `(dataset, settings)` and returns `new_issues(...)`
   or `empty_issues()`.
3. Add a test with one record that passes and one that fails: `tests/testthat/test-checks_NN_<group>.R`.
4. Run `devtools::test("qctool")` and `Rscript dev/stress_app_functions.R`;
   then `Rscript dev/write_check_catalogue.R` (the manual's appendix).

The tool refuses to run when a `check_...()` function has no row in the workbook, so the two
cannot drift apart. A row without a function shows up as *not yet implemented*.

## Where things are

| File | What |
|---|---|
| `R/01_qc_check.R` | **start here**: `qc_check()` |
| `R/02_engine.R` | order of checks, statuses, what happens when a check fails |
| `R/03_issues.R` | how a check reports a problem; compact storage; record ID + file + line |
| `R/04_result.R` | the result, `qc_issues()` |
| `R/05_rules.R` | reading `qc_rules.xlsx` |
| `R/08_options.R` | the settings of the tool itself, in one place: internet, stored answers, size limits (download, unzipped, upload), server safety |
| `R/06_dataset.R` | what a dataset looks like inside the tool |
| `R/07_reference.R` | the reference tables in `inst/reference/` |
| `R/10_read_dwca.R` | reading a Darwin Core Archive (meta.xml decides) |
| `R/11_read_table.R`, `R/12_map_columns.R` | reading CSV/Excel tables, column mapping |
| `R/13_read_eml.R` | reading the metadata into plain lists (`dataset$metadata`) |
| `R/14_read_url.R` | web addresses: IPT, GBIF and OBIS, links to archives and tables (what it is: from the file itself) |
| `R/20_prepare.R` | links between tables, values inherited from parent events, `setting()` |
| `R/30_lookup.R` | online lookups: cache, offline mode, retries (shared by the next three) |
| `R/31_lookup_worms.R`, `R/32_lookup_nerc.R`, `R/33_lookup_xy.R` | WoRMS, NERC Vocabulary Server, OBIS xylookup (+ offline coastline) |
| `R/40_helpers.R` | small helpers: ISO 8601 dates, numbers, "test each value once" |
| `R/45_summaries.R` | essentials, FAIR summary, one record, map points (`inst/reference/fair_checks.csv`) |
| `R/46_explore.R`, `R/47_explore_more.R` | the Explore screen: time, taxa, measurements, events; distributions, relationships, sexes, tracks |
| `R/49_tables.R` | the tables of a dataset, how they are linked, and a search in their rows (Explore > Tables) |
| `R/09_check_catalogue.R` | `qc_checks()`: the check catalogue with search |
| `R/60_app.R` ... `R/69_app_tables.R` | the app: one file per screen (Shiny modules) |
| `inst/manual/` | the manual and the check catalogue (appendix) |
| `dev/new_check.R`, `dev/write_check_catalogue.R` | start a new check; write the catalogue appendix |
| `dev/stress_app_functions.R` | runs every function the app uses on every test dataset and lists failures |
| `R/checks_01_structure.R` ... `R/checks_15_one_to_one.R` | the checks, one file per group (NERC checks: `checks_09_emof_nerc.R`; values and their IDs: `checks_15_one_to_one.R`) |
| `tests/testthat/helper-http.R` | tests of online checks replay recorded answers: no internet needed |
| `R/50_export_xlsx.R`, `R/51_export_csv_json.R` | saving results |
| `dev/run_test_datasets.R` | runs everything in `../Datasets` and saves reports to `../output` |
| `docs/design.md` | how the tool is built and why (the code refers to its sections) |
| `docs/storing_qc_requests.md` | for IT: how the checks people run could be stored (a request log, an API), not built yet |
