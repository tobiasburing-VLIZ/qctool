# QC Tool (Beta): design

How the tool is built and why. For maintainers; users read the manual (`inst/manual/manual.md`). The code refers to the sections of this document (e.g. "docs/design.md, section 4.7"); keep their numbers when you change it.

Status: version 0.0.2, 2026-10-25.

---

## 1. Decisions

| Topic | Decision |
|---|---|
| Language | R only. The data management team works in R and must be able to change checks without waiting for IT. A check may use faster code inside its own function if ever needed; so far nothing needs it (section 8). |
| Start | A new package, `qctool`. Texts, lists and ideas come from EMODnetBiocheck (the reference), BioCheckTNG (Go) and obistools; their code is not reused. |
| Rules | **One set of rules, for EMODnet Biology / EurOBIS.** No profiles. Each check has one severity: by default the strictest of GBIF, OBIS and EMODnet; how GBIF and OBIS differ is text (`other_standards`). The DM team can change every severity. |
| Where rules live | `inst/rules/qc_rules.xlsx` (severity, settings, required fields, field lengths), read at every check. Word lists in `inst/reference/*.csv`. Nothing the DM team may want to change is hard-coded. |
| Essentials | Taxonomy, position, date and occurrenceStatus are what every EurOBIS record needs. They get their own panel (section 4.10). |
| Traceability | Every issue points to a record (its ID and the line in the file), or to a column, file or metadata element (section 4.7). No cap on the number of issues. |
| Never stop halfway | Every check ends with a status; a failing check never stops the others (section 4.4). |
| Missing results are visible | A check that did not run (no internet, a service down, a fault) is never shown as passed: the verdict says *Not complete* when such a check could block the harvest, and lists the checks that did not run. |
| On land | 3 km coastal buffer. A position on land is a warning; a marine-only taxon on land is an error, but only a warning with the coarse offline coastline. |
| Outliers | Cheap, explained in the message, never blocking (section 4.9). |
| Metadata | A basic FAIR check of the EML, with a summary per FAIR letter (section 4.10). |
| The data is never changed | The tool reports and suggests; corrections stay a human decision. Exception: text that is not valid UTF-8 is read as Windows-1252 so that the checks can run, and is reported. |

Decisions taken while testing (kept here so they are not undone by accident):
- Dates with a space instead of the "T" before the time (`2013-08-12 19:47:00`) are valid.
- Shortened intervals (`2011-07/09`) are errors: the harvester cannot read them. An interval whose end is only a time (`2019-05-21T10:00/12:00`) is a warning.
- occurrenceStatus *detected* and *notDetected* are accepted (EMODnet only).
- occurrenceID only has to be unique within the dataset; running numbers are fine.
- Licences: CC0, CC-BY, CC-BY-NC, also as short names (*CC-BY*, *cc by 4.0*, a plain *CC*).
- `modified` (dcterms:modified) missing or empty: info.
- Duplicate occurrences: records of one measured individual (a length or width in the eMoF) are not compared. Several fish of one species with the same length in one haul are normal.
- `type_unit_compatible`: a unit of the same kind as the one NERC links (cm where NERC links mm) is only info.

---

## 2. Test datasets

`Datasets/` next to the package (not part of it):

| Dataset | Structure | Shows |
|---|---|---|
| biofun 2009 | event core, 407 occurrences, 1,371 eMoF | the OBIS training dataset; small problems everywhere |
| FIFD | event core, 43k events, 8.2M occurrences, 33.8M eMoF | speed and memory (section 8) |
| hydrozoa BIOICE | event hierarchy | values only on parent events: inheritance |
| turtles (satellite tracking) | event core, 216k eMoF | 0–360 longitudes, deprecated terms, typos, EML placeholders |
| northern shrimp | event core, 471k eMoF | value IDs, life stages, material entities |
| Wallhamn 18S | occurrence core + DNA extension | DNA fields, missing scientificNameID and occurrenceStatus |
| three MGnify archives | event core, no header row | headerless files, HTML codes in text, no dates or positions |
| Munida (CSV), Oyster (Excel) | simple tables | column mapping, Excel dates and numbers, Windows-1252 text |

In a Darwin Core Archive the link from an extension to the core is the **id column** (core id), not an eventID or occurrenceID column. Several test archives have no eventID or occurrenceID in the eMoF at all. The tool always uses the id link.

---

## 3. Big picture

```
 input                    read                    prepare                 run checks                result
 DwC-A zip / folder  ─┐                           • core-id links         in dependency order:     • run info, verdict
 IPT/GBIF/OBIS URL,   ├─► tables (all text)   ─►  • inherited values  ─►  applicable? online?  ─►  • check statuses
 link to zip or table │   + meta.xml + EML          (on demand)           run → issues / status    • problems + rows
 CSV / TXT / Excel   ─┘   (+ column mapping)                                                        ─► app · xlsx · csv · json
                                                       ▲
                                     lookups: WoRMS · NERC · OBIS xylookup
                                     (memory → disk cache → internet → "not performed")
```

One function does it all, for scripts and for the app: `qc_check()` (`R/01_qc_check.R`).

---

## 4. The parts

### 4.1 Reading (`R/10_read_dwca.R` … `R/14_read_url.R`)

- **meta.xml is the authority** for an archive: file names, separators, quotes, header lines, encoding, and which column is which term. Headerless files work.
- **Web addresses:** an IPT resource page becomes its archive address; GBIF and OBIS dataset pages are resolved through their APIs. Any other link is downloaded as it is. What was downloaded is told from the file itself (a zip with meta.xml, an Excel file, plain text), not from the address. The size is checked before and during the download.
- **Tables** (CSV, TXT, Excel) get a proposed term per column (`R/12_map_columns.R`), which the user confirms. Excel dates become ISO 8601.
- **Everything is read as text.** Checks decide what a valid number or date is; nothing is silently converted. Empty cells become `NA`; spaces, placeholders and capitals stay, so the checks can see them.
- **Text that is not valid UTF-8** (e.g. a CSV saved as Windows-1252 by Excel) is read as Windows-1252 right after reading, and its place is kept in `dataset$invalid_text` for `text_encoding_valid`. Otherwise R's text functions fail on it and checks would break one after the other.
- Each table has two internal columns: `.line` (the line in the original file) and `.coreid` (the link to the core).

### 4.2 Inheritance, and where a problem is reported (`R/20_prepare.R`)

In an event hierarchy (cruise → station → sample), dates and positions are often only on a parent. `inherited_value()` gives every record its own value, or else the value of the nearest parent that has one, plus where that value is written. The tables themselves are not changed. Which fields inherit: `inst/reference/inheritable_fields.csv`.

**Report each problem once, where it can be fixed:**
- *Is the value there?* (required fields) is judged on the inherited value: an occurrence passes when its event has the date. A missing value is reported on the event that should have it.
- *Is the value right?* (format, range) is checked where the value is written: a wrong date on a cruise is reported once, on that event, not on its 50,000 occurrences.

### 4.3 A check (`R/checks_NN_<group>.R`)

One check is one function `check_<check_id>(dataset, settings)` in the file of its group, with a fixed header (What / Why / How / Settings). It returns `new_issues(...)` or `empty_issues()`; `require_fields()` and `skip_check()` make it *not applicable*, `not_performed()` makes it *not performed*. A check does not decide its severity, its dependencies or what happens when it fails: the engine does. **Only check functions may start with `check_`**: the engine finds checks by that name and stops when one has no row in the rules workbook.

### 4.4 The engine (`R/02_engine.R`)

1. Plan: the checks of the rules workbook (or the chosen ones plus their dependencies), in dependency order.
2. For each check: switched off? not implemented? a dependency that could not run? Otherwise run it inside `tryCatch`.
3. Every check ends with one status: *issues found*, *passed*, *not applicable*, *not performed*, *skipped*, *check error*, *not yet implemented*, *switched off*.

Rules:
- **Never stop halfway.** A check error is a status; the next check runs.
- **Dependencies decide the order**, not "stop when issues are found": if `coordinates_numeric` finds 3 bad values, `coordinates_in_range` still checks the others. A dependent check is skipped only when its dependency could not run at all.
- **Severity comes from the rules workbook.** A check may only *lower* the severity of single issues (`severity_hint`), e.g. a name that differs by up to 5 letters from the WoRMS name is a warning.
- **No cap** on the number of issues.

### 4.5 Online lookups (`R/30_lookup.R` … `R/33_lookup_xy.R`)

`values → each different value once → memory → disk cache (with its date) → internet (batched, parallel) → not found / not performed`

| Lookup | Source | Offline |
|---|---|---|
| WoRMS | REST: AphiaRecordsByAphiaIDs (50 per call), AphiaRecordsByMatchNames (25 per call, slow) | stored answers only |
| NERC Vocabulary Server | SPARQL, GET, 40 terms per call: label, alternative labels, deprecated, replacement, linked units, unit dimension | stored answers only |
| Land | OBIS xylookup, distance to the coast (5,000 positions per call) | coarse coastline of the `maps` package, marked "approximate" |
| Sea floor depth | OBIS xylookup, bathymetry | stored answers only |

- Answers are stored per service for 30 days (`R/08_options.R`); writes are atomic.
- **An internet problem is never an error in the data:** the check is *not performed*, with the reason. Each request is tried 3 times; answers that did come are kept, so the next run only asks for the rest. A failed service is not asked again in the same run.
- Tests replay recorded answers (`tests/testthat/helper-http.R`) and never use the internet.

### 4.6 The rules workbook (`R/05_rules.R`)

`inst/rules/qc_rules.xlsx`, read at every check and validated (a wrong value stops the tool with the sheet, row and column):
- sheet *Checks*: one row per check: severity (error / warning / info / off), settings (`name = value; ...`), texts, dependencies;
- sheet *Fields*: required / recommended / optional per term (`applies_to` = all or dna-derived);
- sheet *Field lengths*: the longest value per term (database size: error; current harvester limit: warning).

Edit it in Excel. When a script rebuilds it, rebuild the whole workbook from its sheets as data frames and check that Excel opens it: removing and re-adding Excel tables once crossed their links.

### 4.7 The result: every issue traceable to a record (`R/03_issues.R`, `R/04_result.R`)

**No issue without a pointer.** Every issue carries two pointers:
1. **The record's own ID**: eventID, occurrenceID, measurementID. eMoF records rarely have one; they are identified by occurrenceID (or the core link) plus the measurementType.
2. **The file and the line** (for Excel: the row). This works also for records whose ID is empty or doubled, which are exactly the records with problems.

**Compact storage.** FIFD has 40 million issues that are a handful of identical problems. They are stored as one row per problem (table, field, value, message, suggestion) plus two integers per affected record. The full detail (ID, file, line) is built only when asked for: `qc_issues()`, the exports, the app.

**Reporting layers**, from overview to detail (the same in R, the exports and the app):

| Layer | One row per | Where |
|---|---|---|
| verdict, essentials, FAIR | dataset | `verdict()`, `qc_essentials()`, `qc_fair()`; Overview; Excel Summary |
| checks | check | `result$checks`: status, issues, records affected, reason |
| problems | identical problem | `result$issues_grouped`: "AphiaID not found: 2,300 records" |
| issues | issue × record | `qc_issues()`: every record, no cap |
| one record | record | `qc_record()`: its issues and those of its events, its values, its measurements |

### 4.8 Outputs (`R/50_export_xlsx.R`, `R/51_export_csv_json.R`)

| Format | Content |
|---|---|
| Excel | Summary (verdict, checks that did not run, essentials, FAIR, counts), Checks, Issues grouped, Issues (the 100,000 most important rows by default), About the checks |
| CSV | everything, no limit; `issues.csv.gz` above Excel's row limit; written in chunks |
| JSON | the whole result, for pipelines |

Not built yet: an annotated copy of the provider's own file (extra QC columns, coloured cells).

### 4.9 Outlier checks: cheap, explained, never blocking (`R/checks_13_outliers.R`)

Always warning or info: an outlier can be a real observation. Each message gives the numbers behind it. Skipped for small datasets (fewer than 20 values, or 10 per species).

| Method | Used for | How |
|---|---|---|
| 95 % range + margin | position, date, depth, species position | the range of the middle 95 % of the **different** values, widened by half its width (with a minimum margin); only what lies outside is flagged. A plain 95 % rule would always flag 5 %; the mean ± SD is pulled away by the very errors we look for |
| 3 × IQR on a log scale | measurement values per type and unit, sizes per species | Tukey's "far out" rule on log10 for positive, skewed quantities: a factor-10 error (mm vs cm) stands out |

### 4.10 Essentials and FAIR (`R/45_summaries.R`)

**Essentials:** per essential (taxonomy, position, date, occurrenceStatus) the share of occurrences without errors in its fields, also when the value comes from the event. Only problems about these fields count. Green: all fine; orange: at least 90 %; red: less; grey: not fully checked, because some of its checks did not run.

**FAIR:** the metadata checks per letter (`inst/reference/fair_checks.csv`), e.g. *F 6/9 · A 3/4 · I 7/8 · R 5/7*. Checks that did not run are not counted. A quick indication, not a certification. Neither summary adds rules of its own.

---

## 5. Where things are

See the table *Where things are* in `README.md` and section 7.8 of the manual. The numbers in the file names put them in reading order.

---

## 6. The app (`R/60_app.R` … `R/69_app_tables.R`)

Screens: 1 Load, 2 Overview, 3 Data issues, 4 Metadata, 5 Map, 6 Explore (the *visual checks*: time, taxa, measurements, distributions, relationships, sexes, events, tables), 7 Download, and Help (checks, manual, about). One Shiny module per screen; a screen that fails shows its own message and the rest keeps working (`safely()`).

**How it runs.** The check runs in the app's own R session, with a progress bar. A background worker would have to send a FIFD-size result (about 5 GB of data) back to the app, doubling the memory. Consequences for a server:
- one person per R process: with ShinyProxy each user gets an own container (the deployment plan); otherwise `qc_app(stop_on_close = FALSE)` and one big check makes the others wait;
- size limits: the options in `R/08_options.R` (download, unzipped size, upload);
- web addresses only `http(s)`; addresses on the server's own network can be refused (`qctool.block_private_addresses`).

Storing the checks people run (a request log) and a job API like BioCheckTNG's: `docs/storing_qc_requests.md`.

---

## 7. Milestones

| Milestone | Content | Status |
|---|---|---|
| 4a | readers, engine, rules workbook, issue storage, exports, inheritance, all offline checks | done |
| 4b | WoRMS, NERC and OBIS lookups with cache and offline mode | done |
| 5 | the app | done |
| review | pre-release review: encoding, verdict "not complete", web tables, options | done (0.0.2) |
| 6 | later checks (shown as *not yet implemented*), refined visuals | open |
| 7 | run history, comparison of versions, hosting | open |

---

## 8. Performance and memory, measured on FIFD

FIFD: 43k events, 8.2M occurrences, 33.8M eMoF; 162 MB zipped, 6.2 GB unzipped. Laptop with 16 cores and 32 GB.

| Step | Time |
|---|---|
| reading | 36–84 s |
| all 137 checks, online lookups included (first run, empty cache) | 234 s |
| Excel report | 5 s |
| CSV (452 MB issues.csv.gz, 65 million rows) | 79–83 s |
| **peak memory** | **13.4 GB** (4.7 GB of it the data) |

Memory follows the unzipped size, not the number of rows: about 0.25 GB plus 2.1 GB per GB unzipped. An idle app uses 0.25 GB.

What made it fast, and must stay so:
- work on the **different** values of a column, not on every value (`per_value()`, `rows_where()`, `as_number()`, `lower_trim()`): `trimws()` over 34 million values takes minutes, over its few different values a second;
- group by numbers (`value_codes()`), not by text;
- batch the online lookups on unique values (FIFD: 243 AphiaIDs, 33,190 positions);
- do not compare one measured individual with another in `duplicate_occurrences` (it flagged 6 million fish of the same length);
- store issues compactly (section 4.7) and cap the Excel sheets.

If memory becomes the limit, the biggest tables can move to an on-disk database (e.g. DuckDB) without changing the checks' contract.

---

## 9. What the tool does not do

- It never changes the data (except reading invalid text as Windows-1252, see 4.1). It suggests fixes; corrections stay a human decision.
- Checks on the EurOBIS database itself (duplicate IDs in the harvest, provider display names) stay in the harvest chain.
- No user accounts and no permanent storage of uploaded data (see `docs/storing_qc_requests.md` for how it could be added).

---

## 10. Open questions

- Severities that are stricter than EMODnetBiocheck (e.g. datasetName differs from the title, negative depth, marine taxon on land, the one-to-one checks, 12 metadata checks): confirmed for now; the DM team can change them in the workbook.
- The fitness-for-use label (A/B/C/F) of EMODnetBiocheck is not produced; to be decided whether EurOBIS still needs it.

---

## 11. Licence and distribution

- **Licence:** MIT (`LICENSE`, `LICENSE.md`), copyright Tobias Büring. Creative Commons licences are made for data and documents, not code; MIT is the usual choice for R packages (EMODnetBiocheck uses it too).
- **Now:** internal. Install from the GitHub repository (`remotes::install_github()`) or from a zip (`remotes::install_local()`).
- **At release:** a public repository plus r-universe, which builds the package for Windows, macOS and Linux the same day (`install.packages("qctool", repos = "https://<org>.r-universe.dev")`). The app on a VLIZ server (deployment plan: ShinyProxy).
- **Optional:** CRAN. Possible (all dependencies are on CRAN, tests run offline), but every update goes through a manual review, and the rules workbook would only change with releases. Users would then point `qc_check(rules = )` to their own copy.
