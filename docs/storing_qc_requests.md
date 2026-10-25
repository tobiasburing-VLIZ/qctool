# Storing the QC requests: a design note for IT

Status: not built. This note says how the checks people run with the QC Tool could be kept, so the DM team and IT can see later who checked which dataset, with which rules, and what came out. It also prepares the version checks of milestone 7 (`record_count_not_decreased`, `ids_stable_between_versions`), which need the previous check of the same IPT resource.

## What the Go tool (BioCheckTNG) does

The Go tool keeps every request as a **job in memory**: `POST /jobs` starts a check and returns a job id, `GET /jobs/{id}/status` gives the progress, and `GET /jobs/{id}/result` gives the report. A job is deleted after one hour (`jobRetention` in `src/internal/server/server.go`). Nothing is written to disk or a database, so there is no history: after an hour, or a restart, a check is gone.

The QC Tool can keep the same idea (an id per request) and add what is missing: a permanent record.

## What one request record holds

Everything below already exists in a `qc_check()` result; nothing new has to be computed.

| Part | From | Notes |
|---|---|---|
| request id | new (a UUID) | also the folder name |
| when, how long | `result$run$checked_at`, `duration_s` | |
| who | the user name ShinyProxy passes in the HTTP header (`X-SP-UserId`); in R the system user | only if the DM team wants it (privacy, see below) |
| what was checked | `result$run$source`, `source_name`, `archive_url`, `ipt_url`, `dataset_title` | for uploads: the file name, not the file |
| which version of the data | `result$run$checksum_md5` | the same file gives the same checksum: repeated checks are recognised |
| which rules | `result$run$rules_file`, `tool_version` | add the MD5 of the rules workbook (one line: `tools::md5sum()`), so a report can be tied to the exact rules |
| how | `result$run$online`, the checks chosen | |
| verdict | `verdict(result)`: state, title, text, checks that did not run | |
| numbers | `severity_counts(result)`, `result$tables` (records per table) | |
| the results | `qc_export_json(result, file)` or `qc_export_csv(result, folder)` | the JSON holds run, tables, checks, problems and (below about a million) every issue |

## Where to keep it

**Option A, simplest: a folder per request on a shared volume.** On Kubernetes, a persistent volume mounted in every app container.

```
<log root>/2026/10/<request id>/request.json        the record above (small)
<log root>/2026/10/<request id>/result.json          qc_export_json()
<log root>/2026/10/<request id>/issues.csv.gz        only for big datasets (qc_export_csv)
```

- Every request writes only its own folder, so two users never write to the same file.
- An overview (a table of all requests) is built by reading the `request.json` files. That is fast enough for thousands of requests. Do not use one shared CSV that every request appends to, because two requests at the same moment can mix their lines.

**Option B: a database table plus files.**
- A PostgreSQL table `qc_request` with the columns of the record above, one row per request.
- The result files go to the volume (as in A) or to object storage. The table makes searching easy ("all checks of resource X", "everything that failed last month").
- This is the better choice once the request log feeds the EurOBIS harvest pipeline.

**Not recommended for keeping:** the R result object itself (`saveRDS(result)`). It contains the whole dataset (gigabytes for big datasets) and can only be read by R. The JSON and CSV exports are smaller, readable by any tool, and are what the app already offers for download.

## Where it plugs into the code

One new function, e.g. `qc_log_request(result, log_root)`, that writes the folder of option A (or inserts the row of option B). It is called in two places:

1. **The app:** in `R/61_app_load.R`, right after `result(checked)` in the observer of the button *Check the dataset*. Reading the ShinyProxy header: `session$request$HTTP_X_SP_USERID`.
2. **R / the harvest pipeline:** optionally at the end of `qc_check()` (`R/01_qc_check.R`), switched on by a new option `qctool.request_log` (the folder; `NULL` = off). Add it to `R/08_options.R` with the other settings of the tool.

```r
# Sketch, not part of the package yet.
qc_log_request <- function(result, log_root, user = NA_character_) {
  id <- uuid::UUIDgenerate()                    # or: paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample(1e6, 1))
  folder <- file.path(log_root, format(Sys.Date(), "%Y/%m"), id)
  dir.create(folder, recursive = TRUE)
  judged <- verdict(result)
  record <- c(list(request_id = id, user = user, verdict = judged$state, not_run = judged$not_run$check_id,
                   counts = as.list(severity_counts(result)), rules_md5 = unname(tools::md5sum(result$run$rules_file))),
              result$run)
  jsonlite::write_json(record, file.path(folder, "request.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
  qc_export_json(result, file.path(folder, "result.json"))
  invisible(id)
}
```

## Things to decide first

- **Privacy and retention.** Issues contain values from the data, also of datasets that are not published yet, and maybe people's names (recordedBy). Decide who may read the log, and how long it is kept (e.g. 1 year for published IPT resources, 90 days for uploads). Store the user only if it is needed.
- **Uploads.** Keep the uploaded file itself? Usually not: the checksum is enough to recognise it again, and the provider keeps the file.
- **Size.** The JSON of a small dataset is kilobytes. FIFD-sized results are hundreds of MB: write `issues.csv.gz` instead of the full issue list in JSON (`qc_export_json(..., include_detail = FALSE)`).

## Later: jobs and an API, like the Go tool

Today a check runs inside the user's app session, and the user waits for it. The same request id makes it possible to run checks in the background:

- **For several users on one server:** run `qc_check()` in a separate R process (packages `callr` or `mirai`). The app shows the progress by reading a small status file in the request folder (`status.json`: running / done / failed, percentage). With ShinyProxy (one container per user) this is not needed.
- **For other programs:** a small web API with the R package `plumber`, with the same three endpoints as the Go tool: `POST /jobs` (an address or a file; returns the request id), `GET /jobs/{id}/status`, `GET /jobs/{id}/result` (the `result.json` of the folder). The Go tool's OpenAPI description (`src/api/biochecktng-api.json`) can serve as the starting point, so existing clients keep working.
- **Version checks (milestone 7):** with the request log, the previous check of the same IPT resource is found by its `ipt_url`. Store the record counts and the event and occurrence IDs of every check (a compressed list, `ids.csv.gz`); the next check compares against them.
