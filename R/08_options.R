# =============================================================================
# Options: settings of the tool itself, in one place
#
# How strict the CHECKS are is in the rules workbook (inst/rules/qc_rules.xlsx).
# The options below are about the machine the tool runs on: internet, the
# stored answers of online services, size limits and safety on a server.
# Change one with options() before qc_check() or qc_app(), in the R console,
# in .Rprofile, or in the start script of a server:
#
#   options(qctool.max_unzipped_mb = 1500, qctool.block_private_addresses = TRUE)
#
# option                          default   meaning
# qctool.online                   TRUE      use WoRMS, NERC and OBIS on the internet (FALSE: stored answers only)
# qctool.cache_dir                (*)       folder for the stored answers; (*) tools::R_user_dir("qctool", "cache")
# qctool.cache_days               30        after how many days a stored answer is asked again
# qctool.timeout                  60        seconds per internet request
# qctool.max_download_mb          2000      largest dataset downloaded from a web address (MB)
# qctool.max_unzipped_mb          Inf       largest archive after unzipping (MB); Inf = no limit.
#                                           A server open to everyone should set one (a small zip
#                                           can unpack to many GB)
# qctool.max_upload_mb            3000      largest file uploaded in the app (MB); also qc_app(max_upload_mb = )
# qctool.block_private_addresses  FALSE     TRUE: refuse web addresses on the server's own network
#                                           (localhost, 10.x, 192.168.x, ...). For a public server
#
# qc_app() has two more settings as arguments: port and stop_on_close (TRUE:
# the app stops when its browser tab is closed; FALSE when several people
# share one running app).
# The manual lists the options too (section 5, Options).
# =============================================================================

option_defaults <- list(
  qctool.online                  = TRUE,
  qctool.cache_dir               = NULL,   # NULL: tools::R_user_dir("qctool", "cache")
  qctool.cache_days              = 30,
  qctool.timeout                 = 60,
  qctool.max_download_mb         = 2000,
  qctool.max_unzipped_mb         = Inf,
  qctool.max_upload_mb           = 3000,
  qctool.block_private_addresses = FALSE
)

# The value of an option, or its default.
qc_option <- function(name) {
  if (!name %in% names(option_defaults)) stop("Unknown qctool option: ", name, call. = FALSE)
  value <- getOption(name)
  if (is.null(value)) value <- option_defaults[[name]]
  if (name == "qctool.cache_dir" && is.null(value)) value <- tools::R_user_dir("qctool", "cache")
  value
}
