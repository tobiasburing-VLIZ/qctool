# Tests never use the internet and never touch the user's cache: online lookups
# are off, except in tests that replay recorded answers (helper-http.R).
withr::local_options(qctool.online = FALSE, qctool.cache_dir = file.path(tempdir(), "qctool_test_cache"),
                     .local_envir = testthat::teardown_env())
