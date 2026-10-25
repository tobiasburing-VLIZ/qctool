# duplicates, other, metadata, outliers, DNA

test_that("duplicate_occurrences compares inherited fields and eMoF values", {
  dataset <- qc_read_dwca(make_archive(
    event = data.frame(id = "E1", eventID = "E1", eventDate = "2020-05-01", decimalLatitude = "51", decimalLongitude = "3"),
    occurrence = data.frame(id = "E1", occurrenceID = c("O1", "O2", "O3"), scientificName = "Abra alba"),
    emof = data.frame(id = "E1", occurrenceID = c("O1", "O2", "O3"), measurementType = "sex",
                      measurementTypeID = "http://vocab.nerc.ac.uk/collection/P01/current/ENTSEX01/",
                      measurementValue = c("male", "male", "female"))))
  issues <- run_check("duplicate_occurrences", dataset)
  expect_equal(issues$record_id, c("O1", "O2"))     # O3 differs in sex
  expect_match(issues$value[1], "group of 2, first: O1")
})

test_that("measured individuals (length or weight in the eMoF) are not duplicates", {
  # Three fish of one species, same length and sex, in one haul (as in FIFD); O4 and O5 have no size.
  dataset <- qc_read_dwca(make_archive(
    event = data.frame(id = "E1", eventID = "E1", eventDate = "2020-05-01"),
    occurrence = data.frame(id = "E1", occurrenceID = paste0("O", 1:5), scientificName = "Merluccius hubbsi"),
    emof = data.frame(id = "E1", occurrenceID = c("O1", "O1", "O2", "O2", "O3", "O3", "O4", "O5"),
                      measurementType = c("Length", "Sex", "Length", "Sex", "Length", "Sex", "Sex", "Sex"),
                      measurementValue = c("28", "F", "28", "F", "28", "F", "M", "M"))))
  expect_equal(measured_individuals(dataset), 1:3)
  expect_equal(run_check("duplicate_occurrences", dataset)$record_id, c("O4", "O5"))
})

test_that("size measurements are recognised by code and by word, also with underscores", {
  emof <- data.table(measurementType = c("Dorsal_mantle_length", "Weight", "Sex", "biomass", NA),
                     measurementTypeID = c(NA, NA, NA, NA, "http://vocab.nerc.ac.uk/collection/P01/current/OBSINDLX/"))
  expect_equal(is_size_measurement(emof), c(TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_equal(is_size_measurement(emof, kinds = c("length", "width")), c(TRUE, FALSE, FALSE, FALSE, TRUE))
})

test_that("DNA: different sequences of the same taxon in one sample are not duplicates", {
  dataset <- qc_read_dwca(make_archive(
    occurrence = data.frame(id = c("A1", "A2"), occurrenceID = c("A1", "A2"), eventID = "S1", scientificName = "Bacillariophyta"),
    dna = data.frame(id = c("A1", "A2"), DNA_sequence = c("ACGT", "ACGA"))))
  expect_equal(nrow(run_check("duplicate_occurrences", dataset)), 0)
})

test_that("eMoF duplicates: same measurement, same type with different values", {
  dataset <- qc_read_dwca(make_archive(
    event = data.frame(id = "E1", eventID = "E1"),
    occurrence = data.frame(id = "E1", occurrenceID = "O1"),
    emof = data.frame(id = "E1", occurrenceID = "O1", measurementID = c("M1", "M2", "M3", "M4"),
                      measurementType = c("length", "length", "weight", "weight"), measurementValue = c("12", "12", "3", "4"))))
  expect_equal(run_check("duplicate_emof_records", dataset)$line, 3L)
  expect_equal(run_check("duplicate_type_per_occurrence", dataset)$line, c(4L, 5L))
})

test_that("dynamicProperties JSON, individualCount, organismQuantity pairs", {
  dataset <- qc_read_dwca(make_archive(occurrence = data.frame(
    id = c("O1", "O2"), occurrenceID = c("O1", "O2"), dynamicProperties = c('{"a":"1"}', "a=1; b=2"),
    individualCount = c("3", "2.0"), organismQuantity = c("10", NA), organismQuantityType = c(NA, NA))))
  expect_equal(run_check("dynamicproperties_json", dataset)$record_id, "O2")
  expect_equal(run_check("individualcount_valid", dataset)$suggestion, "2")
  expect_equal(run_check("organismquantity_with_type", dataset)$record_id, "O1")
})

test_that("metadata checks read dataset$metadata", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(id = "E1", eventID = "E1"), eml_title = "SHORT TITLE"))
  expect_equal(nrow(run_check("eml_title_filled", dataset)), 0)
  expect_match(run_check("eml_title_informative", dataset)$message, "fewer than 5 words and is in capitals only")
  expect_equal(nrow(run_check("eml_abstract_filled", dataset)), 1)
  expect_equal(nrow(run_check("eml_contact_filled", dataset)), 1)
  expect_equal(run_check("eml_license_valid", dataset)$message, "The metadata has no licence")

  no_eml <- qc_read_dwca(make_archive(event = data.frame(id = "E1", eventID = "E1"), with_eml = FALSE))
  expect_equal(nrow(run_check("eml_present", no_eml)), 1)
  expect_error(run_check("eml_title_filled", no_eml), class = "qc_not_applicable")
})

test_that("a complete EML passes the FAIR checks", {
  folder <- make_archive(event = data.frame(id = "E1", eventID = "E1"))
  writeLines(example_eml, file.path(folder, "eml.xml"))
  dataset <- qc_read_dwca(folder)
  for (check_id in c("eml_contact_filled", "eml_creator_filled", "eml_metadata_provider_filled", "eml_license_valid",
                     "eml_citation_filled", "eml_geographic_coverage_filled", "eml_temporal_coverage_filled",
                     "eml_taxonomic_coverage_filled", "eml_persistent_identifier",
                     "eml_methods_described", "eml_language_given", "eml_no_placeholders", "ipt_url_length")) {
    expect_equal(nrow(run_check(check_id, dataset)), 0, label = check_id)
  }
  expect_equal(run_check("eml_people_have_orcid", dataset)$value, "Contact (contact)")
  # Only the keywords the IPT adds itself (GBIF dataset type): no themes.
  expect_equal(nrow(run_check("eml_keywords_filled", dataset)), 1)
})

test_that("eml_no_placeholders finds '-----' and 'In press'", {
  folder <- make_archive(event = data.frame(id = "E1", eventID = "E1"))
  writeLines('<eml><dataset><title>A title</title><creator><organizationName>-----</organizationName></creator><abstract><para>Data in Brief, In Press</para></abstract></dataset></eml>',
             file.path(folder, "eml.xml"))
  issues <- run_check("eml_no_placeholders", qc_read_dwca(folder))
  expect_equal(issues$value, c("-----", "Data in Brief, In Press"))
})

test_that("outliers: a far position, a far date, an unusual value", {
  set.seed(1)
  n <- 40
  lat <- c(sprintf("%.3f", 51 + runif(n - 1)), "12.5")
  lon <- c(sprintf("%.3f", 3 + runif(n - 1)), "3.5")
  dates <- c(format(as.Date("2010-01-01") + sample(0:3650, n - 1)), "1898-05-01")
  dataset <- qc_read_dwca(make_archive(
    event = data.frame(id = paste0("E", 1:n), eventID = paste0("E", 1:n), decimalLatitude = lat, decimalLongitude = lon, eventDate = dates),
    occurrence = data.frame(id = paste0("E", 1:n), occurrenceID = paste0("O", 1:n), scientificName = "Abra alba"),
    emof = data.frame(id = paste0("E", 1:n), occurrenceID = paste0("O", 1:n), measurementType = "length", measurementUnit = "mm",
                      measurementValue = c(sprintf("%.1f", 10 + runif(n - 1) * 5), "1200"))))
  expect_equal(run_check("outlier_location_dataset", dataset)$record_id, paste0("E", n))
  expect_equal(run_check("outlier_date_dataset", dataset)$record_id, paste0("E", n))
  expect_equal(run_check("outlier_measurement_value", dataset)$value, "1200")
  expect_equal(run_check("outlier_size_per_species", dataset)$value, "1200")
  expect_equal(run_check("outlier_species_location", dataset)$record_id, paste0("O", n))
  expect_error(run_check("outlier_depth_dataset", dataset), class = "qc_not_applicable")
})

test_that("DNA: extension expected, fields complete, sequences valid", {
  without_extension <- qc_read_dwca(make_archive(occurrence = data.frame(
    id = "O1", occurrenceID = "O1", organismQuantity = "45", organismQuantityType = "DNA sequence reads")))
  expect_equal(nrow(run_check("dna_extension_present", without_extension)), 1)

  with_extension <- qc_read_dwca(make_archive(
    occurrence = data.frame(id = c("O1", "O2"), occurrenceID = c("O1", "O2"), associatedSequences = c("x", "y")),
    dna = data.frame(id = c("O1", "O2"), target_gene = c("18S rRNA", NA), DNA_sequence = c("ACGTN", "ACG T-x"))))
  expect_equal(nrow(run_check("dna_extension_present", with_extension)), 0)
  complete <- run_check("dna_fields_complete", with_extension)
  expect_equal(complete[field == "target_gene", record_id], "O2")
  expect_true(all(complete[field != "target_gene", severity_hint] == "warning"))
  expect_equal(run_check("dna_sequence_valid", with_extension)$record_id, "O2")
})

test_that("duplicate_type_per_occurrence also works in an occurrence core whose eMoF has no occurrenceID", {
  dataset <- qc_read_dwca(make_archive(
    occurrence = data.frame(id = c("O1", "O2"), occurrenceID = c("O1", "O2"), scientificName = "Abra alba"),
    emof = data.frame(id = c("O1", "O1", "O2"), measurementType = "length", measurementValue = c("3", "4", "5"))))
  expect_equal(run_check("duplicate_type_per_occurrence", dataset)$record_id, c("O1", "O1"))
  expect_equal(nrow(run_check("duplicate_type_per_event", dataset)), 0)    # they belong to occurrences, not events
})

test_that("duplicate_emof_records: a different measurementValueID makes a different measurement", {
  dataset <- qc_read_dwca(make_archive(
    event = data.frame(id = "E1", eventID = "E1"),
    occurrence = data.frame(id = "E1", occurrenceID = "O1", scientificName = "Pandalus borealis"),
    emof = data.frame(id = "E1", occurrenceID = "O1", measurementType = "fatty acid", measurementValue = "0.5",
                      measurementValueID = c("http://vocab.nerc.ac.uk/collection/S27/current/CS000001/",
                                             "http://vocab.nerc.ac.uk/collection/S27/current/CS000002/"),
                      measurementRemarks = c("a", "b"))))
  expect_equal(nrow(run_check("duplicate_emof_records", dataset)), 0)
})

test_that("organismquantity_use: in DNA datasets sampleSizeUnit is 'DNA sequence reads'", {
  dataset <- qc_read_dwca(make_archive(
    occurrence = data.frame(id = c("O1", "O2"), occurrenceID = c("O1", "O2"), associatedSequences = "x",
                            sampleSizeValue = c("1000", "2"), sampleSizeUnit = c("DNA sequence reads", "litre"))))
  issues <- run_check("organismquantity_use", dataset)
  expect_equal(issues$record_id, "O2")
  expect_equal(issues$field, "sampleSizeUnit")
})
