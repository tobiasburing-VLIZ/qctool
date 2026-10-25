# Test helper: a small dataset for the Explore screen and the map tracks.
# Ten fish of one species (lengths and weights following w = 0.01 l^3, one far
# off), with their sex, on two stations (S1, S2) of one cruise (C1).
explore_test_archive <- function() {
  lengths <- c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28)
  weights <- round(0.01 * lengths^3 * c(1.05, 0.96, 1.02, 0.97, 1, 1.04, 0.95, 1.03, 0.98, 1.01), 1)   # natural spread
  weights[5] <- weights[5] * 4                      # O5 is far off the curve
  ids <- paste0("O", 1:10)
  make_archive(
    event = data.frame(id = c("C1", "S1", "S2"), eventID = c("C1", "S1", "S2"), parentEventID = c(NA, "C1", "C1"),
                       eventDate = c("2020-05", "2020-05-01", "2020-05-03"),
                       decimalLatitude = c(NA, "51.2345", "51.4567"), decimalLongitude = c(NA, "2.9123", "3.1234")),
    occurrence = data.frame(id = rep(c("S1", "S2"), each = 5), eventID = rep(c("S1", "S2"), each = 5), occurrenceID = ids,
                            scientificName = "Gadus morhua", occurrenceStatus = "present",
                            sex = rep(c("female", "male", "F", "M", "unknown"), 2)),
    emof = data.frame(id = rep(rep(c("S1", "S2"), each = 5), 2), occurrenceID = rep(ids, 2),
                      measurementType = rep(c("Length", "Weight"), each = 10), measurementUnit = rep(c("cm", "g"), each = 10),
                      measurementValue = as.character(c(lengths, weights))))
}
