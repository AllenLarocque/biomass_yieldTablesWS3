test_that("simulateStand returns data.table with age and B_gm2 columns", {
  result <- simulateStand(
    speciesCode = "Pice_mar", site_quality = "med", ecoregion = "ECO_1",
    species = mock_species(), speciesEcoregion = mock_speciesEcoregion()
  )
  expect_s3_class(result, "data.table")
  expect_true(all(c("age", "B_gm2") %in% names(result)))
})

test_that("simulateStand first row is age 0 with B_gm2 = 0", {
  result <- simulateStand(
    speciesCode = "Pice_mar", site_quality = "med", ecoregion = "ECO_1",
    species = mock_species(), speciesEcoregion = mock_speciesEcoregion()
  )
  expect_equal(result$age[1], 0L)
  expect_equal(result$B_gm2[1], 0)
})

test_that("simulateStand B_gm2 is always non-negative", {
  result <- simulateStand(
    speciesCode = "Pice_mar", site_quality = "med", ecoregion = "ECO_1",
    species = mock_species(), speciesEcoregion = mock_speciesEcoregion()
  )
  expect_true(all(result$B_gm2 >= 0))
})

test_that("simulateStand handles factor speciesCode", {
  result <- simulateStand(
    speciesCode  = factor("Pice_mar"),
    site_quality = "med",
    ecoregion    = "ECO_1",
    species          = mock_species(),
    speciesEcoregion = mock_speciesEcoregion()
  )
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 1)
})

test_that("simulateStand handles factor ecoregion", {
  result <- simulateStand(
    speciesCode  = "Pice_mar",
    site_quality = "med",
    ecoregion    = factor("ECO_1"),
    species          = mock_species(),
    speciesEcoregion = mock_speciesEcoregion()
  )
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 1)
})

test_that("simulateStand respects maxAge cap", {
  result <- simulateStand(
    speciesCode = "Pice_mar", site_quality = "med", ecoregion = "ECO_1",
    species = mock_species(), speciesEcoregion = mock_speciesEcoregion(),
    maxAge = 50L
  )
  expect_true(max(result$age) <= 50L)
})

test_that("simulateStand runs for all age-class labels up to longevity without gaps", {
  result <- simulateStand(
    speciesCode = "Pice_mar", site_quality = "med", ecoregion = "ECO_1",
    species = mock_species(), speciesEcoregion = mock_speciesEcoregion()
  )
  # ages should be sequential from 0 to nrow-1 (unless B hits 0 early)
  expect_equal(result$age, seq(0L, nrow(result) - 1L))
})

test_that("simulateStand biomass peaks in the middle then declines", {
  result <- simulateStand(
    speciesCode = "Pice_mar", site_quality = "med", ecoregion = "ECO_1",
    species = mock_species(), speciesEcoregion = mock_speciesEcoregion()
  )
  peak_idx <- which.max(result$B_gm2)
  # peak should not be at the very start or very end
  expect_gt(peak_idx, 2L)
  expect_lt(peak_idx, nrow(result))
})

test_that("simulateStand errors on unknown speciesCode", {
  expect_error(
    simulateStand(
      speciesCode = "UNKNOWN_SPP", site_quality = "med", ecoregion = "ECO_1",
      species = mock_species(), speciesEcoregion = mock_speciesEcoregion()
    ),
    regexp = "not found in species table"
  )
})

test_that("simulateStand errors on unknown ecoregion", {
  expect_error(
    simulateStand(
      speciesCode = "Pice_mar", site_quality = "med", ecoregion = "UNKNOWN_ECO",
      species = mock_species(), speciesEcoregion = mock_speciesEcoregion()
    ),
    regexp = "no speciesEcoregion row"
  )
})

test_that("simulateStand errors when species is not a data.frame", {
  expect_error(
    simulateStand(
      speciesCode = "Pice_mar", site_quality = "med", ecoregion = "ECO_1",
      species = c("Pice_mar", "Pice_gla"),   # atomic vector, not a data.table
      speciesEcoregion = mock_speciesEcoregion()
    ),
    regexp = "must be a data.frame"
  )
})

test_that("simulateStand errors on maxAge <= 0", {
  expect_error(
    simulateStand(
      speciesCode = "Pice_mar", site_quality = "med", ecoregion = "ECO_1",
      species = mock_species(), speciesEcoregion = mock_speciesEcoregion(),
      maxAge = 0L
    )
  )
})

test_that("simulateStand higher maxB produces higher peak biomass (all else equal)", {
  # Construct two single-row speciesEcoregion tables differing only in maxB
  se_lo <- data.table(speciesCode = "Pice_mar", ecoregionGroup = "ECO_1",
                       maxANPP = 100, maxB = 5000)
  se_hi <- data.table(speciesCode = "Pice_mar", ecoregionGroup = "ECO_1",
                       maxANPP = 100, maxB = 15000)
  result_lo <- simulateStand("Pice_mar", "med", "ECO_1",
                              mock_species(), se_lo)
  result_hi <- simulateStand("Pice_mar", "med", "ECO_1",
                              mock_species(), se_hi)
  expect_gt(max(result_hi$B_gm2), max(result_lo$B_gm2))
})
