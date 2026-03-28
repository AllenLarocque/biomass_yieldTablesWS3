mock_cap <- function() {
  mock_speciesEcoregion()[, .(globalMaxANPP = max(maxANPP)), by = speciesCode]
}

test_that("binSiteQuality returns data.table with site_quality column", {
  result <- binSiteQuality(mock_cohortData(), mock_speciesEcoregion(), mock_cap())
  expect_s3_class(result, "data.table")
  expect_true("site_quality" %in% names(result))
})

test_that("binSiteQuality site_quality values are only low/med/high", {
  result <- binSiteQuality(mock_cohortData(), mock_speciesEcoregion(), mock_cap())
  expect_true(all(result$site_quality %in% c("low", "med", "high")))
})

test_that("binSiteQuality preserves input row count", {
  cd     <- mock_cohortData()
  result <- binSiteQuality(cd, mock_speciesEcoregion(), mock_cap())
  expect_equal(nrow(result), nrow(cd))
})

test_that("binSiteQuality does not leave maxANPP or globalMaxANPP columns in output", {
  result <- binSiteQuality(mock_cohortData(), mock_speciesEcoregion(), mock_cap())
  expect_false("maxANPP"       %in% names(result))
  expect_false("globalMaxANPP" %in% names(result))
  expect_false("ratio"         %in% names(result))
})

test_that("binSiteQuality assigns 'low' with warning for unrecognised ecoregion", {
  cd <- copy(mock_cohortData())
  cd[1L, ecoregionGroup := "UNKNOWN_ECO"]
  expect_warning(
    result <- binSiteQuality(cd, mock_speciesEcoregion(), mock_cap()),
    regexp = "unrecognised ecoregion"
  )
  # merge() may reorder rows; look up by content, not position
  expect_equal(result[ecoregionGroup == "UNKNOWN_ECO", site_quality], "low")
})

test_that("binSiteQuality assigns 'high' when ratio = 1 with default bins", {
  se  <- copy(mock_speciesEcoregion())
  cap <- se[, .(globalMaxANPP = maxANPP[1]), by = speciesCode]  # cap == local maxANPP → ratio = 1
  result <- binSiteQuality(mock_cohortData(), se, cap, bins = c(0.33, 0.67))
  expect_true(all(result$site_quality == "high"))
})

test_that("binSiteQuality assigns 'low' when ratio near 0 with default bins", {
  se  <- copy(mock_speciesEcoregion())
  # make globalMaxANPP very large so ratio → 0
  cap <- data.table(speciesCode = c("Pice_mar", "Pice_gla"), globalMaxANPP = c(1e6, 1e6))
  result <- binSiteQuality(mock_cohortData(), se, cap, bins = c(0.33, 0.67))
  expect_true(all(result$site_quality == "low"))
})
