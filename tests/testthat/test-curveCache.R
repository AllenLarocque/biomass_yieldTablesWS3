test_that("devTypeTupleKey returns pipe-delimited string", {
  expect_equal(devTypeTupleKey("Pice_mar", "med", "ECO_1"), "Pice_mar|med|ECO_1")
})

test_that("devTypeTupleKey preserves exact values", {
  expect_equal(devTypeTupleKey("Abies_bal", "high", "BorealPlains_10"), "Abies_bal|high|BorealPlains_10")
})

test_that("devTypeTupleKey errors on NA speciesCode", {
  expect_error(devTypeTupleKey(NA_character_, "med", "ECO_1"))
})

test_that("devTypeTupleKey errors on NA site_quality", {
  expect_error(devTypeTupleKey("Pice_mar", NA_character_, "ECO_1"))
})

test_that("devTypeTupleKey errors on NA ecoregion", {
  expect_error(devTypeTupleKey("Pice_mar", "med", NA_character_))
})

test_that("devTypeTupleKey errors on vector input", {
  expect_error(devTypeTupleKey(c("Pice_mar", "Pice_gla"), "med", "ECO_1"))
})

test_that("diffDevTypes returns keys not in cache", {
  current <- c("A|low|ECO_1", "B|med|ECO_2", "C|high|ECO_1")
  cached  <- c("A|low|ECO_1")
  new     <- diffDevTypes(current, cached)
  expect_setequal(new, c("B|med|ECO_2", "C|high|ECO_1"))
})

test_that("diffDevTypes returns empty when all keys are cached", {
  current <- c("A|low|ECO_1")
  cached  <- c("A|low|ECO_1", "B|med|ECO_2")
  expect_length(diffDevTypes(current, cached), 0L)
})

test_that("diffDevTypes returns all keys when cache is empty", {
  current <- c("A|low|ECO_1", "B|med|ECO_2")
  expect_setequal(diffDevTypes(current, character(0)), current)
})

test_that("loadCurveCache returns empty list when file does not exist", {
  expect_equal(loadCurveCache(tempfile(fileext = ".rds")), list())
})

test_that("saveCurveCache + loadCurveCache round-trip preserves contents", {
  curves <- list(
    "Pice_mar|med|ECO_1" = data.frame(age = 0:5, vol_m3ha = c(0, 1, 3, 6, 8, 9))
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveCurveCache(curves, path)
  result <- loadCurveCache(path)
  expect_equal(result, curves)
})

test_that("saveCurveCache creates parent directories if needed", {
  curves <- list()
  path   <- file.path(tempdir(), "deep", "nested", "cache.rds")
  on.exit(unlink(dirname(dirname(path)), recursive = TRUE))
  expect_no_error(saveCurveCache(curves, path))
  expect_true(file.exists(path))
})
