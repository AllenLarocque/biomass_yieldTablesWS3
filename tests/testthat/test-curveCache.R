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

# ── inputs-aware cache (curveInputsKey, staleDevTypes) ──────────────────────

# A CanFI species / jurisdiction / ecozone present in both Boudewyn tables (black spruce, BC, ecozone 9)
mock_boudKeys <- function() list(canfi_species = 101L, juris_id = "BC", ecozone = 9L)

test_that("curveInputsKey is stable for identical inputs", {
  skip_if_no_boudewyn()
  k1 <- curveInputsKey("Pice_mar", "ECO_1", mock_species(), mock_speciesEcoregion(), 300L, mock_boudKeys())
  k2 <- curveInputsKey("Pice_mar", "ECO_1", mock_species(), mock_speciesEcoregion(), 300L, mock_boudKeys())
  expect_identical(k1, k2)
})

test_that("curveInputsKey changes with every input that shapes the curve", {
  skip_if_no_boudewyn()
  base <- curveInputsKey("Pice_mar", "ECO_1", mock_species(), mock_speciesEcoregion(), 300L, mock_boudKeys())
  sp <- mock_species(); sp[speciesCode == "Pice_mar", growthcurve := 0.717]
  expect_false(identical(base, curveInputsKey("Pice_mar", "ECO_1", sp, mock_speciesEcoregion(), 300L, mock_boudKeys())))
  sp <- mock_species(); sp[speciesCode == "Pice_mar", mortalityshape := 21]
  expect_false(identical(base, curveInputsKey("Pice_mar", "ECO_1", sp, mock_speciesEcoregion(), 300L, mock_boudKeys())))
  sp <- mock_species(); sp[speciesCode == "Pice_mar", longevity := 250L]
  expect_false(identical(base, curveInputsKey("Pice_mar", "ECO_1", sp, mock_speciesEcoregion(), 300L, mock_boudKeys())))
  se <- mock_speciesEcoregion(); se[speciesCode == "Pice_mar" & ecoregionGroup == "ECO_1", maxANPP := 600]
  expect_false(identical(base, curveInputsKey("Pice_mar", "ECO_1", mock_species(), se, 300L, mock_boudKeys())))
  se <- mock_speciesEcoregion(); se[speciesCode == "Pice_mar" & ecoregionGroup == "ECO_1", maxB := 10575]
  expect_false(identical(base, curveInputsKey("Pice_mar", "ECO_1", mock_species(), se, 300L, mock_boudKeys())))
  expect_false(identical(base, curveInputsKey("Pice_mar", "ECO_1", mock_species(), mock_speciesEcoregion(), 250L, mock_boudKeys())))
  bk <- mock_boudKeys(); bk$canfi_species <- 104L
  expect_false(identical(base, curveInputsKey("Pice_mar", "ECO_1", mock_species(), mock_speciesEcoregion(), 300L, bk)))
})

test_that("curveInputsKey ignores other species and ecoregions", {
  skip_if_no_boudewyn()
  base <- curveInputsKey("Pice_mar", "ECO_1", mock_species(), mock_speciesEcoregion(), 300L, mock_boudKeys())
  sp <- mock_species(); sp[speciesCode == "Pice_gla", growthcurve := 0.9]
  se <- mock_speciesEcoregion(); se[ecoregionGroup == "ECO_2", maxB := 1]
  expect_identical(base, curveInputsKey("Pice_mar", "ECO_1", sp, se, 300L, mock_boudKeys()))
})

test_that("curveInputsKey accepts factor codes and errors on a missing row", {
  skip_if_no_boudewyn()
  sp <- mock_species()[, speciesCode := factor(speciesCode)]
  se <- mock_speciesEcoregion()[, `:=`(speciesCode = factor(speciesCode), ecoregionGroup = factor(ecoregionGroup))]
  expect_identical(curveInputsKey(factor("Pice_mar"), factor("ECO_1"), sp, se, 300L, mock_boudKeys()),
                   curveInputsKey("Pice_mar", "ECO_1", mock_species(), mock_speciesEcoregion(), 300L, mock_boudKeys()))
  expect_error(curveInputsKey("Pice_mar", "ECO_9", mock_species(), mock_speciesEcoregion(), 300L, mock_boudKeys()))
})

test_that("staleDevTypes returns missing keys, keys with changed inputs and keys cached without inputsKey", {
  curve <- function(ik) { d <- data.frame(age = 0:1, vol_m3ha = 0:1); attr(d, "inputsKey") <- ik; d }
  cache <- list("A|low|E1" = curve("a1"), "B|med|E1" = curve("b1"),
                "C|high|E1" = data.frame(age = 0:1, vol_m3ha = 0:1))   # cached before inputsKey existed
  stale <- staleDevTypes(c("A|low|E1", "B|med|E1", "C|high|E1", "D|low|E1"),
                         c("a1",       "b2",       "c1",        "d1"), cache)
  expect_setequal(stale, c("B|med|E1", "C|high|E1", "D|low|E1"))
})

test_that("staleDevTypes returns nothing when every curve matches its inputs", {
  curve <- function(ik) { d <- data.frame(age = 0:1, vol_m3ha = 0:1); attr(d, "inputsKey") <- ik; d }
  cache <- list("A|low|E1" = curve("a1"))
  expect_length(staleDevTypes("A|low|E1", "a1", cache), 0L)
  expect_error(staleDevTypes(c("A|low|E1", "B|low|E1"), "a1", cache))
})

test_that("inputsKey survives the cache round-trip", {
  d <- data.frame(age = 0:1, vol_m3ha = 0:1); attr(d, "inputsKey") <- "v1|x"
  path <- tempfile(fileext = ".rds"); on.exit(unlink(path))
  saveCurveCache(list("A|low|E1" = d), path)
  expect_identical(attr(loadCurveCache(path)[["A|low|E1"]], "inputsKey"), "v1|x")
})

test_that("curveInputsKey works on keyed tables like LandR's (column names shadow arguments)", {
  skip_if_no_boudewyn()
  sp <- mock_species(); sp[, `:=`(species = speciesCode, speciesCode = factor(speciesCode))]; setkey(sp, speciesCode)
  se <- mock_speciesEcoregion(); se[, speciesCode := factor(speciesCode)]; setkey(se, speciesCode)
  k <- curveInputsKey("Pice_gla", "ECO_2", sp, se, 300L, mock_boudKeys())
  expect_match(k, paste0("|growthcurve=", sprintf("%a", 0.25), "|mortalityshape=", sprintf("%a", 10),
                         "|maxANPP=", sprintf("%a", 90), "|maxB=", sprintf("%a", 9000), "|"), fixed = TRUE)
})

test_that("curveInputsKey distinguishes nearby doubles and survives an RDS round-trip of its inputs", {
  skip_if_no_boudewyn()
  se <- mock_speciesEcoregion()
  se2 <- copy(se)[speciesCode == "Pice_mar" & ecoregionGroup == "ECO_1", maxANPP := 100 + 1e-13]
  k1 <- curveInputsKey("Pice_mar", "ECO_1", mock_species(), se, 300L, mock_boudKeys())
  expect_false(identical(k1, curveInputsKey("Pice_mar", "ECO_1", mock_species(), se2, 300L, mock_boudKeys())))
  path <- tempfile(fileext = ".rds"); on.exit(unlink(path))
  saveRDS(list(sp = mock_species(), se = se), path); rt <- readRDS(path)
  expect_identical(k1, curveInputsKey("Pice_mar", "ECO_1", rt$sp, rt$se, 300L, mock_boudKeys()))
})

test_that("curveInputsKey changes when a Boudewyn coefficient changes but the identifiers do not", {
  skip_if_no_boudewyn()
  base <- curveInputsKey("Pice_mar", "ECO_1", mock_species(), mock_speciesEcoregion(), 300L, mock_boudKeys())
  bk <- mock_boudKeys()
  keyWithEditedTable <- function(tbl, idCol, coefCol) {
    slot <- paste0(".boudewyn_", tbl)
    orig <- loadBoudewynTable(tbl)
    on.exit(.pkgEnv[[slot]] <- orig)   # restored even if the key throws
    edited <- orig
    row <- which(edited[[idCol]] == bk$canfi_species & edited$juris_id == bk$juris_id &
                   edited$ecozone == bk$ecozone)[1]
    edited[row, coefCol] <- edited[row, coefCol] * 1.01
    .pkgEnv[[slot]] <- edited
    curveInputsKey("Pice_mar", "ECO_1", mock_species(), mock_speciesEcoregion(), 300L, bk)
  }
  expect_false(identical(base, keyWithEditedTable("table3", "canfi_species", "a")))
  expect_false(identical(base, keyWithEditedTable("table6_tb", "canfi_spec", "b2")))
  expect_identical(base, curveInputsKey("Pice_mar", "ECO_1", mock_species(), mock_speciesEcoregion(), 300L, bk))
})

test_that("loadBoudewynTable with an explicit dir reads only from that dir", {
  skip_if_no_boudewyn()
  slot <- ".boudewyn_table3"
  orig <- .pkgEnv[[slot]]
  on.exit(.pkgEnv[[slot]] <- orig)
  d <- tempfile(); dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  tbl <- utils::read.csv(file.path(Sys.getenv("TESTTHAT_WD"), "inputs", "boudewyn_table3.csv"))
  tbl$a <- tbl$a * 2
  utils::write.csv(tbl, file.path(d, "boudewyn_table3.csv"), row.names = FALSE)
  .pkgEnv[[slot]] <- NULL
  expect_equal(loadBoudewynTable("table3", dir = d)$a, tbl$a)
})

test_that("boudewynCoefficients returns the rows boudewynBiomassToVol uses", {
  skip_if_no_boudewyn()
  co <- boudewynCoefficients(101L, "BC", 9L)
  expect_named(co$t6, c("a1", "a2", "a3", "b1", "b2", "b3", "c1", "c2", "c3"))
  expect_named(co$t3, c("a", "b"))
  expect_error(boudewynCoefficients(-1L, "BC", 9L), "Table 6_tb")
})
