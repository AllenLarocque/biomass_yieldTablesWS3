# helper-fixtures.R — shared mock data and setup for biomass_yieldTablesWS3 tests
#
# testthat always evaluates helper files with wd = the test directory
# (modules/biomass_yieldTablesWS3/tests/testthat/).  All source() calls below
# are relative to that directory.
#
# Usage:
#   testthat::test_local("modules/biomass_yieldTablesWS3")   # from project root
#   devtools::test()                                          # from module root

library(data.table)

# Source module helpers — ../../R/ resolves to modules/biomass_yieldTablesWS3/R/
source("../../R/curveCache.R")
source("../../R/siteQuality.R")
source("../../R/simStand.R")
source("../../R/boudewynConvert.R")

# TESTTHAT_WD — absolute path to the project root.
# Tests are 4 directory levels below the project root:
#   <project>/modules/biomass_yieldTablesWS3/tests/testthat/
# Used by loadBoudewynTable() and loadSpeciesLookup() to locate files in
# inputs/ and data/ relative to the project root.
if (nchar(Sys.getenv("TESTTHAT_WD")) == 0L)
  Sys.setenv(TESTTHAT_WD = normalizePath(file.path(getwd(), "..", "..", "..", "..")))

# ── Mock data factories ──────────────────────────────────────────────────────

#' Minimal species data.table (2 species, realistic boreal parameters)
mock_species <- function() {
  data.table(
    speciesCode    = c("Pice_mar", "Pice_gla"),
    longevity      = c(200L, 250L),
    mortalityshape = c(15, 10),
    growthcurve    = c(0.25, 0.25)
  )
}

#' Minimal speciesEcoregion data.table (2 species × 2 ecoregions)
mock_speciesEcoregion <- function() {
  data.table(
    speciesCode    = c("Pice_mar", "Pice_gla", "Pice_mar", "Pice_gla"),
    ecoregionGroup = c("ECO_1",    "ECO_1",    "ECO_2",    "ECO_2"),
    maxANPP        = c(100,        120,         80,          90),
    maxB           = c(10000,      12000,       8000,        9000)
  )
}

#' Minimal cohortData (4 cohorts across 3 pixel groups, 2 ecoregions)
mock_cohortData <- function() {
  data.table(
    pixelGroup     = c(1L, 1L, 2L, 3L),
    speciesCode    = c("Pice_mar", "Pice_gla", "Pice_mar", "Pice_gla"),
    ecoregionGroup = c("ECO_1",    "ECO_1",    "ECO_2",    "ECO_1"),
    age            = c(50L,        30L,         80L,        10L),
    B              = c(5000,       3000,        8000,       500)
  )
}

# ── Skip helpers ─────────────────────────────────────────────────────────────

#' Skip if Boudewyn CSV files are absent from project inputs/
skip_if_no_boudewyn <- function() {
  wd <- Sys.getenv("TESTTHAT_WD")
  if (!file.exists(file.path(wd, "inputs", "boudewyn_table3.csv")) ||
      !file.exists(file.path(wd, "inputs", "boudewyn_table6_tb.csv"))) {
    testthat::skip(
      "Boudewyn CSVs not found in inputs/ — run a full simulation first to cache them"
    )
  }
}

#' Skip if species_boudewyn_lookup.csv is absent from module data/
skip_if_no_lookup <- function() {
  wd   <- Sys.getenv("TESTTHAT_WD")
  path <- file.path(wd, "modules", "biomass_yieldTablesWS3", "data",
                    "species_boudewyn_lookup.csv")
  if (!file.exists(path)) {
    testthat::skip(
      "species_boudewyn_lookup.csv not found in modules/biomass_yieldTablesWS3/data/"
    )
  }
}
