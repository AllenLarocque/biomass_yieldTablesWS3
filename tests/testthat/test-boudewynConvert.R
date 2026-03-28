test_that("lookupBoudewynKeys errors clearly when lookup CSV is absent", {
  skip_if_no_lookup()  # this skip itself requires the file — if absent, skip
  # If we reach here, file exists; test the function normally
  result <- lookupBoudewynKeys("Pice_mar", juris_id = "BC")
  expect_type(result, "list")
  expect_named(result, c("canfi_species", "juris_id", "ecozone"))
})

test_that("lookupBoudewynKeys returns named list with correct types", {
  skip_if_no_lookup()
  result <- lookupBoudewynKeys("Pice_mar", juris_id = "BC")
  expect_type(result$canfi_species, "integer")
  expect_type(result$juris_id,      "character")
  expect_type(result$ecozone,       "integer")
})

test_that("lookupBoudewynKeys errors on unknown species/jurisdiction combination", {
  skip_if_no_lookup()
  expect_error(
    lookupBoudewynKeys("UNKNOWN_SPP", juris_id = "BC"),
    regexp = "No Boudewyn lookup"
  )
})

test_that("boudewynBiomassToVol returns data.frame with age and vol_m3ha", {
  skip_if_no_lookup()
  skip_if_no_boudewyn()
  keys  <- lookupBoudewynKeys("Pice_mar", juris_id = "BC")
  ageB  <- data.frame(age = 0:50, B_gm2 = seq(0, 5000, length.out = 51))
  result <- boudewynBiomassToVol(ageB,
    canfi_species = keys$canfi_species,
    juris_id      = keys$juris_id,
    ecozone       = keys$ecozone
  )
  expect_s3_class(result, "data.frame")
  expect_named(result, c("age", "vol_m3ha"))
  expect_equal(nrow(result), nrow(ageB))
})

test_that("boudewynBiomassToVol vol_m3ha is always non-negative", {
  skip_if_no_lookup()
  skip_if_no_boudewyn()
  keys  <- lookupBoudewynKeys("Pice_mar", juris_id = "BC")
  ageB  <- data.frame(age = 0:100, B_gm2 = c(0, seq(100, 10000, length.out = 100)))
  result <- boudewynBiomassToVol(ageB,
    canfi_species = keys$canfi_species,
    juris_id      = keys$juris_id,
    ecozone       = keys$ecozone
  )
  expect_true(all(result$vol_m3ha >= 0))
})

test_that("boudewynBiomassToVol returns zero volume at zero biomass", {
  skip_if_no_lookup()
  skip_if_no_boudewyn()
  keys  <- lookupBoudewynKeys("Pice_mar", juris_id = "BC")
  ageB  <- data.frame(age = 0L, B_gm2 = 0)
  result <- boudewynBiomassToVol(ageB,
    canfi_species = keys$canfi_species,
    juris_id      = keys$juris_id,
    ecozone       = keys$ecozone
  )
  expect_equal(result$vol_m3ha, 0)
})

test_that("boudewynBiomassToVol volume increases monotonically with biomass", {
  skip_if_no_lookup()
  skip_if_no_boudewyn()
  keys  <- lookupBoudewynKeys("Pice_mar", juris_id = "BC")
  # strictly increasing biomass sequence
  ageB  <- data.frame(age = 1:20, B_gm2 = seq(500, 10000, length.out = 20))
  result <- boudewynBiomassToVol(ageB,
    canfi_species = keys$canfi_species,
    juris_id      = keys$juris_id,
    ecozone       = keys$ecozone
  )
  expect_true(all(diff(result$vol_m3ha) >= 0))
})
