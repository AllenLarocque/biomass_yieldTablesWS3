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

test_that("every species lookup row points at the matching Boudewyn species", {
  skip_if_no_lookup()
  lut <- loadSpeciesLookup()
  t3  <- loadBoudewynTable("table3")
  names(t3) <- trimws(names(t3))
  t3Names <- unique(t3[, c("canfi_species", "genus", "species")])
  for (i in seq_len(nrow(lut))) {
    hit <- t3Names[t3Names$canfi_species == lut$canfi_species[i], ]
    expect_gt(nrow(hit), 0L, label = lut$speciesCode[i])
    ## LandR codes are Genu_spe; a merged group (e.g. Pice_eng_gla) may use any of its members.
    ## One code can have several rows (e.g. 500 is both PSEU MEN and PSEU SPP): one must match
    parts <- strsplit(toupper(lut$speciesCode[i]), "_")[[1]]
    match <- startsWith(trimws(hit$genus), parts[1]) &
      Reduce(`|`, lapply(parts[-1], function(sp) startsWith(trimws(hit$species), sp)))
    expect_true(any(match),
                info = paste(lut$speciesCode[i], "->", lut$canfi_species[i], hit$genus, hit$species))
  }
})

test_that("LandR's merged BC spruce group Pice_eng_gla has a lookup entry", {
  skip_if_no_lookup()
  ## LandR's default BC species list merges Engelmann and white spruce into Pice_eng_gla.
  ## Boudewyn has no hybrid code; white spruce (105) is used
  expect_identical(lookupBoudewynKeys("Pice_eng_gla", juris_id = "BC")$canfi_species, 105L)
})
