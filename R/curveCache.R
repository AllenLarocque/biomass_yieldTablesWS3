# devTypeTupleKey(), loadCurveCache(), saveCurveCache(), diffDevTypes(), curveInputsKey(),
# staleDevTypes(): yield curve cache management

devTypeTupleKey <- function(speciesCode, site_quality, ecoregion) {
  stopifnot(
    length(speciesCode) == 1L, !is.na(speciesCode),
    length(site_quality) == 1L, !is.na(site_quality),
    length(ecoregion) == 1L, !is.na(ecoregion)
  )
  paste(speciesCode, site_quality, ecoregion, sep = "|")
}

diffDevTypes <- function(currentTuples, cachedKeys) {
  currentTuples[!currentTuples %in% cachedKeys]
}

# Version of the curve-building algorithm (simulateStand + boudewynBiomassToVol). Bump it when
# either changes, so curves cached by the old code are rebuilt.
.curveAlgorithmVersion <- 1L

# curveInputsKey(): a string of every input that shapes one development type's yield curve, stored
# on each cached curve as attr(curve, "inputsKey"). A dev type key (species|site quality|ecoregion)
# names a curve but does not fix it: the curve also depends on the species traits and the
# species x ecoregion growth parameters, which change when e.g. Biomass_speciesParameters fits them,
# and on the Boudewyn coefficients that convert biomass to volume.
curveInputsKey <- function(speciesCode, ecoregion, species, speciesEcoregion, maxSimAge, boudKeys) {
  speciesCode <- as.character(speciesCode)
  ecoregion   <- as.character(ecoregion)
  # Masks outside data.table `[`: inside dt[i], bare names such as `speciesCode` resolve to columns
  spMask <- as.character(species$speciesCode) == speciesCode
  seMask <- as.character(speciesEcoregion$speciesCode) == speciesCode &
    as.character(speciesEcoregion$ecoregionGroup) == ecoregion
  sp <- species[spMask]
  se <- speciesEcoregion[seMask]
  if (nrow(sp) == 0L || nrow(se) == 0L) {
    stop("curveInputsKey: no species or speciesEcoregion row for ", speciesCode, " in ", ecoregion)
  }
  # Exact, locale-independent encoding (hexadecimal floating point) of each value, after the same
  # coercions simulateStand() applies, so any change to an input changes the key
  hex <- function(x) sprintf("%a", as.numeric(x[1]))
  coefs <- boudewynCoefficients(boudKeys$canfi_species, boudKeys$juris_id, boudKeys$ecozone)
  paste0("v", .curveAlgorithmVersion,
         "|longevity=", hex(as.integer(sp$longevity[1])), "|growthcurve=", hex(sp$growthcurve),
         "|mortalityshape=", hex(sp$mortalityshape),
         "|maxANPP=", hex(se$maxANPP), "|maxB=", hex(se$maxB),
         "|maxSimAge=", hex(as.integer(maxSimAge)),
         "|canfi=", boudKeys$canfi_species, "|juris=", boudKeys$juris_id, "|ecozone=", boudKeys$ecozone,
         "|t6=", paste(vapply(coefs$t6, hex, ""), collapse = ","),
         "|t3=", paste(vapply(coefs$t3, hex, ""), collapse = ","))
}

# staleDevTypes(): dev type keys whose curve must be (re)built: not cached, or cached from
# different inputs (including curves cached before inputsKey existed).
# currentInputs: character vector of curveInputsKey() values, parallel to currentKeys.
staleDevTypes <- function(currentKeys, currentInputs, cache) {
  stopifnot(length(currentKeys) == length(currentInputs))
  cachedInputs <- vapply(currentKeys, function(k) {
    ik <- attr(cache[[k]], "inputsKey")
    if (is.null(ik)) NA_character_ else ik
  }, character(1), USE.NAMES = FALSE)
  currentKeys[is.na(cachedInputs) | cachedInputs != currentInputs]
}

loadCurveCache <- function(cachePath) {
  if (!file.exists(cachePath)) return(list())
  readRDS(cachePath)
}

saveCurveCache <- function(ws3YieldCurves, cachePath) {
  dir.create(dirname(cachePath), recursive = TRUE, showWarnings = FALSE)
  saveRDS(ws3YieldCurves, cachePath)
  invisible(cachePath)
}
