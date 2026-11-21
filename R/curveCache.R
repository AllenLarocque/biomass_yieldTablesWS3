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
# species x ecoregion growth parameters, which change when e.g. Biomass_speciesParameters fits them.
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
  num <- function(x) sprintf("%.15g", as.numeric(x[1]))
  paste0("v", .curveAlgorithmVersion,
         "|longevity=", num(sp$longevity), "|growthcurve=", num(sp$growthcurve),
         "|mortalityshape=", num(sp$mortalityshape),
         "|maxANPP=", num(se$maxANPP), "|maxB=", num(se$maxB),
         "|maxSimAge=", num(maxSimAge),
         "|canfi=", paste(boudKeys$canfi_species, collapse = ","),
         "|juris=", paste(boudKeys$juris_id, collapse = ","),
         "|ecozone=", paste(boudKeys$ecozone, collapse = ","))
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
