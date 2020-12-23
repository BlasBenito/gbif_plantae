y <- max_moran(
  x = ecoregions$data$richness_species_vascular,
  distance.matrix = distance.matrix,
  distance.thresholds = NULL
)

moran(
  x = ecoregions$data$richness_species_vascular,
  distance.matrix = distance.matrix,
  distance.threshold = 0
  )

pca.factors <- pca_factors(
  distance.matrix = distance.matrix,
  distance.threshold = 0
)