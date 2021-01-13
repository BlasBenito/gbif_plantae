library(raster)


variables <- variables.na[[c("ndvi_average", "bio1", "bio4", "bio12", "bio14", "bio5", "bio6", "diversidad_topo", "human_footprint", "topo_slope")]]

names(variables) <- c("ndvi", "bio1", "bio4", "bio12", "bio14", "bio5", "bio6", "topography_diversity", "human_footprint", "topography_slope")

points <- raster::sampleRegular(
  x = variables,
  size = 300,
  xy = TRUE,
  asRaster = FALSE
) 
points <- na.omit(points)
nrow(points)
points <- as.data.frame(points)

predictors <- c("bio1", "bio4", "bio12", "bio14", "bio5", "bio6", "topography_diversity", "human_footprint", "topography_slope")

library(HH)

vif(points[, predictors])
points$bio4 <- NULL
predictors <- predictors[predictors != "bio4"]
vif(points[, predictors])
points$bio6 <- NULL
predictors <- predictors[predictors != "bio6"]
vif(points[, predictors])
points$topography_slope <- NULL
predictors <- predictors[predictors != "topography_slope"]
vif(points[, predictors])
points$bio5 <- NULL
predictors <- predictors[predictors != "bio5"]
vif(points[, predictors])

ndvi <- points

x11()
plot(ndvi[, c("x", "y")])

ndvi.distances <- as.matrix(dist(as.matrix(ndvi[, c("x", "y")]), upper = TRUE, diag = TRUE))

ndvi.weights <- 1/ndvi.distances
diag(ndvi.weights) <- 0

ape::Moran.I(
  x = ndvi$ndvi,
  weight = ndvi.weights
)

save(ndvi, ndvi.distances, ndvi.weights, file = "content/post/02_parallelizing_loops_with_R/ndvi.RData")
