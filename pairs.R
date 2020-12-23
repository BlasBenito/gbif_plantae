
library(sparklyr)
library(dplyr)
library(dtplyr)
library(dbplyr)
library(data.table)
library(RPostgreSQL)
library(rpostgis)
library(readr)
library(sp)
library(raster)
library(tidyverse)
library(hypervolume)
library(landscapemetrics)
library(tibble)

#loading functions
source("functions.R")

#removing scientific notation
options(scipen = 9999)

postgresql.connection <- RPostgreSQL::dbConnect(
  drv = "PostgreSQL",
  dbname = "flora_ecoregions",
  user = "blas",
  password = "p1p0n3t3",
  port = 5432
)

ecoregions <- DBI::dbGetQuery(
  postgresql.connection, 
  "SELECT
  ecoregion AS ecoregion_name,
  biome AS ecoregion_biome,
  realm AS ecoregion_realm,
  continent AS ecoregion_continent,
  area AS ecoregion_area_km2,
  count_records AS bias_records,
  records_per_km2 AS bias_records_per_km2,
  count_species / count_records AS bias_species_per_record,
  count_species / records_per_km2 AS bias_species_per_record_per_km2,
  count_species AS richness_species,
  count_genera AS richness_genera,
  count_families AS richness_families,
  count_classes AS richness_classes,
  count_phyla AS richness_phyla,
  count_species_vascular AS richness_species_vascular,
  count_genera_vascular AS richness_genera_vascular,
  count_families_vascular AS richness_families_vascular,
  count_classes_vascular AS richness_classes_vascular,
  neighbors_count,
  neighbors_area_sum AS neighbors_area,
  neighbors_percentage_shared_edge,
  neighbors_average_aridity,
  neighbors_species_count_average,
  neighbors_genera_count_average,
  neighbors_families_count_average,
  neighbors_classes_count_average,
  conservation_assessment AS human_conservation_assessment,
  human_population,
  human_population_density,
  human_footprint_minimum,
  human_footprint_maximum,
  human_footprint_average,
  climate_aridity_index_minimum,
  climate_aridity_index_maximum,
  climate_aridity_index_average,
  climate_bio1_minimum,
  climate_bio1_maximum,
  climate_bio1_average,
  climate_bio4_minimum,
  climate_bio4_maximum,
  climate_bio4_average,
  climate_bio5_minimum,
  climate_bio5_maximum,
  climate_bio5_average,
  climate_bio12_minimum,
  climate_bio12_maximum,
  climate_bio12_average,
  climate_bio15_minimum,
  climate_bio15_maximum,
  climate_bio15_average,
  climate_growing_degree_days_5_minimum,
  climate_growing_degree_days_5_maximum,
  climate_growing_degree_days_5_average,
  climate_velocity_lgm_minimum,
  climate_velocity_lgm_maximum,
  climate_velocity_lgm_average,
  landcover_bare_soil_percent_minimum AS landcover_bare_percent_minimum, 
  landcover_bare_soil_percent_maximum AS landcover_bare_percent_maximum,
  landcover_bare_soil_percent_average AS landcover_bare_percent_average,
  landcover_herbaceous_percent_minimum AS landcover_herbs_percent_minimum,
  landcover_herbaceous_percent_maximum AS landcover_herbs_percent_maximum,
  landcover_herbaceous_percent_average AS landcover_herbs_percent_average,
  landcover_trees_percent_minimum,
  landcover_trees_percent_maximum,
  landcover_trees_percent_average,
  ndvi_min_minimum AS landcover_ndvi_minimum,
  ndvi_max_maximum AS landcover_ndvi_maximum,
  (ndvi_min_average + ndvi_max_average) / 2 AS landcover_ndvi_average,
  geo_latitude_average,
  geo_latitude_range,
  geo_longitude_average,
  geo_longitude_range,
  topography_elevation_minimum,
  topography_elevation_maximum,
  topography_elevation_average,
  topography_elevation_range
  FROM ecoregions;"
)

ecoregions_pairs <- DBI::dbGetQuery(
  postgresql.connection, 
  "SELECT
  target_ecoregion AS ecoregion_name,
  neighbor_ecoregion AS neighbor_name,
  target_area AS ecoregion_area,
  neighbor_area,
  target_ecoregion_climate_aridity_index_average AS ecoregion_climate_aridity_index_average,
 neighbor_ecoregion_climate_aridity_index_average AS neighbor_climate_aridity_index_average,
 target_ecoregion_geo_latitude_average AS ecoregion_geo_latitude_average,
 neighbor_ecoregion_geo_latitude_average AS neighbor_geo_latitude_average,
  target_ecoregion_geo_longitude_average AS ecoregion_geo_longitude_average,
 neighbor_ecoregion_geo_longitude_average AS neighbor_geo_longitude_average,
   target_ecoregion_topography_elevation_average AS ecoregion_topography_elevation_average,
 neighbor_ecoregion_topography_elevation_average AS neighbor_topography_elevation_average,
 same_biome AS connection_same_biome,
 same_realm AS connection_same_realm,
 same_continent AS connection_same_continent,
 geographic_distance_km AS connection_distance,
 connected AS connection_are_neighbors,
 shared_edge_length AS connection_shared_edge,
 geo_latitude_diff AS connection_latitude_diff,
 geo_longitude_diff AS connection_longitude_diff,
 area_diff AS connection_area_diff,
 count_species_diff AS richness_species_diff,
 count_genera_diff AS richness_genera_diff,
 count_families_diff AS richness_families_diff,
 count_classes_diff AS richness_classes_diff,
 count_species_vascular_diff AS richness_species_vascular_diff,
 count_genera_vascular_diff AS richness_genera_vascular_diff,
 count_families_vascular_diff AS richness_families_vascular_diff,
 count_classes_vascular_diff AS richness_classes_vascular_diff,
 climate_aridity_index_diff,
 climate_bio1_average_diff,
 climate_bio4_average_diff,
 climate_bio5_average_diff,
 climate_bio12_average_diff,
 climate_bio15_average_diff,
 climate_velocity_lgm_average_diff,
 human_footprint_average_diff,
 human_population_diff,
 human_population_density_diff,
 landcover_bare_soil_percent_average_diff AS landcover_bare_percent_average_diff,
 landcover_herbaceous_percent_average_diff AS landcover_herbs_percent_average_diff,
 landcover_trees_percent_average_diff,
 ndvi_min_diff AS landcover_ndvi_min_diff,
 ndvi_max_diff AS landcover_ndvi_max_diff,
 topography_elevation_diff
 FROM ecoregions_pairs;"
)

ecoregions_neighbors <- DBI::dbGetQuery(
  postgresql.connection, 
  "SELECT 
  ecoregion,
  neighbor
  FROM ecoregions_neighbors;"
)

ecoregions_species  <- DBI::dbGetQuery(
  postgresql.connection, 
  "SELECT * FROM ecoregions_species;"
)

ecoregions_species_taxonomy  <- DBI::dbGetQuery(
  postgresql.connection, 
  "SELECT * FROM ecoregions_species_taxonomy;"
)

ecoregions_genera  <- DBI::dbGetQuery(
  postgresql.connection, 
  "SELECT * FROM ecoregions_genera;"
)

ecoregions_families  <- DBI::dbGetQuery(
  postgresql.connection, 
  "SELECT * FROM ecoregions_families;"
)

ecoregions_classes  <- DBI::dbGetQuery(
  postgresql.connection, 
  "SELECT * FROM ecoregions_classes;"
)

ecoregions_species_vascular  <- DBI::dbGetQuery(
  postgresql.connection, 
  "SELECT * FROM ecoregions_species_vascular;"
)

ecoregions_genera_vascular  <- DBI::dbGetQuery(
  postgresql.connection, 
  "SELECT * FROM ecoregions_genera_vascular;"
)

ecoregions_families_vascular  <- DBI::dbGetQuery(
  postgresql.connection, 
  "SELECT * FROM ecoregions_families_vascular;"
)

ecoregions_classes  <- DBI::dbGetQuery(
  postgresql.connection, 
  "SELECT * FROM ecoregions_classes_vascular;"
)

species <- DBI::dbGetQuery(
  postgresql.connection, 
  "SELECT 
  class,
  family,
  genus,
  species,
  spatial_records
  FROM species;"
)

#loading the tree database from https://tools.bgci.org/global_tree_search.php
trees <- data.table::fread(
  "global_tree_search_trees_1_4.csv",
  header = TRUE)[, 1]
colnames(trees) <- "species"

#getting unique species in ecoregions_species
gbif.species <- unique(ecoregions_species$species)

#separating the dataframe in species with the same name in ecoregions_species
trees.ready <- trees[trees$species %in% gbif.species, ]
trees.not.ready <- trees[!(trees$species %in% gbif.species), ]
rm(trees)

#finding gbif synonym for trees in trees.not.ready
trees.not.ready$synonym <- "none"

for(species.i in trees.not.ready$species){
  
  #retrieving synonyms
  synonyms.i <- rgbif::name_backbone(
    name = species.i, 
    kingdom = "Plantae",
    verbose=TRUE)$data %>% 
    as.data.frame()
  
  #if nothing found
  if(synonyms.i$matchType == "NONE"){
    next
    #if synonyms found  
  } else {
    
    #iterate through synonyms to find them in gbif.species
    for(synonym.i in synonyms.i[, "canonicalName"]){
      
      #chdk if synonym is in gbif.species
      if(synonym.i %in% gbif.species){
        
        #write it in the synonyms column
        trees.not.ready[
          trees.not.ready$species == species.i,
          "synonym"] <- synonym.i
        
        #break loop
        break
        
      }
    }
  }
}

#how many species have I salvaged with this
sum(trees.not.ready$synonym != "none")
#325...

#preparing to join with trees.ready
trees.not.ready <- trees.not.ready[
  trees.not.ready$synonym != "none",
  "synonym"
]
colnames(trees.not.ready) <- "species"

#joining with trees.ready
trees.ready <- rbind(
  trees.ready,
  trees.not.ready
)

#adding boolean column
trees.ready$is.tree <- TRUE

#adding is.tree to ecoregions_species_taxonomy
ecoregions_species_taxonomy <- dplyr::left_join(
  x = ecoregions_species_taxonomy,
  y = trees.ready,
  by = "species"
)
ecoregions_species_taxonomy[
  is.na(ecoregions_species_taxonomy$is.tree), 
  "is.tree"
] <- FALSE

#adding grasses 
ecoregions_species_taxonomy$is.grass <- FALSE
ecoregions_species_taxonomy[
  ecoregions_species_taxonomy$family == "Poaceae",
  "is.grass"
] <- TRUE

#generating the ecoregions_x tables
ecoregions_species_trees <- ecoregions_species_taxonomy[
  ecoregions_species_taxonomy$is.tree == TRUE,
  c("ecoregion", "species")
]

ecoregions_genera_trees <- ecoregions_species_taxonomy[
  ecoregions_species_taxonomy$is.tree == TRUE,
  c("ecoregion", "genus")
] %>% 
  dplyr::distinct()

ecoregions_families_trees <- ecoregions_species_taxonomy[
  ecoregions_species_taxonomy$is.tree == TRUE,
  c("ecoregion", "family")
] %>% 
  dplyr::distinct()

ecoregions_species_grasses <- ecoregions_species_taxonomy[
  ecoregions_species_taxonomy$is.grass == TRUE,
  c("ecoregion", "species")
]

ecoregions_genera_grasses<- ecoregions_species_taxonomy[
  ecoregions_species_taxonomy$is.grass == TRUE,
  c("ecoregion", "genus")
] %>% 
  dplyr::distinct()

#adding the columns
ecoregions <- tibble::add_column(ecoregions, richness_species_trees = NA, .before = "rarity_species_by_presence")
ecoregions <- tibble::add_column(ecoregions, richness_genera_trees = NA, .before = "rarity_species_by_presence")
ecoregions <- tibble::add_column(ecoregions, richness_families_trees = NA, .before = "rarity_species_by_presence")
ecoregions <- tibble::add_column(ecoregions, richness_species_grasses = NA, .before = "rarity_species_by_presence")
ecoregions <- tibble::add_column(ecoregions, richness_genera_grasses = NA, .before = "rarity_species_by_presence")

for(i in 1:nrow(ecoregions)){
  
  #getting ecoregion name
  ecoregion.i <- ecoregions[i, "ecoregion_name"]
  
  #filling richness values
  ecoregions[i, "richness_species_trees"] <- nrow(ecoregions_species_trees[ecoregions_species_trees$ecoregion == ecoregion.i, ])
  ecoregions[i, "richness_genera_trees"] <- nrow(ecoregions_genera_trees[ecoregions_genera_trees$ecoregion == ecoregion.i, ])
  ecoregions[i, "richness_families_trees"] <- nrow(ecoregions_families_trees[ecoregions_families_trees$ecoregion == ecoregion.i, ])
  ecoregions[i, "richness_species_grasses"] <- nrow(ecoregions_species_grasses[ecoregions_species_grasses$ecoregion == ecoregion.i, ])
  ecoregions[i, "richness_genera_grasses"] <- nrow(ecoregions_genera_grasses[ecoregions_genera_grasses$ecoregion == ecoregion.i, ])
  
}

ecoregions <- tibble::add_column(ecoregions, rarity_species_by_presence = NA, .before = "neighbors_count")
ecoregions <- tibble::add_column(ecoregions, rarity_species_by_ecoregion = NA, .before = "neighbors_count")
ecoregions <- tibble::add_column(ecoregions, rarity_species_vascular_by_presence = NA, .before = "neighbors_count")
ecoregions <- tibble::add_column(ecoregions, rarity_species_vascular_by_ecoregion = NA, .before = "neighbors_count")
ecoregions <- tibble::add_column(ecoregions, rarity_genera_by_presence = NA, .before = "neighbors_count")
ecoregions <- tibble::add_column(ecoregions, rarity_genera_by_ecoregion = NA, .before = "neighbors_count")
ecoregions <- tibble::add_column(ecoregions, rarity_genera_vascular_by_presence = NA, .before = "neighbors_count")
ecoregions <- tibble::add_column(ecoregions, rarity_genera_vascular_by_ecoregion = NA, .before = "neighbors_count")
ecoregions <- tibble::add_column(ecoregions, rarity_species_trees_by_presence = NA, .before = "neighbors_count")
ecoregions <- tibble::add_column(ecoregions, rarity_species_trees_by_ecoregion = NA, .before = "neighbors_count")
ecoregions <- tibble::add_column(ecoregions, rarity_genera_trees_by_presence = NA, .before = "neighbors_count")
ecoregions <- tibble::add_column(ecoregions, rarity_genera_trees_by_ecoregion = NA, .before = "neighbors_count")
ecoregions <- tibble::add_column(ecoregions, rarity_species_grasses_by_presence = NA, .before = "neighbors_count")
ecoregions <- tibble::add_column(ecoregions, rarity_species_grasses_by_ecoregion = NA, .before = "neighbors_count")
ecoregions <- tibble::add_column(ecoregions, rarity_genera_grasses_by_presence = NA, .before = "neighbors_count")
ecoregions <- tibble::add_column(ecoregions, rarity_genera_grasses_by_ecoregion = NA, .before = "neighbors_count")

for(i in 1:nrow(ecoregions)){
  
  #getting ecoregion name
  ecoregion.i <- ecoregions[i, "ecoregion_name"]
  
  #rarity species by presence
  ecoregions[i, "rarity_species_by_presence"] <- sum(1 / dplyr::left_join(x = ecoregions_species[ecoregions_species$ecoregion == ecoregion.i,], y = species[, c("species", "spatial_records")], by = "species") %>% dplyr::pull(spatial_records))
  
  ecoregions[i, "rarity_species_vascular_by_presence"] <- sum(1 / dplyr::left_join(x = ecoregions_species_vascular[ecoregions_species_vascular$ecoregion == ecoregion.i,], y = species[, c("species", "spatial_records")], by = "species") %>% dplyr::pull(spatial_records))
  
  #
  ecoregions[i, "rarity_species_trees_by_presence"] <- sum(1 / dplyr::left_join(x = ecoregions_species_trees[ecoregions_species_trees$ecoregion == ecoregion.i,], y = species[, c("species", "spatial_records")], by = "species") %>% dplyr::pull(spatial_records))
  
  #
  ecoregions[i, "rarity_species_grasses_by_presence"] <- sum(1 / dplyr::left_join(x = ecoregions_species_grasses[ecoregions_species_grasses$ecoregion == ecoregion.i,], y = species[, c("species", "spatial_records")], by = "species") %>% dplyr::pull(spatial_records))
  
  #rarity genera by presence
  #
  ecoregions[i, "rarity_genera_by_presence"]  <-  sum( 1 / species %>% dplyr::filter(genus %in% ecoregions_genera[ecoregions_genera$ecoregion == ecoregion.i, "genus"]) %>% dplyr::group_by(genus) %>% dplyr::summarise(spatial_records = sum(spatial_records)) %>% dplyr::pull(spatial_records))
  
  #
  ecoregions[i, "rarity_genera_vascular_by_presence"]  <-  sum( 1 / species %>% dplyr::filter(genus %in% ecoregions_genera_vascular[ecoregions_genera_vascular$ecoregion == ecoregion.i, "genus"]) %>% dplyr::group_by(genus) %>% dplyr::summarise(spatial_records = sum(spatial_records)) %>% dplyr::pull(spatial_records))
  
  #
  ecoregions[i, "rarity_genera_trees_by_presence"] <-  sum(1 / species %>% dplyr::filter(genus %in% ecoregions_genera_trees[ecoregions_genera_trees$ecoregion == ecoregion.i, "genus"]) %>% dplyr::group_by(genus) %>% dplyr::summarise(spatial_records = sum(spatial_records)) %>% dplyr::pull(spatial_records))
  
  #
  ecoregions[i, "rarity_genera_grasses_by_presence"] <- sum(1 / species %>% dplyr::filter(genus %in% ecoregions_genera_grasses[ecoregions_genera_grasses$ecoregion == ecoregion.i, "genus"]) %>% dplyr::group_by(genus) %>% dplyr::summarise(spatial_records = sum(spatial_records)) %>% dplyr::pull(spatial_records))
  
  
  #rarity species by ecoregion
  ecoregions[i, "rarity_species_by_ecoregion"] <- sum(1 /
                                                        ecoregions_species %>% dplyr::filter(species %in% ecoregions_species[ecoregions_species$ecoregion == ecoregion.i, "species"]) %>% dplyr::group_by(species) %>% dplyr::summarise(n_ecoregions = n()) %>% dplyr::pull(n_ecoregions))
  
  #
  ecoregions[i, "rarity_species_vascular_by_ecoregion"] <- sum(1 /
                                                                 ecoregions_species_vascular %>% dplyr::filter(species %in% ecoregions_species_vascular[ecoregions_species_vascular$ecoregion == ecoregion.i, "species"]) %>% dplyr::group_by(species) %>% dplyr::summarise(n_ecoregions = n()) %>% dplyr::pull(n_ecoregions))
  
  #
  ecoregions[i, "rarity_species_trees_by_ecoregion"] <- sum(1 /
                                                              ecoregions_species_trees %>% dplyr::filter(species %in% ecoregions_species_trees[ecoregions_species_trees$ecoregion == ecoregion.i, "species"]) %>% dplyr::group_by(species) %>% dplyr::summarise(n_ecoregions = n()) %>% dplyr::pull(n_ecoregions))
  
  #
  ecoregions[i, "rarity_species_grasses_by_ecoregion"] <- sum(1 /
                                                                ecoregions_species_grasses %>% dplyr::filter(species %in% ecoregions_species_grasses[ecoregions_species_grasses$ecoregion == ecoregion.i, "species"]) %>% dplyr::group_by(species) %>% dplyr::summarise(n_ecoregions = n()) %>% dplyr::pull(n_ecoregions))
  
  #rarity by genus and ecoregion
  
  #
  ecoregions[i, "rarity_genera_by_ecoregion"] <- sum(1 /
                                                       ecoregions_genera %>% dplyr::filter(genus %in% ecoregions_genera[ecoregions_genera$ecoregion == ecoregion.i, "genus"]) %>% dplyr::group_by(genus) %>% dplyr::summarise(n_ecoregions = n()) %>% dplyr::pull(n_ecoregions))
  
  #
  ecoregions[i, "rarity_genera_vascular_by_ecoregion"] <- sum(1 /
                                                                ecoregions_genera_vascular %>% dplyr::filter(genus %in% ecoregions_genera_vascular[ecoregions_genera_vascular$ecoregion == ecoregion.i, "genus"]) %>% dplyr::group_by(genus) %>% dplyr::summarise(n_ecoregions = n()) %>% dplyr::pull(n_ecoregions))
  
  #
  ecoregions[i, "rarity_genera_trees_by_ecoregion"] <- sum(1 /
                                                             ecoregions_genera_trees %>% dplyr::filter(genus %in% ecoregions_genera_trees[ecoregions_genera_trees$ecoregion == ecoregion.i, "genus"]) %>% dplyr::group_by(genus) %>% dplyr::summarise(n_ecoregions = n()) %>% dplyr::pull(n_ecoregions))
  
  #
  ecoregions[i, "rarity_genera_grasses_by_ecoregion"] <-  sum(1 /
                                                                ecoregions_genera_grasses %>% dplyr::filter(genus %in% ecoregions_genera_grasses[ecoregions_genera_grasses$ecoregion == ecoregion.i, "genus"]) %>% dplyr::group_by(genus) %>% dplyr::summarise(n_ecoregions = n()) %>% dplyr::pull(n_ecoregions))
  
  
}

#loading fragmentation data
load("fragmentation_ecoregions.RData")

#joining it with the ecoregions table
ecoregions <-
  ecoregions %>% 
  dplyr::left_join(
    y = fragmentation_ecoregions,
    by = c("ecoregion_name" = "ecoregion")
  )


rm(fragmentation_ecoregions)

#loading hypervolumes
load("hypervolumes_ecoregions.RData")

#creating hypervolume
ecoregions$climate_hypervolume <- NA

#iterating through ecoregions
for(i in 1:nrow(ecoregions)){
  
  #getting ecoregion name
  ecoregion.i <- ecoregions[i, "ecoregion_name"]
  
  #adding climate hypervolume
  if(ecoregion.i %in% names(hypervolumes_ecoregions)){
    ecoregions[i, "climate_hypervolume"] <- hypervolume::get_volume(hypervolumes_ecoregions[[ecoregion.i]])
  }
  
  #if there are gaps to fill
  if(is.na(ecoregions[i, "neighbors_count"])){
    
    #retrieving neighbors 
    neighbors.i <- dplyr::filter(
      ecoregions_pairs,
      ecoregion_name == ecoregion.i,
      connection_distance <= 1000
    ) %>% 
      dplyr::pull(neighbor_name)
    
    #next if empty
    if(length(neighbors.i) == 0){next}
    
    #indices of rows in ecoregions
    indices.i <- which(ecoregions$ecoregion_name %in% neighbors.i)
    
    #filling empty columns
    ecoregions[i, "neighbors_count"] <- length(neighbors.i)
    
    ecoregions[i, "neighbors_area"] <- sum(ecoregions[
      indices.i,
      "ecoregion_area_km2"
    ])
    
    ecoregions[i, "neighbors_percentage_shared_edge"] <- 0
    
    ecoregions[i, "neighbors_average_aridity"] <- mean(ecoregions[
      indices.i,
      "climate_aridity_index_average"
    ])
    
    ecoregions[i, "neighbors_species_count_average"] <- mean(ecoregions[
      indices.i,
      "richness_species"
    ])
    
    ecoregions[i, "neighbors_genera_count_average"] <- mean(ecoregions[
      indices.i,
      "richness_genera"
    ])
    
    ecoregions[i, "neighbors_families_count_average"] <- mean(ecoregions[
      indices.i,
      "richness_families"
    ])
    
    ecoregions[i, "neighbors_classes_count_average"] <- mean(ecoregions[
      indices.i,
      "richness_classes"
    ])
    
  } else {
    next
  }
  
}

#names of betadiversity components
betadiversity.components <- c(
  "a", 
  "b", 
  "c", 
  "a_percent", 
  "b_percent", 
  "c_percent", 
  "R", 
  "R_percent", 
  "C", 
  "Bsor", 
  "Bsim")

#vector of new columns
betadiversity.columns <- c(
  paste(
    "betadiversity_species", 
    betadiversity.components,
    sep = "_"
  ),
  paste(
    "betadiversity_genera", 
    betadiversity.components,
    sep = "_"
  ),
  paste(
    "betadiversity_families", 
    betadiversity.components,
    sep = "_"
  ),
  paste(
    "betadiversity_species_vascular", 
    betadiversity.components,
    sep = "_"
  ),
  paste(
    "betadiversity_genera_vascular", 
    betadiversity.components,
    sep = "_"
  ),
  paste(
    "betadiversity_families_vascular", 
    betadiversity.components,
    sep = "_"
  ),
  paste(
    "betadiversity_species_trees", 
    betadiversity.components,
    sep = "_"
  ),
  paste(
    "betadiversity_genera_trees", 
    betadiversity.components,
    sep = "_"
  ),
  paste(
    "betadiversity_families_trees", 
    betadiversity.components,
    sep = "_"
  ),
  paste(
    "betadiversity_species_grasses", 
    betadiversity.components,
    sep = "_"
  ),
  paste(
    "betadiversity_genera_grasses", 
    betadiversity.components,
    sep = "_"
  )
)

#creating the columns in the dataframe
for(betadiversity.column in betadiversity.columns){
  ecoregions[, betadiversity.column] <- NA
}

rm(betadiversity.column)

#iterating through ecoregions
for(ecoregion.i in ecoregions$ecoregion_name){
  
  #gathering neighbor names
  neighbors.i <- ecoregions_neighbors[
    ecoregions_neighbors$ecoregion == ecoregion.i, 
    "neighbor"
  ]
  
  #if it has no neighbors
  if(length(neighbors.i) == 0){
    
    #retrieving neighbors whithin 1000km
    neighbors.i <- dplyr::filter(
      ecoregions_pairs,
      ecoregion_name == ecoregion.i,
      connection_distance <= 1000
    ) %>% 
      dplyr::pull(neighbor_name)
    
  }
  
  #if again, there are no neighbors, jump to next
  if(length(neighbors.i) == 0){next}
  
  #computing betadiversity for species
  betadiversity.species.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbors.i,
    taxa.list = ecoregions_species,
    taxa.column = "species"
  )
  
  #for genera
  betadiversity.genera.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbors.i,
    taxa.list = ecoregions_genera,
    taxa.column = "genus"
  )
  
  #for family
  betadiversity.families.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbors.i,
    taxa.list = ecoregions_families,
    taxa.column = "family"
  )
  
  #computing vascular
  betadiversity.species.vascular.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbors.i,
    taxa.list = ecoregions_species_vascular,
    taxa.column = "species"
  )
  
  #for genera
  betadiversity.genera.vascular.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbors.i,
    taxa.list = ecoregions_genera_vascular,
    taxa.column = "genus"
  )
  
  #for family
  betadiversity.families.vascular.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbors.i,
    taxa.list = ecoregions_families_vascular,
    taxa.column = "family"
  )
  
  #computing vascular
  betadiversity.species.trees.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbors.i,
    taxa.list = ecoregions_species_trees,
    taxa.column = "species"
  )
  
  #for genera
  betadiversity.genera.trees.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbors.i,
    taxa.list = ecoregions_genera_trees,
    taxa.column = "genus"
  )
  
  #for family
  betadiversity.families.trees.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbors.i,
    taxa.list = ecoregions_families_trees,
    taxa.column = "family"
  )
  
  #computing vascular
  betadiversity.species.grasses.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbors.i,
    taxa.list = ecoregions_species_grasses,
    taxa.column = "species"
  )
  
  #for genera
  betadiversity.genera.grasses.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbors.i,
    taxa.list = ecoregions_genera_grasses,
    taxa.column = "genus"
  )
  
  #writing results to dataframe
  ecoregions[
    ecoregions$ecoregion_name == ecoregion.i,
    betadiversity.columns] <- c(
      betadiversity.species.i,
      betadiversity.genera.i,
      betadiversity.families.i,
      betadiversity.species.vascular.i,
      betadiversity.genera.vascular.i,
      betadiversity.families.vascular.i,
      betadiversity.species.trees.i,
      betadiversity.genera.trees.i,
      betadiversity.families.trees.i,
      betadiversity.species.grasses.i,
      betadiversity.genera.grasses.i
    )
  
}

ecoregions <- na.omit(ecoregions)

ecoregions_pairs <- dplyr::filter(
  ecoregions_pairs,
  ecoregion_name %in% ecoregions$ecoregion_name,
  neighbor_name %in% ecoregions$ecoregion_name
)

#creating the columns in the dataframe
for(betadiversity.column in betadiversity.columns){
  ecoregions_pairs[, betadiversity.column] <- NA
}

#adding environmental overlap
environmental.overlap.columns  <- c(
  "environmental_overlap_jaccard",
  "environmental_overlap_sorensen",
  "environmental_overlap_frac_unique_ecoregion",
  "environmental_overlap_frac_unique_neighbor"
)
for(environmental.overlap.column in environmental.overlap.columns){
  ecoregions_pairs[, environmental.overlap.column] <- NA
}

rm(betadiversity.column, environmental.overlap.column)

for(i in 1:nrow(ecoregions_pairs)){
  
  #getting names
  ecoregion.i <- ecoregions_pairs[i, "ecoregion_name"]
  neighbor.i <- ecoregions_pairs[i, "neighbor_name"]
  
  #computing betadiversity for species
  betadiversity.species.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbor.i,
    taxa.list = ecoregions_species,
    taxa.column = "species"
  )
  
  #for genera
  betadiversity.genera.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbor.i,
    taxa.list = ecoregions_genera,
    taxa.column = "genus"
  )
  
  #for family
  betadiversity.families.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbor.i,
    taxa.list = ecoregions_families,
    taxa.column = "family"
  )
  
  #computing betadiversity for vascular species
  betadiversity.species.vascular.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbor.i,
    taxa.list = ecoregions_species_vascular,
    taxa.column = "species"
  )
  
  #for genera
  betadiversity.genera.vascular.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbor.i,
    taxa.list = ecoregions_genera_vascular,
    taxa.column = "genus"
  )
  
  #for family
  betadiversity.families.vascular.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbor.i,
    taxa.list = ecoregions_families_vascular,
    taxa.column = "family"
  )
  
  #computing betadiversity for vascular species
  betadiversity.species.trees.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbor.i,
    taxa.list = ecoregions_species_trees,
    taxa.column = "species"
  )
  
  #for genera
  betadiversity.genera.trees.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbor.i,
    taxa.list = ecoregions_genera_trees,
    taxa.column = "genus"
  )
  
  #for family
  betadiversity.families.trees.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbor.i,
    taxa.list = ecoregions_families_trees,
    taxa.column = "family"
  )
  
  #computing betadiversity for vascular species
  betadiversity.species.grasses.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbor.i,
    taxa.list = ecoregions_species_grasses,
    taxa.column = "species"
  )
  
  #for genera
  betadiversity.genera.grasses.i <- betadiversity(
    ecoregion.1 = ecoregion.i,
    ecoregion.2 = neighbor.i,
    taxa.list = ecoregions_genera_grasses,
    taxa.column = "genus"
  )
  
  #writing results to dataframe
  ecoregions_pairs[
    i,
    betadiversity.columns] <- c(
      betadiversity.species.i,
      betadiversity.genera.i,
      betadiversity.families.i,
      betadiversity.species.vascular.i,
      betadiversity.genera.vascular.i,
      betadiversity.families.vascular.i,
      betadiversity.species.trees.i,
      betadiversity.genera.trees.i,
      betadiversity.families.trees.i,
      betadiversity.species.grasses.i,
      betadiversity.genera.grasses.i
    )
  
  #computing environmental overlap
  hv.set.i <- hypervolume::hypervolume_set(
    hv1 = hypervolumes_ecoregions[[ecoregion.i]],
    hv2 = hypervolumes_ecoregions[[neighbor.i]],
    check.memory = FALSE
  )
  
  #environmental overlap
  ecoregions_pairs[
    i,
    environmental.overlap.columns
  ] <-suppressMessages(hypervolume::hypervolume_overlap_statistics(hvlist = hv.set.i))
  
}

save(ecoregions, ecoregions_pairs, file = "ecoregions_plant_diversity.RData")
