#automatic install of packages if they are not readily available
list.of.packages <- c(
  "tidyverse", 
  "ggplot2",
  "patchwork",
  "sf",
  "viridis",
  "kableExtra",
  "ggcorrplot",
  "sp",
  "spdep",
  "adespatial",
  "RSpectra",
  "Matrix",
  "parallel",
  "doParallel",
  "foreach",
  "factoextra",
  "igraph",
  "network",
  "sna",
  "ndtv"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#installing missing packages
if(length(new.packages) > 0){
  install.packages(new.packages, dep=TRUE)
}

#loading packages
for(package.i in list.of.packages){
  suppressPackageStartupMessages(library(package.i, character.only = TRUE))
}

load("~/Dropbox/GITHUB/gbif_plantae/ecoregions_plant_diversity_ready.RData")


#loading functions
source("functions.R")

#removing scientific notation
options(scipen = 9999)

#setting ggplot2 theme
ggplot2::theme_set(theme_bw())

#map of ecoregions
ggplot2::ggplot(data = ecoregions$polygons) +
  ggplot2::geom_sf(aes(fill = richness_species_vascular), size = 0.1) +
  ggplot2::scale_fill_viridis_c() + 
  ggplot2::labs(fill = "Richness species")



#plotting relationship with neighboring ecoregions
#subsetting the table
target.ecoregion <- "Gibson desert"
radius.km <- 3000

#df with the subset of target ecoregions
plot.df <- ecoregions_pairs$data %>% 
  dplyr::filter(
    ecoregion_name == target.ecoregion,
    connection_distance < radius.km
  )

all.polygons <- ecoregions$polygons[ecoregions$polygons$ecoregion_name %in% c(target.ecoregion, plot.df$neighbor_name), "ecoregion_name"]
neighbor.polygons <- ecoregions$polygons[ecoregions$polygons$ecoregion_name %in% plot.df$neighbor_name, "ecoregion_name"]

#subset of polygons
polygons.df <- ecoregions$polygons[ecoregions$polygons$ecoregion_name %in% c(target.ecoregion, plot.df$neighbor_name), ]

#network plot

#plotting airport network
p.species <- ggplot2::ggplot(data = ecoregions$polygons[ecoregions$polygons$ecoregion_name %in% c(target.ecoregion, plot.df$neighbor_name), ]) +
  ggplot2::geom_sf() +
  ggplot2::geom_segment(
    data = plot.df, 
    aes(
      x = ecoregion_geo_longitude_average,
      xend = neighbor_geo_longitude_average,
      y = ecoregion_geo_latitude_average,
      yend = neighbor_geo_latitude_average,
      color = betadiversity_a_percent_species_vascular
    ),
    size = 1
  ) +
  ggplot2::scale_color_viridis_c(direction = -1) + 
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude") +
  ggplot2::labs(color = expression(paste("% shared species \n of vascular plants"))) + 
  ggtitle(paste("Target ecoregion:", target.ecoregion, sep = " "))

p.genera <- ggplot2::ggplot(data = ecoregions$polygons[ecoregions$polygons$ecoregion_name %in% c(target.ecoregion, plot.df$neighbor_name), ]) +
  ggplot2::geom_sf() +
  ggplot2::geom_segment(
    data = plot.df, 
    aes(
      x = ecoregion_geo_longitude_average,
      xend = neighbor_geo_longitude_average,
      y = ecoregion_geo_latitude_average,
      yend = neighbor_geo_latitude_average,
      color = betadiversity_a_percent_genera_vascular
    ),
    size = 1
  ) +
  ggplot2::scale_color_viridis_c(direction = -1) + 
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude") +
  ggplot2::labs(color = expression(paste("% shared genera \n of vascular plants"))) + 
  ggtitle(paste("Target ecoregion:", target.ecoregion, sep = " "))

p.families <- ggplot2::ggplot(data = ecoregions$polygons[ecoregions$polygons$ecoregion_name %in% c(target.ecoregion, plot.df$neighbor_name), ]) +
  ggplot2::geom_sf() +
  ggplot2::geom_segment(
    data = plot.df, 
    aes(
      x = ecoregion_geo_longitude_average,
      xend = neighbor_geo_longitude_average,
      y = ecoregion_geo_latitude_average,
      yend = neighbor_geo_latitude_average,
      color = betadiversity_a_percent_families_vascular
    ),
    size = 1
  ) +
  ggplot2::scale_color_viridis_c(direction = -1) + 
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude") +
  ggplot2::labs(color = expression(paste("% shared families \n of vascular plants"))) + 
  ggtitle(paste("Target ecoregion:", target.ecoregion, sep = " "))

x11(width = 22, height = 17)
p.species | p.genera | p.families




#vector with arid ecoregions
arid.ecoregions <- ecoregions$data[ecoregions$data$climate_aridity_index_average > 0.44, "ecoregion_name"]

arid.ecoregions <- c("Colorado Plateau shrublands", "Sierra Madre Occidental pine-oak forests", "Gibson desert", "Dry Chaco", "Tamaulipan mezquital", "Caatinga", "Taklimakan desert", "Namib Desert")

arid.ecoregions <- c("Caatinga")

for(ecoregion.i in arid.ecoregions){
  
  #plotting relationship with neighboring ecoregions
  #subsetting the table
  target.ecoregion <- ecoregion.i
  radius.km <- 8000
  
  #df with the subset of target ecoregions
  neighbors.df <- ecoregions_pairs$data %>% 
    dplyr::filter(
      ecoregion_name == target.ecoregion,
      connection_distance < radius.km
    )
  
  all.polygons <- ecoregions$polygons[ecoregions$polygons$ecoregion_name %in% c(target.ecoregion, neighbors.df$neighbor_name), "ecoregion_name"]
  neighbors.polygons <- ecoregions$polygons[ecoregions$polygons$ecoregion_name %in% neighbors.df$neighbor_name, "ecoregion_name"]
  
  #joining neighbors.df and neighbors.polygons
  neighbors.polygons <- dplyr::left_join(
    neighbors.polygons,
    neighbors.df,
    by = c("ecoregion_name" = "neighbor_name")
  )
  
  
  p.species <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = all.polygons, fill = "red", size = 0.1) +
    ggplot2::geom_sf(data = neighbors.polygons, aes(fill = betadiversity_a_percent_species_vascular), size = 0.1) +
    ggplot2::scale_fill_viridis_c(direction = -1, limits = c(0, 100)) + 
    ggplot2::labs(fill = "Betadiversity vascular species") + 
    ggplot2::ggtitle(paste("Target ecoregion:", target.ecoregion, sep = " ")) + 
    ggplot2::theme(legend.position = "bottom")
  
  p.genera <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = all.polygons, fill = "red", size = 0.1) +
    ggplot2::geom_sf(data = neighbors.polygons, aes(fill = betadiversity_a_percent_genera_vascular), size = 0.1) +
    ggplot2::scale_fill_viridis_c(direction = -1, limits = c(0, 100)) + 
    ggplot2::labs(fill = "Betadiversity vascular genera") + 
    ggplot2::ggtitle(paste("Target ecoregion:", target.ecoregion, sep = " ")) + 
    ggplot2::theme(legend.position = "bottom")
  
  p.families <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = all.polygons, fill = "red", size = 0.1) +
    ggplot2::geom_sf(data = neighbors.polygons, aes(fill = betadiversity_a_percent_families_vascular), size = 0.1) +
    ggplot2::scale_fill_viridis_c(direction = -1, limits = c(0, 100)) + 
    ggplot2::labs(fill = "Betadiversity vascular families") + 
    ggplot2::ggtitle(paste("Target ecoregion:", target.ecoregion, sep = " ")) + 
    ggplot2::theme(legend.position = "bottom")
  
  
  p.species | p.genera | p.families
  ggsave(
    filename = paste("plots/", target.ecoregion, "_vascular_plants.png", sep = ""),
    plot = last_plot(),
    height = 8,
    width = 22
    )
  
  
  p.species <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = all.polygons, fill = "red", size = 0.1) +
    ggplot2::geom_sf(data = neighbors.polygons, aes(fill = betadiversity_a_percent_species_trees), size = 0.1) +
    ggplot2::scale_fill_viridis_c(direction = -1, limits = c(0, 100)) + 
    ggplot2::labs(fill = "Betadiversity vascular species") + 
    ggplot2::ggtitle(paste("Target ecoregion:", target.ecoregion, sep = " ")) + 
    ggplot2::theme(legend.position = "bottom")
  
  p.genera <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = all.polygons, fill = "red", size = 0.1) +
    ggplot2::geom_sf(data = neighbors.polygons, aes(fill = betadiversity_a_percent_genera_trees), size = 0.1) +
    ggplot2::scale_fill_viridis_c(direction = -1, limits = c(0, 100)) + 
    ggplot2::labs(fill = "Betadiversity vascular genera") + 
    ggplot2::ggtitle(paste("Target ecoregion:", target.ecoregion, sep = " ")) + 
    ggplot2::theme(legend.position = "bottom")
  
  p.families <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = all.polygons, fill = "red", size = 0.1) +
    ggplot2::geom_sf(data = neighbors.polygons, aes(fill = betadiversity_a_percent_families_trees), size = 0.1) +
    ggplot2::scale_fill_viridis_c(direction = -1, limits = c(0, 100)) + 
    ggplot2::labs(fill = "Betadiversity vascular families") + 
    ggplot2::ggtitle(paste("Target ecoregion:", target.ecoregion, sep = " ")) + 
    ggplot2::theme(legend.position = "bottom")
  
  
  p.species | p.genera | p.families
  ggsave(
    filename = paste("plots/", target.ecoregion, "_trees.png", sep = ""),
    plot = last_plot(),
    height = 8,
    width = 22
  )
  
  p.species <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = all.polygons, fill = "red", size = 0.1) +
    ggplot2::geom_sf(data = neighbors.polygons, aes(fill = betadiversity_a_percent_species_grasses), size = 0.1) +
    ggplot2::scale_fill_viridis_c(direction = -1, limits = c(0, 100)) + 
    ggplot2::labs(fill = "Betadiversity vascular species") + 
    ggplot2::ggtitle(paste("Target ecoregion:", target.ecoregion, sep = " ")) + 
    ggplot2::theme(legend.position = "bottom")
  
  p.genera <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = all.polygons, fill = "red", size = 0.1) +
    ggplot2::geom_sf(data = neighbors.polygons, aes(fill = betadiversity_a_percent_genera_grasses), size = 0.1) +
    ggplot2::scale_fill_viridis_c(direction = -1, limits = c(0, 100)) + 
    ggplot2::labs(fill = "Betadiversity vascular genera") + 
    ggplot2::ggtitle(paste("Target ecoregion:", target.ecoregion, sep = " ")) + 
    ggplot2::theme(legend.position = "bottom")

  
  p.species | p.genera
  ggsave(
    filename = paste("plots/", target.ecoregion, "_grasses.png", sep = ""),
    plot = last_plot(),
    height = 8,
    width = 22
  )

}



#network plots
#######################################

#input data
adjacency.matrix <- ecoregions_pairs$distance_a_species_vascular
distance.matrix <- ecoregions$autocorrelation$distance
distance.threshold <- 5000

#applying threshold
adjacency.matrix[distance.matrix > distance.threshold] <- 0
adjacency.matrix[adjacency.matrix < 50] <- 0

network <- graph_from_adjacency_matrix(
  adjmatrix = adjacency.matrix,
  mode = "undirected",
  diag = FALSE,
  add.colnames = NA,
  add.rownames = NA,
  weighted = TRUE
  )

l <-layout_with_fr(network)


x11(width = 22, height = 17)
plot(
  network,
  vertex.size = 1,
  vertex.label = NA
)