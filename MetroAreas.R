library(tidyverse)
library(sf)
library(tigris)
SimulateMetroData <- function(States,City,samp_size){
  
  metro_tracts <- map_dfr(States, ~{
    tracts(.x, cb = TRUE, year = 2020)
  }) %>%
    st_transform(8528)
  
  metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
    filter(str_detect(NAME, City)) %>%
    st_transform(8528)
  
  metro_tracts_within <- metro_tracts %>%
    st_filter(metro, .predicate = st_within)
  
  random_points <- st_sample(metro, size = samp_size)
  return(list(metro_tracts_within,metro,random_points))
}

sample_size <- 500
spatial_metro_list <- SimulateMetroData(c("CA"),"Los Angeles",sample_size)

metro_tracts_within <- spatial_metro_list[[1]]
metro <- spatial_metro_list[[2]]
random_points <- spatial_metro_list[[3]]

random_points_latlon <- st_transform(random_points, "+proj=longlat +datum=WGS84")
rcoord <- st_coordinates(random_points_latlon)
metro_tracts_within <- spatial_metro_list[[1]]
metro <- spatial_metro_list[[2]]
random_points <- spatial_metro_list[[3]]

random_points_latlon <- st_transform(random_points, "+proj=longlat +datum=WGS84")
rcoord <- st_coordinates(random_points_latlon)

ggplot() + 
  geom_sf(data = metro_tracts_within, fill = "white", color = "grey") + 
  geom_sf(data = metro, fill = NA, color = "red") + 
  geom_sf(data = random_points, color = "red") + 
  theme_void()

sample_size <- 500
spatial_metro_list <- SimulateMetroData(c("MD"),"Baltimore",sample_size)

metro_tracts_within <- spatial_metro_list[[1]]
metro <- spatial_metro_list[[2]]
random_points <- spatial_metro_list[[3]]

random_points_latlon <- st_transform(random_points, "+proj=longlat +datum=WGS84")
rcoord <- st_coordinates(random_points_latlon)
metro_tracts_within <- spatial_metro_list[[1]]
metro <- spatial_metro_list[[2]]
random_points <- spatial_metro_list[[3]]

random_points_latlon <- st_transform(random_points, "+proj=longlat +datum=WGS84")
rcoord <- st_coordinates(random_points_latlon)

ggplot() + 
  geom_sf(data = metro_tracts_within, fill = "white", color = "grey") + 
  geom_sf(data = metro, fill = NA, color = "red") + 
  geom_sf(data = random_points, color = "red") + 
  theme_void()