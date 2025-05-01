## code to prepare `DATASET` dataset goes here

library(tidyverse)
library(sf)

## 1. 15 minites ###############################################################
# ranges of latitudes and longitudes
lat <- seq(30, 35, by = 0.25)
lon <- seq(135, 140, by = 0.25)

# years and months
years <- 2005L:2020L
months <- 1L:12L

base15 <- expand.grid(year = years,
                      month = months, 
                      Long = lon,
                      Lati = lat)

# areas
grid_points <- expand.grid(Long = lon[-length(lon)], Lati = lat[-length(lat)])
polygons <- lapply(seq_len(nrow(grid_points)), function(x) {
    lon_min <- grid_points$Long[x]
    lat_min <- grid_points$Lati[x]
    lon_max <- lon_min + 0.25
    lat_max <- lat_min + 0.25

    coords <- matrix(c(
        lon_min, lat_min,
        lon_min, lat_max,
        lon_max, lat_max,
        lon_max, lat_min,
        lon_min, lat_min
    ), ncol = 2, byrow = TRUE)

    st_polygon(list(coords))
})

poly_sf <- st_sfc(polygons, crs = 4326)
poly_sf_df <- st_sf(id = seq_len(length(polygons)), geometry = poly_sf,
                    area = st_area(poly_sf))

grid_area <- cbind(grid_points, area = as.integer(st_area(poly_sf)))

# Check
plot(poly_sf_df["id"])
plot(poly_sf_df["area"])

# make example data
set.seed(20250501)

Egg_anchovy0 <- rbinom(nrow(base15), size = 1, prob = 0.50)
Egg_anchovy1 <- rnbinom(nrow(base15), size = 2, mu = 250)
Egg_anchovy <- ifelse(Egg_anchovy0 == 1, 0, Egg_anchovy1)
hist(Egg_anchovy)

Egg_sardine0 <- rbinom(nrow(base15), size = 1, prob = 0.75)
Egg_sardine1 <- rnbinom(nrow(base15), size = 1.5, mu = 150)
Egg_sardine <- ifelse(Egg_sardine0 == 1, 0, Egg_sardine1)
hist(Egg_sardine)

Egg15min <- left_join(base15, grid_area) %>%
    dplyr::mutate(Nrec = sample(1:50, n(), replace = TRUE),
                  Anchovy_Egg = Egg_anchovy,
                  Sardine_Egg = Egg_sardine,
                  SSTmean = runif(n(), min = 2.5, max = 35))

colnames(Egg15min)

## 2. 5 minutes ################################################################
# ranges of latitudes and longitudes
lat <- seq(30, 35, by = 0.25/3)
lon <- seq(135, 140, by = 0.25/3)

# years and months
years <- 2005L:2020L
months <- 1L:12L

base05 <- expand.grid(year = years,
                      month = months, 
                      Long = lon,
                      Lati = lat)

# areas
grid_points <- expand.grid(Long = lon[-length(lon)], Lati = lat[-length(lat)])
polygons <- lapply(seq_len(nrow(grid_points)), function(x) {
    lon_min <- grid_points$Long[x]
    lat_min <- grid_points$Lati[x]
    lon_max <- lon_min + 0.25/3
    lat_max <- lat_min + 0.25/3

    coords <- matrix(c(
        lon_min, lat_min,
        lon_min, lat_max,
        lon_max, lat_max,
        lon_max, lat_min,
        lon_min, lat_min
    ), ncol = 2, byrow = TRUE)

    st_polygon(list(coords))
})

poly_sf <- st_sfc(polygons, crs = 4326)
poly_sf_df <- st_sf(id = seq_len(length(polygons)), geometry = poly_sf,
                    area = st_area(poly_sf))

grid_area <- cbind(grid_points, area = as.integer(st_area(poly_sf)))

# Check
plot(poly_sf_df["id"])
plot(poly_sf_df["area"])

# make example data
set.seed(20250501)

Egg_anchovy0 <- rbinom(nrow(base05), size = 1, prob = 0.60)
Egg_anchovy1 <- rnbinom(nrow(base05), size = 1, mu = 250/3)
Egg_anchovy <- ifelse(Egg_anchovy0 == 1, 0, Egg_anchovy1)
hist(Egg_anchovy)

Egg_sardine0 <- rbinom(nrow(base05), size = 1, prob = 0.80)
Egg_sardine1 <- rnbinom(nrow(base05), size = 1, mu = 50)
Egg_sardine <- ifelse(Egg_sardine0 == 1, 0, Egg_sardine1)
hist(Egg_sardine)

Egg05min <- left_join(base05, grid_area) %>%
    dplyr::mutate(Nrec = sample(1:25, n(), replace = TRUE),
                  Anchovy_Egg = Egg_anchovy,
                  Sardine_Egg = Egg_sardine,
                  SSTmean = runif(n(), min = 2.5, max = 35))

colnames(Egg05min)

usethis::use_data(Egg15min, Egg05min, overwrite = TRUE)

rm(list = ls())
