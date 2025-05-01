# CREATE 15 arc-min example data

# [1] Spatial dummy ============================================================
# remotes::install_github("ropensci/rnaturalearthhires")

# Load necessary libraries
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(units)
library(ggplot2)

meshsize <- 0.25/3

# 1. Define bounding box for Japan area (in EPSG:4326)
bbox_japan <- st_bbox(c(xmin = 122, xmax = 153, ymin = 24, ymax = 46), crs = 4326)

poly_NEPO <- st_as_sf(st_sfc(
    st_polygon(list(rbind(
        c(126, 30), c(130, 32), c(135, 35), c(140, 37), c(142, 42), c(153, 45),
        c(153, 25), c(126, 25), c(126, 30)
        ))),
    crs = 4326
    ))

# 2. Create 0.25/3-degree grid mesh (in 4326)
grid <- st_make_grid( st_as_sfc(bbox_japan), cellsize = meshsize, what = "polygons")
grid_sf <- st_sf(geometry = grid)

within_id <- st_within(grid_sf, poly_NEPO, sparse = FALSE)[, 1]
grid_sf <- grid_sf[within_id, ]

# 3. Transform to high-accuracy Japanese projection (EPSG:6677)
grid_sf <- st_transform(grid_sf, 6677)

# 4. Calculate area in square meters and km²
grid_sf$cell_area_m2 <- set_units(st_area(grid_sf), m^2)
grid_sf$cell_area_km2 <- as.numeric(grid_sf$cell_area_m2) / 1e6

# 5. Download land data (in 4326)
land <- ne_countries(scale = "large", returnclass = "sf")
land <- st_make_valid(land)

# 6. Crop while still in 4326
land_japan <- st_crop(land, bbox_japan)

# 7. Transform land data to EPSG:6677
land_japan <- st_transform(land_japan, 6677)

# 8. Subtract land areas from the grid (to keep only ocean)
sea_only <- st_difference(grid_sf, land_japan)

par(mfrow = c(2, 1))
hist(st_area(grid_sf))
hist(st_area(sea_only))
dev.off()

# 9. Define custom centroid (off Daiozaki cape)
custom_centroid <- st_sfc(st_point(c(137.0160364707836, 34.39121414050595)), crs = 4326)
custom_centroid <- st_transform(custom_centroid, 6677)

# 10. Calculate distance from each grid centroid to custom centroid
sea_only$distance_to_centroid <- st_distance(st_centroid(sea_only), custom_centroid)

hist(-sea_only$distance_to_centroid / 1e6)
dev.off()

range(-sea_only$distance_to_centroid / 1e6)
# Units: [m]
# [1] -1.92332966 -0.01018655

# 11. Random observation values and counts
set.seed(123)
sea_only$observed_value <- runif(nrow(sea_only), min = 0, max = 500)
sea_only$observed_count <- sample(0:25, nrow(sea_only), replace = TRUE)

# 12. Adjust values/counts based on distance (decay with distance)
sea_only$adjusted_observed_value <- 
    sea_only$observed_value * exp(-drop_units(sea_only$distance_to_centroid) / 5e5)
sea_only$adjusted_observed_count <- 
    sea_only$observed_count * exp(-drop_units(sea_only$distance_to_centroid) / 5e5)

sea_only$adjusted_observed_value <- round(sea_only$adjusted_observed_value)
sea_only$adjusted_observed_count <- round(sea_only$adjusted_observed_count)

range(sea_only$adjusted_observed_value)
# [1]   0 472
range(sea_only$adjusted_observed_count)
# [1]  0 47

# 13. Convert final results to a data frame if needed
# sea_only_df <- as.data.frame(sea_only)
# head(sea_only_df)

g1 <- 
    ggplot(sea_only) +
    geom_sf(aes(fill = as.numeric(adjusted_observed_value)), color = NA) +
    geom_sf(data = land_japan) +
    scale_fill_viridis_c(option = "magma", name = "Observed val.") +
    theme_minimal()
# ggplot(sea_only) +
#     geom_sf(aes(fill = as.numeric(distance_to_centroid)), color = NA) +
#     scale_fill_viridis_c(option = "plasma", name = "Distance (m)") +
#     theme_minimal()

# [2] Temporal dummy ===========================================================
library(dplyr)
library(scales) # for rescale()

# Set years and months
years <- 1978:(1978 + 42)
months <- 1:12
n_years <- length(years)

# # Latitude and longitude mesh (15-minute intervals)
lat_seq <- seq(25, 45, by = meshsize)
lon_seq <- seq(126, 153, by = meshsize)

# Number of observations per year: linear increase in first half, constant in second half
half_year <- ceiling(n_years / 2)
n_per_year <- c(seq(3000, 3200, length.out = half_year),
rep(3200, n_years - half_year))

# Annual trends of mean values for observation1 and observation2 (10-year cycles)
mean_per_year_obs1 <- 40 + 3 * sin(2 * pi * (years - 1978) / 40)
mean_per_year_obs2 <- 40 + 3 * sin(2 * pi * (years - 1978) / 40 + pi)
mean_per_year_obs3 <- rescale(mean_per_year_obs2, to = c(25, 32)) # max + 5

# Annual trends of zero probabilities (inverse trend)
p_zero_obs1 <- rescale(mean_per_year_obs2, to = c(0.4, 0.6))
p_zero_obs2 <- rescale(mean_per_year_obs1, to = c(0.3, 0.6))

# Function to generate ZIP-distributed integer values
rzip <- function(n, lambda, p_zero) { 
    zeros <- rbinom(n, 1, p_zero) 
    values <- rpois(n, lambda) 
    ifelse(zeros == 1, 0, values) 
}

# Function to adjust monthly observation counts:
# 2nd month (February) most frequent, then March, then other months evenly
adjusted_n_per_month <- function(n_total) {
    n_feb_mar <- round(n_total * 0.3)
    n_feb <- round(n_feb_mar * 0.6)
    n_mar <- n_feb_mar - n_feb
    n_remaining <- n_total - n_feb_mar
    n_other <- rep(round(n_remaining / 10), 10)
    return(c(n_feb, n_mar, n_other))
}

# Generate data
tsdata <- lapply(1:n_years, function(i) {
    n_total <- n_per_year[i]
    n_per_month <- adjusted_n_per_month(n_total)
    do.call(rbind, lapply(1:12, function(m) {
        n_this_month <- n_per_month[m]
        lat_this <- sample(lat_seq, n_this_month, replace = TRUE)
        lon_this <- sample(lon_seq, n_this_month, replace = TRUE)

        # Monthly effect for observation1 (peak in Feb., sardine)
        lambda_obs1 <- mean_per_year_obs1[i] + 2 * sin(2 * pi * (m - 2) / 12)
        # Monthly effect for observation2 (peak in June, anchovy)
        lambda_obs2 <- mean_per_year_obs2[i] + 2 * sin(2 * pi * (m - 6) / 12)
        # Baseline for observation3: yearly value
        base_mu <- mean_per_year_obs3[i]
        # Add monthly effect (peak in September) and latitude effect (lower latitude = higher value)
        mu_obs3 <- base_mu + 10 * sin(2 * pi * (m - 9) / 12 + pi/2) - 0.3 * lat_this
        # Generate observation3 values from normal distribution
        obs3 <- rnorm(n_this_month, mean = mu_obs3, sd = 0.4)
        # Replace negative values with zero
        obs3 <- ifelse(obs3 < 0, 0, obs3)
        data.frame(
            year = years[i],
            month = months[m],
            lat = lat_this,
            lon = lon_this,
            obs = rzip(n_this_month, lambda = lambda_obs1, p_zero = p_zero_obs1[i]),
            obs2 = rzip(n_this_month, lambda = lambda_obs2, p_zero = p_zero_obs2[i]),
            obs3 = round(obs3, digit = 3)
        )
    }))
}) %>% bind_rows()

# Check: Yearly trends ---------------------------------------------------------
## 1. sardine
tsdata %>%
    group_by(year) %>%
    summarise(mean_val = sum(obs)) %>%
    ggplot(aes(x = year, y = mean_val)) + geom_line() + theme_minimal()

## 2. anchovy
tsdata %>%
    group_by(year) %>%
    summarise(mean_val = sum(obs2)) %>%
    ggplot(aes(x = year, y = mean_val)) + geom_line() + theme_minimal()

## 3. SST
tsdata %>%
    group_by(year) %>%
    summarise(mean_val = mean(obs3)) %>%
    ggplot(aes(x = year, y = mean_val)) + geom_line() + theme_minimal()

# Check: Monthly fluctuations --------------------------------------------------
tsdata %>%
    ggplot(aes(x = month, y = obs3, group = month)) + geom_boxplot() + theme_minimal() 

tsdata %>%
    ggplot(aes(x = month, y = obs2, group = month)) + geom_boxplot() + theme_minimal() 

hist(tsdata$obs)
hist(tsdata$obs2)

# Check: Relationship between latitude and obs3 --------------------------------
tsdata %>% 
    ggplot(aes(x = lat, y = obs3)) + geom_point(alpha = 0.1) + theme_minimal() +
    stat_summary(geom = "point", color = "red", shape = 17, size = 3)

# 3. Combine ===================================================================
sea_only_wgs84 <- st_transform(sea_only, crs = 4326)
coords <- st_coordinates(st_centroid(sea_only_wgs84))
spdata <- data.frame(
    lon = floor(coords[, 1] / meshsize) * meshsize,
    lat = floor(coords[, 2] / meshsize) * meshsize,
    area = drop_units(sea_only_wgs84$cell_area_m2),
    Nrec = sea_only_wgs84$adjusted_observed_count
) %>% dplyr::distinct(lon, lat, area, .keep_all = TRUE)

nrow(spdata)
# [1] 48922

nrow(tsdata)
# [1] 135400

head(tsdata)
#   year month      lat      lon obs obs2   obs3
# 1 1978     1 37.50000 149.0833   0    0 12.343
# 2 1978     1 37.50000 138.5833  37   37 12.311
# 3 1978     1 25.91667 139.3333  37    0 15.275
# 4 1978     1 27.50000 150.2500  39    0 15.455
# 5 1978     1 40.50000 149.7500  39    0 11.409
# 6 1978     1 39.83333 129.8333  38   52 11.572

head(spdata)
#        lon      lat     area Nrec
# 1 126.0833 25.08333 81327661    1
# 2 126.1667 25.08333 81280759    1
# 3 126.2500 25.08333 81234175    1
# 4 126.3333 25.08333 81187908    0
# 5 126.4167 25.08333 81141957    0
# 6 126.5000 25.08333 81096323    1

# NOTE: Handling errors in left_join() below
# tsdata[2, ]
# #   year month   lat    lon obs obs2   obs3
# # 2 1978     1 36.75 140.75   0   37 12.142
# tsdata[4213, ]
# #      year month   lat    lon obs obs2  obs3
# # 4213 1979     4 41.25 133.25   0    0 7.104
# 
# data.table::setDT(spdata)
# spdata[lat == 36.75 & lon == 140.75, ]
# #       lon   lat      area  Nrec
# #     <num> <num>     <num> <num>
# # 1: 140.75 36.75 618396979    15
# spdata[lat == 41.25 & lon == 133.25, ]
# # Empty data.table (0 rows and 4 cols): lon,lat,area,Nrec

Egg05min <- dplyr::left_join(tsdata, spdata, by = c("lon", "lat")) %>%
    dplyr::filter(!is.na(area)) %>%
    dplyr::relocate(year, month, lat, lon, area, Nrec) %>%
    dplyr::mutate(obs = obs * area,
                  obs2 = obs2 * area) %>%
    dplyr::rename(Long = lon, Lati = lat, Sardine_Egg = obs,
                  Anchovy_Egg = obs2, SSTmean = obs3)
nrow(Egg05min)
# [1] 49719

head(Egg05min)
#   year month     Lati     Long     area Nrec Sardine_Egg Anchovy_Egg SSTmean
# 1 1978     1 27.50000 150.2500 78026711    1  3043041742           0  15.455
# 2 1978     1 40.50000 149.7500 66467066    1  2592215587           0  11.409
# 3 1978     1 30.91667 138.9167 73560731    3  2353943380           0  14.450
# 4 1978     1 29.91667 136.0000 74537548    2           0  3875952510  14.262
# 5 1978     1 38.75000 141.9167 67019639    6           0  3149923016  12.333
# 6 1978     1 35.66667 140.4167 69718574    7  2928180126  2997898701  13.480

save(Egg05min, file = "../data/Egg05min.Rda")
