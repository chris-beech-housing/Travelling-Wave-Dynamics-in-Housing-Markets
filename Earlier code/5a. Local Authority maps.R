library(scales)
library(sf)

## Local Authority shapefile from https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-may-2025-boundaries-uk-bfe-v2/about
uk_shapefile <-
  st_read(
    "Data/LAD_MAY_2025_UK_BFE_V2_-5625528287746105523/LAD_MAY_2025_UK_BFE_V2.shp"
  )

## Median prices

uk_shapefile <-
  uk_shapefile |>
  left_join(median_prices, by = c("LAD25NM" = "Local Authority")) |>
  filter(!is.na(median_price))

uk_shapefile |>
  ggplot() +
  geom_sf(aes(fill = log(median_price))) +
  scale_fill_distiller(palette = "RdBu") +
  theme_minimal() +
  theme(legend.position = "none")

## Kmeans results

kmeans_north_south <-
  tibble(
    `Local Authority` = rownames(cluster_data_scaled),
    kmeans = as.numeric(kmeans_result$cluster)
  )

uk_shapefile <-
  uk_shapefile |>
  left_join(kmeans_north_south, by = c("LAD25NM" = "Local Authority")) |>
  filter(!is.na(kmeans))

uk_shapefile |>
  ggplot() +
  geom_sf(aes(fill = factor(kmeans))) +
  scale_fill_manual(values = hue_pal()(2)) +
  theme_minimal() +
  theme(legend.position = "none")
