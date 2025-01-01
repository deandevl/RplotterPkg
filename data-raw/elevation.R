library(maps)
library(sf)
library(elevatr)
library(data.table)
library(raster)

kentucky_state <- maps::map(database = 'county', regions = 'kentucky', fill = TRUE, plot = FALSE)

kentucky_counties <- sf::st_as_sf(kentucky_state) |>
  data.table::as.data.table() |>
  _[ID %in% c(
    "kentucky,martin",
    "kentucky,pike",
    "kentucky,knott",
    "kentucky,floyd",
    "kentucky,perry",
    "kentucky,leslie",
    "kentucky,letcher",
    "kentucky,harlan",
    "kentucky,bell"
  ),] |>
  _[, ID := stringr::str_split_i(ID, ",", 2)] |>
  sf::st_as_sf()

elevation <- elevatr::get_elev_raster(kentucky_counties, src = "aws", z = 3,clip = "bbox", expand = .2)

#plot(kentucky_elevation, axes=TRUE)
#plot(sf::st_geometry(kentucky_counties), add=TRUE)

elevation_rat <- raster::deratify(elevation)
kentucky_elevation <- as.data.frame(elevation_rat, xy=TRUE)
colnames(kentucky_elevation) <- c("x","y","elevation")

# RplotterPkg::create_raster_plot(
#   df = kentucky_elevation,
#   aes_x = "x",
#   aes_y = "y",
#   aes_fill = "elevation"
# ) +
#   ggplot2::geom_sf(
#     data = kentucky_counties,
#     aes(x = NULL, y = NULL),
#     alpha = 0,
#     linewidth = 1.5
#   ) +
#   ggplot2::geom_sf_text(
#     data = kentucky_counties,
#     aes(x = NULL, y = NULL, label = ID)
#   )

usethis::use_data(kentucky_counties, overwrite = TRUE)
usethis::use_data(kentucky_elevation, overwrite = TRUE)


