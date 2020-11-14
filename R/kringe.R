# Kriging (interna)
kringe <- function(valor, lon, lat, mascara) {
  frame <- arg_buffer
  bbox <- sf::st_bbox(frame)


  lons <- seq(bbox[["xmin"]], bbox[["xmax"]], length.out = 100)
  lats <- seq(bbox[["ymin"]], bbox[["ymax"]], length.out = 100)


  points <- expand.grid(lon = lons,
                        lat = lats)
  points_sf <- sf::st_as_sf(points, coords = c("lon", "lat"), crs = 4326)
  inside <-  lengths(sf::st_intersects(points_sf, frame)) != 0

  points <- points[inside, ]

  datos <- data.frame(valor = valor, lon = lon, lat = lat)
  datos <- stats::na.omit(datos)

  sp::coordinates(datos) <- ~ lon + lat
  sp::coordinates(points) <- ~ lon + lat


  k <- automap::autoKrige(valor ~ 1, datos, new_data = points)


  campo <- as.data.frame(k$krige_output)
  campo


}
