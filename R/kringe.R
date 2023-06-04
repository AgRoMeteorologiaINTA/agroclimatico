# Kriging (interna)
kringe <- function(valor, lon, lat) {
  points <- arg_grid

  datos <- data.frame(valor = valor, lon = lon, lat = lat)
  datos <- stats::na.omit(datos)

  datos <- sf::st_as_sf(datos, coords = c("lon", "lat"))
  points <- sf::st_as_sf(points, coords = c("lon", "lat"))


  k <- automap::autoKrige(valor ~ 1, datos, new_data = points)


  campo <- as.data.frame(k$krige_output)
  coords <- sf::st_coordinates(campo$geometry)
  campo$lon <- coords[,1]
  campo$lat <- coords[,2]

  campo


}
