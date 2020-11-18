# Kriging (interna)
kringe <- function(valor, lon, lat) {
  points <- arg_grid

  datos <- data.frame(valor = valor, lon = lon, lat = lat)
  datos <- stats::na.omit(datos)

  sp::coordinates(datos) <- ~ lon + lat
  sp::coordinates(points) <- ~ lon + lat


  k <- automap::autoKrige(valor ~ 1, datos, new_data = points)


  campo <- as.data.frame(k$krige_output)
  campo


}
