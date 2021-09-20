#' Lectura de Archivos con Formato NH
#'
#' Lee uno o más archivos siempre que mantengan el formato NH de ancho fijo.
#'
#' La función está preparada para leer datos díarios con el formato NH que incluye
#' 25 variables:
#' * `codigo` (caracter)
#' * `codigo_nh` (caracter), variable llave para acceder a los metadatos de estaciones
#' * `fecha` (fecha)
#' * `t_max` (numérico), temperatura máxima en grados centígrados
#' * `t_min` (numérico), temperatura mínima en grados centígrados
#' * `precip` (numérico), precipitación acumulada en milímetros
#' * `lluvia_datos` (numérico), ocurrencia de precipitación 1 indica lluvia
#' * `lluvia` (numérico),  ocurrencia de lluvia
#' * `llovizna` (numérico), ocurrencia de llovizna
#' * `granizo` (numérico), ocurrencia de granizo
#' * `nieve` (numérico), ocurrencia de nieve
#' * `t_min_5cm` (numérico), temperatura mínima a intemperie a 5cm en grados centígrados
#' * `t_min_50cm` (numérico), temperatura mínima a intemperie a 50cm en grados centígrados
#' * `t_suelo_5cm` (numérico), temperatura media del suelo a 5cm en grados centígrados
#' * `t_suelo_10cm` (numérico), temperatura media del suelo a 10cm en grados centígrados
#' * `heliofania_efec` (numérico), heliofanía efectiva en horas
#' * `heliofania_rel` (numérico), heliofanía relativa en porcentaje
#' * `p_vapor` (numérico), tensión de vapor en hPa
#' * `hr` (numérico), humedad relativa en porcentaje
#' * `td` (numérico), temperatura de rocío en grados centígrados
#' * `rocio` (numérico), ocurrencia de rocío
#' * `viento_10m` (numérico), viento a 10 metros en km/h
#' * `viento_2m` (numérico), viento a 2 metros en km/h
#' * `rad` (numérico), radiación en MJ/m2
#' * `etp` (numérico), evapotranspiración en milímetros
#'
#' @param archivos Caracter o vector de caracteres con nombre y ubicación de los
#' archivos a leer.
#'
#' @return Devuelve un data.frame con tantas filas como líneas en el o los archivos leidos y
#' las 25 variables presentes.
#'
#' @examples
#' archivo <- system.file("extdata", "NH0358.DAT", package = "agromet")
#' datos <- leer_nh(archivo)
#'
#' @seealso [metadatos_nh()] devuelve los metadatos de las estaciones meteorológicas.
#'
#' @export
leer_nh <- function(archivos) {

  colnames_nh <- c("codigo", "codigo_nh", "anio", "mes", "dia",
                   "t_max", "t_min", "precip",
                   "lluvia_datos", "lluvia", "llovizna", "granizo",
                   "nieve", "t_min_5cm", "t_min_50cm", "t_suelo_5cm", "t_suelo_10cm",
                   "heliofania_efec", "heliofania_rel", "p_vapor", "hr", "td", "rocio",
                   "viento_10m", "viento_2m", "rad", "etp")
  widths <- c(2, 3, 4, 2, 2, 5, 5, 5, 1, 1, 1, 1, 1,
              5, 5, 5, 5, 4, 3, 4, 3, 5, 1, 3, 5, 4, 4)

  out <- list()

  for (i in seq_along(archivos)) {

    data <- readr::read_fwf(file = archivos[i],
                            col_positions = readr::fwf_widths(widths, col_names = colnames_nh),
                            col_types = "idddddddddddddddddddddddddd",
                            na = c("-99.9", "-99"))

    data$codigo_nh <- formatC(data$codigo_nh, width = 4, flag = "0")
    data$fecha <- as.Date(paste0(data$anio, "-", data$mes, "-", data$dia))
    data$anio <- NULL
    data$mes <- NULL
    data$dia <- NULL
    data$lluvia[data$lluvia == 9] <- NA
    data$llovizna[data$llovizna == 9] <- NA
    data$granizo[data$granizo == 9] <- NA
    data$nieve[data$nieve == 9] <- NA
    data$rocio[data$rocio == 9] <- NA
    data$heliofania_efec[data$heliofania_efec == -9.9] <- NA
    data$p_vapor[data$p_vapor == -9.9] <- NA
    data$rad[data$rad == -9.9] <- NA
    data$etp[data$etp == -9.9] <- NA


    out[[i]] <- data[, c(1:2, 25, 3:24)]

  }

  as.data.frame(data.table::rbindlist(out))
}

#' Tabla de Metadatos de Estaciones NH
#'
#' Devuelve los metadatos de estaciones incluyendo el código único, ubicación (latitud y
#' longitud) y el nombre.
#'
#' Esta función por defecto devuelve la lista completa de estaciones pero alternativamente
#' se puede devolver estaciones espećificar a partir de sus códigos o todas las estaciones
#' incluidas en una región. Además incluye un método plot para visualizar rápidamente
#' la ubicación de las estaciones.
#'
#' @param codigo,provincia,organismo caracter o vector de caracteres para filtrar según código de estación, provincia o provincia.
#' @param lat vector numérico con las latitudes límite de la región de interés.
#' @param lon vector numérico con las longitudes límite de la región de interés
#' (entre -180 y 180).
#'
#' @examples
#' # listado completo de estaciones
#' metadatos_nh()
#'
#' # listado de estaciones específicas
#' metadatos_nh(codigo = c("0001", "0011"))
#'
#' # Filtrar por provincias
#' metadatos_nh(provincia = c("La Pampa", "Catamarca"))
#'
#' # Filtrar por organismo
#' metadatos_nh(organismo = "INTA")
#'
#' # listados de estaciones en una región
#' metadatos_nh(lat = c(-30, -20), lon = c(-65, -55))
#'
#' # gráfico
#' plot(metadatos_nh())
#'
#' @export
metadatos_nh <- function(codigo = NULL, provincia = NULL, organismo = NULL, lat = NULL, lon = NULL) {
  data <- estaciones_nh

  if (!is.null(codigo)) {
    data <- data[data$codigo_nh %in% codigo, ]
  }

  if (!is.null(provincia)) {
    data <- data[data$provincia %in% provincia, ]
  }

  if (!is.null(organismo)) {
    data <- data[data$organismo %in% organismo, ]
  }

  if (!is.null(lat)) {
    data <- data[data.table::between(data$lat, lat[1], lat[2]), ]
  }

  if (!is.null(lon)) {
    data <- data[data.table::between(data$lon, lon[1], lon[2]), ]
  }

  rownames(data) <- NULL
  class(data) <- c("metadatos_nh", class(data))
  return(data)
}

#' @export
plot.metadatos_nh <- function(x, ...) {
  organismo <- lon <- lat <- NULL
  prov <- mapa_provincias()
  prov$name <- NULL


  sf::st_crs(prov) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  sink <- capture.output(old <- sf::sf_use_s2(FALSE))
  on.exit(sf::sf_use_s2(old))

  ggplot2::ggplot(x, ggplot2::aes(lon, lat)) +
    ggplot2::geom_sf(data = prov, inherit.aes = FALSE) +
    ggplot2::geom_point(ggplot2::aes(color = organismo)) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    theme_inta_mapa() +
    # coord_argentina() +
    NULL

}



