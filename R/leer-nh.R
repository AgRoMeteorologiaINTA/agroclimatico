#' Lectura de Archivos con Formato NH
#'
#' Lee uno o más archivos siempre que mantengan el formato NH de ancho fijo.
#'
#' La función está preparada para leer datos díarios con el formato NH que incluye
#' 25 variables:
#' * **codigo** (caracter)
#' * **codigo_nh** (caracter), variable llave para acceder a los metadatos de estaciones
#' * **fecha** (fecha)
#' * **t_max** (numérico), temperatura máxima en grados centígrados
#' * **t_min** (numérico), temperatura mínima en grados centígrados
#' * **precip** (numérico), precipitación acumulada en milímetros
#' * **lluvia_datos** (numérico), ocurrencia de precipitación 1 indica lluvia
#' * **lluvia** (numérico),  ocurrencia de lluvia
#' * **llovizna** (numérico), ocurrencia de llovizna
#' * **granizo** (numérico), ocurrencia de granizo
#' * **nieve** (numérico), ocurrencia de nieve
#' * **t_aire_max** (numérico), temperatura máxima del aire en grados centígrados
#' * **t_aire_min** (numérico), temperatura mínima del aire en grados centígrados
#' * **t_suelo_max** (numérico), temperatura máxima del suelo en grados centígrados
#' * **t_suelo_min** (numérico), temperatura mínima del suelo en grados centígrados
#' * **heliofania_efec** (numérico), heliofanía efectiva en horas
#' * **heliofania_rel** (numérico), heliofanía relativa en porcentaje
#' * **p_vapor** (numérico), tensión de vapor en hPa
#' * **hr** (numérico), humedad relativa en porcentaje
#' * **td** (numérico), temperatura de rocío en grados centígrados
#' * **rocio** (numérico), ocurrencia de rocío
#' * **viento_10m** (numérico), viento a 10 metros en km/h
#' * **viento_2m** (numérico), viento a 2 metros en km/h
#' * **rad** (numérico), radiación en MJ/m2
#' * **etp** (numérico), evapotranspiración en milímetros
#'
#' @param archivos Caracter o vector de caracteres con nombre y ubicación de los
#' archivos a leer.
#'
#' @return Devuelve un data frame con tantas filas como líneas en el o los archivos leidos y
#' las 25 variables presentes.
#'
#' @examples
#' archivo <- system.file("extdata", "NH0011.DAT", package = "agromet")
#' datos <- leer_nh(archivo)
#'
#' @seealso [metadatos_nh()] devuelve los metadatos de las estaciones meteorológicas.
#'
#' @export
leer_nh <- function(archivos) {

  colnames_nh <- c("codigo", "codigo_nh", "anio", "mes", "dia",
                   "t_max", "t_min", "precip",
                   "lluvia_datos", "lluvia", "llovizna", "granizo",
                   "nieve", "t_aire_max", "t_aire_min", "t_suelo_max", "t_suelo_min",
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
#' @param codigo caracter o vector de caracteres con los códigos de estaciones de interés.
#' Por defecto devuelve todas las estaciones.
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
#' # listados de estaciones en una región
#' metadatos_nh(lat = c(-30, -20), lon = c(-65, -55))
#'
#' # gráfico
#' plot(metadatos_nh())
#'
#' @export
metadatos_nh <- function(codigo = NULL, lat = NULL, lon = NULL) {

  data <- estaciones_nh

  if (!is.null(codigo)) {
    data <- data[data$codigo_nh %in% codigo, ]
  }

  if (!is.null(lat)) {
    data <- data[data.table::between(data$lat, lat[1], lat[2]), ]
  }

  if (!is.null(lon)) {
    data <- data[data.table::between(data$lon, lon[1], lon[2]), ]
  }
  class(data) <- c("metadatos_nh", class(data))
  return(data)
}

#' @export
plot.metadatos_nh <- function(x, ...) {
  lon <- lat <- NULL

  ggplot2::ggplot(x, ggplot2::aes(lon, lat)) +
    ggplot2::geom_sf(data = mapa_provincias(), inherit.aes = FALSE) +
    ggplot2::geom_point() +
    theme_inta_mapa() +
    coord_argentina()
}

