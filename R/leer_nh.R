#' Lectura de archivos del sistema NH
#'
#' Lee uno o más archivos
#'
#' @param archivo caracter o vector de caracteres con el nombre y ubicación de los
#' archivos a leer
#' @export
leer_nh <- function(archivo) {

  colnames_nh <- c("codigo", "codigo_nh", "fecha",
                   "t_max", "t_min", "prcp",
                   "lluvia_datos", "lluvia", "llovizna", "granizo",
                   "nieve", "t_aire_max", "t_aire_min", "t_suelo_max", "t_suelo_min",
                   "heliofania", "heliofania_rel", "p_vapor", "hr", "td", "rocio",
                   "viento_med", "viento_2m", "rad", "etp")
  widths <- c(1, 4, 8, 5, 5, 5, 1, 1, 1, 1, 1,
              5, 5, 5, 5, 4, 3, 4, 3, 5, 1, 3, 5, 4, 4)

  out <- list()

  for (i in 1:length(archivo)) {

    data <- readr::read_fwf(file = archivo,
                            col_positions = readr::fwf_widths(widths, col_names = colnames_nh),
                            col_types = "icTdddddddddddddddddddddd",
                            na = c("-99.9", "-99"))


    data$lluvia[data$lluvia == 9] <- NA
    data$llovizna[data$llovizna == 9] <- NA
    data$granizo[data$granizo == 9] <- NA
    data$nieve[data$nieve == 9] <- NA
    data$rocio[data$rocio == 9] <- NA
    data$heliofania[data$heliofania == -9.9] <- NA
    data$p_vapor[data$p_vapor == -9.9] <- NA
    data$rad[data$rad == -9.9] <- NA
    data$etp[data$etp == -9.9] <- NA


    out[[i]] <- data

  }

  as.data.frame(data.table::rbindlist(out))
}

#' Tabla de metadatos de estaciones NH
#'
#' @param codigo caracter o vector de caracteres con los códigos de estadiones
#' Por defecto devuelve todas las estaciones.
#' @param lat vector numérico con las latitudes límite de la región de interes.
#' @param lon vector numérico con las longitudes límite de la región de interes
#' (entre -180 y 180).
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
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('Esta funci\u00f3n necesita el paquete ggplot2. Puedes instalarlo con `install.packages("ggplot2")')
  }

  ggplot2::ggplot(x, ggplot2::aes(lon, lat)) +
    ggplot2::geom_sf(data = mapa_argentina_provincias, inherit.aes = FALSE) +
    ggplot2::geom_point()
}

