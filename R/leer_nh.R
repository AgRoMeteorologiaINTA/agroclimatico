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
                            col_positions = fwf_widths(widths, col_names = colnames_nh),
                            col_types = "icTdddddddddddddddddddddd",
                            na = c("-99.9", "-99"))


  data$lluvia[data$lluvia == 9] <- NA
  data$llovizna[data$llovizna == 9] <- NA
  data$granizo[data$granizo == 9] <- NA
  data$nieve[data$nieve == 9] <- NA
  data$rocio[data$rocio == 9] <- NA
  data$viento_med <- round(data$viento_med*5/18, digits = 1)

  out[[i]] <- data

}

out %>%
  rbindlist()
}

#' Tabla de metadatos de estaciones NH
#'
#' @export
metadatos_nh <- function() {
  estaciones_nh
}

