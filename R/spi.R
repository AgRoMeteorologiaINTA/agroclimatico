#' Calcula el SPI
#'
#' @param fecha Vector de fechas.
#' @param precipitacion Vector de precipitacion.
#' @param escalas Vector numérico con las escalas requeridas
#'
#' @export
spi <- function(fecha, precipitacion, escalas) {
  promedio <- spi <- NULL  # esto es para que no se queje CRAN
  data <- data.table::data.table(fecha = fecha, pp = precipitacion)

  # "Completa" los datos (convierte datos faltantes implícitos en explícitos)
  data_full <- data.table::data.table(fecha = seq(min(fecha), max(fecha),
                                                  by = resolution(fecha)))
  data <- data[data_full, on = "fecha"]

  # Calcula los promedios corridos para cada escala
  data[, as.character(escalas) := data.table::frollmean(pp, escalas,
                                                        align = "right", na.rm = TRUE)]
  data[, pp := NULL]

  # Fitea el SPI para cada escala y cada mes.
  data[,  as.character(escalas) := spi_fit(.SD), by = .(data.table::month(fecha))]

  # Formatea los datos para la salida
  data <- data.table::melt(data, id.vars = "fecha",
                   variable.name = "escala",
                   value.name = "spi")
  data[, escala := as.numeric(escala)]

  return(data)

}




spi_fit <- function(pp) {
  fit <- SPEI::spi(pp, scale = 1, na.rm = TRUE)
  return(as.data.frame(fit$fitted))
}


# Sacado de ggplot2::resolution
resolution <- function (x) {
  if (is.integer(x) || zero_range(range(x, na.rm = TRUE)))
    return(1)
  x <- unique(as.numeric(x))

  min(diff(sort(x)))
}


.datatable.aware <- TRUE
