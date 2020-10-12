#' Calcula el SPI
#'
#' @param fecha Vector de fechas.
#' @param precipitacion Vector de precipitacion.
#' @param escalas Vector numérico con las escalas requeridas
#' @param ... Argumentos pasados a [SPEI::spi]
#'
#' @export
spi <- function(fecha, precipitacion, escalas, ...) {
  promedio <- spi <- NULL  # esto es para que no se queje CRAN
  data <- data.table::data.table(fecha = fecha, pp = precipitacion)

  # "Completa" los datos (convierte datos faltantes implícitos en explícitos)
  data_full <- data.table::data.table(fecha = seq(min(fecha), max(fecha),
                                                  by = "1 month"))  ##TODO ¿cómo generalizar a cualquier resolución?
  data <- data[data_full, on = "fecha"]

  # Fitea el SPI para cada escala
  sink <- lapply(escalas, function(e) {
    data[, as.character(e) := as.data.frame(SPEI::spi(pp, scale = e, na.rm = TRUE, ...)$fitted)]
    })
  data[, pp := NULL]

  # Formatea los datos para la salida
  data <- data.table::melt(data, id.vars = "fecha",
                   variable.name = "escala",
                   value.name = "spi")
  data[, escala := as.numeric(escala)][]

  return(as.data.frame(data))

}




spi_fit <- function(pp) {
  fit <- SPEI::spi(pp, scale = 1, na.rm = TRUE)
  return(as.data.frame(fit$fitted))
}


# Sacado de ggplot2::resolution
resolution <- function (x) {
  x <- unique(x)

  min(diff(sort(x)))
}


.datatable.aware <- TRUE
