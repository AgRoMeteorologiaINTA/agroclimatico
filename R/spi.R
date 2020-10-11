#' Calcula el SPI
#'
#' @param fecha Vector de fechas.
#' @param precipitacion Vector de precipitacion.
#' @param k Vector numérico con las escalas requeridas
#'
#' @export
spi <- function(fecha, precipitacion, k) {
  promedio <- spi <- NULL
  # browser()
  data <- data.table::data.table(fecha = fecha, pp = precipitacion)
  data_full <- data.table::data.table(fecha = seq(min(fecha), max(fecha),
                                                  by = resolution(fecha)))
  data <- data[data_full, on = "fecha"]

  names(k) <- k

  # browser()
  data[, as.character(k) := frollmean(pp, k, align = "right", na.rm = TRUE)]
  data[, pp := NULL]

  data[,  as.character(k) := spi_fit(.SD), by = .(data.table::month(fecha))]

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


resolution <- function (x) {
  if (is.integer(x) || zero_range(range(x, na.rm = TRUE)))
    return(1)
  x <- unique(as.numeric(x))

  min(diff(sort(x)))
}


.datatable.aware <- TRUE
