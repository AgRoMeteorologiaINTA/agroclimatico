#' Calcula el SPI
#'
#' Calcula esl Índice Estandarizado de Precipitación para distintas escalas. El `spi` toma valores
#' de precipitación mientras que el `spei` toma valores del balance entre precipitación y evapotranspiración
#' potencial. Internamente hacen lo mismo; la única diferencia es la distribución teórica
#' usada por defecto para ajustar los datos.
#'
#' @param fecha vector de fechas.
#' @param precipitacion vector de precipitacion.
#' @param balance balance entre precipitación y evapotaranspiración potencial.
#' @param escalas vector numérico con las escalas requeridas
#' @param distribucion distribución usada para ajustar los datos.
#' @param referencia serie de precipitación para usar de referencia en el ajuste
#' a la distribución teórica. Puede ser:
#' * vector lógico o numérico que se usará para filtrar los datos de entrada.
#' * un `data.frame` con columna `fecha` y `precipitacion`. La función
#' `spi_referencia()` es un simple wrapper a `data.frame` que le pone el nombre
#'  correcto a las variables.
#' @param ... argumentos pasados a [SPEI::spi]
#'
#' @return
#' Un data.frame con columnas `fecha`, `escala` y `spi` (o `spei`).
#'
#' @examples
#' # datos aleatorios
#' datos <- data.frame(fecha = seq(as.Date("1985-01-01"), as.Date("2015-12-01"), by = "1 month"))
#' set.seed(42)
#' datos$pp <- rgamma(nrow(datos), shape = 2, scale = 10)
#'
#' with(datos, spi(fecha, pp, escalas = 1:5))
#'
#' # Si entran nuevos datos y hay que calcular el spi nuevamente pero sin que
#' # cambien los valores viejos, hay que usar `referencia`
#'
#' nuevos_datos <- data.frame(fecha = seq(as.Date("2016-01-01"),
#'                                        as.Date("2017-12-01"), by = "1 month"))
#' nuevos_datos$pp <- rgamma(nrow(nuevos_datos), shape = 2, scale = 10)
#' nuevos_datos <- rbind(datos, nuevos_datos)
#'
#' # Usando un vector lógico
#' with(nuevos_datos, spi(fecha, pp, escalas = 1:5,
#'                        referencia = data.table::year(fecha) < 2016))
#'
#' # O un data.frame
#' with(nuevos_datos, spi(fecha, pp, escalas = 1:5,
#'                        referencia = spi_referencia(datos$fecha, datos$pp)))
#'
#'
#' @export
#' @importFrom data.table .BY :=
spi <- function(fecha, precipitacion, escalas, referencia = rep(TRUE, length(fecha)),
                distribucion = "Gamma", ...) {
  . <- pp <- escala <- month <- NULL

  # Le da formato a los datos y calculando las medias móviles (acumuladas)
  data <- data.table::as.data.table(completar_serie(data.frame(fecha = fecha, precipitacion = precipitacion),
                                                    fecha, "1 mes"))
  data[, as.character(escalas) := data.table::frollmean(precipitacion, escalas)]
  data[, precipitacion := NULL]
  data <- data.table::melt(data, id.vars = "fecha", value.name = "precipitacion", variable.name = "escala")
  data[, escala := as.numeric(as.character(escala))][]

  # Hace lo mismo con la serie de referencia
  if (is.vector(referencia)) {
    referencia <- data.table::data.table(fecha = fecha[referencia],
                                         precipitacion = precipitacion[referencia])

  }

  referencia <- data.table::as.data.table(completar_serie(referencia, fecha, "1 mes"))
  referencia[, as.character(escalas) := data.table::frollmean(precipitacion, escalas)]
  referencia[, precipitacion := NULL]
  referencia <- data.table::melt(referencia, id.vars = "fecha", value.name = "precipitacion",
                                 variable.name = "escala")
  referencia[, escala := as.numeric(as.character(escala))][]

  # Fitea los parámetros de la distribución para cada mes y escala
  # Como el resultado son arrays, está metido en una lista.
  params <- referencia[, .(params = .(spi_params(precipitacion, scale = 1, na.rm = TRUE,
                                                 distribucion = distribucion, ...))),
                       by = .(escala, month = data.table::month(fecha))]

  # Calcula el SPI usando los parámetros.
  data[, spi := spi_core(stats::ts(precipitacion, frequency = 1), scale = 1, na.rm = TRUE,
                         distribucion = distribucion,
                         params = params[escala == .BY$escala & month == .BY$month]$params[[1]], ...)$fitted,
       by = .(escala, month = data.table::month(fecha))]
  data[, spi := as.vector(spi)]
  data[, precipitacion := NULL]
  data.table::setnames(data, "fecha", deparse(substitute(fecha)))

  return(as.data.frame(data))
}





#' @export
#' @rdname spi
spei <- function(fecha, balance, escalas, distribucion = "log-Logistic", ...) {
  data <- spi(fecha = fecha, precipitacion = balance, escalas = escalas, distribucion = distribucion, ...)

  data.table::setnames(data, c("fecha", "spi"), c(deparse(substitute(fecha)), "spei"))
  return(data)
}

#' @export
#' @rdname spi
spi_referencia <- function(fecha, precipitacion) {
  data.frame(fecha = fecha, precipitacion = precipitacion)
}


.datatable.aware <- TRUE
