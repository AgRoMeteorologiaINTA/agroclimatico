#' Calcula el SPI
#'
#' @param fecha Vector de fechas.
#' @param precipitacion Vector de precipitacion.
#' @param balance Balance entre precipitación y evapotaranspiración potencial.
#' @param escalas Vector numérico con las escalas requeridas
#' @param distribucion Distribución usada para ajustar los datos.
#' @param referencia Serie de precipitación para usar de referencia en el ajuste
#' a la distribución teórica como un data.frame con una columna llamada fecha y otra
#' precipitación. La función `spi_referencia()` es un simple wrapper a `data.frame`
#' que le pone el nombre correcto a las variables. Por defecto, usa toda la serie.
#'
#' @param ... Argumentos pasados a [SPEI::spi]
#'
#' @export
#' @importFrom data.table .BY :=
spi <- function(fecha, precipitacion, escalas, referencia = spi_referencia(fecha, precipitacion),
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

#' Completa una serie de datos
#'
#' @param datos Tabla (data.frame, data.table, tibble) a completar.
#' @param fecha Columna de fecha.
#' @param resolucion Texto para definir la resolución puede ser
#' cualquier valor acpetado por el argumento `by` de la función [seq.Date()]
#' o su traducción al español. Es decir, un valor que contanga "día" (o "dia"),
#' "semana", "mes", "trimestre" o "año", así como los plurales.
#'
#' @export
completar_serie <- function(datos, fecha, resolucion) {
  resolution <- resolve_resolucion(resolucion)

  fecha_string <- deparse(substitute(fecha))
  fecha <- datos[[fecha_string]]

  data_full <- data.frame(fecha = seq(min(fecha), max(fecha), by = resolution))
  colnames(data_full)[1] <-  fecha_string

  datos <- merge(datos, data_full, by = fecha_string)
  datos
}

resolve_resolucion <- function(resolucion) {
  spanish <- c("d\u00edas", "dias", "semanas", "meses", "trimestres", "a\u00f1os")
  english <- c("days", "days", "weeks", "months", "quarters", "years")

  by2 <- strsplit(resolucion, " ")[[1]]

  valid <- pmatch(by2[length(by2)], english)
  if (!is.na(valid)) {
    # Está en inglés
    return(resolucion)
  }


  valid <- pmatch(by2[length(by2)], spanish)
  by2_eng <- english[valid]

  paste(by2[-length(by2)], by2_eng)
}


# Sacado de ggplot2::resolution
resolution <- function (x) {
  x <- unique(x)

  min(diff(sort(x)))
}


.datatable.aware <- TRUE
