#' Completa una Serie de Datos
#'
#' La función permite completar una serie de datos temporales definiendo alguna
#' resolución disponible. Es compatible con datos agrupados por [dplyr::group_by()].
#'
#' @param datos tabla (data.frame, data.table, tibble) a completar.
#' @param fecha variable tipo fecha (Date, IDate, POSIXct, etc).
#' @param resolucion texto que define resolución de salida de los datos. Puede ser
#' cualquier valor aceptado por el argumento `by` de la función [seq.Date()]
#' o su traducción al español. Es decir, "día" (o "dia"),
#' "semana", "mes", "trimestre" o "año", así como los plurales.
#' @param rango un vector cuyo rango define el período a completar. Es útil si
#' se quiere que múltiples grupos de datos tengan el mismo rango de fechas. Es
#' posible definir un rango por fuera del rango original de los datos para homogeneizar
#' series temporales.
#'
#' @return Devuelve un data.frame con las mismas variables de origen. La variable
#' asociada a las fechas ahora se encuentra completa para la resolución indicada
#' y el resto de las variables se completan con NA.
#'
#' @examples
#' # Datos de prueba completos
#' datos <- data.frame(fechas = seq(as.Date("1985-01-01"), as.Date("2015-12-01"),
#'                              by = "1 month"),
#'                     pp = 1)
#'
#' set.seed(42)
#' datos_perdidos <- sample(nrow(datos), nrow(datos)/10)
#' datos$pp[datos_perdidos] <- NA
#' datos_incompletos <- na.omit(datos) #Serie de datos a completar
#'
#' completar_serie(datos_incompletos, fechas, resolucion = "1 mes")
#'
#' @export
completar_serie <- function(datos, fecha, resolucion, rango = range(fecha)) {
  resolution <- resolve_resolucion(resolucion)

  fecha_string <- deparse(substitute(fecha))

  # El default de rango es range(fecha), asi que esta variable
  # tiene que existir.
  fecha <- datos[[fecha_string]]
  force(rango)

  fecha <- seq(min(rango, na.rm = TRUE), max(rango, na.rm = TRUE), by = resolution)

  args <- list(data = datos, fecha = fecha)
  names(args)[[2]] <- fecha_string

  complete_data <- as.data.frame(do.call(tidyr::complete, args))
  complete_data[, colnames(datos)]
  complete_data
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

  number <- by2[-length(by2)]
  if (length(number) != 0) {
    return(paste(by2[-length(by2)], by2_eng))
  }

  return(by2_eng)
}

