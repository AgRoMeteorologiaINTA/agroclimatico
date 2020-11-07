#' Completa una serie de datos
#'
#' @param datos Tabla (data.frame, data.table, tibble) a completar.
#' @param fecha Columna de fecha.
#' @param resolucion Texto para definir la resolución puede ser
#' cualquier valor acpetado por el argumento `by` de la función [seq.Date()]
#' o su traducción al español. Es decir, un valor que contanga "día" (o "dia"),
#' "semana", "mes", "trimestre" o "año", así como los plurales.
#' @param rango Un vector cuyo rango define el período a rellenar. Útil si
#' se quiere que múltiples grupos de datos tengan el mismo rango de fechas.
#'
#' @export
completar_serie <- function(datos, fecha, resolucion, rango = range(fecha)) {
  resolution <- resolve_resolucion(resolucion)

  fecha_string <- deparse(substitute(fecha))
  fecha <- datos[[fecha_string]]

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

  paste(by2[-length(by2)], by2_eng)
}

