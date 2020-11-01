#' Transforma cualquier variable en deciles
#'
#' @param variable Vector de observaciones de la variable de interés.
#' @param variable_referencia Vector de observacione de la variable de interés para usar de
#' referencia al calcular el valor de los deciles. Por defecto usa todos los
#' datos incluidos en el argumento variable.
#' @param normalizado Valor lógico para indicar si los deciles se devuelven
#' normalizados según (deciles - 5)/10 para que devuelva valores entre -0.5 y 0.5
#' en vez de entre 0 y 10.
#'
#' @export
decil <- function(precipitacion, pp_referencia = precipitacion, normalizado = FALSE) {
  deciles <- stats::ecdf(pp_referencia)(precipitacion)*10
  if (normalizado) {
    deciles <- (deciles - 5)/10
  }
  return(deciles)
}

#' Tranforma una variable en porcentaje con respecto a sus valores normales.
#'
#' @param variable Vector de observaciones de la variable de interés.
#' @param variable_referencia Vector de observacione de la variable para usar de
#' referencia al calcular el valor normal. Por defecto usa todos los
#' datos.
#' @param normalizado Valor lógico para indicar si los deciles se devuelven
#' normalizados según (porcentaje - 1).
#'
#' @export
anomalia_porcentual <- function(precipitacion, pp_referencia = precipitacion,
                                normalizado = TRUE) {
  percent <- precipitacion/mean(pp_referencia)

  if (normalizado) {
    percent <- (percent - 1)
  }
  return(percent)

}


