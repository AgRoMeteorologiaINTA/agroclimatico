#' Transformaciones de variables
#'
#'
#'
#' @param variable Vector de observaciones de la variable de interés.
#' @param variable_referencia Vector de observacione de la variable de interés para usar de
#' referencia al calcular la transformación. Por defecto usa todos los
#' datos incluidos en el argumento variable.
#'
#' @export
decil <- function(variable, variable_referencia = variable) {
  deciles <- stats::ecdf(variable_referencia)(variable)*10
  return(deciles)
}


#' @export
#' @rdname decil
anomalia_porcentual <- function(variable, variable_referencia = variable) {
  percent <- variable/mean(variable_referencia) - 1
  return(percent)
}


