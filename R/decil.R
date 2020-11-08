#' Transformaciones de variables
#'
#'
#'
#' @param variable Vector de observaciones de la variable de interés.
#' @param referencia Vector de observacione de la variable de interés para usar de
#' referencia al calcular la transformación. Por defecto usa todos los
#' datos incluidos en el argumento variable.
#'
#'
#' @export
decil <- function(variable, referencia = variable) {
  deciles <- stats::ecdf(referencia)(variable)*10
  return(deciles)
}


#' @export
#' @rdname decil
anomalia_porcentual <- function(variable, referencia = variable) {
  percent <- variable/mean(referencia) - 1
  return(percent)
}


