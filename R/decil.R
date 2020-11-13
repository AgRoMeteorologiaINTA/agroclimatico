#' Transformaciones de variables
#'
#'
#'
#' @param variable vector de observaciones de la variable de interés.
#' @param referencia serie de precipitación para usar de referencia en el ajuste
#' a la distribución teórica. Puede ser:
#' * vector lógico que se usará para filtrar los datos de entrada.
#' * vector numérico de observaciones.
#'
#' @export
decil <- function(variable, referencia = rep(TRUE, length(variable))) {
  if (is.logical(referencia)) {
    referencia <- variable[referencia]
  }

  deciles <- stats::ecdf(referencia)(variable)*10
  return(deciles)
}


#' @export
#' @rdname decil
anomalia_porcentual <- function(variable, referencia = rep(TRUE, length(variable))) {
  if (is.logical(referencia)) {
    referencia <- variable[referencia]
  }
  percent <- variable/mean(referencia) - 1
  return(percent)
}


