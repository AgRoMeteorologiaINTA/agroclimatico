#' Transformaciones estadísticas de variables
#'
#' Las funciones `decil()` y `anomalia_porcentual()` devuelven estos estadísticos
#' para alguna variable dado un periodo de referencia especificado.
#'
#' @param variable vector de observaciones de la variable de interés.
#' @param referencia serie de observaciones para usar de referencia en el ajuste
#' a la distribución teórica. Puede ser:
#' * vector lógico que se usará para filtrar los datos de entrada.
#' * vector numérico de observaciones.
#' * la serie completa (opción por defecto).
#' @param na.rm lógico. Define si se utilizan o no valores faltantes en el cálculo
#' de la anomalía porcentual.
#'
#' @return Devuelve un data.frame con el valor del decil o la anomalía porcentual
#' según sea el caso. Es compatible con [dplyr::group_by()] y [dplyr::mutate()].
#'
#' @examples
#' library(dplyr)
#' data(NH0358)
#'
#' # Deciles de precipitación usando como referencia la serie completa
#' NH0358 %>%
#'   mutate(deciles = decil(precip))
#'
#' # Deciles mensuales
#' NH0358 %>%
#'   group_by(mes = lubridate::month(fecha)) %>%
#'   mutate(deciles = decil(precip))
#'
#' # Definiendo un periodo de referencia
#' NH0358 %>%
#'   mutate(deciles = decil(precip,
#'                          referencia = lubridate::year(fecha) <= 1958))
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
anomalia_porcentual <- function(variable, referencia = rep(TRUE, length(variable)), na.rm = FALSE) {
  if (is.logical(referencia)) {
    referencia <- variable[referencia]
  }
  percent <- variable/mean(referencia, na.rm = na.rm) - 1
  return(percent)
}


