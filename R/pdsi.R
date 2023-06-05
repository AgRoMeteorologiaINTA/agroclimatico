#' Índice de Severidad de Sequía de Palmer
#'
#' Calcula el Indice de Severidad de Sequía de Palmer.
#' `psdi_ac()` calcula la versión autocalibrada.
#'
#' @param precipitacion serie de precipitación sin datos faltantes.
#' @param etp serie de evapotranspiración potencial sin datos faltantes.
#' @param cc capacidad de campo (en mm).
#' @param coeficientes list de coeficientes que devuelve `pdsi_coeficientes()`
#'
#' @return
#' Un vector de la misma longitud que `precipitacion` con el PSDI correspondiente a cada caso.
#'
#' @details
#'
#' El Índice de Severidad de Sequía de Palmer, propuesto por Palmer (1965) es usado como indicador para
#' cuantificar las condiciones de sequía a largo plazo. El el cálculo usa constantes definidas empíricamente,
#' originalmente utilizando datos meteorológicos de Kansas y de Iowa en Estados Unidos. Estas constantes no
#' representan necesariamente cualquier región del planeta por lo que puede ser redefinidas para el cálculo
#' del índice usando la función `pdsi_coeficientes()`.
#'
#' Alternativamente, Wells et al. (2004) propuso el Indice de Severidad de Sequía de Palmer Autocalibrado
#' que tiene la capacidad de ajustar las constantes empíricas durante el cálculo del indice.
#'
#' @examples
#' # datos aleatorios
#' datos <- data.frame(fecha = seq(as.Date("1985-01-01"), as.Date("2015-12-01"), by = "1 month"))
#' set.seed(42)
#' datos$pp <- rgamma(nrow(datos), shape = 2, scale = 10)
#' datos$etp <- rgamma(nrow(datos), shape = 1, scale = 3)
#'
#' datos$pdsi_ac <- with(datos, pdsi(pp, etp))
#'
#'
#' @export
pdsi  <- function(precipitacion, etp, cc = 100, coeficientes = pdsi_coeficientes()) {
  missing <- !is.finite(precipitacion) | !is.finite(etp)
  precipitacion[missing] <- NA
  etp[missing] <- NA

  as.vector(pdsi_internal(precipitacion, etp, AWC = cc, sc = FALSE)$X)[seq_along(precipitacion)]
}



#' @export
#' @rdname pdsi
pdsi_ac  <- function(precipitacion, etp, cc = 100, coeficientes = pdsi_coeficientes()) {
  missing <- !is.finite(precipitacion) | !is.finite(etp)
  precipitacion[missing] <- NA
  etp[missing] <- NA

  as.vector(pdsi_internal(precipitacion, etp, AWC = cc, sc = TRUE)$X)[seq_along(precipitacion)]
}

#' @rdname pdsi
#'
#' @param p,q factores de duración
#' @param K1.1,K1.2,K1.3,K2 coeficientes de características climáticas
#' @export

pdsi_coeficientes <- function(p = 0.897,
                              q = 1/3,
                              K1.1 = 1.5,
                              K1.2 = 2.8,
                              K1.3 = 0.5,
                              K2 = 17.67) {

  list(p = p, q = q, K1.1 = K1.1, K1.2 = K1.2, K1.3 = K1.3, K2 = K2)

}
