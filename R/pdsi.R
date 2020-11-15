#' Índice de Severidad de Sequía de Palmer
#'
#' Usa [scPDSI::pdsi] para calcular el Indice de Severidad de Sequia de Palmer.
#' `psdi_ac()` calcula la versión autocalibrada.
#'
#' @param precipitacion serie de precipitación sin datos faltantes.
#' @param etp serie de evapotranspiración potencial sin datos faltantes.
#' @param cc capacidad de campo (en mm).
#'
#' @return
#' Un vector de la misma longitud que `precipitacion` con el PSDI correspondiente a cada caso.
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
pdsi  <- function(precipitacion, etp, cc = 100) {
  missing <- !is.finite(precipitacion) | !is.finite(etp)
  precipitacion[missing] <- NA
  etp[missing] <- NA

  as.vector(scPDSI::pdsi(precipitacion, etp, AWC = cc, sc = FALSE)$X)[seq_along(precipitacion)]
}



#' @export
#' @rdname pdsi
pdsi_ac  <- function(precipitacion, etp, cc = 100) {
  missing <- !is.finite(precipitacion) | !is.finite(etp)
  precipitacion[missing] <- NA
  etp[missing] <- NA

  as.vector(scPDSI::pdsi(precipitacion, etp, AWC = cc, sc = TRUE)$X)[seq_along(precipitacion)]
}
