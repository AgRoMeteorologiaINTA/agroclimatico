#' Índice de severidad de sequía de palmer
#'
#' `psdi_ac` calcula el PSDI autocalibrado.
#'
#' @param precipitacion Serie de precipitación sin datos faltantes.
#' @param etp Serie de evapotranspiración potencial sin datos faltantes.
#' @param cad Cantidad de Agua Disponible (en mm).
#
#
#' @export
pdsi  <- function(precipitacion, etp, cad = 100) {
  as.vector(scPDSI::pdsi(precipitacion, etp, AWC = cad, sc = FALSE)$X)[seq_along(precipitacion)]
}



#' @export
#' @rdname pdsi
pdsi_ac  <- function(precipitacion, etp, cad = 100) {
  as.vector(scPDSI::pdsi(precipitacion, etp, AWC = cad, sc = TRUE)$X)[seq_along(precipitacion)]
}
