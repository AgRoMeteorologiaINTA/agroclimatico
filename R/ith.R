#' Índice de temperatura y humedad
#'
#'  Calcula el índice de temperatura y humedad (ITH)
#'
#'  @param temperatura vector numérico con valores (o valor) de temperatura en grados
#'  centígrados.
#'  @param hr vector númerico (o valor) de la misma longitud que temperatura con la
#'  humedad relativa en porcentaje.
#'
#' @export
ith <- function(temperatura, hr) {

  (1.8 * temperatura + 32) - (0.55 - 0.55 * hr/100) * (1.8 * temperatura - 26)
}
