#' Coeficientes de características climáticas
#'
#' Funcion que devuelve los coeficientes de características climáticas necesarios
#' para calcular el Índice de Severidad de Sequía de Palmer con [psdi()] o su
#' versión autocalibrada [psdi_ac()].
#'
#' @param p,q factores de duración
#' @param K1.1,K1.2,K1.3,K2 coeficientes de características climáticas
#'
#' @return
#' Una lista con los coeficientes climáticos.
#'
#' @details
#'
#' El cálculo usa constantes definidas empíricamente, originalmente utilizando datos
#' meteorológicos de Kansas y de Iowa en Estados Unidos. Estas constantes no
#' representan necesariamente cualquier región del planeta por lo que puede ser
#' redefinidas para el cálculo del índice usando la función
#' del índice usando la función `pdsi_coeficientes()`.
#'
#' @references
#' Palmer (1965), Meteorological Drought. U.S Weather Bureau, Washington, D.C. (book).
#'
#' Wells et. al. (2004), A Self-Calibrating Palmer Drought Severity Index. Journal
#' of Climate \doi{10.1175/1520-0442(2004)017<2335:ASPDSI>2.0.CO;2}
#'
#' @export

pdsi_coeficientes <- function(p = 0.897,
                              q = 1/3,
                              K1.1 = 1.5,
                              K1.2 = 2.8,
                              K1.3 = 0.5,
                              K2 = 17.67) {

  list(p = p, q = q, K1.1 = K1.1, K1.2 = K1.2, K1.3 = K1.3, K2 = K2)

}
