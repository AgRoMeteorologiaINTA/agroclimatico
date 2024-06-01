#' Índice de Temperatura y Humedad
#'
#' Calcula el índice de temperatura y humedad (ITH)
#'
#' @param temperatura vector numérico con valores (o valor) de temperatura en grados
#' centígrados.
#' @param hr vector númerico (o valor) de la misma longitud que temperatura con la
#' humedad relativa en porcentaje.
#'
#' @return Devuelve un valor o vector de valores con el ITH. Este valor es utilizado
#' como una medida de la intensidad de las condiciones de estrés por calor a la
#' que se encuentra expuesto el animal. Para bovinos se categoriza como:
#' * **Normal** si ITH < 75
#' * **Alerta** para ITH entre 75 y 78
#' * **Peligro** para ITH entre 79 y 83
#' * **Emergencia** para ITH >= 84
#'
#' @references
#' * Armendano, J. I. ¿Cuándo se generan condiciones de estrés por calor en bovinos para carne?
#' [link](https://inta.gob.ar/sites/default/files/inta_estres_por_calor_bovinos_para_carne.pdf)
#' * Armstrong, DV. 1994. Heat stress interaction with shade and cooling. J. Diary Sci. 77:2004-2050
#'
#' @examples
#' ith(temperatura = 23, hr = 65)
#'
#' data(NH0358)
#'
#' # En el contexto de mutate
#' library(dplyr)
#' NH0358 %>%
#'   mutate(t_media = (t_max + t_min)/2) %>%
#'   mutate(ith = ith(t_media, hr)) %>%
#' slice_head(n = 10)
#'
#' @export
ith <- function(temperatura, hr) {
  (1.8 * temperatura + 32) - (0.55 - 0.55 * hr/100) * (1.8 * temperatura - 26)
}
