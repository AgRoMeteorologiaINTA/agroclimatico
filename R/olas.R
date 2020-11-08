#' Olas
#'
#' Identifica periodos de persistencia de un evento definido a partir de alguna
#' condición lógica, por ejemplo días consecutivos donde la temperatura mínima
#' fue igual o menor a 0°C para calcular días acumulados de heladas.
#'
#' La función es sensible a los datos faltantes, esto quiere decir que si se
#' encuentra con un dato faltante la función "corta" el periodo de persistencia.
#' Puede utilizarse en el contexto de `summarise()` y `group_by()` para hacer
#' este cálculo por grupos.
#'
#' @param fecha vector de fechas.
#' @param condicion vector lógico.
#'
#' @return Devuelve un data frame con 3 variables fijas y las posibles variables
#' asociadas al agrupamiento:
#' * **inicio** (fecha) fecha de inicio de la ola o periodo de persistencia
#' * **fin** (fecha) fecha de finalización de la ola o periodo de persistencia
#' * **longitud** (diferencia de fechas, drtn) duración de la ola
#'
#' Si una ola todavía no terminó, su día de fin es NA y su longitud es NA.
#'
#' @examples
#' archivo <- system.file("extdata", "NH0011.DAT", package = "agromet")
#' datos <- leer_nh(archivo)
#'
#' library(dplyr)
#' datos %>%
#'   summarise(olas(fecha, t_max > 20))
#'
#' @export
olas <- function(fecha, condicion) {
  ola <- rle(condicion)

  fin <- fecha[cumsum(ola$lengths)]
  inicio <- fecha[c(1, cumsum(ola$lengths)[-length(ola$lengths)] + 1)]
  data <- data.frame(
    inicio = inicio,
    fin = fin)

  data[nrow(data), ]$fin <- NA   # La última ola todavía no termino. Su longitud es NA.
  data$longitud <- data$fin - data$inicio + 1
  data <- data[ola$values == TRUE & !is.na(ola$values), ]
  data
}
