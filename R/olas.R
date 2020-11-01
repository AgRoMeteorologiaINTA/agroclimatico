#' Computa olas
#'
#' Ignora los datos faltnates!
#'
#' @param Vector de fechas
#' @param Conficion Vector lógico
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
  data$longitud <- data$fin - data$inicio
  data <- data[ola$values == TRUE, ]
  data
}
