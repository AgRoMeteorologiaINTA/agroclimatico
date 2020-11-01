#' Computa olas
#'
#' @param Vector de fechas
#' @param Conficion Vector lógico
#'
#'  @export
olas <- function(fecha, condicion) {
  ola <- rle(condicion)
  fin <- fecha[cumsum(ola$lengths)]
  inicio <- fecha[c(1, cumsum(ola$lengths)[-length(ola$lengths)] + 1)]
  data <- data.frame(
    inicio = inicio,
    fin = fin,
    longitud = fin - inicio +1)
  data[nrow(data), ]$longitud <- NA   # La última ola todavía no termino. Su longitud es NA.
  data <- data[ola$values == TRUE, ]
  data
}
