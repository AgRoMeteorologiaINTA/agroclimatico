#' Calcula extremos
#'
#' @param ... Extremos a calcular.
#'
#' @export
extremos <- function(...) {
  values <- list(...)

  datos <- lapply(values, function(x) {

    data.frame(N = sum(x, na.rm = TRUE),
               prop = mean(x, na.rm = TRUE),
               na = sum(is.na(x)))

  })
  datos <- do.call(rbind, datos)
  datos$extremo <- names(values)
  rownames(datos) <- NULL
  datos
}
