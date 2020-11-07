#' Calcula extremos
#'
#' @param ... Extremos a calcular.
#'
#' @export
umbrales <- function(...) {
  values <- list(...)
  if (length(values) == 0) {
    stop("Ningún extremo definido.")
  }

  datos <- lapply(values, function(x) {
    prop <- mean(x, na.rm = TRUE)
    prop <- ifelse(is.finite(prop), prop, NA_real_)
    data.frame(N = sum(x, na.rm = TRUE),
               prop = prop,
               na = mean(is.na(x)))

  })
  datos <- do.call(rbind, datos)

  if (is.null(names(values))) {
    names <- paste0("V", seq_along(values))
    warning("Los argumentos no tienen nombre asignando nombres: ", paste0(names, collapse = ", "), ".")
  } else {
    names <- names(values)
  }
  datos$extremo <- names
  rownames(datos) <- NULL
  datos
}
