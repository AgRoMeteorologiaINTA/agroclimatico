#' Formatea tablas con el estilo de INTA
#'
#' Llama a [kableExtra::kbl()] con valores por defecto apropiados acordes al
#' estilo utilizado por INTA.
#'
#' @param x Una tabla.
#' @param ... Otros argumentos que se pasan a [kableExtra::kbl()]
#'
#' @return tabla, objeto kbl.
#'
#' @examples
#'
#' library(dplyr)
#' library(kableExtra)
#' metadatos <- metadatos_nh()
#'
#' metadatos %>%
#'  head() %>%
#'  select(codigo_nh, estacion) %>%
#'  kable_inta(caption = "Ejemplo",
#'            col.names = c("Código", "Estación")) %>%
#'  kable_styling(latex_options = "scale_down")
#'
#' @export
kable_inta <- function(x, ...) {
  args <- c(list(x = x), list(...))
  defaults <- list(booktabs = TRUE,
                   position = "H", digits = 2,
                   toprule = "\\arrayrulecolor{cyan}\\toprule\\arrayrulecolor{black}",
                   midrule = "\\arrayrulecolor{cyan}\\midrule\\arrayrulecolor{black}")

  args <- replace_if_null(args, defaults)

  do.call(kableExtra::kbl, args)
}


replace_if_null <- function(x, y) {

  for (i in seq_along(y)) {
    name <- names(y)[i]
    if (is.null(x[[name]])) {
      x[[name]] <- y[[name]]
    }
  }
  x

}
