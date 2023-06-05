#' Formato de salida para Informes
#'
#' Los formatos utilizan como base [rmarkdown::pdf_document()] y una plantilla
#' específica de LaTex.
#'
#' @param ... cualquier argumento que requiera [rmarkdown::pdf_document()].
#' @param latex_engine Caracter con el compilador de latex a usar.
#'
#' @return documento compilado.
#'
#' @example
#' ## Not run:
#'
#' agromet_informe("Informe.Rmd")
#'
#' ## End(Not run)
#'
#' @export
agromet_informe <- function(..., latex_engine = "xelatex") {
  rmarkdown::pdf_document(..., latex_engine = latex_engine,
                          template = system.file("rmarkdown", "templates",
                                                 "agromet_informe", "resources",
                                                 'template.tex', package = "agromet")
  )
}
