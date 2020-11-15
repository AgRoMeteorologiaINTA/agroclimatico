#' Formato de salida para Informes
#'
#' Los formatos utilizan como base [rmarkdown::pdf_document()] y una plantilla
#' específica de LaTex.
#'
#' @param ... cualquier argumento que requiera [rmarkdown::pdf_document()].
#'
#' @export
agromet_informe <- function(...) {
  rmarkdown::pdf_document(...,
                          template = system.file("rmarkdown", "templates",
                                                 "agromet_informe", "skeleton",
                                                 'template.tex', package = "agromet")
  )
}
