#' Renderiza un archivo Rmd agregando un resumen
#'
#' @param archivo La ruta al archivo.
#'
#'
#' @keywords internal
#' @export
knitr_agromet <- function(archivo, ...) {
  yaml <- rmarkdown::yaml_front_matter(archivo)
  env <- new.env(parent  = globalenv())

  if (!yaml$resumen) {
    return(rmarkdown::render(archivo, envir = env))
  }

  lines <- readLines(archivo)
  dir <- dirname(archivo)

  resumen_start <- grep("## Resumen", lines)

  sections <- grep("^##", lines)

  resumen_lines <- lines[setdiff(seq(1, sections[2]-1), resumen_start)]
  resumen_file <- tempfile("resumen",  tmpdir = dir, fileext = ".Rmd")
  writeLines(resumen_lines, resumen_file)

  # resumen <- rmarkdown::render(resumen_file, output_format = rmarkdown::latex_fragment(),
  #                   output_file = "resumen", envir = env)


  resumen <- callr::r(function(file, dir) {
    on.exit(unlink(file))

    rmarkdown::render(file, output_format = rmarkdown::latex_fragment(),
                      output_file = "resumen")
  }, args = list(file = resumen_file, dir = dir))


  other_lines <- lines[-seq(sections[1], sections[2]-1)]
  other_file <- tempfile("informe",  tmpdir = dir, fileext = ".Rmd")
  writeLines(other_lines, other_file)

  out_file <- tools::file_path_sans_ext(basename(archivo))

  on.exit(unlink(c(other_file, resumen, resumen_file)))
  file <- rmarkdown::render(other_file,
                            output_file = out_file, envir = env)

  file
}
