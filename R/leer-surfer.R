#' Lee escalas de Surfer
#'
#' Lee archivos en el formato "Level File Format" de Surfer
#' (ver http://surferhelp.goldensoftware.com/topics/level_file_format.htm).
#'
#' @param archivo ruta al archivo a laeer.
#' @param color caracter que indica qué color leer. Puede ser "primario"
#' (que corresponde a "FFGColor") o "secundario" (que corresponde a "FBGColor").
#' No tiene efecto si el archivo es formato LVL1
#'
#' @return
#' Si el archivo es LVL1, un vector con los niveles. Si el archivo es LVL2 o LVL3,
#' una lista con elementos:
#' * `niveles` (numérico), el nivel a que corresopnde cada color.
#' * `colores` (caracter), la representación hexadecimal del color de cada break.
#' * `paleta` (función), una función que toma un entero `n` y devuelve
#' un vector de caracter con `n` colores interpolados a partir de los colores
#' de la escala.
#'
#' @examples
#' escala <- system.file("extdata", "escala_pp_mensual.lvl", package = "agromet")
#'
#' escala_pp_mensual <- leer_surfer(escala)
#'
#' # Valores a los que corresponde cada color
#' escala_pp_mensual$niveles
#'
#' # Ver los colores
#' scales::show_col(escala_pp_mensual$colores)
#'
#' # Obtener más colores usando la misma paleta
#' muchos_colores <- escala_pp_mensual$paleta(25)
#' scales::show_col(muchos_colores)
#'
#' @export
leer_surfer <- function(archivo, color = c("primario", "secundario")) {
  format <- readLines(archivo, n = 1)
  format <- switch(format,
                   "LVL1" = 1,
                   "LVL2" = 2,
                   "LVL3" = 3,
                   stop("El archivo no es un archivo de nivel surfer válido"))


  level <- utils::read.delim(archivo, header = format != 1, sep = " ", skip = 1)
  if (format == 1) {
    return(level[, 1])
  }

  which_color <- switch(color[1],
                        primario = "FFGColor",
                        secundario = "FBGColor")


  breaks <- level[, 1]
  colours <- surfer_to_hex(level[[which_color]])

  list(niveles = c(-Inf, breaks, Inf),
       colores = c(colours[1], colours),
       paleta = grDevices::colorRampPalette(colours))
}


surfer_to_hex <- function(colours) {
  named <- colours %in% names(surfer_colors)
  decoded <- colours

  decoded[named] <- surfer_colors[colours[named]]

  unnamed <- gsub("[RGBA]", "", decoded[!named])
  decoded[!named] <- vapply(strsplit(unnamed, " "),
                            function(x) grDevices::rgb(x[1], x[2], x[3], x[4],
                                                       maxColorValue = 255), "char")
  return(decoded)
}
