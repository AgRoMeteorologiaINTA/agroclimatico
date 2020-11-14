#' Escalas comunes del INTA
#'
#' Escalas típicas usadas por INTA para distintas variables.
#'
#' @format
#' Listas en el mismo formato que devuelve `leer_surfer()` con elementos:
#' * `niveles` (numérico), el nivel a que corresopnde cada color.
#' * `colores` (caracter), la representación hexadecimal del color de cada break.
#' * `paleta` (función), una función que toma un entero `n` y devuelve
#' un vector de caracter con `n` colores interpolados a partir de los colores
#' de la escala.
#'
#' @rdname escalas
"escala_temp_min"

#' @format
#' @rdname escalas
"escala_temp_max"

#' @format
#' @rdname escalas
"escala_pp_mensual"

#' @format
#' @rdname escalas
"escala_pp_diaria"

