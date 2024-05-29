#' Escalas de colores usadas por el INTA
#'
#' Escalas de colores típicas usadas por INTA para distintas variables.
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

#' @format NULL
#' @rdname escalas
"escala_temp_max"

#' @format NULL
#' @rdname escalas
"escala_pp_mensual"

#' @format NULL
#' @rdname escalas
"escala_pp_diaria"

#' @examples
#'
#' library(ggplot2)
#' library(dplyr)
#'
#' pp_enero <- datos_nh_mensual |>
#'   filter(mes == unique(mes)[1])
#'
#' # En el contexto de la función mapear():
#' mapear(pp_enero, precipitacion_mensual, lon, lat,
#' escala = escala_pp_mensual, cordillera = TRUE)
#'
#' # Con ggplot2
#' # Los contornos llenos requieren que los datos estén en una grilla
#' # regular, necesitamos hacer una interpolación con kriging.
#' with(pp_enero, agroclimatico:::kringe(precipitacion_mensual, lon, lat)) |>
#' ggplot(aes(lon, lat)) +
#'  geom_contour(aes(z = var1.pred)) +
#'  geom_contour_filled(aes(z = var1.pred)) +
#'  scale_fill_inta(escala = escala_pp_mensual)


