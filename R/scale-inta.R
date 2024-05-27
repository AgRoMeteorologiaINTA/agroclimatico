#' Escalas de colores para precipitacion y temperatura
#'
#' Escalas para `color` y `fill` para variables discretas.
#'
#' @param escala escala de colores. Puede ser
#' * una lista con elementos `niveles` y `paleta` (ver `[leer_surfer()]` y [escala_temp_min]).
#' * una función que toma un entero `n` y devuelve
#' un vector de caracter con `n` colores interpolados a partir de los colores
#' de la escala.)
#' @param name nombre de la escala.
#' @param breaks niveles de la escala. Si no es `waiver()`, tiene prioridad por sobre
#' los niveles definidos en `escala`.
#' @param drop lógico que indica si se muestran todos los valores o sólo los
#' presentes en los datos. Por defecto, es `FALSE` si la escala define los niveles usando
#' `breaks` o `escala`.
#' @param ... otros argumentos que se pasan a [ggplot2::scale_fill_manual()] o [ggplot2::discrete_scale()].
#'
#' @return objeto ggproto compatible con ggplot2.
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' pp_enero <- datos_nh_mensual |>
#'   filter(mes == unique(mes)[1])
#'
#' # Los contornos llenos requieren que los datos estén en una grilla
#' # regular, necesitamos hacer una interpolación con kriging.
#' with(pp_enero, agroclimatico:::kringe(precipitacion_mensual, lon, lat)) |>
#' ggplot(aes(lon, lat)) +
#'  geom_contour(aes(z = var1.pred)) +
#'  geom_contour_filled(aes(z = var1.pred)) +
#'  scale_fill_inta(escala = escala_pp_mensual)
#'
#' @rdname scale_inta
#' @export
scale_fill_inta <- function(escala, name = waiver(), breaks = waiver(), drop = waiver(), ...) {
  scale_inta("fill", escala = escala, name = name, breaks = breaks, drop = drop, ...)

}

#' @rdname scale_inta
#' @export
scale_color_inta <- function(escala, name = waiver(), breaks = waiver(), drop = waiver(), ...) {
  scale_inta("color", escala = escala, name = name, breaks = breaks, drop = drop, ...)
}


scale_inta <- function(aes, escala, name = waiver(), breaks = waiver(), drop = waiver(), ...) {

  if (is.list(escala)) {
    palette <- escala[["paleta"]]
    if (inherits(breaks, "waiver")) {
      breaks  <- escala[["niveles"]]
    }

  } else {
    palette <- escala
  }

  if (inherits(drop, "waiver")) {
    if (is.vector(breaks)) {
      drop <- FALSE
    } else {
      drop <- TRUE
    }

  }
  if (is.vector(breaks)) {
    manual_scale <- match.fun(paste0("scale_", aes, "_manual"))
    manual_scale(name = name,
                 drop = drop,
                 values = palette(length(breaks)),
                 ...)

  } else {
    discrete_scale(aes,
                   name = name,
                   scale_name = "precipitacion",
                   palette = palette,
                   breaks = breaks,
                   drop = drop,
                   ...)
  }

}
