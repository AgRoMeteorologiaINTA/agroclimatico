#' Escalas para precipitacion
#'
#' @param escala escala de colores. Puede ser
#' * una lista con elementos `niveles` y `paleta` (ver `[leer_surfer()]` y [escala_temp_min]).
#' * una función que toma un entero `n` y devuelve
#' un vector de caracter con `n` colores interpolados a partir de los colores
#' de la escala.)
#' @param name nombre de la escala.
#' @param breaks niveles de la escala. Si no es `waiver()`, tiene prioridad por sobre
#' los niveles deinidos en `escala`.
#' @param drop lógico que indica si se muestran todos los valores o sólo los
#' presentes en los datos. Por defecto, es `FALSE` si la escala define los niveles usando
#' `breaks` o `escala`.
#' @param ... otros argumentos que se padan a [ggplot2::scale_fill_manual()] o [ggplot2::discrete_scale()].
#'
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
  # browser()
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
