#' Olas
#'
#' Identifica periodos de persistencia de un evento definido a partir de alguna
#' condición lógica, por ejemplo días consecutivos donde la temperatura mínima
#' fue igual o menor a 0°C para calcular días acumulados de heladas.
#'
#' La función Puede utilizarse en el contexto de [dplyr::summarise()] y [dplyr::group_by()]
#' para hacer este cálculo por grupos.
#'
#' @param fecha vector de fechas, la serie temporal debe estar completa, sin datos
#' faltantes implicitos.
#' @param ... umbral o umbrales a calcular utilizando operadores lógicos.
#' @param remplaza.na lógico. Por defecto es FALSE, es decír que si la función
#' encuentra un dato faltante "corta" la ola o periodo de persitencia. Si es
#' TRUE, la función reemplaza cada NA por el valor previo en la serie, por lo tanto
#' la ola no se interrumpe si hay NAs.
#'
#' @return Devuelve un data.frame con 3 variables fijas y las posibles variables
#' asociadas al agrupamiento:
#' * `ola` (caracter) nombre de la ola definido por el usuario
#' (si los argumentos de `...` no tienen nombre, se usa `V1`, `V2`, etc...)
#' * `inicio` (fecha) fecha de inicio de la ola o periodo de persistencia
#' * `fin` (fecha) fecha de finalización de la ola o periodo de persistencia
#' * `duracion` (diferencia de fechas, tipo drtn) duración de la ola
#'
#' Si una ola todavía no terminó, fin y longitud son NA.
#'
#' @examples
#' data(NH0358)
#'
#' library(dplyr)
#' NH0358 %>%
#'   reframe(olas(fecha, calor = t_max > 20, frio = t_min <= 0)) %>%
#'   slice_head(n = 10)
#'
#' @export
olas <- function(fecha, ..., remplaza.na = FALSE) {

  # Revisa que la serie temporal este completa, sin NA implicitos
  diff <- fecha - data.table::shift(fecha, 1)
  diff <- c(0, diff[-c(1)])

  na_implicitos <- sum(diff[diff - 1 > 0] - 1)

  if (any(diff > 1)) {
    cli::cli_abort(c(
      "La serie temporal debe estar completa:",
      "i" = "falta{?n} {na_implicitos} tiempo{?s}. Pod\u00F3s usar agroclimatico::completar_serie()."
    ))
  }

  condiciones <- list(...)
  names <- paste0("V", seq_along(condiciones))
  if (is.null(names(condiciones))) {
    names(condiciones) <- names
  } else {
    names(condiciones)[names(condiciones) == ""] <- names[names(condiciones) == ""]
  }

  data.table::rbindlist(lapply(condiciones, computar_olas, fecha = fecha, remplaza.na = remplaza.na),
                        idcol = "ola")
}

computar_olas <- function(fecha, condicion, remplaza.na) {

  # Reemplaza NA por el valor previo (locf). NA no interrumpen la ola
  if (remplaza.na) {
   condicion <- as.logical(data.table::nafill(as.numeric(condicion), "locf"))
  }

  ola <- rle(condicion)

  fin <- fecha[cumsum(ola$lengths)]
  inicio <- fecha[c(1, cumsum(ola$lengths)[-length(ola$lengths)] + 1)]
  data <- data.frame(
    inicio = inicio,
    fin = fin)

  data[nrow(data), ]$fin <- NA   # La última ola todavía no termino. Su longitud es NA.
  data$duracion <- data$fin - data$inicio + 1
  data <- data[ola$values == TRUE & !is.na(ola$values), ]
  data
}

