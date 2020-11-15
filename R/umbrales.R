#' Calcula la Ocurrencia de eventos a Partir de Umbrales
#'
#' La función `umbrales()` permite contar la ocurrencia de eventos definidos a
#' partir de uno o más umbrales.
#'
#' Debe utilizarse en el contexto de [dplyr::summarise()] y opcionalmente [dplyr::group_by()].
#' Esto permite calcular distintos umbrales y obtener resultados para distintos grupos.
#'
#' @param ... umbral o umbrales a calcular utilizando operadores lógicos.
#'
#' @return La función devuelve un data.frame con 4 variables fijas junto a
#' posibles variables asociadas a los agrupamientos.
#'
#' Variables fijas
#' * `N`` (numérico) ocurrencia del evento
#' *`prop` (numérico) proporción eventos respecto del total de observaciones
#' * `na` (numérico)  proporción de datos faltantes respecto del total de observaciones
#' * `extremo` (caracter) nombre del extremo definido por el usuario
#'
#' @examples
#' archivo <- system.file("extdata", "NH0011.DAT", package = "agromet")
#' datos <- leer_nh(archivo)
#'
#' library(dplyr)
#' # Sin agrupar devuelve un único valor
#' datos %>%
#'  summarise(umbrales(t_30 = t_max >= 30))
#'
#' # Si se agrupan los datos devuelve un valor por cada grupo
#' datos %>%
#'   group_by(fecha = lubridate::floor_date(fecha, "1 month")) %>%
#'   summarise(umbrales(t_30 = t_max >= 30))
#'
#' # Se pueden calcular varios umbrales al mismo tiempo
#' datos %>%
#'  summarise(umbrales(t_30 = t_max >= 30,
#'                     t_0  = t_min <= 0))
#'
#' @export
umbrales <- function(...) {
  values <- list(...)
  if (length(values) == 0) {
    stop("Ning\u00fan extremo definido.")
  }

  datos <- lapply(values, function(x) {
    prop <- mean(x, na.rm = TRUE)
    N <-  sum(x, na.rm = TRUE)

    N <- ifelse(is.finite(prop), N, NA_integer_)
    prop <- ifelse(is.finite(prop), prop, NA_real_)
    data.frame(N = N,
               prop = prop,
               na = mean(is.na(x)))

  })
  datos <- do.call(rbind, datos)

  if (is.null(names(values))) {
    names <- paste0("V", seq_along(values))
    warning("Los argumentos no tienen nombre asignando nombres: ", paste0(names, collapse = ", "), ".")
  } else {
    names <- names(values)
  }
  datos$extremo <- names
  rownames(datos) <- NULL
  datos
}
