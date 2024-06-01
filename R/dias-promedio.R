#' Días Promedio
#'
#' Calcula el primer y último día del año promedio a partir de una serie de fechas.
#'
#' Esta función solo requiere un vector de fechas para calcular el primer y último
#' día del año en promedio. Si este vector incluye todos los días de muchos años
#' el resultado será el 1° de enero y el 31 de diciembre. Pero si solo se utilizan
#' las fechas que cumplen con una determinada condición, por ejemplo aquellos
#' días donde la temperatura mínima fue menor o igual a 0°C, entonces devuelve
#' el primer y último día de ocurrencia en promedio para este evento.
#'
#' @param fechas vector de fechas
#'
#' @return La función devuelve un data.frame con 4 variables fijas y variables extras
#' en el caso de hacer el cálculo para distintos grupos:
#' * `variable` (caracter) primer_dia o ultimo_dia según corresponda
#' * `dia` (numérico) día del mes
#' * `mes` (numérico) mes de ocurrencia
#' * `dia_juliano` (numérico) día del año
#'
#' @details
#' La función se puede usar tanto con la sintaxis de base como con dplyr (ver ejemplos).
#' En el caso de dplyr es necesario usar la función [dplyr::reframe()] ya que [dias_promedio()]
#' devuelve un data.frame. Es posible hacer cálculos agrupando datos con [dplyr::group_by()].
#'
#' @examples
#' data(NH0358)
#'
#' # Usando la serie completa
#' dias_promedio(NH0358$fecha)
#'
#' # Filtrando los datos para un determinado evento
#' library(dplyr)
#' NH0358 %>%
#'   filter(t_min <= 0) %>%
#'   reframe(dias_promedio(fecha))
#'
#' # Por grupos, si tenemos por ejemplo más de una estación
#' data(NH0114)
#'
#' rbind(NH0358, NH0114) %>%
#'   filter(t_min <= 0) %>%
#'   group_by(codigo_nh) %>%
#'   reframe(dias_promedio(fecha))
#'
#' @export
dias_promedio <- function(fechas) {
  fecha <- value <- variable <- dia_medio <- . <- NULL

  data.table::data.table(fecha = fechas) %>%
    .[, .(primer_dia = min(fecha),
          ultimo_dia  = max(fecha)), by = .(data.table::year(fecha))] %>%
    data.table::melt(measure.vars = c("primer_dia", "ultimo_dia")) %>%
    .[, fecha := as.Date(paste0("1900-",
                        data.table::month(value),
                        "-",
                        data.table::mday(value)), formar = "%Y-%m-%d")] %>%
    .[, .(dia_medio = mean.Date(fecha, na.rm = TRUE)), by = variable] %>%
    .[, ":="(dia = data.table::mday(dia_medio),
             mes = data.table::month(dia_medio),
             dia_juliano = data.table::yday(dia_medio))] %>%
    .[, dia_medio := NULL] %>%
    .[]
}
