#' Días promedios
#'
#' Calcula el primer y último día del año promedio para una seria de fechas
#'
#' @param fechas vector de fechas
#'
#' @export
dias_promedio <- function(fechas) {

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
