#' Observaciones Meteorológicas
#'
#' Datos mensuales de temperatura y precipitación en estaciones meteorológicas
#' en Argentina.
#'
#' @format Un data.table con 381 filas y 10 columnas
#'
#'\describe{
#'  \item{mes}{Fecha en formato %Y-%m-%d}
#'  \item{precipitacion_mensual}{Suma de la precipitación durante el mes en mm.}
#'  \item{temperatura_media_mensual}{Promedio de la temperatura en el mes en grados celsius.}
#'  \item{codigo_nh}{Código de identificación de la estación}
#'  \item{estacion}{Nombre de la estación.}
#'  \item{provincia}{Provincia donde se ubica la estación.}
#'  \item{organismo}{Organismo a cargo de la estación.}
#'  \item{lat}{Latitud.}
#'  \item{lon}{Longitud.}
#'  \item{altura}{Altura sobre el nivel del mar donde se ubica la estación.}
#'}
"datos_nh_mensual"
