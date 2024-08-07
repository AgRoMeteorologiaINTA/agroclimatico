% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/umbrales.R
\name{umbrales}
\alias{umbrales}
\title{Calcula la Ocurrencia de eventos a Partir de Umbrales}
\usage{
umbrales(...)
}
\arguments{
\item{...}{umbral o umbrales a calcular utilizando operadores lógicos.}
}
\value{
La función devuelve un data.frame con 4 variables fijas junto a
posibles variables asociadas a los agrupamientos.

Variables fijas
\itemize{
\item \code{extremo} (caracter) nombre del extremo definido por el usuario
(si los argumentos de \code{...} no tienen nombre, se usa \code{V1}, \code{V2}, etc...)
\item \code{N} (numérico) ocurrencia del evento
\item \code{prop} (numérico) proporción de eventos respecto del total de observaciones
\item \code{na} (numérico)  proporción de datos faltantes respecto del total de observaciones
}
}
\description{
La función \code{umbrales()} permite contar la ocurrencia de eventos definidos a
partir de uno o más umbrales.
}
\details{
Debe utilizarse en el contexto de \code{\link[dplyr:summarise]{dplyr::summarise()}} y opcionalmente \code{\link[dplyr:group_by]{dplyr::group_by()}}.
Esto permite calcular distintos umbrales y obtener resultados para distintos grupos.
}
\examples{
data(NH0358)
library(dplyr)

# Sin agrupar devuelve un único valor
NH0358 \%>\%
 summarise(umbrales(t_30 = t_max >= 30))

# Si se agrupan los datos devuelve un valor por cada grupo
NH0358 \%>\%
  group_by(fecha = lubridate::floor_date(fecha, "1 month")) \%>\%
  summarise(umbrales(t_30 = t_max >= 30))

# Se pueden calcular varios umbrales al mismo tiempo
NH0358 \%>\%
 reframe(umbrales(t_30 = t_max >= 30,
                    t_0  = t_min <= 0))

}
