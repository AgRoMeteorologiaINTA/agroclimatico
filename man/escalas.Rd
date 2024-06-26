% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/escalas.R
\docType{data}
\name{escala_temp_min}
\alias{escala_temp_min}
\alias{escala_temp_max}
\alias{escala_pp_mensual}
\alias{escala_pp_diaria}
\title{Escalas de colores usadas por el INTA}
\format{
Objeto tipo lista con 3 elementos.
}
\usage{
escala_temp_min

escala_temp_max

escala_pp_mensual

escala_pp_diaria
}
\value{
Lista en el mismo formato que devuelve \code{leer_surfer()} con elementos:
\itemize{
\item \code{niveles} (numérico), el nivel a que corresopnde cada color.
\item \code{colores} (caracter), la representación hexadecimal del color de cada break.
\item \code{paleta} (función), una función que toma un entero \code{n} y devuelve
un vector de caracter con \code{n} colores interpolados a partir de los colores
de la escala.
}
}
\description{
Escalas de colores típicas usadas por INTA para distintas variables.
}
\examples{

library(ggplot2)
library(dplyr)

pp_enero <- datos_nh_mensual |>
  filter(mes == unique(mes)[1])

# En el contexto de la función mapear():
mapear(pp_enero, precipitacion_mensual, lon, lat,
escala = escala_pp_mensual, cordillera = TRUE)

# Con ggplot2
# Los contornos llenos requieren que los datos estén en una grilla
# regular, necesitamos hacer una interpolación con kriging.
with(pp_enero, agroclimatico:::kringe(precipitacion_mensual, lon, lat)) |>
ggplot(aes(lon, lat)) +
 geom_contour(aes(z = var1.pred)) +
 geom_contour_filled(aes(z = var1.pred)) +
 scale_fill_inta(escala = escala_pp_mensual)

}
\keyword{datasets}
