
<!-- README.md is generated from README.Rmd. Please edit that file -->

# agroclimatico <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![R build
status](https://github.com/AgRoMeteorologiaINTA/agroclimatico/workflows/R-CMD-check/badge.svg)](https://github.com/AgRoMeteorologiaINTA/agroclimatico/actions)
[![Codecov test
coverage](https://codecov.io/gh/AgRoMeteorologiaINTA/agroclimatico/branch/master/graph/badge.svg)](https://codecov.io/gh/AgRoMeteorologiaINTA/agroclimatico?branch=master)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/599_status.svg)](https://github.com/ropensci/software-review/issues/599)
<!-- badges: end -->

El paquete {agroclimatico} incluye una serie de funciones para calcular
índices y estadísticos climáticos e hidrológicos a partir de datos tidy.
Por ejemplo `umbrales()` permite contar la cantidad de observaciones que
cumplen una determinada condición y `dias_promedio()` devuelve el primer
y último día del año promedio de ocurrencia de un evento.

Otras funciones como `spi_indice()` funcionan como wrappers de funciones
de otros paquetes y buscan ser compatibles con el manejo de datos
[*tidy*](https://es.r4ds.hadley.nz/12-tidy.html) usando por ejemplo los
verbos de dplyr.

Finalmente el paquete incluye una función de graficado de datos
georeferenciados `mapear()` con el estilo y logo propios de [Instituto
Nacional de Tecnología Agropecuaria](https://www.argentina.gob.ar/inta)
(INTA). Como complementos el paquete también provee mapas de Argentina a
nivel nacional, provincial y por departamentos que se pueden usar en el
contexto de ggplot para graficar variables meteorológicas e índices
agroclimáticos.

## Instalación

Para instalar la versión de desarrollo desde
[GitHub](https://github.com/), usá:

``` r
# install.packages("remotes")
remotes::install_github("AgRoMeteorologiaINTA/agroclimatico", build_vignettes = TRUE)
```

## Ejemplos

A continuación se muestra el uso de algunas funciones. Podés encontrar
más ejemplos y usos en la viñeta, con

``` r
vignette("estadisticas-e-indices-climaticos", "agroclimatico")
```

Si se quieren utilizar los datos con formato NH (archivos con columnas
de ancho fijo usado por el INTA para distribuir los datos de las
estaciones meteorológicas de su red) se puede utilizar la función
`leer_nh()` y opcionalmente acceder a sus metadatos con
`metadatos_nh()`.

``` r
library(agroclimatico)
library(dplyr)

archivo <- system.file("extdata", "NH0358.DAT", package = "agroclimatico")

datos <- leer_nh(archivo)
```

### Días promedio

Si por ejemplo se quiere obtener el día de la primera y última helada en
promedio, asumiendo que la ocurrencia de helada corresponde a
temperatura mínima menor a 0°C, se puede utilizar la función
`dias_promedio()` en el contexto de `reframe()`.

``` r
datos %>% 
  filter(t_min <= 0) %>% 
  reframe(dias_promedio(fecha))
#>     variable dia mes dia_juliano
#> 1 primer_dia  16   5         136
#> 2 ultimo_dia  14   9         257
```

### Mapear

La función `mapear()` grafica una variable dada en puntos discretos e
irregulares (en general observaciones o datos derivados de estaciones
meteorológicas) utilizando kriging. Se puede definir el título y
epígrafe y opcionamente se puede incluir la cordillara para enmascarar
la variable a graficar.

``` r
abril <- datos_nh_mensual %>%
  filter(mes == unique(mes)[4]) #datos del cuarto mes en la base, abril.

abril %>% 
  with(mapear(precipitacion_mensual, lon, lat, cordillera = TRUE,
              escala = escala_pp_mensual,
              titulo = "Precipitación en abril de 2019", 
              fuente = "Fuente: INTA",
              variable = "pp"))
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

## Cómo contribuir

Para contribuir con este paquete podés leer la siguiente [guía para
contribuir](https://github.com/AgRoMeteorologiaINTA/agroclimatico/blob/master/.github/CONTRIBUTING.md).
Te pedimos también que revises nuestro [Código de
Conducta](https://www.contributor-covenant.org/es/version/2/0/code_of_conduct/code_of_conduct.md).
