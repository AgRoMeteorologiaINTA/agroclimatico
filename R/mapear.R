#' Grafica variables en Argentina
#'
#' Dadas mediciones de una veriable en puntos ubicados en Argentina,
#' interpola a el resto del territorio usando kriging y grafica con contornos
#'
#' @param valor vector con los valores medidos.
#' @param lon,lat vectores de ubicación en longitud y latitud.
#' @param breaks valores donde graficar los contornos. Si es`NULL` hace 10 contornos.
#' @param paleta paleta de colores a usar. Tiene que ser una función que reciba un número
#' y devuelva esa cantidad de colores. Por ejemplo [paleta_precipitacion()].
#' @param cordillera lógico indicando si hay que tapar los datos donde está
#' la coordillera (donde el kriging es particularmente problemático). Si es `TRUE`
#' pinta con gris donde las alturas son masyores a 1500m. También puede ser un número,
#' indicando el valor mínimo desde donde empezar a pintar.
#' @param titulo,subtitulo,fuente texto para usar como título, subtítulo y
#' epígrafe.
#'
#' @return
#' Un objeto ggplot2.
#'
#' @examples
#' set.seed(934)
#' datos_aleatorios <- data.frame(metadatos_nh(), pp = rgamma(nrow(metadatos_nh()), 1, scale = 5))
#'
#' with(datos_aleatorios, mapear(pp, lon, lat, cordillera = TRUE,
#'                               paleta = paleta_precipitacion,
#'                               titulo = "Precipitación aleatoria",
#'                               fuente = "Fuente: datos de ejemplo"))
#'
#' @export
#' @import ggplot2
mapear <- function(valor, lon, lat,
                   breaks = NULL,
                   paleta = scales::viridis_pal(),
                   # mapas = c("limítrofes", "argentina", "provincias"),
                   cordillera = FALSE,
                   titulo = NULL,
                   subtitulo = NULL,
                   fuente = NULL) {
  h <- level_mid <- var1.pred <- NULL
  datos <- data.frame(valor = valor, lon = lon, lat = lat)
  campo <- kringe(valor, lon, lat)

  logo <- png::readPNG(system.file("logo.png", package = "agromet"))
  logoGrob <- grid::rasterGrob(logo, interpolate = TRUE)


  if (!isFALSE(cordillera)) {
    if (isTRUE(cordillera)) {
      breaks_cords <- seq(1500, 8000, by = 700)
    } else  {
      breaks_cords <- seq(cordillera, 8000, by = 700)
    }
    cordillera <- list(
      ggnewscale::new_scale_fill(),
      geom_contour_filled(data = arg_topo, aes(z = h, fill = stat(level_mid)),
                          breaks = breaks_cords),
      scale_fill_gradient(low = "#E2E6E6", high = "#7E7E7E", guide = "none",
                          oob = scales::squish))
  } else {
    cordillera <- NULL
  }
  # browser()
  #
  #

  if (is.vector(breaks)) {
    breaks_labs <- paste0("(", breaks[-length(breaks)], ", ", breaks[2:length(breaks)], "]")
  } else {
    breaks_labs <- breaks
  }

  guide_fill <- guide_colorsteps(barheight = grid::unit(.3, "npc"), show.limits = TRUE)

  escala <- scale_fill_precipitacion_d(name = deparse(substitute(valor)), breaks = breaks_labs,
                                       guide = guide_fill, palette = paleta)

  # browser()
  ggplot(campo, aes(lon, lat)) +
    geom_contour_filled(aes(z = var1.pred), breaks = breaks) +
    geom_contour(aes(z = var1.pred), color = "gray20", size = 0.2, breaks = breaks) +
    escala +
    cordillera +
    geom_sf(data = arg_buffer_limite, fill = "white", color = NA, inherit.aes = FALSE) +
    geom_sf(data = mapa_provincias(), fill = NA, color = "black", size = 0.2,
            inherit.aes = FALSE) +
    geom_sf(data = mapa_argentina_limitrofes(), fill = "gray90",
            color = "black", size= 0.2, inherit.aes = FALSE) +
    geom_point(data = datos, size = 0.2) +
    annotation_custom(logoGrob, xmin = -55, xmax = -50, ymin = -23.5, ymax = -20.5) +

    scale_x_continuous(labels = lon_label) +
    coord_argentina() +
    theme_inta_mapa() +
    theme(legend.position = c(0.85, 0.25)) +
    labs(title = titulo,
         subtitle = subtitulo,
         caption = fuente)


}

#' Escalas para precipitacion
#'
#' @param name Nombre de la escala.
#' @param breaks Cortes o valores de la escala.
#' @param drop Lógico que indica si se muestran todos los valores o sólo los
#' presentes en los datos.
#' @param palette Paleta a usar. Una función que recibe un parámetro (el número de colores
#' deseados) y devuelve un vector con colores.
#' @param n El número de colores deseados
#' @param ... otros argumentos que se padan a [ggplot2::scale_fill_manual()] o [ggplot2::discrete_scale()].
#'
#' @export
scale_fill_precipitacion_d <- function(name = waiver(), breaks = waiver(), drop = !is.vector(breaks),
                                       palette = paleta_precipitacion, ...) {
  if (is.character(palette)) {
    palette <- match.fun(paste0("paleta_", palette))

  }

  if (is.vector(breaks)) {
    scale_fill_manual(name = name,
                      drop = FALSE,
                      values = stats::setNames(palette(length(breaks)),
                                               breaks), ...
    )
  } else {
    discrete_scale("fill",
                   name = name,
                   scale_name = "precipitacion",
                   palette = palette,
                   breaks = if (is.null(breaks)) waiver() else breaks,
                   drop = drop,
                   ...)
  }


}

#' @export
#' @rdname scale_fill_precipitacion_d
breaks_precipitacion <- function() c(0, 10, 20, 30, 40, 50, 60, 70, Inf)

#' @export
#' @rdname scale_fill_precipitacion_d
paleta_precipitacion <- function(n) grDevices::colorRampPalette(colores_precipitacion)(n)


colores_precipitacion <- c(rgb(209/255, 227/255, 31/255),
                           rgb(78/255, 195/255, 106/255),
                           rgb(32/255, 147/255, 140/255),
                           rgb(72/255, 23/255, 105/255),
                           rgb(172/255, 3/255, 94/255),
                           rgb(218/255, 63/255, 245/255),
                           rgb(173/255, 129/255, 252/255),
                           rgb(141/255, 172/255, 255/255))



# De metR https://github.com/eliocamp/metR/
lat_label <- function(lat, north = "\u00B0N", south = "\u00B0S", zero = "\u00B0") {
  lat <- as.numeric(lat)
  newlat <- ifelse(lat < 0, paste0(abs(lat), south), paste0(lat, north))
  newlat[lat == 0 & !is.na(lat)] <- paste0(lat[lat == 0 & !is.na(lat)], zero)
  return(newlat)
}

lon_label <- function(lon, east = "\u00B0E", west = "\u00B0O", zero = "\u00B0") {
  lon <- as.numeric(lon)
  lon <- ifelse(lon > 180, ConvertLongitude(lon), lon)
  newlon <- ifelse(lon < 0, paste0(abs(lon), west), paste0(lon, east))
  newlon[lon == 0 & !is.na(lon)] <- paste0(lon[lon == 0 & !is.na(lon)], zero)
  newlon[lon == 180 & !is.na(lon)] <- paste0(lon[lon == 180 & !is.na(lon)], zero)
  return(newlon)
}

ConvertLongitude <- function(lon, group = NULL, from = NULL) {
  if (all(is.na(lon))) return(lon)

  m <- min(lon, na.rm = TRUE)
  if (m < -180) stop("lon lower than 180, not a valid longitude")

  M <- max(lon, na.rm = TRUE)
  if (M > 360) stop("lon greater than 360, not a valid longitude")

  lon360 <- FALSE
  lon180 <- FALSE

  new.lon <- lon
  if (is.null(from) || from == 180) {
    lon180 <- which(lon < 0)
    new.lon[lon180] <- new.lon[lon180] + 360
  }
  if (is.null(from) || from == 360) {
    lon360 <- which(lon > 180)
    new.lon[lon360] <- new.lon[lon360] - 360
  }

  if (!is.null(group)) {

    group.c <- as.character(group)
    group.c[lon360 | lon180] <- paste0(group.c[lon360 | lon180], "_2")

    if (is.factor(group)) {
      group.c <- factor(group.c)
    }

    return(list(lon = new.lon, group = group.c))
  }

  return(new.lon)
}
