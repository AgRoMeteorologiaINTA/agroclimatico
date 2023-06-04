#' Grafica variables en Argentina
#'
#' Dadas mediciones de una veriable en puntos ubicados en Argentina,
#' interpola a el resto del territorio usando kriging y grafica con contornos
#'
#' @param valor vector con los valores medidos.
#' @param lon,lat vectores de ubicación en longitud y latitud.
#' @param breaks valores donde graficar los contornos. Si es`NULL` hace 10 contornos.
#' @param escala paleta de colores a usar. Tiene que ser una función que reciba un número
#' y devuelva esa cantidad de colores. Por ejemplo [escala_temp_min].
#' @param cordillera lógico indicando si hay que tapar los datos donde está
#' la coordillera (donde el kriging es particularmente problemático). Si es `TRUE`
#' pinta con gris donde las alturas son masyores a 1500m. También puede ser un número,
#' indicando el valor mínimo desde donde empezar a pintar.
#' @param titulo,subtitulo,fuente,variable texto para usar como título, subtítulo,
#' epígrafe y nombre de la guía.
#' @param xlim,ylim límites en longitud y latitud.
#' @param ... otros argumentos que se pasan a [ggplot2::coord_sf()] o [ggplot2::theme_linedraw()].
#'
#'
#' @return
#' Un objeto ggplot2.
#'
#' @examples
#' \dontrun{
#' set.seed(934)
#' datos_aleatorios <- subset(metadatos_nh(), codigo_nh != "0226")
#' datos_aleatorios <- data.frame(datos_aleatorios,
#'                                pp = rgamma(nrow(datos_aleatorios), 0.5, scale = 1)*25)
#'
#' with(datos_aleatorios, mapear(pp, lon, lat, cordillera = TRUE,
#'                               escala = escala_pp_diaria,
#'                               variable = "mm",
#'                               titulo = "Precipitación aleatoria",
#'                               fuente = "Fuente: datos de ejemplo"))
#' }
#'
#' @export
#' @import ggplot2
mapear <- function(valor, lon, lat,
                   breaks = waiver(),
                   escala = scales::viridis_pal(),
                   cordillera = FALSE,
                   variable = NULL,
                   titulo = NULL,
                   subtitulo = NULL,
                   fuente = NULL) {

  h <- level_mid <- var1.pred <- NULL
  datos <- data.frame(valor = valor, lon = lon, lat = lat)
  sink <- utils::capture.output(campo <- suppressWarnings(kringe(valor, lon, lat)))

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
      geom_contour_filled(data = arg_topo, aes(z = h, fill = after_stat(level_mid)),
                          breaks = breaks_cords),
      scale_fill_gradient(low = "#E2E6E6", high = "#7E7E7E", guide = "none",
                          oob = scales::squish))
  } else {
    cordillera <- NULL
  }

  if (inherits(breaks, "waiver")) {
    if (is.list(escala)) {
      breaks <- escala[["niveles"]]
    } else {
      breaks <- compute_breaks(range(campo$var1.pred, na.rm = TRUE))
    }
  }

  breaks_mid <- breaks[-length(breaks)] + diff(breaks)/2

  if (is.list(escala)) {
    palette <- escala$paleta
  } else {
    palette <- escala
  }

  guide_fill <- guide_colorsteps(barheight = grid::unit(.35, "npc"),
                                 barwidth = grid::unit(.015, "npc"),
                                 show.limits = FALSE)

  if (is.null(variable)) {
    variable <- deparse(substitute(valor))
  }

  ggplot(campo, aes(lon, lat)) +
    geom_contour_filled(aes(z = var1.pred),
                        breaks = breaks) +
    geom_contour(aes(z = var1.pred), color = "gray20", linewidth = 0.2, breaks = breaks) +
    scale_fill_inta(name = variable,
                    escala = palette,
                      guide = guide_fill,
                      breaks = breaks_mid,
                      drop = FALSE) +
    cordillera +
    geom_sf(data = arg_buffer_limite, fill = "white", color = NA, inherit.aes = FALSE) +
    geom_sf(data = mapa_provincias(), fill = NA, color = "black", linewidth = 0.2,
            inherit.aes = FALSE) +
    geom_sf(data = mapa_argentina_limitrofes(), fill = "gray90",
            color = "black", linewidth = 0.2, inherit.aes = FALSE) +
    geom_point(data = datos, size = 0.2) +
    annotation_custom(logoGrob, xmin = -55, xmax = -50, ymin = -23.5, ymax = -20.5) +

    scale_x_continuous(labels = lon_label) +
    scale_y_continuous(labels = lat_label) +
    coord_argentina() +
    theme_inta_mapa() +
    theme(legend.position = c(0.85, 0.3), legend.text = element_text(size = 7),
          legend.background = element_blank()) +
    labs(title = titulo,
         subtitle = subtitulo,
         caption = fuente)


}



#' @export
#' @rdname mapear
coord_argentina <- function(xlim = c(-77, -50), ylim = c(-57, -20), ...) {
  coord_sf(xlim = xlim, ylim = ylim, expand = TRUE, ...)
}

#' @export
#' @rdname mapear
theme_inta_mapa <- function(...) {
  list(
    theme_linedraw(...) ,
    theme(panel.grid = element_blank(),
          axis.title = element_blank()))
}


# From ggplot2
#nocov start
compute_breaks <- function (z_range, bins = NULL, binwidth = NULL, breaks = NULL)  {
  if (!is.null(breaks)) {
    return(breaks)
  }
  if (is.null(bins) && is.null(binwidth)) {
    breaks <- pretty(z_range, 10)
    return(breaks)
  }
  if (!is.null(bins)) {
    accuracy <- signif(diff(z_range), 1)/10
    z_range[1] <- floor(z_range[1]/accuracy) * accuracy
    z_range[2] <- ceiling(z_range[2]/accuracy) * accuracy
    if (bins == 1) {
      return(z_range)
    }
    binwidth <- diff(z_range)/(bins - 1)
    breaks <- scales::fullseq(z_range, binwidth)
    if (length(breaks) < bins + 1) {
      binwidth <- diff(z_range)/bins
      breaks <- scales::fullseq(z_range, binwidth)
    }
    return(breaks)
  }
  scales::fullseq(z_range, binwidth)
}



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

# De metR https://github.com/eliocamp/metR/
ConvertLongitude <- function(lon, group = NULL, from = NULL) {
  if (all(is.na(lon))) return(lon)

  m <- min(lon, na.rm = TRUE)
  if (m < -180) cli::cli_abort(c("Longitud no v\u00e1lida.",
                                 "i" = "La longitud debe ser mayor a 180 grados."))

  M <- max(lon, na.rm = TRUE)
  if (M > 360) cli::cli_abort(c("Longitud no v\u00e1lida.",
                                "i" = "La longitud debe ser menor a 360 grados."))

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
#nocov end
