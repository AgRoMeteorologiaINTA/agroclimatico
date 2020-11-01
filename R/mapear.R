#' @export
#' @import ggplot2
mapear <- function(valor, lon, lat, escala = "temperatura",
                   mapas = c("limítrofes", "argentina", "provincias"),
                   cordillera = FALSE,
                   titulo = NULL,
                   subtitulo = NULL,
                   fuente = NULL

) {

  datos <- data.frame(valor = valor, lon = lon, lat = lat)
  campo <- kringe(valor, lon, lat)

  logo <- png::readPNG(system.file("logo.png", package = "agromet"))
  logoGrob <- grid::rasterGrob(logo, interpolate = TRUE)


  if (!isFALSE(cordillera)) {
    if (isTRUE(cordillera)) {
      breaks <- seq(1500, 8000, by = 700)
    } else  {
      breaks <- seq(cordillera, 8000, by = 700)
    }
    cordillera <- list(
    ggnewscale::new_scale_fill(),
      geom_contour_filled(data = arg_topo, aes(z = h, fill = stat(level_mid)),
                          breaks = breaks),
    scale_fill_gradient(low = "#E2E6E6", high = "#7E7E7E", guide = "none",
                        oob = scales::squish))
  } else {
    cordillera <- NULL
  }

  ggplot(campo, aes(lon, lat)) +
    geom_contour_filled(aes(z = var1.pred)) +
    geom_contour(aes(z = var1.pred), color = "gray20", size = 0.2) +
    scale_fill_viridis_d(deparse(substitute(valor)),
                         guide = guide_colorsteps(barheight = grid::unit(.3, "npc"))) +
    cordillera +
    geom_sf(data = arg_buffer_limite, fill = "white", color = NA, inherit.aes = FALSE) +
    # geom_sf(data = mapa_argentina(), fill = NA, color = "black", inherit.aes = FALSE) +
    # geom_sf(data = mapa_departamentos(), fill = NA, color = "gray20", size = 0.2,
    # inherit.aes = FALSE) +
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


# de metR
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


