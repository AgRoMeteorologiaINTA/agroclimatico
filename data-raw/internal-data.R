## code to prepare `mapas` dataset goes here
library(magrittr)

mapa_argentina_limitrofes_data <- rnaturalearth::ne_countries(scale = 50, country = c("chile", "uruguay",
                                                "paraguay", "brazil", "bolivia"),
                                    returnclass = "sf")

mapa_argentina_limitrofes_data <- sf::st_crop(mapa_argentina_limitrofes_data,
            xmin = -77, xmax = 0, ymin = -57, ymax = 20)



argentina <- rnaturalearth::ne_countries(country = "argentina", returnclass = "sf",
                                         scale = 10)

arg_buffer <- sf::st_buffer(argentina, 0.7)
arg_buffer_limite <- sf::st_difference(arg_buffer, argentina)



arg_topo <- metR::GetTopography(-75+360, -50+360, -20, -60, resolution = 1/10)
arg_topo[, lon := metR::ConvertLongitude(lon)]


## code to prepare `estaciones_nh` dataset goes here

col_metadatos_nh <- c("n", "codigo_nh", "ext", "nombre", "lat", "lon", "tmp")

estaciones_nh <- readr::read_fwf(file = "data-raw/LISTA.DAT",
                                 readr::fwf_widths(widths = c(2, 4, 5, 31, 9, 9, 7),
                                                   col_names = col_metadatos_nh)) %>%
  dplyr::select(codigo_nh, nombre, lat, lon) %>%
  as.data.frame()



usethis::use_data(arg_buffer, arg_buffer_limite, mapa_argentina_limitrofes_data,
                  arg_topo,estaciones_nh,
                  overwrite = TRUE, internal = TRUE)