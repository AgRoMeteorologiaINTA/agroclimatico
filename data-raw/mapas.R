## code to prepare `mapas` dataset goes here

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


usethis::use_data(arg_buffer, arg_buffer_limite, mapa_argentina_limitrofes_data,
                  arg_topo,


                  overwrite = TRUE, internal = TRUE)
