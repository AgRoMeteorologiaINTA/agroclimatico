## code to prepare `mapas` dataset goes here

mapa_argentina_limitrofes_data <- rnaturalearth::ne_countries(scale = 50, country = c("chile", "uruguay",
                                                "paraguay", "brazil", "bolivia"),
                                    returnclass = "sf")

mapa_argentina_limitrofes_data <- sf::st_crop(mapa_argentina_limitrofes_data,
            xmin = -77, xmax = 0, ymin = -57, ymax = 20)



usethis::use_data(mapa_argentina_limitrofes_data, overwrite = TRUE, internal = TRUE)
