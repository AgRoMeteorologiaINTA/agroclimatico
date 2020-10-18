## code to prepare `mapas` dataset goes here

mapa_argentina <- rnaturalearth::ne_countries(scale = 50, country = c("argentina", "falkland islands"),
                                                    returnclass = "sf")

mapa_argentina_provincias <- rnaturalearth::ne_states(country = c("argentina",  "falkland islands"), returnclass = "sf")

mapa_argentina_limitrofes <- rnaturalearth::ne_countries(scale = 50, country = c("argentina", "chile", "uruguay",
                                                "paraguay", "brazil", "bolivia",
                                                "falkland islands"),
                                    returnclass = "sf")

usethis::use_data(mapa_argentina, overwrite = TRUE)
usethis::use_data(mapa_argentina, internal = TRUE, overwrite = TRUE)

usethis::use_data(mapa_argentina_provincias, overwrite = TRUE)
usethis::use_data(mapa_argentina_provincias, overwrite = TRUE, internal = TRUE)

usethis::use_data(mapa_argentina_limitrofes, overwrite = TRUE)
usethis::use_data(mapa_argentina_limitrofes, overwrite = TRUE, internal = TRUE)
