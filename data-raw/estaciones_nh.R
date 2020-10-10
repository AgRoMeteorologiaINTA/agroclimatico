## code to prepare `estaciones_nh` dataset goes here

col_metadatos_nh <- c("n", "codigo_nh", "ext", "nombre", "lat", "lon", "tmp")

estaciones_nh <- readr::read_fwf(file = "data-raw/LISTA.DAT",
                         fwf_widths(widths = c(2, 4, 5, 31, 9, 9, 7),
                                    col_names = col_metadatos_nh)) %>%
  dplyr::select(codigo_nh, nombre, lat, lon)

usethis::use_data(estaciones_nh, overwrite = TRUE, internal = TRUE)

