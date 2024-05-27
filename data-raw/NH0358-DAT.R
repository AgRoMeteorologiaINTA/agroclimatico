## code to prepare `NH` datasets goes here


NH0358 <- agroclimatico::leer_nh("data-raw/NH0358.DAT")

usethis::use_data(NH0358, overwrite = TRUE)

data <- fread("data-raw/NH0114.csv")

NH0114 <- with(data, data.frame(codigo = NA,
                                codigo_nh = gsub("NH", "", id),
                                fecha = fecha,
                                t_max = temperatura_abrigo_150cm_maxima,
                                t_min = temperatura_abrigo_150cm_minima,
                                precip = precipitacion_pluviometrica,
                                lluvia_datos = NA,
                                lluvia = NA,
                                llovizna = NA,
                                granizo = granizo,
                                nieve = nieve,
                                t_aire_max = NA,
                                t_aire_min = temperatura_intemperie_150cm_minima,
                                t_suelo_max = NA,
                                t_suelo_min = NA,
                                heliofania_efec = heliofania_efectiva,
                                heliofania_rel = heliofania_relativa,
                                p_vapor = tesion_vapor_media,
                                hr = humedad_media,
                                td = NA,
                                rocio = rocio_medio,
                                viento_10m = velocidad_viento_1000cm_media,
                                viento_2m = velocidad_viento_200cm_media,
                                rad = radiacion_global,
                                etp = evapotranspiracion_potencial))

usethis::use_data(NH0114, overwrite = TRUE)
