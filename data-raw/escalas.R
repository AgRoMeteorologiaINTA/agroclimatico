## code to prepare `escala_pp_mensual` dataset goes here


escala_pp_mensual <- agroclimr::leer_surfer("inst/extdata/escala_pp_mensual.lvl")
escala_pp_mensual <- list(niveles = escala_pp_mensual$niveles[-c(1:2)],
                          colores = escala_pp_mensual$colores[-c(1:2)],
                          paleta = colorRampPalette(escala_pp_mensual$colores[-c(1:2)]))

escala_temp_max <- agroclimr::leer_surfer("inst/extdata/escala_temp_max.lvl")
escala_temp_min <- agroclimr::leer_surfer("inst/extdata/escala_temp_min.lvl")

colores_precipitacion <- c(rgb(209/255, 227/255, 31/255),
                           rgb(78/255, 195/255, 106/255),
                           rgb(32/255, 147/255, 140/255),
                           rgb(72/255, 23/255, 105/255),
                           rgb(172/255, 3/255, 94/255),
                           rgb(218/255, 63/255, 245/255),
                           rgb(173/255, 129/255, 252/255),
                           rgb(141/255, 172/255, 255/255))

escala_pp_diaria <- list(
  niveles = c(0, 10, 20, 30, 40, 50, 60, 70, Inf),
  colores = c(colores_precipitacion),
  paleta = grDevices::colorRampPalette(colores_precipitacion)
)



usethis::use_data(escala_temp_max, overwrite = TRUE)
usethis::use_data(escala_temp_min, overwrite = TRUE)
usethis::use_data(escala_pp_diaria, overwrite = TRUE)
usethis::use_data(escala_pp_mensual, overwrite = TRUE)
