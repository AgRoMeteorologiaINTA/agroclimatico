## code to prepare `NH0011.DAT` dataset goes here


NH0011 <- agromet::leer_nh("datos/NH0011.DAT")

usethis::use_data(NH0011, overwrite = TRUE)
