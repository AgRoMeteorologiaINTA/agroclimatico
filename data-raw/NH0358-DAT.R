## code to prepare `NH0358.DAT` dataset goes here


NH0358 <- agroclimatico::leer_nh("data-raw/NH0358.DAT")

usethis::use_data(NH0358, overwrite = TRUE)
