
skip_if_offline()

# Para testear también que descargue el mapa correctamente
agroclimr:::borrar_cache()

test_mapa <- function(mapa_fun) {

  test_that(deparse(substitute(mapa_fun)), {
    expect_s3_class(mapa_data <- mapa_fun(), "sf")
    expect_true("geometry" %in% colnames(mapa_data))
  })

  return(invisible())

}


mapas_fun = list(arg = mapa_argentina,
                 provincias = mapa_provincias,
                 limitrofes = mapa_argentina_limitrofes,
                 deptos = mapa_departamentos)

sink <- lapply(mapas_fun, test_mapa)


test_that("mapa_provincias filtra", {
  provs <- c("Chaco", "Formosa")
  mapa <- mapa_provincias(provs)
  expect_equal(sort(mapa$name), sort(provs))

  mapa2 <- mapa_provincias(provs, departamentos = TRUE)
  expect_equal(mapa2[1:2, ], mapa)

  expect_error(mapa_provincias(c("Chaco", "CABA")), "no se encontraron entre la lista de posibles provincias.")

})
