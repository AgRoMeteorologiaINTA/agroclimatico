library(vdiffr)

context("plots")

# Saltea los tests en CI en linux (fallan por alguna razón en github actions)
# skip_if(Sys.info()[["sysname"]] == "Linux" & isTRUE(as.logical(Sys.getenv("CI"))))
skip_on_os("mac")

test_that("plot metadatos, functiona", {
  expect_doppelganger("metadatos-nh", plot(metadatos_nh()))
})


set.seed(934)
datos_aleatorios <- subset(metadatos_nh(), codigo_nh != "0226")
datos_aleatorios <- data.frame(datos_aleatorios,
                               pp = rgamma(nrow(datos_aleatorios), 0.5, scale = 1)*25)

test_that("mapear", {
  expect_doppelganger("mapear-default",
                      mapear(datos_aleatorios, pp, lon, lat)
  )


  expect_doppelganger("mapear-cordillera",
                      mapear(datos_aleatorios, pp, lon, lat, cordillera = TRUE)
  )

  expect_doppelganger("mapear-cordillera-2000",
                      mapear(datos_aleatorios, pp, lon, lat, cordillera = 2000)
  )

  expect_doppelganger("mapear-escala",
                      mapear(datos_aleatorios, pp, lon, lat, escala = escala_pp_diaria)
  )
  expect_doppelganger("mapear-breaks",
                      mapear(datos_aleatorios, pp, lon, lat, breaks = escala_pp_diaria$niveles)
  )

  expect_doppelganger("mapear-breaks-escala",
                      mapear(datos_aleatorios, pp, lon, lat,
                             breaks = escala_pp_diaria$niveles,
                             escala = escala_pp_diaria$paleta
                      )
  )


  expect_doppelganger("mapear-detalles",
                      mapear(datos_aleatorios, pp, lon, lat,titulo = "Título",
                             variable = "mm",
                             subtitulo = "Subtítulo",
                             fuente = "Fuente")
  )



})



