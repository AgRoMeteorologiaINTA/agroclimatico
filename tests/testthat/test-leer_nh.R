test_that("leer_nh lee los datos", {

  expect_error(datos <- leer_nh("NH0011.DAT"), NA)
  expect_s3_class(datos, "data.frame")
  expect_equal(nrow(datos), 1388)

  expect_equal(unique(datos$codigo_nh), "0011")
})

test_that("leer_nh está vectorizada", {

  expect_equal(leer_nh(c("NH0011.DAT", "NH00112.DAT")),
               rbind(leer_nh("NH0011.DAT"), leer_nh("NH00112.DAT")))

})

test_that("metadatos_nh devuelve metadatos filtrados", {

  expect_error(metadatos <- metadatos_nh(), NA)
  expect_s3_class(metadatos, "data.frame")

  expect_s3_class(metadatos <- metadatos_nh(lat = c(-45, -40),
                                            lon = c(-70, -64)),
                  "data.frame")
  expect_true(all(metadatos$lat > -45 | metadatos$lat < -40))
  expect_true(all(metadatos$lon > -70 | metadatos$lon < -64))

  codigos <- c("0248", "0229")
  expect_s3_class(metadatos <- metadatos_nh(codigo = codigos),
                  "data.frame")
  expect_equal(unique(metadatos$codigo_nh), codigos)

  provincias <- sort(c("Buenos Aires", "La Pampa"))
  expect_equal(sort(unique(metadatos_nh(provincia = provincias)$provincia)), provincias)


  expect_equal(unique(metadatos_nh(organismo = "INTA")$organismo), "INTA")
})

