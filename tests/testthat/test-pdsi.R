

# datos aleatorios
datos <- data.frame(fecha = seq(as.Date("1985-01-01"), as.Date("2015-12-01"), by = "1 month"))
set.seed(42)
datos$pp <- rgamma(nrow(datos), shape = 2, scale = 10)
datos$etp <- rgamma(nrow(datos), shape = 1, scale = 3)


test_that("pdsi funciona", {
  expect_error(with(datos, pdsi(pp, etp)), NA)
  expect_error(with(datos, pdsi_ac(pp, etp)), NA)
  expect_equal(

    with(datos, pdsi(pp, etp)),
    with(datos, pdsi(pp, etp, coeficientes = pdsi_coeficientes(p = 0.897)))
  )
})

