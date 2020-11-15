set.seed(42)
N <- 100
x <- rnorm(N)
x[2] <- NA
ref <- rnorm(N, mean = 0.2)

test_that("deciles", {
  d <- decil(ref)

  expect_length(d, N)
  d2 <- decil(c(ref, x), referencia = ref)
  expect_equal(d2[seq_len(N)], d)
})



test_that("percentiles", {
  d <- anomalia_porcentual(ref)
  expect_length(d, N)

  d2 <- anomalia_porcentual(c(ref, x), referencia = ref)
  expect_equal(d2[seq_len(N)], d)
})


test_that("ith", {
  expect_vector(ith(temperatura = runif(100, 10, 30), hr =  runif(100, 0, 100)))
})

file <- system.file("extdata", "NH0011.DAT", package = "agromet")
datos <- leer_nh(file)
test_that("umbrales", {
  expect_warning(cuenta <- umbrales(datos$t_max > 17,
                                    datos$t_min < 10),
                 "Los argumentos no tienen nombre")

  expect_error(umbrales(), "Ningún extremo definido.")
  expect_s3_class(cuenta, "data.frame")
  expect_equal(colnames(cuenta), c("extremo", "N", "prop", "na"))

  expect_equal(umbrales(frio = datos$t_aire_min > 10), data.frame(extremo = "frio",
                                                                  N = NA_integer_,
                                                                  prop = NA_real_,
                                                                  na = 1))
})



test_that("olas", {
  expect_s3_class(calor <- olas(datos$fecha, datos$t_max > 17), "data.frame")
  expect_equal(colnames(calor), c("ola", "inicio", "fin", "longitud"))
  expect_equal(nrow(olas(datos$fecha, datos$t_max > 47)), 0)

})

test_that("dias_promedio", {

  fechas <- seq(as.Date("1990-01-01"), as.Date("1995-12-31"), "1 day")
  expect_s3_class(res <- dias_promedio(fechas), "data.frame")
  expect_equal(res$dia_juliano, c(1, 365))

})



pp <- rgamma(500, shape = 1)
etp <- rgamma(500, shape = 1)
test_that("pdsi", {
  expect_vector(res <- pdsi(pp, etp))
  expect_length(res, length(pp))
})


test_that("pdsi", {
  expect_vector(res <- pdsi_ac(pp, etp))
  expect_length(res, length(pp))
})
