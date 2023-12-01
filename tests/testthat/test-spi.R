data <- data.table::CJ(fecha = seq(as.Date("1985-01-01"),
                                   as.Date("2015-12-01"),
                                   by = "1 month"))

data2 <- data.table::CJ(fecha = seq(as.Date("2016-01-01"),
                                    as.Date("2017-12-01"),
                                    by = "1 month"))

set.seed(42)    # ¡reproducible!
# Simula datos de precipitación con una distribución gamma con un siclo anual en
# el parámetro de forma
data$pp <- rgamma(nrow(data), shape = 1, scale = 10*(cos((lubridate::month(data$fecha) - 1)/11*pi*2) + 1))
data2$pp <- rgamma(nrow(data2), shape = 1, scale = 10*(cos((lubridate::month(data$fecha) - 1)/11*pi*2) + 1))
escalas <- 1:12
data2 <- rbind(data, data2)

test_that("sp(e)i calcula s(e)ip", {
  expect_s3_class(SPI <<- with(data, spi_indice(fecha, pp, escalas)), "data.frame")
  expect_known_value(SPI, "SPI1")

  SPEI <- with(data, spei_indice(fecha, pp, escalas))
  SPEI2 <- with(data, spi_indice(fecha, pp, escalas, distribucion = "log-Logistic"))
  colnames(SPEI2)[colnames(SPEI2) == "spi"] <- "spei"

  expect_equal(SPEI, SPEI2)
})


test_that("sp(e)i considera el período de referenica", {
  # SPI <- with(data, spi(fecha, pp, escalas))
  SPI2 <- with(data2, spi_indice(fecha, pp, escalas, referencia = spi_referencia(data$fecha, data$pp)))

  old <- SPI2[SPI2$fecha <= max(data$fecha), ]
  rownames(old) <- seq_len(nrow(old))
  expect_equal(old, SPI)

  SPI2 <- with(data2, spi_indice(fecha, pp, escalas, referencia = fecha <= max(data$fecha)))

  old <- SPI2[SPI2$fecha <= max(data$fecha), ]
  rownames(old) <- seq_len(nrow(old))
  expect_equal(old, SPI)



})

set.seed(42)
missing <- sample(nrow(data), nrow(data)/10)

data_missing <- data2[!missing]

test_that("sp(e)i funciona con datos faltantes", {

  expect_error(SPI <- with(data_missing, spi_indice(fecha, pp, escalas)), NA)

  fechas_missing <- data2[missing, ]$fecha

  expect_true(all(is.na(SPI[SPI$fecha %in% fechas_missing, "spi"])))


})
