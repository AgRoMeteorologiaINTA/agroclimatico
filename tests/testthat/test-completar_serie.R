data <- data.table::CJ(estacion = letters[1:4],
                       fechas = seq(as.Date("1985-01-01"),
                                  as.Date("2015-12-01"),
                                  by = "1 month"), sorted = FALSE)
data$values <- 1
set.seed(42)
missing <- sample(nrow(data), nrow(data)/10)
data[sample(.N, .N/10), values := NA]
implicit <- na.omit(data)


test_that("completar_serie agrega todos los datos faltnates", {
  data_new <- implicit[, completar_serie(.SD, fechas, "1 mes", rango = range(data$fechas)), by = estacion]
  expect_equal(data, data_new)
  data_new <- implicit[, completar_serie(.SD, fechas, "1 month", rango = range(data$fechas)), by = estacion]
  expect_equal(data, data_new)

  grouped <- dplyr::group_by(implicit, estacion)
  data_new <- data.table::as.data.table(completar_serie(grouped, fechas, "1 mes", rango = range(data$fechas)))
  expect_equal(data, data_new)


})
