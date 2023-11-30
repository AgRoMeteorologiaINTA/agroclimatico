test_that("multiplication works", {
  escala <- system.file("extdata", "escala_pp_mensual.lvl", package = "agroclimr")

  expect_known_output(leer_surfer(escala), "escala_surfer")
})
