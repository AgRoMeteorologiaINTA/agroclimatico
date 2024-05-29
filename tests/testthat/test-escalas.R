
test_that("escalas", {

  expect_equal(
    scale_color_inta(escala = escala_pp_diaria),
    scale_color_inta(escala = escala_pp_diaria$paleta, breaks = escala_pp_diaria$niveles)
  )

  escala1 <- scale_color_inta(escala = escala_pp_diaria$paleta)
  escala2 <- ggplot2::discrete_scale("color",
                           name = waiver(),
                           palette = escala_pp_diaria$paleta,
                           breaks = waiver(),
                           drop = TRUE)

  expect_equal(escala1$palette(10), escala2$palette(10))

}
)
