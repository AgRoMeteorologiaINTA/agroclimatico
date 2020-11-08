library(vdiffr)

context("plots")

# test_that("plot metadatos, functiona", {
#   expect_doppelganger("metadatos-nh", plot(metadatos_nh()))
# })



# test_that("plots tiran warnings si ggplot2 no está instalado", {
#   expect_error(
#     with_mock(requireNamespace = function(...) return(FALSE),
#               plot(metadatos_nh())),
#     "Esta función necesita el paquete ggplot2")
#
# })
