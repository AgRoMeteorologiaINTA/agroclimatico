# Sacado de https://github.com/Sibada/scPDSI, que no está en CRAN.
# El paquete scPDSI se distribuye bajo licencia GNU GPL-3.
# Authors@R: c(
# Authors@R: c(person("Ruida", "Zhong", email = "zrd2017@163.com", role = c("aut", "cre")),
#              person("Xiaohong", "Chen", email = "eescxh@mail.sysu.edu.cn", role = c("aut", "ctb")),
#              person("Zhaoli", "Wang", email = "wangzhl@scut.edu.cn", role = c("aut", "ctb")),
#              person("Chengguang", "Lai", email = "laichg@scut.edu.cn", role = c("aut", "ctb")),
#              person("Steve", "Goddard", role = c("ctb")),
#              person("Nathan", "Wells", role = c("ctb")),
#              person("Mike", "Hayes", role = c("ctb")))
#
# Computation of the conventional Palmer Drought Severity Index (PDSI)
# and Self-calibrating Palmer Drought Severity Index (scPDSI).

#' @useDynLib agroclimatico
#' @importFrom Rcpp sourceCpp
NULL


#' @importFrom stats ts
pdsi_internal <- function(P, PE, AWC = 100, start = NULL, end = NULL, cal_start = NULL, cal_end = NULL,
                 sc = TRUE, coefficients = pdsi_coeficientes()) {

  freq <- 12

  if(is.null(start)) start <-  1
  if(is.null(end)) end <- start + ceiling(length(P)/freq) - 1

  if(is.null(cal_start)) cal_start <- start
  if(is.null(cal_end)) cal_end <- end

  res <- C_pdsi(P, PE, AWC, start, end, cal_start, cal_end, sc,
                coefficients$K1.1,
                coefficients$K1.2,
                coefficients$K1.3,
                coefficients$K2,
                coefficients$p,
                coefficients$q)

  inter.vars <- res[[1]]
  clim.coes <- res[[2]]
  calib.coes <- res[[3]]
  inter.vars[inter.vars == -999.] <- NA

  out <- list(call = match.call(expand.dots=FALSE),
              X = ts(inter.vars[, 14], start = start, frequency = freq),
              PHDI = ts(inter.vars[, 15], start = start, frequency = freq),
              WPLM = ts(inter.vars[, 16], start = start, frequency = freq),
              inter.vars = ts(inter.vars[, 3:13], start = start, frequency = freq))

  colnames(out$inter.vars) <- c("P", "PE", "PR", "PRO", "PL", "d", "Z",
                                "Prob", "X1", "X2", "X3")

  dim(calib.coes) <- c(2, 5)
  colnames(calib.coes) <- c("m", "b", "p", "q", "K2")
  rownames(calib.coes) <- c('wet', 'dry')

  colnames(clim.coes) <- c("alpha", "beta", "gamma", "delta", "K1")
  rownames(clim.coes) <- month.name

  out$clim.coes <- clim.coes
  out$calib.coes <- calib.coes

  out$self.calib <- sc
  out$range <- c(start, end)
  out$range.ref <- c(cal_start, cal_end)

  class(out) <- "pdsi"
  out
}
