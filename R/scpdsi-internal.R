# Computation of the conventional Palmer Drought Severity Index (PDSI)
# and Self-calibrating Palmer Drought Severity Index (scPDSI).

#' @useDynLib agromet
#' @importFrom Rcpp sourceCpp
NULL

.onLoad <- function(libname, pkgname) {
  ops <- options()
  pdsi.ops <- list(
    # Calculating the conventional PDSI
    # Duration factors
    PDSI.p = 0.897,
    PDSI.q = 1/3,

    # Coefficients of climate characteristic
    PDSI.coe.K1.1 = 1.5,
    PDSI.coe.K1.2 = 2.8,
    PDSI.coe.K1.3 = 0.5,

    PDSI.coe.K2 = 17.67
  )

  toset <- !(names(pdsi.ops) %in% names(ops))
  if(any(toset)) {
    options(pdsi.ops[toset])
  }
}

#' @importFrom stats ts
pdsi_internal <- function(P, PE, AWC = 100, start = NULL, end = NULL, cal_start = NULL, cal_end = NULL,
                 sc = TRUE) {

  freq <- 12

  if(is.null(start)) start <-  1;
  if(is.null(end)) end <- start + ceiling(length(P)/freq) - 1

  if(is.null(cal_start)) cal_start <- start
  if(is.null(cal_end)) cal_end <- end

  res <- C_pdsi(P, PE, AWC, start, end, cal_start, cal_end, sc,
                getOption("PDSI.coe.K1.1"),
                getOption("PDSI.coe.K1.2"),
                getOption("PDSI.coe.K1.3"),
                getOption("PDSI.coe.K2"),
                getOption("PDSI.p"),
                getOption("PDSI.q"))

  #names(res) <- c("inter.vars", "clim.coes", "calib.coes")

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
