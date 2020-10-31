

# Sacado de https://github.com/indecis-eu/SPEI, fork de
# https://github.com/sbegueria/SPEI que tiene un fix de un error que aparece cuando
# uno pasa parámetros.
# Esto se distribuye bajo licencia GNU GPL.
spi_core <- function(data, scale, kernel=list(type='rectangular',shift=0),
                 distribution='log-Logistic', fit='ub-pwm', na.rm=FALSE,
                 ref.start=NULL, ref.end=NULL, x=FALSE, params=NULL, ...) {

  scale <- as.numeric(scale)
  na.rm <- as.logical(na.rm)
  x <- as.logical(x)
  #if (!exists("data",inherits=F) | !exists("scale",inherits=F)) {
  #	stop('Both data and scale must be provided')
  #}
  if (anyNA(data) && na.rm==FALSE) {
    stop('Error: Data must not contain NAs')
  }
  if (!(distribution %in% c('log-Logistic', 'Gamma', 'PearsonIII'))) {
    stop('Distrib must be one of "log-Logistic", "Gamma" or "PearsonIII"')
  }
  if (!(fit %in% c('max-lik', 'ub-pwm', 'pp-pwm'))) {
    stop('Method must be one of "ub-pwm" (default), "pp-pwm" or "max-lik"')
  }
  if ( (!is.null(ref.start) && length(ref.start)!=2) | (!is.null(ref.end) && length(ref.end)!=2) ) {
    stop('Start and end of the reference period must be a numeric vector of length two.')
  }

  if (!stats::is.ts(data)) {
    data <- stats::ts(as.matrix(data), frequency = 12)
  } else {
    data <- stats::ts(as.matrix(data), frequency=stats::frequency(data), start=stats::start(data))
  }
  m <- ncol(data)
  fr <- stats::frequency(data)


  coef = switch(distribution,
                "Gamma" = array(NA,c(2,m,fr),list(par=c('alpha','beta'),colnames(data),NULL)),
                "log-Logistic" = array(NA,c(3,m,fr),list(par=c('xi','alpha','kappa'),colnames(data),NULL)),
                "PearsonIII" = coef <- array(NA,c(3,m,fr),list(par=c('mu','sigma','gamma'),colnames(data),NULL))
  )

  dim_one = ifelse(distribution == "Gamma", 2, 3)

  if (!is.null(params)) {
    if (dim(params)[1]!=dim_one | dim(params)[2]!=m | dim(params)[3]!=fr) {
      stop(paste('parameters array should have dimensions (', dim_one, ', ', m, ', fr)',sep=' '))
    }
  }

  # Loop through series (columns in data)
  if (!is.null(ref.start) && !is.null(ref.end)) {
    data.fit <- stats::window(data,ref.start,ref.end)
  } else {
    data.fit <- data
  }
  std <- data*NA
  for (s in 1:m) {
    # Cumulative series (acu)
    acu <- data.fit[,s]
    acu.pred <- data[,s]
    if (scale>1) {
      wgt <- kern(scale,kernel$type,kernel$shift)
      acu[scale:length(acu)] <- rowSums(stats::embed(acu,scale)*wgt,na.rm=na.rm)
      acu[1:(scale-1)] <- NA
      acu.pred[scale:length(acu.pred)] <- rowSums(stats::embed(acu.pred,scale)*wgt,na.rm=na.rm)
      acu.pred[1:(scale-1)] <- NA
    }

    # Loop through the months
    for (c in (1:fr)) {
      # Filter month m, excluding NAs
      f <- which(stats::cycle(acu)==c)
      f <- f[!is.na(acu[f])]
      ff <- which(stats::cycle(acu.pred)==c)
      ff <- ff[!is.na(acu.pred[ff])]

      # Monthly series, sorted
      month <- sort.default(acu[f], method="quick")

      if (length(month)==0) {
        std[f] <- NA
        next()
      }

      if(distribution != "log-Logistic"){
        pze <- sum(month==0)/length(month)
        month = month[month > 0]
      }

      if (is.null(params)) {
        month_sd = stats::sd(month,na.rm=TRUE)
        if (is.na(month_sd) || (month_sd == 0)) {
          std[f] <- NA
          next
        }

        # Stop early and assign NAs if month's data is length < 4
        if(length(month) < 4){
          std[ff,s] = NA
          coef[,s,c] <- NA
          next
        }

        # Calculate probability weighted moments based on fit with lmomco or TLMoments
        pwm = switch(fit,
                     "pp-pwm" = lmomco::pwm.pp(month,-0.35,0, nmom=3),
                     #pwm.ub(month, nmom=3)
                     TLMoments::PWM(month, order=0:2)
        )

        # Check L-moments validity
        lmom <- lmomco::pwm2lmom(pwm)
        if ( !lmomco::are.lmom.valid(lmom) || anyNA(lmom[[1]]) || any(is.nan(lmom[[1]])) ){
          next
        }

        # lmom fortran functions need specific inputs L1, L2, T3
        # this is handled by lmomco internally with lmorph
        fortran_vec = c(lmom$lambdas[1:2], lmom$ratios[3])

        # Calculate parameters based on distribution with lmom then lmomco
        f_params = switch(distribution,
                          "log-Logistic" = tryCatch(lmom::pelglo(fortran_vec), error = function(e){ lmomco::parglo(lmom)$para }),
                          "Gamma" = tryCatch(lmom::pelgam(fortran_vec), error = function(e){ lmomco::pargam(lmom)$para }),
                          "PearsonIII" = tryCatch(lmom::pelpe3(fortran_vec), error = function(e){ lmomco::parpe3(lmom)$para })
        )

        # Adjust if user chose log-Logistic and max-lik
        if(distribution == 'log-Logistic' && fit=='max-lik'){
          f_params = parglo.maxlik(month, f_params)$para
        }
      } else {

        f_params = as.vector(params[,s,c])

      }

      # Calculate cdf based on distribution with lmom
      cdf_res = switch(distribution,
                       "log-Logistic" = lmom::cdfglo(acu.pred[ff], f_params),
                       "Gamma" = lmom::cdfgam(acu.pred[ff], f_params),
                       "PearsonIII" = lmom::cdfpe3(acu.pred[ff], f_params)
      )

      std[ff,s] = stats::qnorm(cdf_res)
      coef[,s,c] <- f_params

      # Adjust if user chose Gamma or PearsonIII
      if(distribution != 'log-Logistic'){
        std[ff,s] = stats::qnorm(pze + (1-pze)*stats::pnorm(std[ff,s]))
      }

    } # next c (month)
  } # next s (series)
  colnames(std) <- colnames(data)

  z <- list(call=match.call(expand.dots=FALSE),
            fitted=std,coefficients=coef,scale=scale,kernel=list(type=kernel$type,
                                                                 shift=kernel$shift,values=kern(scale,kernel$type,kernel$shift)),
            distribution=distribution,fit=fit,na.action=na.rm)
  if (x) z$data <- data
  if (!is.null(ref.start)) z$ref.period <- rbind(ref.start,ref.end)

  class(z) <- 'spei'
  return(z)
}


spi_params <- function(pp, ...)  {
  spi_core(stats::ts(pp, frequency = 1), ...)$coefficients
}



spi_fit <- function(pp, ref,...) {
  browser()
  pp <- stats::ts(pp, frequency = 1)
  ref <- stats::ts(ref, frequency = 1)

  coefficients <- spi_core(ref, scale = 1, na.rm = TRUE)$coefficients
  names <- dimnames(coefficients)

  coefficients <- array(rep(coefficients[, 1,1], 12), dim = c(dim(coefficients)[1], 1, 12))

  dimnames(coefficients) <- names

  a <- spi_core(pp, params = coefficients, scale = 1, na.rm = TRUE)

  a

}
