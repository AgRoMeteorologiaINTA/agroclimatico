kern <- function(scale, type='rectangular', shift=0) {
	if(!(type %in% c('rectangular', 'triangular', 'circular', 'gaussian'))){
		stop('type must be one of: rectangular, triangular, circular, gaussian')
	}
	#
	s <- scale
	h <- shift
	if(h>=s) {
		stop('Parameter "shift" must be lower than "scale"')
	}
	if(h<0) {
		stop('Parameter "shift" cannot have a negative value')
	}
	if(s<=0) {
		stop('Parameter "scale" must be higher than zero')
	}

	if(s == 1) type == "rectangular"

	k = switch(type,
	       rectangular = rep(1,s),
	       triangular = s:1,
	       circular = (s^2+(1-(1:s)^2)),
	       gaussian = (1/0.4)*1/sqrt(2*pi*1^2)*
	                  exp(-(seq(0,-3,-3/(s-1))-0)^2/2*1^2)
	    )

	if(h) k <- c(k[(h+1):2],k[1:(s-h)])
	return(k/sum(k))
}
