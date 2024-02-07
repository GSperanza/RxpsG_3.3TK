## =======================================================================
## Fit functions
## =======================================================================

#' @title XPSfitAlgorithms
#' @description XPSfitAlgorithms groups all the definitions of the fit functions,
#'   the definition of fit parameters, and the initialization of the parameter
#'   slots of objects of class 'XPSCoreLine'.
#'@return Returns the selected fit algorithm.
#'@examples
#'\dontrun{
#'	XPSfitAlgorithms()
#'}
#'@export
#'

XPSfitAlgorithms <- function() {
     bho <- sapply(fitAlgorithms, function(x) {
  	       cat(sprintf("%15s  %-30s\n", slot(x,"funcName"), slot(x,"description") ))
  	  })
}
## =======================================================================

#>>>> IMPORTANT: IF NEW FUNCTIONS ARE ADDED ALSO  XPSFitCompClass() and getHvalue() HAS TO BE MODIFIED CORRESPONDINGLY !!!!!!




## =======================================================================
## Symmetric functions
## =======================================================================
#' @title Initialize
#' @description Initialize function to initalizes parameter slot for a generic function
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigma function full width at half maximum
#' @export

  Initialize <- function(x, h, mu, sigma) {

    return(x*NA)     #initialize the component with NA values
}


## =======================================================================
#' @title Generic
#' @description Generic is a function to initialize the new Fit Component Slots
#' @param x numeric vector
#' @returns a series of NA of length = length(x)
#' @export

Generic <- function(x) { 
    return(x*NA)     #initialize the component with NA values
}


## =======================================================================
#' @title Gauss function
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigma function full width at half maximum
#' @export

Gauss <- function(x, h, mu, sigma) { 
    return( h*exp(-(2.77258872*((x-mu)/sigma)^2)) )
}


## =======================================================================
#' @title Lorentz function
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigma function full width at half maximum
#' @export

Lorentz <- function(x, h, mu, sigma) { 

     return( h/(1+(4*(((x-mu)/sigma)^2))) ) 
}


## =======================================================================
## Voigt require NORMT3 package
#' @title Voigt function
#' @description The Voigt function is obtained by convolution of
#'   a Gauss and a Lorentz functions using the convolve() function
#'   based on FFT
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigma function full width at half maximum
#' @param lg mix Gauss-Lorentz obtained via FFT convolution
#' @export

Voigt <- function(x, h, mu, sigma, lg) {
  if (lg > 0.95){   #For lg to small or to when (1-lg)~1 convolve has problems
      Cnv <- Lorentz(x, h, mu, sigma) #with lg<0.05 we essentially we have a Gaussian
  } else if (lg < 0.05){
      Cnv <- Gauss(x, h, mu, sigma)   #with lg>0.095 we essentially have a Lorentzian
  } else {
      sigma <- 2*sigma #this to account for the decimation by 2
      LL <- length(x)
      dE <- x[2]-x[1]
      #extension of the RegionToFit since convolve uses a too limited zero padding
      LL2 <- floor(LL/4) #number of point to add at the edge o the energy scale
      xx1 <- seq(from=(x[1]-LL2*dE), to=x[1], by=dE)
      xx2 <- x[2:(LL-1)]
      xx3 <- seq(from=x[LL], to=(x[LL]+LL2*dE), by=dE)
      xx <- c(xx1, xx2, xx3)
      Cnv <- convolve(Gauss(xx, h, mu, (1-lg)*sigma),
                rev(Lorentz(xx, h, mu, lg*sigma)), type="o")
      #Cnv is long 2*LL => decimation to reduce Cnv length to LL
      LLc <- length(Cnv)
      xx <- seq(from=1, to=LLc, by=2)
      Cnv <- Cnv[xx]
      LLc <- length(Cnv)        #Convolve exits with LL-1 values, with decimation may loose one point
      Cnv <- Cnv[(LL2+1):(LLc-LL2)] #eliminates the RegionToFit Extensions
      Cnv <- (Cnv/max(Cnv))*h   #convolution normalization to set the correct amplitude = h
  }
  return(Cnv)
}


## =======================================================================
## Definition: pulses with a temporal intensity profile which has the shape of a sech2 function
## http://www.rp-photonics.com/sech2_shaped_pulses.html
#' @title Sech2 function
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigma function full width at half maximum
#' @export

Sech2 <- function(x, h, mu, sigma) { 

    return( h/( cosh((x-mu)/sigma)*cosh((x-mu)/sigma) ) ) 
}


## =======================================================================
## Gaussian Lorentz cross product
# http://www.casaxps.com/help_manual/line_shapes.htm
#' @title GaussLorentzProd function
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigma function full width at half maximum
#' @param lg mix Gauss-Lorentz
#' @export

GaussLorentzProd <- function(x, h, mu, sigma, lg) {

 	  return( h / (1+(4*lg*((x-mu)/sigma)^2)) * exp(-2.77258872*(1-lg)*((x-mu)/sigma)^2) )
}


## =======================================================================
## Gaussian Lorentz Sum form
# http://www.casaxps.com/help_manual/line_shapes.htm
#' @title GaussLorentzSum function
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigma function full width at half maximum
#' @param lg mix Gauss-Lorentz
#' @export

GaussLorentzSum <- function(x, h, mu, sigma, lg) {

		  return(h*(lg*exp(-2.77258872*((x-mu)/sigma)^2) + (1-lg)*(1/(1+4*(((x-mu)/sigma)^2)))))
}



## =======================================================================
## Asym functions
## =======================================================================



## =======================================================================
## Tail modified Gauss used in Genplot
#' @title AsymmGauss function
#' @description Tail modified Gauss by an exponential function
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigma function full width at half maximum
#' @param asym function asymmetry value
#' @export

AsymmGauss <- function(x, h, mu, sigma, asym) {

	   Tail <- function(x, mu, asym) {
	   	   Y <- vector("numeric", length=length(x))
	   	   Ex <- (x-mu)
	   	   index <- which(Ex >= 0)
	   	   Y[index] <- exp(-abs(Ex[index])*(1-asym)/asym)
	   	   return(Y)
	   }
	   return( Gauss(x, h, mu, sigma) + (h - Gauss(x, h, mu, sigma))*Tail(x, mu, asym) )
}

## =======================================================================
## Tail modified Gauss-Lorentz used in Genplot
#' @title AsymmGaussLorentz function
#' @description Tail modified Gauss-Lorentz Sum modifed by using an exponential function
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigma function full width at half maximum
#' @param asym function asymmetry value
#' @export

AsymmLorentz <- function(x, h, mu, sigma, asym) {

	   Tail <- function(x, mu, asym) {
		          Y <- vector("numeric", length=length(x))
		          Ex <- (x-mu)
		          index <- which(Ex >= 0)
		          Y[index] <- exp(-abs(Ex[index])*(1-asym)/asym)
		          return(Y)
		  }
	   return( Lorentz(x, h, mu, sigma) + (h - Lorentz(x, h, mu, sigma))*Tail(x, mu, asym) )
}


## =======================================================================
## Tail modified Voigt function
#' @title AsymmVoigt
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigma function full width at half maximum
#' @param lg mix Gauss-Lorentz
#' @param asym function asymmetry value
#' @export

AsymmVoigt <- function(x, h, mu, sigma, lg, asym) {

	   Tail <- function(x, mu, sigma, asym) {
            Y <- asym / (sigma^2 + (x - mu)^2)^(1-asym/2)
            return(Y)
    }

	   Y <- vector("numeric", length=length(x))
	   dE <- abs(x[2]-x[1])
	   LL <- length(x)
	   indx1 <- which(x < mu)                          #DX part: normal Voigt
	   Y[indx1] <- Voigt(x, h, mu, sigma, lg)[indx1]
    MM<-max(Y[indx1])
	   indx2 <- which(x >= mu)                        #SX part: Voigt + voigt*tail
    Y[indx2] <- Voigt(x, h, mu, sigma, lg)[indx2]+h*Tail(x, mu, sigma, asym)[indx2]
	   Y[indx2] <- MM * Y[indx2]/max(Y[indx2])
	   return (Y)
}


## =======================================================================
## Tail modified Gauss-Lorentz used in Genplot
#' @title AsymmGaussLorentz function
#' @description Tail modified Gauss-Lorentz Sum modifed by using an exponential function
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigma function full width at half maximum
#' @param lg mix Gauss-Lorentz
#' @param asym function asymmetry value
#' @export

AsymmGaussLorentz <- function(x, h, mu, sigma, lg, asym) {

	   Tail <- function(x, mu, asym) {
		          Y <- vector("numeric", length=length(x))
		          Ex <- (x-mu)
		          index <- which(Ex >= 0)
		          Y[index] <- exp(-abs(Ex[index])*(1-asym)/asym)
		          return(Y)
		  }
	   return( GaussLorentzSum(x, h, mu, sigma, lg) + (h - GaussLorentzSum(x, h, mu, sigma, lg))*Tail(x, mu, asym) )
}


## =======================================================================
## Tail modified Gaussian-Voigt function
#' @title Gaussian-Voigt
#' @description Gaussian-Voigt symmetric Gaussian-Voigt function see CasaXPS
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigma function full width at half maximum
#' @param lg mix Gauss-Lorentz
#' @param asym function asymmetry value
#' @param gv mix Gauss-Voigt
#' @export

AsymmGaussVoigt <- function(x, h, mu, sigma, lg, asym, gv) {
	   Tail <- function(x, mu, asym) {
		          Y <- vector("numeric", length=length(x))
	          	Ex <- (x-mu)
          		index <- which(x >= mu)
          		Y[index] <- exp(-abs(Ex[index])*(1-asym)/asym)
          		return(Y)
	   }
	   return((gv*Gauss(x, h, mu, sigma)+(h - Gauss(x, h, mu, sigma))*Tail(x, mu, asym)) + (1-gv)*Voigt(x, h, mu, sigma, lg) )
}


## =======================================================================
## Asym Gaussian Lorentz cross product from UNIFIT Publication
#'@title AsymmGaussLorentzProd function
#'@description Asym Gaussian Lorentz cross product from UNIFIT Publication
#'@param x numeric vector
#'@param h function amplitude
#'@param mu position of function center
#'@param sigma function full width at half maximum
#'@param asym function asymmetry value
#'@param lg mix Gauss-Lorentz
#'@export

AsymmGaussLorentzProd <- function(x, h, mu, sigma, asym, lg) {

	   z <- (x-mu)/(sigma + asym * (x-mu))
   	return(h / (1+(4*lg*(z)^2)) * exp(-2.77258872*(1-lg)*(z)^2) )
}

## =======================================================================
## Pure DoniachSunjic Lineshape
## (see Leiro et al. J. El. Spectr. Rel. Phen. 128, 205, (2003).
#' @title DoniachSunjic function
#' @description Correct version of the Doniach-Sunjic function (see Wertheim PRB 25(3), 1987, (1982))
#'   see Leiro et al. J. El. Spectr. Rel. Phen. 128, 205, (2003)
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigmaDS function full width at half maximum
#' @param asym function asymmetry value
#' @export

DoniachSunjic <- function(x, h, mu, sigmaDS, asym) {

 	  DS <- h/4*( (gamma(1-asym)/((mu-x)^2+(sigmaDS/2)^2)^((1-asym)/2) ) * cos((pi*asym)/2+(1-asym)*atan((mu-x)*2/sigmaDS)) )  #DoniachSunjic
    return (DS)
}


## =======================================================================
## DoniachSunjic(x, h, mu, sigmaDS, asym, tail)
#corrected version of  Doniach -Sunjic Lineshape adding a Tail
#on the low BE (high KE) side following the model of Wertheim PRB 25(3), 1987, (1982)
#(see also Leiro et al. J. El. Spectr. Rel. Phen. 128, 205, (2003).
#mu-x instead of x-mu to work on energies < mu
#tail damps the lineshape at low BE

#' @title DoniachSunjic function
#' @description Version of the Doniach-Sunjic function (see Wertheim PRB 25(3), 1987, (1982))
#'   Corrected for an exponential decay (tail) on the low BE side
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigmaDS function full width at half maximum
#' @param asym function asymmetry value
#' @param tail amplitude of the spectral tail on the low BE (high KE) side
#' @export

DoniachSunjicTail <- function(x, h, mu, sigmaDS, asym, tail) {

 	  Tail <- function(x, mu, tail) {
		          Y <- vector("numeric", length=length(x))
		          Ex <- (mu-x)    #tail at the right of mu (component position)
	          	index <- which(Ex >= 0)
	          	Y[index] <- exp(-abs(Ex[index])*(1-tail)/tail)
		          return(Y)
	   }

    DS1 <- vector("numeric", length=length(x))
    Ex <- (mu-x)
    index <- which(Ex < 0)
 	  DS1[index] <- h/4*( (gamma(1-asym)/((Ex[index])^2+(sigmaDS/2)^2)^((1-asym)/2) ) * cos((pi*asym)/2+(1-asym)*atan((Ex[index])*2/sigmaDS)) )  #DoniachSunjic
 	  DS2 <- h/4*( (gamma(1-asym)/((mu-x)^2+(sigmaDS/2)^2)^((1-asym)/2) ) * cos((pi*asym)/2+(1-asym)*atan((mu-x)*2/sigmaDS)) )  #DoniachSunjic
    return(DS1+DS2*Tail(x, mu, tail) )
}


## =======================================================================
## DSunjicGauss(x, h, mu, sigmaDS, sigmaG, asym)
#a gaussian broadening is added by multiplying the DS function with a Gauss
#' @title DoniachSunjicGauss function
#' @description Doniach Sunjic function multiplied for a Gaussian broadening
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigmaDS function full width at half maximum
#' @param sigmaG full width at half maximum of superimpossed Gaussian broadening
#' @param asym function asymmetry value
#' @export

DoniachSunjicGauss <- function(x, h, mu, sigmaDS, sigmaG, asym) {

    DS = ( (gamma(1-asym)/((mu-x)^2+(sigmaDS/2)^2)^((1-asym)/2) ) * cos((pi*asym)/2+(1-asym)*atan((mu-x)*2/sigmaDS)) )  #DoniachSunjic
    maxDS<-max(DS)
    minDS<-min(DS)
    DS<-(DS-minDS)/(maxDS-minDS)                   #normalize
    Gs = exp(-(2.77258872*(0.5*(x-mu)/sigmaG)^2))  #Gauss function

    LL <- length(DS)
    stp <- (DS[LL]-DS[1])/LL
    bkg <- stp*seq(1,LL,1)                         #DS background generation
    DS <- DS-bkg-DS[1]                             #BKG subtraction: recovers convolution distortions inroduced by the non-zer values of the asymmetric tail of the DS

    ConvDSGs<-convolve(DS, rev(Gs), type = "open")
    ConvDSGs<-h/4*maxDS*ConvDSGs/max(ConvDSGs)     #normalize and moltiply by h/4 to get the proper height

    LL=length(ConvDSGs)               #the convolution doubles the original number of data
    X <- ConvDSGs[seq(1,LL,2)]        #decimation 1 each two values
    return(X)
}

## =======================================================================
## DSunjicGauss(x, h, mu, sigmaDS, sigmaG, asym, tail)
#corrected version of  Doniach -Sunjic Lineshape adding a Tail
#on the low BE (high KE) side following the model of Wertheim PRB 25(3), 1987, (1982)
#(see also Leiro et al. J. El. Spectr. Rel. Phen. 128, 205, (2003).
#mu-x instead of x-mu to work on energies < mu
#tail damps the lineshape at low BE
#a gaussian broadeniing is added by multiplying the DS function with a Gauss

#' @title DoniachSunjicGauss function
#' @description Doniach Sunjic function multiplied for a Gaussian broadening
#'   corrected for an exponential decay on the low BE side
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigmaDS function full width at half maximum
#' @param sigmaG full width at half maximum of superimpossed Gaussian broadening
#' @param asym function asymmetry value
#' @param tail amplitude of the spectral tail on the low BE (high KE) side
#' @export

DoniachSunjicGaussTail <- function(x, h, mu, sigmaDS, sigmaG, asym, tail) {

 	  Tail <- function(x, mu, tail) {
		          Y <- vector("numeric", length=length(x))
		          Ex <- (mu-x)    #coda a DX di mu (posizione componente)
	          	index <- which(Ex >= 0)
	          	Y[index] <- exp(-abs(Ex[index])*(1-tail)/tail)
	          	return(Y)
	   }

    DS = ( (gamma(1-asym)/((mu-x)^2+(sigmaDS/2)^2)^((1-asym)/2) ) * cos((pi*asym)/2+(1-asym)*atan((mu-x)*2/sigmaDS)) )  #DoniachSunjic
    maxDS<-max(DS)
    minDS<-min(DS)
    DS<-(DS-minDS)/(maxDS-minDS)                    #normalize
    Gs = exp(-(2.77258872*(0.5*(x-mu)/sigmaG)^2))   #Gauss function

    LL <- length(DS)
    stp <- (DS[LL]-DS[1])/LL
    bkg <- stp*seq(1,LL,1)                         #DS background generation
    DS <- DS-bkg-DS[1]                             #BKG subtraction: recovers convolution distortions inroduced by the non-zer values of the asymmetric tail of the DS

    ConvDSGs<-convolve(DS, rev(Gs), type = "open")
    ConvDSGs<-h/4*maxDS*ConvDSGs/max(ConvDSGs)     #normalize and moltiply by h/4 to get the proper height

    LL=length(ConvDSGs)               #convoluzione provides the double of elements
    DSG <- ConvDSGs[seq(1,LL,2)]        #since DS and Gs have the same number of elements: decimation 1 each two elements


    DSG1 <- vector("numeric", length=length(x))
    Ex <- (mu-x)
    index <- which(Ex < 0)
 	  DSG1[index] <- DSG[index]
    return(DSG1+DSG*Tail(x, mu, tail) )

}

## =======================================================================
## SimplifiedDoniachSunjic(x, h, mu, sigma, asym)
# http://www.casaxps.com/help_manual/line_shapes.htm 
# mu-x instead of x-mu to get asymmetries for energies > mu

#' @title SimplifiedDoniachSunjic function
#' @description see http://www.casaxps.com/help_manual/line_shapes.htm
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param sigma function full width at half maximum
#' @param asym function asymmetry value
#' @export

SimplifiedDoniachSunjic <- function(x, h, mu, sigma, asym) {

	   return( h/2 * cos(pi*asym/2 + (1-asym)*atan((mu-x)/sigma)) / (sigma^2 + (mu-x)^2)^((1-asym)/2) )
}

## =======================================================================
#' @title linear function
#' @param x numeric vector
#' @param m slope
#' @param c constant value
#' @param mu function position
#' @export

Linear <- function(x, m, c, mu) {

    return( m*x+c )
}


## =======================================================================
## Functions useful for VB top estimation
## =======================================================================


## =======================================================================
#' @title Eponential decay function
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param k decay constant
#' @param c constant value
#' @export

ExpDecay <- function(x, h, mu, k, c) {

    return( h*exp(-k*(mu-x))+c)
}

## =======================================================================
## Power decay function
#' @title Power decay function
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param pow value of the power exponent
#' @param c constant value
#' @export

PowerDecay <- function(x, h, mu, pow, c) {

    return( h/((mu-x+1)^pow+1)+c)
}

## =======================================================================
## Sigmoid function
#' @title Sigmoid function
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param k sigmoid decay rate
#' @param c constant value
#' @export

Sigmoid <- function(x, h, mu, k, c) {

    return( h/(1+exp(-k*(x-mu))+c))
}

## =======================================================================
## Hill Sigmoid function
#' @title HillSigmoid function
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param pow sigmoid decay rate
#' @param A Sigmoid upper limit
#' @param B Sigmoid lower limit
#' @export

HillSigmoid <- function(x, h, mu, pow, A, B) { #decreases with incresing x values
                                               #i.e. decreas\ing the KE towards the Fermi
    return( (A-B)-(A-B)*x^pow/(mu^pow+x^pow) ) #HillSigmoind.Right decreases
}


## =======================================================================
## The Fermi Function is: 1/(exp(-(E-Ef)/KbT)+1)  Ef=Fermi energy Kb=K Boltzman T=temperature
# For fitting we use h/(exp(-k*(x-Ef)/Kb*T)+1)
# Kb = 8.617333262145*10-5 eV/K-1
# we set T=295K = room temperature

#' @title VBFermi function
#' @param x numeric vector
#' @param h function amplitude
#' @param mu position of function center
#' @param k Fermi Distribution decay rate
#'


VBFermi <- function(x, h, mu, k) {

    return( h/(1+exp(-k*(x - mu)/(8.617333262145*1e-5*295))) )
}

## =======================================================================
## VBtop function needed to store VBtop in a CoreLine@Components slot
# evaluated either using Linear Fit or Decay Fit
#
#' @title VBtop function
#' @description VBtop function to store VBtop position in a CoreLine@Components slot
#' @param x numeric vector
#' @param mu position of function center
#'


VBtop <- function(x, mu) {

   return(x*NA)
}

## =======================================================================
## Derivate function needed to store in a CoreLine@Components slot
#' @title Derivative function
#' @description Derivative function to store Derivate position in a CoreLine@Components slot
#' @param x numeric vector
#' @param mu position of function center
#' @export
#'

Derivative <- function(x, mu) {

   return(x*NA)
}



## =======================================================================
## ETG
# Empirically Transformed Gaussian function (presa da xcms)
#etg <- function(x, H, t1, tt, k1, kt, lambda1, lambdat, alpha, beta) {
#    2*H*exp(0.5)/((1+lambda1*exp(k1*(t1-x)))^alpha + (1+lambdat*exp(kt*(x-tt)))^beta - 1) }
## =======================================================================
# Empirically Transformed Gaussian function
# Journal of Chromatography A, 952 (2002) 63-70
# Comparison of the capability of peak functions in describing real chromatographic peaks
# Jianwei Li
# http://scholar.google.com/scholar?as_q=Comparison+of+the+capability+of+peak+functions+in+describing+real+chromatographic+peaks.
# t denotes time; substitute with x
# H is related to peak amplitude;
# lambdaL and lambdaT are pre-exponential parameters,
# kL and kT are the parameters related to the speeds of the rise and fall of the leading and trailing edges, respectively;
# xL and xT are the inflection times of the leading and trailing edges, respectively - The selection of xL and xT does not have to be accurate, and can be replaced by the times at half height.

# alpha and beta are the parameters to further modify the shapes of the leading and trailing edges, respectively.

#etg <- function(x, h, xL, xT, kL, kT, lambdaL, lambdaT, alpha, beta) {
#    h/((1+lambdaL*exp(kL*(xL-x)))^alpha + (1+lambdaT*exp(kT*(x-xT)))^beta - 1)
#}
## =======================================================================





