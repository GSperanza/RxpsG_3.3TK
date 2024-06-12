# definition of CUSTOM baseline functions

#====================================
#    Spline background
#====================================
#' @title Spline Baseline
#' @description definition of the Spline Algorythms
#'   Computes the Spline curve trough a set of splinePoints
#'   provided by the user by mouse-clicking on the Core-Line spectrum
#'   This function is called by the XPSAnalysisGUI
#' @param spectra coreline on which apply spline function
#' @param splinePoints set of points where the spline has to pass through
#' @param limits the abscissa range where the spline is defined
#' @export
#'

baseSpline <- function(spectra, splinePoints, limits) {
          object <- list(y=spectra[1,]) #spectra contiene solo l'ordinata
          npt <- length(object$y)
          BGND<-spline(x=splinePoints$x, y=splinePoints$y, method="natural", n=npt)
          if (limits$x[1] > limits$x[2]) {
             BGND$y<-rev(BGND$y)   #se ho scala in BE inverto la spline
          }
          baseline <- matrix(data=BGND$y, nrow=1)
#here a list following the baseline package format is returned
          return(list(baseline = baseline, corrected = spectra - baseline, spectra=spectra))
      }

#====================================
#    Classic Shirley background
#====================================
#' @title Shirley0 Classic Shirley background
#' @description Shirley0 function prepresents the classic
#'   algorithm to define the Shirley background below a given Core-Line
#'   Shirley0(E) = S(E) is calculated iteratively, at step i:
#'   S(E)i) = integral[S(E)i-1) J(E) dE ]
#'   S(E)i), S(E)i-1) are the Shirley at step i and i-1
#'   J(E) is the measured spectrum
#'   This function is called by the XPSAnalysisGUI
#' @param object XPSSample CoreLine
#' @param limits limits of the XPSSample object
#' @return returns the Two_parameter Shirley background.
#' @export
#'

Shirley0 <- function(object, limits) {   #Iterative Shirley following the Sherwood method
   obj <- object@RegionToFit$y
   LL <- length(object@RegionToFit$y)
   if(LL == 0){
      tkmessageBox(message="WARNING: no data to generate the baseline", title="WARNING", icon="warning")
      return()
   }
   if (object@Flags[1] == FALSE){   #KE scale
       object@RegionToFit$x<-rev(object@RegionToFit$x)
   }
   Lim1 <- limits$y[1]
   Lim2 <- limits$y[2]
   spect <- object@RegionToFit$y-Lim2
   LL <- length(obj)
   Ymin <- min(limits$y)
	  BGND <- rep.int(Ymin, LL)
	  SumBGND <- sum(BGND)
	  SumRTF <- sum(obj)
	  RangeY <- diff(range(limits$y))
   maxit <- 50
   err <- 1e-6
   exitLoop <- FALSE
   if (diff(limits$y) > 0) {
       for (nloop in seq_len(LL)) {
    		    for (idx in rev(seq_len(LL))) {
    			       BGND[LL - idx + 1] <- ((RangeY/(SumRTF-sum(BGND)))*(sum(obj[LL:idx])-sum(BGND[LL:idx])))+Ymin
    		    }
    	    	if ( abs( (sum(BGND)-SumBGND)/SumBGND ) < err ) { break }
    		    SumBGND <- abs(sum(BGND))
    	  }
   } else {
       for (nloop in seq_len(LL)) {
    		    for (idx in seq_len(LL)) {
    			       BGND[idx] <- ((RangeY/(SumRTF-sum(BGND)))*(sum(obj[idx:LL])-sum(BGND[idx:LL])))+Ymin
              if(BGND[idx] < 0) { 
                 BGND[idx] <- 0
                 exitLoop <- TRUE
                 break() 
              }
    		    }
          if(exitLoop) {
             break()
             tkmessageBox(message="WARNING: Shirley baseline computation diverges! \nPlease change Baseline End-Points",
                          title="WARNING", icon="warning")
          }
    	    	if ( abs( (sum(BGND)-SumBGND)/SumBGND ) < err ) { break }
    		    SumBGND <- abs(sum(BGND))
    	  }
   }
   return(BGND)
}


#============================================
# 2 Parameter CrossSection Shirley background
#============================================
# Shirley2P, 2 parameter Shirley background derived from J. Vegh, Surface Science 563, (2004), 183
# This background is based on Aproximated Shirley Cross section function defined by
# SC = Integral[ K(E'-E) dE]
# The energy loss (background) is then defined by
#
# Shirley(E) = Integral[ K(E'-E) * J(E)dE' ]    J(E) = acquired spectrum
#
# K is the universal cross section function see S. Tougaard, Surf. Sci. 216 (1989) 343.
# which may be expressed by a two paramenter function:
# K(T) =  BsT/(Cs + T^2) where T=E'-E
#
# Shirley(E) = Integral[ BsT/(Cs + T^2) * J(E)dT ]
#
#' @title Shirley2P a two parameter Shirley function
#' @description Shirley2P  is a Shirley backgroung generated
#'   using the two parameter Bs, Cs. Shirley(E) = S(E)is evaluated iteratively.
#'   S(E) = Integral[ K(E'-E) * J(E)dE' ]
#'   J(E) is the acquired spectrum
#'   K(E'-E) describes the energy loss E-E' and is described by: B*(E-E')/[C + (E-E')^2]
#'   see J. Vegh, Surface Science 563 (2004) 183,
#'   J. Vegh  J.Electr.Spect.Rel.Phen. 151 (2006) 159
#'   This function is called by the XPSAnalysisGUI
#' @param object XPSSample CoreLine
#' @param limits limits of the XPSSample object
#' @return returns the Two_parameter Shirley background.
#' @export
#'

Shirley2P <- function(object, limits){
   LL <- length(object@RegionToFit$x)
#KE scale: reverse vectors to make the routine compatible with BE scale
   if (object@Flags[1] == FALSE){  #KE scale
       object@RegionToFit$x <- rev(object@RegionToFit$x)
   }
   Lim1 <- limits$y[1]
   Lim2 <- limits$y[2]
   spect <- object@RegionToFit$y - Lim2

   Cs <- 750
   shirley2P <- vector(mode="numeric", length=LL)
   lK <- vector(mode="numeric", length=LL)
# Evaluation of the Shirley Cross Section lK() the Bs parameter
   tmp <- 0
   dE <- (object@RegionToFit$x[2] - object@RegionToFit$x[1]) # Estep
   for (ii in 1:LL){
       deltaE <- (ii-1)*dE
       num <- dE                    #dE
       denom <- (Cs+deltaE^2)       #(Cs+[E'-E]^2)
       lK[ii] <- num/denom          #dE/{Cs+[E'-E]^2)   the Shirley Cross Section/B
       tmp <- tmp+lK[ii]*spect[ii]       #j(E')dE/{Cs+[E'-E]^2}
   }
   Bs <- (Lim1-Lim2)/tmp

#--- Computation shirley2P background: OK for both KE and BE scale

   for (ii in 1:LL){
       shirley2P[ii] <- Bs*sum(lK[1:(LL-ii+1)]*spect[ii:LL])
   }
   shirley2P <- shirley2P+Lim2    #add the Low-Energy-Side value of the spectrum
   return(shirley2P)
}

#============================================
# 3 Parameter CrossSection Shirley background
#============================================
# 3 parameter Shirley background derived from J. Vegh  J.Electr.Spect.Rel.Phen. 151 (2006) 159
# This background is based on Aproximated Shirley Cross section function defined by
# SC = Integral( L(E)* K(E'-E) )
# The energy loss (background) is then defined by
#
# Shirley(E) = Integral( L(E)*K(E'-E) * J(E)dE'     
# J(E) = acquired spectrum
# K(E'-E) is the universal cross section function (see S. Tougaard, Surf. Sci. 216 (1989) 343)
#
# which may be expressed by a two paramenter function plus an exponential decay:
#
# Shirley3p = Integral[ BsT/(Cs + T^2)*(1-exp(-Ds*T)) * J(E)dT ]   where T=E'-E
#
#' @title Shirley3P three parameter Shirley background
#' @description Shirley3p is  described by three parameters 
#'   Bs=30eV^2, Cs=750eV^2, Ds=0.75
#'   which are needed to describe the
#'   Universal Cross Section function described by: B*T/(C + T^2)
#'   and an exponential decay L(E)= (1-exp(-Ds*T))
#'   where T = E'-E = energy loss
#'   The Shirley3P background is then described by
#'   Shirley(E) = Integral[ L(E)*K(E'-E) * J(E)dE' ]
#'   J(E) = acquired spectrum
#'   see J. Vegh  J.Electr.Spect.Rel.Phen. 151 (2006) 159
#'   This function is called by the XPSAnalysisGUI
#' @param object XPSSample object
#' @param Wgt XPSSample object
#' @param limits limits of the XPSSample object
#' @return returns the 3-parameter Shirley background.
#' @export
#'

Shirley3P <- function(object, Wgt, limits){
   LL <- length(object@RegionToFit$x)
#KE scale: reverse vectors to make the routine compatible with BE scale
   if (object@Flags[1] == FALSE){
       object@RegionToFit$x<-rev(object@RegionToFit$x)
   }
   Lim1 <- limits$y[1]
   Lim2 <- limits$y[2]
   spect <- object@RegionToFit$y-Lim2

   Cs <- 2500
   shirley3P <- vector(mode="numeric", length=LL)
   lK <- vector(mode="numeric", length=LL)
# Evaluation of the Shirley Cross Section lK() the Bs parameter
   tmp <- 0
   dE <-(object@RegionToFit$x[2] - object@RegionToFit$x[1]) # Estep
   for (ii in 1:LL){
       deltaE <- (ii-1)*dE
       num <- 1-exp(-Wgt*abs(deltaE))   #(1-exp(-Ds*T)*dE
       denom <- (Cs+deltaE^2)           #(Cs+[E'-E]^2)
       lK[ii] <- dE*num/denom           #(1-exp(-Ds*T)*dE/(Cs+[E'-E]^2)   the Shirley Cross Section/B
       tmp <- tmp+lK[ii]*spect[ii]      #j(E')dE/{Cs+[E'-E]^2}
   }
   Bs <- (Lim1-Lim2)/tmp

#--- Computation shirley3P background: OK for both KE and BE scale

   for (ii in 1:LL){
       shirley3P[ii] <- Bs*sum(lK[1:(LL-ii+1)]*spect[ii:LL])
   }
   shirley3P <- shirley3P+Lim2    #add the Low-Energy-Side value of the spectrum
   return(shirley3P)
}


#====================================
#    Polynomial+Shirley background
#====================================
#
#' @title LPShirley combination of a Shirley and linear Baselines
#' @description LPShirley generates a Baseline which is defined by
#'   LinearPolynomial * Shirley Baseline see Practical Surface Analysis Briggs and Seah Wiley Ed.
#'   Applies in all the cases where the Shirley background crosses the spectrum
#'   causing the Shirley algorith to diverge.
#'   In LPShirley firstly a linear background subtraction is performed to recognize 
#'   presence of multiple peaks. Then a Modified Bishop polynom weakens the Shirley cross section
#'   Bishop polynom:  PP[ii]<-1-m*ii*abs(dX)   dX = energy step, m=coeff manually selected
#'   in LPShirley the classical Shirley expression is multiplied by PP:
#'   S(E)i) =  Shirley(E) = Integral[ S(E)i-1)* PP(E)* J(E)dE' ]
#'   S(E)i), S(E)i-1) are the Shirley at step i and i-1
#'   J(E) = acquired spectrum
#'   This function is called by XPSAnalysisGUI
#' @param object  CoreLine where to apply LPshirley function
#' @param mm  coefficient of the linear BKG component
#' @param limits limits of the XPSSample CoreLine
#' @return returns the LPShirley background
#'

LPShirley <- function(object, mm, limits) {
   LL <- length(object@RegionToFit$y)
   objX <- object@RegionToFit$x
   objY <- object@RegionToFit$y
   if (object@Flags[1] == FALSE){  #KE scale
       objX <- rev(objX)
   }
   Lim1 <- limits$y[1]
   Lim2 <- limits$y[2]
#   objY<-objY-Lim2

   MinLy <- min(limits$y)
   MaxLy <- max(limits$y)
   dX <- abs(objX[1]-objX[2])  #energy step

	  BGND <- rep.int(MinLy, LL)
	  SumBGND <- sum(BGND)
	  SumRTF <- sum(objY)
	  RangeY <- diff(range(limits$y))
   maxit <- 50
   err <- 1e-6

   dY <- (limits$y[2]-limits$y[1])/LL
   LinBkg <- vector(mode="numeric", length=LL)
   for (ii in 1:LL){ #generating a linear Bkg under the RegionToFit
       LinBkg[ii] <- limits$y[1]+(ii-1)*dY
   }
   object@Baseline$x <- objX
   object@Baseline$y <- LinBkg

#LinBKg subtraction
   objY_L <- objY-LinBkg
   maxY_L <- max(objY_L)
#search for multiple peaks: no derivative used because noise causes uncontrollable derivative oscillation
#are there multiple peaks (spin orbit splitting, oxidized components...)
   idx <- which(objY_L > maxY_L/2)  #regions where peaks are > maxY_L/2
   Lidx <- length(idx)
#idx contains the indexes of the spectral points where the intensity I > maxY/2
#in presence of spin orbit splitting there are two regions satisfying this condition
#in presence of oxidized components the spin orbit leads to 4 peaks: 
#two for the pure element two for the oxidized element.
#In general more than one peak could be present => more than one region where I > maxY/2
#How to identify the number of these regions? 
#idx2[ii] = idx[ii]-idx[ii-1]
#if idx[1]==0 this difference > 1 in correspondence of the beginning of each region:
#
   idx[1] <- 0                        #idx==   0 225 226  ...  344 345  655 656 657  ...   943 944   1164 1165 1166 ...
   idx2 <- idx[2:Lidx]-idx[1:Lidx-1]  #idx2==  225 1  1  1   ...  1  1  310  1  1  1   ...    1  1   220  1   1   1  ....
                                      #        ----------REG1---------  ++++++++++++REG2++++++++++   ----------REG3---------  ...
                                      #        255-0=255                655-345==310                 1164-944=220
   Regs <- which(idx2>1)
   Nr <- length(Regs)

#now working on the original non subtracted data
#position  of the last peak if BE scale, of the first peak if KE scale
   PeakMax <- max(objY[idx[Regs[Nr]+1]:LL])    #max of the last region
   posPeak <- which(objY==PeakMax)
#Is there any flat region at the edge of the coreline?
   tailTresh <- (max(objY)-MinLy)/50+MinLy  #define a treshold on the basis of peak and edge intensities
	  for (Tpos in LL:1){   #Tpos = beginning of the flat region
       if(objY[Tpos] > tailTresh) { break }
   }
   PP <- vector(mode="numeric", length=LL)
   for (ii in 1:posPeak) {
       PP[ii] <- 1-mm*ii*abs(dX)  #Bishop weakening polynom see Practical Surface Analysis Briggs and Seah Wiley Ed.
   }
#Bishop polynom modified to better describe the flat region at high BE (lowKE)
#PP decreases linearly until the max of peak at higher BE (lower KE) then linearly
#increases till PP==1 the value assumed in the flat region at the end (beginning) of the
#core line. PP==1 means background == non weakened classical Shirley
   dPP <- (1-PP[posPeak])/abs(Tpos-posPeak)
   for (ii in (posPeak+1):Tpos) {
       PP[ii] <- PP[posPeak]+dPP*(ii-posPeak)
   }
   PP[Tpos:LL] <- 1
   
#   PP <- baseline(objY, method="modpolyfit", t=objX, degree = 3, tol = 0.01, rep = 100)
#   PP <- as.vector(polyBkg@baseline)
#   PP <- 1-mm*polyBkg

   for (nloop in seq_len(maxit)) {
		     for (ii in 1:LL) {
			         BGND[ii] <- ((RangeY/(SumRTF-sum(BGND)))*(sum(objY[ii:LL]*PP[ii:LL])-sum(BGND[ii:LL])))+MinLy
		     }
	     	if ( abs( (sum(BGND)-SumBGND)/SumBGND ) < err ) { break }
		     SumBGND <- abs(sum(BGND))
	  }

## reverse y-baseline in case of reverse bgnd
#	if ( limits$y[which.max(limits$x)] < limits$y[which.min(limits$x)]  ) {
#	baseline <- matrix(data=rev(BGND), nrow=1) }
#	else { baseline <- matrix(data=BGND, nrow=1) }
	  baseline <- matrix(data=BGND, nrow=1)
	  MinBGND <- min(c(BGND[1], BGND[LL]))
	  BGND <- (limits$y[1]-limits$y[2])/(BGND[1]-BGND[LL]) * (BGND-MinBGND)+MinLy   #Adjust BGND on the limits$y values
   return(BGND)
}


#====================================
#    2 Parameter Tougaard background
#====================================

# 2 parameter Tougaard background for metallic like samples
# This background is based on the Universal Cross section function defined by
# UC = Integral( L(E)* K(E'-E) )
# The energy loss (background) is then defined by
#
# Tougaard(E) = Integral[ L(E)*K(E'-E) * J(E)dE' ]   J(E) = acquired spectrum
#
# which may be expressed by a two paramenter function:
#
# Tougaard2p = Integral( BT/(C + T^2)^2 * J(E)dT   where T=E'-E
#
#' @title Tougaard2P two parameter Togaard Baseline
#' @description Tougaard2P two parameter B and C are used to define
#'   the Tougaard Baseline:
#'   Tougaard(E) = L * Integral[ K(E'-E) * J(E)dE' ]
#'   J(E) = acquired spectrum
#'   K(E'-E) = Universal Cross Section described by: B*T/(C + T^2)^2   T=E'-E
#'   see S. Tougaard, Surf. Sci. (1989), 216, 343;  S. Tougaard, Sol. Stat. Comm.(1987), 61(9), 547
#'   L = l/(1-lcos a) l=mean free path, a = take-off angle
#'   This function is called by XPSAnalysisGUI
#' @param object XPSSample CoreLine
#' @param limits limits of the XPSSample CoreLine
#' @return returns the 2parameter Tougaard background.
#' @export
#'
Tougaard2P <- function(object, limits){
   LL <- length(object@RegionToFit$x)
   if (object@Flags[1] == FALSE){  #KE scale
       object@RegionToFit$x <- rev(object@RegionToFit$x)
   }
   avg1 <- limits$y[1]
   avg2 <- sum(object@RegionToFit$y[(LL-4):LL])/5
   object@RegionToFit$y <- object@RegionToFit$y-avg2

   C <- 1643
   tougaard <- vector(mode="numeric", length=LL)
   lK <- vector(mode="numeric", length=LL)
# Evaluation of the Universal Cross Section lK() and the B parameter
   tmp <- 0
   dE <- (object@RegionToFit$x[2] - object@RegionToFit$x[1]) # Estep
   for (ii in 1:LL){
       deltaE <- (ii-1)*dE
       num <- deltaE*dE             #(E'-E)*dE
       denom <- (C+deltaE^2)^2      #(C+[E'-E]^2}^2
       lK[ii] <- num/denom          #(E'-E)/{C+[E'-E]^2}^2  the uniiversal Cross Section/B
       tmp <- tmp+lK[ii]*object@RegionToFit$y[ii]       #j(E')(E'-E)/{C+[E'-E]^2}^2
   }
   B <- (avg1-avg2)/tmp

#--- Computation Tougaard background: OK for both KE and BE scale

   for (ii in 1:LL){
       tougaard[ii] <- B*sum(lK[1:(LL-ii+1)]*object@RegionToFit$y[ii:LL])
   }

   tougaard<-tougaard+avg2        #add the Low-Energy-Side value of the spectrum
   return(tougaard)
}


#====================================
#    3 Parameter Tougaard background
#====================================

# 3 parameter Tougaard background for non metallic  matter such as polymers
# This background is based on the Universal Cross section function defined by
# UC = Integral( L(E)* K(E'-E) )
# The energy loss (background) is then defined by
#
# Tougaard(E) = L * Integral[ K(E'-E) * J(E)dE' ]    
# J(E) = acquired spectrum
# L = l/(1-1cos a)  l inelastic mean free path  a=take-off angle
#
# which may be expressed by a three paramenter B, C, D function:
#
# Tougaard3p = Integral( BT/[(C + T^2)^2 + D*T^2] * J(E)dT   where T=E'-E
#
#' @title Tougaard3P three parameter Togaard Baseline
#' @description Tougaard3P is the three parameter Tougaard Baseline
#    defined using three B, C and D parameters
#'   Tougaard3p = Integral[ BT/[(C + T^2)^2 + D*T^2] * J(T)dT ]
#'   J(T) = the measured spectrum
#'   B*T/[(C + T^2)^2 + D*T^2] = Modified Universal Cross Section      
#'   T=energy loss = E'-E
#'   see S. Hajati, Surf. Sci. (2006), 600, 3015
#'   This function is called by XPSAnalysisGUI
#' @param object XPSSample CoreLine
#' @param limits limits of the XPSSample CoreLine
#' @return returns the 3parameter Tougaard background.
#' @export

Tougaard3P <- function(object, limits){
   LL <- length(object@RegionToFit$x)
   if (object@Flags[1] == FALSE){  #KE scale
       object@RegionToFit$x <- rev(object@RegionToFit$x)
   }
   avg1 <- limits$y[1]
   avg2 <- sum(object@RegionToFit$y[(LL-4):LL])/5
   object@RegionToFit$y<-object@RegionToFit$y-avg2

   C <- 551
   D <- 436
   tougaard <- vector(mode="numeric", length=LL)
   lK <- vector(mode="numeric", length=LL)
# Evaluation of the Universal Cross Section lK() and the B parameter
   tmp <- 0
   dE <- (object@RegionToFit$x[2] - object@RegionToFit$x[1]) # Estep
   for (ii in 1:LL){
       deltaE <- (ii-1)*dE
       num <- deltaE*dE                      #(E'-E)*dE
       denom <- (C+deltaE^2)^2 + D*deltaE^2  #(C+[E'-E]^2}^2 + D(E'-E)^2
       lK[ii] <- num/denom                   #(E'-E)/{C+[E'-E]^2}^2  the modified uniiversal Cross Section L(E)*K(E,T)  T=E'-E
       tmp <- tmp+lK[ii]*object@RegionToFit$y[ii]       #j(E')(E'-E)/{C+[E'-E]^2}^2
   }
   B <- (avg1-avg2)/tmp

#--- Computation Tougaard background: OK for both KE and BE scale

   for (ii in 1:LL){
       tougaard[ii] <- B*sum(lK[1:(LL-ii+1)]*object@RegionToFit$y[ii:LL])
   }

   tougaard <- tougaard+avg2    #add the Low-Energy-Side value of the spectrum
   return(tougaard)
}



#====================================
#    4 Parameter Tougaard background
#====================================

# 4 parameter Tougaard background for non metallic homogeneous matter such as polymers
# This background is based on the Universal Cross section function defined by
# UC = Integral( L(E)* K(E'-E) )
# The energy loss (background) is then defined by
#
# Tougaard(E) = Integral( L(E)*K(E'-E) * J(E)dE'     J(E) = acquired spectrum
#
# which may be expressed by a three paramenter B, C, C', D function:
#
# Tougaard4p = Integral( BT/[(C + C'*T^2)^2 + D*T^2] * J(E)dT )   where T=E'-E
#
#'  @title  Tougaard4P four parameters Tougaard Baseline
#'  @description Tougaard4P is a Tougaard Baseline defined usinf
#'   the four parameters B, C, C' and D
#'   Tougaard4p = Integral( BT/[(C + C'*T^2)^2 + D*T^2] * J(E)dT )
#'   J(T) = the measured spectrum
#'   B*T/[(C + C'*T^2)^2 + D*T^2] = Modified Universal Cross Section
#'   T=energy loss = E'-E
#'   see R. Hesse et al., Surf. Interface Anal. (2011), 43, 1514
#'   This function is called by XPSAnalysisGUI
#' @param object XPSSample CoreLine
#' @param C1 numeric is a parameter value
#' @param limits limits of the XPSSample CoreLine
#' @return Return the 4parameter Tougaard background.
#' @export

Tougaard4P <- function(object, C1, limits){
   LL <- length(object@RegionToFit$x)
   if (object@Flags[1] == FALSE){  #KE scale
       object@RegionToFit$x <- rev(object@RegionToFit$x)
   }
   avg1 <- limits$y[1]
   avg2 <- limits$y[2] #sum(object@RegionToFit$y[(LL-4):LL])/5
   object@RegionToFit$y <- object@RegionToFit$y-avg2

   C <- 551
   D <- 436
   tougaard <- vector(mode="numeric", length=LL)
   lK <- vector(mode="numeric", length=LL)
# Evaluation of the Universal Cross Section lK() and the B parameter
   tmp <- 0
   dE <- (object@RegionToFit$x[2] - object@RegionToFit$x[1]) # Estep
   for (ii in 1:LL){
       deltaE <- (ii-1)*dE
       num <- deltaE*dE                      #(E'-E)*dE
       denom <- (C+C1*deltaE^2)^2 + D*deltaE^2  #(C+[E'-E]^2}^2 + D(E'-E)^2
       lK[ii] <- num/denom                   #(E'-E)/{C+[E'-E]^2}^2  the modified universal Cross Section L(E)*K(E,T)  T=E'-E
       tmp <- tmp+lK[ii]*object@RegionToFit$y[ii]       #j(E')(E'-E)/{C+[E'-E]^2}^2
   }
   B <- (avg1-avg2)/tmp

#--- Computation Tougaard background: OK for both KE and BE scale

   for (ii in 1:LL){
       tougaard[ii] <- B*sum(lK[1:(LL-ii+1)]*object@RegionToFit$y[ii:LL])
   }

   tougaard <- tougaard+avg2    #add the Low-Energy-Side value of the spectrum
   return(tougaard)
}

