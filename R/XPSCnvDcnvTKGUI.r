# Convolution and Deconvolution of arrays carried out using FFT
# routine written on the base of Matlab indications

#' @title XPSCnvDcnv concolution, deconvolution functions
#' @description XPSCnvDcnv() is used to calculate the convolution of 
#'   the two X, Y input arrays or deconvolve Y from X.
#'   deconvolution can be made using the fft and its inverse or the
#    van Cittert algorithm.
#' @param x array to convolve
#' @param y array to convolve
#' @param deco = FALSE convolution of X, Y arrays, TRUE deconvolution of Y from X
#' @examples
#' \dontrun{
#'  x <- c(4, 13, 28, 34, 32, 21)
#'  y <- c(1, 2, 3)
#'	 C <- XPSCnvDcnv(x, y, deco=TRUE)
#'  print(C)
#'  [1]  4 5 6 7
#' }
#' @export
#'

XPSCnvDcnv <- function(x, y, deco=FALSE){

   CtrlEstep <- function(){
       if (SpectIdx1 > 0 && SpectIdx2 > 0) {  #core-line1 and core-line2 must be selected
          dE1 <- abs(FName[[SpectIdx1]]@.Data[[1]][2] -  FName[[SpectIdx1]]@.Data[[1]][1])
          dE1 <- round(dE1, 2)
          dE2 <- abs(FName[[SpectIdx2]]@.Data[[1]][2] -  FName[[SpectIdx2]]@.Data[[1]][1])
          dE2 <- round(dE2, 2)
          cat("\n Energy step Core-Line1: ", dE1)
          cat("\n Energy step Core-Line2: ", dE2)
          if (dE1 != dE2) {
              txt <- c("Selected core-lines have different energy step: cannot proceed",
                       "Use option INTERPOLATE - DECIMATE to make the energy steps be the same")
              tkmessageBox(message=txt, title="ERROR", icon="error")
              return(-1)
          } else if (dE1 == dE2){
              return(1)
          }
       }
       return(-1) #if one or both the core-lines are not selected return(-1)
   }

   Padding <- function(){
       LL1 <- length(FName[[SpectIdx1]]@.Data[[1]][1])
       LL2 <- length(FName[[SpectIdx2]]@.Data[[1]][1])
       idx <- SpectIdx1
       LL <- LL1
       if(LL2 < LL1){
          idx <- SpectIdx2   #idx identify the XPSSample to padd
          LL <- LL2
       }
       DN1 <- DN2 <- abs(LL2-LL1)/2
       if (DN1 > trunc(DN1)){
           DN1 <- floor(DN1)
           DN2 <- ceiling(DN2)
       }
       FName[[idx]]@.Data[[1]] <<- c(rep(FName[[idx]]@.Data[[1]][1], DN1),
                                        FName[[idx]]@.Data[[1]],
                                        rep(FName[[idx]]@.Data[[1]][LL], DN2))
       FName[[idx]]@.Data[[2]] <<- c(rep(FName[[idx]]@.Data[[2]][1], DN1),
                                        FName[[idx]]@.Data[[2]],
                                        rep(FName[[idx]]@.Data[[2]][LL], DN2))
   }
   
#------- Array Alignment -------------------------
   AlignArray <- function(){
       LL1 <- length(FName[[SpectIdx1]]@.Data[[1]]) #Auger spectrum
       LL2 <- length(FName[[SpectIdx2]]@.Data[[1]]) #Core-Line
       CLmax <- max(FName[[SpectIdx2]]@.Data[[2]])
       idx2 <- findYIndex(FName[[SpectIdx2]]@.Data[[2]], CLmax)
       Eref <<- FName[[SpectIdx2]]@.Data[[1]][idx2]
       idx1 <- findXIndex(FName[[SpectIdx1]]@.Data[[1]], Eref)
       dE1 <- abs(FName[[SpectIdx1]]@.Data[[1]][2] -  FName[[SpectIdx1]]@.Data[[1]][1])
       dE1 <- round(dE1, 2)

       tmp <- NULL
#--- accord initial energies of Auger and Core-line
       if (idx1 > idx2){
           DN2 <- idx1 - idx2  #N. elements to add Core-Line at beginning to start at the same energy of Auger
           FName[[SpectIdx2]]@.Data[[2]] <<- c(rep(FName[[SpectIdx2]]@.Data[[2]][1], DN2),
                                               FName[[SpectIdx2]]@.Data[[2]])
           #No need to extend energy scale CL2: it will be plotted on energy scale of CL1
       }
       if (idx2 > idx1){
           DN1 <- idx2 - idx1  #N. elements to add Auger at beginning to start at the same energy of CL
           FName[[SpectIdx1]]@.Data[[2]] <<- c(rep(FName[[SpectIdx1]]@.Data[[2]][1], DN1),
                                               FName[[SpectIdx1]]@.Data[[2]])
           for(ii in 1:DN1){
               tmp[ii] <- FName[[SpectIdx1]]@.Data[[1]][1]-(DN1+1-ii)*dE1
           }
           #extend Escale of CL1 at the beginning
           FName[[SpectIdx1]]@.Data[[1]] <<- c(tmp, FName[[SpectIdx1]]@.Data[[1]])
       }
#--- accord end energies of Auger and Core-line
       LL1 <- length(FName[[SpectIdx1]]@.Data[[1]]) #Auger spectrum
       LL2 <- length(FName[[SpectIdx2]]@.Data[[1]]) #Core-Line
#--- after padding recalculate the position of Eref
       idx1 <- findXIndex(FName[[SpectIdx1]]@.Data[[1]], Eref)
       idx2 <- findXIndex(FName[[SpectIdx2]]@.Data[[1]], Eref)

       if ((LL1-idx1) > (LL2-idx2)){
           DN2 <- (LL1-idx1) - (LL2-idx2) #N. elements to add CL at the end  to obtain same length of Auger
           FName[[SpectIdx2]]@.Data[[2]] <<- c(FName[[SpectIdx2]]@.Data[[2]],
                                               rep(FName[[SpectIdx2]]@.Data[[2]][LL2], DN2))
           #No need to extend energy scale CL2: it will be plotted on energy scale of CL1
       }
       tmp <- NULL
       if ((LL2-idx2) > (LL1-idx1)){
           DN1 <- (LL2-idx2) - (LL1-idx1) #N. elements to add Auger at the end to obtain same length of CL
           FName[[SpectIdx1]]@.Data[[2]] <<- c(FName[[SpectIdx1]]@.Data[[2]],
                                               rep(FName[[SpectIdx1]]@.Data[[2]][LL1], DN1))
           #extend Escale of CL1 at the end
           for(ii in 1:DN1){
               tmp[ii] <- FName[[SpectIdx1]]@.Data[[1]][LL1]+ii*dE1
           }
           FName[[SpectIdx1]]@.Data[[1]] <<- c(FName[[SpectIdx1]]@.Data[[1]], tmp)
       }
       FName[[SpectIdx2]]@.Data[[1]] <- FName[[SpectIdx1]]@.Data[[1]] #Escale CL2 == Escale CL1
       CvDcv[[1]] <<- FName[[SpectIdx1]]
       CvDcv[[2]] <<- FName[[SpectIdx2]]
   }


#-------FFT -------------------------
   CnvDcnv <- function(x, y, deco=FALSE){
      cnv <- NULL
      dcnv <- NULL
      eps <- 1e-15

# CONVOLUTION via FFT
      if ( deco == FALSE){
           LLx <- length(x)
           LLy <- length(y)
           x <- c(x, rep(0, (LLy-1)))
           y <- c(y, rep(0, (LLx-1)))
           LL <- LLx+LLy-1
           cnv <- ( fft(x) * fft(y) )
           cnv <- Re(fft(cnv, inverse=TRUE))/LL
           return(cnv)
      }
# DECONVOLUTION deconvolve y from xvia iFFT
      if ( deco == TRUE ){
           LLx <- length(x)
           LLy <- length(y)
           #now create two vectors x, y of same length
           if(LLx > LLy) { y <- c(y, rep(0, (LLx-LLy))) }
           if(LLy > LLx) { x <- c(x, rep(0, (LLy-LLx))) }
           
           dcnv <- ( (eps + fft(x)) / (eps + fft(y)) )
           dcnv <- Re(fft(dcnv, inverse = TRUE))/LLx
           return(dcnv)
      }
   }

#------- Van Citter -------------------------
   VanCittert <- function(s, y){  #Deconvolution using the iterative VanCittert algorithm

       AlignCnv <- function(s, cnv, EE){
           LL <- length(s)
           LL2 <- LL*2
           s <- s/max(s)
           cnv <- cnv/max(cnv)
           err1 <- sum(abs(cnv[, 1]))
           for(ii in seq(1,LL,1)){
               err2 <- sum(abs(cnv[ii:(LL+ii-1), 1] - s))
               if(err2 < err1){
                  err1 <- err2
                  shift <- ii
               }
           }
           matplot(x=EE, y=cbind(s, cnv[shift:(shift+LL-1),1]), xlab="Kinetic energy [eV]", ylab="Intensity [a.u.]",
                                 type=c("l", "l"), lty=c(1,1), lwd=1, col=c("black", "red"))

           #The low KE tail of the convolution needs to be cut to reduce length(cnv) to LL
           #we cannot simply extract data cnv[shift:(shift+LL)] from cnv
           #eliminate part of the tail MUST be done by forcing to zero C1s elements.
           #This corresponds to make part of the C1s tail = 0, i.e. y[1:zz] <- 0
           #Then we have to update the matrix Y to compute the correct convolution.

#--- Open TmpWin toplevel()
           TmpWin <- tktoplevel()
           tkwm.title(TmpWin,"DAMPING FACTOR")
           tkwm.geometry(TmpWin, "+100+50")   #SCREEN POSITION from top-left corner
           DFGroup1 <- ttkframe(TmpWin,  borderwidth=2, padding=c(10,10,10,10))  #padding values=20 to allow scrollbars
           tkgrid(DFGroup1, row = 1, column=1, sticky="w")

           TmpFrame1 <- ttklabelframe(DFGroup1, text = " Relaxation Factor MU ", borderwidth=2)
           tkgrid(TmpFrame1, row = 2, column=1, padx=5, pady=5, sticky="w")


           MuFact <<- 0.3     #relaxation factor mu see ref[@@@]
# [@@@] Bandzuch et al Nucl.Instr.Meth.Phys.Res.A (1997), 384, 506)
           MU <- tclVar("0.3")
           MuFact <- ttkentry(TmpFrame1, textvariable=MU)
           tkgrid(MuFact, row = 1, column=1, padx=5, pady=5, sticky="w")
           tkbind(MuFact, "<FocusIn>", function(K){
                               tkconfigure(MuFact, foreground="red")
                     })
           tkbind(MuFact, "<Key-Return>", function(K){
                               tkconfigure(MuFact, foreground="black")
                               MuFact <<- as.numeric(tclvalue(DF))
                     })

           TmpFrame2 <- ttklabelframe(DFGroup1, text = " Factor to Damp the Convolution tail to 0 ", borderwidth=2)
           tkgrid(TmpFrame2, row = 3, column=1, padx=5, pady=5, sticky="w")

           DF <- tclVar("Damp Factor from 0 to 1 ")
           CutFact <- ttkentry(TmpFrame2, textvariable=DF, foreground="grey")
           tkbind(CutFact, "<FocusIn>", function(K){
                               tkconfigure(CutFact, foreground="red")
                               tclvalue(DF) <- ""
                     })
           tkbind(CutFact, "<Key-Return>", function(K){
                               tkconfigure(CutFact, foreground="black")
                               DFact <- as.numeric(tclvalue(DF))
                               DFact <- as.integer(DFact*shift)+1 #DFact=0 no damping the cnv is just shifted
                               y[1:DFact] <- 0 #DFact=1 tail C1s to 0 => tail cnv is damped

                               Yc <<- c(y, rep(0, LL))        #now length(ss2) = LL2
                               Yc <<- matrix(data=Yc, ncol=1) #Yc is a prototype column for the YY matrix is 2*LL elements long

                               for(ii in 1:LL2){              #Y square matrix LL2 x LL2
                                   Y[ ,ii] <<- Yc
                                   Yc[,1] <<- c(0, Yc[1:(LL2-1)])
                               }
                               cnv <- Y %*% ss2
                               cnv <- cnv/max(cnv)
                               matplot(x=EE, y=cbind(x=s, y=cnv[shift:(shift+LL-1),1]), xlab="Kinetic energy [eV]", ylab="Intensity [a.u.]",
                                                     type=c("l", "l"), lty=c(1,1), lwd=1, col=c("black", "red"))
                     })
           tkgrid(CutFact, row = 1, column=1, padx=5, pady=5, sticky="w")

           TmpFrame3 <- ttklabelframe(DFGroup1, text = " Apply de-noise to iterations ", borderwidth=2)
           tkgrid(TmpFrame3, row = 4, column=1, padx=5, pady=5, sticky="w")

           YN <- tclVar(FALSE)   #starts with cleared buttons
           DeNs <- tkcheckbutton(TmpFrame3, text="Apply DeNoising", variable=YN, onvalue = 1, offvalue = 0,
                           command=function(){
                               DeNoise <<- as.integer(tclvalue(YN))
                               DeNoise <<- as.logical(DeNoise)
                           })
           tkgrid(DeNs, row = 1, column=1, padx=c(10,0), pady=5, sticky="w")

           ExitButt <- tkbutton(DFGroup1, text="SAVE Damp Factor & START ITERATION",
                           command=function(){
                               tkgrab.release(TmpWin)
                               tkdestroy(TmpWin)
                           })
           tkgrid(ExitButt, row = 5, column=1, padx=5, pady=5, sticky="w")
           tkgrab(TmpWin) #confine pointer and key inputs only from TmpWin
           tkwait.window(TmpWin)  #set modal window
#--- TmpWin toplevel modal

           return(shift)
       }
#----- End AlignCnv function


       LLs <- length(s)       #real spectrum to deconvolve
       LLy <- length(y)       #deconvolving array of data
       if (LLs < LLy){        #same length for the two vectors
           s  <- c(rep(s[1], (LLy-LLs)), s) #to respect s,y alignment zers at beginning
           LL <- LLy
       }
       if (LLy < LLs){
           y  <- c(rep(y[1], (LLs-LLy)), y) #to respect s,y alignment zers at beginning
           LL <- LLs
       }
       if (LLs == LLy) { LL <- LLs }
       #Now Length(s) = Length(y) = LL. If LL odd makes it even
       if (LL/2 > floor(LL/2)){ #LL is odd
           LL <- LL+1
           s[LL] <- 0
           y[LL] <- 0
       }
#Energy alignment of the two spectra: CKVV and C 1s in KE units!       
       EE <- CvDcv[[1]]@.Data[[1]]     #array of kinetic energies for the abscissa
       dE <- CvDcv[[1]]@.Data[[1]][2] - CvDcv[[1]]@.Data[[1]][1]
       if ((LL - LLs) > 0){
           for(ii in 1:(LL-LLs)){      #if length vector s is changed, harmonize the array of energies
               EE <- c((EE[1]-dE), EE)
           }
       }
       Es <- EE[1]-LL*dE    #build the energy scale for the convolution
       EE2 <- NULL
       for(ii in 1:(2*LL)){
           EE2[ii] <- Es+dE*(ii-1)
       }
       s <- s/max(s)
       y <- y/max(y)

#--- If we want to deconvolve only LOSSES the main Peak should be removed
#    However removing the Core-Line main peak makes the deconvolution to fail...
#    Losses are present from all energies < Peak_position (284.4eV for the C1s) 
#    and cannot be neglected.

#       matplot(x=EE, y=y, xlab="Kinetic energy [eV]", ylab="Intensity [a.u.]", type="l", lty=1, lwd=1, col="black")
#       cat("==> Removing Core-Line Main Peak")
#       tkmessageBox(message="Locate the Limits Peak to Remove", title="WARNING", icon="warning")
#       pos <- locator(n=2, type="p", pch=3, cex=1.5, col="red")
#       pos$x <- sort(pos$x, decreasing = FALSE)
#       idx1 <- findXIndex(EE, pos$x[1])
#       idx2 <- findXIndex(EE, pos$x[2])
#       YY <- y[idx1]
#       stp <- length(EE[idx1:idx2]) #number of points to remove
#       WW <- HannWin(1, stp) #Hanning window to replace the Peak
#       WW <- WW[, 2] #descending branch of the Hanning window
#       y2 <- c(y[1:(idx1-1)], WW*YY, y[(idx2+1):LL]) #y with removed Peak
#       matplot(x=EE, y=cbind(s, y, y2), xlab="Kinetic energy [eV]", ylab="Intensity [a.u.]",
#                                 type=c("l","l","l"), lty=c(1,1,1), lwd=1, col=c("black", "red", "blue"))
#       lines(x=c(Eref, Eref), y=c(max(y), 0), lty=2, col="black")
#       answ <- tkmessageBox(message="Is Peak Removal Correct?", type="yesno", title="WARNING", icon="warning")
#       if (tclvalue(answ) == "yes") { y <- y2 } #operate deconvolution just on losses: main Peak removed in y2

       #plot s=Convolved and y=deconvolving data
       matplot(x=EE, y=cbind(s, y), xlab="Kinetic energy [eV]", ylab="Intensity [a.u.]",
                                 type=c("l", "l"), lty=c(1,1), lwd=1, col=c("black", "red"))
       lines(x=c(Eref, Eref), y=c(max(y), 0), lty=2, col="black")
       tkmessageBox(message="Control if Alignment is correct", title="WARNING", icon="warning")

       LL2 <- 2*LL
       ss2 <<- c(s, rep(0, LL))      #now length(ss2) = LL2
       s <- matrix(data=s, ncol=1)   # transform s in a column of data
       y <- matrix(data=y, ncol=1)   # transform y in a column of data
       ss <- matrix(data=s, ncol=1)  # transform ss in a column of data
       ss2 <<- matrix(data=ss2, ncol=1)  # transform ss2 in a column of data
       cnv <- NULL
       dcnv <- matrix(nrow=LL, ncol=4)
       dcnv[ ,1] <- s
       Sum.S <- sum(abs(s))

#Van Cittert:  x(k+1) = x(k) + [s - Y x ]
# [&&&] Chengqi Xu et al J.Opt.Soc.Am. (1994), 11(11), 2804
# [@@@] Bandzuch et al Nucl.Instr.Meth.Phys.Res.A (1997), 384, 506)

       cat("\n WORKING... please wait \n")
       tkconfigure(InfoMsg2, text="==> WORKING... please wait")
       cat("\n ==> Building data matrix Y for convolution  ")
       tkconfigure(InfoMsg3, text="==> Building data matrix Y for convolution ")
       tcl("update", "idletasks")
       Y <<- matrix(nrow=LL2, ncol=LL2)
       Yc <<- c(y, rep(0, LL))        #now length(ss2) = LL2
       Yc <<- matrix(data=Yc, ncol=1) #Yc is a prototype column for the YY matrix is 2*LL elements long
       for(ii in 1:LL2){              # Y square matrix LL2 x LL2
           Y[ ,ii] <<- Yc
           Yc[,1] <<- c(0, Yc[1:(LL2-1)])
       }
       cnv <- Y %*% ss2          #this corresponds to the convolution s*y
       if(DecoC1s == TRUE){
          cat("\n ==> Reshaping the Convolution matrix  ")
          tkconfigure(InfoMsg4, text="==> Reshaping the Convolution matrix ")
          tcl("update", "idletasks")
          shift <- AlignCnv(s, cnv, EE)     # <===============
       }
       AA <- err <- sum(abs(s))  #area of the spectrum to deconvolve
       AA <- AA/100
       cat("\n ==> Start van Cittert iteration  ")
       tkconfigure(InfoMsg5, text="==> Start van Cittert iteration ")
       tcl("update", "idletasks")
       T1 <- TRUE
       T2 <- T3 <- FALSE
       LL <- length(s)
       tmp <- NULL
       iter <- 1
       while (err > AA){
           cnv <- Y %*% ss2            #update cnv using the new Y matrix defined in AlignCnv()
           if(DecoC1s == TRUE){
              cnv <- cnv[shift:(shift+LL-1),1]#take only one half of the convolution
           } else {
              cnv <- cnv[seq(1,LL2,2)] #decimation by 2
           }
           Sum.C <- sum(abs(cnv))
           if(Sum.C == 0){
              NormFact <- 1
           } else {
              NormFact <- Sum.S/sum(cnv)
           }
           cnv <- NormFact*cnv
#           if (iter == 1) { ss <- 0 * ss }
           if(DeNoise){
              if(T1){
                 T1 <- T3 <- FALSE
                 T2 <- TRUE                        #Set T2 for the next iteration
                 tmp <- s                          #no shift applied so s_matrix
              } else if(T2){
                 T1 <- T2 <- FALSE
                 T3 <- TRUE                        #Set T3 for the next iteration
                 tmp <- c(rep(0, 5), s[1:(LL-5)])  #shift s_matrix to right
              } else if(T3){
                 T2 <- T3 <- FALSE
                 T1 <- TRUE                        #Set T1 for the next iteration
                 tmp <- c(s[6:LL], rep(0, 5))      #shift s_matrix to left
              }
              ss <- ss + MuFact*(tmp - cnv)
           } else {
              ss <- ss + MuFact*(s - cnv)   #this corresponds to eq. (***)
           }
           ss2 <<- c(ss, rep(0, LL)) #now length(ss2) = LL2
           err <- sum(abs(s - cnv))
           dcnv[ ,2] <- cnv          #green
           dcnv[ ,3] <- MuFact*(s - cnv) #blue
           dcnv[ ,4] <- ss           #red
           matplot(x=EE, y=dcnv, type="l", xlab="Kinetic Energy [eV]", ylab="Intensity [a.u.]",
                   lwd=1, lty=c(1,1,1,1), col=c("black", "green", "blue", "red"))
           legend(x="topleft", bty="n", legend=c("Original Data", "Convolution","Difference","Deconvolved Spectrum"), text.col=c("black", "green", "blue", "red"), cex=1)
           txt <- paste("Iteration: ", as.character(iter), "   error = ", as.character(round(err, 3)), " ", sep="")
           answ <- tclVar("")
           answ <- tkmessageBox(message="Exit iteration?", type="yesno",
                                title="Stop iteration", icon="warning")
           if (tclvalue(answ) == "yes"){ break }
           cat("\n ==> Iteration N.", iter)
           iter <- iter + 1
        }
        return(dcnv[ ,4])
   }
#------- Van Citter END -------------------------

   HannWin <- function(strt, stp){
       LL <- abs(stp-strt)+1 #length of the descending/ascending hanning branches
       Hann <- matrix(nrow=LL, ncol=2)
       for (ii in 1:LL){
           Hann[ii, 1] <- 0.5 - 0.5*cos(pi*(ii-1)/(LL-1))      #ascending hanning window branch
           Hann[ii, 2] <- 1-(0.5 - 0.5*cos(pi*(ii-1)/(LL-1)) ) #descending hanning window branch
       }
       #Hann = 0, 0.1, 0.3, 0.5, 0.8, 1  ,  1, 0.8 0.5, 0.3, 0,1, 0
       return(Hann)
   }

   interp <- function(s){
           LL <- length(s)
           s2 <- NULL #interpolated signal
           jj <- 1
           for(ii in 1:(LL-1)){ #s2 long 2*LL by interpolation of adjacent data
              s2[jj] <- s[ii]
              s2[jj+1] <- 0.5*(s[ii+1] - s[ii]) + s[ii]
              jj <- jj+2
           }
           s2[jj] <- s[LL]
           s2[jj+1] <- 0
           return(s2)
   }


#--- Variables ---
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FName <- get(activeFName,envir=.GlobalEnv)
   FNameList <- XPSFNameList() #list of XPSSamples
   SpectList <- XPSSpectList(activeFName)
   SpectIdx1 <- -1  #inital value must be non-null
   SpectIdx2 <- -1
   CLName <- ""
   CutOFF <- NULL
   CvDcv <- new("XPSSample")
   Eref <- NULL
   CvDcv[[1]] <- NULL
   CvDcv[[2]] <- NULL
   CvDcv[[3]] <- NULL
   CvDcv[[4]] <- NULL
   Y <- NULL
   Yc <- NULL
   ss2 <- NULL
   DecoC1s <- FALSE
   DeNoise <- FALSE
   MuFact <- 0.3


#--- GUI ---
   ConvWin <- tktoplevel()
   tkwm.title(ConvWin,"CONVOLUTION - DECONVOLUTION")
   tkwm.geometry(ConvWin, "+100+50")   #SCREEN POSITION from top-left corner
   ConvGroup1 <- ttkframe(ConvWin,  borderwidth=2, padding=c(10,10,10,10))  #padding values=20 to allow scrollbars
   tkgrid(ConvGroup1, row = 1, column=1, sticky="w")

   ConvGroup2 <- ttkframe(ConvGroup1,  borderwidth=2, padding=c(10,10,10,10))  #padding values=20 to allow scrollbars
   tkgrid(ConvGroup2, row = 1, column=1, sticky="w")

   CVframe1 <- ttklabelframe(ConvGroup1, text = " XPS Sample ", borderwidth=2)
   tkgrid(CVframe1, row = 1, column=1, padx=5, pady=5, sticky="w")

   XSName <- tclVar()
   XPSSample <- ttkcombobox(CVframe1, width = 25, textvariable = XSName, values = FNameList)
   tkbind(XPSSample, "<<ComboboxSelected>>", function(){
                           activeFName <<- tclvalue(XSName)
                           FName <<- get(activeFName, envir=.GlobalEnv)
                           SpectList <<- XPSSpectList(activeFName)
                           plot(FName)
                           tkconfigure(CoreLine1, values = SpectList)
                           tkconfigure(CoreLine2, values = SpectList)
                       })
   tkgrid(XPSSample, row = 1, padx=5, pady=5, column=1, sticky="w")
   xx <- as.integer(tkwinfo("reqwidth", XPSSample))+35

   CVframe2 <- ttklabelframe(ConvGroup1, text = " Core-Line 1 ", borderwidth=2)
   tkgrid(CVframe2, row = 2, column = 1, padx=5, pady=5, sticky="we")

   CLName1 <- tclVar("")
   CoreLine1 <- ttkcombobox(CVframe2, width = 20, textvariable = CLName1, values = SpectList)
   tkbind(CoreLine1, "<<ComboboxSelected>>", function(){
                           SpectName <- tclvalue(CLName1)
                           SpectIdx1 <<- grep(SpectName, SpectList)
                           CvDcv[[1]] <<- FName[[SpectIdx1]]
                           plot(CvDcv)
                           if (CtrlEstep() == -1) {return()}
                           if (length(tclvalue(CLName2)) > 0){
                               WidgetState(ConvButton1, "normal")
                               WidgetState(ConvButton2, "normal")
                               WidgetState(DCnvButt1, "normal")
                               WidgetState(DCnvButt2, "normal")
                           }
                 })
   tkgrid(CoreLine1, row = 1, column = 1, padx=5, pady=5, sticky="w")
   
   txt <- " Core-Line 1 is the spectrum to which\n    an operation is applied"
   tkgrid( ttklabel(CVframe2, text=txt), row = 1, column = 2, padx = 5, pady = 0, sticky="w")

   CVframe3 <- ttklabelframe(ConvGroup1, text = " Core-Line 2 ", borderwidth=2)
   tkgrid(CVframe3, row = 3, column = 1, padx=5, pady=5, sticky="we")
   CLName2 <- tclVar("")
   CoreLine2 <- ttkcombobox(CVframe3, width = 20, textvariable = CLName2, values = SpectList)
   tkbind(CoreLine2, "<<ComboboxSelected>>", function(){
                           SpectName <- tclvalue(CLName2)
                           SpectIdx2 <<- grep(SpectName, SpectList)
                           CvDcv[[2]] <<- FName[[SpectIdx2]]
                           if (is.null(CvDcv[[1]])){
                               plot(CvDcv[[2]])
                           } else {
                               plot(CvDcv)
                           }
                           if (CtrlEstep() == -1) {return()}
                           if (length(tclvalue(CLName1)) > 0){
                               WidgetState(ConvButton1, "normal")
                               WidgetState(ConvButton2, "normal")
                               WidgetState(DCnvButt1, "normal")
                               WidgetState(DCnvButt2, "normal")
                           }
                 })
   tkgrid(CoreLine2, row = 1, column=1, padx=5, pady=5, sticky="w")
   
   txt <- " Core-Line 2 is the spectrum used \n    to convolve or deconvolve"
   tkgrid( ttklabel(CVframe3, text=txt), row = 1, column = 2, padx = 5, pady = 0, sticky="w")

   CVframe4 <- ttklabelframe(ConvGroup1, text = " INFO ", borderwidth=2)
   tkgrid(CVframe4, row = 4, column = 1, padx=5, pady=5, sticky="we")

   InfoMsg1 <- ttklabel(CVframe4, text="==>           ")
   tkgrid(InfoMsg1, row = 1, column=1, pady=2, sticky="w")
   InfoMsg2 <- ttklabel(CVframe4, text="           ")
   tkgrid(InfoMsg2, row = 2, column=1, pady=2, sticky="w")
   InfoMsg3 <- ttklabel(CVframe4, text="           ")
   tkgrid(InfoMsg3, row = 3, column=1, pady=2, sticky="w")
   InfoMsg4 <- ttklabel(CVframe4, text="           ")
   tkgrid(InfoMsg4, row = 4, column=1, pady=2, sticky="w")
   InfoMsg5 <- ttklabel(CVframe4, text="           ")
   tkgrid(InfoMsg5, row = 5, column=1, pady=2, sticky="w")

   txt <- " CONVOLVE Core Line 1 and Core Line 2 via FFT "
   ConvButton1 <- tkbutton(ConvGroup1, text=txt, width=55, command=function(){
                           cnv <- NULL
                           cnv <- CnvDcnv(CvDcv[[1]]@.Data[[2]], CvDcv[[2]]@.Data[[2]], deco=FALSE)
                           CvDcv[[3]] <<- new("XPSCoreLine")  #define a New Coreline to store Convolution
                           dE <- CvDcv[[1]]@.Data[[1]][2] - CvDcv[[1]]@.Data[[1]][1]  #energy step of CoreLine1
                           LL <- length(cnv)
                           xx <- NULL
                           xx[1] <- FName[[SpectIdx1]]@.Data[[1]][1]
                           for(ii in 2:LL){    #Build energy scale for the Convolution
                              xx[ii] <- xx[(ii-1)] + dE
                           }
                           #store information in the New Convolution Core-Line
                           CvDcv[[3]]@.Data[[1]] <<- xx
                           CvDcv[[3]]@.Data[[2]] <<- cnv
                           CvDcv[[3]]@units <<- FName[[SpectIdx1]]@units
                           CvDcv[[3]]@Flags <<- FName[[SpectIdx1]]@Flags
                           CvDcv[[3]]@Info <<- paste("Convolution of CoreLines ",FName[[SpectIdx1]]@Symbol," and ",FName[[SpectIdx2]]@Symbol, sep="")
                           CvDcv[[3]]@Symbol <<- "Convolution"
                           CLName <<- "Conv"
                           plot(CvDcv)
                 })
   tkgrid(ConvButton1, row = 5, column = 1, padx=5, pady = 5, sticky="w")

   txt <- " CONVOLVE Core Line 1 and Core Line 2 by Sum of Products "
   ConvButton2 <- tkbutton(ConvGroup1, text=txt, width=55, command=function(){
                           cnv <- NULL
                           x <- CvDcv[[1]]@.Data[[2]]
                           y <- CvDcv[[2]]@.Data[[2]]

                           LLx <- length(x)       #spectrum to deconvolve
                           LLy <- length(y)       #deconvolving spectrum
                           #if LLx != LLy make array length the same
                           if (LLx < LLy){
                              x  <- c(x,rep(0, (LLy-LLx))) #both long LL = LLy
                              LL <- LLy
                           }
                           if (LLy < LLx){
                              y  <- c(y,rep(0, (LLx-LLy))) #both long LL = LLx
                              LL <- LLx
                           }
                           if (LLx == LLy) { LL <- LLx }
                           #make array length equal to LL = LL1+LL2
                           x <- c(x,rep(0, LL))
                           y <- c(y,rep(0, LL))

                           LL2 <- 2*LL
                           cnv <- rep(0, LL2)
                           dE <- CvDcv[[1]]@.Data[[1]][2] - CvDcv[[1]]@.Data[[1]][1]  #energy step of CoreLine1
                           EE <- NULL
                           EE[1] <- CvDcv[[1]]@.Data[[1]][1]
                           #now convolution
                           for (jj in 1:LL2){
                               summ <- 0
                               for(ii in 1:jj){
                                   summ <- summ + x[ii] * y[(jj-ii+1)]
                               }
                               cnv[jj] <- summ   #convolution
                               EE[jj] <- EE[1] +(jj-1)*dE #array of energies (abscissa) for plotting Conv
                           }
                           #store information in the New Convolution Core-Line
                           CvDcv[[3]] <<- new("XPSCoreLine")  #define a New Coreline to store Convolution
                           CvDcv[[3]]@.Data[[1]] <<- EE
                           CvDcv[[3]]@.Data[[2]] <<- cnv
                           CvDcv[[3]]@units <<- FName[[SpectIdx1]]@units
                           CvDcv[[3]]@Flags <<- FName[[SpectIdx1]]@Flags
                           CvDcv[[3]]@Info <<- paste("Convolution of CoreLines ",FName[[SpectIdx1]]@Symbol," and ",FName[[SpectIdx2]]@Symbol, sep="")
                           CvDcv[[3]]@Symbol <<- "Convolution"
                           CLName <<- "Conv"
                           plot(CvDcv)
                 })
   tkgrid(ConvButton2, row = 6, column = 1, padx=5, pady = 5, sticky="w")

   txt <- " DECONVOLVE Core Line 2 from Core Line 1 via iFFT "
   DCnvButt1 <- tkbutton(ConvGroup1, text=txt, width=55, command=function(){
                           dE1 <- CvDcv[[1]]@.Data[[1]][2] - CvDcv[[1]]@.Data[[1]][1]
                           dE2 <- CvDcv[[2]]@.Data[[1]][2] - CvDcv[[2]]@.Data[[1]][1]
                           if (abs(dE1-dE2) > 1e3) {
                               tkmessageBox(message="ERROR: Core-Line 1 and Core-Line 2 must have the same energy step", title="ERROR", icon="error")
                               return()
                           }
                           DeCnv <- NULL
                           DeCnv <- CnvDcnv(CvDcv[[1]]@.Data[[2]], CvDcv[[2]]@.Data[[2]], deco=TRUE)
                           CvDcv[[3]] <<- new("XPSCoreLine")  #define a New Coreline to store DEconvolution
                           dE <- CvDcv[[2]]@.Data[[1]][2] - CvDcv[[2]]@.Data[[1]][1] #energy step of CoreLine2
                           xx <- NULL
                           xx[1] <- FName[[SpectIdx2]]@.Data[[1]][1]
                           LL <- length(DeCnv)
                           for(ii in 2:LL){    #Build energy scale for the DEconvolution
                               xx[ii] <- xx[(ii-1)] + dE
                           }
                           #store information in the New Convolution Core-Line
                           CvDcv[[3]]@.Data[[1]] <<- xx
                           CvDcv[[3]]@.Data[[2]] <<- DeCnv
                           CvDcv[[3]]@units <<- FName[[SpectIdx2]]@units
                           CvDcv[[3]]@Flags <<- FName[[SpectIdx2]]@Flags
                           CvDcv[[3]]@Info <<- paste("Deconvolution of CoreLine ",FName[[SpectIdx2]]@Symbol," from ",FName[[SpectIdx1]]@Symbol, sep="")
                           CvDcv[[3]]@Symbol <<- "Deconvolution"
                           CLName <<- "Deconv"
                           plot(CvDcv)
                 })
   tkgrid(DCnvButt1, row = 7, column = 1, padx=5, pady = 5, sticky="w")

   txt <- " DECONVOLVE Core Line 2 from Core Line 1 via VAN CITTERT "
   DCnvButt2 <- tkbutton(ConvGroup1, text=txt, width=55, command=function(){
                           AlignArray()
                           LL1 <- length(CvDcv[[1]]@.Data[[1]])
                           LL2 <- length(CvDcv[[2]]@.Data[[1]])
                           if (LL1 != LL2){
                               txt <- c("WARNING: Core-Line 1 and Core-Line 2 must have the same length\n Apply padding to use Van Cittert?\n Otherwise select Deconvolution via IFFT.")
                               answ <- tclVar("")
                               answ <- tkmessageBox(message=txt, type="yesno", title="WARNING", icon="warning")
                               if (tclvalue(answ) == "no") {return()}
                               Padding()
                           }
                           tclvalue(InfoMsg1) <- "==> If Van Cittert iteration does not converge PRESS 'x' to EXIT"
                           tkconfigure(InfoMsg1, text="==> If Van Cittert iteration does not converge PRESS 'x' to EXIT")
                           tcl("update", "idletasks")
                           dE1 <- CvDcv[[1]]@.Data[[1]][2] - CvDcv[[1]]@.Data[[1]][1]
                           dE2 <- CvDcv[[2]]@.Data[[1]][2] - CvDcv[[2]]@.Data[[1]][1]
                           if (abs(dE1-dE2) > 1e3 ) {
                               tkmessageBox(massage="ERROR: Core-Line 1 and Core-Line 2 must have the same energy step", title="ERROR", icon="error")
                               return()
                           }
                           answ <<- tkmessageBox(message="Is the FWHM of Core-Line 2 much\n narrower than FWHM of Core-Line1?",
                                                    type="yesno", title="WARNING", icon="warning")
                           if (tclvalue(answ) == "yes") DecoC1s <<- TRUE
                           ###---- Call Van Cittert Deconv. Function -----###
                           DeCnv <- NULL
                           DeCnv <- VanCittert(CvDcv[[1]]@.Data[[2]], CvDcv[[2]]@.Data[[2]])
                           #store information in the New Convolution Core-Line
                           LL <- length(FName[[SpectIdx1]]@.Data[[1]])
                           CvDcv[[3]] <<- new("XPSCoreLine")  #define a New Coreline to store DEconvolution
                           CvDcv[[3]]@.Data[[1]] <<- FName[[SpectIdx1]]@.Data[[1]]
                           CvDcv[[3]]@.Data[[2]] <<- DeCnv[1:LL] #remember that in VanCittert() if LLx < LLy zeroPadding
                           CvDcv[[3]]@units <<- FName[[SpectIdx1]]@units
                           CvDcv[[3]]@Flags <<- FName[[SpectIdx1]]@Flags
                           CvDcv[[3]]@Info <<- paste("Deconvolution of CoreLine ",FName[[SpectIdx2]]@Symbol," from ",FName[[SpectIdx1]]@Symbol, sep="")
                           CvDcv[[3]]@Symbol <<- "Deconvolution"
                           CLName <<- "Deconv"
                           plot(CvDcv)
                 })
   tkgrid(DCnvButt2, row = 8, column = 1, padx=5, pady = 5, sticky="w")

#   WinButt <- tkgbutton(ConvGroup1, text=" Apply Hanning window to smooth spectrum edges ", command=function(){
#                     HannWin <- tktoplevel()
#                     tkwm.title(ConvWin,"HANNING WINDOW")
#                     tkwm.geometry(ConvWin, "+150+100")   #SCREEN POSITION from top-left corner
#                     HannGroup1 <- ttkframe(HannWin,  borderwidth=2, padding=c(10,10,10,10))  #padding values=20 to allow scrollbars
#                     tkgrid(HannGroup1, row = 1, column=1, padx=5, pady=5, sticky="w")
#                     Hframe1 <- ttklabelframe(HannGroup1, text = " Select Spectrum to smooth ", borderwidth=2)
#                     tkgrid(Hframe1, row = 2, column=1, padx=0, pady=5, sticky="w")

#                     CLName <- tclVar("")
#                     CLCombo <- ttkcombobox(Hframe1, width = 20, textvariable = CLName1, values = SpectList)
#                     tkbind(CLCombo, "<<ComboboxSelected>>", function(){
#                                            SpectName <- tclvalue(CLName1)
#                                            SpectName <- unlist(strsplit(SpectName, "\\."))
#                                            idx <- as.integer(SpectName[1])
#                                            tkconfigure(HLabel, text="Now define the edge extension to smooth")
#                                            plot(FName[[idx]])
#                                            pos <- locator(n=2, type="p", col="red", pch=3)
#                                            idx1 <- findXIndex(FName[[idx]]@.Data[[1]], pos$x[1])
#                                            idx2 <- findXIndex(FName[[idx]]@.Data[[1]], pos$x[2])
#                                            WW <- abs(idx1-idx2)+1  #length of half Hanning window
#                                            LL <- length(FName[[idx]]@.Data[[2]])
#                                            Hann <- HannWin(1, WW) #Hann() generates two branches of lenght W
#                                            for(ii in 1:WW){ #Now apply Hann brances at beginning and at end of selected Core-Line
#                                                FName[[idx]]@.Data[[2]][ii] <<- FName[[idx]]@.Data[[2]][ii]*Hann[ii, 1]   #smooth the left Core-Line edge
#                                                FName[[idx]]@.Data[[2]][(LL-WW+ii)] <<- FName[[idx]]@.Data[[2]][(LL-WW+ii)]*Hann[ii, 2]  #smooth the right Core-Line edge
#                                            }
#                                            plot(FName[[idx]])
#                                            if (idx == SpectIdx1) {
#                                                CvDcv[[1]] <<- FName[[idx]]
#                                            } else if (idx == SpectIdx2){
#                                                CvDcv[[2]] <<- FName[[idx]]
#                                            } else if(SpectIdx1== -1 || SpectIdx2== -1){
#                                                tkmessageBox(message="Please select Core-Lines to proceed", title="WARNING", icon="warning")
#                                            } else {
#                                                tkmessageBox(message="Now ready to proceed", title="INFO", icon="info")
#                                            }
#                                            tkdestroy(HannWin)
#                               })
#                        tkgrid(CLCombo, row = 3, column = 1, padx=5, pady=5, sticky="w")
#
#                        HLabel <- ttklabel(Hframe1, text="==>           "),
#                        tkgrid(HLabel, row = 1, column = 1, padx=5, pady=5, sticky="w")
#                  })

   ResetButt <- tkbutton(ConvGroup1, text=" RESET ", width=22, command=function(){
                           SpectIdx1 <<- -1
                           SpectIdx2 <<- -1
                           CutOFF <<- NULL
                           CvDcv[[1]] <<- NULL
                           CvDcv[[2]] <<- NULL
                           CvDcv[[3]] <<- NULL
                           Y <<- NULL
                           Yc <<- NULL
                           ss2 <<- NULL
                           DecoC1s <<- FALSE
                           DeNoise <<- FALSE
                           MuFact <<- 0.3
                           tclvalue(CLName1) <- ""
                           tclvalue(CLName2) <- ""
                           plot(FName)
                 })
   tkgrid(ResetButt, row = 9, column = 1, padx=5, pady = 5, sticky="w")
   xx <- as.integer(tkwinfo("reqwidth", ResetButt))+20

   SavExitButt <- tkbutton(ConvGroup1, text=" SAVE & EXIT ", width=28, command=function(){
                           if (length(CvDcv) == 3){
                               LL <- length(FName)+1
                               FName[[LL]] <<- CvDcv[[3]]
                               names(FName)[LL] <- CLName
                               assign(activeFName, FName, envir=.GlobalEnv)
                           }
                           plot(FName)
                           tkdestroy(ConvWin)
                 })
   tkgrid(SavExitButt, row = 9, column = 1, padx=c(xx+10,0), pady = 5, sticky="w")

   WidgetState(ConvButton1, "disabled")
   WidgetState(ConvButton2, "disabled")
   WidgetState(DCnvButt1, "disabled")
   WidgetState(DCnvButt2, "disabled")
}


