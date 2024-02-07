#function to remove noise from XPS-Sample spectra

#' @title XPSFilter smoothing functions to remove noise from spectra.
#' @description XPSFilter contains a list of different filters to remove noise from spectra.
#'  - In Sawitzy Golay filter at point i is a weighted average of data i-n... i ... i+n
#'  - In Autoregressive filters the output variable depends linearly on its own previous values.
#'  - In the Moving Average filters the output at point i is the average of data i-n ... i ... i+n .
#'  - The FFT filter applys the FFT transform to perform filtering.
#'  - The Wavelets filter uses the wavelets to perform filtering.
#'  - The FIRfilter is a Finite Impulse Response with a zero distortion.
#'  - The Butterworth is an Infinite Impulse Response filter.
#' @examples
#' \dontrun{
#'	  XPSFilter()
#' }
#' @export
#'


XPSFilter <- function() {
   import::from(signal, filter, freqz, sgolay, hamming, fir1, filtfilt, butter)

   PlotData <- function() {
      XXX <- cbind(unlist(Object@.Data[1]))
      YYY <- cbind(unlist(Object@.Data[2]),Filtered)
      if (BkgSubtr == "1") {
         YYY <- cbind(YYY, BackGnd)
      }
      Xlim <- range(XXX)
      if (Object@Flags[1]==TRUE) {
         Xlim <- rev(Xlim)  ## reverse x-axis
      }
      Ylim <- range(YYY)
      matplot(x=XXX, y=YYY, type="l", lty=c("solid", "solid", "solid"), lwd=c(1,1.75), col=c("black", "red", "green"),
              xlim=Xlim, ylim=Ylim, xlab=Object@units[1], ylab=Object@units[2])
      points(x=pos$x, y=pos$y, pch=3, cex=1.5, col="red")
      return()
   }

   BkgSubtraction <- function(data){
      BackGnd <<- NULL
      LL <- length(data)
      rng <- 10
      bkg1 <- mean(data[1:rng])
      bkg2 <- mean(data[(LL-rng):LL])
      stp <- (bkg1-bkg2)/LL
      Dta_Bkg <- NULL
      for (ii in 1:LL){
          Dta_Bkg[ii] <- data[ii]-(bkg1-ii*stp)
          BackGnd[ii] <<- bkg1-ii*stp
      }
      pos$x <<- c(Object@.Data[[1]][1], Object@.Data[[1]][LL])
      pos$y <<- c(bkg1, bkg2)
      return(Dta_Bkg)
   }

   alignSpect <- function(){            #levels the filtered spectrum to the original one
       LL <- length(Object@.Data[[2]])  #modifying the bacground intensity
       dH <- Object@.Data[[2]][1] - Filtered[1]
       Filtered <<- Filtered + dH    #the filtered spectrum is overlapped to the original data
       Err <- 10    #initialize the error
       OldErr <- sum((Object@.Data[[2]][1:10]-Filtered[1:10])^2) +    #sum of square differences
                 sum((Object@.Data[[2]][(LL-10):LL]-Filtered[(LL-10):LL])^2)
       dH <- dH/10
       Filtered <<- Filtered+dH
       Err <- sum((Object@.Data[[2]][1:10]-Filtered[1:10])^2) +
              sum((Object@.Data[[2]][(LL-10):LL]-Filtered[(LL-10):LL])^2)
       dErr <- abs(OldErr-Err)
       while(dErr > 0.0001){
             Filtered <<- Filtered+dH
             Err <- sum((Object@.Data[[2]][1:10]-Filtered[1:10])^2) +
                    sum((Object@.Data[[2]][(LL-10):LL]-Filtered[(LL-10):LL])^2)
             if (Err > OldErr) dH <- -dH/2
             if (abs(dH) < 10^-4) {
                 break
             }
             dErr <- abs(OldErr-Err)
             OldErr <- Err
       }
   }

   normalize <- function(data){
      #apply 7-coeff SGfilter to clean data
      FiltLgth <- 2*7+1
      coeff <- sgolay(p=1, n=FiltLgth, m=0, ts=1) #7 coefficient for Sawitzky-Golay filtering
      data <- unlist(data)
      LL <- length(data)
      #compute averaged noise free value around the data minimum
      data <- c(rep(data[1],FiltLgth), data, rep(data[LL],FiltLgth)) #FiltLgth Padding at edges
      FFF <- filter(filt=coeff, x=data) #Savitzky-Golay filtering
      data <- data[(FiltLgth+1):(LL+FiltLgth)]
      Min <- min(FFF)  #Max and Min computed on filtered data
      Max <- max(FFF)
      if (SaveAmpli) SigAmpli <<- c(Min, Max)
      data <- (data-Min)/(Max-Min)
      return(data)
   }

   resize <- function(Data){
      cat("\n==> Resize Data: please wait")
      LL <- length(Data)
      FiltLgth <- as.numeric(tclvalue(F3Deg))    #Filter length
      NN <- 20
      while(NN > LL/2){           #if there are just few original Ndata reduce the extension of
         NN <- ceiling(NN/2)      #range to evaluate average at data edges
      }
      SaveAmpli <<- FALSE         #do not preserve ampliture range of original data
#XX <-cbind(seq(1:LL))
      Data[1:(2*FiltLgth)] <- rep(Data[(2*FiltLgth)],2*FiltLgth)
      Data <- normalize(Data)
      Data <- Data*(max(Object@.Data[[2]]) - min(Object@.Data[[2]]))
      Data <- Data + (RawData[LL] - Data[LL]) #align spectra on the background
      AA <- 1
      dAA <- 0.2
      SS <- sum(RawData-Data)^2/LL
      while (SS > 0){
            oldSS <- SS
            Data <- Data*AA+dAA
            SS <- sum(RawData-Data)^2/LL
#matplot(x=XX, y=cbind(RawData, Data), type="l", lty=1, col=c("black", "red"))
#cat("\n ==> SS = ", SS)
#scan(n=1, quiet=TRUE)
            if (abs(SS-oldSS) < 10^-4) {
                break
            }
            if (SS > oldSS) dAA <- -dAA/2
            Data <- Data*(AA+dAA)
      }
      return(Data)
   }


   SetAmplitude <- function(){
#amplitude filtered data does not correspond to that of original data: matching is needed
      LL <- length(Object@.Data[[2]])
      BkgSubtraction(Object@.Data[[2]])  #To Define just the background
      coeff <- sgolay(p=1, n=(2*7+1), ts=1) #7 coefficient for Sawitzky-Golay filtering
#Padding data at edges: adds a number of data corresponding to the N. coeff Sawitzki-Golay filter.
#to avoid negative indexing when min(data( corresponds to 1 or LL
      BackGndO <- BackGnd
      XXX <- Object@.Data[[2]]-BackGndO
      pad <- rep(mean(XXX[1:5], 7))
      XXX <- c(pad, XXX)
      pad <- rep(mean(XXX[(LL-5):LL], 7))
      XXX <- c(XXX, pad)

      Min <- min(XXX)      #minimum of BKG subtracted original data Object@.Data[[2]]
      indx <- which(XXX == Min)
      rng <- 10
      while ( (indx+rng)>LL || (indx-rng)<0) { rng <- rng-1 }
      FFF <- filter(filt=coeff, x=XXX[(indx-rng):(indx+rng)]) #Savitzky-Golay filtering  BKG subtr original data
      Min <- min(FFF)

      Max <- max(XXX)      #maximum of BKG subtracted original data Object@.Data[[2]]
      indx <- which(XXX == Max)
      rng <- 10
      while ( (indx+rng)>LL || (indx-rng)<0) { rng <- rng-1 }
      FFF <- filter(filt=coeff, x=XXX[(indx-rng):(indx+rng)]) #Savitzky-Golay filtering
      Max <- max(FFF)
      AmpliFact <- Max-Min              #amplification factor

#Do the same for filtered data
      XXX <- BkgSubtraction(Filtered)  #To Define just the background
      BackGndF <- BackGnd
      minF <- min(XXX)      #minimum of BKG subtracted filtered data
      indx <- which(XXX == minF)
      rng <- 5
      while ( (indx+rng)>LL || (indx-rng)<0) { rng <- rng-1 }
      minF <- mean(XXX[(indx-rng):(indx+rng)])  #average of BKG subtracted original data around minimum value

      maxF <- max(XXX)      #maximum of BKG subtracted original data
      indx <- which(XXX == maxF)
      rng <- 5
      while ( (indx+rng)>LL || (indx-rng)<0) { rng <- rng-1 }
      maxF <- mean(XXX[(indx-rng):(indx+rng)])  #average of BKG subtracted original data around maximum value
      DampFact <-  maxF-minF                      #damping factor

#Filtered data may contain noise: is needed a mean value around the maximum
      if (! BkgSubtr == "1"){
         Dta_Amp <- XXX*AmpliFact/DampFact+BackGndO #match amplitude of filtered data on BKG-UNSUBTRACTED original data
      } else {
         Dta_Amp <- XXX*AmpliFact/DampFact #match amplitude of filtered data on UNSUBTRACTED original data
      }

      return(Dta_Amp)
   }

   SavGolay <- function() {
      FiltInfo <<- paste("Savitzky Golay, Degree: ", tclvalue(F2Deg), sep="")
      BkgSubtr <<- tclvalue(F2Bkg)
      FiltLgth <- as.numeric(tclvalue(F2Deg))
      RawData <<- unlist(Object@.Data[[2]])
      LL <- length(RawData)
      FiltLgth <- 2*FiltLgth+1
      BackGnd <<- NULL
      Filtered <<- NULL
      coeff <<- NULL
      if (BkgSubtr == "1") {
         RawData <<- BkgSubtraction(RawData)
      }
      RawData <<- c(rep(RawData[1],FiltLgth), RawData, rep(RawData[LL],FiltLgth)) #FiltLgth Padding at edges
      coeff <<- sgolay(p=1, n=FiltLgth, m=0, ts=1)
      #Be careful: sgolay provides a matrix of coeff. and you can get the smoothed value y[k] as y[k] = F[i,] * x[(k-i+1):(k+N-i)]
      #y[k] does not depend on i. The filtering must be symmetric with respect to the central value k
      #SG acts as a weighted sum of values of n data preceding the k point and n following data. In tital 2n+1 values = filter length
      #For filter length N = 2n+1 = 5, the matrix of coeff. F is organized as
      #      |   A   c1       |
      #      |       c1       |
      #  F = | c1 c1 c1 c1, c1|
      #      |       c1       |
      #      |       c1   At  |
      #is divided in four quadrants by the central column and row which are the coeff of the median value k
      #At is the transpose of A Then for example we will have
      #F[1,1] = F[2n+1,2n+1]; 
      #F[1,2] = F[2n,2n+1];
      #F[1,3] = c1 = F[2n-1,2n+1];
      #F[1,4] = F[2n-2,2n+1]
      #F[1,5] = F[2n-3,2n+1] = 1st element of column 2n+1
      #This holds for all the columns.
      #I suppose y=filter(F, x=Data) uses the whole matrix with: y[k+i-1] = = F[i,] * x[(k-i+1):(k+N-i)]
      #which allow obtaining 2n+1 values by using the whole F matrix.
      #Same result is obtained using y=filter(F[1, ], a=1, x=Data)  where a=1 eliminates the autoregressive iteration requiring the whole F matrix
      Filtered <<- filter(filt=coeff, x=RawData) #Savitzky-Golay filtering
      Filtered <<- Filtered[(FiltLgth+1) : (FiltLgth+LL)]
      if (BkgSubtr == "1") {
          Filtered <<- Filtered + BackGnd  #add the background of the original data
      }
      coeff <<- coeff[1, ] #select just the 1st row of SG coeff.
      PlotData()
      return()
   }                                 


   AutoRegMedMob <- function() {
      FiltInfo <<- paste(tclvalue(F3Type), ", Degree: ", tclvalue(F3Deg), sep="")
      BkgSubtr <<- tclvalue(F3Bkg)
      FilterType <- tclvalue(F3Type)
      FiltLgth <- as.numeric(tclvalue(F3Deg))    #Filter length
      coeff <<- NULL
      Filtered <<- NULL
      BackGnd <<- NULL
      #preprocessing
      RawData <<- unlist(Object@.Data[[2]])
      LL <- length(RawData)
      if (BkgSubtr == "1") { RawData <<- BkgSubtraction(RawData) }
      #now filtering
      if (FilterType=="AutoRegressive"){
#--- Autoregressive filter
         SaveAmpli <<- TRUE
         RawData <<- c(rep(RawData[1],2*FiltLgth), RawData, rep(RawData[LL],2*FiltLgth)) #FiltLgth Padding at edges
         Ndta <- length(RawData)
         Filtered <<- array(data=0, dim=Ndta)
         #define the AR coefficents
         if(FiltLgth == 1){  #computing the AR coefficients
            coeff <<- 1
         } else if(FiltLgth == 2){
            coeff <<- c(1, 0.25)
         } else if(FiltLgth > 2){
            P <- FiltLgth-1
            for(ii in P:0){
                coeff[(P-ii+1)] <<- 1/P*sinpi(0.5*ii/P)
            }
         }
         #now apply the AR filter
         for(ii in (FiltLgth+1):Ndta){
             sum <- 0
             for(jj in 2:FiltLgth){
                 sum <- sum + coeff[jj]*Filtered[(ii-jj+1)]
             }
             Filtered[ii] <<- sum + coeff[1]*RawData[ii]
         }
         P <- as.integer(P/2)
         #now set correct amplitude of filtered data
         Filtered <<- resize(Filtered)   #match the amplitude of Filtered with the original data
         ## Attention: here recover for the delay eliminating half of the period P used for the AR coeff.
         ## instead of Filtered <<- Filtered[(2*FiltLgth+1) : (LL+2*FiltLgth)]
         Filtered <<- Filtered[(2*FiltLgth+1+P) : (LL+2*FiltLgth+P)]
      } else if (FilterType=="MovingAverage") {
#--- Moving average filter
         coeff <<- rep(1/(2*FiltLgth+1),(2*FiltLgth+1))  #1/FiltLgth because of the average
         RawData <<- c(rep(RawData[1],FiltLgth), RawData, rep(RawData[LL],FiltLgth)) #FiltLgth Padding at edges
         for(ii in 1:LL){
             Filtered[ii] <<- mean(RawData[ii:(ii+2*FiltLgth)]) #Forward filtering
         }
      }
      if (BkgSubtr == "1") { Filtered <<- Filtered + BackGnd }
#recover AutoRegressive filter distortions on signal amplitude
      if (tclvalue(F3Edges) == "1"){
         Filtered <<- BkgSubtraction(Filtered)
         Filtered <<- normalize(Filtered)

         Filtered <<- Filtered-0.5*(mean(Filtered[1:5]) + mean(Filtered[(LL-5):LL])) #shift the Filtered edges at zero
#         BackGnd <- BkgSubtraction(Object@.Data[[2]])
         BkgSubtraction(Object@.Data[[2]])
         SaveAmpli <<- TRUE
         normalize(Object@.Data[[2]]-BackGnd)
         A1 <- SigAmpli[2]-SigAmpli[1]    #with SaveAmpli=TRUE normalize() returns also SigAmpli=c(min, max)
         Filtered <<- Filtered*A1         #Adapt filtered signal on the bckgnd subtracted original signal
         Filtered <<- Filtered + BackGnd  #add the background of the original data
      }
      PlotData()
      return()
   }
   
   medianFilt <- function(){
      BkgSubtr <<- tclvalue(F3Bkg)
      FilterType <- tclvalue(F3Type)
      FO <- as.numeric(tclvalue(F3Deg))
      coeff <<- NULL
      Filtered <<- NULL
      BackGnd <<- 0
      #preprocessing
      RawData <<- unlist(Object@.Data[[2]])
      LL <- length(RawData)
      if (BkgSubtr == "1") { RawData <<- BkgSubtraction(RawData) }
      SaveAmpli <<- TRUE
      RawData <<- normalize(RawData)
      #now filtering
      tmp1 <- NULL
      tmp2 <- NULL
      FFO <- FO/(15+1)  #input degree of moving average filter [0.1 : 0.99]
      coeff <<- c(FFO, 1-FFO)
      LL <- length(RawData)
      tmp1[1] <- RawData[1]
      tmp2[LL] <- RawData[LL]
      for(ii in 2:LL){
          tmp1[ii]=FFO*tmp1[ii-1] + (1-FFO)*RawData[ii];              #forward filtering
          tmp2[LL-ii+1]=FFO*tmp2[LL-ii+2]+(1-FFO)*RawData[LL-ii+1];   #backward filtering
      }
      Filtered <<- (tmp1+tmp2)/2
   }

   FFTfilt <- function() {

      HannWin <- function(strt, stp){
          W <- stp-strt  #total lenght of the window
          Hann1 <- NULL
          Hann2 <- NULL
          Hann <- NULL
          LL <- floor((stp-strt)/20) #length of the descending/ascending hanning branches
          for (ii in 1:LL){
              Hann1[ii] <- -(0.5 - 0.5*cos(2*pi*ii/(LL-1)) ) #descending hanning window branch
              Hann2[ii] <- 0.5 - 0.5*cos(2*pi*(5+ii)/(LL-1)) #ascending hanning window branch
          }
          #Hann = 1, 1, 1, 1, 1, 0.8 0.5, 0.3, 0,1, 0, 0, 0, 0.1, 0.3, 0.5, 0.8, 1, 1, 1, 1, 1
          Hann <- c(rep(1, strt), Hann1, rep(0, (W-2*LL)), Hann2, rep(1, strt))
          return(Hann)
      }

      FiltInfo <<- paste("FFT filter, Degree: ", tclvalue(F4Deg), sep="")
      BkgSubtr <<- tclvalue(F4Bkg)
      FiltLgth <- as.numeric(tclvalue(F4Deg))
      RawData <<- NULL
      Filtered <<- NULL
      BackGnd <<- NULL
      RawData <<- unlist(Object@.Data[[2]])
      LL <- length(RawData)
      ##preprocessing
      if (BkgSubtr == "1") RawData <<- BkgSubtraction(RawData) #background subtraction to avoid FFT spourious oscillations
      if (floor(LL/2) < ceiling(LL/2)){ #if LL is ODD
          RawData[LL+1] <<- RawData[LL]   #then RawData has an even number=LL+1 of data
      }
      NPad <- 20 #for FFT filter zero padding must be independent on the filrer order
      if( 20 > LL/2) NPad <- floor(LL/2)
      RawData <<- c(rep(RawData[1],NPad), RawData, rep(RawData[LL],NPad)) #NPad Padding at edges
      LL_DF <- length(RawData)     #LL_DF is even
      ##Filtering
      stopF <- 0.5* FiltLgth/15   #for rejection noise=1 the cutoff freq=0.5=Nyquist Freq., for rejection=15 cutoff freq=1/15*0.5 = 0.0333
      dF <- 0.5/(LL_DF*0.5)               #FFT is composed by LL_DF/2 points; from 1+LL_DF/2 to LL_DF the FFT is specular to the first half
      freq <- seq(from=0, to=0.5, by=dF)  #frequency axis from 0 to 0.5 step dF same length of RawData
      idx <- length(which(freq <= stopF)) #idx is the array index corresponding to frequency <= stopF
      fftTransf <- fft(RawData)/LL_DF  #FFT must be devided by its length as indicated in the FFT R-documentation
#      Hann <- HannWin(idx, (LL_DF-idx))
      #Filtering
      fftRej <- fftTransf[1:idx] #also fftRej has an even number of data
      LLr <- length(fftRej)/2
      fftTransf[(idx+1):(LL_DF-idx)] <- 0+0i   #low filtering: force to zero frequencies > stopF in the first and second  half of the FFT
#     we want to characterize the rejected part of the signal corresponding to the
#     FFT coeff of the rejected frequencies
      coeff <<- Mod(fftRej[1:LLr]) #consider only the first half the second half is specular
      coeff <<- coeff/sum(coeff)
      Filtered <<- Re(fft(fftTransf, inverse = TRUE))
      Filtered <<- Filtered[(NPad+1) : (NPad+LL)]
      Filtered <<- Filtered[1:LL] #eliminates the added element to have odd data
      if (BkgSubtr == "1") Filtered <<- Filtered + BackGnd  #add the background of the original data
      PlotData()
      return()
   }

   msSmoothMRA <- function(){
      FiltInfo <<- paste("Wavelets filter, N.wavelets: ", tclvalue(F5WavN), " Degree: ", tclvalue(F5Deg), sep="")
      import::from(wavelets, mra, dwt) #cannot use import::here because rootSolve not listed in DESCRIPTION
      RawData <<- NULL
      Filtered <<- NULL
      coeff <<- NULL
      BackGnd <<- NULL
      BkgSubtr <<- tclvalue(F5Bkg)
      WTnum <- 2*as.numeric(tclvalue(F5WavN))   #This defines the N. Daubechies wavelets of the transform
      NPad <- 2*WTnum                           #This defines the N.ZeroPadding data
      RejLev <- as.numeric(tclvalue(F5Deg))     #this defines the level of noise rejection
      ##preprocessing
      RawData <<- (Object@.Data[[2]])
      if (BkgSubtr == "1") RawData <<- BkgSubtraction(RawData)
#      RawData <<- normalize(RawData)
      LL <- length(RawData)
      RawData <<- c(rep(RawData[1],NPad), RawData, rep(RawData[LL],NPad)) #NPad Padding at edges
      ##Filtering
      WTfilt <- paste("d", WTnum, sep="")  #d2 means Daubechies length=2,  d8 means Daubechies length=8
#maximum level according to the length(x) and selected filter degree WTnum
#maxLevels <- floor( log2((LL-1)/(WTnum-1) + 1 )  #following wavelets::mra suggestions
      maxLevels <- 10 #fixed n.levels of the decomposition. This allows increasing the RejLev (filter force) up to 10!

      if (RejLev > maxLevels){
         tkmessageBox(message="Attention: Nlevel must be < 1/2 * Filter Order", title="BAD DEGREE OF NOISE REJECTION", icon="error")
      }
      Response <- mra(RawData, filter = WTfilt, n.levels = maxLevels,
                      method = "modwt", boundary="periodic", fast=TRUE)
      Response <- sapply(c(Response@D[RejLev], Response@S[RejLev]), cbind)
      Filtered <<- rowSums(Response)
      Filtered <<- Filtered[(NPad+1):(LL+NPad)] #eliminates head and tail padded values
      if (BkgSubtr == "1") Filtered <<- Filtered + BackGnd

      LL <- length(RawData)
      while(log2(LL) < 10){  #increase the length of RawData till to allow dwt - Nlevels=10
          RawData <<- c(rep(RawData[1],NPad), RawData, rep(RawData[LL],NPad)) #NPad Padding at edges
          LL <- length(RawData)
      }
      Response <- dwt(RawData, filter = WTfilt, n.levels = maxLevels, boundary="periodic", fast=TRUE)
      ff <- 10-RejLev+1
      coeff <<- Response@V[[ff]]/sum(Response@V[[ff]]) #the higher the level selected for Response@V the higher the detail degree
      PlotData()
      return()
   }

   FirFilt <- function() {
      FiltInfo <<- paste("FIR filter, ", tclvalue(F6Ord), ", Degree: ", CutOFF, sep="")
      coeff <<- NULL
      RawData <<- NULL
      Filtered <<- NULL
      BackGnd <<- NULL
      BkgSubtr <<- tclvalue(F6Bkg)
      FiltLgth <- as.numeric(tclvalue(F6Ord))
      NPad <- 2*FiltLgth
      if (CutOFF == 0 || CutOFF > 20){
         tkmessageBox(message="WARNING: Cut-OFF frequency must be in the range 1 - 20", title="WRONG CUT-OFF F.", icon="warning")
         return()
      }
      CutOFF <<- (20.001-CutOFF)/20    #here cut-off would be freq/Nyquist
      #preprocessing
      RawData <<- unlist(Object@.Data[[2]])
      LL <- length(RawData)
#function filtfilt zeropadds the ends of the array introducing distortions if the signal
#intensity is different form zero leading to a discontinuity. FIR filter needs background subtraction
      if (BkgSubtr == "1") RawData <<- BkgSubtraction(RawData)
      #now filtering
      RawData <<- c(rep(RawData[1],NPad), RawData, rep(RawData[LL],NPad)) #N,coeff = 2*FiltLgth = NPad padding at edges
      coeff <<- fir1(n=FiltLgth, w=CutOFF, type = "low", window = hamming(FiltLgth+1), scale = TRUE)
      Filtered <<- filtfilt(filt=coeff, x=RawData)
      Filtered <<- Filtered[(NPad+1):(NPad+LL)]          #eliminates initial and end padded values
      if (BkgSubtr == "1") Filtered <<- Filtered + BackGnd  #after filtering add background

#recover filter distoritions on signal amplitude
      Filtered <<- BkgSubtraction(Filtered)
      Filtered <<- normalize(Filtered)
      Filtered <<- Filtered-0.5*(mean(Filtered[1:5]) + mean(Filtered[(LL-5):LL])) #shift the Filtered edges at zero
      tmp <- BkgSubtraction(Object@.Data[[2]])
      SaveAmpli <<- TRUE
      tmp <- normalize(tmp)
      A1 <- SigAmpli[2]-SigAmpli[1]
      Filtered <<- Filtered*A1         #Adapt filtered signal on the bckgnd subtracted original signal
      Filtered <<- Filtered + BackGnd
      PlotData()
      return()
   }

   ButtFilt <- function() {
      FiltInfo <<- paste("Butterworth filter, ", tclvalue(F7Ord), ", Degree: ", tclvalue(F7Coff), sep="")
      coeff <<- NULL
      RawData <<- NULL
      Filtered <<- NULL
      BackGnd <<- NULL
      BkgSubtr <<- tclvalue(F7Bkg)
      FiltLgth <- as.numeric(tclvalue(F7Ord))
      NPad <- FiltLgth*CutOFF
      if (NPad < 30) NPad <- 30      #minimum number of padding elements
      CutOFF <<- (20.5-CutOFF)/20    #here cut-off were freq/Nyquist for example 40/500Hz (sampling freq = 1kHz) where 40Hz is adapted to the typical XPS noise
      ##preprocessing
      RawData <<- unlist(Object@.Data[[2]])
      if (BkgSubtr == "1") RawData <<- BkgSubtraction(RawData)
      LL <- length(RawData)
      RawData <<- c(rep(RawData[1],NPad), RawData, rep(RawData[LL],NPad)) #WTdeg Padding at edges
      ##Filtering
      coeff <<- butter(n=FiltLgth, W=CutOFF, type = "low", window = hamming(FiltLgth+1), scale = TRUE)
      Filtered <<- filtfilt(filt=coeff, x=RawData)
      Filtered <<- Filtered[(NPad+1):(NPad+LL)]    #eliminates initial and end padded values
      if (BkgSubtr == "1") Filtered <<- Filtered + BackGnd  #after filtering add background
      PlotData()
      return()
   }

   FindPattern <- function(TxtVect, Pattern){
      chrPos <- NULL
      LL <- length(TxtVect)
      for(ii in 1:LL){
          Pos <- gregexpr(pattern=Pattern,TxtVect[ii])  #the result of gregexpr is a list containing the character position of D.x x=differentiation degree
          if (Pos[[1]][1] > 0) {
              chrPos[1] <- ii
              chrPos[2] <- Pos[[1]][1]
              break
          }
      }
      return(chrPos)
   }
   
   EnblDsbl <- function(){
      WidgetState(F2filter, "disabled")
      WidgetState(F3filter, "disabled")
      WidgetState(F4filter, "disabled")
      WidgetState(F5filter, "disabled")
      WidgetState(F6filter, "disabled")
      WidgetState(F7filter, "disabled")
      WidgetState(F2Response, "disabled")
      WidgetState(F2ResetBtn, "disabled")
      WidgetState(F3Response, "disabled")
      WidgetState(F3ResetBtn, "disabled")
      WidgetState(F4Response, "disabled")
      WidgetState(F4ResetBtn, "disabled")
      WidgetState(F5Response, "disabled")
      WidgetState(F5ResetBtn, "disabled")
      WidgetState(F6Response, "disabled")
      WidgetState(F6ResetBtn, "disabled")
      WidgetState(F7Response, "disabled")
      WidgetState(F7ResetBtn, "disabled")
      WidgetState(SaveNewButt, "disabled")
      WidgetState(SaveButt, "disabled")
      WidgetState(SaveTestButt, "disabled")
      WidgetState(F3group2, "disabled")
      WidgetState(F3group3, "disabled")
   }


#----- Variabiles -----
   plot.new()
   if (is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   ActiveFName <- get("activeFName", envir=.GlobalEnv)  #cload the XPSSample name (string)
   FName <- get(ActiveFName, envir=.GlobalEnv)   #load the active XPSSample (data)
   FNameList <- XPSFNameList()                   #list of all XPSSamples loaded in .GlobalEnv
   FNameIdx <- grep(ActiveFName,FNameList)
   SpectIndx <- NULL
   Object <- NULL
   SpectList <- XPSSpectList(ActiveFName)

   RawData <- NULL
   Filtered <- NULL
   coeff <- NULL
   CutOFF <- NULL
   FiltResp <- NULL
   BackGnd <- NULL
   BkgSubtr <- FALSE
   SigAmpli <- NULL
   SaveAmpli <- FALSE
   filterType <- c("AutoRegressive", "MovingAverage")
   waveletType <- list("Daubechies", "LeastAsymmetric", "BestLocalized", "Coiflet")
   waveletType[["Daubechies"]] <- c(2,4,6,8,10,12,14,16,18,20)
   waveletType[["LeastAsymmetric"]] <- c(8,10,12,14,16,18,20)
   waveletType[["BestLocalized"]] <- c(14,18,20)
   waveletType[["Coiflet"]] <- c(6,12,18,24,30)
   waveNumber <- "  "
   FiltInfo <- NULL
   pos <- list(x=NULL, y=NULL)

#----- WIDGET -----
   mainFwin <- tktoplevel()
   tkwm.title(mainFwin,"DATA FILTERING")
   tkwm.geometry(mainFwin, "+100+50")

   F1group1 <- ttkframe(mainFwin, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(F1group1, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   F1group2 <- ttkframe(F1group1, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(F1group2, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   F1frame1 <- ttklabelframe(F1group2, text="Select XPS Sample", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(F1frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   XS <- tclVar()
   F1XpsSpect <- ttkcombobox(F1frame1, width = 20, textvariable = XS, values = FNameList)
   tkgrid(F1XpsSpect, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(F1XpsSpect, "<<ComboboxSelected>>", function(){
                        ActiveFName <<- tclvalue(XS)
                        FName <<- get(ActiveFName,envir=.GlobalEnv)  #carico in SampID il relativo XPSSAmple
                        SpectList <<- XPSSpectList(ActiveFName)
                        SpectIndx <<- 1
                        tkconfigure(F1CoreLine, values=SpectList)
                        plot(FName)
                  })

   F1frame2 <- ttklabelframe(F1group2, text="Select Core-Line", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(F1frame2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
   CL <- tclVar()
   F1CoreLine <- ttkcombobox(F1frame2, width = 20, textvariable = CL, values = FNameList)
   tkgrid(F1CoreLine, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(F1CoreLine, "<<ComboboxSelected>>", function(){
                        XPSComponent <- tclvalue(CL)
                        XPSComponent <- unlist(strsplit(XPSComponent, "\\."))   #skip the ".NUMBER" at beginning CoreLine name
                        SpectIndx <- as.integer(XPSComponent[1])
                        SpectName <- XPSComponent[2]
                        assign("activeSpectName", SpectName,.GlobalEnv) #set the activeSpectral name to the actual CoreLine name
                        assign("activeSpectIndx", SpectIndx,.GlobalEnv) #set the activeSpectIndex to the actual CoreLine
                        Object <<- FName[[SpectIndx]]
                        plot(FName[[SpectIndx]])
                        WidgetState(F2group1, "normal")
                        WidgetState(F3group1, "normal")
                        WidgetState(F4group1, "normal")
                        WidgetState(F5group1, "normal")
                        WidgetState(F6group1, "normal")
                        WidgetState(F7group1, "normal")
                        WidgetState(F2filter, "disabled")
                        WidgetState(F3filter, "disabled")
                        WidgetState(F4filter, "disabled")
                        WidgetState(F5filter, "disabled")
                        WidgetState(F6filter, "disabled")
                        WidgetState(F7filter, "disabled")
                  })

   F1frame3 <- ttklabelframe(F1group2, text="Update XPS Sample List", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(F1frame3, row = 1, column = 3, padx = 5, pady = 5, sticky="w")
   UpdateButt <- tkbutton(F1frame3, text="UPDATE", width=15, command=function(){
                        FNameList <<- XPSFNameList()  #list of all XPSSamples loaded in .GlobalEnv
                        tkconfigure(F1XpsSpect, values=FNameList)
                        tkconfigure(F1CoreLine, values="")
                        plot.new()

                  })
   tkgrid(UpdateButt, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

#   tkgrid( ttklabel(F1group1, text="    "),
#          row = 2, column = 1, padx = 5, pady = 5, sticky="w")


#===== NoteBook =====

   NB <- ttknotebook(F1group1)
   tkgrid(NB, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

# --- Tab1 ---
   F2group1 <- ttkframe(NB,  borderwidth=0, padding=c(0,0,0,0) )
   tkadd(NB, F2group1, text=" SAVITZKY GOLAY ")
   F2group2 <- ttkframe(F2group1,  borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(F2group2, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   F2frame1 <- ttklabelframe(F2group2, text="Degree of Noise Rejection", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(F2frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   F2Deg <- tclVar()
   F2FiltDeg <- ttkcombobox(F2frame1, width = 20, textvariable = F2Deg, values = c(1:20))
   tkgrid(F2FiltDeg, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(F2FiltDeg, "<<ComboboxSelected>>", function(){
                        WidgetState(F2filter, "normal")
                  })

   F2Bkg <- tclVar(FALSE)
   BkgSubtr2 <- tkcheckbutton(F2group2, text="BKG Subtraction", variable=F2Bkg, onvalue = 1, offvalue = 0)
   tkgrid(BkgSubtr2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   F2group3 <- ttkframe(F2group1,  borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(F2group3, row = 2, column = 1, padx = 0, pady = 0, sticky="w")

   F2filter <- tkbutton(F2group3, text="FILTER", width=15, command=function(){
                        if(tclvalue(XS) == "" || tclvalue(CL) == ""){
                           tkmessageBox(message="Please select The XPSSample and the Core-Line to filter",
                                        title = "WARNING", icon="warning")
                           return()
                        }
                        SavGolay()
                        WidgetState(F2Response, "normal")
                        WidgetState(F2ResetBtn, "normal")
                        WidgetState(SaveNewButt, "normal")
                        WidgetState(SaveButt, "normal")
                        WidgetState(SaveTestButt, "normal")
                  })
   tkgrid(F2filter, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   F2Response <- tkbutton(F2group3, text="FREQ. RESPONSE", width=15, command=function(){
                        FiltResp <- freqz(filt=coeff, Fs=1)
                        plot(FiltResp)
                  })
   tkgrid(F2Response, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   F2ResetBtn <- tkbutton(F2group3, text="RESET", width=12, command=function(){
                        RawData <<- NULL
                        Filtered <<- NULL
                        FiltResp <<- NULL
                        BackGnd <<- 0
                        plot(Object)
                  })
   tkgrid(F2ResetBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")


# --- Tab2 ---
   F3group1 <- ttkframe(NB,  borderwidth=0, padding=c(0,0,0,0) )
   tkadd(NB, F3group1, text="AUTOREGRESSIVE : MOVING AVERAGE")
   F3group2 <- ttkframe(F3group1,  borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(F3group2, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   F3frame1 <- ttklabelframe(F3group2, text="Select Filter Type", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(F3frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   F3Type <- tclVar()
   F3FiltType <- ttkcombobox(F3frame1, width = 20, textvariable = F3Type, values = filterType)
   tkgrid(F3FiltType, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(F3FiltType, "<<ComboboxSelected>>", function(){
                        WidgetState(F3FiltDeg, "normal")
                        WidgetState(BkgSubtr3, "normal")
                        WidgetState(MatchEdges3, "normal")
                  })

   F3frame2 <- ttklabelframe(F3group2, text="Degree of Noise Rejection", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(F3frame2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
   F3Deg <- tclVar()
   F3FiltDeg <- ttkcombobox(F3frame2, width = 20, textvariable = F3Deg, values = c(2:15))
   tkgrid(F3FiltDeg, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(F3FiltDeg, "<<ComboboxSelected>>", function(){
                        WidgetState(F3filter, "normal")
                  })

   F3Bkg <- tclVar(FALSE)
   BkgSubtr3 <- tkcheckbutton(F3group2, text="BKG Subtraction", variable=F3Bkg, onvalue = 1, offvalue = 0)
   tkgrid(BkgSubtr3, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

   F3Edges <- tclVar(FALSE)
   MatchEdges3 <- tkcheckbutton(F3group2, text="Match Edges", variable=F3Edges, onvalue = 1, offvalue = 0)
   tkgrid(MatchEdges3, row = 1, column = 4, padx = 5, pady = 5, sticky="w")

   F3group3 <- ttkframe(F3group1,  borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(F3group3, row = 2, column = 1, padx = 0, pady = 0, sticky="w")
   F3filter <- tkbutton(F3group3, text="FILTER", width=15, command=function(){
                        if(tclvalue(XS) == "" || tclvalue(CL) == ""){
                           tkmessageBox(message="Please select The XPSSample and the Core-Line to filter",
                                        title = "WARNING", icon="warning")
                           return()
                        }
                        AutoRegMedMob()
                        WidgetState(F3Response, "normal")
                        WidgetState(F3ResetBtn, "normal")
                        WidgetState(SaveNewButt, "normal")
                        WidgetState(SaveButt, "normal")
                        WidgetState(SaveTestButt, "normal")
                  })
   tkgrid(F3filter, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   F3Response <- tkbutton(F3group3, text="FREQ. RESPONSE", width=15, command=function(){
                      if(tclvalue(F3Type) == "AutoRegressive"){
                         FiltLgth <- as.numeric(tclvalue(F3Deg))
                         FiltResp <- freqz(filt=coeff, a=1, region="half", Fs=1) #signal::impz(filt=1, a=coeff)
                      } else {
                         FiltResp <- freqz(filt=coeff, a=1, region="half", Fs=1)
                      }
                  })
   tkgrid(F3Response, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   F3ResetBtn <- tkbutton(F3group3, text="RESET", width=12, command=function(){
                        RawData <<- NULL
                        Filtered <<- NULL
                        FiltResp <<- NULL
                        BackGnd <<- 0
                        plot(Object)
                  })
   tkgrid(F3ResetBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

# --- Tab3 ---
   F4group1 <- ttkframe(NB,  borderwidth=0, padding=c(0,0,0,0) )
   tkadd(NB, F4group1, text="FFT FILTER")
   F4group2 <- ttkframe(F4group1,  borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(F4group2, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   F4frame1 <- ttklabelframe(F4group2, text="Degree of Noise Rejection", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(F4frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   F4Deg <- tclVar()
   F4FiltDeg <- ttkcombobox(F4frame1, width = 20, textvariable = F4Deg, values = c(1:15))
   tkgrid(F4FiltDeg, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(F4FiltDeg, "<<ComboboxSelected>>", function(){
                        WidgetState(F4filter, "normal")
                  })

   F4Bkg <- tclVar(FALSE)
   BkgSubtr4 <- tkcheckbutton(F4group2, text="BKG Subtraction", variable=F4Bkg, onvalue = 1, offvalue = 0)
   tkgrid(BkgSubtr4, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

   F4group3 <- ttkframe(F4group1,  borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(F4group3, row = 2, column = 1, padx = 0, pady = 0, sticky="w")
   F4filter <- tkbutton(F4group3, text="FILTER", width=15, command=function(){
                        if(tclvalue(XS) == "" || tclvalue(CL) == ""){
                           tkmessageBox(message="Please select The XPSSample and the Core-Line to filter",
                                        title = "WARNING", icon="warning")
                           return()
                        }
                        FFTfilt()
                        WidgetState(F4Response, "normal")
                        WidgetState(F4ResetBtn, "normal")
                        WidgetState(SaveNewButt, "normal")
                        WidgetState(SaveButt, "normal")
                        WidgetState(SaveTestButt, "normal")
                  })
   tkgrid(F4filter, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   F4Response <- tkbutton(F4group3, text="FREQ. RESPONSE", width=15, command=function(){
                        FiltResp <- freqz(filt=coeff, a=1, whole=FALSE, Fs=1)
                        plot(FiltResp)
                  })
   tkgrid(F4Response, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   F4ResetBtn <- tkbutton(F4group3, text="RESET", width=12, command=function(){
                        RawData <<- NULL
                        Filtered <<- NULL
                        FiltResp <<- NULL
                        BackGnd <<- 0
                        plot(Object)
                  })
   tkgrid(F4ResetBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")


# --- Tab4 ---
   F5group1 <- ttkframe(NB,  borderwidth=0, padding=c(0,0,0,0) )
   tkadd(NB, F5group1, text="WAVELETS FILTERING")
   F5group2 <- ttkframe(F5group1,  borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(F5group2, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   F5frame1 <- ttklabelframe(F5group2, text="Select N. Wavelets", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(F5frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   F5WavN <- tclVar()
   F5WTnum <- ttkcombobox(F5frame1, width = 20, textvariable = F5WavN, values = c(1:10))
   tkgrid(F5WTnum, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   F5frame2 <- ttklabelframe(F5group2, text="Degree of Noise Rejection", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(F5frame2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
   F5Deg <- tclVar()
   F5WTLevel <- ttkcombobox(F5frame2, width = 20, textvariable = F5Deg, values = c(1:10))
   tkgrid(F5WTLevel, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(F5WTLevel, "<<ComboboxSelected>>", function(){
                        WidgetState(F5filter, "normal")
                  })

   F5Bkg <- tclVar(FALSE)
   BkgSubtr5 <- tkcheckbutton(F5group2, text="BKG Subtraction", variable=F5Bkg, onvalue = 1, offvalue = 0)
   tkgrid(BkgSubtr5, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

   F5group3 <- ttkframe(F5group1,  borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(F5group3, row = 2, column = 1, padx = 0, pady = 0, sticky="w")
   F5filter <- tkbutton(F5group3, text="FILTER", width=15, command=function(){
                        if(tclvalue(XS) == "" || tclvalue(CL) == ""){
                           tkmessageBox(message="Please select The XPSSample and the Core-Line to filter",
                                        title = "WARNING", icon="warning")
                           return()
                        }
                        if( is.na(match("wavelets", Pkgs)) == TRUE ){      #check if the package 'wavelets' is installed
                            txt <- "Package 'wavelets' not installed. \nOption 'Wavelets filtering' cannot be done"
                            tkmessageBox(message=txt, title="WARNING", icon="error")
                            return()
                        }
                        msSmoothMRA()
                        WidgetState(F5Response, "normal")
                        WidgetState(F5ResetBtn, "normal")
                        WidgetState(SaveNewButt, "normal")
                        WidgetState(SaveButt, "normal")
                        WidgetState(SaveTestButt, "normal")
                  })
   tkgrid(F5filter, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   F5Response <- tkbutton(F5group3, text="FREQ. RESPONSE", width=15, command=function(){
                        FiltResp <- freqz(filt=coeff, a=1, region="half", Fs=1)  #this is the LOW-PASS filter frequency response
                        plot(FiltResp)
                  })
   tkgrid(F5Response, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   F5ResetBtn <- tkbutton(F5group3, text="RESET", width=12, command=function(){
                        RawData <<- NULL
                        Filtered <<- NULL
                        FiltResp <<- NULL
                        BackGnd <<- 0
                        plot(Object)
                  })
   tkgrid(F5ResetBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

# --- Tab5 ---
   F6group1 <- ttkframe(NB,  borderwidth=0, padding=c(0,0,0,0) )
   tkadd(NB, F6group1, text="FIR FILTER")
   F6group2 <- ttkframe(F6group1,  borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(F6group2, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   F6frame1 <- ttklabelframe(F6group2, text="Select Filter Order", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(F6frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   F6Ord <- tclVar()
   F6FiltOrder <- ttkcombobox(F6frame1, width = 20, textvariable = F6Ord, values = c(10,20,40,60,80, 120))
   tkgrid(F6FiltOrder, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(F6FiltOrder, "<<ComboboxSelected>>", function(){
                        WidgetState(F6CutOFF, "normal")
                  })

   F6frame2 <- ttklabelframe(F6group2, text="Degree of Noise Rejection", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(F6frame2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
   F6Coff <- tclVar()
   F6CutOFF <- ttkcombobox(F6frame2, width = 20, textvariable = F6Coff, values = c(c(1:20), "Custom"))
   tkgrid(F6CutOFF, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(F6CutOFF, "<<ComboboxSelected>>", function(){
                        WidgetState(F6filter, "normal")
                        CutOFF <<- tclvalue(F6Coff)
                        if (CutOFF == "Custom") {
                            tclvalue(F6Coff) <- "Cut.Off Freq. "  #sets the initial msg
                            EnterCutOff <- ttkentry(F6frame2, textvariable=F6Coff, width=10, foreground="grey")
                            tkbind(EnterCutOff, "<FocusIn>", function(K){
                                     tclvalue(F6Coff) <- ""
                                     tkconfigure(EnterCutOff, foreground="red")
                                  })
                            tkbind(EnterCutOff, "<Key-Return>", function(K){
                                     tkconfigure(EnterCutOff, foreground="black")
                                     CutOFF <<- as.numeric(tclvalue(F6Coff))
                                     tkdestroy(F6CutOFF)
                                  })
                            tkgrid(EnterCutOff, row = 1, column = 3, padx = 5, pady = 5, sticky="w")
                        } else {
                            CutOFF <<- as.numeric(CutOFF)
                        }
                        WidgetState(F6filter, "normal")
                  })

   F6Bkg <- tclVar(FALSE)
   BkgSubtr6 <- tkcheckbutton(F6group2, text="BKG Subtraction", variable=F6Bkg, onvalue = 1, offvalue = 0)
   tkgrid(BkgSubtr6, row = 1, column = 4, padx = 5, pady = 5, sticky="w")

   F6group3 <- ttkframe(F6group1,  borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(F6group3, row = 2, column = 1, padx = 0, pady = 0, sticky="w")
   F6filter <- tkbutton(F6group3, text="FILTER", width=15, command=function(){
                        if(tclvalue(XS) == "" || tclvalue(CL) == ""){
                           tkmessageBox(message="Please select The XPSSample and the Core-Line to filter",
                                        title = "WARNING", icon="warning")
                           return()
                        }
                        FirFilt()
                        WidgetState(F6Response, "normal")
                        WidgetState(F6ResetBtn, "normal")
                        WidgetState(SaveNewButt, "normal")
                        WidgetState(SaveButt, "normal")
                        WidgetState(SaveTestButt, "normal")
                  })
   tkgrid(F6filter, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   F6Response <- tkbutton(F6group3, text="FREQ. RESPONSE", width=15, command=function(){
                        FiltResp <- freqz(filt=coeff, a=1, n=512, region="half", Fs=1)
                        plot(FiltResp)
                  })
   tkgrid(F6Response, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   F6ResetBtn <- tkbutton(F6group3, text="RESET", width=12, command=function(){
                        RawData <<- NULL
                        Filtered <<- NULL
                        FiltResp <<- NULL
                        BackGnd <<- 0
                        plot(Object)
                  })
   tkgrid(F6ResetBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

# --- Tab6 ---
   F7group1 <- ttkframe(NB,  borderwidth=0, padding=c(0,0,0,0) )
   tkadd(NB, F7group1, text="BUTTERWORTH FILTER")
   F7group2 <- ttkframe(F7group1,  borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(F7group2, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   F7frame1 <- ttklabelframe(F7group2, text="Select Filter Order", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(F7frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   F7Ord <- tclVar()
   F7FiltOrder <- ttkcombobox(F7frame1, width = 20, textvariable = F7Ord, values = c(6,8,12,16))
   tkgrid(F7FiltOrder, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(F7FiltOrder, "<<ComboboxSelected>>", function(){
                        WidgetState(F7CutOFF, "normal")
                  })

   F7frame2 <- ttklabelframe(F7group2, text="Degree of Noise Rejection", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(F7frame2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
   F7Coff <- tclVar()
   F7CutOFF <- ttkcombobox(F7frame2, width = 20, textvariable = F7Coff, values = c(c(1:20), "Custom"))
   tkgrid(F7CutOFF, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(F7CutOFF, "<<ComboboxSelected>>", function(){
                        WidgetState(F7filter, "normal")
                        CutOFF <<- tclvalue(F6Coff)
                        if (CutOFF == "Custom") {
                            tclvalue(F7Coff) <- "Cut.Off Freq. "  #sets the initial msg
                            EnterCutOff <- ttkentry(F7frame2, textvariable=F7Coff, width=10, foreground="grey")
                            tkbind(EnterCutOff, "<FocusIn>", function(K){
                                     tclvalue(F7Coff) <- ""
                                     tkconfigure(EnterCutOff, foreground="red")
                                  })
                            tkbind(EnterCutOff, "<Key-Return>", function(K){
                                     tkconfigure(EnterCutOff, foreground="black")
                                     CutOFF <<- as.numeric(tclvalue(F7Coff))
                                     tkdestroy(CutOFF)
                                  })
                            tkgrid(EnterCutOff, row = 1, column = 3, padx = 5, pady = 5, sticky="w")
                        } else {
                            CutOFF <<- as.numeric(CutOFF)
                        }
                        WidgetState(F7filter, "normal")
                  })

   F7Bkg <- tclVar(FALSE)
   BkgSubtr7 <- tkcheckbutton(F7group2, text="BKG Subtraction", variable=F7Bkg, onvalue = 1, offvalue = 0)
   tkgrid(BkgSubtr7, row = 1, column = 4, padx = 5, pady = 5, sticky="w")

   F7group3 <- ttkframe(F7group1,  borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(F7group3, row = 2, column = 1, padx = 0, pady = 0, sticky="w")
   F7filter <- tkbutton(F7group3, text="FILTER", width=15, command=function(){
                        if(tclvalue(XS) == "" || tclvalue(CL) == ""){
                           tkmessageBox(message="Please select The XPSSample and the Core-Line to filter",
                                        title = "WARNING", icon="warning")
                           return()
                        }
                        CutOFF <<- as.numeric(tclvalue(F7Coff))
                        ButtFilt()
                        WidgetState(F7Response, "normal")
                        WidgetState(F7ResetBtn, "normal")
                        WidgetState(SaveNewButt, "normal")
                        WidgetState(SaveButt, "normal")
                        WidgetState(SaveTestButt, "normal")
                  })
   tkgrid(F7filter, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   F7Response <- tkbutton(F7group3, text="FREQ. RESPONSE", width=15, command=function(){
                        FiltResp <- freqz(filt=coeff$b, a=coeff$a, n=512, region="half", Fs=1)
                        plot(FiltResp)
                  })
   tkgrid(F7Response, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   F7ResetBtn <- tkbutton(F7group3, text="RESET", width=12, command=function(){
                        RawData <<- NULL
                        Filtered <<- NULL
                        FiltResp <<- NULL
                        BackGnd <<- 0
                        plot(Object)
                  })
   tkgrid(F7ResetBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

#--- Common buttons

   FButtgroup1 <- ttkframe(F1group1,  borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(FButtgroup1, row = 4, column = 1, padx = 0, pady = 0, sticky="w")

   SaveNewButt <- tkbutton(FButtgroup1, text="SAVE AS A NEW CORE LINE", command=function(){
                        SpectIndx <- get("activeSpectIndx",.GlobalEnv)
                        Symbol <- get("activeSpectName",.GlobalEnv)
                        NCL <- length(FName)  #number of XPSSample corelines
                        CLNames <- names(FName)
                        FName[[NCL+1]] <<- FName[[SpectIndx]]
                        FName[[NCL+1]]@Symbol <<- Symbol
                        chrPos <- FindPattern(Info, "   ::: Smoothing")
                        if (length(chrPos[1]) > 0){
                            Info <- FName[[NCL+1]]@Info
                            nI <- length(Info)
                            Info[nI+1] <- paste("   ::: Smoothing: ", FiltInfo, sep="")
                            FName[[NCL+1]]@Info <<- Info
                        }
                        FName@names <<- c(CLNames,Symbol)
                        FName[[NCL+1]]@.Data[[2]] <<- Filtered
                        if (length(FName[[SpectIndx]]@RegionToFit$y>0)){ #RegionToFit defined
                           rng <- range(FName[[SpectIndx]]@RegionToFit$x)
                           if (Object@Flags[1]==TRUE) {rng <- rev(rng)}    #Binding energy set
                           idx1 <- which(FName[[SpectIndx]]@.Data[[1]] == rng[1])
                           idx2 <- which(FName[[SpectIndx]]@.Data[[1]] == rng[2])
                           FName[[SpectIndx]]@RegionToFit$y <<- Filtered[idx1:idx2]
                        }
                        assign(ActiveFName, FName,envir=.GlobalEnv)   #save XPSSample with additional smoothed coreline
#                        assign("activeSpectIndx", (NCL+1), envir=.GlobalEnv)      #set the activeSpectIndx be the smoothed core line
                        RawData <<- NULL
                        Filtered <<- NULL
                        BackGnd <<- 0
                        plot(FName)
                        XPSSaveRetrieveBkp("save")

                  })
   tkgrid(SaveNewButt, row = 1, column = 1, padx = 5, pady = 5, sticky="w")


   SaveButt <- tkbutton(FButtgroup1, text="SAVE IN THE ORIGINAL CORE LINE", command=function(){
                        SpectIndx <- get("activeSpectIndx",.GlobalEnv)
                        FName[[SpectIndx]]@.Data[[2]] <<- Filtered
                        if (length(FName[[SpectIndx]]@RegionToFit$y > 0)){ #RegionToFit defined
                           rng <- range(FName[[SpectIndx]]@RegionToFit$x)
                           if (Object@Flags[1] == TRUE) {rng <- rev(rng)}    #Binding energy set
                           idx1 <- which(FName[[SpectIndx]]@.Data[[1]] == rng[1])
                           idx2 <- which(FName[[SpectIndx]]@.Data[[1]] == rng[2])
                           FName[[SpectIndx]]@RegionToFit$y <<- Filtered[idx1:idx2]
                        }
                        Info <- FName[[SpectIndx]]@Info
#                        xxx <- grep("Smoothing:", Info)     #is the Test Filtering already present?
                        chrPos <- FindPattern(Info, "   ::: Smoothing")
                        if (length(chrPos[1]) > 0) {         #Filtering Comments are present?
                             nI <- length(Info)
                        } else {
                             nI <- length(Info)+1
                        }
                        Info[nI] <- paste("   ::: Smoothing: ", FiltInfo, sep="")  #Add or Change filter information
                        FName[[SpectIndx]]@Info <<- Info
                        assign(ActiveFName, FName,envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                        Object <<- FName[[SpectIndx]]       #update Object used for filtering
                        plot(FName)
                        XPSSaveRetrieveBkp("save")
                  })
   tkgrid(SaveButt, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   SaveTestButt <-  tkbutton(FButtgroup1, text="SAVE AS SMOOTHING TEST", command=function(){
                        SpectIndx <- get("activeSpectIndx",.GlobalEnv)
                        Symbol <- get("activeSpectName",.GlobalEnv)
                        CLNames <- names(FName)
                        LL <- length(FName)
#                        chrPos <- regexpr("ST.", CLNames[SpectIndx])    #Are we working on a Smoothing-Test core line?
                        chrPos <- FindPattern(CLNames[SpectIndx], "ST.")
                        if (length(chrPos[2]) > 0) {                               #Smoothing-Test Coreline IS PRESENT
                           TestIdx <- SpectIndx
                           Info <- FName[[TestIdx]]@Info                #update filter information
                           LL <- length(Info)
                           Info[LL] <- paste("   ::: Smoothing Test: ", FiltInfo, sep="")
                           FName[[TestIdx]]@Info <<- Info
                        } else {
#                           chrPos <- which(regexpr("ST.", CLNames) > 0)  #Find the index if there is a "ST." identifying the Smoothing-Test coreline
                           chrPos <- FindPattern(CLNames, "ST.")
                           if (length(chrPos[2])==0) {                   #Smoothing-Test coreline is NOT present
                              TestIdx <- LL+1                            #Smoothing-Test coreline is added to FName as a new coreline
                              FName[[TestIdx]] <<- FName[[SpectIndx]]    #We are testing a filter on a coreline and save results in the Smoothing-Test coreline
                              FName@names <<- c(CLNames,paste("ST.", Symbol, sep=""))  #modify the names and info of the new Smoothing-Test coreline
                              Info <- FName[[TestIdx]]@Info
                              LL <- length(Info)
                              chrPos <- FindPattern(Info[LL], "   ::: Smoothing Test")
                              if (length(chrPos[1]) == 0){ LL <- LL+1 }    #Smoothing-Test Info still not present
                              Info[LL] <- paste("   ::: Smoothing Test: ", FiltInfo, sep="")
                              FName[[TestIdx]]@Info <<- Info
                              FName[[TestIdx]]@Symbol <<- paste("ST.", Symbol, sep="")
                           } else {
                               TestIdx <- chrPos[2]                            #Smoothing-Test coreline is present
                              Info <- FName[[TestIdx]]@Info
                              LL <- length(Info)
                              Info[LL] <- paste("   ::: Smoothing Test: ", FiltInfo, sep="")
                              FName[[TestIdx]]@Info <<- Info
                           }
                        }
                        FName[[TestIdx]]@.Data[[2]] <<- Filtered         #change the original data with the filtered data
                        if (length(FName[[SpectIndx]]@RegionToFit$y>0)){ #RegionToFit defined
                           rng <- range(FName[[SpectIndx]]@RegionToFit$x)
                           if (Object@Flags[1]==TRUE) {rng <- rev(rng)}    #Binding energy set
                           idx1 <- which(FName[[TestIdx]]@.Data[[1]] == rng[1])
                           idx2 <- which(FName[[TestIdx]]@.Data[[1]] == rng[2])
                           FName[[TestIdx]]@RegionToFit$y <<- Filtered[idx1:idx2]
                        }
                        assign(ActiveFName, FName,envir=.GlobalEnv)      #Save the modified XPSSample in the globalEnv
#                        assign("activeSpectIndx", TestIdx, envir=.GlobalEnv)  #set the activeSpectIndx be the smoothed core line
                        RawData <<- NULL
                        Filtered <<- NULL
                        BackGnd <<- 0
                        plot(FName)
                        XPSSaveRetrieveBkp("save")
                  })
   tkgrid(SaveTestButt, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

   ResetBtn <- tkbutton(FButtgroup1, text="RESET", width=12, command=function(){
                        RawData <<- NULL
                        Filtered <<- NULL
                        BackGnd <<- 0
                        tclvalue(XS) <- ""
                        tclvalue(CL) <- ""
                        EnblDsbl()
                        plot(FName)
                  })
   tkgrid(ResetBtn, row = 1, column = 4, padx = 5, pady = 5, sticky="w")

   exitBtn <- tkbutton(FButtgroup1, text="  EXIT  ", width=12, command=function(){
                        tkdestroy(mainFwin)
                        XPSSaveRetrieveBkp("save")
                  })
   tkgrid(exitBtn, row = 1, column = 5, padx = 5, pady = 5, sticky="w")

   EnblDsbl()


}
