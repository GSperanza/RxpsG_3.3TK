## XPSoverPlot   engine to perform overlay of XPSspectra

#' @title XPSovEngine
#' @description XPSOverlay Engine is a macro called by XPSOverlayGUI
#'    which uses lattice to plot data following the graphical options
#     set by XPSOverlayGUI and stored in PlotParameters and Plot_Args.
#' @param PlotParameters the plot parameters asociated to the XPSOverlayGUI options;
#' @param Plot_Args list of plot options;
#' @param AutoKey_Args list of options for annotation;
#' @param SelectedNames list containing the XPSSample names and the Corelines to be plotted;
#' @param Xlim Xrange of the data to be plotted;
#' @param Ylim Yrange of the data to be plotted;
#' @return Return c(Xlim, Ylim) eventually modified
#' @export
#'

XPSovEngine <-  function(PlotParameters, Plot_Args, AutoKey_Args, SelectedNames, Xlim, Ylim) {

#---  SetPltArgs sets the Plot_Arg list following selections in OverlayGUI
   SetPltArgs <- function(LType,SType , palette, FitStyle) {
        if (PlotParameters$OverlayMode=="Multi-Panel") {
            palette <- rep("black", 20)
        }
        Ylength <- lapply(Y, sapply, length)
        idx <- 1
        cx <<- list()
        levx <<- list()
        for (ii in seq_along(Ylength) ) {               #corro sulle CoreLines dei vari XPSSamples
             tmp1 <- NULL
             tmp2 <- NULL
             Cex <- Plot_Args$cex
             BaseIdx <- CompIdx <- FitIdx <- 1
             for ( jj in seq_along(Ylength[[ii]]) ) {    #jj corre sulle componenti Corelines
                   if (attr(Ylength[[ii]][jj], "names") == "MAIN"     #holds when just the spectrum is plotted
                       || attr(Ylength[[ii]][jj], "names") =="RTF"){  #holds when spectrum + Baseline or fit are plotted
                       Plot_Args$col[idx] <<- palette[ii]
                       Plot_Args$lty[idx] <<- LType[ii]
                       Plot_Args$pch[idx] <<- SType[ii]
                       Plot_Args$cex[idx] <<- Plot_Args$cex
                   }
                   if (attr(Ylength[[ii]][jj], "names") == "BASE"){
                       Plot_Args$col[idx] <<- FitStyle$BaseColor[BaseIdx]
                       Plot_Args$lty[idx] <<- "dashed"
                       Plot_Args$pch[idx] <<- 3 #"Cross"
                       Plot_Args$cex[idx] <<- 0.3*Plot_Args$cex
                       BaseIdx <- BaseIdx+1
                   }
                   if (attr(Ylength[[ii]][jj], "names") == "COMPONENTS" ){
                       Plot_Args$col[idx] <<- FitStyle$CompColor[CompIdx]
                       Plot_Args$lty[idx] <<- FitStyle$Lty[1]
                       Plot_Args$pch[idx] <<- 8 #"Star"  2 #"VoidTriangleUp"
                       Plot_Args$cex[idx] <<- 0.4*Plot_Args$cex
                       CompIdx <- CompIdx+1
                   }
                   if (attr(Ylength[[ii]][jj], "names") == "FIT" ){
                       Plot_Args$col[idx] <<- FitStyle$FitColor[FitIdx]
                       Plot_Args$lty[idx] <<- "solid"
                       Plot_Args$pch[idx] <<- 8 #"Star" 2 #"VoidTriangleUp"
                       Plot_Args$cex[idx] <<- Plot_Args$cex
                       FitIdx <- FitIdx+1
                   }
                   tmp1 <- c(tmp1, rep(ii, times=as.integer(Ylength[[ii]][jj])))   #indice associato ai vari XPSSample
                   tmp2 <- c(tmp2, rep(idx, times=as.integer(Ylength[[ii]][jj])))  #indice associato alle componenti della CoreLine (baseline o componenti del fit)
                   idx <- idx+1
             }
             levx[[ii]] <<- tmp1 #required to distinguish multiple panels
             cx[[ii]] <<- tmp2   #required to distinguish different curves
             Xlimits[[ii]] <<- rev(range(X[[ii]]))  #costruisco una lista di xlim invertiti
  	     }
    }


#---  rescale a vector so that it lies between the specified minimum and maximum
    rescale <- function(x, newrange=c(0,1)) {

	       if (!is.numeric(x) || !is.numeric(newrange)){
	          stop("Must supply numerics for the x and the new scale")
        }
        if (length(newrange) != 2) {
           stop("newrange must be a numeric vector with 2 elements")
        }
        newmin <- min(newrange)
        newmax <- max(newrange)
        oldrange <- range(x)
        if (oldrange[1] == oldrange[2]) {
           if (newmin==0) {
              return(x-oldrange[1])
           } else {
             warning("The supplied vector is a constant. Cannot rescale")
             return(x)
           }
	       } else {
	          ratio <- (newmax - newmin) / (oldrange[2] - oldrange[1])
	          return( newmin + (x - oldrange[1]) * ratio )
        }
    }

#----- OverlayEngine FUNCTIONS -----

#--- Define a NEW XPSSample containing all the spectra to be plotted with related fits (if present)
    XPSSettings <- get("XPSSettings", envir=.GlobalEnv)
    if (length(SelectedNames$XPSSample) == 0) { return() } #no selected XPSSamples to plot
    overlayXPSSample <- new("XPSSample")
    LL<-length(SelectedNames$XPSSample)

    SpectLengths <- NULL
    idx <- 1
    select <- list()
    for(ii in 1:LL){
        if (length(SelectedNames$XPSSample[ii]) > 0 && SelectedNames$XPSSample[ii] != "-----") { #Set XPSSample==FName only if SelectedNames$XPSSample != -----
            FName <- SelectedNames$XPSSample[ii]  #this is a string
            FName <- get(FName, envir=.GlobalEnv) #this is an XPSSample datafile
        }
        SpectName <- SelectedNames$CoreLines[ii]  #Load all the selected  indicated in SelectedNames$XPSSample
        SpectName <- unlist(strsplit(SpectName, "\\."))   #skip the initial number at beginning of the CoreLine name
        SpectIdx <- as.numeric(SpectName[1])
        #now modify the amplitude of the spectrum, baseline, and fit
        FName[[SpectIdx]]@.Data[[2]] <- FName[[SpectIdx]]@.Data[[2]]*SelectedNames$Ampli[ii]    #if an amplification factor was seleted, AmpliFact != 1
        if (length(FName[[SpectIdx]]@Baseline) > 0){
           FName[[SpectIdx]]@Baseline[[2]] <- FName[[SpectIdx]]@Baseline[[2]]*SelectedNames$Ampli[ii]
           FName[[SpectIdx]]@RegionToFit$y <- FName[[SpectIdx]]@RegionToFit$y*SelectedNames$Ampli[ii]
        }
        LL <- length(FName[[SpectIdx]]@Components)
        if (LL > 0){
           for(jj in 1:LL){
               FName[[SpectIdx]]@Components[[jj]]@ycoor <- FName[[SpectIdx]]@Components[[jj]]@ycoor*SelectedNames$Ampli[ii]
           }
           FName[[SpectIdx]]@Fit$y <- FName[[SpectIdx]]@Fit$y*SelectedNames$Ampli[ii]
        }

        SpectLengths[ii] <- length(FName[[SpectIdx]]@.Data[[2]]) #it is needed for 3Dplot
        overlayXPSSample[[idx]] <- FName[[SpectIdx]]
        names(overlayXPSSample)[idx] <- FName[[SpectIdx]]@Symbol

#--- selection of corelines format: simple spectrum, spectrum+Baseline, spectrum+CompleteFit
        if (PlotParameters$OverlayType == "Spectrum") select[[idx]] <- "MAIN"
        if (PlotParameters$OverlayType == "Spectrum+Baseline") {  # CTRL if baseline and fit components are present
           if (length(FName[[SpectIdx]]@RegionToFit) > 0) {       # if not only baseline or main spectrum are ploted
              select[[idx]] <- c("RTF", "BASE")
           } else {
              select[[idx]] <- "MAIN"
           }
        }
        if (PlotParameters$RTFLtd == TRUE && length(FName[[SpectIdx]]@RegionToFit) > 0) {  # Plot limited to the RegionToFit
            select[[idx]] <- "RTF"
        }
        if (PlotParameters$OverlayType == "Spectrum+Fit") {
           if (length(FName[[SpectIdx]]@Components) > 0) {
              select[[idx]] <- c("RTF", "BASE", "COMPONENTS", "FIT")
           } else if (length(FName[[SpectIdx]]@RegionToFit) > 0) {
              select[[idx]] <- c("RTF", "BASE")
           } else {
              select[[idx]] <- "MAIN"
           }
        }
        idx <- idx+1
    }
    NXPSSamp <- idx-1

# set Titles and axis labels
    activeFName <- get("activeFName", envir=.GlobalEnv)       #load the active XPSSample
    FName <- get(activeFName, envir=.GlobalEnv)               #load the active XPSSample
    SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv)     #load the active spectrum index
    SpectName <- get("activeSpectName", envir=.GlobalEnv)     #load the active spectrum name
    if (length(Plot_Args$xlab$label) == 0) Plot_Args$xlab$label <- FName[[SpectName]]@units[1] #set the axis labels if not defined
    if (length(Plot_Args$ylab$label) == 0) Plot_Args$ylab$label <- FName[[SpectName]]@units[2]

#--- Now transform XPSSample into a list
#--- The asList function allows including/skipping fit components, baseline etc. following the select options
#--- NOTE USE of sapply instead of lapply!!!
    XPSSampLen <- length(overlayXPSSample)
    XPSSampNames <- names(overlayXPSSample)
    Nspettri <- length(XPSSampNames)
    X <- NULL
    Y <- NULL
    for (ii in 1:NXPSSamp){
        tmp <- as.matrix(asList(overlayXPSSample[[ii]], select = select[[ii]]))
        X <- c(X, tmp["x", ])   #X coords of the selected spectra
        Y <- c(Y, tmp["y", ])   #Y coords of the selected spectra
    }
    Xlim0 <- range(sapply(X, sapply, range))
    Ylim0 <- range(sapply(Y, sapply, range))

#--- Now all the data manipulation options
#--- set xlim, and reverseX if BE
    if (is.null(Plot_Args$xlim)) {  #Xlim undefined, No Zoom
	      Plot_Args$xlim <- range(sapply(X, sapply, range))
	      wdth<-Plot_Args$xlim[2]-Plot_Args$xlim[1]
	      Plot_Args$xlim[1] <- Plot_Args$xlim[1]-wdth/15
	      Plot_Args$xlim[2] <- Plot_Args$xlim[2]+wdth/15
       Xlim <- Plot_Args$xlim    #Original Xlim must be used
    }
    if (PlotParameters$Reverse) Plot_Args$xlim <- sort(Plot_Args$xlim, decreasing=TRUE)

#--- Switch BE/KE scale
    if (PlotParameters$SwitchE) {
       XEnergy <- XPSSettings$General[5] #the fifth element of the first column of XPSSettings
       XEnergy<-as.numeric(XEnergy)
       Plot_Args$xlim<-XEnergy-Plot_Args$xlim  #Transform X BE limits in X KE limits
       for (idx in 1:XPSSampLen) {
	          X[[idx]] <- lapply(X[[idx]], function(z, XEnergy){ XEnergy-z }, XEnergy) #Binding to Kinetic energy abscissa
       }
       if (FName[[SpectName]]@Flags[1]==TRUE){ #The original spectra has BE scale
          Plot_Args$xlab$label<-"Kinetic Energy [eV]"
       } else if (FName[[SpectName]]@Flags[1]==FALSE){ #The original spectra has KE scale
          Plot_Args$xlab$label<-"Binding Energy [eV]"
       }
    }

#--- X offset
    if ( ! is.null(PlotParameters$XOffset) ) {
	      if ( XPSSampLen > 1 ) {
           offset_sequence <- seq(from = 0, by = PlotParameters$XOffset, length.out = XPSSampLen)  #costruisco un vettore con gli Xoffset necessari a Xshiftare i vari spettri
       } else {
           offset_sequence <- PlotParameters$XOffset
       }
	      for (idx in 1:XPSSampLen) {
	          X[[idx]] <- lapply(X[[idx]], "+", offset_sequence[idx])
	      }
    }

#--- Here Y alignment
    if (PlotParameters$Align) {
       LL <- length(Y)
       if ( all( sapply(Y, function(z) !is.na(charmatch("BASE", names(z)))) )) {
			       minybkg <- sapply(Y, function(z) min(z$BASE))
			       for (idx in 1:LL) {
			          	Y[[idx]] <- lapply(Y[[idx]], "-", minybkg[idx])
		       	}
       } else {
          minybkg <- sapply(Y, function(z){
                           LL1 <- length(z$MAIN)
                           K <- 8
                           while(LL1-K < 0){
                                 K <- K/2
                           }
                           min(mean(z$MAIN[1:10]), mean(z$MAIN[(LL1-K):LL1]))
                       })
          for(idx in 1:LL) {
			          	Y[[idx]] <- lapply(Y[[idx]], "-", minybkg[idx])
		       	}
       }
    }

#--- Y normalization == scale c(0,1)

    if (PlotParameters$Normalize) {
			    maxy <- sapply(Y, function(z) max(sapply(z, max))) #here Y is the list of XPSSamples with baseline fitComp...
			    if (PlotParameters$NormPeak > 0) {
          yy <- NULL
          for (idx in 1:XPSSampLen){
              Ndata <- length(unlist(X[[idx]]))
              PeakPos <- PlotParameters$NormPeak + offset_sequence[idx]
			           PeakIdx <- findXIndex(unlist(X[[idx]]),  PeakPos) #data may be acquired with different Estep => each X[[ ]] must be analyzed
              p1 <- PeakIdx-20
              p2 <- PeakIdx+20
              if (p1 < 1) p1 <- 1       #limits the range where to find the peak max to available data
              if (p2 > Ndata) p2 <- Ndata
              yy[[idx]] <- lapply(Y[[idx]], function(z) {z[p1:p2]} )
          }
		        maxy <- sapply(yy, function(z) max(sapply(z, max))) #here yy is the a region around the selected peak selected for normalization
       }
			    for (idx in 1:XPSSampLen) {
				       Y[[idx]] <- lapply(Y[[idx]], "/", maxy[idx])
			    }
    }

#--- Y offset
    if ( ! is.null(PlotParameters$YOffset) ) {
	      if ( length(PlotParameters$YOffset)!=XPSSampLen ) {
           offset_sequence <- seq(from = 0, by = PlotParameters$YOffset, length.out = XPSSampLen)  #costruisco un vettore con gli Xoffset necessari a Xshiftare i vari spettri
       } else {
           offset_sequence <- PlotParameters$YOffset
       }
		     for (idx in 1:XPSSampLen) {
		         Y[[idx]] <- lapply(Y[[idx]], "+", offset_sequence[idx])
		     }
    }

#--- After processing set Ylim
    if (is.null(Plot_Args$ylim)) {  #non ho fissato ylim per fare lo zoom
       	Plot_Args$ylim <- range(sapply(Y, sapply, range))
	       wdth <- Plot_Args$ylim[2]-Plot_Args$ylim[1]
	       Plot_Args$ylim[1] <- Plot_Args$ylim[1]-wdth/15
	       Plot_Args$ylim[2] <- Plot_Args$ylim[2]+wdth/15
    }
    Ylim <- Plot_Args$ylim

#------- APPLY GRAPHIC OPTION TO PLOTTING XYplot() ARGS -----------------
    Ylength <- lapply(Y, sapply, length)
    cx <- list()
	   levx <- list()
	   FitStyle <- list(Lty=NULL, BaseColor=NULL, CompColor=NULL, FitColor=NULL)
	   panel <- sapply(Ylength, sum)
    PanelTitles <- NULL
    Xlimits <- list() # buld a list of limits to invert X axis if revers=TRUE

    if (XPSSettings$General[8] == "FC.MonoChrome") {  #BaseLine, FitComponents Fit in grey45, cadetblue, orangered
        PlotParameters$FitCol$BaseColor <- rep(PlotParameters$FitCol$BaseColor[1], 20)
        PlotParameters$FitCol$CompColor <- rep(PlotParameters$FitCol$CompColor[1], 20)
        PlotParameters$FitCol$FitColor <- rep(PlotParameters$FitCol$FitColor[1], 20)
    }
    if ( Plot_Args$type=="l") { #lines are selected for plot
         AutoKey_Args$lines <- TRUE
         AutoKey_Args$points <- FALSE
         if (length(PlotParameters$Colors)==1) {   # B/W LINES
             LType <- Plot_Args$lty                # "solid", "dashed", "dotted" ....
             SType <- rep(NA, 20)
             palette <-  rep(PlotParameters$Colors[1], 20)          # "Black","black","black",....
             FitStyle$Lty <- PlotParameters$CompLty
             FitStyle$BaseColor <- rep("black", 20)
             FitStyle$CompColor <- rep("grey45", 20)
             FitStyle$FitColor <- rep("black", 20)
             SetPltArgs(LType, SType, palette, FitStyle)
         } else if (length(PlotParameters$Colors) > 1) {   # RainBow LINES
             if (length(Plot_Args$lty) == 1){          # chosen "solid" for all core-lines
                 LType <- rep(Plot_Args$lty[1], 20)    # "solid", "solid", "solid", ....
             } else {
                 LType <- Plot_Args$lty                # "solid", "dashed", "dotted" ....
             }
             SType <- rep(NA, 20)
             palette <- PlotParameters$Colors      #"black", "red", "green"....
             FitStyle$Lty <- PlotParameters$CompLty
             FitStyle$BaseColor <- PlotParameters$FitCol$BaseColor
             FitStyle$CompColor <- PlotParameters$FitCol$CompColor   #Single or MultiColors
             FitStyle$FitColor <- PlotParameters$FitCol$FitColor
             SetPltArgs(LType, SType, palette, FitStyle)
         }
    } else if (Plot_Args$type=="p") { #symbols are selected for plot
         AutoKey_Args$lines<-FALSE
         AutoKey_Args$points<-TRUE
         if (length(PlotParameters$Colors)==1) {   # B/W  SYMBOLS
             LType <- rep(NA, 20)
             SType <- Plot_Args$pch                # VoidCircle", "VoidSquare", "VoidTriangleUp" ....
             palette <-  rep(PlotParameters$Colors[1], 20)          # "black","black","black",....
             FitStyle$Lty <- PlotParameters$CompLty
             FitStyle$BaseColor <- rep("black", 20)
             FitStyle$CompColor <- rep("grey45", 20)
             FitStyle$FitColor <- rep("black", 20)
             SetPltArgs(LType, SType, palette, FitStyle)
         } else if (length(PlotParameters$Colors) > 1) {   # RainBow SYMBOLS
             LType <- rep(NA, 20)
             SType <- rep(Plot_Args$pch[1], 20)    # "VoidCircle", "VoidCircle", "VoidCircle", ....
             palette <- PlotParameters$Colors      # "black", "red", "green"....
             FitStyle$Lty <- PlotParameters$CompLty
             FitStyle$BaseColor <- PlotParameters$FitCol$BaseColor
             FitStyle$CompColor <- PlotParameters$FitCol$CompColor   #Single or MultiColors
             FitStyle$FitColor <- PlotParameters$FitCol$FitColor
             SetPltArgs(LType, SType, palette, FitStyle)
         }
    } else if (Plot_Args$type=="b") { #Lines + symbols are selected for plot
         AutoKey_Args$lines<-TRUE
         AutoKey_Args$points<-TRUE
         if (length(PlotParameters$Colors)==1) {   # B/W LINES & SYMBOLS
             LType <- Plot_Args$lty                # "solid", "dashed", "dotted" ....
             SType <- Plot_Args$pch                # "VoidCircle", "VoidSquare", "VoidTriangleUp" ....
             palette <-  rep(PlotParameters$Colors[1], 20)          # "black","black","black",....
             FitStyle$Lty <- PlotParameters$CompLty
             FitStyle$Col <- c("black", "grey45", "black")
             SetPltArgs(LType, SType, palette, FitStyle)
         } else if (length(PlotParameters$Colors) > 1) {   # RainBow LINES & SYMBOLS
             LType <- rep(Plot_Args$lty[1], 20)    #"solid", "solid", "solid", ....
             SType <- rep(Plot_Args$pch[1], 20)    # "VoidCircle", "VoidCircle", "VoidCircle", ....
             palette <- PlotParameters$Colors      #"black", "red", "green"....
             FitStyle$Lty <- PlotParameters$CompLty
             FitStyle$BaseColor <- PlotParameters$FitCol$BaseColor
             FitStyle$CompColor <- PlotParameters$FitCol$CompColor  #Single or MultiColors
             FitStyle$FitColor <- PlotParameters$FitCol$FitColor
             SetPltArgs(LType, SType, palette, FitStyle)
         }
    }

##--- SINGLE PANEL---
    if (PlotParameters$OverlayMode == "Single-Panel") {
       if (length(Plot_Args$main$label) == 0) Plot_Args$main$label<-SpectName
   	   df <- data.frame(x = unname(unlist(X)), y = unname(unlist(Y)) )
	      Plot_Args$x	<- formula("y ~ x")
       Plot_Args$data	<- df
       Plot_Args$groups	<- unlist(cx)
       graph <- do.call(xyplot, args = Plot_Args)
       plot(graph)
       assign("RxpsGGraph", graph, envir=.GlobalEnv)
    }

##--- MULTI PANEL---
    if (PlotParameters$OverlayMode == "Multi-Panel") {
       #define row and columns of the panel matrix
       Nspettri<-length(XPSSampNames)
       Ncol <- 1
       Nrow <- 1
       rr <- FALSE
       while(Nspettri>Ncol*Nrow) {
          if (rr){
             Nrow <- Nrow+1
             rr <- FALSE
          } else {
             Ncol <- Ncol+1
             rr <- TRUE
          }
       }

       Plot_Args$xlim <- NULL  #X range is defined inside xyplot
       Plot_Args$ylim <- NULL  #Y range is defined inside xyplot
       if (PlotParameters$Reverse) { #If reverse is TRUE limits must be given through a list containing:
          Plot_Args$xlim <- Xlimits    #Xlimits[[1]]=X1max, X1min
       }                             #Xlimits[[2]]=X2max, X2min
       cx <- unlist(cx)
       levx <- unlist(levx)
       df <- data.frame(x = unname(unlist(X)), y = unname(unlist(Y)))

#in DF are grouped curves following their category (spect, base, comp, fit)
       PanelTitles <- Plot_Args$PanelTitles  #recover Panel Titles from Plot_Args$PanelTitles. Plot_Args$PanelTitles ia a personal argument ignored by xyplot
       if (length(Plot_Args$main$label) > 0) { PanelTitles <- Plot_Args$main$label } #instead of the default MainLabel uses the title set by the user in OverlayGUI
#in formula y~x is plotted against levx: produces panels showing single XPSSamples
	      Plot_Args$x	<- formula("y ~ x| factor(levx, labels=PanelTitles)")
	      Plot_Args$data	<- df
       Plot_Args$par.settings$strip <- TRUE

	      Plot_Args$groups	<- cx
	      Plot_Args$layout	<- c(Nrow, Ncol)
       Plot_Args$main	<- NULL

	      graph <- do.call(xyplot, args = Plot_Args)
       plot(graph)
       assign("RxpsGGraph", graph, envir=.GlobalEnv)
    }

##--- Pseudo  TreD ---
    if (PlotParameters$XOffset*PlotParameters$YOffset != 0  #both XOffset che YOffset different from zero
        && PlotParameters$OverlayMode=="Single-Panel" ){    #This means no 3D, no Multipanel was set
#if Yoffset is positive
#plot() draws the spectrum LL as the last => LL spectrum is in front. To make the first spectum to be
#in front plot() is applied to a reversed list of spectra: the LL spectrum is the first_plotted the 1 sectrum is the last_plotted
#the effect is the 1th spectrum in from, the LL spectrum on the back
#if Yoffset negative original order is OK
       tmp <- list()
       LL <- length(X)
       for(ii in 1:LL){
          tmp[[ii]] <- X[[(LL-ii+1)]]   #reverse X column order
       }
       X <- tmp
       if (PlotParameters$YOffset > 0){
           tmpLT <- array()
           tmpST <- array()
           tmpPal <- array()
           tmp <- list()
           for(ii in 1:LL){
              tmp[[ii]] <- Y[[(LL-ii+1)]]   #reverse Y column order
              tmpLT[ii] <- LType[(LL-ii+1)]
              tmpST[ii] <- SType[(LL-ii+1)]
              tmpPal[ii] <- palette[(LL-ii+1)]
           }
           Y <- tmp
#group properties cx and levx will be reversed in SetPltArgs
           SetPltArgs(tmpLT, tmpST, tmpPal, FitStyle)

           df <- data.frame(x = unname(unlist(X)), y = unname(unlist(Y)) )
           Plot_Args$data	<- df
	          Plot_Args$x	<- formula("y ~ x")
           Plot_Args$groups	<- unlist(cx)
           graph <- do.call(xyplot, args = Plot_Args)
           plot(graph)
           assign("RxpsGGraph", graph, envir=.GlobalEnv)
       }
       if (PlotParameters$Pseudo3D == TRUE){
           Xrng <- sort(Plot_Args$xlim)
           Yrng <- sort(Plot_Args$ylim)
           if (PlotParameters$XOffset >0) {
              SgmntX1 <- c(Xrng[1], Xrng[1]+(Nspettri-1)*PlotParameters$XOffset) #bottom diag. segment1 on the right Xcoord  row1= x1,x2
              SgmntX2 <- c(Xrng[1]+(Nspettri-1)*PlotParameters$XOffset, Xrng[2]) #bottom orizz. segment2 in front Xcoord  row2= x3,x4
              SgmntX3 <- c(Xrng[1]+(Nspettri-1)*PlotParameters$XOffset, Xrng[1]+(Nspettri-1)*PlotParameters$XOffset) #segment3 vertical on the right Xcoord  row3= x5,x6
              SgmntY1 <- c(Yrng[1], Yrng[1]+(Nspettri-1)*PlotParameters$YOffset) #segment1 Ycoord  row1= y1,y2
              SgmntY2 <- c(Yrng[1]+(Nspettri-1)*PlotParameters$YOffset, Yrng[1]+(Nspettri-1)*PlotParameters$YOffset) #segment2 Ycoord  row2= y3,y4
              SgmntY3 <- c(Yrng[1]+(Nspettri-1)*PlotParameters$YOffset, Yrng[2]) #segment3 Ycoord  row3= y5,y6
           }
           if (PlotParameters$XOffset <0) {
              SgmntX1 <- c(Xrng[2], Xrng[2]+(Nspettri-1)*PlotParameters$XOffset) #segment1 diag. on the left Xcoord  row1= x1,x2
              SgmntX2 <- c(Xrng[2]+(Nspettri-1)*PlotParameters$XOffset, Xrng[1]) #segment2 orizz. Xcoord  row2= x3,x4
              SgmntX3 <- c(Xrng[2]+(Nspettri-1)*PlotParameters$XOffset, Xrng[2]+(Nspettri-1)*PlotParameters$XOffset) #segment3 vertical on the left Xcoord  row3= x5,x6
              SgmntY1 <- c(Yrng[1], Yrng[1]+(Nspettri-1)*PlotParameters$YOffset) #segment1 Ycoord  row1= y1,y2
              SgmntY2 <- c(Yrng[1]+(Nspettri-1)*PlotParameters$YOffset, Yrng[1]+(Nspettri-1)*PlotParameters$YOffset) #segment2 Ycoord  row2= y3,y4
              SgmntY3 <- c(Yrng[1]+(Nspettri-1)*PlotParameters$YOffset, Yrng[2]) #segment3 Ycoord  row3= y5,y6
           }
           Plot_Args$x	<- formula("y ~ x")
           Plot_Args$groups	<- cx <-c(1,1)
           Plot_Args$col[1] <- "gray25"
           Plot_Args$lty[1] <- "dashed"

           Plot_Args$data <- data.frame(x = SgmntX1, y = SgmntY1, groups = factor(cx))
           segm1 <- do.call(xyplot, args = Plot_Args)

           Plot_Args$data <- data.frame(x = SgmntX2, y = SgmntY2, groups = factor(cx))
           segm2 <- do.call(xyplot, args = Plot_Args)
           segm1 <- segm1+as.layer(segm2)      #overlay segment 2 to previous plot stored in graph

           Plot_Args$data <- data.frame(x = SgmntX3, y = SgmntY3, groups = factor(cx))
           segm3 <- do.call(xyplot, args = Plot_Args)
           segm1 <- segm1+as.layer(segm3)      #overlay segment 3 to previous plot stored in graph

           segm1 <- segm1+as.layer(graph)
           plot(segm1)
           assign("RxpsGGraph", graph, envir=.GlobalEnv)
       }
    }#end of Pseudo  TreD

##--- Real TreD ---
    LL <- length(XPSSampNames) #number of spectra
    if (PlotParameters$OverlayMode == "TreD") {
       Z <- NULL
       Cloud_Args <- list()
       for (ii in 1:XPSSampLen){
            Z <- c(Z, rep(ii, SpectLengths[ii]))
       }

   	   df <- data.frame(x =unname(unlist(X)), y=as.vector(Z), z=unname(unlist(Y)) )

#---Set Cloud Style parameters---
       LType <- Plot_Args$lty   # "solid", "dashed", "dotted" ....
       SType <- Plot_Args$pch   # "VoidCircle", "VoidSquare", "VoidTriangleUp" ....
       LW <- Plot_Args$lwd
       CX <- Plot_Args$cex
       if(Plot_Args$type !="l" && Plot_Args$type != "p" && Plot_Args$type != "b") { return() } #if style not defined return!

       if (length(PlotParameters$Colors) == 1) { # B/W Lines
          if (Plot_Args$type=="l"){ #lines
             Cloud_Args <- list(lty=LType,cex=CX,lwd=LW,type="l")    #Plot_Args$lty = "solid", "dashed", "dotted" ....
          } else if(Plot_Args$type=="p"){ #symbols
             Cloud_Args <- list(pch=SType,cex=CX,lwd=LW,type="p")
          } else if(Plot_Args$type=="b"){ #lines & symbols
             Cloud_Args <- list(lty="solid", pch=SType,cex=CX,lwd=LW,type="b")
          }
          Cloud_Args$col <- Plot_Args$col <- rep("black", LL)
       } else if (length(PlotParameters$Colors) > 1) {   # Rainbow Lines
          if (Plot_Args$type=="l"){ # lines
             Cloud_Args <- list(lty=LType,cex=CX,lwd=LW,type="l")
          } else if(Plot_Args$type=="p"){  #symbols
             Cloud_Args <- list(pch=SType,cex=CX,lwd=LW,type="p")  #pch=1  voidcircle
          } else if(Plot_Args$type=="b"){  # lines & symbols
             Cloud_Args <- list(lty=LType, pch=SType,cex=CX,lwd=LW,type="b")
          }
          Cloud_Args$col <- Plot_Args$col <- PlotParameters$Colors
       }
#---axis options---
       if (length(Plot_Args$main$label) == 0) { Plot_Args$main$label <- SpectName }
       if (length(Plot_Args$xlab$label) == 0) { Plot_Args$xlab$label <- FName[[SpectName]]@units[1] }
       if (length(Plot_Args$ylab$label) == 0) { Plot_Args$ylab$label <- "Sample" }
       if (length(Plot_Args$zlab$label) == 0) { Plot_Args$zlab$label <- FName[[SpectName]]@units[2] }
       if (PlotParameters$Normalize == TRUE ) { Plot_Args$zlab$label <- "Intensity [a.u.]" }
       if (PlotParameters$Reverse) { #If reverse==TRUE compute limits and reverse
          Xmax <- max(sapply(Xlimits, max))
          Xmin <- min(sapply(Xlimits, min))
          Cloud_Args$xlim<-c(Xmax,Xmin)  #Xlimits[[1]]=X1max, X1min
       }

       Cloud_Args$ylim<-as.character(c(1:XPSSampLen))  #in Y ho messo i channels Z
       LogOnOff<-Plot_Args$scales$x$log #if x ax log TRUE all axes TRUE
       Cloud_Args$scales<-list(cex=Plot_Args$scales$cex, tck=c(1,0), alternating=c(1), relation="free",
                               x=list(log=LogOnOff), y=list(log=LogOnOff),z=list(log=LogOnOff),
                               arrows=FALSE)
#---3D rendering---
       Cloud_Args$aspect<-as.numeric(PlotParameters$TreDAspect)
       Cloud_Args$screen<-list(x=-60,
                               y= PlotParameters$AzymuthRot,
                               z= PlotParameters$ZenithRot)
       Cloud_Args$main<-list(label=Plot_Args$main$label,cex=Plot_Args$main$cex)
       Cloud_Args$xlab<-list(label=Plot_Args$xlab$label, rot=PlotParameters$AzymuthRot-10, cex=Plot_Args$xlab$cex)
       Cloud_Args$ylab<-list(label=Plot_Args$ylab$label, rot=PlotParameters$AzymuthRot-80, cex=Plot_Args$xlab$cex)
       Cloud_Args$zlab<-list(label=Plot_Args$zlab$label, rot=90, cex=Plot_Args$xlab$cex)
       LL<-length(Y)

#---legend options---
       if (length(Plot_Args$auto.key) > 1) { #auto.key TRUE
          Plot_Args$auto.key$space <- NULL   #Inside top right position
          Plot_Args$auto.key$corner<-c(1,1)
          Plot_Args$auto.key$x<- 0.95
          Plot_Args$auto.key$y<- 0.95

          Cloud_Args$auto.key<-Plot_Args$auto.key
          Cloud_Args$par.settings<-Plot_Args$par.settings
       }
#---plot commands---
	      Cloud_Args$x <- formula("z ~ x*y")
       Cloud_Args$data <- df
       Cloud_Args$groups	<- unlist(cx)

       graph <- do.call(cloud, args=Cloud_Args)
       plot(graph)
       assign("RxpsGGraph", graph, envir=.GlobalEnv)
    }  #---end of TreD

#########################################################################

    if(PlotParameters$Annotate){

       CtrlPlot <- function(){
               x0 <- y0 <- x1 <- y1 <- cex <- col <- adj <- labels <- NULL
               graph <<- AcceptedGraph
               plot(graph)
               if (! is.null(TextPosition$x) && ! is.null(TextPosition$y)){
                   graph <<- graph + layer(data=list(x0=TextPosition$x, y0=TextPosition$y, labels=AnnotateText,
                                       cex=TextSize, col=TextColor),
                                       panel.text(x0, y0, labels=labels, cex=cex, col=col)
                                      )

               }
               if (! is.null(ArrowPosition0$x) && ! is.null(ArrowPosition1$y)){
                   graph <<- graph + layer(data = list(x0=ArrowPosition0$x, y0=ArrowPosition0$y,
                                                  cex=1.1, pch=20, col=TextColor),
                                                  panel.points(x0, y0, pch=20, cex=cex, col=col)
                                          )
                   graph <<- graph + layer(data = list(x0=ArrowPosition0$x, y0=ArrowPosition0$y,
                                                  x1=ArrowPosition1$x, y1=ArrowPosition1$y,
                                                  length = 0.07, col=TextColor),
                                                  panel.arrows(x0, y0, x1, y1, length = length, col = col)
                                          )
                   ArrowPosition0 <<- ArrowPosition1 <<- list(x=NULL, y=NULL)
               }
               plot(graph)
               trellis.unfocus()
       }

       ConvertCoords <- function(pos){
               X1 <- min(Xlim)  
               if (PlotParameters$Reverse) {
                   X1 <- max(Xlim)   #Binding Energy Set
               }
               RangeX <- abs(Xlim[2]-Xlim[1])
               Y1 <- min(Ylim)
               RangeY <- Ylim[2]-Ylim[1]
               PosX <- max(convertX(unit(Xlim, "native"), "points", TRUE))
               PosY <- max(convertY(unit(Ylim, "native"), "points", TRUE))
               if (PlotParameters$Reverse){
                   pos$x <- X1-as.numeric(pos$x)*RangeX/PosX
               } else {
                   pos$x <- X1+as.numeric(pos$x)*RangeX/PosX
               }
               pos$y <- Y1+as.numeric(pos$y)*RangeY/PosY
               return(pos)
      }


#--- variables ---
       Colors <- XPSSettings$Colors
       FontSize <- c(0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)
       FontCol <- XPSSettings$Colors

       TextPosition <- list(x=NULL, y=NULL)
       ArrowPosition0 <- list(x=NULL, y=NULL)
       ArrowPosition1 <- list(x=NULL, y=NULL)
       TextSize <- 1
       TextColor <- "black"
       SpectColor <- "black"
       AnnotateText <- "?"
       OrigGraph <- graph
       AcceptedGraph <- graph   #save the plot before Annotation to make UNDO
       SampData <- setAsMatrix(FName[[SpectIndx]],"matrix") #store spectrum baseline etc in a matrix


#----- Widget -----
       AnnWin <- tktoplevel()
       tkwm.title(AnnWin,"ANNOTATE")
       tkwm.geometry(AnnWin, "+100+50")   #SCREEN POSITION from top-left corner

       AnnGroup <- ttkframe(AnnWin,  borderwidth=0, padding=c(0,0,0,0))
       tkgrid(AnnGroup, row = 1, column = 1, sticky="we")

       INFOframe <- ttklabelframe(AnnGroup, text = " HELP ", borderwidth=2)
       tkgrid(INFOframe, row = 2, column = 1, padx = 5, pady = 5, sticky="we")
       tkgrid( ttklabel(INFOframe, text="1. Mouse-locate Position and Define the Label"),
               row = 1, column=1, pady=2, sticky="w")
       tkgrid( ttklabel(INFOframe, text="2. Change Size and Color if Needed"),
               row = 2, column=1, pady=2, sticky="w")
       tkgrid( ttklabel(INFOframe, text="3. If Label OK ACCEPT or UNDO to the Previous Plot "),
               row = 3, column=1, pady=2, sticky="w")

       Anframe1 <- ttklabelframe(AnnGroup, text = " Text ", borderwidth=2)
       tkgrid(Anframe1, row = 3, column = 1, padx = 5, pady = 5, sticky="we")
       tkgrid( ttklabel(Anframe1, text=" Text to Annotate: "),
               row = 1, column = 1, padx = 5, pady = 5, sticky="we")
       ANNTXT <- tclVar("Label?")
       AnnEntry1 <- ttkentry(Anframe1, textvariable=ANNTXT, foreground="grey")
       tkgrid(AnnEntry1, row = 1, column = 2, padx=5, pady=5, sticky="we")
       tkbind(AnnEntry1, "<FocusIn>", function(K){
                              tclvalue(ANNTXT) <- ""
                              tkconfigure(AnnEntry1, foreground="red")
                       })
       tkbind(AnnEntry1, "<Key-Return>", function(K){
                              tkconfigure(AnnEntry1, foreground="black")
                              AnnotateText <<- tclvalue(ANNTXT)
                       })

       Anframe2 <- ttklabelframe(AnnGroup, text = "  Set Text Position ", borderwidth=2)
       tkgrid(Anframe2, row = 5, column = 1, padx = 5, pady = 5, sticky="we")
       TxtButt <- tkbutton(Anframe2, text=" TEXT POSITION ", command=function(){
                            if (is.null(AnnotateText)){
                                tkmessageBox(message="Set Label Text Please", title="WARNING", icon="warning")
                                return()
                            }
                            trellis.focus("panel", 1, 1, clip.off=TRUE, highlight=FALSE)
                            pos <- list(x=NULL, y=NULL)
                            WidgetState(Anframe1, "disabled")
                            WidgetState(Anframe2, "disabled")
                            WidgetState(Anframe3, "disabled")
                            WidgetState(BtnGroup, "disabled")
                            pos <- grid::grid.locator(unit = "points")
                            TextPosition <<- ConvertCoords(pos)
                            if (is.null(TextPosition$x) && is.null(TextPosition$x))  {
                               return()
                            }
                            TextSize <<- as.numeric(tclvalue(TSIZE))
                            if (is.na(TextSize)) {TextSize <<- 1}
                            TextColor <<- tclvalue(TCOLOR)
                            if (is.na(TextColor)) {TextColor <<- "black"}

                            txt <- paste("Text Position: X = ", round(TextPosition$x, 1), "  Y = ", round(TextPosition$y, 1), sep="")
                            tkconfigure(AnnotePosition, text=txt)
                            WidgetState(Anframe1, "normal")
                            WidgetState(Anframe2, "normal")
                            WidgetState(Anframe3, "normal")
                            WidgetState(BtnGroup, "normal")
                            CtrlPlot()
                       })
       tkgrid(TxtButt, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       AnnotePosition <- ttklabel(Anframe2, text="Text Position                ")
       tkgrid(AnnotePosition, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

       Anframe3 <- ttklabelframe(AnnGroup, text = " Text Size & Color ", borderwidth=2)
       tkgrid(Anframe3, row = 6, column = 1, padx = 5, pady =c(2, 5), sticky="we")
       tkgrid( ttklabel(Anframe3, text=" Size "),
               row = 1, column = 1, padx = 5, pady = 2, sticky="w")
       tkgrid( ttklabel(Anframe3, text=" Color "),
               row = 1, column = 2, padx = 5, pady = 2, sticky="w")

       TSIZE <- tclVar("1")  #TEXT SIZE
       AnnoteSize <- ttkcombobox(Anframe3, width = 15, textvariable = TSIZE, values = FontSize)
       tkbind(AnnoteSize, "<<ComboboxSelected>>", function(){
                            if (is.na(TextPosition)) {
                                tkmessageBox(message="Please set the Label Position first!", title="WARNING: position lacking", icon="warning")
                            } else {
                                TextSize <<- as.numeric(tclvalue(TSIZE))
                                trellis.focus("panel", 1, 1, clip.off=TRUE, highlight=FALSE)
                                CtrlPlot()
                            }
                       })
       tkgrid(AnnoteSize, row = 2, column = 1, padx = 5, pady =c(2, 5), sticky="w")

       TCOLOR <- tclVar("black") #TEXT COLOR
       AnnoteColor <- ttkcombobox(Anframe3, width = 15, textvariable = TCOLOR, values = FontCol)
       tkbind(AnnoteColor, "<<ComboboxSelected>>", function(){
                            if (is.na(TextPosition)) {
                                tkmessageBox(message="Please set the Label Position first!", title="WARNING: position lacking", icon="warning")
                            } else {
                                TextColor <<- tclvalue(TCOLOR)
                                trellis.focus("panel", 1, 1, clip.off=TRUE, highlight=FALSE)
                                CtrlPlot()
                            }
                       })
       tkgrid(AnnoteColor, row = 2, column = 2, padx = 5, pady =c(2, 5), sticky="w")

       BtnGroup <- ttkframe(AnnWin, borderwidth=0, padding=c(0,0,0,0))
       tkgrid(BtnGroup, row = 7, column = 1, sticky="we")
       tkgrid.columnconfigure(BtnGroup, 1, weight = 1)  #needed to extend buttons with sticky="we"

       AddArwButt <- tkbutton(BtnGroup, text=" ADD ARROW ", command=function(){
                            TextColor <- tclvalue(TCOLOR)
                            WidgetState(Anframe1, "disabled")
                            WidgetState(Anframe2, "disabled")
                            WidgetState(Anframe3, "disabled")
                            WidgetState(BtnGroup, "disabled")
                            trellis.focus("panel", 1, 1, clip.off=TRUE, highlight=FALSE)
                            pos <- grid::grid.locator(unit = "points")
                            ArrowPosition0 <<- ConvertCoords(pos)
                            panel.points(x = ArrowPosition0$x, y = ArrowPosition0$y, cex=1.1, pch=20, col=TextColor)
                            pos <- grid::grid.locator(unit = "points") #first mark the arrow start point
                            ArrowPosition1 <<- ConvertCoords(pos)
                            WidgetState(Anframe1, "normal")
                            WidgetState(Anframe2, "normal")
                            WidgetState(Anframe3, "normal")
                            WidgetState(BtnGroup, "normal")
                            CtrlPlot()
                       })
       tkgrid(AddArwButt, row = 1, column = 1, padx = 5, pady = 5, sticky="we")

       AcceptButt <- tkbutton(BtnGroup, text=" ACCEPT ", command=function(){
                            AcceptedGraph <<- graph  #accept the new annotation
                            TextPosition <<- list(x=NULL, y=NULL)
                            AnnotateText <<- NULL
                            TextSize <<- 1
                            TextColor <<- "black"
                       })
       tkgrid(AcceptButt, row = 2, column = 1, padx = 5, pady = 5, sticky="we")

       UndoButt <- tkbutton(BtnGroup, text=" UNDO ", command=function(){
                            #Restore the last accepted plot
                            TextPosition <<- list(x=NULL, y=NULL)
                            AnnotateText <<- NULL
                            TextSize <<- 1
                            TextColor <<- "black"
                            plot(AcceptedGraph)
                            trellis.unfocus()
                       })
       tkgrid(UndoButt, row = 3, column = 1, padx = 5, pady = 5, sticky="we")

       RestoreButt <- tkbutton(BtnGroup, text=" REFRESH PLOT ", command=function(){
                            #Restore the initial original plot
                            AcceptedGraph <<- OrigGraph   #restore graph at the previous annotation step
                            TextPosition <<- list(x=NULL, y=NULL)
                            AnnotateText <<- NULL
                            TextSize <<- 1
                            TextColor <<- "black"
                            plot(AcceptedGraph)
                            trellis.unfocus()
                       })
       tkgrid(RestoreButt, row = 4, column = 1, padx = 5, pady = 5, sticky="we")

       ExitButt <- tkbutton(BtnGroup, text="  EXIT  ", command=function(){
                            trellis.unfocus()
                            tkdestroy(AnnWin)
                       })
       tkgrid(ExitButt, row = 5, column = 1, padx = 5, pady = 5, sticky="we")
    }

    return(c(Xlim, Ylim))

}

