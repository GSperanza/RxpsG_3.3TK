## XPScomparePlot: macro to compare corelines in multi-panel mode

#' @title XPScompare allows compare Core-Line and their best fits
#' @description XPScompEngine is the software powering the XPSCompareGUI.
#'   XPScompEngine() is called by XPScompareGUI() and not directly accesible.
#'   This macro compares corelines in multi-panel mode following
#'   options selected by the user in XPScompareGUI
#' @param PlotParameters the plot parameters asociated to the XPSOverlayGUI options;
#' @param Plot_Args list of plot options;
#' @param SelectedNames list containing the XPSSample names and the Corelines to be plotted;
#' @param Xlim Xrange of the data to be plotted;
#' @param Ylim Yrange of the data to be plotted;
#' @return Returns c(Xlim, Ylim) eventually modified
#' @export
#'

XPScompEngine <-  function(PlotParameters, Plot_Args, SelectedNames, Xlim, Ylim) {

#---  SetPltArgs sets the Plot_Arg list following selections in OverlayGUI
   SetPltArgs <- function(LType,SType){ 
            Ylength <- lapply(Y, sapply, length)
            N.XS <- length(SelectedNames$XPSSample)
            N.CL <- length(SelectedNames$CoreLines)
            idx <- 1
            cx <<- list()
            levx <<- list()

            #now extract data: for ii, for jj in agreement with the compareXPSSample structure
            #see the reading data section
            #if C1s, O1s of XPSSamples X1, X2, X3 are compared then compareXPSSample is organized as follow:
            #
            #   C1s(X1) C1s(X2) C1s(X3) O1s(X1) O1s(X2) O1s(X3)
            #
            #Then the first for ii runs on the corelines, the second for jj runs on the XPSSamples

            Xrng <- list()
            Yrng <- list()
            for (ii in 1:N.CL) {        #ii runs on the CoreLines of the XPSSamples
                tmp1 <- NULL
                tmp2 <- NULL
                Xrng[[ii]] <- list()
                Yrng[[ii]] <- list()
                Cex <- Plot_Args$cex
                for (jj in 1:N.XS) {    #jj runs on the fit components Corelines
                   if (attr(Ylength[[idx]][1], "names") == "MAIN"){   #holds when just the spectrum is plotted
                       Plot_Args$col[idx] <<- PlotParameters$Colors[jj] #palette[jj]
                       Plot_Args$lty[idx] <<- LType[jj]
                       Plot_Args$pch[idx] <<- SType[jj]
                       Plot_Args$cex[idx] <<- Plot_Args$cex
                       Plot_Args$auto.key$col[idx] <<- PlotParameters$Colors[jj] # palette[jj]
                   }
                   tmp1 <- c( tmp1, rep(ii, times=as.integer(Ylength[[idx]][1])) )  #tmp1 contains indexes associated to the Coreline (group of spectra)
                   tmp2 <- c( tmp2, rep(idx, times=as.integer(Ylength[[idx]][1])) ) #tmp0 contains indexes associated to the XPSSamples... (distinguish different spectra inside the group)
                   Xrng[[ii]][[jj]] <- range(X[[idx]]) #range computed on CoreLine1 (CoreLine2...) of different XPSSamples
                   Yrng[[ii]][[jj]] <- range(Y[[idx]]) #range computed on CoreLine1 (CoreLine2...) of different XPSSamples
                   levx[[ii]] <<- tmp1 #required to distinguish multiple panels
                   cx[[ii]] <<- tmp2   #required to distinguish different curves
                   idx <- idx+1
                }
            }
            #All Xrng and Yrng MUST be computed before calculate the cumulative range of each group of CoreLines
            for (ii in 1:N.CL) {        #ii runs on the CoreLines of the XPSSamples
                Xlimits[[ii]] <<- range(Xrng[[ii]])
                Ylimits[[ii]] <<- range(Yrng[[ii]])
                w <- Ylimits[[ii]][2]-Ylimits[[ii]][1]
                Ylimits[[ii]][1] <<- Ylimits[[ii]][1] - w/15
                Ylimits[[ii]][2] <<- Ylimits[[ii]][2] + w/15
                if (length(PlotParameters$CustomXY) > 0 && ii==PlotParameters$CustomXY[1]){ #PlotParameters$CustomXY[1] indicated the CLine with custom XY scale
                    Xlimits[[ii]] <<- c(PlotParameters$CustomXY[2], PlotParameters$CustomXY[3]) #customX range
                    Ylimits[[ii]] <<- c(PlotParameters$CustomXY[4], PlotParameters$CustomXY[5]) #custom Yrange
                } 
  	         }
   }

#---  rescale a vector so that it lies between the specified minimum and maximum
   rescale <- function(x, newrange=c(0,1)) {
	    if (!is.numeric(x) || !is.numeric(newrange)){
	        stop("Please supply numerics for the x and the new scale")
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


#----- Reading data  -----

#--- compareXPSSample will contain the selected XPSSamples and the selected CoreLines to compare
    compareXPSSample <- new("XPSSample")
    N.XS <- length(SelectedNames$XPSSample)
    N.CL <- length(SelectedNames$CoreLines)
    SpectLengths <- NULL
    idx <- 1
    select <- list()
    if (PlotParameters$OverlayType == "Compare.CoreLines") { #Compare CoreLines in multi.spectrum mode
       for(jj in 1:N.CL){
          SpectName <- SelectedNames$CoreLines[jj] #load all the selected corelines always when SelectedNames$XPSSample == '-----'
          for(ii in 1:N.XS){
             FName <- SelectedNames$XPSSample[ii]
             FName <- get(FName, envir=.GlobalEnv)
             FName[[SpectName]]@.Data[[2]] <- FName[[SpectName]]@.Data[[2]]*SelectedNames$Ampli[ii] #if an amplification factor != 1 was selected
             compareXPSSample[[idx]] <- new("XPSCoreLine")
             compareXPSSample[[idx]]@.Data[1] <- FName[[SpectName]]@.Data[1]  #SpectName not SpectIdx: is possible that Corelines are acquired in different order in different XPSSamples
             compareXPSSample[[idx]]@.Data[2] <- FName[[SpectName]]@.Data[2]  #SpectName not SpectIdx: is possible that Corelines are acquired in different order in different XPSSamples
             compareXPSSample[[idx]]@Flags <- FName[[SpectName]]@Flags
             names(compareXPSSample)[idx] <- SpectName
             select[[idx]] <- "MAIN"
             idx <- idx+1
          }
       }
    } else {
       return()
    }
    NSpect <- idx-1

#----- CompareEngine FUNCTIONS -----

# set Titles and axis labels
    SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv)     #active spectrum index
    SpectName <- get("activeSpectName", envir=.GlobalEnv)     #active spectrum namne
    if (length(Plot_Args$xlab$label) == 0) Plot_Args$xlab$label <- compareXPSSample[[1]]@units[1]
    if (length(Plot_Args$ylab$label) == 0) Plot_Args$ylab$label <- compareXPSSample[[1]]@units[2]

    #--- Now transform XPSSample into a list
    #--- The asList function allows including/skipping fit components, baseline etc. following the select options
    #--- NOTE: USE of sapply instead of lapply!!!

    XPSSampLen <- length(compareXPSSample)
    XPSSampNames <- names(compareXPSSample)
    X <- NULL
    Y <- NULL

    for (ii in 1:NSpect){
        tmp <- as.matrix(asList(compareXPSSample[[ii]], select = select[[ii]]))
        X <- c(X, tmp["x", ])   #X coords of the selected spectra
        Y <- c(Y, tmp["y", ])   #Y coords of the selected spectra
    }
    Xlim0 <- range(sapply(X, sapply, range))
    Ylim0 <- range(sapply(Y, sapply, range))

#--- Now all the data manipulation options

#--- X offset
    if (PlotParameters$XOffset$Shift != 0) {
	       idx <- PlotParameters$XOffset$CL
        for(ii in 1:N.CL){
            jj <- ii+(N.XS*(idx-1))
	           X[[jj]][[1]] <- X[[jj]][[1]]+ii*PlotParameters$XOffset$Shift
			     }
    }
#--- set xlim, and reverseX if BE
    if (is.null(Plot_Args$xlim)) {  #non ho fissato xlim per fare lo zoom
	      Plot_Args$xlim <- range(sapply(X, sapply, range))
	      wdth <- Plot_Args$xlim[2]-Plot_Args$xlim[1]
	      Plot_Args$xlim[1] <- Plot_Args$xlim[1]-wdth/15
	      Plot_Args$xlim[2] <- Plot_Args$xlim[2]+wdth/15
              Xlim <- Plot_Args$xlim    #Xlim iniziali senza alcuna operazione: vanno mantenute
    }
    if (PlotParameters$Reverse) Plot_Args$xlim <- sort(Plot_Args$xlim, decreasing=TRUE)

#--- Switch BE/KE scale
    if (PlotParameters$SwitchE == TRUE) {
       XEnergy <- get("XPSSettings", envir=.GlobalEnv)$General[5] #this is the X-Ray photo energy
       XEnergy <- as.numeric(XEnergy)
       Plot_Args$xlim <- XEnergy-Plot_Args$xlim  #Transform X BE limits in X KE limits
       for(idx in 1:XPSSampLen) {
           if (compareXPSSample[[idx]]@Flags[1] == TRUE){ #The original X scale is BE
              Plot_Args$xlab$label <- "Kinetic Energy [eV]"

     	        X[[idx]][[1]] <- XEnergy - X[[idx]][[1]] #Binding to Kinetic energy abscissa
     	        compareXPSSample[[idx]]@Flags[1] <- FALSE
  	        }
       }
    } else if(PlotParameters$SwitchE == FALSE) {
       XEnergy <- get("XPSSettings", envir=.GlobalEnv)$General[5] #this is the X-Ray photo energy
       XEnergy <- as.numeric(XEnergy)
       for(idx in 1:XPSSampLen) {
           if (compareXPSSample[[idx]]@Flags[1] == FALSE){ #The original X scale is KE
              Plot_Args$xlab$label <- "Binding Energy [eV]"
              X[[idx]][[1]] <- XEnergy - X[[idx]][[1]]  #Binding to Kinetic energy abscissa
              compareXPSSample[[idx]]@Flags[1] <- TRUE
           }
       }
    }

#--- Here Y alignment
    if (PlotParameters$Align) {
       LL <- length(Y)
       if ( all(sapply(Y, function(x) !is.na(charmatch("BASE", names(x))))) ) {
		       	minybase <- sapply(Y, function(x) min(x$BASE))
		       	for (idx in c(1:LL)) {
		          		Y[[idx]] <- lapply(Y[[idx]], "-", minybase[idx])
			       }
       } else {
          for (idx in c(1:LL)) {
              Y[[idx]] <- lapply(Y[[idx]], function(j) {
	                         return( rescale(j, newrange = c(0, diff(range(j))) ) )
	                      })
          }
       }
    }

#--- Y normalization == scale c(0,1)
    if (!is.null(PlotParameters$Normalize)) {
		    	maxY <- sapply(Y, function(x) max(sapply(x, max))) #here Y is the list of XPSSamples
		    	for(idx in PlotParameters$Normalize) {
           for(ii in 1:N.XS){
               jj <- ii+(N.XS*(idx-1))
		       		    Y[[jj]][[1]] <- Y[[jj]][[1]]/max(Y[[jj]][[1]])
     		    }
			    }
    }

#--- Y offset
    if (PlotParameters$YOffset$Shift != 0) {
	       idx <- PlotParameters$YOffset$CL
        for(ii in 1:N.CL){
            jj <- ii+(N.XS*(idx-1))
	           Y[[jj]][[1]] <- Y[[jj]][[1]]+ii*PlotParameters$YOffset$Shift
			     }
    }

#--- Y ScaleFact
    if (PlotParameters$ScaleFact$ScFact != 0) {
	       XSidx <- PlotParameters$ScaleFact$XS
	       CLidx <- PlotParameters$ScaleFact$CL
	       ScFact <- PlotParameters$ScaleFact$ScFact
        for(ii in 1:N.CL){
            jj <- XSidx +(CLidx-1)*N.XS
	           Y[[jj]][[1]] <- Y[[jj]][[1]]*ScFact
			     }
    }


#------- APPLY GRAPHIC OPTION TO PLOTTING XYplot() ARGS -----------------
    Ylength <- lapply(Y, sapply, length)
    cx <- list()
    levx <- list()
    panel <- sapply(Ylength, sum)
    PanelTitles <- NULL
    Xlimits <- list() # make a list of X limits in the case Xaxis revers=TRUE
    Ylimits <- list() # make a list of Y limits normalization present?

    if (Plot_Args$type=="l") { #lines are selected for plot
          Plot_Args$auto.key$lines <- TRUE
          Plot_Args$auto.key$points <- FALSE
          if (length(PlotParameters$Colors)==1) {   # B/W LINES
              LType <- Plot_Args$lty                # "solid", "dashed", "dotted" ....
              SType <- rep(NA, 20)
              PlotParameters$Colors <-  rep("black", 20)          # "Black","black","black",....
              SetPltArgs(LType, SType)
          } else if (length(PlotParameters$Colors) > 1) {   # RainBow LINES
              LType <- Plot_Args$lty                # selected patterns
              SType <- rep(NA, 20)                  # NO symbols
              SetPltArgs(LType, SType)
          }
    } else if (Plot_Args$type=="p") { #symbols are selected for plot
          Plot_Args$auto.key$lines <- FALSE
          Plot_Args$auto.key$points <- TRUE
          if (length(PlotParameters$Colors)==1) {   # B/W  SYMBOLS
              LType <- rep(NA, 20)
              SType <- Plot_Args$pch                # VoidCircle", "VoidSquare", "VoidTriangleUp" ....
              PlotParameters$Colors <-  rep("black", 20)          # "Black","black","black",....
              SetPltArgs(LType, SType)
          } else if (length(PlotParameters$Colors) > 1) {   # RainBow SYMBOLS
              LType <- rep(NA, 20)
              SType <- Plot_Args$pch                # slected symbol style
              SetPltArgs(LType, SType)
          }
    } else if (Plot_Args$type=="b") { #Lines + symbols are selected for plot
          Plot_Args$auto.key$lines <- TRUE
          Plot_Args$auto.key$points <- TRUE
          if (length(PlotParameters$Colors)==1) {   # B/W LINES & SYMBOLS
              LType <- Plot_Args$lty                # "solid", "dashed", "dotted" ....
              SType <- Plot_Args$pch                # "VoidCircle", "VoidSquare", "VoidTriangleUp" ....
              PlotParameters$Colors <-  rep("black", 20) # "Black","black","black",....
              SetPltArgs(LType, SType)
          } else if (length(PlotParameters$Colors) > 1) {   # RainBow LINES & SYMBOLS
              LType <- Plot_Args$lty                # selected patterns
              SType <- Plot_Args$pch                # slected symbol style
              SetPltArgs(LType, SType)
          }
    }

##--- MULTI PANEL---

    if (PlotParameters$OverlayMode=="Multi-Panel") {
       #define row and columns of the panel matrix
       Nspect <- length(SelectedNames$CoreLines)
       Ncol <- 1
       Nrow <- 1
       rr <- FALSE
       while(Nspect > Ncol*Nrow) {
          if (rr){
             Nrow <- Nrow+1
             rr <- FALSE
          } else {
             Ncol <- Ncol+1
             rr <- TRUE
          }
       }

       if (PlotParameters$Reverse) {
          Xlimits <- lapply(Xlimits, sort, decreasing=TRUE) #reverse energy scale
       }

       #X-scale in 'scientific' format
       if (Plot_Args$scales$x$log == "Xpow10" || Plot_Args$scales$x$log == "Xe+0n") {
           x_at <- as.list(array(dim=N.CL))
           x_labels <- as.list(array(dim=N.CL))
           xscl <- NULL
           for(ii in 1:N.CL){
               Xlim <- range(X[(1+(ii-1)*N.XS):(ii*N.XS)]) #cumulative range of the N.XS corelines  to compare
               xscl <- xscale.components.default(
                                   lim=Xlim, packet.number = 0,
                                   packet.list = NULL, right = TRUE
                                   )
               x_at[[ii]] <- xscl$bottom$labels$at
               eT <- floor(log10(abs(x_at[[ii]])))# at == 0 case is dealt with below
               mT <- x_at[[ii]] / 10 ^ eT
               if (Plot_Args$scales$x$log == "Xpow10"){
                   ss <- lapply(seq(along = x_at[[ii]]),
                            function(jj) {
                                if (x_at[[ii]][jj] == 0)
                                    quote(0)
                                else
                                    substitute(A %*% 10 ^ E, list(A = mT[jj], E = eT[jj]))
                            })
                   xscl$bottom$labels$labels <- do.call("expression", ss)
                   x_labels[[ii]] <- xscl$bottom$labels$labels
               } else if (Plot_Args$scales$x$log == "Xe+0n"){
                   x_labels[[ii]] <- formatC(unlist(x_at[[ii]]), digits = 1, format = "e")
               }
           }
           Plot_Args$scales$x <- list(at = x_at, labels = x_labels)
       }

       #Y-scale in 'scientific' format
       if (Plot_Args$scales$y$log == "Ypow10" || Plot_Args$scales$y$log == "Ye+0n") {
           y_at <- as.list(array(dim=N.CL))
           y_labels <- as.list(array(dim=N.CL))
           yscl <- NULL
           for(ii in 1:N.CL){
               Ylim <- range(Y[(1+(ii-1)*N.XS):(ii*N.XS)]) #cumulative range of the N.XS corelines  to compare
               yscl <- yscale.components.default(
                                   lim=Ylim, packet.number = 0,
                                   packet.list = NULL, right = TRUE
                                   )
               y_at[[ii]] <- yscl$left$labels$at
               eT <- floor(log10(abs(y_at[[ii]])))# at == 0 case is dealt with below
               mT <- y_at[[ii]] / 10 ^ eT
               if (Plot_Args$scales$y$log == "Ypow10"){
                   ss <- lapply(seq(along = y_at[[ii]]),
                            function(jj) {
                                if (y_at[[ii]][jj] == 0){
                                    quote(0)
                                } else {
                                    substitute(A %*% 10 ^ E, list(A = mT[jj], E = eT[jj]))
                                }
                            })
                   yscl$left$labels$labels <- do.call("expression", ss)
                   y_labels[[ii]] <- yscl$left$labels$labels
               } else if (Plot_Args$scales$y$log == "Ye+0n"){
                   y_labels[[ii]] <- formatC(unlist(y_at[[ii]]), digits = 1, format = "e")
               }
           }
           Plot_Args$scales$y <- list(at = y_at, labels = y_labels)
       }

       Plot_Args$xlim <- Xlimits
       Plot_Args$ylim <- Ylimits
       cx <- unlist(cx)
       levx <- unlist(levx)
       df <- data.frame(x = unname(unlist(X)), y = unname(unlist(Y)))

#in df spectra are organized following their category (spect, base, comp, fit)
       PanelTitles <- Plot_Args$PanelTitles  #recover Panel Titles from Plot_Args$PanelTitles. Plot_Args$PanelTitles is a personal argument ignored by xyplot
#      if (length(Plot_Args$main$label) > 0) { PanelTitles <- Plot_Args$main$label } #instead of the default MainLabel uses the title set by the user in OverlayGUI

#in formula y~x is plotted against levx: produces panels showing single XPSSamples
       Plot_Args$x <- formula("y ~ x| factor(levx, labels=PanelTitles)")
       Plot_Args$data <- df
       Plot_Args$par.settings$strip <- TRUE

       Plot_Args$groups <- cx
       Plot_Args$layout <- c(Nrow, Ncol)
       Plot_Args$main	<- NULL

       if (Plot_Args$auto.key[[1]][1] == FALSE) {
          Plot_Args$auto.key <- list()
          Plot_Args$auto.key <- FALSE
       }
	      graph <- do.call(xyplot, args = Plot_Args)
       plot(graph)
    }
    return(c(Xlimits, Ylimits))
}

