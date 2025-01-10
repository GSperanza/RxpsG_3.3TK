#Macro to change Baseline extremes

#'@title XPSMoveBaseLine
#'@description XPSMoveBaseLine modifies the BaseLine level and 
#'  limits of a given Coreline. New edge values are provided by
#   clicking with mouse on the CoreLine plot
#'@examples
#'\dontrun{
#' 	XPSMoveBaseLine()
#'}
#'@export
#'


XPSMoveBaseLine <- function(){

   ReDraw <- function(){
#--- Here the coreline and Baseline+Fit has to be displayed separately
       SampData <- as.matrix(XPSSample[[indx]]@.Data) #create spectrum data matrix for plot
       plot(x=SampData[[1]], y=SampData[[2]], xlim=Xrange1, ylim=Yrange1, type="l", lty="solid", lwd=1, col="black")
       SampData <- setAsMatrix(XPSSample[[indx]], "matrix") #create Baseline+Fit data matrix for plot
       NC <- ncol(SampData)
       if (NC > 2) { #there is a Baseline
          BaseLine <- SampData[,3]
          matlines(x=SampData[,1], y=BaseLine, xlim=Xrange1, type="l", lty="solid", lwd=1, col="sienna")
       }
       if (NC > 3){ #there is a fit
          FitComp <- SampData[,4:NC-1]  #Only components and fit
          SpectFit <- SampData[,NC]  #fit
          matlines(x=SampData[,1], y=FitComp, xlim=Xrange1, type="l", lty="solid", lwd=1, col="blue")
          matlines(x=SampData[,1], y=SpectFit, xlim=Xrange1, type="l", lty="solid", lwd=1, col="red")
       }
       if (SetZoom == TRUE){   #set zoom area corners
           points(Corners, type="p", col="blue", pch=3, cex=1.2, lwd=2.5)
           rect(Corners$x[1], Corners$y[1], Corners$x[4], Corners$y[4])
       }
   }

   MakeBaseLine <- function(){
        deg <- NULL
        Wgt <- NULL
        BLinfo <- XPSSample[[indx]]@Baseline$type
        BasLinType <- BLinfo[1]
        BasLinType <<- tolower(BasLinType)
        if (BasLinType == "linear" || BasLinType == "shirley" || BasLinType == "2p.shirley" || BasLinType == "2p.tougaard" || BasLinType == "3p.tougaard") {
           XPSSample[[indx]] <<- XPSsetRegionToFit(XPSSample[[indx]])
           XPSSample[[indx]] <<- XPSbaseline(XPSSample[[indx]], BasLinType, deg, Wgt, splinePoints )
           XPSSample[[indx]] <<- XPSsetRSF(XPSSample[[indx]])
        } else if (BasLinType == "polynomial") {
           deg <- as.numeric(BLinfo[2])
           XPSSample[[indx]] <<- XPSbaseline(XPSSample[[indx]], BasLinType, deg, Wgt, splinePoints )
        } else if (BasLinType == "spline") {
            # Now make BaseLine
            decr <- FALSE #Kinetic energy set
            if (XPSSample[[indx]]@Flags[1] == TRUE) { decr <- TRUE }
            idx <- order(splinePoints$x, decreasing=decr)
            splinePoints$x <- splinePoints$x[idx] #splinePoints$x in ascending order
            splinePoints$y <- splinePoints$y[idx] #following same order select the correspondent splinePoints$y
            LL <- length(splinePoints$x)
            XPSSample[[indx]]@Boundaries$x <<- c(splinePoints$x[1],splinePoints$x[LL]) #set the boundaries of the baseline
            XPSSample[[indx]]@Boundaries$y <<- c(splinePoints$y[1],splinePoints$y[LL])
            XPSSample[[indx]] <<- XPSsetRegionToFit(XPSSample[[indx]])
            XPSSample[[indx]] <<- XPSbaseline(XPSSample[[indx]], BasLinType, deg, Wgt, splinePoints )
            XPSSample[[indx]] <<- XPSsetRSF(XPSSample[[indx]])
        } else if (BasLinType == "3p.shirley" || BasLinType == "lp.shirley" || BasLinType == "4p.tougaard") {
            Wgt <- as.numeric(BLinfo[2])
            XPSSample[[indx]] <<- XPSsetRegionToFit(XPSSample[[indx]])
            XPSSample[[indx]] <<- XPSbaseline(XPSSample[[indx]], BasLinType, deg, Wgt, splinePoints )
            XPSSample[[indx]] <<- XPSsetRSF(XPSSample[[indx]])
        }
        LL <- length(XPSSample[[indx]]@Components)
        if (LL > 0) {
           for(ii in 1:LL){
              XPSSample[[indx]]@Components[[ii]] <<- Ycomponent(XPSSample[[indx]]@Components[[ii]], x=XPSSample[[indx]]@RegionToFit$x, y=XPSSample[[indx]]@Baseline$y) #calcola la Y eed aggiunge la baseline
           }
# update fit$y with sum of components
           tmp <- sapply(XPSSample[[indx]]@Components, function(z) matrix(data=z@ycoor))
           XPSSample[[indx]]@Fit$y <<- ( colSums(t(tmp)) - length(XPSSample[[indx]]@Components)*(XPSSample[[indx]]@Baseline$y))
        }
        EndPts <<- FALSE
   }


#--- Variables ---
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }

   XPSSample <- get(activeFName, envir=.GlobalEnv)
   XPSSampleBkp <- XPSSample
   XPSSampleList <- XPSFNameList()
   SpectList <- XPSSpectList(activeFName)
   BasLinType <- NULL
   indx <- NULL
   Xrange0 <- NULL
   Yrange0 <- NULL
   Xrange1 <- NULL
   Yrange1 <- NULL
   Corners <- list(x=NULL, y=NULL)
   splinePoints <- list(x=NULL, y=NULL)
   ExitWhile <- NULL
   SetZoom <- FALSE
   EndPts <- FALSE
   
   SourceFile <- list()
   SourceCoreline <- list()

   plot.new() #reset the graphic window

#--- GUI ---
   MBLwin <- tktoplevel()
   tkwm.title(MBLwin,"MOVE BASELINE")
   tkwm.geometry(MBLwin, "+100+50")   #position respect topleft screen corner
   MBLgroup <- ttkframe(MBLwin, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(MBLgroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   MBLFrame1 <- ttklabelframe(MBLgroup, text = "SELECT THE XPS-SAMPLE", borderwidth=2)
   tkgrid(MBLFrame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   XS <- tclVar(activeFName)
   SourceFile <- ttkcombobox(MBLFrame1, width = 25, textvariable = XS, values = XPSSampleList)
   tkbind(SourceFile, "<<ComboboxSelected>>", function(){
                    activeFName <<- tclvalue(XS)
                    XPSSample <<- get(activeFName,envir=.GlobalEnv)
                    SpectList <<- XPSSpectList(activeFName)
                    tkconfigure(SourceCoreline, values=SpectList)
                    plot(XPSSample)
            })
   tkgrid(SourceFile, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   MBLFrame2 <- ttklabelframe(MBLgroup, text = "SELECT THE CORELINE", borderwidth=2)
   tkgrid(MBLFrame2, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
   CL <- tclVar()
   SourceCoreline <- ttkcombobox(MBLFrame2, width = 25, textvariable = CL, values = SpectList)
   tkbind(SourceCoreline, "<<ComboboxSelected>>", function(){
                    SourceXS <- tclvalue(XS)
                    XPSSample <<- get(SourceXS, envir=.GlobalEnv)
                    SourceCL <- tclvalue(CL)
                    SourceCL <- unlist(strsplit(SourceCL, "\\."))   #extract the spectrum idx
                    indx <<- as.numeric(SourceCL[1])
                    Xrange0 <<- range(XPSSample[[indx]]@.Data[[1]])
                    Yrange0 <<- range(XPSSample[[indx]]@.Data[[2]])
                    BLtype <- XPSSample[[indx]]@Baseline$type
                    XPSSampleBkp <<- XPSSample #save for reset plot
                    Xrange1 <<- range(XPSSample[[indx]]@.Data[[1]])
                    if (XPSSample[[indx]]@Flags[1]) {   #reverse if BE scale
                        Xrange1 <<- rev(Xrange1)
                    }
                    Yrange1 <<- range(XPSSample[[indx]]@.Data[[2]])
                    WidgetState(MBLbutton, "normal")
                    WidgetState(ADBLbutton, "normal")
                    WidgetState(SZAbutton, "normal")
                    WidgetState(RSTZMbutton, "normal")
                    ReDraw()
            })
   tkgrid(SourceCoreline, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   MBLFrame3 <- ttklabelframe(MBLgroup, text = "REFINE BASELINE EDGES", borderwidth=2)
   tkgrid(MBLFrame3, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

   MBLbutton <- tkbutton(MBLFrame3, text="Set Baseline End Points", width=45, command=function(){
                    BasLinType <<- XPSSample[[indx]]@Baseline$type[1]
                    if (BasLinType == "spline") {
                       txt <- "Spline background: \n ==> LEFT click to set spline points; RIGHT click to exit"
                       tkmessageBox(message=txt, title="HELP INFO", icon="info")
                       splinePoints <<- list(x=NULL, y=NULL)
                       pos <- c(1,1) # only to enter in  the loop
                       while (length(pos) > 0) {  #pos != NULL => mouse right button not pressed
                              pos <- locator(n=1, type="p", col=3, cex=1.5, lwd=2, pch=1)
                              if (length(pos) > 0) {
                                  splinePoints$x <<- c(splinePoints$x, pos$x)  # $x and $y must be separate to add new coord to splinePoints
                                  splinePoints$y <<- c(splinePoints$y, pos$y)
                              }
                       }
                    } else {
                       txt <- paste(BasLinType, " background found!\n  ==> Set the Baseline Limits")
                       tkmessageBox(message=txt, title="HELP INFO", icon="info")
                       pos <- locator(n=2, type="p", pch=1, cex=1.5, col="red", lwd=2)
                       decr <- FALSE #Kinetic energy set
                       if (XPSSample[[indx]]@Flags[1] == TRUE) { decr <- TRUE } #Binding Energy set
                       idx <- order(pos$x, decreasing = decr)
                       pos$x <- pos$x[idx] #put Pos$X, Pos$y elements in the correct order
                       pos$y <- pos$y[idx]
                       if (XPSSample[[indx]]@Flags[1]) { #Binding energy set
                           idx <- order(pos$x, decreasing = TRUE)
                           Xrange1 <<- pos$x <- pos$x[idx] #put Pos$X, Pos$y elements in the correct order
                           pos$y <- pos$y[idx]
                           idx1 <- findXIndex(XPSSample[[indx]]@.Data[[1]], Xrange1[2])  #we must have idx1 < idx2
                           idx2 <- findXIndex(XPSSample[[indx]]@.Data[[1]], Xrange1[1])
                       } else {                      #Kinetic energy set
                           idx <- order(pos$x, decreasing = FALSE)
                           Xrange1 <<- pos$x <- pos$x[idx] #put Pos$X, Pos$y elements in the correct order
                           pos$y <- pos$y[idx]
                           idx1 <- findXIndex(XPSSample[[indx]]@.Data[[1]], Xrange1[1])
                           idx2 <- findXIndex(XPSSample[[indx]]@.Data[[1]], Xrange1[2])
                       }
                       XPSSample[[indx]]@RegionToFit$x <<- XPSSample[[indx]]@.Data[[1]][idx1:idx2] # idx1 < idx2
                       XPSSample[[indx]]@RegionToFit$y <<- XPSSample[[indx]]@.Data[[2]][idx1:idx2]
                       Yrange1 <<- range(XPSSample[[indx]]@RegionToFit$y)
                       XPSSample[[indx]]@Boundaries <<- pos
                    }
                    EndPts <<- TRUE
            })
   tkgrid(MBLbutton, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   ADBLbutton <- tkbutton(MBLFrame3, text=" Refine Baseline  ", width=45, command=function(){
                    if (EndPts == FALSE) {
                        tkmessageBox(message="Please set the end-points before adjusting the BaseLine", title="WARNING", icon="warning")
                        return()
                    }
                    MakeBaseLine()
                    #changing the Baseline requires the fit components and the fit have to be recomputed
                    LL <- length(XPSSample[[indx]]@Components)
                    tmp <- NULL
                    if (LL > 0) {
                       for(ii in 1:LL) {
                          XPSSample[[indx]]@Components[[ii]] <<- Ycomponent(XPSSample[[indx]]@Components[[ii]], x=XPSSample[[indx]]@RegionToFit$x, y=XPSSample[[indx]]@Baseline$y) #computes the Y and add the Baseline
                       }
                       tmp <- sapply(XPSSample[[indx]]@Components, function(z) matrix(data=z@ycoor))
                       XPSSample[[indx]]@Fit$y <<- (colSums(t(tmp)) - length(XPSSample[[indx]]@Components)*(XPSSample[[indx]]@Baseline$y))
                    }
                    plot(XPSSample[[indx]], xlim=Xrange1, ylim=Yrange1)  #if zoom is present keeps new X, Y limits
            })
   tkgrid(ADBLbutton, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

   SZAbutton <- tkbutton(MBLFrame3, text=" Set the Zoom Area ", width=20, command=function(){
                    row1 <- " => Left click to define the 2 ZOOM REGION CORNERS (opposite in diagonal)"
                    row2 <- "\n => Click near corners to adjust Zoom Region Dimensions"
                    row3 <- "\n => Right click to Make Zoom when Zoom Region OK"
                    msg <- paste(row1, row2, row3, sep="")
                    tkmessageBox(message=msg, icon="warning")
                    SetZoom <<- TRUE
                    pos <- locator(n=2, type="p", pch=3, cex=1, col=4, lwd=2)
                    Corners$x <<- c(pos$x[1],pos$x[1],pos$x[2],pos$x[2])
                    Corners$y <<- c(pos$y[1],pos$y[2],pos$y[1],pos$y[2])
                    decr <- FALSE #Kinetic energy set
                    if (XPSSample[[indx]]@Flags[1] == TRUE) { decr <- TRUE }
                    Xrange1 <<- sort(c(pos$x[1], pos$x[2]), decreasing=decr)
                    Yrange1 <<- sort(c(pos$y[1], pos$y[2]), decreasing=FALSE)
                    ReDraw()  #plots the zoom region
                    pos <- list(x=NULL, y=NULL)
                    ExitWhile <- 1
                    while(ExitWhile > 0 ){
                          pos <- locator(n=1)
                          if (is.null(pos) ){
                              ExitWhile <- -1
                              SetZoom <<- FALSE
                              if (XPSSample[[indx]]@Flags[1]) { #Binding energy set
                                  Corners$x <<- sort(Corners$x, decreasing=TRUE)
                              } else {                      #Kinetic energy set
                                  Corners$x <<- sort(Corners$x, decreasing=FALSE)
                              }
                              break()  #stops the while loop and exit
                          }
                          if (pos$x < Xrange0[1]) {pos$x <- Xrange0[1]}
                          if (pos$x > Xrange0[2]) {pos$x <- Xrange0[2]}
                          if (pos$y < Yrange0[1]) {pos$y <- Yrange0[1]}
                          if (pos$y > Yrange0[2]) {pos$y <- Yrange0[2]}

                          Dist <- NULL
                          Dmin <- ((pos$x-Corners$x[1])^2 + (pos$y-Corners$y[1])^2)^0.5  #valore di inizializzazione
                          for (ii in 1:4) {
                               Dist <- ((pos$x-Corners$x[ii])^2 + (pos$y-Corners$y[ii])^2)^0.5  #dist P0 P1
                               if(Dist <= Dmin){
                                  Dmin <- Dist
                                  idx <- ii
                               }
                          }
                          if (idx == 1){
                              Corners$x[1] <<- Corners$x[2] <<- pos$x
                              Corners$y[1] <<- Corners$y[3] <<- pos$y
                          } else if (idx == 2){
                              Corners$x[1] <<- Corners$x[2] <<- pos$x
                              Corners$y[2] <<- Corners$y[4] <<- pos$y
                          } else if (idx == 3){
                              Corners$x[3] <<- Corners$x[4] <<- pos$x
                              Corners$y[1] <<- Corners$y[3] <<- pos$y
                          } else if (idx == 4){
                              Corners$x[3] <<- Corners$x[4] <<- pos$x
                              Corners$y[2] <<- Corners$y[4] <<- pos$y
                          }
                          Xrange1 <<- c(Corners$x[1], Corners$x[3])  #modify only the Y range to give possibility to re-define the Baseline edges
                          Yrange1 <<- sort(c(Corners$y[1], Corners$y[2]), decreasing=FALSE)  #modify only the Y range to give possibility to re-define the Baseline edges
                          ReDraw()
                    } ### while loop end
                    plot(XPSSample[[indx]], xlim=Xrange1, ylim=Yrange1)
            })
   tkgrid(SZAbutton, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

   RSTZMbutton <- tkbutton(MBLFrame3, text=" Reset Zoom ", width=20, command=function(){
                    Xrange1 <<- range(XPSSample[[indx]]@RegionToFit$x) #if Baseline present limit the
                    Yrange1 <<- range(XPSSample[[indx]]@RegionToFit$y) #plot limits to the RegionToFit
                    XPSSample[[indx]]@Boundaries$x <<- Xrange1
                    XPSSample[[indx]]@Boundaries$y <<- Yrange1
                    LL <- length(XPSSample[[indx]]@RegionToFit$x)
                    if (XPSSample[[indx]]@Flags[1]) { #Binding energy set
                        Xrange1 <<- sort(Xrange1, decreasing=TRUE)  #pos$x in decreasing order
                    } else {
                        Xrange1 <<- sort(Xrange1, decreasing=FALSE) #pos$x in increasing order
                    }
                    Yrange1 <<- sort(Yrange1, decreasing=FALSE) #pos$ in increasing order
                    SetZoom <<- FALSE  #definition of zoom area disabled
                    ReDraw()
            })
   tkgrid(RSTZMbutton, row = 3, column = 1, padx = c(180,0), pady = 5, sticky="w")

   MBLFrame4 <- ttklabelframe(MBLgroup, text = "SAVE AND EXIT", borderwidth=2)
   tkgrid(MBLFrame4, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

   RSTbutton <- tkbutton(MBLFrame4, text=" RESET ", width=45, command=function(){
                    Yrange1 <<- range(XPSSample[[indx]]@.Data[[2]])
                    SetZoom <<- FALSE
                    LL <- length(XPSSample[[indx]]@Components)
                    XPSSample[[indx]] <<- XPSSampleBkp[[indx]]
                    Xrange0 <<- range(XPSSample[[indx]]@.Data[[1]])
                    Yrange0 <<- range(XPSSample[[indx]]@.Data[[2]])
                    Xrange1 <<- range(XPSSample[[indx]]@.Data[[1]])
                    if (XPSSample[[indx]]@Flags[1]) {   #reverse if BE scale
                        Xrange1 <<- rev(Xrange1)
                    }
                    Yrange1 <<- range(XPSSample[[indx]]@.Data[[2]])
                    ReDraw()
            })
   tkgrid(RSTbutton, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   SAVEbutton <- tkbutton(MBLFrame4, text=" SAVE ", width=45, command=function(){
    	               assign(activeFName, XPSSample, envir=.GlobalEnv)
                    XPSSaveRetrieveBkp("save")
            })
   tkgrid(SAVEbutton, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

   SAVEXITbutton <- tkbutton(MBLFrame4, text=" SAVE & EXIT ", width=45, command=function(){
                    tkdestroy(MBLwin)
                    assign("activeFName", activeFName, envir=.GlobalEnv)
     	              assign(activeFName, XPSSample, envir=.GlobalEnv)
                    assign("activeSpectIndx", indx, envir=.GlobalEnv)
                    XPSSaveRetrieveBkp("save")
                    UpdateXS_Tbl()
            })
   tkgrid(SAVEXITbutton, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

   WidgetState(MBLbutton, "disabled")
   WidgetState(ADBLbutton, "disabled")
   WidgetState(SZAbutton, "disabled")
   WidgetState(RSTZMbutton, "disabled")
}
