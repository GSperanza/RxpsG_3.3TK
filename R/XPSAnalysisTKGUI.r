
## =======================================================================================
## XPSAnalysis function to add baseline and fit components for best fit and quantification
## =======================================================================================

#' @title XPSAnalysis to analyze XPS-Corelines and XPS spectra in general
#' @description XPSAnalysis() function is an interactive GUI to add BaseLines
#'   and Fitting components to a CoreLine needed for the best fit and quantification
#' @return Returns the processed \code{object}.
#' @examples
#' \dontrun{
#'    XPSAnalysis()
#' }
#' @export
#'


XPSAnalysis <- function() {

  WidgetState <- function(widget, value) {
       childID <- tclvalue(tkwinfo("children", widget))
       selected <- sapply(unlist(strsplit(childID, " ")), function(x) {
                             Optn <- tkconfigure(x)  #get child options
                             if (length(grep("state", Optn)) > 0){ #controls if 'state' option is present
                                 tkconfigure(x, state=value)  #set child 'disabled' or 'normal'
                             }
                          })
       Done <- tcl("update", "idletasks")   #conclude pending event before exiting function
  }

  GetCurPos <- function(SingClick){
       tabMain <- as.numeric(tcl(NB, "index", "current"))+1 #Notebook pages start from 0
       coords <<- NULL
       WidgetState(MainFrame1, "disabled")
       WidgetState(T1Frame1, "disabled")
       WidgetState(T1Frame2, "disabled")
       WidgetState(T1Frame3, "disabled")
       WidgetState(T2Frame1, "disabled")
       WidgetState(T2Frame2, "disabled")
       WidgetState(T2Frame3, "disabled")
       WidgetState(T2Frame4, "disabled")
       WidgetState(T2Frame5, "disabled")
       WidgetState(ButtFrame, "disabled")

       EXIT <<- FALSE
       while(EXIT == FALSE){
            pos <- locator(n=1)
            if (is.null(pos)) {
                WidgetState(MainFrame1, "normal")
                WidgetState(T1Frame1, "normal")
                WidgetState(T1Frame2, "normal")
                WidgetState(T1Frame3, "normal")
                WidgetState(T2Frame1, "normal")
                WidgetState(T2Frame2, "normal")
                WidgetState(T2Frame3, "normal")
                WidgetState(T2Frame4, "normal")
                WidgetState(T2Frame5, "normal")
                WidgetState(ButtFrame, "normal")
                EXIT <<- TRUE
            } else {
                if (SingClick == TRUE ){
                    WidgetState(MainFrame1, "normal")
                    WidgetState(T1Frame1, "normal")
                    WidgetState(T1Frame2, "normal")
                    WidgetState(T1Frame3, "normal")
                    WidgetState(T2Frame1, "normal")
                    WidgetState(T2Frame2, "normal")
                    WidgetState(T2Frame3, "normal")
                    WidgetState(T2Frame4, "normal")
                    WidgetState(T2Frame5, "normal")
                    WidgetState(ButtFrame, "normal")
                    EXIT <<- TRUE
                }
                if (tabMain == 1 && SetZoom == TRUE) {
                    coords <<- unlist(pos)
                    SetCurPos2()  #selection of the zoom area
                }
                if (tabMain == 1 && SetZoom == FALSE && BType != "Spline") {
                    coords <<- unlist(pos)
                    SetCurPos1()  #selection of the BaseLine Edges
                }
                if (tabMain == 1 && BType == "Spline") {
                    coords <<- unlist(pos)
                    SetCurPos2()  #plot spline points
                }
                if (tabMain == 2 ) {
                    coords <<- unlist(pos)
                    SetCurPos1()
                }
            }
       }
       return()
  }

  SetCurPos1 <- function() {   #Set marker position for Baseline and Fit Component
     tabMain <- as.numeric(tcl(NB, "index", "current"))+1 #Notebook pages start from 0
     tabComp <- as.numeric(tcl(nbComponents, "index", "current"))+1 #Notebook pages start from 0
     if (coreline == 0) { return() }
     if (tabMain == 1 && length(Object[[coreline]]@Components) > 0) {
         tkmessageBox(message="Fit present: \nChange notebook page, Baseline Operations not Allowed", title="WARNINR", icon="warning")
         return()
     }
     if (coreline != 0) { #coreline != "All Spectra"
         xx <- coords[1]
         yy <- coords[2]
         #compute the distance between consecutive clicks
         if (Object[[coreline]]@Flags[1]) { #Binding energy set
             Xlimits <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=TRUE)  #point.coords$x in decreasing order
         } else {
             Xlimits <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=FALSE) #point.coords$x in increasing order
         }
         if (! is.null(point.coords$x[1]) && tabMain==1 ) { #initially point.coords contains the spectrum boundaries
             d.pts <- (point.coords$x - xx)^2  #distance between spectrum edge and marker position
             point.index <<- min(which(d.pts == min(d.pts)))  #which is the edge nearest to the marker?
         } else {
             point.index <<- 1
         }

         if (tabMain == 1 && SetZoom == FALSE) {  # Baseline notebook page
             point.coords$x[point.index] <<- xx   # set the marker position or modify the position for the baseline (
             point.coords$y[point.index] <<- yy
             Object[[coreline]]@Boundaries <<- point.coords
             if (Object[[coreline]]@Flags[1]) {   #Binding energy set
               Xlimits <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=TRUE) #pos$x in decreasing order
             } else {
               Xlimits <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=FALSE) #pos$x in increasing order
             }
             splinePoints <<- point.coords
             Make.Baseline()
         }
         if (tabMain == 1 && SetZoom == TRUE) {   # Baseline notebook page
             point.coords$x[point.index] <<- xx   # set the marker position or modify the position for the baseline (
             point.coords$y[point.index] <<- yy
             Object[[coreline]]@Boundaries <<- Corners
             if (Object[[coreline]]@Flags[1]) { #Binding energy scale
                 Xlimits <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE)  # X zoom limits
                 Ylimits <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE) # Y zoom limits
             } else {                           #Kinetic energy scale
                 Xlimits <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE)
                 Ylimits <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
             }
             point.coords$x <<- Xlimits   #Baseline edges == X zoom limits
             idx <- findXIndex(Object[[coreline]]@.Data[1], Xlimits[1])
             point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][idx]
             idx <<- findXIndex(Object[[coreline]]@.Data[1], Xlimits[2])
             point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][idx]
         }
         if (tabMain == 2 && tabComp == 1) { # add FitComp
             point.coords$x <<- xx
             point.coords$y <<- yy
             point.index <<- 1
             add.component()
         }
         if (tabMain == 2 && tabComp == 2) { # Components/Move Notebook page
             point.coords$x <<- xx
             point.coords$y <<- yy
             point.index <<- 1
             move.Comp()
         }
     }
     Marker$Points <<- point.coords
     replot()
     return()
  }

  SetCurPos2 <- function() {   #Right mouse button down
     xx <- coords[1]
     yy <- coords[2]

     if (BType=="Spline") {
         splinePoints$x <<- c(splinePoints$x, coords[1])
         splinePoints$y <<- c(splinePoints$y, coords[2])
         Marker$Points <<- splinePoints
     } else if ( SetZoom == TRUE ) {   #RGT click to define zoom area
         point.coords$x[point.index] <<- coords[1]   #add component 3 to abscissa
         point.coords$y[point.index] <<- coords[2]   #add component 3 to ordinate
         xlim <- sort(Xlimits,decreasing=FALSE)
         DY <- (Ylimits[2]-Ylimits[1])/30
         if(xx < xlim[1]) { xx <- xlim[1] }
         if(xx > xlim[2]) { xx <- xlim[2] }
         if(yy < (Ylimits[1]-DY)) { yy <- Ylimits[1] }
         if(yy > (Ylimits[2]+DY)) { yy <- Ylimits[2] }
         if (point.index==1) {
             Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
             Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
             point.index <<- 3    #to add comp. 3 to points.coord and save the new marker position
         } else if (point.index==3) {
             D <- vector("numeric", 4)
             Dmin <- ((point.coords$x[3]-Corners$x[1])^2 + (point.coords$y[3]-Corners$y[1])^2)^0.5  #valore di inizializzazione
             for (ii in 1:4) {
                 D[ii] <- ((point.coords$x[3]-Corners$x[ii])^2 + (point.coords$y[3]-Corners$y[ii])^2)^0.5  #dist P0 P1
                 if(D[ii] <= Dmin){
                    Dmin <- D[ii]
                    idx <- ii
                 }
             }
             if (idx == 1){
                Corners$x[1] <<- Corners$x[2] <<- point.coords$x[3]
                Corners$y[1] <<- Corners$y[3] <<- point.coords$y[3]
             } else if (idx==2){
                Corners$x[1] <<- Corners$x[2] <<- point.coords$x[3]
                Corners$y[2] <<- Corners$y[4] <<- point.coords$y[3]
             } else if (idx==3){
                Corners$x[3] <<- Corners$x[4] <<- point.coords$x[3]
                Corners$y[1] <<- Corners$y[3] <<- point.coords$y[3]
             } else if (idx==4){
               Corners$x[3] <<- Corners$x[4] <<- point.coords$x[3]
               Corners$y[2] <<- Corners$y[4] <<- point.coords$y[3]
             }
             if (Object[[coreline]]@Flags[1]) { #Binding energy set
                point.coords$x <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE) #pos$x in decreasing order
                point.coords$y <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
             } else {
                point.coords$x <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE) #pos$x in increasing order
                point.coords$y <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
             }
         }
         Marker$Points <<- Corners
     }
     replot()
     return()
  }

  replot <- function(...) {
     if (coreline == 0) {      #coreline == "All spectra"
         plot(Object)
     } else {
         tabMain <- as.numeric(tcl(NB, "index", "current"))+1  #Notebook pages start from 0
         tabComp <- as.numeric(tcl(nbComponents, "index", "current"))+1
         if (tabMain == 1) {   #Baseline definition
             plot(Object[[coreline]], xlim=Xlimits, ylim=Ylimits)
             if (SetZoom == TRUE) {  #in Zoom Mode  BType is set to ""
                 points(Marker$Points, col=Marker$col, cex=Marker$cex, lwd=Marker$lwd, pch=Marker$pch) #draw zoom corners
                 rect(point.coords$x[1], point.coords$y[1], point.coords$x[2], point.coords$y[2])      #draw zoom frame area
             } else {
                 points(Marker$Points, col=Marker$col, cex=Marker$cex, lwd=Marker$lwd, pch=Marker$pch) #plot markers at the CoreLine edges to define the Baseline
             }
         } else if (tabMain == 2 && (tabComp == 1)) {  #Components-Add/Delete Tab
             if (tclvalue(PS) == "residual" && hasFit(Object[[coreline]])) {
                 XPSresidualPlot(Object[[coreline]])
             } else {
                 plot(Object[[coreline]])
                 points(point.coords, col=2, cex=1.2, lwd=2, pch=3)  #draw component position
             }
         } else if ((tabMain == 2) && (tabComp == 2) ){#Components-Move Tab
             MaxCompCoords <- getMaxOfComponents(Object[[coreline]])
             point.coords$x <<- MaxCompCoords[[1]][compIndx]
             point.coords$y <<- MaxCompCoords[[2]][compIndx]
             if (tclvalue(PS) == "residual" && hasFit(Object[[coreline]])) {
                 XPSresidualPlot(Object[[coreline]])
             } else {
                plot(Object[[coreline]])
             }
             points(point.coords, col=2, cex=1, lwd=2, pch=1)
         }
         if (is.null(point.coords$x) || is.null(point.coords$y)){
             tkconfigure(StatusBar, text=" ")
         } else {
             tkconfigure(StatusBar, text=paste("Cursor position: x =",round(point.coords$x[1],2),
                                              "y =",round(point.coords$y[2],2), sep="  "))
         }
     }
  }

  Set.Coreline <- function(h, ...) {
     WidgetState(T1Frame1, "normal")
     WidgetState(T1Frame2, "normal")
     WidgetState(T1Frame3, "normal")
     WidgetState(T2Frame1, "normal")
     WidgetState(T2Frame2, "normal")
     WidgetState(T2Frame3, "normal")
     WidgetState(T2Frame4, "normal")
     WidgetState(T2Frame5, "normal")
     WidgetState(ButtFrame, "normal")

     tclvalue(BL) <<- ""  #reset baseline selection
     coreline <<- tclvalue(CL)
     coreline <<- unlist(strsplit(coreline, "\\."))   #"number." and "CL name" are separated
     assign("activeSpectName",coreline[2], envir=.GlobalEnv)
     coreline <<- as.integer(coreline[1])
     assign("activeSpectIndx", coreline, envir=.GlobalEnv)
     if (coreline > 0) {    #coreline != "All spectra"
         WidgetState(T1Frame1, "normal")
         WidgetState(T1Frame2, "normal")
         if (length(Object[[coreline]]@Baseline) > 0 ){
             WidgetState(T2group1, "normal")
         }
         # Zoom disabled
         SetZoom <<- FALSE
         # if boundaries already defined
         if (hasBaseline(Object[[coreline]]) ) {
           LL <- length(Object[[coreline]]@RegionToFit$x)
            BType <<- Object[[coreline]]@Baseline$type
            NN <- nchar(BType)
            BL_type <- paste(toupper(substr(BType, 1, 1)), substr(BType, 2, NN), sep="")
            tclvalue(BL) <- BL_type
            point.coords$x <<- c(Object[[coreline]]@RegionToFit$x[1], Object[[coreline]]@RegionToFit$x[LL])
            point.coords$y <<- c(Object[[coreline]]@RegionToFit$y[1], Object[[coreline]]@RegionToFit$y[LL])
            Xlimits <<- range(Object[[coreline]]@RegionToFit$x)
            Ylimits <<- range(Object[[coreline]]@RegionToFit$y)
         } else if (!hasBoundaries(Object[[coreline]]) ) {
            LL <- length(Object[[coreline]]@.Data[[1]])
            point.coords$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
            point.coords$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
            Xlimits <<- range(Object[[coreline]]@.Data[1])
            Ylimits <<- range(Object[[coreline]]@.Data[2])
            Object[[coreline]] <<- XPSsetRSF(Object[[coreline]], Object[[coreline]]@RSF)
         } else {
            Reset.Baseline()   #defines the spectral limits and marker positions
         }
         if (hasComponents(Object[[coreline]]) ) {
             tcl(NB,"select", 1)  #Select NBpage=2 'select' works on base 0
             tcl(nbComponents,"select", 0)
             CompNames <<- names(Object[[coreline]]@Components)
             tkconfigure(DelComponents, values=c(CompNames, "Fit"))
             tkconfigure(FitComponents, values=CompNames)
         }
         Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
         Object[[coreline]]@Boundaries <<- point.coords #boundaries correspond to coreline limits or RegToFit limits
     }
     tcl(NB,"select", 0)  #Select NBpage=1 'select' works on base 0
     replot()
  }

  Make.Baseline <- function(){     #deg, Wgt, splinePoints
     #Now generate the Base-Line
     if (BType == "") {
         tkmessageBox(message="Select the Base-Line Please", title="WARNING", icon="warning")
         return() 
     }
     if (BType == "Spline" && is.null(splinePoints$x)) { return() }

     Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])
     Object[[coreline]] <<- XPSbaseline(Object[[coreline]], BType, deg, Wgt, splinePoints )
     replot()
     WidgetState(T2group1, "normal")
  }

  Reset.Baseline <- function(h, ...) {
     ClearWidget(T1group2)
     T1Lab1 <<- ttklabel(T1group2, text="               ")     #add a void row
     tkgrid(T1Lab1, row = 1, column = 1, padx=5, pady=5, sticky="w")
     T1Lab2 <<- ttklabel(T1group2, text="               ")     #add a void row
     tkgrid(T1Lab2, row = 2, column = 1, padx=5, pady=5, sticky="w")
     if (coreline != 0) {   #coreline != "All spectra"
        if (FreezeLimits == FALSE) {  #ZOOM not activated
           if (Object[[coreline]]@Flags[1] == TRUE){  #Binding Energy scale
              point.coords$x <<- sort(range(Object[[coreline]]@.Data[[1]]),decreasing=TRUE)
           } else {                                   #Kinetic energy scale
              point.coords$x <<- sort(range(Object[[coreline]]@.Data[[1]]),decreasing=FALSE)
           }
           Xlimits <<- point.coords$x
           idx1 <- findXIndex(Object[[coreline]]@.Data[[1]], point.coords$x[1])
           point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][idx1]
           idx2 <- findXIndex(Object[[coreline]]@.Data[[1]], point.coords$x[2])
           point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][idx2]
           Ylimits <<- c(min(Object[[coreline]]@.Data[[2]][idx1:idx2]), max(Object[[coreline]]@.Data[[2]][idx1:idx2]))
           Object[[coreline]] <<- XPSremove(Object[[coreline]], "all")
           splinePoints <<- list(x=NULL, y=NULL)  #reset preivous baseline
        } else {  #a zoom is present: we preserve Xlimits and Ylimits and point.coords values
           Object[[coreline]] <<- XPSremove(Object[[coreline]], "all")
           splinePoints <<- list(x=NULL, y=NULL)  #reset preivous baseline
        }
          WidgetState(T2group1, "disabled")
       }
       Object[[coreline]]@Boundaries <<- point.coords
  }


  BLSelect <- function(){
     Ctrl <- 1
     BType <<- tclvalue(BL)    #save the last selection
     if (coreline == 0) {
         tkmessageBox(message="Please select the Core-Line to analyze!", title="WARNING", icon="warning")
         Ctrl <- -1
         return(Ctrl)
     } else if (BType == "") {
         tkmessageBox(message="Please select the BaseLine type!", title="WARNING", icon="warning")
         Ctrl <- -1
         return(Ctrl)
     } else if (BType == "Spline" && length(splinePoints)==0) { #splinePoints not defined skip XPSBaseline()
         tkmessageBox(message="Please select the spline points with the LEFT mouse button!", title="WARNING", icon="warning")
         splinePoints <<- list(x=NULL, y=NULL)
     } else if (hasBoundaries(Object[[coreline]]) == FALSE) {
         tkmessageBox(message="Set BaseLine edges. Right button to stop selection", title="WARNING", icon="warning")
     }
     Reset.Baseline()

     switch(BType,
#--- BaseLine type 1
            "Linear" = {
                        #fast generation of background do not need background reset
                        Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                        Make.Baseline()
                        replot()
                        GetCurPos(SingClick=FALSE)
                       },
            "Polynomial" = {
                        Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                        tkconfigure(T1Lab1, text="Polynom degree:")
                        BLvalue <- tclVar()
                        BLParam <<- ttkcombobox(T1group2, width = 10, textvariable = BLvalue, values = c("1","2","3","4","5","6") )
                        tkgrid(BLParam, row = 2, column = 1, padx=5, pady=5, sticky="w")
                        BLBtn <<- tkbutton(T1group2, text="Make BaseLine", command=function(){
                                         deg <<- as.numeric(tclvalue(BLvalue))
                                         Make.Baseline()
                                         replot()
                                         GetCurPos(SingClick=FALSE)
                                      })
                                      tkgrid(BLBtn, row = 2, column = 2, sticky="w")
                       },
            "Spline" = {
                        Marker <<- list(Points=point.coords, col=3, cex=1.15, lwd=2, pch=16)
                        tkconfigure(T1Lab1, text="Left button select spline points - Right button EXIT")
                        cat("\nLeft button select spline points - Right button EXIT  then press MAKE SPLINE BASELINE")
                                         BLBtn <<- tkbutton(T1group2, text="Make Spline Baseline", command=function(){
                                            #--- Interactive mouse control ---
                                            if (length(splinePoints$x) == 2) {
                                                BType <<- "linear" #"linear" #plot Linear baseline until splinePoints are selected
                                                decr <- FALSE #Kinetic energy set
                                                if (Object[[coreline]]@Flags[1] == TRUE) { decr <- TRUE }
                                                idx <- order(splinePoints$x, decreasing=decr)
                                                splinePoints$x <<- splinePoints$x[idx] #splinePoints$x in ascending order
                                                splinePoints$y <<- splinePoints$y[idx] #following same order select the correspondent splinePoints$y
                                                Object[[coreline]]@Boundaries$x <<- Xlimits <<- c(splinePoints$x[1],splinePoints$x[2]) #set the boundaries of the baseline
                                                Object[[coreline]]@Boundaries$y <<- c(splinePoints$y[1],splinePoints$y[2])
                                            } else {
                                                decr <- FALSE #Kinetic energy set
                                                if (Object[[coreline]]@Flags[1] == TRUE) { decr <- TRUE }
                                                idx <- order(splinePoints$x, decreasing=decr)
                                                splinePoints$x <<- splinePoints$x[idx] #splinePoints$x in ascending order
                                                splinePoints$y <<- splinePoints$y[idx] #following same order select the correspondent splinePoints$y
                                                LL <- length(splinePoints$x)
                                                Object[[coreline]]@Boundaries$x <<- Xlimits <<- c(splinePoints$x[1],splinePoints$x[LL]) #set the boundaries of the baseline
                                                Object[[coreline]]@Boundaries$y <<- c(splinePoints$y[1],splinePoints$y[LL])
                                                Marker <<- list(Points=splinePoints, col=3, cex=1.15, lwd=2, pch=16)
                                            }
                                            Make.Baseline()
                                            replot()
                                         })
                                         tkgrid(BLBtn, row = 2, column = 1, padx=5, pady=5, sticky="w")
                                         GetCurPos(SingClick=FALSE)
                       },
#--- BaseLine type 2
            "Shirley" = {
                        #fast generation of background do not need background reset
                        Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                        Make.Baseline()
                        replot()
                        GetCurPos(SingClick=FALSE)
                       },
            "2P.Shirley" = {
                        #fast generation of background do not need background reset
                        Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                        Make.Baseline()
                        replot()
                        GetCurPos(SingClick=FALSE)
                       },
            "3P.Shirley" = {
                        Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                        tkconfigure(T1Lab1, text="Distortion Parameter Ds")
                        BLvalue <- tclVar("0.3")
                        BLParam <<- ttkentry(T1group2, textvariable=BLvalue, width=10, foreground="gray")
                        tkgrid(BLParam, row = 2, column = 1, padx=5, pady=5, sticky="w")
                        tkbind(BLParam, "<FocusIn>", function(K){
                                         tkconfigure(BLParam, foreground="red")
                                 })
                        tkbind(BLParam, "<Key-Return>", function(K){
                                         tkconfigure(BLParam, foreground="black")
                                 })
                        ww <- as.numeric(tkwinfo("reqwidth", BLParam))+20
                        BLBtn <<- tkbutton(T1group2, text="Make Baseline", command=function(){
                                         Wgt <<- as.numeric(tclvalue(BLvalue))
                                         slot(Object[[coreline]],"Boundaries") <<- point.coords
                                         Make.Baseline()
                                         replot()
                                         GetCurPos(SingClick=FALSE)
                                 })
                        tkgrid(BLBtn, row = 2, column = 1, padx=ww, pady=5, sticky="w")
                       },
            "LP.Shirley" = {
                        Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                        tkconfigure(T1Lab1, text="B coeff.")
                        BLvalue <- tclVar("0.005")
                        BLParam <<- ttkentry(T1group2, textvariable=BLvalue, width=10, foreground="black")
                        tkgrid(BLParam, row = 2, column = 1, padx=5, pady=5, sticky="w")
                        tkbind(BLParam, "<FocusIn>", function(K){
                                         tkconfigure(BLParam, foreground="red")
                                 })
                        tkbind(BLParam, "<Key-Return>", function(K){
                                         tkconfigure(BLParam, foreground="black")
                                 })
                        ww <- as.numeric(tkwinfo("reqwidth", BLParam))+20
                        BLBtn <<- tkbutton(T1group2, text="Make Baseline", command=function(){
                                         Wgt <<- as.numeric(tclvalue(BLvalue))
                                         Object[[coreline]]@Boundaries <<- point.coords
                                         Make.Baseline()
                                         replot()
                                         GetCurPos(SingClick=FALSE)
                                 })
                        tkgrid(BLBtn, row = 2, column = 1, padx=ww, pady=5, sticky="w")
                       },
#--- BaseLine type 3
            "2P.Tougaard" = {
                        #fast generation of background do not need background reset
                        Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                        tkconfigure(T1Lab1, text="               ") #add an empty rows
                        tkconfigure(T1Lab2, text="               ") #add a second empty row
                        Make.Baseline()
                        replot()
                        GetCurPos(SingClick=FALSE)
                       },
            "3P.Tougaard" = {
                        #fast generation of background do not need background reset
                        Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                        tkconfigure(T1Lab1, text="               ") #add an empty rows
                        tkconfigure(T1Lab2, text="               ") #add a second empty row
                        Make.Baseline()
                        replot()
                        GetCurPos(SingClick=FALSE)
                       },
            "4P.Tougaard" = {
                        Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                        tkconfigure(T1Lab1, text="C coeff.") #add an empty rows
                        BLvalue <<- tclVar("0.5")
                        BLParam <<- ttkentry(T1group2, textvariable=BLvalue, width=10, foreground="black")
                        tkgrid(BLParam, row = 2, column = 1, padx=5, pady=5, sticky="w")
                        tkbind(BLParam, "<FocusIn>", function(K){
                                         tkconfigure(BLParam, foreground="red")
                                 })
                        tkbind(BLParam, "<Key-Return>", function(K){
                                         tkconfigure(BLParam, foreground="black")
                                 })
                        ww <- as.numeric(tkwinfo("reqwidth", BLParam))+20
                        BLBtn <<- tkbutton(T1group2, text="Make Baseline", command=function(){
                                         Wgt <<- as.numeric(tclvalue(BLvalue))
                                         Object[[coreline]]@Boundaries <<- point.coords
                                         Make.Baseline()
                                         replot()
                                         GetCurPos(SingClick=FALSE)
                                 })
                        tkgrid(BLBtn, row = 2, column = 1, padx=ww, pady=5, sticky="w")
                       }
             )
  }

  add.component <- function() {
     if (coreline != 0 && hasBaseline(Object[[coreline]]) ) {
          if (is.null(point.coords$x[1])) {
             return() 
         } else {
             LL <- length(point.coords$x)
             for(ii in 1:LL){
                 Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = tclvalue(FF),
                                                 peakPosition = list(x = point.coords$x[point.index], y = point.coords$y[point.index]))
                 #--- update fit
                 tmp <- sapply(Object[[coreline]]@Components, function(z) matrix(data=z@ycoor))  #create a matrix formed by ycoor of all the fit Components
                 CompNames <<- names(Object[[coreline]]@Components)
                 tkconfigure(DelComponents, values=c(CompNames, "Fit"))  #Update component selection in MOVE COMP panel
                 tkconfigure(FitComponents, values=CompNames)  #Update component selection in MOVE COMP panel

                 Object[[coreline]]@Fit$y <<- ( colSums(t(tmp)) - length(Object[[coreline]]@Components)*(Object[[coreline]]@Baseline$y))
                 replot()
             }
         }
     }
  }

  del.component <- function(h, ...) {
     answ <- tclVar("")
     txt <- "All Constraints will be lost! Proceed anyway?"
     answ <- tkmessageBox(message=txt, type="yesno", title="WARNING", icon="warning")
     if (tclvalue(answ) == "yes") {
         LL<-length(Object[[coreline]]@Components)
         for (ii in 1:LL) { #remove all CONSTRAINTS
               Object[[coreline]]<<-XPSConstrain(Object[[coreline]],ii,action="remove",variable=NULL,value=NULL,expr=NULL)
         }
         if (coreline != 0 && hasComponents(Object[[coreline]]) ) {
             Component <- tclvalue(FC)
             if (Component != "Fit"){
                 Component <- grep(Component, CompNames)
                 Object[[coreline]] <<- XPSremove(Object[[coreline]], what="components", number=Component)
                 CompNames <<- names(slot(Object[[coreline]],"Components"))
                 if (length(Object[[coreline]]@Components) > 0 ) {
                     #update fit without deteted component
                     tmp <- sapply(Object[[coreline]]@Components, function(z) matrix(data=z@ycoor))
                     Object[[coreline]]@Fit$y <<- ( colSums(t(tmp)) - length(Object[[coreline]]@Components)*(Object[[coreline]]@Baseline$y))
                 }
                 tkconfigure(DelComponents, values=c(CompNames, "Fit"))  #Update component selection in MOVE COMP panel
                 tkconfigure(FitComponents, values=CompNames)  #Update component selection in MOVE COMP panel
             } else {
                 Object[[coreline]] <<- XPSremove(Object[[coreline]], "fit")
                 CompNames <<- ""
                 tkconfigure(DelComponents, values=c(CompNames, "Fit"))  #Update component selection in MOVE COMP panel
                 tkconfigure(FitComponents, values=CompNames)  #Update component selection in MOVE COMP panel
             }
             #---Update Fit Component Combobox
             tclvalue(FC) <- ""
             tclvalue(PS) <- "Normal"
             point.coords <<- list(x=NULL,y=NULL)
             replot()
         }
     }
  }

  move.Comp <- function(...) {
     compIndx <- tclvalue(MFC)
     compIndx <- unlist(strsplit(compIndx, split="C"))   #index selected component
     compIndx <- as.integer(compIndx[2])
     if (coreline != 0) {
          xx <- point.coords$x[point.index]
          yy <- point.coords$y[point.index]

          varmu <- getParam(Object[[coreline]]@Components[[compIndx]],variable="mu")
          minmu <- varmu$start-varmu$min
          maxmu <- varmu$max-varmu$start
          newmu <- c(xx, xx-minmu, xx+maxmu)
          FuncName <- Object[[coreline]]@Components[[compIndx]]@funcName
          idx <- findXIndex(Object[[coreline]]@Baseline$x, xx)
          yy <- yy - Object[[coreline]]@Baseline$y[idx]
          newh <- GetHvalue(Object[[coreline]],compIndx, FuncName, yy)  #provides the correct yy value for complex functions
          newh <- c(newh, 0, 5*newh)

          Object[[coreline]]@Components[[compIndx]] <<- setParam(Object[[coreline]]@Components[[compIndx]], parameter=NULL, variable="mu", value=newmu)
          Object[[coreline]]@Components[[compIndx]] <<- setParam(Object[[coreline]]@Components[[compIndx]], parameter=NULL, variable="h", value=newh)
          Object[[coreline]]@Components[[compIndx]] <<- Ycomponent(Object[[coreline]]@Components[[compIndx]], x=Object[[coreline]]@RegionToFit$x, y=Object[[coreline]]@Baseline$y)

          tmp <- sapply(Object[[coreline]]@Components, function(z) matrix(data=z@ycoor))
          Object[[coreline]]@Fit$y <<- ( colSums(t(tmp)) - length(Object[[coreline]]@Components)*(Object[[coreline]]@Baseline$y) )
          Object[[coreline]] <<- sortComponents(Object[[coreline]]) #order components in decreasing order
          updateOutput()
     }
  }

  updateOutput <- function(...) {
     if (coreline != 0 ) {
           disp <- tclvalue(DT)
           if (disp == 1) {
               tclvalue(DispTxt[2]) <- FALSE  #cleares the checkButton2
               XPScalc(Object[[coreline]])
           } #CheckButton 1 selected
           if (disp == 2 && hasFit(Object[[coreline]])) {
               tclvalue(DispTxt[1]) <- FALSE  #cleares the checkButton1
               XPSquantify(Object)
           }
     }
  }



#---  Variables  -----------------------------
  activeFName <- get("activeFName", envir = .GlobalEnv)
  if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
      tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
      return()
  }

  FNameList <- XPSFNameList() #list of XPSSamples
  if(is.null(FNameList)) {
     return()
  }
  Object <- get(activeFName,envir=.GlobalEnv)
  SpectList <- XPSSpectList(activeFName) #list of CoreLines if selected XPSSample
  SpectList <- c("0.All spectra", SpectList)
  coreline <- 0
  deg <- 1   #by default the baseline polynom degree = 1
  Wgt <- 0.3 #LinearPolynomial weigth in LP.Shirley
  BType <- "" #defaul BKground
  splinePoints <- list(x=NULL, y=NULL)
  ZoomPoints <- list(x=NULL, y=NULL)
  BaseLines1 <- c("Linear", "Polynomial", "Spline")
  BaseLines2 <- c("Shirley", "2P.Shirley", "3P.Shirley", "LP.Shirley")
  BaseLines3 <- c("2P.Tougaard", "3P.Tougaard", "4P.Tougaard")
  FitFunct <- c("Gauss", "Lorentz", "Voigt", "Sech2", "GaussLorentzProd", "GaussLorentzSum",
              "AsymmGauss", "AsymmLorentz", "AsymmVoigt", "AsymmGaussLorentz", "AsymmGaussVoigt",
              "AsymmGaussLorentzProd", "DoniachSunjic", "DoniachSunjicTail",
              "DoniachSunjicGauss", "DoniachSunjicGaussTail", "SimplifiedDoniachSunjic", "ExpDecay",
              "PowerDecay", "Sigmoid")
  CompNames <- "   "
  SetZoom <- FALSE
  FreezeLimits <- FALSE

  coords <- NULL # for printing mouse coordinates on the plot
  point.coords <- list(x=NULL, y=NULL)
  point.index <- 1
  Corners <- point.coords
  compIndx <- 1
  Marker <- list(Points=list(x=NULL, y=NULL), col=2, cex=2, lwd=1.5, pch=10)
  Xlimits <- NULL
  Ylimits <- NULL
  EXIT <- NULL

  BLgroup <- list()
  BLvalue <- list()

  WinPointers <- NULL
  WinSize <- XPSSettings$General[4]
  plot(Object)

  BLParam <- tclVar("")
  BLBtn <- tclVar("")
  T1Lab1 <- NULL
  T1Lab2 <- NULL

#----- ANALYSIS GUI -----------------------------
  MainWindow <- tktoplevel()
  tkwm.title(MainWindow,"XPS ANALYSIS")
  tkwm.geometry(MainWindow, "+100+50")

  MainGroup <- ttkframe(MainWindow,  borderwidth=2, padding=c(5,5,5,5))
  tkgrid(MainGroup, row = 1, column=1, sticky="w")

## XPS Sample & Core lines
  MainFrame1 <- ttklabelframe(MainGroup, text = " XPS Sample and Core line Selection ", borderwidth=2)
  tkgrid(MainFrame1, row = 2, column=1, sticky="w")

  XS <- tclVar(activeFName)
  XPS.Sample <- ttkcombobox(MainFrame1, width = 25, textvariable = XS, values = FNameList)
  tkbind(XPS.Sample, "<<ComboboxSelected>>", function(){
            activeFName <<- tclvalue(XS)
            Object <<- get(activeFName, envir=.GlobalEnv)
            SpectList <<- XPSSpectList(activeFName)
            SpectList <<- c("0.All spectra", SpectList)
            tkconfigure(Core.Lines, values=SpectList)
            coreline <<- 0
            replot()
         })
  tkgrid(XPS.Sample, row = 3, column=1, padx=5, pady=5, sticky="w")

  CL <- tclVar("")
  Core.Lines <- ttkcombobox(MainFrame1, width = 25, textvariable = CL, values = SpectList)
  tkbind(Core.Lines, "<<ComboboxSelected>>", function(){ Set.Coreline() })
  tkgrid(Core.Lines, row = 3, column=2, padx=5, pady=5, sticky="w")

#----- Notebook -----------------------------
  NB <- ttknotebook(MainGroup)
  tkbind(NB, "<<NotebookTabChanged>>", function(){
        nbPage <- as.integer(tcl(NB, "index", "current"))+1 #Notebook pages start from 0
        coreline <<- tclvalue(CL)
        coreline <<- unlist(strsplit(coreline, "\\."))   #split string in correspondence of the point: coreline[1]=index, coreline[2]=spect name
        coreline <<- as.numeric(coreline[1])             #this is the coreline index
        if (is.na(coreline)) { return() } #CoreLine still not selected
        if (nbPage > 1) {
            #Resets the Baseline Radio_Panel in the initial form
            if(tclvalue(tkwinfo("exists", BLParam)) == "1") {  #reset BL panel to the original form
               tkdestroy(BLParam)
               BLParam <<- tclVar("")
            }
            if(tclvalue(tkwinfo("exists", BLBtn)) == "1") {
               tkdestroy(BLBtn)
               BLBtn <<- tclVar("")
            }
            tkconfigure(T1Lab1, text="               ") #add an empty rows
            tkconfigure(T1Lab2, text="               ") #add a second empty row
            tclvalue(BL) <- ""
            point.coords <<- list(x = NULL, y = NULL)
        }
        tclvalue(PS) <- "Normal"
        tkconfigure(StatusBar, text=" Status:     ")
        if (coreline > 0){
            if(length(Object[[coreline]]@RegionToFit) > 0 ) {
                WidgetState(T2group1, "disabled")
            }
        }   #baseline already defined enable component selection
        Xlimits <<- range(Object[[coreline]]@.Data[[1]])
        Ylimits <<- range(Object[[coreline]]@.Data[[2]])
        plot(Object[[coreline]])
     }
  )
  tkgrid(NB, row = 4, column = 1, pady=5, sticky="w")

#----- TAB1: Baseline -----
  T1group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )  #definition of the first NB page
  tkadd(NB, T1group1, text="  Baseline  ")

  T1Frame1 <- ttklabelframe(T1group1, text = " Baseline Functions ", borderwidth=2)
  tkgrid(T1Frame1, row = 1, column = 1)

  BL <- tclVar("")
  BaselineType <- NULL
  jj <- 1
  for(ii in 1:length(BaseLines1)){
      BaselineType[ii] <- ttkradiobutton(T1Frame1, text=BaseLines1[ii], variable=BL, value=BaseLines1[ii], 
      command=function(){ 
      BLSelect() })
      tkgrid(unlist(BaselineType[ii]), row = 2, column = ii, padx=5, pady=3, sticky="w")
  }
  LL <- ii
  for(ii in 1:length(BaseLines2)){
      BaselineType[(ii+LL)] <- ttkradiobutton(T1Frame1, text=BaseLines2[ii], variable=BL, value=BaseLines2[ii], command=function(){ BLSelect() })
      tkgrid(unlist(BaselineType[(ii+LL)]), row = 3, column = ii, padx=5, pady=3, sticky="w")
  }
  LL <- ii+LL
  for(ii in 1:length(BaseLines3)){
      BaselineType[(ii+LL)] <- ttkradiobutton(T1Frame1, text=BaseLines3[ii], variable=BL, value=BaseLines3[ii], command=function(){ BLSelect() })
      tkgrid(unlist(BaselineType[(ii+LL)]), row = 4, column = ii, padx=5, pady=3, sticky="w")
  }
  BaselineType <- unlist(BaselineType)

  T1group2 <- ttkframe(T1group1,  borderwidth=2, padding=c(0,0,0,0) )  #definition of the first NB page
  tkgrid(T1group2, row = 5, column = 1, padx=0, pady=3, sticky="w")

  T1Lab1 <- ttklabel(T1group2, text="               ")     #add a void row
  tkgrid(T1Lab1, row = 1, column = 1, padx=5, pady=5, sticky="w")
  T1Lab2 <- ttklabel(T1group2, text="               ")     #add a void row
  tkgrid(T1Lab2, row = 2, column = 1, padx=5, pady=5, sticky="w")

  T1Frame2 <- ttklabelframe(T1group1, text = " Reset ", borderwidth=2)
  tkgrid(T1Frame2, row = 6, column = 1, sticky="w")
  BLReset <- tkbutton(T1Frame2, text="   Reset Baseline   ", command=function(){
                   splinePoints <<- list(x=NULL, y=NULL)
                   tclvalue(BL) <- ""
                   BType <<- ""  #otherwise conflict between mouseRGT-selected points for zooming and for splinePoints
                   Reset.Baseline()
                   Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                   replot()
                })
  tkgrid(BLReset, row = 7, column = 1, padx=5, pady=5, sticky="w")

  T1Frame3 <- ttklabelframe(T1group1, text = " Plot ", borderwidth=2)
  tkgrid(T1Frame3, row = 8, column = 1, sticky="w")

  ZRbutton <- tkbutton(T1Frame3, text="SET ZOOM REGION", command=function(){
                   point.coords <<- list(x=NULL, y=NULL)   #point.coords contain the X, Y data ranges
                   txt <- paste("Left click near the blue corners to adjust the zoom area",
                                "\n right click to stop and select zoo area", sep="")
                   tkmessageBox(message=txt, title="WARNING", icon="warning")
                   tclvalue(BL) <- ""
                   point.index <<- 3
                   txt <- "Reset Baseline and CoreLine Fit and start from beginning?"
                   answ <- tclVar("")
                   answ <- tkmessageBox(message=txt, type="yesno", title="WARNING", icon="warning")
                   answ <- tclvalue(answ)
                   if(answ == "yes"){
                      Reset.Baseline()
                      point.coords$x <<- range(Object[[coreline]]@.Data[[1]])
                      point.coords$y <<- range(Object[[coreline]]@.Data[[2]])
                   } else {
                      if(hasRegionToFit(Object[[coreline]])){
                         LL <- length(Object[[coreline]]@RegionToFit$x)
                         Object[[coreline]]@Boundaries$x <<- range(Object[[coreline]]@RegionToFit$x)
                         Object[[coreline]]@Boundaries$y <<- range(Object[[coreline]]@RegionToFit$y)
                         point.coords$x <<- Object[[coreline]]@Boundaries$x
                         point.coords$y <<- Object[[coreline]]@Boundaries$y
                      } else {
                         point.coords$x <<- range(Object[[coreline]]@.Data[[1]])
                         point.coords$y <<- range(Object[[coreline]]@.Data[[2]])
                      }
                   }
                   if (Object[[coreline]]@Flags[1]) { #Binding energy set
                       point.coords$x <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=TRUE) #pos$x in decreasing order
                   }
                   Xlimits <<- point.coords$x
                   Ylimits <<- point.coords$y
                   Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
                   Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
                   Marker <<- list(Points=Corners, col=4, cex=1.2, lwd=2.5, pch=3)
                   SetZoom <<- TRUE    #definition of zoom area disabled
                   FreezeLimits <<- TRUE  #reset spectrum range disabled
                   WidgetState(MZbutton, "normal")
                   WidgetState(ZObutton, "disabled")
                   replot()
                   GetCurPos(SingClick=FALSE)
                })
  tkgrid(ZRbutton, row = 1, column = 1, padx=5, pady=5, sticky="w")

  MZbutton <- tkbutton(T1Frame3, text="  MAKE ZOOM  ", command=function(){
                   if (Object[[coreline]]@Flags[1]) { #Binding energy set
                       point.coords$x <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=TRUE) #pos$x in decreasing order
                   } else {
                       point.coords$x <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=FALSE) #pos$x in increasing order
                   }
                   point.coords$y <<- sort(c(point.coords$y[1], point.coords$y[2]), decreasing=FALSE) #pos$x in increasing order
                   Xlimits <<- point.coords$x  #Baseline edges == X zoom limits
                   Ylimits <<- point.coords$y
                   idx <- findXIndex(Object[[coreline]]@.Data[[1]], Xlimits[1])
                   point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][idx]
                   idx <- findXIndex(Object[[coreline]]@.Data[[1]], Xlimits[2])
                   point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][idx]
                   Object[[coreline]]@Boundaries <<- point.coords
                   tclvalue(BL) <- ""
                   point.index <<- 1
                   Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                   SetZoom <<- FALSE  #definition of zoom area disabled
                   FreezeLimits <<- TRUE #reset spectrum range disabled
                   replot()
                   WidgetState(ZObutton, "normal")
                })
  tkgrid(MZbutton, row = 1, column = 2, padx=5, pady=5, sticky="w")

  ZObutton <- tkbutton(T1Frame3, text="   ZOOM OUT   ", command=function(){
                   Xlimits <<- range(Object[[coreline]]@.Data[1])  #Set plot limits to the whole coreline extension
                   Ylimits <<- range(Object[[coreline]]@.Data[2])
                   Object[[coreline]]@Boundaries$x <<- Xlimits
                   Object[[coreline]]@Boundaries$y <<- Ylimits
                   if ( hasBaseline(Object[[coreline]]) ) {
                       Xlimits <<- range(Object[[coreline]]@RegionToFit$x) #if Baseline present limit the
                       Ylimits <<- range(Object[[coreline]]@RegionToFit$y) #plot limits to the RegionToFit
                       LL<-length(Object[[coreline]]@RegionToFit$x)
                       point.coords$x[1] <<- Object[[coreline]]@RegionToFit$x[1]
                       point.coords$x[2] <<- Object[[coreline]]@RegionToFit$x[LL]
                       point.coords$y[1] <<- Object[[coreline]]@RegionToFit$y[1]
                       point.coords$y[2] <<- Object[[coreline]]@RegionToFit$y[LL]
                   } else {
                       LL<-length(Object[[coreline]]@.Data[[1]])
                       point.coords$x[1] <<- Object[[coreline]]@.Data[[1]][1]
                       point.coords$x[2] <<- Object[[coreline]]@.Data[[1]][LL]
                       point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][1]
                       point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][LL]
                       point.index <<- 1
                   }
                   if (Object[[coreline]]@Flags[1]) { #Binding energy set
                       Xlimits <<- sort(Xlimits, decreasing=TRUE)  #pos$x in decreasing order
                   } else {
                       Xlimits <<- sort(Xlimits, decreasing=FALSE) #pos$x in increasing order
                   }
                   Ylimits <<- sort(Ylimits, decreasing=FALSE) #pos$ in increasing order
                   SetZoom <<- FALSE  #definition of zoom area disabled
                   FreezeLimits <<- FALSE #reset spectrum range enabled
                   Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                   replot()
                   WidgetState(ZRbutton, "disabled")
                   WidgetState(MZbutton, "disabled")
                   WidgetState(ZObutton, "disabled")
                })
  tkgrid(ZObutton, row = 1, column = 3, padx=5, pady=5, sticky="w")

#----- TAB2: Components -----
  T2group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )  #definition of the first NB page
  tkadd(NB, T2group1, text=" Components ")

  nbComponents <- ttknotebook(T2group1)
  tkgrid(nbComponents, row = 1, column = 1, padx=5, pady=5, sticky="w")

#----- SubNotebook: Fit Components -----
  T2group2 <- ttkframe(nbComponents,  borderwidth=2, padding=c(5,5,5,5) )  #definition of the first NB page
  tkadd(nbComponents, T2group2, text=" Add/Delete ")
  T2group3 <- ttkframe(nbComponents,  borderwidth=2, padding=c(5,5,5,5) )  #definition of the first NB page
  tkadd(nbComponents, T2group3, text=" Move ")

#----- Add/Delete component subtab
  T2Frame1 <- ttklabelframe(T2group2, text = " Component LineShapes ", borderwidth=2)
  tkgrid(T2Frame1, row = 1, column = 1, sticky="w")

  FF <- tclVar("")
  fitFunction <- ttkcombobox(T2Frame1, width = 20, textvariable = FF, values = FitFunct)
  tkbind(fitFunction, "<<ComboboxSelected>>", function(){
                      cat("\n Selected Fit Function: ", tclvalue(FF))
                })
  tkgrid(fitFunction, row = 1, column = 1, padx=5, pady=5, sticky="w")

  FC <- tclVar("")
  DelComponents <- ttkcombobox(T2Frame1, width = 10, textvariable = FC, values = CompNames)
  tkbind(DelComponents, "<<ComboboxSelected>>", function(){
                      compIndx <- tclvalue(FC)
                      cat("\n Selected component to delete: ", compIndx)
                      compIndx <- unlist(strsplit(compIndx, split="C"))   #index of the selected component (numeric)
                      compIndx <<- as.integer(compIndx)
                })
  tkgrid(DelComponents, row = 1, column = 2, padx=5, pady=5, sticky="w")


  T2Frame2 <- ttklabelframe(T2group2, text = " Set Fit Components ", borderwidth=2)
  tkgrid(T2Frame2, row = 2, column = 1, sticky="w")


  tkgrid(ttklabel(T2Frame2, text="Select Fit Function and Press 'Add'"),
                  row = 1, column = 1, padx=5, pady=5, sticky="w")

  add_btn <- tkbutton(T2Frame2, text="       ADD      ", command=function(){
                   if (tclvalue(FF) == "") {
                       tkconfigure(StatusBar, text=sprintf(" Selected Function %s", tclvalue(FF)))
                       txt <- paste("1) Select the fit function. \n",
                                "2) Press 'Add' button to add the fit components. \n",
                                "2) Left mouse button to enter the Fit component position \n",
                                "3) Stop entering positions with rigth mouse button. \n",
                                "5) Change fit function and restart from point (2).", sep="")
                       tkmessageBox(message=txt, title="WARNING", icon="warning")
                       tcl("update", "idletasks") #closes the message window
                       return()
                   }
                   GetCurPos(SingClick=FALSE)
                })
  tkgrid(add_btn, row = 2, column = 1, padx=5, pady=5, sticky="w")

  del_btn <- tkbutton(T2Frame2, text="     DELETE     ", command=function(){
                   del.component()
                })
  tkgrid(del_btn, row = 2, column = 1, padx=c(185, 5), pady=5, sticky="w")

#----- Move component subtab
  T2Frame3 <- ttklabelframe(T2group3, text = " Select Fit Component ", borderwidth=2)
  tkgrid(T2Frame3, row = 1, column = 1, padx=5, pady=5, sticky="w")
  MFC <- tclVar("")
  FitComponents <- ttkcombobox(T2Frame3, width = 15, textvariable = MFC, values = CompNames)
  tkbind(FitComponents, "<<ComboboxSelected>>", function(){
                               compIndx <- tclvalue(MFC)
                               compIndx <- unlist(strsplit(compIndx, split="C"))   #index of the selected component (numeric)
                               compIndx <<- as.integer(compIndx)
                               replot()
                               tkmessageBox(message="\nLeft Button Component Position. \nRight Button to Exit", title="WARNING", icon="warning")
                               tcl("update", "idletasks")
                               GetCurPos(SingClick=FALSE)
                           })
  tkgrid(FitComponents, row = 1, column = 1, padx=5, pady=5, sticky="w")

  T2Frame4 <- ttklabelframe(T2group3, text = " Print ", borderwidth=2)
  tkgrid(T2Frame4, row = 2, column = 1, padx=5, pady=5, sticky="w")

  DispTxt <- c("Area table",  "Quantification table")
  DT <- tclVar(FALSE)
  for(ii in 1:2){
     tkgrid( tkcheckbutton(T2Frame4, text=DispTxt[ii], variable=DT, onvalue = ii, offvalue = 0,
          command=function(){ updateOutput() }),
          row = 1, column = ii, padx=5, pady=5, sticky="w")
#          tclvalue(DispTxt[ii]) <- FALSE # checkbuttons unset
  }

  #--- plot type : Residual or simple
  T2Frame5 <- ttklabelframe(T2group1, text = " Plot ", borderwidth=2)
  tkgrid(T2Frame5, row = 2, column = 1, padx=5, pady=5, sticky="w")

  PlotStyle=c("Normal", "Residue")
  PS <- tclVar("")
  for(ii in 1:length(PlotStyle)){
      plotFit <- ttkradiobutton(T2Frame5, text=PlotStyle[ii], variable=PS, value=PlotStyle[ii],
                              command=function(){ replot() })
      tkgrid(plotFit, row = 1, column = ii, padx=15, pady=5, sticky="w")
  }
  tclvalue(PS) <- "Normal"

#----- SAVE&CLOSE buttons -----
  ButtFrame <- ttklabelframe(MainGroup, text = " Save & Exit ", borderwidth=2)
  tkgrid(ButtFrame, row = 5, column = 1, padx=5, pady=5, sticky="w")

  resetBtn <- tkbutton(ButtFrame, text="         RESET        ", command=function(){
                  tclvalue(BL) <- ""
                  tkconfigure(T1Lab1, text="               ") #add an empty rows
                  tkconfigure(T1Lab2, text="               ") #add a second empty row
                  Object <<- get(activeFName, envir = .GlobalEnv)
                  Xlimits <<- range(Object[[coreline]]@.Data[1])
                  if (Object[[coreline]]@Flags[1] == TRUE){
                      Xlimits <<- sort(Xlimits, decreasing=TRUE)
                  }
                  Ylimits <<- range(Object[[coreline]]@.Data[2])
                  LL <- length(Object[[coreline]]@.Data[[2]])
                  point.coords$x <<- Xlimits
                  point.coords$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
                  Object[[coreline]]@Boundaries$x <<- Xlimits
                  Object[[coreline]]@Boundaries$y <<- Ylimits
                  Marker <<- list(Points=point.coords, col=2, cex=2, lwd=1.5, pch=10)
                  splinePoints <- list(x=NULL, y=NULL)
                  tcl(NB, "select", 0)
                  WidgetState(ZRbutton, "disabled")
                  WidgetState(MZbutton, "disabled")
                  WidgetState(ZObutton, "disabled")
                  replot()
              })
  tkgrid(resetBtn, row = 1, column = 1, padx=5, pady=5, sticky="w")

  saveBtn <- tkbutton(ButtFrame, text="         SAVE         ", command=function(){
                  coreline <<- tclvalue(CL)
                  coreline <<- unlist(strsplit(coreline, "\\."))
                  assign(activeFName, Object, envir = .GlobalEnv)
                  assign("activeSpectIndx", as.integer(coreline[1]), envir=.GlobalEnv)
                  assign("activeSpectName", coreline[2], envir = .GlobalEnv)
                  XPSSaveRetrieveBkp("save")
                  coreline <<- as.integer(coreline[1])
                  plot(Object[[activeSpectIndx]])
              })
  tkgrid(saveBtn, row = 1, column = 2, padx=5, pady=5, sticky="w")

  savexitBtn <- tkbutton(ButtFrame, text="    SAVE & EXIT    ", command=function(){
                  EXIT <<- TRUE
                  coreline <<- tclvalue(CL)
                  coreline <<- unlist(strsplit(coreline, "\\."))
                  assign(activeFName, Object, envir = .GlobalEnv)
                  assign("activeSpectIndx", as.integer(coreline[1]), envir=.GlobalEnv)
                  assign("activeSpectName", coreline[2], envir = .GlobalEnv)
                  XPSSettings$General[4] <<- 7      #Reset to normal graphic win dimension
                  assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                  XPSSaveRetrieveBkp("save")
                  tkdestroy(MainWindow)
                  coreline <<- as.integer(coreline[1])
                  plot(Object[[activeSpectIndx]])
              })
  tkgrid(savexitBtn, row = 2, column = 1, padx=5, pady=5, sticky="w")

  exitBtn <- tkbutton(ButtFrame, text="         EXIT          ", command=function(){
                  EXIT <<- TRUE
                  XPSSettings$General[4] <<- 7      #Reset to normal graphic win dimension
                  assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                  XPSSaveRetrieveBkp("save")
                  tkdestroy(MainWindow)
                  plot(Object)
              })
  tkgrid(exitBtn, row = 2, column = 2, padx=5, pady=5, sticky="w")

#----- State bar -----
  tkSep <- ttkseparator(MainGroup, orient="horizontal")
  tkgrid(tkSep, row = 6, column = 1, padx = 5, pady = 5, sticky="we")
  StatusBar <- ttklabel(MainGroup, text=" Status : ", relief="sunken", foreground="blue")
  tkgrid(StatusBar, row = 7, column = 1, padx = 5, pady = 5, sticky="we")

#----- Disable until XPSSample&Core Line Selected -----
  WidgetState(T1Frame1, "disabled")
  WidgetState(T1Frame2, "disabled")
  WidgetState(T1Frame3, "disabled")
  WidgetState(T2Frame1, "disabled")
  WidgetState(T2Frame2, "disabled")
  WidgetState(T2Frame3, "disabled")
  WidgetState(T2Frame4, "disabled")
  WidgetState(T2Frame5, "disabled")
  WidgetState(ButtFrame, "disabled")

  tcl(nbComponents,"select", 0)  #Set NB page=1, Select works on base 0
  tcl(NB,"select", 0)  #Set NB page=1, Select works on base 0
  tcl("update", "idletasks") #Complete the idle tasks
#  tkwait.window(MainWindow)


}




