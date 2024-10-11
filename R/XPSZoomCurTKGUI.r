## zoom function


#' @title XPSZoomCur
#' @description XPSZoomCur for zooming a portion of a plotted spectrum.
#' @examples
#' \dontrun{
#' 	XPSZoomCur()
#' }
#' @export
#'



XPSZoomCur <- function(){

  GetCurPos <- function(SingClick){
       coords <- NULL
       WidgetState(ZMframe1, "disabled")   #prevent exiting Analysis if locatore active
       WidgetState(OKBtn, "disabled")
       WidgetState(CurPosBtn, "disabled")
       WidgetState(ExitBtn, "disabled")
       EXIT <- FALSE
       LocPos <<- list(x=0, y=0)
       Nclk <- SingClick
       if (Nclk == FALSE) Nclk <- 1
       while(EXIT == FALSE){  #if pos1 not NULL a mouse butto was pressed
            LocPos <<- locator(n=Nclk, type="p", pch=3, cex=1.5, col="blue", lwd=2) #to modify the zoom limits
            if (is.null(LocPos)) {
                WidgetState(ZMframe1, "normal")
                WidgetState(OKBtn, "normal")
                WidgetState(CurPosBtn, "normal")
                WidgetState(ExitBtn, "normal")

                EXIT <- TRUE
            } else {
                if (SingClick == 1){
                     WidgetState(ZMframe1, "normal")
                     WidgetState(OKBtn, "normal")
                     WidgetState(CurPosBtn, "normal")
                     WidgetState(ExitBtn, "normal")
                     EXIT <- TRUE
                } else if (ZOOM==TRUE) {
                     if (SingClick == 2) {
                         Corners <<- LocPos  #plot the zoom area the first time
                         if (FName[[SpectIndx]]@Flags[1]) { #Binding energy set
                             Corners$x <<- sort(Corners$x, decreasing=TRUE) #pos$x in decrescent ordered => Corners$x[1]==Corners$x[2]
                         } else {
                             Corners$x <<- sort(Corners$x, decreasing=FALSE) #pos$x in ascending order
                         }
                         Corners$x <<- c(Corners$x[1], Corners$x[1], Corners$x[2], Corners$x[2])
                         Corners$y <<- c(sort(c(Corners$y[1], Corners$y[2]), decreasing=FALSE),
                                         sort(c(Corners$y[1], Corners$y[2]), decreasing=FALSE))
                         SingClick <- FALSE
                         Nclk <- 1
                     } else {            #modify the zoom area
                         FindNearest()
                     }
                     XYrange$x <<- c(Corners$x[1], Corners$x[3])
                     XYrange$y <<- c(Corners$y[1], Corners$y[2])
                     ReDraw()  #refresh graph
                     rect(Corners$x[1], Corners$y[1], Corners$x[3], Corners$y[2])
                     points(Corners, type="p", pch=3, cex=1.5, col="blue", lwd=2)
                } else {
                     ReDraw() #refresh graph  to cancel previous cursor markers
                     points(LocPos, type="p", pch=3, cex=1.5, lwd=2, col="red")
                     LocPos <<- round(x=c(LocPos$x, LocPos$y), digits=2)
                     txt <- paste("X: ", as.character(LocPos[1]), ",   Y: ", as.character(LocPos[2]), sep="")
                     tkconfigure(ZMLbl3, text=txt)
                     tcl("update", "idletasks")  #force writing cursor position in the glabel
                }
            }
       }
  }


ReDraw <- function(){   #redraw all spectrum with no restrictions to RegionToFit
   SampData <- as.matrix(FName[[SpectIndx]]@.Data) #Whole coreline in SampData
   plot(x=SampData[[1]], y=SampData[[2]], xlim=XYrange$x, ylim=XYrange$y, type="l", lty="solid", lwd=1, col="black")
   SampData <- setAsMatrix(FName[[SpectIndx]], "matrix") # Regiontofit, Baseline, ecc in a matrix
   NC <- ncol(SampData)
   if (NC > 2) { #a Baseline is present
       BaseLine <- SampData[,3]
       matlines(x=SampData[,1], y=BaseLine, xlim=XYrange$x, ylim=XYrange$y, type="l", lty="solid", lwd=1, col="sienna")
   }
   if (NC > 3){ #c'e' un fit
       FitComp <- SampData[,4:NC-1]  #first three column skipped
       SpectFit <- SampData[,NC]  #fit
       matlines(x=SampData[,1], y=FitComp, xlim=XYrange$x, ylim=XYrange$y, type="l", lty="solid", lwd=1, col="blue")
       matlines(x=SampData[,1], y=SpectFit, xlim=XYrange$x, ylim=XYrange$y, type="l", lty="solid", lwd=1, col="red")
   }
}



FindNearest <- function(){
   D <- NULL
   Dmin <- ((LocPos$x-Corners$x[1])^2 + (LocPos$y-Corners$y[1])^2)^0.5  #init value
   for (ii in 1:4) {
       D[ii] <- ((LocPos$x-Corners$x[ii])^2 + (LocPos$y-Corners$y[ii])^2)^0.5  #dist P0 P1
       if(D[ii] <= Dmin){
          Dmin <- D[ii]
          idx=ii
       }
   }
   if (idx==1){
       Corners$x[1] <<- Corners$x[2] <<- LocPos$x
       Corners$y[1] <<- Corners$y[3] <<- LocPos$y
   } else if (idx==2){
       Corners$x[1] <<- Corners$x[2] <<- LocPos$x
       Corners$y[2] <<- Corners$y[4] <<- LocPos$y
   } else if (idx==3){
       Corners$x[3] <<- Corners$x[4] <<- LocPos$x
       Corners$y[1] <<- Corners$y[3] <<- LocPos$y
   } else if (idx==4){
       Corners$x[3] <<- Corners$x[4] <<- LocPos$x
       Corners$y[2] <<- Corners$y[4] <<- LocPos$y
   }
   return()
}



#----- Variabiles -----
   XPSSettings <- get("XPSSettings", envir=.GlobalEnv)
   WarnMsg <- XPSSettings$General[9]
   coord <- list()
   LocPos <- list()
   Corners <- list(x=NULL, y=NULL)
   ZOOM <- FALSE
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FNameList <- XPSFNameList() #list of XPSSamples
   FName <- get(activeFName,envir=.GlobalEnv)
   SpectList <- XPSSpectList(activeFName)
   SpectIndx <- get("activeSpectIndx",envir=.GlobalEnv)
   activeSpectName <- get("activeSpectName",envir=.GlobalEnv)
   if (SpectIndx > length(FName)) {
       SpectIndx <- 1 
       activeSpectName <- names(FName)[1]
   }
   XYrange <- list(x=range(FName[[SpectIndx]]@.Data[1]), y=range(FName[[SpectIndx]]@.Data[2]))
   if (FName[[SpectIndx]]@Flags[1]) {   #reverse if BE scale
      XYrange$x <- rev(XYrange$x)
   }
   ResetXYrange <- XYrange

#===== Graphic Library Cntrl ===
   MatPlotMode <- get("MatPlotMode", envir=.GlobalEnv)
   if (MatPlotMode==FALSE){
      tkmessageBox(message="Overlay or CustomPlot active: DoubleClick on your XPSsample", title = "BAD GRAPHIC MODE",  icon = "error")
      return()
   }


#--- Redraw is used because plot is limited to the RegionToFit

   ReDraw()

#--- Zoom Cursor window ---

   ZMwin <- tktoplevel()
   tkwm.title(ZMwin,"ZOOM/CURSOR OPTION")
   tkwm.geometry(ZMwin, "+100+50")   #position respect topleft screen corner
   ZMgroup <- ttkframe(ZMwin, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(ZMgroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   ZMframe0 <- ttklabelframe(ZMgroup, text = "XPS.Sample and Core.Line Selection", borderwidth=2)
   tkgrid(ZMframe0, row = 1, column = 1, padx = 5, pady = 5, sticky="we")
   XS <- tclVar(activeFName)
   XPS.Sample <- ttkcombobox(ZMframe0, width = 20, textvariable = XS, values = FNameList)
   tkbind(XPS.Sample, "<<ComboboxSelected>>", function(){
                     activeFName <<- tclvalue(XS)
                     FName <<- get(activeFName, envir=.GlobalEnv)
                     SpectList <<- XPSSpectList(activeFName)
                     tkconfigure(Core.Lines, values=SpectList)
                 })
   tkgrid(XPS.Sample, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   CL <- tclVar()
   Core.Lines <- ttkcombobox(ZMframe0, width = 20, textvariable = CL, values = SpectList)
   tkbind(Core.Lines, "<<ComboboxSelected>>", function(){
                     SpectName <- tclvalue(CL)
                     SpectName <- unlist(strsplit(SpectName, "\\."))   #drop the N. at beginning core-line name
                     SpectIndx <<- as.integer(SpectName[1])
                     XYrange <<- list(x=range(FName[[SpectIndx]]@.Data[1]), y=range(FName[[SpectIndx]]@.Data[2]))
                     if (length(FName[[SpectIndx]]@RegionToFit)>0) {   #reverse if BE scale
                        XYrange$x <<- range(FName[[SpectIndx]]@RegionToFit$x)
                        XYrange$y <<- range(FName[[SpectIndx]]@RegionToFit$y)
                     } else {
                        XYrange$x <<- range(FName[[SpectIndx]]@.Data[[1]])
                        XYrange$y <<- range(FName[[SpectIndx]]@.Data[[2]])
                     }
                     if (FName[[SpectIndx]]@Flags[1]) {   #reverse if BE scale
                        XYrange$x <<- rev(XYrange$x)
                     }
                     ResetXYrange <<- XYrange
                     ReDraw()
                 })
   tkgrid(Core.Lines, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   ZMframe1 <- ttklabelframe(ZMgroup, text = "Define the Zoom Area", borderwidth=2)
   tkgrid(ZMframe1, row = 2, column = 1, padx = 5, pady = 5, sticky="we")
   ZMareaBtn <- tkbutton(ZMframe1, text=" Set the Zoom Area ", width=22, command=function(){
                     txt <- " LEFT Mouse Button to Set the TWO Opposite Corners of the Zoom Area\n RIGHT Mouse Button to exit \n Click Near Markers to Modify The zoom area"
                     tkmessageBox(message=txt , title = "WARNING",  icon = "warning")
                     ZOOM <<- TRUE
                     GetCurPos(SingClick=2)
                     ZOOM <<- FALSE
                     plot(FName[[SpectIndx]], xlim=XYrange$x, ylim=XYrange$y) #zoom
                 })
   tkgrid(ZMareaBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   ZMresetBtn <- tkbutton(ZMframe1, text=" Reset Plot ", width=22, command=function(){
                     XYrange <<- ResetXYrange
                     ReDraw()
                 })
   tkgrid(ZMresetBtn, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   ZMframe2 <- ttklabelframe(ZMgroup, text = "Manual zoom values", borderwidth=2)
   tkgrid(ZMframe2, row = 3, column = 1, padx = 5, pady = 5, sticky="we")

   ZMlbl1 <- ttklabel(ZMframe2, text=" Exact Range Values: ")
   tkgrid(ZMlbl1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   X1 <- tclVar("Xmin=")  #sets the initial msg
   XMin <- ttkentry(ZMframe2, textvariable=X1, width=18, foreground="grey")
   tkbind(XMin, "<FocusIn>", function(K){
                          tclvalue(X1) <- ""
                          tkconfigure(XMin, foreground="red")
                      })
   tkbind(XMin, "<Key-Return>", function(K){
                          tkconfigure(XMin, foreground="black")
                      })
   tkgrid(XMin, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

   X2 <- tclVar("Xmax=")  #sets the initial msg
   XMax <- ttkentry(ZMframe2, textvariable=X2, width=18, foreground="grey")
   tkbind(XMax, "<FocusIn>", function(K){
                          tclvalue(X1) <- ""
                          tkconfigure(XMax, foreground="red")
                      })
   #now ttkentry waits for a return to read the entry_value
   tkbind(XMax, "<Key-Return>", function(K){
                          tkconfigure(XMax, foreground="black")
                      })
   tkgrid(XMax, row = 2, column = 2, padx = 5, pady = 5, sticky="w")

   Y1 <- tclVar("Ymin=")  #sets the initial msg
   YMin <- ttkentry(ZMframe2, textvariable=Y1, width=18, foreground="grey")
   tkbind(YMin, "<FocusIn>", function(K){
                          tclvalue(X1) <- ""
                          tkconfigure(YMin, foreground="red")
                      })
   #now ttkentry waits for a return to read the entry_value
   tkbind(YMin, "<Key-Return>", function(K){
                          tkconfigure(YMin, foreground="black")
                      })
   tkgrid(YMin, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

   Y2 <- tclVar("Ymax=")  #sets the initial msg
   YMax <- ttkentry(ZMframe2, textvariable=Y2, width=18, foreground="grey")
   tkbind(YMax, "<FocusIn>", function(K){
                          tclvalue(Y2) <- ""
                          tkconfigure(YMax, foreground="red")
                      })
   #now ttkentry waits for a return to read the entry_value
   tkbind(YMax, "<Key-Return>", function(K){
                          tkconfigure(YMax, foreground="black")
                      })
   tkgrid(YMax, row = 3, column = 2, padx = 5, pady = 5, sticky="w")

   OKBtn <- tkbutton(ZMframe2, text="  OK  ", width=22, command=function(){
                          x1 <- as.numeric(tclvalue(X1))
                          x2 <- as.numeric(tclvalue(X2))
                          y1 <- as.numeric(tclvalue(Y1))
                          y2 <- as.numeric(tclvalue(Y2))
                          if (FName[[SpectIndx]]@Flags) { #Binding energy set
                              XYrange$x <<- sort(c(x1, x2), decreasing=TRUE)
                              XYrange$y <<- sort(c(y1, y2))
                          } else {
                              XYrange$x <<- sort(c(x1, x2))
                              XYrange$y <<- sort(c(y1, y2))
                          }
                          ReDraw()
                      })
   tkgrid(OKBtn, row = 4, column = 1, padx = 5, pady = 5, sticky="we")

   ZMframe3 <- ttklabelframe(ZMgroup, text = "CURSOR POSITION", borderwidth=2)
   tkgrid(ZMframe3, row = 4, column = 1, padx = 5, pady = 5, sticky="we")
   CurPosBtn <- tkbutton(ZMframe3, text="  Cursor Position  ", width=22, command=function(){
                          if (WarnMsg == "ON") {
                              tkmessageBox(message="LEFT Mouse Button to Read Marker's Position; RIGHT Mouse Button to Exit" , title = "WARNING",  icon = "warning")
                          }
                          GetCurPos(SingClick=FALSE)
                          ReDraw()
                      })
   tkgrid(CurPosBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="we")

   ZMLbl3 <- ttklabel(ZMframe3, text = "Cursor position: ", font="Sans 12", borderwidth=2)
   tkgrid(ZMLbl3, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

   ExitBtn <- tkbutton(ZMgroup, text="  SAVE & EXIT  ", width=22, command=function(){
                          assign("activeFName", activeFName, envir=.GlobalEnv)
                          assign(activeFName, FName, envir=.GlobalEnv)
                          assign("activeSpectIndx", SpectIndx, envir=.GlobalEnv)
                          tkdestroy(ZMwin)
                          UpdateXS_Tbl()
                      })
   tkgrid(ExitBtn, row = 12, column = 1, padx = c(15,5), pady = 5, sticky="w")

}
