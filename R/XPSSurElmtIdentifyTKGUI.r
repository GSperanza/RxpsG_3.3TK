
#'@title XPSSurveyElementIdentify
#'@description function to Identify Elements present in a Survey spectrum.
#'@examples
#'\dontrun{
#'	XPSSurveyElementIdentify()
#'}
#'@export
#'                             


XPSSurveyElementIdentify <- function(){

replot <- function(show.points = FALSE, ...) {
	  plot(newcoreline, xlim = point.coords$x, ylim = point.coords$y)
	     if ( show.points ) {
		       points(point.coords, col=2, cex=2, lwd=1.5, pch=10)
      }
   }

reset.boundaries <- function(h, ...) {
	  newcoreline <<- XPSremove(newcoreline, "all") # reset all
   point.coords <<- IniBoundaries
	  slot(newcoreline,"Boundaries") <<- point.coords
	  newcoreline <<- XPSsetRegionToFit(newcoreline)
}


#---- Variables ----
   baseline.PKG <- get("baseline.PKG", envir=.GlobalEnv)
   if( baseline.PKG == FALSE ){
       txt <- "Pakage 'baseline' is NOT Installed. \nCannot Execute the 'Element Identification' option!"
       tkmessageBox(message= txt, title = "ERROR", icon="error")
       return()
   }
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
  
   Object <- get(activeFName, envir = .GlobalEnv)
   CurrentCoreLine <- activeSpectIndx
   nameCtrl <- Object[[CurrentCoreLine]]@Symbol
   if (nameCtrl != "Survey" && nameCtrl != "survey") {
      tkmessageBox(message="Please choose a survey spectrum" , title = "WARNING",  icon = "warning") #controlla che lo spettro sia un survey
      return()
   }
   if (is.numeric(CurrentCoreLine) ) {
   	   newcoreline <- Object[[CurrentCoreLine]] # duplicate CoreLine
   } else if ( is.character(CurrentCoreLine) ) {
   	   indx <- which(CurrentCoreLine == names(Object), arr.ind=TRUE)
   	   newcoreline <- Object[[indx]] # duplicate CoreLine
   }
   if (newcoreline@Flags[3]) {
       ftype <-"scienta" #scienta filetype
   } else {
       ftype <-"kratos"  #kratos filetype
   }
   point.coords <- list(x=range(newcoreline[1]), y=range(newcoreline[2]))
   IniBoundaries <- point.coords       #save the original boundaries
   coord <- NULL
   Peaks <- NULL
   IdPeaks <- NULL
   rangeY <- range(Object[[CurrentCoreLine]]@.Data[2])
   ElmtList <- ReadElmtList("CoreLines") #reads the CoreLine/Auger List Table
   ElmtList <- format(ElmtList, justify="centre", width=10)
   RecPlot <- recordPlot()   #save graph for Refresh
   CL_Auger <- c("CoreLines", "AugerTransitions")

#----- GUI -----
   MainWindow <- tktoplevel()
   tkwm.title(MainWindow,"ELEMENT IDENTIFICATION")
   tkwm.geometry(MainWindow, "+100+50")   #position respect topleft screen corner
   MainGroup <- ttkframe(MainWindow, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(MainGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

#--- Notebook
   NB <- ttknotebook(MainGroup)
   tkgrid(NB, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

#--- TAB1
   NBgroup1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
   tkadd(NB, NBgroup1, text=" Analyze Survey ")
   Frame11 <- ttklabelframe(NBgroup1, text = " Peak Detection ", borderwidth=2)
   tkgrid(Frame11, row = 1, column = 1, padx = 5, pady = 3, sticky="w")

   tkgrid( ttklabel(Frame11, text=" Noise Level: "),
          row = 1, column = 1, padx = 5, pady = 3, sticky="w")
   tkgrid( ttklabel(Frame11, text=" Energy Window: "),
          row = 1, column = 2, padx = 5, pady = 3, sticky="w")
   NL <- tclVar("7 ")  #sets the initial msg
   NoiseLevel <- ttkentry(Frame11, textvariable=NL, width=15, foreground="grey", takefocus=0)
   tkbind(NoiseLevel, "<FocusIn>", function(K){
                    tclvalue(NL) <- ""
                    tkconfigure(NoiseLevel, foreground="red")
          })
   tkbind(NoiseLevel, "<Key-Return>", function(K){
                    tkconfigure(NoiseLevel, foreground="black")
                    tkconfigure(StatusBar, text=paste("Noise Level: ",tclvalue(NL), sep=""))
          })
   tkgrid(NoiseLevel, row = 2, column = 1, padx = 5, pady = 3, sticky="w")

   EW <- tclVar("5 ")  #sets the initial msg
   EnergyWin <- ttkentry(Frame11, textvariable=EW, width=15, foreground="grey", takefocus=0)
   tkbind(EnergyWin, "<FocusIn>", function(K){
                    tclvalue(EW) <- ""
                    tkconfigure(EnergyWin, foreground="red")
          })
   tkbind(EnergyWin, "<Key-Return>", function(K){
                    tkconfigure(EnergyWin, foreground="black")
          })
   tkgrid(EnergyWin, row = 2, column = 2, padx = 5, pady = 3, sticky="w")


   DetectBtn <- tkbutton(Frame11, text=" DETECTION ", width=15, command=function(){
                    snmin <- as.numeric(tclvalue(NL))
                    Ewin <- as.numeric(tclvalue(EW))
                    if( baseline.PKG == FALSE ){      #the package 'baseline' is NOT installed
                        txt <- "Package 'baseline' is NOT Installed. \nCannot Execute 'Peak Detection' option"
                        tkmessageBox(message=txt, title="WARNING", icon="error")
                        return()
                    }
	                   Peaks <<- peakDetection(newcoreline, snmin, Ewin)
	                   tkconfigure(StatusBar, text="Element detection done.")
	                   plotPeaks(newcoreline, Peaks)
                    RecPlot <<- recordPlot()   #save graph for Refresh
          })
   tkgrid(DetectBtn, row = 3, column = 1, padx = 5, pady = 3, sticky="w")

   Frame12 <- ttklabelframe(NBgroup1, text = " Peak Identification ", borderwidth=2)
   tkgrid(Frame12, row = 2, column = 1, padx = 5, pady = 3, sticky="w")
   tkgrid( ttklabel(Frame12, text="  Precision (eV):  "),
          row = 1, column = 1, padx = 5, pady = 3, sticky="w")

   PE1 <- tclVar("1")  #sets the initial msg
   PrecEnergy <- ttkentry(Frame12, textvariable=PE1, width=15, foreground="grey", takefocus=0)
   tkbind(PrecEnergy, "<FocusIn>", function(K){
                          tclvalue(PE1) <- ""
                          tkconfigure(PrecEnergy, foreground="red")
          })
   tkbind(PrecEnergy, "<Key-Return>", function(K){
                    tkconfigure(PrecEnergy, foreground="black")
                    tkconfigure(StatusBar, text=paste("Precision : ",tclvalue(PE1), sep=""))
          })
   tkgrid(PrecEnergy, row = 2, column = 1, padx = 5, pady = 3, sticky="w")

   IdentifyBtn <- tkbutton(Frame12, text=" IDENTIFICATION ", width=15, command=function(){
                    DEnergy <- as.numeric(tclvalue(PE1))
                    IdPeaks <<- peakIdentification(Peaks, DEnergy, newcoreline, tclvalue(ST1), ElmtList)
                    ShowTablePeaks(IdPeaks, tclvalue(ST1))   #show table on the consolle
          })
   tkgrid(IdentifyBtn, row = 2, column = 2, padx = 5, pady = 3, sticky="w")

   Frame13 <- ttklabelframe(NBgroup1, text = " Spectral Type selection ", borderwidth=2)
   tkgrid(Frame13, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
   ST1 <- tclVar("CoreLines")
   for(ii in 1:2){
       SpectType1 <- ttkradiobutton(Frame13, text=CL_Auger[ii], variable=ST1, value=CL_Auger[ii],
                        command=function(){
                           SType <- tclvalue(ST1)
                           ElmtList <<- ReadElmtList(SType) #reads the CoreLine/Auger List Table
                           ElmtList <<- format(ElmtList, justify="centre", width=10)
                           tcl(HeaderList, "delete", "0", "end")
                           if (SType == "CoreLines"){
                               headers <- format(c("Element", "Orbital ", "   BE ", "   KE   ", "RSF_K   ", "RSF_S    "),
                                                   justify = "centre", width = 11) #let header as it is to be aligned with ElementTbl
                           } else {
                               headers <- format(c("Element", "Transition ", "   BE ", "   KE   ", "RSF_K   ", "RSF_S    "),
                                                    justify = "centre", width = 11) #let header as it is to be aligned with ElementTbl
                           }
                           tcl(HeaderList, "insert", "end", paste(headers, collapse = ""))

                           tcl(ElementTbl, "delete", "0", "end")  #clear the ElementTbl
                           LL <- length(ElmtList[[1]])
                           for (jj in 1:LL) {
                                row_data <- paste(ElmtList[jj, ], collapse="")
                                tcl(ElementTbl, "insert", "end", row_data)
                           }
                           tkconfigure(StatusBar, text=paste("Selected ",tclvalue(ST1), " Table", sep=""))
                           tclvalue(ST1) <- SType
                           tclvalue(ST3) <- SType
                           replot()
                     })
       tkgrid(SpectType1, row = 1, column = ii, padx = 5, pady = 5, sticky="w")
   }

   Frame14 <- ttklabelframe(NBgroup1, text = " Plot ", borderwidth=2)
   tkgrid(Frame14, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
   PT <- tclVar("normal")
   Ptype <- c("normal", "corrected")
   for(ii in 1:2){
       plotType <- ttkradiobutton(Frame14, text=Ptype[ii], variable=PT, value=Ptype[ii],
                        command=function(){
                           SType <- tclvalue(ST1)
		                         if (is.null(Peaks) == FALSE) {
		   	                         plotPeaks(newcoreline, Peaks, type = tclvalue(PT))
       		                  }
                     })
       tkgrid(plotType, row = 2, column = ii, padx = 5, pady = 5, sticky="w")
   }


#--- TAB2
   NBgroup2 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
   tkadd(NB, NBgroup2, text=" Single peak ")
   Frame21 <- ttklabelframe(NBgroup2, text = " Peak Position ", borderwidth=2)
   tkgrid(Frame21, row = 1, column = 1, padx = 5, pady = 3, sticky="w")

   tkgrid( ttklabel(Frame21, text=" Locate Peak: "),
           row = 1, column = 1, padx = 5, pady = 3, sticky="w")

   PositionBtn <- tkbutton(Frame21, text=" Press to Locate ", command=function(){
                    coord <<- locator(n=1, type="p", col="red", lwd=2)
                    tclvalue(EE) <- as.character(round(as.numeric(coord$x),digits=2))
                    WidgetState(Frame23, "normal")
          })
   tkgrid(PositionBtn, row = 1, column = 2, padx = 5, pady = 3, sticky="w")

   tkgrid( ttklabel(Frame21, text=" Energy value: "),
           row = 2, column = 1, padx = 5, pady = 3, sticky="w")
   EE <- tclVar("Energy")  #sets the initial msg
   EnterE <- ttkentry(Frame21, textvariable=EE, foreground="grey", takefocus=0)
   tkbind(EnterE, "<FocusIn>", function(K){
                    tclvalue(EE) <- ""
                    tkconfigure(EnterE, foreground="red")
          })
   tkbind(EnterE, "<Key-Return>", function(K){
                    tkconfigure(EnterE, foreground="black")
                    Energy <- as.numeric(tclvalue(EE))
                    Precision <- as.numeric(tclvalue(PE2))
                    if (Energy > 0 && Precision > 0){
                        WidgetState(Frame23, "normal")
                    }
                    replayPlot(RecPlot)
          })
   tkgrid(EnterE, row = 2, column = 2, padx = 5, pady = 3, sticky="w")

   tkgrid( ttklabel(Frame21, text=" Precision (eV): "),
           row = 3, column = 1, padx = 5, pady = 3, sticky="w")
   PE2 <- tclVar("2")  #sets the initial msg
   E_win <- ttkentry(Frame21, textvariable=PE2, foreground="grey", takefocus=0)
   tkbind(E_win, "<FocusIn>", function(K){
                    tclvalue(PE2) <- ""
                    tkconfigure(E_win, foreground="red")
          })
   tkbind(E_win, "<Key-Return>", function(K){
                    tkconfigure(E_win, foreground="black")
                    Energy <- as.numeric(tclvalue(EE))
                    Precision <- as.numeric(tclvalue(PE2))
                    if (Energy > 0 && Precision > 0){
                        WidgetState(Frame23, "normal")
                    }
                    replayPlot(RecPlot)
          })
   tkgrid(E_win, row = 3, column = 2, padx = 5, pady = 3, sticky="w")

   Frame22 <- ttklabelframe(NBgroup2, text = " Spectrum Type Selection ", borderwidth=2)
   tkgrid(Frame22, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
   ST2 <- tclVar("CoreLines")
   for(ii in 1:2){
       SpectType2 <- ttkradiobutton(Frame22, text=CL_Auger[ii], variable=ST2, value=CL_Auger[ii],
                        command=function(){
                           SType <- tclvalue(ST2)
                           ElmtList <<- ReadElmtList(SType) #reads the CoreLine/Auger List Table
                           ElmtList <<- format(ElmtList, justify="centre", width=10)
                           tcl(ElementTbl, "delete", "0.0", "end")  #clear the ElementTbl
                           LL <- length(ElmtList[[ii]])
                           for (ii in 1:LL) {
                                row_data <- paste(ElmtList[ii, ], collapse="")
                                tcl(ElementTbl, "insert", "end", row_data)
                           }
                           tclvalue(ST2) <- SType
                           tclvalue(ST3) <- SType
                           replot()
                     })
       tkgrid(SpectType2, row = 1, column = ii, padx = 5, pady = 5, sticky="w")
   }

   Frame23 <- ttklabelframe(NBgroup2, text = " Core Line Search Mode ", borderwidth=2)
   tkgrid(Frame23, row = 3, column = 1, padx = 5, pady = 3, sticky="w")

   NearerBtn <- tkbutton(Frame23, text=" Find Elements Nearer to the Selected Energy ", width=50, command=function(){
                    replot()
                    energy <- as.numeric(tclvalue(EE))
                    precision <- as.numeric(tclvalue(PE2))
		                  if (is.na(energy) == FALSE && is.na(precision) == FALSE) {
			                     IdPeaks <<- NearerElement(energy, precision, newcoreline, ElmtList)
			                     if (length(IdPeaks) > 0){
   		                       ShowTablePeaks(IdPeaks, tclvalue(ST2))
                        } else {
                            cat("\n No element found with the selected precision!")
                        }
		                  }
          })
   tkgrid(NearerBtn, row = 1, column = 1, padx = 5, pady = 3, sticky="w")

   MaxRSFBtn <- tkbutton(Frame23, text=" Core Lines with Max RSF in the Selected Energy Range ", width=50, command=function(){
                    replot()
                    energy <- as.numeric(tclvalue(EE))
                    precision <- as.numeric(tclvalue(PE2))
		                  if (is.na(energy) == FALSE && is.na(precision) == FALSE) {
			                     IdPeaks <<- CoreLinesMaxRSF(energy, precision, newcoreline, ElmtList)
			                     if (length(IdPeaks) > 0){
   		                      ShowTablePeaks(IdPeaks, tclvalue(ST2))
                        } else {
                           cat("\n No element found with the selected precision!")
                        }
		                   }
          })
   tkgrid(MaxRSFBtn, row = 2, column = 1, padx = 5, pady = 3, sticky="w")

   OrbitalBtn <- tkbutton(Frame23, text=" Find Any Element-Orbital in the Selected Energy Range ", width=50, command=function(){
                    replot()
                    energy <- as.numeric(tclvalue(EE))
                    precision <- as.numeric(tclvalue(PE2))
		                  if (is.na(energy) == FALSE && is.na(precision) == FALSE) {
			                     IdPeaks <<- AllElements(energy, precision, newcoreline, ElmtList)
			                     if (length(IdPeaks) > 0){
   		                      ShowTablePeaks(IdPeaks, tclvalue(ST2))
                        } else {
                           cat("\n No element found with the selected precision!")
                        }
                    }
          })
   tkgrid(OrbitalBtn, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

   WidgetState(Frame23, "disabled")

#--- TAB3
   NBgroup3 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
   tkadd(NB, NBgroup3, text=" Peak Table ")
   Frame31 <- ttklabelframe(NBgroup3, text = " Peak Table ", borderwidth=2)
   tkgrid(Frame31, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

# Insert headers
   headers <- format(c("Element", "Orbital ", "   BE ", "   KE   ", "RSF_K   ", "RSF_S    "),
                  justify = "centre", width = 11) #let header as it is to be aligned with ElementTbl
   HeaderList <- tklistbox(Frame31, selectmode = "single", height=1, font="Courier 10 bold",
                            width = 65, background="#E0E0E0",borderwidth=0)
   tkgrid(HeaderList, row = 1, column = 1, padx = 5, pady = 0, sticky="w")
   tcl(HeaderList, "insert", "end", paste(headers, collapse = ""))
# Element Table
   ElementTbl <- tklistbox(parent=Frame31, selectmode = "single", height=13, 
                           font="Courier 10 normal", width = 65, borderwidth=0)
   tkgrid(ElementTbl, row = 2, column = 1, padx = 5, pady =c(0,5), sticky="w")
   LL <- length(ElmtList[[1]])
# Insert data rows
   for (ii in 1:LL) {
        row_data <- paste(ElmtList[ii, ], collapse="")
        tcl(ElementTbl, "insert", "end", row_data)
   }
   tkbind(ElementTbl, "<Double-1>", function() {
                    elmt <- as.numeric(tclvalue(tcl(ElementTbl, "curselection")))+1
                    elmt <- ElmtList[elmt,1] #get the selected element symbol
                    elmt <- trimws(elmt, which="left") #remove white spaces at beginning
                    tclvalue(ELMT) <- elmt
                    idx <- grep(elmt, ElmtList[,1])
                    elmtEE <- NULL
                    if (tclvalue(HP) == "0"){
                       replot()    #refresh plot
                    }
                    barCol <- "red"
                    if (tclvalue(ST3) == "AugerTransitions") barCol <- "blue3"
                    for (ii in seq_along(idx)){
                        xx <- ElmtList[idx[ii],3]
                        lines(x=c(xx, xx), y=rangeY, col=barCol)   #plot corelines of the selected elements
                    }
          })
   tkgrid.columnconfigure(Frame31, 1, weight=2)
   addScrollbars(Frame31, ElementTbl, type="y", Row = 2, Col = 1, Px=0, Py=0)


   Frame32 <- ttklabelframe(NBgroup3, text = " Spectral Type Selection ", borderwidth=2)
   tkgrid(Frame32, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

   ST3 <- tclVar("CoreLines")
   for(ii in 1:2){
       SpectType3 <- ttkradiobutton(Frame32, text=CL_Auger[ii], variable=ST3, value=CL_Auger[ii],
                        command=function(){
                           SType <- tclvalue(ST3)
                           ElmtList <<- ReadElmtList(SType) #reads the CoreLine/Auger List Table
                           ElmtList <<- format(ElmtList, justify="centre", width=10)
                           tcl(HeaderList, "delete", "0", "end")
                           if (SType == "CoreLines"){
                               headers <- format(c("Element", "Orbital ", "   BE ", "   KE   ", "RSF_K   ", "RSF_S    "),
                                                   justify = "centre", width = 11) #let header as it is to be aligned with ElementTbl
                           } else {
                               headers <- format(c("Element", "Transition ", "   BE ", "   KE   ", "RSF_K   ", "RSF_S    "),
                                                    justify = "centre", width = 11) #let header as it is to be aligned with ElementTbl
                           }
                           tcl(HeaderList, "insert", "end", paste(headers, collapse = ""))

                           tcl(ElementTbl, "delete", "0", "end")  #clear the ElementTbl
                           LL <- length(ElmtList[[1]])
                           for (jj in 1:LL) {
                                row_data <- paste(ElmtList[jj, ], collapse="")
                                tcl(ElementTbl, "insert", "end", row_data)
                           }
                           tclvalue(ST1) <- SType
                           tclvalue(ST2) <- SType
                           replot()
                     })
       tkgrid(SpectType3, row = 1, column = ii, padx = 5, pady = 5, sticky="w")
   }

   ELMT <- tclVar("Element? ")  #sets the initial msg
   Search <- ttkentry(Frame32, textvariable=ELMT, foreground="grey", takefocus=0)
   tkbind(Search, "<FocusIn>", function(K){
                    tclvalue(ELMT) <- ""
                    tkconfigure(Search, foreground="red")
          })
   tkbind(Search, "<Key-Return>", function(K){
                    tkconfigure(Search, foreground="black")
                    elmt <- tclvalue(ELMT)
                    SType <- tclvalue(ST3)
                    elmt <- paste(elmt, " ", sep="")
                    idx <- grep(elmt, ElmtList[,1])
                    if (tclvalue(HP) == "0"){
                       replot()    #refresh plot
                    }
                    if(SType == "CoreLines") {
                       color="red"
                    } else if(SType == "AugerTransitions"){
                       color="blue"
                    }
                    for (ii in seq_along(idx)){
                        xx <- ElmtList[idx[ii],3]
                        lines(x=c(xx, xx), y=rangeY, col=color)   #plot corelines of the selected elements
                    }
                    replayPlot(RecPlot)
          })
   tkgrid(Search, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

   HP <- tclVar("0")
   HoldPlot <- tkcheckbutton(Frame32, text="Hold plot", variable=HP, onvalue = 1, offvalue = 0)
   tkgrid(HoldPlot, row = 1, column = 4, padx = 5, pady = 5, sticky="w")

#--- COMMON
   CommFrame <- ttklabelframe(MainGroup, text = " Cursor and Zoom ", borderwidth=2)
   tkgrid(CommFrame, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

   SetPosBtn <- tkbutton(CommFrame, text=" Cursor Position ", width=18, command=function(){
                    txt <- "Left Mouse Button to Get Position, Right Button to Exit"
                    tkmessageBox(message=txt,  title="WARNING", icon="warning")
                    pos <- locator(n=1, type="p", pch=3, cex=1.5, lwd=1.5, col="blue") #to modify zoom limits
                    if (length(pos) > 0) { #non ho premuto tasto DX
                        replot()
                        points(pos, type="p", pch=3, cex=1.5, lwd=1.8, col="red")
                        pos <- round(x=c(pos$x, pos$y), digits=2)
                        txt <- paste("X: ", as.character(pos[1]), ", Y: ", as.character(pos[2]), sep="")
                        tkconfigure(ShowPos, text=txt)
                        tcl("update", "idletasks")  #force text to be shown in the glabel ShowPos
                    }

          })
   tkgrid(SetPosBtn, row = 1, column = 1, padx = 5, pady = 3, sticky="w")

   ShowPos <- ttklabel(CommFrame, text=" X, Y:")
   tkgrid(ShowPos, row = 1, column = 2, padx = 5, pady = 3, sticky="w")

   BoundaryBtn <- tkbutton(CommFrame, text=" Set Zoom Boundaries ", width=18, command=function(){
	                   replot()
                    point.coords <<- locator(n=2, type="p", pch=3, cex=1.5, lwd=1.5, col="blue")
                    DE <- abs(newcoreline[[1]][1]-newcoreline[[1]][2]) #Get the energy scale step: newcoreline[[1]] == newcoreline$x
	                   idx1 <- which(abs(newcoreline[[1]]-point.coords$x[1]) < DE) #get the element index corresponding to coords[1]
	                   idx1 <- idx1[1] #in idx there could be more than one value: select the first
	                   idx2 <- which(abs(newcoreline[[1]]-point.coords$x[2]) < DE) #get the element index corresponding to coords[2]
	                   idx2 <- idx2[1]
	                   point.coords$y <<- c(min(newcoreline[[2]][idx1:idx2]), max(newcoreline[[2]][idx1:idx2]))
                    slot(newcoreline,"Boundaries") <<- point.coords
	                   newcoreline <<- XPSsetRegionToFit(newcoreline)
		                  replot()
          })
   tkgrid(BoundaryBtn, row = 2, column = 1, padx = 5, pady = 3, sticky="w")

   ResetBoundBtn <- tkbutton(CommFrame, text=" Reset Boundaries ", width=16, command=function(){
  		              reset.boundaries()
	                 replot()
          })
   tkgrid(ResetBoundBtn, row = 2, column = 2, padx = 5, pady = 3, sticky="w")

   RefreshBtn <- tkbutton(CommFrame, text=" Refresh Plot ", width=16, command=function(){
                  IdPeaks <<- NULL
                  replayPlot(RecPlot)
          })
   tkgrid(RefreshBtn, row = 2, column = 3, padx = 5, pady = 3, sticky="w")

   ResetAnalBtn <- tkbutton(CommFrame, text=" Reset Analysis ", width=16, command=function(){
                  Peaks <<- NULL
                  IdPeaks <<- NULL
                  replot()
                  RecPlot <<- recordPlot()   #save graph for Refresh
          })
   tkgrid(ResetAnalBtn, row = 2, column = 4, padx = 5, pady = 3, sticky="w")


   ExitBtn <- tkbutton(MainGroup, text=" EXIT ", width=20, command=function(){
                  tkdestroy(MainWindow)
                  replot()
          })
   tkgrid(ExitBtn, row = 3, column = 1, padx = 5, pady = c(5, 10), sticky="w")

   tkSep <- ttkseparator(MainGroup, orient="horizontal")
   tkgrid(tkSep, row = 4, column = 1, padx = 5, pady = 10, sticky="we")
   StatusBar <- ttklabel(MainGroup, text=" Status : ", relief="sunken", foreground="blue3")
   tkgrid(StatusBar, row = 5, column = 1, padx = 5, pady = 5, sticky="we")

   plot(newcoreline)
}
