#-----------------------------------------
# XPS processing with gWidgets2 and tcltk
#-----------------------------------------
#' @title XPSExtract extract a portion of spectrum from a XPS survey
#'
#' @description XPSExtract function extracts spectral features from
#'   a XPS survey in a XPSSample. Mouse is used to identify the portion
#'   of the spectrum to extract. The user is asked to assign a name
#'   (i.e. Cl2p, Li1s, Br3d...) to the extracted spectrum representing
#'   a Core-Line associated to an element. Coherently with the
#'   name, a RSF will be automatically assigned to that spectrum.
#' @return XPSExtract returns the extracted spectrum, and the original 
#'   XPSSample will show an additional coreline
#' @examples
#' \dontrun{
#'  XPSextractGUI()
#' }
#' @export
#'

XPSExtract <- function() {

  GetCurPos <- function(SingClick){
       WidgetState(OptFrame, "disabled")   #prevent exiting Analysis if locatore active
       WidgetState(PlotFrame, "disabled")
       WidgetState(SaveFrame, "disabled")
       EXIT <- FALSE
       while(EXIT == FALSE){
            pos <- locator(n=1)
            if (is.null(pos)) {
                WidgetState(OptFrame, "normal")   #prevent exiting Analysis if locatore active
                WidgetState(PlotFrame, "normal")
                WidgetState(SaveFrame, "normal")
                EXIT <- TRUE
            } else {
                if ( SingClick ){
                    coords <<- c(pos$x, pos$y)
                    WidgetState(OptFrame, "normal")   #prevent exiting Analysis if locatore active
                    WidgetState(PlotFrame, "normal")
                    WidgetState(SaveFrame, "normal")
                    EXIT <- TRUE
                } else {
                    Xlim1 <- min(range(Object@.Data[[1]]))   #limits coordinates in the Spectrum Range
                    Xlim2 <- max(range(Object@.Data[[1]]))
                    Ylim1 <- 0.95*min(range(Object@.Data[[2]]))
                    Ylim2 <- 1.05*max(range(Object@.Data[[2]]))

                    if (pos$x < Xlim1 ) {pos$x <- Xlim1}
                    if (pos$x > Xlim2 ) {pos$x <- Xlim2}
                    if (pos$y < Ylim1 ) {pos$y <- Ylim1}
                    if (pos$y > Ylim2 ) {pos$y <- Ylim2}
                    coords <<- c(pos$x, pos$y)
                    LBmousedown()  #selection of the BaseLine Edges
                }
            }
       }
       return()
  }

  LBmousedown <- function() {
      point.coords$x[point.index] <<- coords[1]   #abscissa
      point.coords$y[point.index] <<- coords[2]   #ordinate
      if (point.index == 1) {
         point.index <<- 2    #to modify the second edge of the selected area
         Corners$x<<-c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
         Corners$y<<-c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
      } else if (point.index == 2) {
         Corners$x<<-c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
         Corners$y<<-c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
         point.index<<-3
      } else if (point.index == 3) {
         D<-vector("numeric", 4)
         Dmin<-((point.coords$x[3]-Corners$x[1])^2 + (point.coords$y[3]-Corners$y[1])^2)^0.5  #initialization value
         for (ii in 1:4) {
             D[ii]<-((point.coords$x[3]-Corners$x[ii])^2 + (point.coords$y[3]-Corners$y[ii])^2)^0.5  #distance P0 - P1
             if(D[ii] <= Dmin){
                Dmin<-D[ii]
                idx=ii
             }
         }
         if (idx == 1){
            Corners$x[1]<<-Corners$x[2]<<-point.coords$x[3]
            Corners$y[1]<<-Corners$y[3]<<-point.coords$y[3]
         } else if (idx == 2){
             Corners$x[1]<<-Corners$x[2]<<-point.coords$x[3]
             Corners$y[2]<<-Corners$y[4]<<-point.coords$y[3]
         } else if (idx == 3){
             Corners$x[3]<<-Corners$x[4]<<-point.coords$x[3]
             Corners$y[1]<<-Corners$y[3]<<-point.coords$y[3]
         } else if (idx == 4){
            Corners$x[3]<<-Corners$x[4]<<-point.coords$x[3]
            Corners$y[2]<<-Corners$y[4]<<-point.coords$y[3]
         }
         if (Object@Flags[1]) { #Binding energy set
            point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE)
            point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
         } else {
            point.coords$x<<-sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE)
            point.coords$y<<-sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
         }
      }
      replot()
  }


  undo.plot <- function(...){
      if (SelReg == 1) {
         reset.boundaries()
         replot()
      } else if (SelReg > 1) {
         Object@Boundaries$x <<- OldCoords$x
         Ylimits <<- OldCoords$y
         replot()
      }
  }


  replot <- function(...) {
      if (point.index == 0) {   #Extract active
          plot(Object, xlim=Xlimits, ylim=Ylimits)
      } else if (point.index == 1 || point.index == 2) { #generic plot
          plot(Object, xlim=Xlimits)
          points(point.coords, col="blue", cex=1.5, lwd=2, pch=3)
      } else if (point.index > 2){   #plot spectrum and frame for region selection
          plot(Object, xlim=Xlimits, ylim=Ylimits)
          points(Corners, type="p", col="blue", cex=1.5, lwd=2, pch=3)
          rect(point.coords$x[1], point.coords$y[1], point.coords$x[2], point.coords$y[2])
      }
  }

  reset.boundaries <- function(h, ...) {
     Object <<- XPSSample[[oldcoreline]]   #switch to the initially selected coreline
     Object <<- XPSremove(Object, "all")
     LL <- length(Object@.Data[[1]])
     point.coords$x[1] <<- Object@.Data[[1]][1]  #abscissa of the first survey edge
     point.coords$y[1] <<- Object@.Data[[2]][1]  #ordinate of the first survey edge
     point.coords$x[2] <<- Object@.Data[[1]][LL] #abscissa of the second survey edge
     point.coords$y[2] <<- Object@.Data[[2]][LL] #ordinate of the second survey edge
     slot(Object,"Boundaries") <<- point.coords
     Xlimits <<- c(min(Object@.Data[[1]]), max(Object@.Data[[1]]))
     Ylimits <<- c(min(Object@.Data[[2]]), max(Object@.Data[[2]]))
     OldCoords <<- point.coords #for undo
     Corners <<- point.coords
     point.index <<- 1
     replot()
  }

  Extract <- function(h, ...){
         Object@Boundaries <<- point.coords
         Xlimits <<- Object@Boundaries$x   #visualize selected region
         Ylimits <<- sort(Object@Boundaries$y, decreasing=FALSE)
         point.index <<- 0
         Object@Symbol <- " "
         replot()

         txt <- paste("Elem.Name \U21B2", sep="")  #/U000D = carriage return symbol (ENTER)
         ELMNT <- tclVar(txt)  #sets the initial msg
         EnterElmnt <- ttkentry(ElementFrame, textvariable=ELMNT, width=12, foreground="grey")
         tkgrid(EnterElmnt, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
         tkbind(EnterElmnt, "<FocusIn>", function(K){
                         tclvalue(ELMNT) <- ""
                         tkconfigure(EnterElmnt, foreground="red")
                     })
         tkbind(EnterElmnt, "<Key-Return>", function(K){
                         tkconfigure(EnterElmnt, foreground="black")
                         Element <<- tclvalue(ELMNT)
                         Element <<- gsub(" ", "", Element)   #eliminates white spaces from Symbol
                         IsElmnt <- ElementCheck(Element) #see XPSElement()
                         if (IsElmnt == FALSE){
                             txt <- paste( " ATTENTION: Element ", Element, " NOT Found in the Periodic Table \n Proceed anyway?", sep="")
                             yesno <- tkmessageBox(message=txt, type="yesno", title="WARNING", icon="warning")
                             if (tclvalue(yesno) =="No"){
                                 return()
                             }
                         }
                     })

         txt <- paste("Orbital \U21B2", sep="")  #/U000D = carriage return symbol (ENTER)
         ORB <- tclVar(txt)  #sets the initial msg
         EnterOrb <- ttkentry(ElementFrame, textvariable=ORB, width=12, foreground="grey")
         tkgrid(EnterOrb, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
         tkbind(EnterOrb, "<FocusIn>", function(K){
                         tclvalue(ORB) <- ""
                         tkconfigure(EnterOrb, foreground="red")
                     })
         tkbind(EnterOrb, "<Key-Return>", function(K){
                         tkconfigure(EnterOrb, foreground="black")
                         Orbital <<- tclvalue(ORB)
                         Orbital <<- gsub(" ", "", Orbital)   #eliminates white spaces from Symbol
                         OrbList <- c("1s", "2s", "3s", "4s", "5s", "6s", "2p", "3p", "4p", "5p", "6p", "3d", "4d", "5d", "4f")
                         IsOrbital <- ifelse(Orbital %in% OrbList, TRUE, FALSE)
                         if (IsOrbital == FALSE){
                             txt <- paste( " ATTENTION: Wrong Orbital: ", Orbital, "\n Proceed anyway?", sep="")
                             yesno <- tkmessageBox(message=txt, type="yesno", title="WARNING", icon="warning")
                             if (tclvalue(yesno) =="No"){
                                 return()
                             }
                         }
                     })

         OKBtn <- tkbutton(ElementFrame, text="  OK  ", width=10, command=function(){
                         if (length(Element) == 0) {
                             tkmessageBox(message="Enter the CoreLine Name Please", title="ERROR", icon ="error")
                             return()
                         }
                         tkdestroy(EnterElmnt)
                         tkdestroy(EnterOrb)
                         tkdestroy(OKBtn)
                         if (length(Orbital) == 0) { Orbital <<- "" }
                         Symbol <- paste(Element, Orbital, sep="") #biold the exact CoreLine name
                         newcoreline <- Object   #creates a new coreline
                         Xmax <- max(range(newcoreline@.Data[1]))
                         Xmin <- min(range(newcoreline@.Data[1]))
                         #is selected region out of limits?
                         if (point.coords$x[1] > Xmax) {point.coords$x[1] <<- Xmax}
                         if (point.coords$x[1] < Xmin) {point.coords$x[1] <<- Xmin}
                         if (point.coords$x[2] > Xmax) {point.coords$x[2] <<- Xmax}
                         if (point.coords$x[2] < Xmin) {point.coords$x[2] <<- Xmin}

                         idx1 <- findXIndex(unlist(newcoreline@.Data[1]), point.coords$x[1]) #index corresponding to the selected BE1 (or KE1 value) of RegionToFit
                         idx2 <- findXIndex(unlist(newcoreline@.Data[1]), point.coords$x[2]) #index corresponding to the selected BE2 (or KE2 value) of RegionToFit
                         tmp <- unlist(Object@.Data[1])  #extract correspondent X values for the selected region
                         newcoreline@.Data[[1]] <- tmp[idx1:idx2]    #save the X values in the new coreline
                         OldEnergyScale <<- newcoreline@.Data[[1]]
                         newcoreline@Boundaries$x <- c(tmp[idx1], tmp[idx2])
                         tmp <- unlist(Object@.Data[2])  #extract correspondent Y values for the selected region
                         newcoreline@.Data[[2]] <- tmp[idx1:idx2]    #save the Y values in the new coreline
                         newcoreline@Boundaries$y <- range(tmp)
                         tmp <- unlist(Object@.Data[3])  #extract correspondent transmission Factor values for the selected region
                         newcoreline@.Data[[3]] <- tmp[idx1:idx2]    #save the transmission Factor values in the new coreline
                         newcoreline@Symbol <- Symbol
                         ## add extracted coreline to original XPSSample
                         idx <- length(XPSSample) + 1
                         XPSSample[[idx]] <<- newcoreline
                         names(XPSSample) <<- unname(sapply(XPSSample, slot, "Symbol"))
                         LL <- length(Object@.Data[[1]])
                         point.coords$x[1] <<- Object@.Data[[1]][1]
                         point.coords$y[1] <<- Object@.Data[[2]][1]
                         point.coords$x[2] <<- Object@.Data[[1]][LL]
                         point.coords$y[2] <<- Object@.Data[[2]][LL]
                         Object <<- XPSSample[[idx]]
                         coreline <<- idx
                         assign("activeSpectIndx", idx, envir=.GlobalEnv)
                         assign("activeSpectName", Symbol, envir=.GlobalEnv)
                         plot(XPSSample)
                     })
         tkgrid(OKBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

  }


  ResetVars <- function(){
         point.index <<- 1
         coords <<- NA # for printing mouse coordinates on the plot
         xx <<- NULL
         yy <<- NULL
         NO.Fit <<- FALSE
         Object <<- NULL

#Coreline boundaries
         point.coords$x <<- NULL
         point.coords$y <<- NULL
         Xlimits <<- NULL
         Ylimits <<- NULL
         Corners <<- point.coords
         OldCoords <<- point.coords #for undo
         OldEnergyScale <<- NULL
         OldFlag <<- NULL
         OldUnits <<- NULL
         SelReg <<- 0
         coreline <<- oldcoreline
         Element <<- NULL
         Orbital <<- NULL
         Eidx <<- NA

         WinSize <<- as.numeric(XPSSettings$General[4])
  }

#----- Variables -----
  XPSSample <- NULL
  coreline <- NULL
  oldcoreline <- coreline
  Object <- NULL
  point.coords <- list(x=NA,y=NA)
  point.index <- 1
  coords <- NA # for printing mouse coordinates on the plot
  xx <- NULL
  yy <- NULL
  NO.Fit <- FALSE
  Element <- NULL
  Orbital <- NULL

#Coreline boundaries
  point.coords$x <- NULL
  point.coords$y <- NULL
  Xlimits <- NULL
  Ylimits <- NULL
  Corners <- NULL

  OldCoords <- point.coords #for undo
  OldEnergyScale <- NULL
  OldFlag <- NULL
  OldUnits <- NULL
  SelReg <- 0
  FNameList <- XPSFNameList()
  SpectList <- XPSSpectList(activeFName)
  Eidx <- NA

  WinSize <- as.numeric(XPSSettings$General[4])
  WinScale  <- NULL
  cat("\n Please select the portion of the Survey containing the CoreLine to extract. \n")
  plot.new()

#----- Widget definition
  Ewindow <- tktoplevel()
  tkwm.title(Ewindow,"XPS EXTRACT GUI")
  tkwm.geometry(Ewindow, "+100+50")

  Egroup1 <- ttkframe(Ewindow, borderwidth=0, padding=c(0,0,0,0) )
  tkgrid(Egroup1, row=1, column=1, padx = 0, pady = 0, sticky="w")

  ## XPSSample and Core lines
  Eframe1 <- ttklabelframe(Egroup1, text="XPS Sample and Core line Selection", borderwidth=2, padding=c(5,5,5,5) )
  tkgrid(Eframe1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  XS <- tclVar()
  XPS.Sample <- ttkcombobox(Eframe1, width = 15, textvariable = XS, values = FNameList)
  tkgrid(XPS.Sample, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
  tkbind(XPS.Sample, "<<ComboboxSelected>>", function(){
                        activeFName <<- tclvalue(XS)
                        XPSSample <<- get(activeFName, envir=.GlobalEnv)
                        SpectList <<- XPSSpectList(activeFName)
                        indx <- grep("Survey", SpectList, value=FALSE)
                        Object <<- XPSSample[[indx[1]]]
                        ResetVars()
                        tkconfigure(Core.Lines, values=SpectList)
                        plot(XPSSample)
                 })

  CL <- tclVar()
  Core.Lines <- ttkcombobox(Eframe1, width = 15, textvariable = CL, values = SpectList)
  tkgrid(Core.Lines, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
  tkbind(Core.Lines, "<<ComboboxSelected>>", function(){
                        CLine <- tclvalue(CL)
                        CLine <- unlist(strsplit(CLine, "\\."))   #"number." and "CL name" are separated
                        if (CLine[2] != "survey" && CLine[2] != "Survey"){
                            yesno <- tkmessageBox(message=" ATTENTION: This is NOT a Survey spectrum! \n Proceed anyway?", type="yesno", icon="warning")
                            if (tclvalue(yesno) == "No"){
                                return()
                            }
                        }
                        coreline <<- as.integer(CLine[1])   # Coreline index
                        oldcoreline <<- coreline
                        Object <<- XPSSample[[coreline]]
                        LL <- length(Object@.Data[[1]])
                        point.coords$x[1] <<- Object@.Data[[1]][1]
                        point.coords$y[1] <<- Object@.Data[[2]][1]
                        point.coords$x[2] <<- Object@.Data[[1]][LL]
                        point.coords$y[2] <<- Object@.Data[[2]][LL]
                        Xlimits <<- c(min(Object@.Data[[1]]), max(Object@.Data[[1]]))
                        Ylimits <<- c(min(Object@.Data[[2]]), max(Object@.Data[[2]]))
                        Corners <<- point.coords
                        Object@Boundaries$x <<- c(point.coords$x)
                        Object@Boundaries$y <<- c(point.coords$y)
                        OldCoords <<- point.coords #for undo
                        OldEnergyScale <<- Object@.Data[[1]]
                        OldFlag <<- Object@Flags[1]
                        OldUnits <<- Object@units[1]
                        SelReg <<- 0
                        if ( OldFlag == TRUE ) {Eidx <<- 1 }
                        if ( OldFlag == FALSE) {Eidx <<- 2 }
                        plot.new()
                        plot(Object)
                 })

  OptFrame <- ttklabelframe(Egroup1, text="Options", borderwidth=2, padding=c(5,5,5,5) )
  tkgrid(OptFrame, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

  SelRegBtn <- tkbutton(OptFrame, text="SELECT REGION", width=20, command=function(){
                        CLine <- tclvalue(CL)
                        CLine <- unlist(strsplit(CLine, "\\."))   #"number." and "CL name" are separated
                        coreline <<- as.integer(CLine[1])   # Coreline index
                        oldcoreline <<- coreline
                        Object <<- XPSSample[[coreline]]
                        Xlimits <<- c(min(Object@.Data[[1]]), max(Object@.Data[[1]]))
                        Ylimits <<- c(min(Object@.Data[[2]]), max(Object@.Data[[2]]))
                        point.index <<- 1
                        replot()
                        OldCoords <<- Object@Boundaries
                        SelReg <<- SelReg+1
                        txt <- paste("Left Mouse Button to Define the Region to Extract\n",
                                     "Right Mouse Button to ZOOM\n",
                                     "Then Optimize the Region Selection Clicking near Corners\n",
                                     "When OK Right Mouse Button and then Press the EXTRACT REGION Button", sep="")
                        tkmessageBox(message=txt, title="WARNING", icon="warning")
                        GetCurPos(SingClick=FALSE)
                        rngX <- range(point.coords$x)
                        rngX <- (rngX[2]-rngX[1])/20
                        rngY <- range(point.coords$y)
                        rngY <- (rngY[2]-rngY[1])/20
                        if (Object@Flags[1]) { #Binding energy set
                            point.coords$x <<- sort(point.coords$x, decreasing=TRUE)  #pos$x in decreasing order
                            point.coords$x[1] <<- point.coords$x[1]+rngX/20
                            point.coords$x[2] <<- point.coords$x[2]-rngX/20
                        } else {
                            point.coords$x <<- sort(point.coords$x, decreasing=FALSE) #pos$x in increasing order
                            point.coords$x[1] <<- point.coords$x[1]-rngX/20
                            point.coords$x[2] <<- point.coords$x[2]+rngX/20
                        }
                        point.coords$y <<- sort(point.coords$y, decreasing=FALSE)
                        Xlimits <<- c(point.coords$x[1]-rngX/10, point.coords$x[2]+rngX/10)
                        Ylimits <<- c(point.coords$y[1]-rngY/10, point.coords$y[2]+rngY/10)
                        slot(Object,"Boundaries") <<- point.coords
                        replot()
                        GetCurPos(SingClick=FALSE)
                 })
  tkgrid(SelRegBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  ExtractBtn <- tkbutton(OptFrame, text="EXTRACT REGION", width=20, command=function(){
                        Extract()
                 })
  tkgrid(ExtractBtn, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

  UndoBtn <- tkbutton(OptFrame, text="UNDO", width=20, command=function(){
                        undo.plot()
                 })
  tkgrid(UndoBtn, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

  ResetBtn <- tkbutton(OptFrame, text="RESET BOUNDARIES", width=20, command=function(){
                        Object@.Data[[1]] <<- OldEnergyScale
                        Object@Flags[1] <<- OldFlag
                        reset.boundaries()
                 })
  tkgrid(ResetBtn, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
  
  ElementFrame <- ttklabelframe(Egroup1, text="Element Name and Orbital", borderwidth=2, padding=c(5,5,5,5) )
  tkgrid(ElementFrame, row = 3, column = 1, padx = 5, pady = 5, sticky="we")
  tkgrid(ttklabel(ElementFrame, text="   "),  #just to create space for the entry
                  row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  PlotFrame <- ttklabelframe(Egroup1, text="Plot", borderwidth=2, padding=c(5,5,5,5) )
  tkgrid(PlotFrame, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

  txt <- c("BINDING ENERGY SCALE", "KINETIC ENERGY SCALE")
  WhichE <- tclVar()
  for(ii in 1:2){
      SwitchE <- ttkradiobutton(PlotFrame, text=txt[ii], variable=WhichE, value=ii,
                    command=function(){
                        BE.KE <- tclvalue(WhichE)
                        XEnergy <- get("XPSSettings", envir=.GlobalEnv)$General[5] #the fifth element of the first column of XPSSettings
                        XEnergy <- as.numeric(XEnergy)
                        if (BE.KE == "2"){ #Convert to Kinetic Energy
                            if (XPSSample[[coreline]]@Flags[1] == TRUE) { #original energy scale Binding
                                XPSSample[[coreline]]@.Data[[1]] <<- XEnergy-XPSSample[[coreline]]@.Data[[1]]
                                XPSSample[[coreline]]@Boundaries$x <<- sort(range(XPSSample[[coreline]]@.Data[[1]]), decreasing=FALSE)
                                point.coords$x <<- XEnergy-XPSSample[[coreline]]@Boundaries$x
                                Corners$x <<- XEnergy-Corners$x
                                XPSSample[[coreline]]@Flags[1] <<- FALSE   #set KE scale
                                XPSSample[[coreline]]@units[1] <<- "Kinetic Energy [eV]"
                            }
                        }
                        if (BE.KE == "1"){ #Convert to Binding Energy
                            if (XPSSample[[coreline]]@Flags[1] == FALSE) { #original energy scale Kinetic
                                XPSSample[[coreline]]@.Data[[1]] <<- XEnergy-XPSSample[[coreline]]@.Data[[1]]
                                XPSSample[[coreline]]@Boundaries$x <<- sort(range(XPSSample[[coreline]]@.Data[[1]]), decreasing=TRUE)
                                point.coords$x <<- XEnergy-XPSSample[[coreline]]@Boundaries$x
                                Corners$x <<- XEnergy-Corners$x
                                XPSSample[[coreline]]@Flags[1] <<- TRUE   #set BE scale
                                XPSSample[[coreline]]@units[1] <<- "Binding Energy [eV]"
                            }
                        }
                        plot(XPSSample[[coreline]])
                 })
      tkgrid(SwitchE, row = 1, column = ii, padx = 5, pady = 5, sticky="w")
  }


#---- Buttons ----
  SaveFrame <- ttklabelframe(Egroup1, text="Plot", borderwidth=2, padding=c(5,5,5,5) )
  tkgrid(SaveFrame, row = 5, column = 1, padx = 5, pady = 5, sticky="w")

  SaveBtn <- tkbutton(SaveFrame, text="SAVE", width=11, command=function(){
                        assign(activeFName, XPSSample, envir = .GlobalEnv)
                        assign("activeSpectIndx", coreline, envir = .GlobalEnv)
                        assign("activeSpectName", XPSSample[[coreline]]@Symbol, envir = .GlobalEnv)
                        XPSSaveRetrieveBkp("save")
                 })
  tkgrid(SaveBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  SaveExitBtn <- tkbutton(SaveFrame, text="SAVE & EXIT", width=15,  command=function(){
                        XPSSettings$General[4] <<- 7      #Reset to normal graphic win dimension
                        assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                        assign(activeFName, XPSSample, envir = .GlobalEnv)
                        assign("activeSpectIndx", coreline, envir = .GlobalEnv)
                        assign("activeSpectName", XPSSample[[coreline]]@Symbol, envir = .GlobalEnv)
                        tkdestroy(Ewindow)  #this calls the handlerdestroy(Ewindow...)
                        XPSSaveRetrieveBkp("save")
                        plot(XPSSample)
                 })
  tkgrid(SaveExitBtn, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

  ExitBtn <- tkbutton(SaveFrame, text="EXIT", width=11, command=function(){
                        XPSSettings$General[4] <<- 7      #Reset to normal graphic win dimension
                        assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                        tkdestroy(Ewindow)  #this calls the handlerdestroy(Ewindow...)
                        XPSSaveRetrieveBkp("save")
                        plot(XPSSample)
                 })
  tkgrid(ExitBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")
}
