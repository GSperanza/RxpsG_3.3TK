
#' @title XPSVBFermi
#' @description XPSVBFermi function to estimate the position of the Valence Band Top
#'   Interactive GUI to add BaseLines and Fitting components to
#'   the region of the VB proximal to the Fermi Edge needed
#'   for the estimation of the VB-Top position
#' @examples
#' \dontrun{
#'  XPSVBFermi()
#' }
#' @export
#'


XPSVBFermi <- function() {

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

  LoadCoreLine <- function(h, ...){
     Object_name <- get("activeFName", envir=.GlobalEnv)
     Object <<- get(Object_name, envir=.GlobalEnv)  #load the XPSSample from the .Global Environment
     ComponentList <- names(slot(Object[[coreline]],"Components"))
     if (length(ComponentList)==0) {
         tkmessageBox(message="ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
         return()
     }
     plot(Object)   #replot spectrum of the selected component
  }


  set.coreline <- function(h, ...) {
     CL <- tclvalue(CL)
     CL <- unlist(strsplit(CL, "\\."))   #select the NUMBER. before the CoreLine name
     coreline <<- as.integer(CL[1])

     if (coreline == 0) {    #coreline == "All spectra"
         tclvalue(PLT) <- "normal"
         WidgetState(FunctionFrame, "disabled")
         plot(Object)
         tkmessageBox(message="Please select a Valence Band spectrum", title="WRONG SPECTRUM", icon="error")
         return()
     } else {
         if (length(Object[[coreline]]@Components) > 0) {
             tkmessageBox(message=" Analysis already present on this Coreline", title = "WARNING: Analysis Done",  icon = "warning")
             return()
         }
         WidgetState(FunctionFrame, "disabled")
         Object[[coreline]]@RSF <<- 0 #set the VB sensitivity factor to zero to avoid error wornings

         LL <- length(Object[[coreline]]@.Data[[1]])
         Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
         Object[[coreline]]@Boundaries$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
         Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
         BaseLevel <- mean(Object[[coreline]]@.Data[[2]][(LL-5):LL])
         Object[[coreline]]@Baseline$x <<- Object[[coreline]]@RegionToFit$x
         Object[[coreline]]@Baseline$y <<- rep(BaseLevel, LL)

     }
     ObjectBKP <<- Object[[coreline]]
     plot(Object[[coreline]])
  }


  reset.baseline <- function(h, ...) {
     if (coreline != 0) {   #coreline != "All spectra"
         LL <- length(Object[[coreline]]@.Data[[1]])
         Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
         Object[[coreline]]@Boundaries$y <<- c(Object[[coreline]]@.Data[[2]][LL], Object[[coreline]]@.Data[[2]][LL])
         Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
         BaseLevel <- mean(Object[[coreline]]@.Data[[2]][(LL-5):LL])
         Object[[coreline]]@Baseline$x <<- Object[[coreline]]@RegionToFit$x
         Object[[coreline]]@Baseline$y <<- rep(BaseLevel, LL)
         WidgetState(FunctionFrame, "disabled")
     }
  }

  SelectRegion <- function(h, ...){
      txt <- c("1) Left mouse button define the opposite corners of the VB-Near-Fermi region",
               "2) Left mouse button click near markers to modify the zoom area extension",
               "3) Right mouse button to exit")
      tkmessageBox(message=txt , title = "WARNING",  icon = "warning")
      pos <- locator(n=2, type="p", pch=3, col="blue", lwd=2) #first the two corners are drawn
      Xlim1 <- min(range(Object[[coreline]]@.Data[[1]]))   #limits coordinates in the Spectrum Range
      Xlim2 <- max(range(Object[[coreline]]@.Data[[1]]))
      Ylim1 <- min(range(Object[[coreline]]@.Data[[2]]))
      Ylim2 <- max(range(Object[[coreline]]@.Data[[2]]))
      if (Object[[coreline]]@Flags[1]) { #Binding energy set
          pos$x <- sort(pos$x, decreasing=TRUE)  #point.coords$x in decreasing order
          if (pos$x[2] < Xlim1 ) {pos$x[2] <- Xlim1}
          if (pos$x[1] > Xlim2 ) {pos$x[1] <- Xlim2}
      } else {
          pos$x <- sort(pos$x, decreasing=FALSE) #point.coords$x in increasing order
          if (pos$x[1] < Xlim1 ) {pos$x[1] <- Xlim1}
          if (pos$x[2] > Xlim2 ) {pos$x[2] <- Xlim2}
      }
      pos$y <- sort(pos$y, decreasing=FALSE)
      if (pos$y[1] < Ylim1 ) {pos$y[1] <- Ylim1}
      if (pos$y[2] > Ylim2 ) {pos$y[2] <- Ylim2}
      rect(pos$x[1], pos$y[1], pos$x[2], pos$y[2])  #marker-Corners are ordered with ymin on Left and ymax on Right
      Object[[coreline]]@Boundaries <<- pos
# 			  idx1 <- findXIndex(Object[[coreline]]@Baseline$x, pos$x[1])
# 			  idx2 <- findXIndex(Object[[coreline]]@Baseline$x, pos$x[2])
#      Object[[coreline]]@RegionToFit$x <<- Object[[coreline]]@.Data[[1]][idx1:idx2]  #Define RegionToFit see XPSClass.r
#      Object[[coreline]]@RegionToFit$y <<- Object[[coreline]]@.Data[[2]][idx1:idx2]  #Define RegionToFit see XPSClass.r
      Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])
      idx1 <- 1
      idx2 <- length(Object[[coreline]]@RegionToFit$x)
      yy <- min(Object[[coreline]]@RegionToFit$y)
      Object[[coreline]]@Baseline$x <<- Object[[coreline]]@RegionToFit$x
      Object[[coreline]]@Baseline$y <<- rep(yy, idx2)
      plot(Object[[coreline]])
      XX <- Object[[coreline]]@Boundaries$x
      YY <- c(Object[[coreline]]@Baseline$y[idx1], Object[[coreline]]@Baseline$y[idx2])
      points(XX, YY, type="p", pch=9, cex=1.5, col="red", lwd=1.5)

      pos <- list(x=0, y=0)
      while (length(pos) > 0) {      #if pos not NULL a mouse button was pressed
         pos <- locator(n=1) #to modify the zoom limits
         if (length(pos$x) > 0) { #if the right mouse button NOT pressed
            dX1 <- abs(XX[1] - pos$x)
            dX2 <- abs(XX[2] - pos$x)
            if (dX1 < dX2) {
                XX[1] <- pos$x
                idx1 <- findXIndex(Object[[coreline]]@Baseline$x, pos$x)
                YY[1] <- Object[[coreline]]@Baseline$y[idx1]
                Object[[coreline]]@Boundaries$x[1] <<- XX[1]
                Object[[coreline]]@Boundaries$y <<- range(Object[[coreline]]@RegionToFit$y[idx1:idx2])
            }
            if (dX2 < dX1) {
                XX[2] <- pos$x
                idx2 <- findXIndex(Object[[coreline]]@Baseline$x, pos$x)
                YY[2] <- Object[[coreline]]@Baseline$y[idx2]
                Object[[coreline]]@Boundaries$x[2] <<- XX[2]
                Object[[coreline]]@Boundaries$y <<- range(Object[[coreline]]@RegionToFit$y[idx1:idx2])
            }
            plot(Object[[coreline]], xlim=XX, ylim=Object[[coreline]]@Boundaries$y)  #refresh graph
            points(XX, YY, type="p", pch=9, cex=1.5, col="red", lwd=1.5)
         }
      }
      pos$x <- c(XX[1], XX[2])
      pos$y <- c(YY[1], YY[2])
      Object[[coreline]]@Boundaries <<- pos
      Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
      Object[[coreline]] <<- XPSbaseline(Object[[coreline]], "linear", deg=1, splinePoints=NULL )
      plot(Object[[coreline]])
  }


#--- Functions, Fit and VB_Top estimation
  add.FitFunct <- function(h, ...) {
     ObjectBKP <<- Object[[coreline]]
     tkmessageBox(message="Please define the mid point of the VB descendent tail", title="DEFINE POSITION", icon="warning")
     pos <- locator(n=1, type="p", col="red", lwd=2, cex=1.5, pch=3)     
     if (coreline != 0 && hasBaseline(Object[[coreline]])) {
#Fit parameter are set in XPSAddComponent()
         Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = "VBFermi",
                                             peakPosition = list(x = pos$x, y = 2*pos$y), ...)
         Object[[coreline]]@Fit$y <<- Object[[coreline]]@Components[[1]]@ycoor-Object[[coreline]]@Baseline$y #subtract the Baseline
         Object[[coreline]]@RegionToFit$x <- ObjectBKP@RegionToFit$x #restore original abscissas changed in XPSAddComponent()
         plot(Object[[coreline]])
     }
  }


  del.FitFunct <- function(h, ...) {  #title="DELETE COMPONENT KILLS CONSTRAINTS!!!"
     ObjectBKP <<- Object[[coreline]]
     if (tkmessageBox(message="Deleting fit function. Are you sure you want to proceed?", title="DELETE", icon="warning")) {
         LL<-length(Object[[coreline]]@Components)
         for (ii in 1:LL) { #Remove all CONSTRAINTS
              Object[[coreline]] <<- XPSConstrain(Object[[coreline]],ii,action="remove",variable=NULL,value=NULL,expr=NULL)
         }
         if (coreline != 0 && hasComponents(Object[[coreline]])) {
             DelWin <- tktoplevel()
             tkwm.title(DelWin,"DELETE")
             tkwm.geometry(DelWin, "+200+100")   #position respect topleft screen corner
             DelGroup <- ttkframe(DelWin, borderwidth=0, padding=c(0,0,0,0) )
             tkgrid(DelGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
             tkgrid( ttklabel(DelGroup, text="Select the fit component to delete"),
                     row = 1, column = 1, padx = 5, pady = c(5, 10), sticky="w")

             tkSep <- ttkseparator(DelGroup, orient="horizontal")
             tkgrid(tkSep, row=2, column=1, padx = 5, pady = c(10, 5),sticky="we")   #important sticky"NS" to expand the separator

             FCMP <- tclVar()
             FitComp <- ttkcombobox(DelGroup, width = 15, textvariable = FCMP,
                                    values = c(names(Object[[coreline]]@Components),"All"))
             tkgrid(FitComp, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

             OKBtn <- tkbutton(DelGroup, text="  OK  ", width=15, command=function(){
                            if (tclvalue(FCMP) != "All"){
                                indx <- grep(tclvalue(FCMP), names(Object[[coreline]]@Components))
                                Object[[coreline]] <<- XPSremove(Object[[coreline]], what="components", number=indx )
                                if (length(Object[[coreline]]@Components) > 0 ) {
                                    #to update the plot:
                                    tmp <- sapply(Object[[coreline]]@Components, function(z) matrix(data=z@ycoor))
                                    Object[[coreline]]@Fit$y <<- ( colSums(t(tmp)) - length(Object[[coreline]]@Components)*(Object[[coreline]]@Baseline$y))
                                }
                            } else {
                                Object[[coreline]] <<- XPSremove(Object[[coreline]], "components")
                            }
                            tclvalue(PLT) <- "normal"
                            tkdestroy(DelWin)
                        })
             tkgrid(OKBtn, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

             CancelBtn <- tkbutton(DelGroup, text="  Cancel  ", command=function(){
                            tkdestroy(DelWin)
                       })
             tkgrid(CancelBtn, row = 5, column = 1, padx = 5, pady = 5, sticky="w")
         }
     }
  }

  do.Fit <- function(h, ...) {
     ObjectBKP <<- Object[[coreline]]
     FitRes <- NULL

     if (coreline != 0) {  #VB Hill Sigmoid
         if (reset.fit==FALSE){
#Fit parameters and X coords are set in XPSAddComponent()
#also the FermiSigmoid was defined using the X coords
             Object[[coreline]] <<- XPSFitLM(Object[[coreline]], plt=FALSE, verbose=FALSE)   #Levenberg Marquardt fit
             if (tclvalue(PLT) == "normal") plot(Object[[coreline]])
             if (tclvalue(PLT) == "residual") XPSresidualPlot(Object[[coreline]])
         } else if (reset.fit==TRUE){
             Object[[coreline]] <<- XPSremove(Object[[coreline]],"fit")
             Object[[coreline]] <<- XPSremove(Object[[coreline]],"components")
             reset.fit <<- FALSE
             plot(Object[[coreline]])
         }
     }
  }

  calcVBFermi <- function(h, ...) {
      #retrieve fitted h, Ef and K values
      h <- Object[[coreline]]@Components[[1]]@param[1,1]  #fitted h value
      
      Ef.Posx <- Object[[coreline]]@Components[[1]]@param[2,1]  #fitted Ef value
      Ef.Posx <- round(Ef.Posx, digits=3)
      idx <- findXIndex(Object[[coreline]]@RegionToFit$x, Ef.Posx)
      bkg <- Object[[coreline]]@Baseline$y[idx]
      Ef.Posy <- h/2 + bkg   #Ef.Posy correspondent to Ef.Posx
      Object[[coreline]]@Components[[1]]@label <<- "VBFermi"  #Label indicating the VBFermi in the plot
      Object[[coreline]]@Components[[1]]@param[1,1] <<- Ef.Posy  # VBFermi stored in param "h"
      plot(Object[[coreline]])
      lines(x=c(Ef.Posx, Ef.Posx), y=c(0,h+1), col="blue")
      points(Ef.Posx, Ef.Posy, col="orange", cex=2, lwd=2, pch=3)
      txt <- paste("==> Estimated position of Fermi Level : ", Ef.Posx, sep="")
      tkconfigure(FermiLevl, text=txt)
      tkconfigure(StatusBar, text=txt)
      cat("\n", txt)

      charPos <- FindPattern(Object[[coreline]]@Info, "   ::: Fermi Edge position = ")[1]  #charPos[1] = row index of Info where "::: Fermi Edge position" is found
      if(length(charPos) > 0){ #Overwrite  the Fermi Edge Position in FName Info
         Object[[coreline]]@Info[charPos] <<- paste("   ::: Fermi Edge position = ", Ef.Posy, sep="") #overwrite previous MAx\Min Dist value
      } else {
         answ <- tkmessageBox(message="Save new value of Fermi Edge POsition?",
                              type="yesno", title="WARNING", icon="warning")
         if (tclvalue(answ) == "1"){
             nI <- length(Object[[coreline]]@Info)+1
             Object[[coreline]]@Info[nI] <<- paste("   ::: Fermi Edge position = ", Ef.Posy, sep="")
         }
      }
  }


#----Set Default Variable Values
  ResetVars <- function(){
     Object[[coreline]] <<- XPSremove(Object[[coreline]],"all")
     LL <- length(Object[[coreline]]@.Data[[1]])
     Object[[coreline]]@Boundaries$x <<- c(XPSSample[[coreline]]@.Data[[1]][1], XPSSample[[coreline]]@.Data[[1]][LL])
     Object[[coreline]]@Boundaries$y <<- c(XPSSample[[coreline]]@.Data[[2]][1], XPSSample[[coreline]]@.Data[[2]][LL])
     Object[[coreline]]@RegionToFit$x <<- XPSSample[[coreline]]@.Data[[1]]  #Define RegionToFit see XPSClass.r
     Object[[coreline]]@RegionToFit$y <<- XPSSample[[coreline]]@.Data[[2]]  #Define RegionToFit see XPSClass.r
     Object[[coreline]]@Boundaries$y[1] <<- min(XPSSample[[coreline]]@Boundaries$y)
     Object[[coreline]]@Boundaries$y[2] <<- max(XPSSample[[coreline]]@Boundaries$y)
     BaseLevel <- mean(XPSSample[[coreline]]@.Data[[2]][(LL-5):LL])
     Object[[coreline]]@Baseline$x <<- XPSSample[[coreline]]@.Data[[1]]
     Object[[coreline]]@Baseline$y <<- rep(BaseLevel, LL)

     VBbkgOK <<- FALSE
     VBlimOK <<- FALSE
     tkconfigure(FermiLevl, text="Estimated position of VB top : ")
     plot(Object[[coreline]])
     return()
  }

#--- variables
  activeFName <- get("activeFName", envir = .GlobalEnv)
  if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
      tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
      return()
  }

  XPSSample <- get(activeFName,envir=.GlobalEnv)  #this is the XPS Sample
  Object <- XPSSample
  Object_name <- get("activeFName", envir = .GlobalEnv) #XPSSample name
  ObjectBKP <- NULL   #CoreLine bkp to enable undo operation
  FNameList <- XPSFNameList() #list of XPSSamples
  SpectList <- XPSSpectList(activeFName) #list of XPSSample Corelines
  coreline <- 0
  VBbkgOK <- FALSE
  VBlimOK <- FALSE
  FitFunct <- "VBFermi"
  reset.fit <- FALSE

#--- Widget definition
  VBwindow <- tktoplevel()
  tkwm.title(VBwindow," XPS VB FERMI EDGE ")
  tkwm.geometry(VBwindow, "+100+50")   #position respect topleft screen corner
  VBGroup <- ttkframe(VBwindow, borderwidth=0, padding=c(0,0,0,0) )
  tkgrid(VBGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

  ## Core lines
  SelectFrame <- ttklabelframe(VBGroup, text = " XPS Sample and Core line Selection ", borderwidth=2)
  tkgrid(SelectFrame, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
  XS <- tclVar(activeFName)
  XPS.Sample <- ttkcombobox(SelectFrame, width = 15, textvariable = XS, values = FNameList)
  tkbind(XPS.Sample, "<<ComboboxSelected>>", function(){
                      activeFName <<- tclvalue(XS)
                      Object <<- get(activeFName, envir=.GlobalEnv)
                      Object_name <<- activeFName
                      SpectList <<- XPSSpectList(activeFName)
                      tkconfigure(Core.Lines, values=SpectList)
                      coreline <<- 0
                      VBbkgOK <<- FALSE
                      VBlimOK <<- FALSE
                      reset.baseline()
                      plot(Object)
                      WidgetState(FunctionFrame, "normal")
                 })
  tkgrid(XPS.Sample, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  CL <- tclVar()
  Core.Lines <- ttkcombobox(SelectFrame, width = 15, textvariable = CL, values = SpectList)
  tkbind(Core.Lines, "<<ComboboxSelected>>", function(){
                      set.coreline()
                 })
  tkgrid(Core.Lines, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

#----- Button Function
  FunctionFrame <- ttklabelframe(VBGroup, text = " Set VB upper region ", borderwidth=2)
  tkgrid(FunctionFrame, row = 2, column = 1, padx = 5, pady = 5, sticky="we")

  tkgrid( ttklabel(FunctionFrame, text=" Press 'Define the VB Upper Region' Button \n LEFT CLICKING THE OPPOSITE CORNERS"),
          row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  Select_btn <- tkbutton(FunctionFrame, text=" Define the VB Upper Region ", command=function(){
                      SelectRegion()
                  })
  tkgrid(Select_btn, row = 2, column = 1, padx = 5, pady = 5, sticky="we")

  Add_btn <- tkbutton(FunctionFrame, text=" Add Fermi-Dirac function ", command=function(){
                      add.FitFunct()
                  })
  tkgrid(Add_btn, row = 3, column = 1, padx = 5, pady = 5, sticky="we")

  Fit_btn <- tkbutton(FunctionFrame, text=" Fit ", command=function(){
                      do.Fit()
                  })
  tkgrid(Fit_btn, row = 4, column = 1, padx = 5, pady = 5, sticky="we")

  Fit_GetFermi <- tkbutton(FunctionFrame, text=" Get Fermi Edge Position ", command=function(){
                      calcVBFermi()
                  })
  tkgrid(Fit_GetFermi, row = 5, column = 1, padx = 5, pady = 5, sticky="we")

  Reset_btn <- tkbutton(FunctionFrame, text=" Reset Analysis ", command=function(){
                      ResetVars()
                      tkconfigure(FermiLevl, text="Estimated position of VB top : ")
                  })
  tkgrid(Reset_btn, row = 6, column = 1, padx = 5, pady = 5, sticky="we")

#----
  ResultFrame <- ttklabelframe(VBGroup, text = " VB Fermi Level ", borderwidth=2)
  tkgrid(ResultFrame, row = 3, column = 1, padx = 5, pady = 5, sticky="we")
  FermiLevl <- ttklabel(ResultFrame, text="Estimated position of VB top : ")
  tkgrid(FermiLevl, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

#----
  PlotFrame <- ttklabelframe(VBGroup, text = " Plot ", borderwidth=2)
  tkgrid(PlotFrame, row = 4, column = 1, padx = 5, pady = 5, sticky="we")
  PLT <- tclVar("Normal")
  PltType <- c("normal", "residual")
  for(ii in 1:2){
      Radio <- ttkradiobutton(PlotFrame, text=PltType[ii], variable=PLT, value=PltType[ii],
                              command=function(){
                                  plot()
                              })
      tkgrid(Radio, row = 1, column = ii, padx = 5, pady = 5, sticky="w")
  }

#----- SAVE&CLOSE button -----
#  gseparator(container = MainGroup)
  ButtGroup <- ttkframe(VBGroup, borderwidth=0, padding=c(0,0,0,0) )
  tkgrid(ButtGroup, row = 5, column = 1, padx = 0, pady = 0, sticky="w")

  SaveBtn <- tkbutton(ButtGroup, text="  SAVE  ", width=15, command=function(){
                     activeSpecIndx <- coreline[1]
                     assign("activeFName", Object_name, envir = .GlobalEnv)
                     assign(Object_name, Object, envir = .GlobalEnv)
                     assign("activeSpectIndx", activeSpecIndx, envir = .GlobalEnv)
                     assign("activeSpectName", coreline[2], envir = .GlobalEnv)
                     plot(Object[[coreline]])
                     XPSSaveRetrieveBkp("save")
                  })
  tkgrid(SaveBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  SaveExitBtn <- tkbutton(ButtGroup, text="  SAVE & EXIT ", width=15, command=function(){
                     LL <- length(XPSSample)
                     XPSSample[[LL+1]] <- Object[[coreline]]
                     XPSSample[[LL+1]]@Symbol <- "VBf"
                     XPSSample@names[LL+1] <- "VBf" #Object coreline was the initial VB possessing non-NULL name "VB"
                     activeSpecIndx <- coreline
                     assign("activeFName", Object_name, envir = .GlobalEnv)
                     assign(Object_name, XPSSample, envir = .GlobalEnv)
                     assign("activeSpectIndx", activeSpecIndx, envir = .GlobalEnv)
                     assign("activeSpectName", coreline, envir = .GlobalEnv)
                     idx <- which(XPSFNameList() == activeFName)      #index of the XPSSample_Name in tle FNameList
                     idx <- paste("I00", idx, sep="")                 #build index compatible with the TCL
                     XS_Tbl <- get("XS_Tbl", envir=.GlobalEnv)        #get the ttktreview ID
                     tcl(XS_Tbl, "selection", "set", idx)             #set the actual XPSSample as selected in the MAIN-GUI
                     tkdestroy(VBwindow)
                     XPSSaveRetrieveBkp("save")
                     plot(XPSSample[[LL+1]])
                     UpdateXS_Tbl()
                  })
  tkgrid(SaveExitBtn, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

  SaveExitBtn <- tkbutton(ButtGroup, text="  EXIT ", width=15, command=function(){
                     tkdestroy(VBwindow)
                     XPSSaveRetrieveBkp("save")
                     plot(Object[[coreline]])
                  })
  tkgrid(SaveExitBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

  tkSep <- ttkseparator(VBGroup, orient="horizontal")
  tkgrid(tkSep, row = 6, column = 1, padx = 5, pady = 5, sticky="we")
  StatusBar <- ttklabel(VBGroup, text=" Estimated position of Fermi Level : ", relief="sunken", foreground="blue")
  tkgrid(StatusBar, row = 7, column = 1, padx = 5, pady = 5, sticky="we")

  WidgetState(FunctionFrame, "disabled")
}
