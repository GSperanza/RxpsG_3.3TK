# XPSEshift function to apply energy shifts to spectra to correct for charging effects

#' @title XPSEShift
#' @description XPSEshift function correct the energy scale of spectra
#'   affected by charging effects. Generally C1s from hydrocarbons at BE=285eV
#'   or Au4f 7/2 at BE=84eV  are chosen as reference peaks. Charges spectra are
#'   shifted forcing the reference spectra to fall at their correct BE position.
#' @examples
#'  \dontrun{
#' 	XPSEshift()
#' }
#' @export
#'

XPSEshift <- function(){

Widget_State <- function(widget, state = c("normal", "disabled")) {
  state <- match.arg(state)
  childID <- tclvalue(tkwinfo("children",widget))
  selected <- sapply(unlist(strsplit(childID, " ")), function(x) {
  tkconfigure(x, "-state", state)})
}

  GetCurPos <- function(SingClick){
       Widget_State(Frame01,"disabled") #prevent exiting Analysis if locatore active
       Widget_State(Frame2,"disabled")
       Widget_State(Frame3,"disabled")
       Widget_State(Frame4,"disabled")
       Widget_State(Frame5,"disabled")
       Widget_State(Frame6,"disabled")
       tcl("update", "idletasks")
       EXIT <- FALSE
       LocPos <<- list(x=0, y=0)
       Nclk <- SingClick
       if (Nclk == FALSE) Nclk <- 1
       while(EXIT == FALSE){  #if pos1 not NULL a mouse butto was pressed
            LocPos <<- locator(n=Nclk, type="p", pch=3, cex=1.5, col="blue", lwd=2) #to modify the zoom limits
            if (is.null(LocPos)) {
                Widget_State(Frame01,"normal") #prevent exiting Analysis if locatore active
                Widget_State(Frame2,"normal")
                Widget_State(Frame3,"normal")
                Widget_State(Frame4,"normal")
                Widget_State(Frame5,"normal")
                Widget_State(Frame6,"normal")
                EXIT <- TRUE
            } else {
                if (SingClick == 1){
                    Widget_State(Frame01,"normal") #prevent exiting Analysis if locatore active
                    Widget_State(Frame2,"normal")
                    Widget_State(Frame3,"normal")
                    Widget_State(Frame4,"normal")
                    Widget_State(Frame5,"normal")
                    Widget_State(Frame6,"normal")
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
                     plot(FName[[SpectIndx]], xlim=XYrange$x, ylim=XYrange$y)  #refresh graph
                     rect(Corners$x[1], Corners$y[1], Corners$x[3], Corners$y[2])
                     points(Corners, type="p", pch=3, cex=1.5, col="blue", lwd=2)
                } else {
                     plot(FName[[SpectIndx]], xlim=XYrange$x, ylim=XYrange$y)  #refresh graph
                     points(LocPos, type="p", pch=3, cex=1.5, lwd=2, col="red")
                     LocPos$x <<- round(LocPos$x, digits=2)
                     position <<- LocPos$x
                     tcl("update", "idletasks")  #force writing cursor position in the glabel
                }
            }
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

#--- Variables ---
   XPSSettings <- get("XPSSettings", envir=.GlobalEnv)
   WarnMsg <- XPSSettings$General[9]
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FName <- get(activeFName,envir=.GlobalEnv)  #this is the XPS Sample
   SpectIndx <- get("activeSpectIndx",envir=.GlobalEnv)
   SpectList <- XPSSpectList(activeFName)
   FNameList <- XPSFNameList()
   FitCompList <- names(FName[[SpectIndx]]@Components)
   if( is.null(names(FName[[SpectIndx]]@Components)) ){ FitCompList = ""  }
   position <- NULL
   Eshift <- NULL
   XYrange <- list(x=NULL, y=NULL)
   LocPos <- list(x=NULL, y=NULL)
   Corners <- list(x=NULL, y=NULL)
   ZOOM <- FALSE


#----- Widget ---
    if (is.na(activeSpectName)) {activeSpectName <- ""}


    ESWin <- tktoplevel()
    tkwm.title(ESWin,"ENERGY SHIFT")
    tkwm.geometry(ESWin, "+100+50")
    

    ESGroup <- ttkframe(ESWin, borderwidth=0, padding=c(0,0,0,0))
    tkgrid(ESGroup, row=1, column=1, padx = 5, pady=5, sticky="news")

    Frame01 <- ttklabelframe(ESGroup, text = "Select the XPSSample and Core Line", borderwidth=2, padding=c(5,5,5,5))
    tkgrid(Frame01, row=1, column=1, padx = 5, pady=5, sticky="news")
    XS <- tclVar(activeFName)
    Eobj0 <- ttkcombobox(Frame01, width = 20, textvariable = XS, values = FNameList)
    tkbind(Eobj0, "<<ComboboxSelected>>", function(){
                      activeFName <<- tclvalue(XS)
                      FName <<- get(activeFName,envir=.GlobalEnv)
                      SpectList <<- XPSSpectList(activeFName)
                      FNameList <<- XPSFNameList()
                      plot(FName)
                      tkconfigure(Eobj1, values=SpectList)
                      tkconfigure(Eobj2, values="")
                      tclvalue(CL) <- ""
                      tclvalue(FC) <- ""
           })
    tkgrid(Eobj0, row=1, column=1, padx=5, pady=5)

    CL <- tclVar("")
    Eobj1 <- ttkcombobox(Frame01, width = 20, textvariable = CL, values = SpectList)
    tkbind(Eobj1, "<<ComboboxSelected>>", function(){
                      SpectName <- tclvalue(CL)
                      SpectName <- unlist(strsplit(SpectName, "\\."))   #skip the number at beginning of string
                      SpectIndx <<- as.numeric(SpectName[1])
                      plot(FName[[SpectIndx]])
                      FitCompList <<- names(FName[[SpectIndx]]@Components)
#now update the VALUES of combobox Eobj2 initially void
#tkconfigure ensures the handler of Eobj2 is working properly
                      tkconfigure(Eobj2, values=FitCompList)
           })
    tkgrid(Eobj1, row=1, column=2, padx=5, pady=5)

    Frame2 <- ttklabelframe(ESGroup, text = "Select the Fit Component", borderwidth=2, padding=c(5,5,5,5))
    tkgrid(Frame2, row=2, column=1, padx = 5, pady=5, sticky="news")
    FC <- tclVar("")
    EE <- tclVar("")
    Eobj2 <- ttkcombobox(Frame2, width = 20, textvariable = FC, values = FitCompList)
    tkbind(Eobj2, "<<ComboboxSelected>>", function(){
                      Component <- tclvalue(FC)
                      FName <- get(activeFName, envir=.GlobalEnv)  #reload FName if a new EShift will be set
                      OldValue <- FName[[SpectIndx]]@Components[[Component]]@param["mu","start"] # actual value of the fit component position
                      OldValue <- round(as.numeric(OldValue), digits=2)
                      EE <<- tclVar(as.character(OldValue))
                      tkconfigure(Eobj5, textvariable=EE, foreground="black")  #update the Edit window with the componento position
                      if (length(FName[[SpectIndx]]@RegionToFit$x) > 0){
                          XYrange$x <<- range(FName[[SpectIndx]]@RegionToFit$x)
                          XYrange$y <<- range(FName[[SpectIndx]]@RegionToFit$y)
                      } else {
                          XYrange$x <<- range(FName[[SpectIndx]]@.Data[[1]])
                          XYrange$y <<- range(FName[[SpectIndx]]@.Data[[2]])
                      }
                      if (FName[[SpectIndx]]@Flags[1]) {   #reverse if BE scale
                          XYrange$x <<- rev(XYrange$x)
                      }
          })
    tkgrid(Eobj2, row=2, column=1, padx=5, pady=5)

    Frame3 <- ttklabelframe(ESGroup, text="Apply Shift", borderwidth=2, padding=c(5,5,5,5))
    tkgrid(Frame3, row = 3, column=1, padx = 5, pady=5, sticky="news")

    AllCL <- tclVar("All Core Lines")
    RadioAll <- ttkradiobutton(Frame3, text="All Core Lines", variable=AllCL, value="All Core Lines" )
    tkgrid(RadioAll, row=3, column=1, padx=15, pady=5, sticky="w")
    RadioSingle <- ttkradiobutton(Frame3, text="Selected Core Line Only", variable=AllCL, value="Selected Core Line Only" )
    tkgrid(RadioSingle, row=3, column=2, padx=15, pady=5, sticky="w")

    Frame4 <- ttklabelframe(ESGroup, text = "Define the Zoom Region", borderwidth=2, padding=c(5,10,20,5) )
    tkgrid(Frame4, row = 4, column=1, padx = 5, pady=5, sticky="news")
    ZMButton <- tkbutton(Frame4, text="   Set the Zoom Region   ", command=function(){
                      SpectName <- tclvalue(CL)
                      if(length(SpectName)==0) {
                         tkmessageBox(message="WARNING: No Coreline Selected, Zoom Stopped", title = "WARNING",icon = "warning" )
                         return()
                      }
                      txt <- " LEFT Mouse Button to Set the TWO Opposite Corners of the Zoom Area\n RIGHT Mouse Button to exit \n Click Near Markers to Modify the Zoom area"
                      tkmessageBox(message=txt , title = "WARNING",  icon = "warning")
                      ZOOM <<- TRUE
                      GetCurPos(SingClick=2)     #this to draw/modify the zooming area
                      ZOOM <<- FALSE
                      plot(FName[[SpectIndx]], xlim=XYrange$x, ylim=XYrange$y)  #refresh graph
          })
    tkgrid(ZMButton, row=4, column=1)

    Frame5 <- ttklabelframe(ESGroup, text = "Peak Position", borderwidth=2, padding=c(5,5,20,5) )
    tkgrid(Frame5, row = 5, column=1, padx = 5, pady=5, sticky="news")
    CurButton <- tkbutton(Frame5, text="          Cursor          ", command=function(){
                      SpectName <- tclvalue(CL)
                      if(length(SpectName)==0) {
                         tkmessageBox(message="WARNING: no coreline selected", title = "WARNING",icon = "warning" )
                         return()
                      }
                      if (WarnMsg == "ON"){
                          tkmessageBox(message="LEFT Mouse Button to Read Marker's Position; RIGHT Mouse Button to Exit" , title = "WARNING",  icon = "warning")
                      }
                      GetCurPos(SingClick=FALSE)     #this to draw/modify the zooming area
                      EE <<- tclVar(as.character(position))
                      tkconfigure(Eobj5, textvariable=EE, foreground="black")  #update the Edit window with the componento position
                      plot(FName[[SpectIndx]], xlim=XYrange$x, ylim=XYrange$y)  #refresh graph
          })
    tkgrid(CurButton, row = 1, column=1, pady=5, sticky="w")
    tkgrid(ttklabel(Frame5, text="Please Enter the New Energy Value:"), row=2, column=1, pady=5, sticky="w")

    NewE <- tclVar("E?")
    Eobj5 <- ttkentry(Frame5, textvariable=EE, foreground="grey")
    tkbind(Eobj5, "<FocusIn>", function(K){
                          tkconfigure(Eobj5, foreground="red")
          })
    tkbind(Eobj5, "<Key-Return>", function(K){
                      tkconfigure(Eobj5, foreground="black")
                      NewE <<- tclvalue(EE)
                      Component <- tclvalue(FC)
                      if (is.null(position) && length(Component)==0){
                          tkmessageBox(message="Cursor-Peak-Position or Fit Component lacking! Please select", title="LACKING REFERENCE POSITION", icon="warning")
                          tkconfigure(Eobj5, NewE="E?")
                          return()
                      }
                      if (NewE == "" ){
                          tkmessageBox(message="New energy position NULL. Please give a correct value", title="ENERGY POSITION NULL", icon="warning")
                          tkconfigure(Eobj5, NewE="E?")
                          return()
                      }
                      if (NewE != "E?"){
                          NewE <- as.numeric(NewE)
                          Component <- tclvalue(FC)
                          if (nchar(Component)==0){ #no fit present, no components, position read from cursor
                              Eshift <<- as.numeric(NewE-position)
                          } else {
                              Component <- as.numeric(substr(Component, 2,nchar(Component))) #exttract the component name
  	                           CompPos <- FName[[SpectIndx]]@Components[[Component]]@param["mu","start"]
                              Eshift <<- as.numeric(NewE-CompPos)
                          }
                          Escale <- FName[[SpectIndx]]@units[1]
                          txt <- paste("Applied Energy Shift: ", round(Eshift, 3), "          ", sep="")
                          tkgrid(ttklabel(Frame6, text=txt), row = 1, column=1, pady=5, sticky="w")
                          All_Sing <- tclvalue(AllCL)
                          if (All_Sing=="All Core Lines") {
                              NCoreLines <- length(FName)
                              for (ii in 1:NCoreLines){
                                   if (Escale == FName[[ii]]@units[1]) { #Eshift calculated on a BE scale and CoreLine[[ii]] sa same Energy Units
                                       FName[[ii]] <<- XPSapplyshift(FName[[ii]], Eshift)
                                   } else {
                                       Eshift <<- -Eshift
                                       FName[[ii]] <<- XPSapplyshift(FName[[ii]], Eshift)  # Eshift calculated on a BE scale while CoreLine[[ii]] is in Kinetic (or viceversa)
                                   }
                              }
                              plot(FName[[SpectIndx]])
                          } else if (All_Sing=="Selected Core Line Only"){ #Apply Eshift only on the selected coreline
                              if (Escale == FName[[SpectIndx]]@units[1]) {         #Eshift calculated on a BE scale and CoreLine[[ii]] sa same Energy Units
                                  FName[[SpectIndx]] <<- XPSapplyshift(FName[[SpectIndx]], Eshift)
                              } else {
                                  Eshift <<- -Eshift
                                  FName[[SpectIndx]] <<- XPSapplyshift(FName[[SpectIndx]], Eshift)  # Eshift calculated on a BE scale while CoreLine[[ii]] is in Kinetic (or viceversa)
                              }
                              plot(FName[[SpectIndx]])
 		                       }

                   }
          })
    tkgrid(Eobj5, row = 3, column = 1, pady=5, sticky = "w")

    Frame6 <- ttklabelframe(ESGroup, text = "Applied E-Shift", borderwidth=2, padding=c(5,5,20,5) )
    tkgrid(Frame6, row = 6, column=1, padx = 5, pady=5, sticky="news")
    txt <- paste("Applied Energy Shift: ", round(FName[[SpectIndx]]@Shift, 3), sep="")
    tkgrid(ttklabel(Frame6, text=txt), row = 1, column=1, pady=5, sticky="w")

    tkgrid(tkbutton(Frame6, text=" RESET E-SHIFT ", command=function(){
                    Eshift <<- FName[[SpectIndx]]@Shift
		                  FName <<- XPSapplyshift(FName, shift = 0)
                    txt <- paste("Applied Energy Shift: ", round(FName[[SpectIndx]]@Shift, 3), "          ", sep="")
                    tkgrid(ttklabel(Frame6, text=txt), row = 1, column=1, pady=5, sticky="w")
                    tkconfigure(Eobj2, textvariable="")
                    tkconfigure(Eobj5, textvariable="E?")
                    if (length(FName[[SpectIndx]]@RegionToFit$x) > 0){
                        XYrange$x <<- range(FName[[SpectIndx]]@RegionToFit$x)
                        XYrange$y <<- range(FName[[SpectIndx]]@RegionToFit$y)
                    } else {
                        XYrange$x <<- range(FName[[SpectIndx]]@.Data[[1]])
                        XYrange$y <<- range(FName[[SpectIndx]]@.Data[[2]])
                    }
                    plot(FName[[SpectIndx]])
          }),row = 2, column = 1, padx=1, pady=5, sticky="w")


    Frame7 <- ttkframe(ESGroup,  borderwidth=0, padding=c(0,0,0,0) )
    tkgrid(Frame7, row = 7, column = 1, padx = 5, pady = 5, sticky = "news")

    tkgrid(tkbutton(Frame7, text="        SAVE        ", command=function(){
    	                   assign("activeFName", activeFName, envir=.GlobalEnv)
    	                   assign(activeFName, FName, envir=.GlobalEnv)
    	                   XPSSaveRetrieveBkp("save")
          }),row = 1, column = 1, padx=8, pady=5, sticky="w")


    tkgrid(tkbutton(Frame7, text="      SAVE & EXIT     ", command=function(){
    	                   tkdestroy(ESWin)
    	                   assign("activeFName", activeFName, envir=.GlobalEnv)
    	                   assign(activeFName, FName, envir=.GlobalEnv)
                        assign("activeSpectIndx", SpectIndx, envir=.GlobalEnv)
    	                   XPSSaveRetrieveBkp("save")
    	                   plot(FName)
                        UpdateXS_Tbl()
          }),row = 1, column = 2, padx=1, pady=5, sticky="w")
}
