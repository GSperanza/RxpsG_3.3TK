#' @title XPSMoveComp
#' @description XPSMoveComp function to modify component position and intensity in a fit
#'   this function provides a userfriendly interface change position and intensity of each
#'   individual fitting component of a selected XPSCoreline. Changes are saved
#'   in the .GlobalEnv main software memory.
#' @examples
#' \dontrun{
#' 	XPSMoveComp()
#' }
#' @export
#'

XPSMoveComp <- function(){

  GetCurPos <- function(SingClick){
       coords <<- NULL
       EXIT <<- FALSE
       WidgetState(MCFrame3, "disabled")   #prevent exiting Analysis if locatore active
       WidgetState(MCFrame4, "disabled")   #prevent exiting Analysis if locatore active
       Estep <- abs(Object@RegionToFit[[1]][1] - Object@RegionToFit[[1]][2])
       while(EXIT == FALSE){
            pos <- locator(n=1)
            if (is.null(pos)) {
                WidgetState(MCFrame3, "normal")
                WidgetState(MCFrame4, "normal")
                EXIT <<- TRUE
            } else {
                if (SingClick){
                     WidgetState(MCFrame3, "normal")
                     WidgetState(MCFrame4, "normal")
                     EXIT <<- TRUE
                }
                if (SetZoom == TRUE) {  #define zoom area
                    xx <- pos$x
                    yy <- pos$y
                    Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                    yy_BasLin <- yy-Object@Baseline$y[Xindx]  #spectral intensity at xx without Baseline
                    coords <<- c(xx, yy, yy_BasLin)
                    RBmousedown()  #selection of the zoom area
                } else {
                    xx <- pos$x
                    yy <- pos$y
                    Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                    yy_BasLin <- yy-Object@Baseline$y[Xindx]  #spectral intensity at xx without Baseline
                    coords <<- c(xx, yy, yy_BasLin)
                    LBmousedown()
                }
            }
       }
       return()
  }

  LBmousedown <- function() {   #Left mouse button down
	    xx <- coords[1]
	    yy <- coords[2]
     if (SetZoom == FALSE) { #left button works only when SET ZOOM REGION inactive
        MoveComp()
    	   ## loop on spectra and retrieve Pass Energy
        XPSquantify(XPSSample)
        refresh <<- FALSE
     }
     replot()
  }

  RBmousedown <- function() {   #Right mouse button down
	    xx <- coords[1]
	    yy <- coords[2]
     if (SetZoom == TRUE) { #left button works only when SET ZOOM REGION button pressed
     	  point.coords$x[point.index] <<- coords[1]   #abscissa
     	  point.coords$y[point.index] <<- coords[2]   #ordinate
     	  if (point.index == 1) {   #First rect corner C1
     	     point.index <<- 3
           Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
           Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
#  	     } else if (point.index == 2) { #Second rect corner C2 opposite to C1
#           point.index <<- 3
#           Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
#           Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
  	     } else if (point.index == 3) { #modifies corner positions
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
           if (Object@Flags[1]) { #Binding energy set
              point.coords$x <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE) #pos$x in decreasing order
              point.coords$y <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
           } else {
              point.coords$x <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE) #pos$x in increasing order
              point.coords$y <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
           }
        }
        replot()
     }
  }

  Check.PE <- function(){
    	PassE <- NULL
       	PassE <- sapply( XPSSample, function(x) {
                      info <- x@Info[1]   #retrieve info containing PE value
                      sss <- strsplit(info, "Pass energy")  #extract PE value
                      PE <- strsplit(sss[[1]][2], "Iris") #PE value
                      PE <- gsub(" ", "", PE[[1]][1], fixed=TRUE) #drop white spaces in string PE
                      PE <- as.integer(PE)
                      return(PE)
		                  }
                  )
        SpectList <- XPSSpectList(activeFName)
        idx <- grep("Survey", SpectList)       #recognize presence of a Survey
        if (length(idx) == 0){ idx <- grep("survey", SpectList) }
        if (length(idx) == 0){ return(TRUE) }  #No survey spectra are present in XPSSample: continue working

        PEsur <- PassE[[ idx[1] ]]             #if idx is a vector, select the first element

        if (length(idx) > 0){                  #a survey is present
           SpectList <- SpectList[-idx]        #eliminate the "Survey" from list of Spectral Names
           PassE <- PassE[-idx]                #eliminate the PassE(survey) to compare PE of only core-lines
        }
        Extracted <- which(PassE == 160)
        LL <- length(Extracted)
        if (LL > 0){
            for(ii in 1:LL){
                Indx <- Extracted[ii]
                Indx <- unlist(strsplit(SpectList[Indx], "\\."))   
                Indx <- as.integer(Indx[1])                    #select "NUMber." in component name
                if ( hasRegionToFit(XPSSample[[Indx]])){
                     txt <- paste(SpectList[Extracted], collapse="  ")
                     txt <- paste(" Found Core Line: ", txt, " extracted from Survey.
                     \nCannot perform quantification here!
                     \nPlease exit  MOVE COMPONENT  and run  QUANTIFY  option to correct Core Line intensity")
                     tkmessageBox(message=txt, title="WARNING", icon="warning")
                     return(FALSE)
                     break
                }
            }
        }
        return(TRUE)
  }

  MoveComp <- function(...) {
     if (length(FComp) == 0) {
         tkmessageBox(message="Select Component Please", title="WARNING", icon = "warning")
     } else {
        xx <- coords[1]
        yy <- coords[2]  #Component max value with baseline
        zz <- coords[3]  #Component max value without baseline
        FitFunct <- Object@Components[[FComp]]@funcName
        newh <- GetHvalue(Object, FComp, FitFunct, zz)  #Get value computes the Component value given the fit parameters and the Ymax value
        #range limits for mu
        varmu <- getParam(Object@Components[[FComp]],variable="mu")
        minmu <- varmu$start-varmu$min
        maxmu <- varmu$max-varmu$start
        newmu <- c(xx, xx-minmu, xx+maxmu)
        #range limits for h
        varh <- getParam(Object@Components[[FComp]],variable="h")
        minh <- varh$start-varh$min
        maxh <- varh$max-varh$start
        if (maxh > 0) {
            newh <- c(newh, 0, newh*5)    # No constraints on h
        }
        if (maxh==0){
            newh <- c(newh, newh, newh)   # h is fixed
        }
        if (maxh < 0){
            newh <- c(newh, 0, newh*5)    # maxh cannot be <0: => force newH to correct values
        }
        if (varh$start < 0) {
            newh <- c(0, 0, 1e5)   #set a positive value for an hypotheic fit
        }
        Object@Components[[FComp]] <<- setParam(Object@Components[[FComp]], parameter=NULL, variable="mu", value=newmu)
        Object@Components[[FComp]] <<- setParam(Object@Components[[FComp]], parameter=NULL, variable="h", value=newh)
#Now compute the new component Y values for the new xy position
        Object@Components[[FComp]] <<- Ycomponent(Object@Components[[FComp]], x=Object@RegionToFit$x, y=Object@Baseline$y) #eomputes the Y value and add baseline
#Fit computed addind fit components with the modified ones
        tmp <- sapply(Object@Components, function(z) matrix(data=z@ycoor))
        Object@Fit$y <<- ( colSums(t(tmp)) - length(Object@Components)*(Object@Baseline$y))

        Object <<- sortComponents(Object)
#if component order changed then re-number them
        LL <- length(Object@Components) #N. fit components
        idx <- 0
        for(ii in 1:LL){
           if (xx == Object@Components[[ii]]@param["mu",1]) { #compare marker position with component positions
               idx <- ii
               break()
           }
        }
        tclvalue(FC) <- ComponentList[[idx]]  #update component gradio
        XPSSample[[Indx]] <<- Object
     }
  }

  replot <- function(...) {
     if (point.index==1 && refresh==FALSE) {  #point.index==1 when moving mcomponent
         plot(Object, xlim=Xlimits, ylim=Ylimits)
         points(x=coords[1], y=coords[2], col=2, cex=1.2, lwd=2.5, pch=1)  # if refresh==FALSE plot spectrum with component marker
     } else if (SetZoom == TRUE){   #set zoom area corners
	        if (point.index == 1) {    #normal plot
 	           plot(Object)
             points(point.coords, type="p", col=4, cex=1.2, lwd=2.5, pch=3)
  	      } else if (point.index == 3){  #plot zoom area corners
 	           plot(Object, xlim=Xlimits, ylim=Ylimits)
             points(Corners, type="p", col=4, cex=1.2, lwd=2.5, pch=3)
             rect(point.coords$x[1], point.coords$y[1], point.coords$x[2], point.coords$y[2])
         }
     } else {
         plot(Object, xlim=Xlimits, ylim=Ylimits)
     }
     if (! is.null(coords)){
         txt <- paste("x =",round(coords[1],2), "y =",round(coords[2],2), sep="   ")
         tkconfigure(StatusBar, text=txt)
     }
  }

  reset.plot <- function(h, ...) {
       point.coords$x <<- range(Object@RegionToFit$x) #set original X range
       point.coords$y <<- range(Object@RegionToFit$y) #set original Y range
       Object@Boundaries <<- point.coords
       Xlimits <<- point.coords$x
       Ylimits <<- sort(point.coords$y, decreasing=FALSE)
       Corners <- list(x=c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2]),
                       y=c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2]))
       replot()
  }

  ComponentMenu <- function(){
       ClearWidget(MCFrame3) #clears previous tkradio of fit components
       LL <- length(ComponentList)
       if (LL > 1){    #gradio works with at least 2 items
           FC <<- tclVar()
           NCol <- ceiling(LL/5) #ii runs on the number of columns
           for(ii in 1:NCol){
               NN <- (ii-1)*5    #jj runs on the number of column_rows
               for(jj in 1:5) {
                   if ((jj+NN) > LL) {break} #exit loop if all FitComp are in RadioBtn
                   FitComp <<- ttkradiobutton(MCFrame3 , text=ComponentList[[(jj+NN)]], variable=FC,
                                value=ComponentList[(jj+NN)], command=function(){
                                FComp <<- tclvalue(FC)
                                cat("\n selected component:", FComp)
                                FComp <<- as.numeric(unlist(strsplit(FComp, split="C")))   #index selected component
                                FComp <<- FComp[2]
                                xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                                Rng <- range(Object@RegionToFit$x)
                                if (xx < Rng[1]) {xx <- Rng[1]}
                                if (xx > Rng[2]) {xx <- Rng[2]}
                                yy <- Object@Components[[FComp]]@param[1,1] #component height h
                                FuncName <- Object@Components[[FComp]]@funcName
                                yy <- yy/GetHvalue(Object,FComp, FuncName, 1)  #provides the correct yy value for complex functions
                                Estep <- abs(Object@RegionToFit$x[1]-Object@RegionToFit$x[2])
                                Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                                yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
                                coords[1] <<- xx
                                coords[2] <<- yy
                                refresh <<- TRUE    #cancel previous selections
                                replot()   #plot spectrum without marker
                                refresh <<- FALSE #now plot also the component marker
                                replot()   #replot spectrum and marker
                                if (ShowMsg == TRUE && WarnMsg == "ON"){
                                    tkmessageBox(message="Left click to enter Fit Component position. Right click to stop slection", title="WARNING", icon="warning")
                                }
                                GetCurPos(SingClick=FALSE)
                       })
                   tkgrid(FitComp, row = jj, column = ii, padx = 5, pady = 5, sticky="w")
               }
           }
           tclvalue(FC) <- FALSE
       }
       if (length(ComponentList) == 1){    #gradio works with at least 2 items
           FC <<- tclVar(FALSE)
           FitComp <<- tkcheckbutton(MCFrame3 , text=ComponentList[[1]], variable=FC, onvalue = ComponentList[[1]],
                            offvalue = 0, command=function(){
                                FComp <<- tclvalue(FC)
                                if (FComp == "0") {
                                    tkmessageBox(message="Select the Fit Component Please", title="WARNING", icon="warning")
                                    return()
                                }
                                FComp <<- as.numeric(unlist(strsplit(FComp, split="C")))   #index selected compoent
                                FComp <<- FComp[2]
                                xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                                yy <- Object@Components[[FComp]]@param[1,1] #component height h
                                Rng <- range(Object@RegionToFit[[1]])
                                if (xx < Rng[1]) {xx <- Rng[1]}
                                if (xx > Rng[2]) {xx <- Rng[2]}
                                Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                                Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                                yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                                coords[1] <<- xx
                                coords[2] <<- yy
                                refresh<<-TRUE    #cancel previous component markers
                                replot()   #plot spectrum only
                                refresh <<- FALSE #now plot spectrum + component marker
                                replot()
                                if (ShowMsg == TRUE && WarnMsg == "ON"){
                                    tkmessageBox(message="Left click New Fit_Component Position. Right click to Stop Selection", title="WARNING", icon="warning")
                                }
                                GetCurPos(SingClick=FALSE)
                       })
           tkgrid(FitComp, row = 1, column = 1, padx = 5, pady=5, sticky="w")
       }
       txt <- paste("The selection of a Core-Line or Fit-Component always activates reading the [X,Y] cursor position.",
                    "\n=> Left click with the mouse to enter the cursor coordinates.",
                    "\n=> Right click to stop position selection and cursor position reading when not required.",
                    "\n",
                    "=> Show the WARNING Messages Again? ", sep="")
       if (ShowMsg == TRUE && WarnMsg == "ON"){
           ShowMsg <<- tclvalue(tkmessageBox(message=txt, type="yesno", title="WARNING", icon="question")) #ShowMsg==FALSE if answer=YES
           if (ShowMsg == "no") {ShowMsg <<- FALSE}
           if (ShowMsg == "yes") {ShowMsg <<- TRUE}
       }
  }

  LoadCoreLine <- function(){
      XPSSample <<- get(activeFName, envir=.GlobalEnv)       #load XPSdata values from main memory
      Indx <<- activeSpectIndx
      Object <<- XPSSample[[Indx]]
      Xlimits <<- range(Object@RegionToFit$x)
      Ylimits <<- range(Object@RegionToFit$y)
      Object@Boundaries$x <<- Xlimits
      Object@Boundaries$y <<- Ylimits
      point.coords$x <<- Xlimits #set original X range
      point.coords$y <<- Ylimits #set original Y range

      ComponentList <<- names(slot(Object,"Components"))
      if (length(ComponentList)==0) {
          tkmessageBox(message="ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
          return()
      }
      xx <- Object@Components[[1]]@param[2,1] #component position mu
      yy <- Object@Components[[1]]@param[1,1] #component height h
      Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
      Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
      yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
      coords[1] <<- xx
      coords[2] <<- yy
      refresh <<- FALSE #now plot the component marker
      replot()   #replot spectrum and marker of selected fit component
      if (UpdateCompMenu == TRUE){ ComponentMenu() }
  }

  editFitFrame <- function(h,...){
      FComp <- tclvalue(FC)
      FComp <- as.integer(gsub("[^0-9]", "", FComp))   #index selected component
      fitParam <<- Object@Components[[FComp]]@param #load DataFrame relative to the selected component
      fitParam <<- round(fitParam, 4)
      TT <- paste("FIT PARAMETERS: component ", tclvalue(FC),sep="")
      ParNames <- rownames(fitParam)
      idx <- grep("lg", ParNames)
      if(length(idx) > 0){ParNames[idx] <- "Mix.L.G"}
      idx <- grep("gv", ParNames)
      if(length(idx) > 0){ParNames[idx] <- "Mix.G.V"}
      CNames <- c("Start", "Min", "Max")
      fitParam <<- as.matrix(fitParam) #this is needed to construct correctly the data.frame
      fitParam <<- data.frame(fitParam, stringsAsFactors=FALSE) #in the dataframe add a column with variable names
      fitParam <<- DFrameTable(Data=fitParam, Title=TT, ColNames=CNames, RowNames=ParNames,
                               Width=15, Modify=TRUE, Env=environment(), parent=NULL)
#--- Save changes ---
      Object@Components[[FComp]]@param <<- fitParam #save parameters in the slot of XPSSample
      XPSSample[[Indx]] <<- Object
      NComp <- length(Object@Components)
      tmp <- NULL
      for(ii in 1:NComp){
          Object@Components[[ii]] <<- Ycomponent(Object@Components[[ii]], x=Object@RegionToFit$x, y=Object@Baseline$y)
          tmp <- cbind(tmp, Object@Components[[ii]]@ycoor)  #fit is the sum of fitting components
          Object@Fit$y <<- colSums(t(tmp)) - length(Object@Components)*(Object@Baseline$y) #substract NComp*Baseline
      }
      Object <<- sortComponents(Object)
      xx <- Object@Components[[FComp]]@param[2,1] #component position mu
      Rng <- range(Object@RegionToFit[[1]])
      if (xx < Rng[1]) {xx <- Rng[1]}
      if (xx > Rng[2]) {xx <- Rng[2]}
      yy <- Object@Components[[FComp]]@param[1,1] #component height h
      FuncName <- Object@Components[[FComp]]@funcName
      yy <- yy/GetHvalue(Object,FComp, FuncName, 1)  #provides the correct yy value for complex functions
      Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
      Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
      yy <- yy + Object@Baseline$y[Xindx]  #spectral intensity + baseline at point xx
      coords[1] <<- xx   #Marker coords
      coords[2] <<- yy
      assign(activeFName, XPSSample, envir = .GlobalEnv)
      XPSSaveRetrieveBkp("save")
      replot()
      return()
  }

  reset.vars <- function(){
     Indx <<- 2
     assign("activeSpectIndx", 1, envir=.GlobalEnv)
     OldXPSSample <<- XPSSample
     Object <<- XPSSample[[Indx]]
     SpectName <<- NULL
     ComponentList <<- names(Object@Components)
     FNameList <<- XPSFNameList()
     SpectList <<- XPSSpectList(activeFName)
     FComp <<- 1
     if (is.null(FitComp) == FALSE){
         tkdestroy(FitComp)
     }
     UpdateCompMenu <<- TRUE
     coords <<- c(xx=NA, yy=NA, yy_BasLin=NA)
     CompCoords <<- c(xx=NA, yy=NA, yy_BasLin=NA)

     refresh <<- TRUE
     SetZoom <<- FALSE
     NoFit <<- FALSE
     ShowMsg <<- TRUE
     WinSize <<- as.numeric(XPSSettings$General[4])
     hscale <<- as.numeric(WinSize)

     if (length(ComponentList)==0) {
        tkmessageBox(message="ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
        Xlimits <<- range(Object@.Data[1])
        Ylimits <<- range(Object@.Data[2])
        NoFit <<- TRUE
        return()
     }
  }


# --- Variable definitions ---
     XPSSettings <- get("XPSSettings", envir=.GlobalEnv)
     WarnMsg <- XPSSettings$General[9]
     activeFName <- get("activeFName", envir = .GlobalEnv)
     if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
         tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
         return()
     }
     XPSSample <- get(activeFName, envir=.GlobalEnv)       #load XPSdata values from main memory
     if (exists("activeSpectIndx")){
        Indx <- activeSpectIndx
     } else {
        Indx <- 1
     }
     if(activeSpectIndx > length(XPSSample)) { Indx <- 1 }
     OldXPSSample <- XPSSample
     Object <- XPSSample[[Indx]]
     SpectName <- NULL
     ComponentList <- names(Object@Components)
     FNameList <- XPSFNameList()
     SpectList <- XPSSpectList(activeFName)
     FComp <- 1
     FitComp <- list()
     FC <- list()
     UpdateCompMenu <- TRUE
     fitParam <- NULL
     EXIT <- NULL
     
     WinSize <- as.numeric(XPSSettings$General[4])
     hscale <- hscale <- as.numeric(WinSize)

     coords <- c(xx=NA, yy=NA, yy_BasLin=NA)
     CompCoords <- c(xx=NA, yy=NA, yy_BasLin=NA)
     xx <- NULL
     yy <- NULL

     refresh <- TRUE
     SetZoom <- FALSE
     NoFit <- TRUE
     ShowMsg <- TRUE
#Coreline boundaries
     if (length(ComponentList) == 0) {
        tkmessageBox(message="ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
        point.index <- 1
        point.coords <- list(x=range(Object@RegionToFit$x), #set the X window extension == x range
                             y=range(Object@RegionToFit$y)) #set the Y window extension == x range
        Xlimits <- range(Object@.Data[1])
        Ylimits <- range(Object@.Data[2])
        Object@Boundaries$x <- Xlimits
        Object@Boundaries$y <- Ylimits
        Corners <- list(x=NULL, y=NULL)
        NoFit <- TRUE
     } else {
        LL <- length(Object@.Data[[1]])
        point.index <- 1
        point.coords <- list(x=range(Object@RegionToFit$x), #set the X window extension == x range
                             y=range(Object@RegionToFit$y)) #set the Y window extension == x range
        Corners <- list(x=c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2]),
                        y=c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2]))
        Xlimits <- range(Object@RegionToFit$x)
        Ylimits <- range(Object@RegionToFit$y)
        Object@Boundaries$x <- Xlimits
        Object@Boundaries$y <- Ylimits
     }
     plot(Object)
     if (Check.PE() == FALSE) { return() }

#--- Widget ---
     MCWindow <- tktoplevel()
     tkwm.title(MCWindow,"XPS MOVE COMPONENTS")
     tkwm.geometry(MCWindow, "+100+50")   #position respect topleft screen corner
     tkbind(MCWindow, "<Destroy>", function(){
                           EXIT <<- TRUE
                           XPSSettings$General[4] <<- 7      #Reset to normal graphic win dimension
                           assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                           plot(XPSSample[[Indx]])         #replot the CoreLine
                 })

     SelectGroup <- ttkframe(MCWindow, borderwidth=0, padding=c(0,0,0,0) )
     tkgrid(SelectGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

#--- Selection Group ---
     MCFrame1 <- ttklabelframe(SelectGroup, text = " XPS Samples ", borderwidth=2)
     tkgrid(MCFrame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
     XS <- tclVar(activeFName)
     XPS.Sample <- ttkcombobox(MCFrame1, width = 15, textvariable = XS, values = FNameList)
     tkgrid(XPS.Sample, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
     tkbind(XPS.Sample, "<<ComboboxSelected>>", function(){
                           activeFName <<- tclvalue(XS)
                           assign("activeFName", activeFName, envir=.GlobalEnv)
                           XPSSample <<- get(activeFName, envir=.GlobalEnv)
                           reset.vars()
                           tkconfigure(Core.Lines, values = SpectList)
                           refresh <<- FALSE #now plot also the component marker
                           Xlimits <<- range(Object@RegionToFit$x)
                           Ylimits <<- range(Object@RegionToFit$y)
                           Object@Boundaries$x <<- Xlimits
                           Object@Boundaries$y <<- Ylimits
                           plot(Object)
                 })

     MCFrame2 <- ttklabelframe(SelectGroup, text = " Core-Lines ", borderwidth=2)
     tkgrid(MCFrame2, row = 1, column = 1, padx = c(175, 0), pady = 5, sticky="w")
     CL <- tclVar("")
     Core.Lines <- ttkcombobox(MCFrame2, width = 15, textvariable = CL, values = SpectList)
     tkgrid(Core.Lines, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
     tkbind(Core.Lines, "<<ComboboxSelected>>", function(){
                           XPS.CL <- tclvalue(CL)
                           XPS.CL <- unlist(strsplit(XPS.CL, "\\."))
                           Indx <<- as.integer(XPS.CL[1])               #select "NUMber." in component name
                           SpectName <<- XPS.CL[2]
                           Object <<- XPSSample[[Indx]]
                           ComponentList <<- names(slot(Object,"Components"))
                           assign("activeSpectName", SpectName,envir=.GlobalEnv) #set activeSpectName == last selected spectrum
                           assign("activeSpectIndx", Indx,envir=.GlobalEnv) #set the activeIndex == last selected spectrum
                           point.index <<- 1
                           UpdateCompMenu <<- TRUE
                           LoadCoreLine()
                 })

     MCFrame3 <- ttklabelframe(SelectGroup, text = " COMPONENTS ", borderwidth=2)
     tkgrid(MCFrame3, row = 2, column = 1, padx = 5, pady = 5, sticky="we")

     CompLabel <- ttklabel(MCFrame3, text=" \n \n")
     tkgrid(CompLabel, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     MCFrame4 <- ttklabelframe(SelectGroup, text = " OPTIONS ", borderwidth=2)
     tkgrid(MCFrame4, row = 3, column = 1, padx = 5, pady = 5, sticky="we")

     LMFitbutton <- tkbutton(MCFrame4, text=" FIT Lev.Marq. ", width=20, command=function(){
                           FComp <<- tclvalue(FC)
                           if (FComp > "0"){  # "0" == no FComp selected
                               FComp <<- as.numeric(unlist(strsplit(FComp, split="C")))   #index selected component
                               FComp <<- FComp[2]
                           } else {
                               FComp <- 1
                           }
                           Object <<- XPSFitLM(Object, plt=FALSE)
                           xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                           yy <- Object@Components[[FComp]]@param[1,1] #component height h
                           Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                           Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                           yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                           coords[1] <<- xx #coords of marker of the first fit component
                           coords[2] <<- yy
                           Object <<- sortComponents(Object)
                           refresh <<- FALSE  #now plot also the component marker
                           assign("Object", Object, envir = .GlobalEnv)
                           replot()
                 })
     tkgrid(LMFitbutton, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     MFFitbutton <- tkbutton(MCFrame4, text=" FIT Modfit ", width=20, command=function(){
                           Pkgs <- get("Pkgs", envir=.GlobalEnv)
                           FME.PKG <- "FME" %in% Pkgs
                           if( FME.PKG == FALSE ){       #check if the package 'FME' is installed
                               txt <- "Package 'FME' is NOT Installed. \nCannot Execute the 'ModFit' Option"
                               tkmessageBox(message=txt, title="WARNING", icon="error")
                               return()
                           }
                           FComp <<- tclvalue(FC)
                           FComp <<- as.numeric(unlist(strsplit(FComp, split="C")))   #index selected component
                           FComp <<- FComp[2]
                           Object <<- XPSModFit(Object, plt=FALSE)
                           xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                           yy <- Object@Components[[FComp]]@param[1,1] #component height h
                           Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                           Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                           yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                           coords[1] <<- xx
                           coords[2] <<- yy
                           Object <<- sortComponents(Object)
                           refresh <<- FALSE #now plot also the component marker
                           assign("Object", Object, envir = .GlobalEnv)
                           replot()
                 })
     tkgrid(MFFitbutton, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

     ZRbutton <- tkbutton(MCFrame4, text=" SET ZOOM REGION ", width=20, command=function(){
                           CompCoords <<- coords   #save the of position component_marker
                           point.coords <<- NULL   #point.coords contain the X, Y data ranges
                           WidgetState(LMFitbutton, "disabled")
                           WidgetState(MFFitbutton, "disabled")
                           WidgetState(RSTbutton, "disabled")
                           SetZoom <<- TRUE
                           point.coords$x <<- range(Object@RegionToFit$x)
                           point.coords$y <<- range(Object@RegionToFit$y)
                           if (Object@Flags[1]) { #Binding energy set
                               point.coords$x <<- sort(c(point.coords$x[1], point.coords$x[2]), decreasing=TRUE) #pos$x in decreasing order
                           }
                           Xlimits <<- point.coords$x
                           Ylimits <<- point.coords$y
                           Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
                           Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
#                           Marker <<- list(Points=Corners, col=4, cex=1.2, lwd=2.5, pch=3)
                           point.index <<- 3 #plot initial zoom area
                           replot()
                           msg <- paste("\n => Left click near corners to adjust Zoom Region Dimensions",
                                        "\n => When Zoom Region OK, right click and press  MAKE ZOOM", sep="")
                           tkmessageBox(message=msg, title="WARNING", icon="warning")
                           GetCurPos(SingClick=FALSE)
                 })
     tkgrid(ZRbutton, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

     MZbutton <- tkbutton(MCFrame4, text=" MAKE ZOOM ", width=20, command=function(){
                           if (Object@Flags[1]) { #Binding energy set
                               point.coords$x <<- sort(point.coords$x, decreasing=TRUE) #pos$x in decreasing order
                           } else {
                               point.coords$x <<- sort(point.coords$x, decreasing=FALSE) #pos$x in increasing order
                           }
                           Xlimits <<- point.coords$x
                           Ylimits <<- sort(point.coords$y, decreasing=FALSE)
                           slot(Object,"Boundaries") <<- point.coords
                           point.index <<- 1
                           coords <<- CompCoords #restore of position component_marker
                           refresh <<- FALSE
                           SetZoom <<- FALSE
                           assign("Object", Object, envir = .GlobalEnv)
                           replot()
                           WidgetState(LMFitbutton, "normal")
                           WidgetState(MFFitbutton, "normal")
                           WidgetState(RSTbutton, "normal")
                  })
     tkgrid(MZbutton, row = 2, column = 2, padx = 5, pady = 5, sticky="w")

     RSTbutton <- tkbutton(MCFrame4, text=" RESET PLOT ", width=20, command=function(){
                           SetZoom <<- FALSE
                           refresh <<- FALSE
                           point.index <<- 1
                           reset.plot()
                  })
     tkgrid(RSTbutton, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

     RSTbutton <- tkbutton(MCFrame4, text=" UNDO ", width=20, command=function(){
                           FComp <<- tclvalue(FC)
                           FComp <<- as.numeric(unlist(strsplit(FComp, split="C")))   #index selected component
                           FComp <<- FComp[2]
                           XPSSample <<- OldXPSSample
                           Object <<- XPSSample[[Indx]]
                           xx <- Object@Components[[FComp]]@param[2,1] #component position mu
                           yy <- Object@Components[[FComp]]@param[1,1] #component height h
                           Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
                           Xindx <- which(Object@RegionToFit[[1]] > xx-Estep/2 & Object@RegionToFit[[1]] < xx+Estep/2)
                           yy <- yy+Object@Baseline$y[Xindx]  #spectral intensity + Baseline at point xx
                           coords[1] <<- xx
                           coords[2] <<- yy
                           replot()
                  })
     tkgrid(RSTbutton, row = 3, column = 2, padx = 5, pady = 5, sticky="w")

     RSTbutton <- tkbutton(MCFrame4, text=" EDIT PARAMETERS ", width=20, command=function(){
                           editFitFrame()
                  })
     tkgrid(RSTbutton, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

     RLDbutton <- tkbutton(MCFrame4, text=" RE-LOAD DATA ", width=20, command=function(){
                           tclvalue(FC) <- ComponentList[[1]]
                           UpdateCompMenu <<- FALSE
                           LoadCoreLine()
                           tclvalue(FC) <- ComponentList[[1]]
                           OldXPSSample <<- XPSSample
                  })
     tkgrid(RLDbutton, row = 4, column = 2, padx = 5, pady = 5, sticky="w")

     SAVbutton <- tkbutton(MCFrame4, text=" SAVE ", width=20, command=function(){
                           tclvalue(FitComp) <- ""
                           Indx <- get("activeSpectIndx", envir=.GlobalEnv)
                           XPSSample[[Indx]] <<- Object
                           OldXPSSample[[Indx]] <<- XPSSample[[Indx]]
                           assign("Object", XPSSample[[Indx]], envir = .GlobalEnv)
                           assign(activeFName, XPSSample, envir = .GlobalEnv)
                           replot()
                           XPSSaveRetrieveBkp("save")
                           UpdateXS_Tbl()
                  })
     tkgrid(SAVbutton, row = 5, column = 1, padx = 5, pady = 5, sticky="w")

     EXTbutton <- tkbutton(MCFrame4, text=" EXIT ", width=20, command=function(){
                           tkdestroy(MCWindow)    #Disposing MCWindow will activate GDestroyHandler which re-opens the graphic window
                           XPSSample <- get(activeFName, envir=.GlobalEnv) #Update XPSSample with all changes before plotting
                           XPSSaveRetrieveBkp("save")
                           plot(XPSSample[[Indx]])         #replot the CoreLine
                  })
     tkgrid(EXTbutton, row = 5, column = 2, padx = 5, pady = 5, sticky="w")

#--- StatBar-----
     MCFrame5 <- ttklabelframe(SelectGroup, text = " OPTIONS ", borderwidth=2)
     tkgrid(MCFrame5, row = 4, column = 1, padx = 5, pady = 2, sticky="we")
     StatusBar <- ttklabel(MCFrame5, text="x = ...   y = ...", relief="sunken", foreground="blue")
     tkgrid(StatusBar, row = 1, column = 1, padx = 5, pady = 5, sticky="we")

#--- Marker on First FitComp-----
     if (NoFit == FALSE){
        coords[1] <<- Object@Components[[1]]@param[2,1] #component position mu
        coords[2] <<- Object@Components[[1]]@param[1,1] #component1 height h
        FuncName <- Object@Components[[1]]@funcName
        coords[2] <<- coords[2]/GetHvalue(Object,1, FuncName, 1)
        Estep <- abs(Object@RegionToFit[[1]][1]-Object@RegionToFit[[1]][2])
        Xindx <- which(Object@RegionToFit[[1]] > coords[1]-Estep/2 & Object@RegionToFit[[1]] < coords[1]+Estep/2) #indice del vettore X corrispondente alla posizione della componente
        coords[2] <<- coords[2] + Object@Baseline$y[Xindx]
        refresh <- FALSE
        replot()
        refresh <- TRUE
     }

     WidgetState(LMFitbutton, "normal")
     WidgetState(MFFitbutton, "normal")
     WidgetState(RSTbutton, "normal")
     if (length(ComponentList) > 0) {
         ComponentMenu()
     }
}





