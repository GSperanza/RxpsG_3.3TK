## =====================================================
## VBtop: function to compute the upper edge of the HOMO
## =====================================================

#' @title XPSVBTop
#' @description XPSVBTop function to estimate the position of the Valence Band Top
#'   the interactive GUI adds a BaseLines and Fitting components to
#'   the region of the VB proximal to the Fermi Edge needed
#'   for the estimation of the VB-Top position
#' @examples
#' \dontrun{
#'  XPSVBTop()
#' }
#' @export
#'                             

XPSVBTop <- function() {

  GetCurPos <- function(SingClick){
       coords <<- NULL
       WidgetState(T1Frame1, "disabled")
       WidgetState(T21Frame1, "normal")
       WidgetState(T22Frame1, "normal")
       WidgetState(T22Frame2, "normal")
       WidgetState(T23Frame1, "normal")
       WidgetState(BtnGroup, "disabled")
       EXIT <- FALSE
       while(EXIT == FALSE){
            pos <- locator(n=1)
            if (is.null(pos)) {
                WidgetState(T1Frame1, "normal")
                WidgetState(T21Frame1, "normal")
                WidgetState(T22Frame1, "normal")
                WidgetState(T22Frame2, "normal")
                WidgetState(T23Frame1, "normal")
                WidgetState(BtnGroup, "normal")
                EXIT <- TRUE
            } else {
                if ( SingClick ){ 
                    coords <<- c(pos$x, pos$y)
                    WidgetState(T1Frame1, "normal")
                    WidgetState(T21Frame1, "normal")
                    WidgetState(T22Frame1, "normal")
                    WidgetState(T22Frame2, "normal")
                    WidgetState(T23Frame1, "normal")
                    WidgetState(BtnGroup, "normal")
                    EXIT <- TRUE
                } else {
                    Xlim1 <- min(range(Object[[coreline]]@.Data[[1]]))   #limits coordinates in the Spectrum Range
                    Xlim2 <- max(range(Object[[coreline]]@.Data[[1]]))
                    Ylim1 <- min(range(Object[[coreline]]@.Data[[2]]))
                    Ylim2 <- max(range(Object[[coreline]]@.Data[[2]]))

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
     tab1 <- as.numeric(tclvalue(tcl(nbMain, "index", "current")))+1  #retrieve  nbMain page index
     tab2 <- as.numeric(tclvalue(tcl(nbVBfit, "index", "current")))+1 #retrieve nbVBfit page index
#--- define point.coords
     if (is.null(point.coords$x) && VBlimOK == FALSE) {
        if (hasBoundaries == FALSE) {
           Object[[coreline]]@Boundaries$x <- range(Object[[coreline]]@.Data[[1]])
           Object[[coreline]]@Boundaries$y <- range(Object[[coreline]]@.Data[[2]])
        }
        point.coords <<- Object[[coreline]]@Boundaries  #point.coord list was reset
     }
     if (coreline != 0 && tab1 == 1) { #coreline != "All Spectra" and tab Baseline
        xx <- coords[1]
        yy <- coords[2]
        if (! is.na(point.coords$x[1]) ) {
# Crtl which marker position at VB ends has to be changed
           tol.x <- abs(diff(point.coords$x)) / 25
           tol.y <- abs(diff(point.coords$y)) / 25
           d.pts <- (point.coords$x - xx)^2 #+ (point.coords$y - yy)^2   #distance between mouse position and initial marker position
           point.index <- min(which(d.pts == min(d.pts)))  #which of the two markers has to be moved in the new position?
        } else {
           point.index <- 1
        }
        point.coords$x[point.index] <<- xx
        point.coords$y[point.index] <<- yy
     }
#---make plot changes upon mouse position and option selection
     if (is.null(point.coords$x) && VBlimOK == FALSE) { point.coords <<- Object[[coreline]]@Boundaries } #point.coord list was reset
     if (coreline != 0 && tab1 == 1) {   #coreline != "All spectra"  and Baseline tab
         point.coords$x[point.index] <<- coords[1]
         point.coords$y[point.index] <<- coords[2]
         tab1 <- as.numeric(tclvalue(tcl(nbMain, "index", "current")))+1  #retrieve  nbMain page index
         tab2 <- as.numeric(tclvalue(tcl(nbVBfit, "index", "current")))+1 #retrieve nbVBfit page index
         if (tab1 == 1 && BType=="linear") {    ### notebook tab Baseline
            point.coords$y[1] <<- point.coords$y[2]   #keep linear BKG alligned to X
         }
         slot(Object[[coreline]],"Boundaries") <<- point.coords
         MakeBaseline(deg, splinePoints)  #modify the baseline
         if (VBbkgOK==FALSE){ #we are still modifying the Shirley baseline
            LL <- length(Object[[coreline]]@.Data[[1]])
            VBintg <<- sum(Object[[coreline]]@RegionToFit$y - Object[[coreline]]@Baseline$y)/LL #Integral of BKG subtracted VB / number of data == average intensity of VB points
         }
         replot()
     }
     if (tab1 == 2 && tab2 == 1) { ### tab=VB Fit, Linear Fit
         if (coreline == 0) {
            tkmessageBox(message="Please select te VB spectrum", title = "WARNING: WRONG CORELINE SELECTION",  icon = "warning")
         }
         if (VBlimOK==TRUE) {
            point.coords$x <<- c(point.coords$x, coords[1])
            point.coords$y <<- c(point.coords$y, coords[2])
            replot()
         } else {
            tkmessageBox(message="Region proximal to Fermi not defined! ", title = "LIMITS FOR VB LINEAR FIT NOT CONFIRMED",  icon = "warning")
            return()
         }
     }
     if (tab1 == 2 && tab2 == 2) { ### tab=VB Fit, NON-Linear Fit
         if (coreline == 0) {
            tkmessageBox(message="Please select the VB spectrum", title = "WARNING: WRONG CORELINE SELECTION",  icon = "warning")
         }
#         point.coords$x <<- c(point.coords$x, coords[1])
#         point.coords$y <<- c(point.coords$y, coords[2])
         point.coords$x <<- coords[1]
         point.coords$y <<- coords[2]
         add.FitFunct()
         replot()
     }
     if (tab1 == 2 && tab2 == 3) { ### tab=VB Fit, Hill Sigmoid Fit
         if (coreline == 0) {
            tkmessageBox(message="Please select the VB spectrum", title = "WARNING: WRONG CORELINE SELECTION",  icon = "warning")
         }
         if (VBlimOK==FALSE) {
            tkmessageBox(message="Region proximal to Fermi not defined! ", title = "LIMITS FOR VB HILL SIGMOID FIT NOT CONFIRMED",  icon = "warning")
            return()
         }
         point.coords$x <<- c(point.coords$x, coords[1])
         point.coords$y <<- c(point.coords$y, coords[2])
         replot()
     }
     return()
  }

  replot <- function(...) {
     tab1 <- as.numeric(tclvalue(tcl(nbMain, "index", "current")))+1  #retrieve  nbMain page index
     tab2 <- as.numeric(tclvalue(tcl(nbVBfit, "index", "current")))+1 #retrieve nbVBfit page index
     if (coreline == 0) {     # coreline == "All spectra"
         plot(Object)
     } else {
        if (tab1 == 1) {  ### tab1 Baseline
            if (tclvalue(ZM) == "1") {
               lastX <- length(Object[[coreline]][[2]])
               baseline.ylim <- c( min(Object[[coreline]][[2]]),
                                2*max( c(Object[[coreline]][[2]][1], Object[[coreline]][[2]][lastX]) ) )
               plot(Object[[coreline]], ylim=baseline.ylim)
               points(point.coords, col="red", cex=SymSiz, lwd=1.5, pch=MarkSym)
            } else {
               plot(Object[[coreline]])     #plots the Baseline limits
               points(point.coords, col="red", cex=SymSiz, lwd=1.5, pch=MarkSym)
            }
        } else if ((tab1 == 2) && (tab2==1) ){ ### tab VB Fit, Linear Fit
            Xrng <- range(Object[[coreline]]@RegionToFit$x)
            Yrng <- range(Object[[coreline]]@RegionToFit$y)
            plot(Object[[coreline]], xlim=Xrng, ylim=Yrng)  #plot confined in the original X, Y range
            if (length(point.coords$x) > 0 && VBtEstim == FALSE) { #Points defining the 2 regions for the linear fit
                points(point.coords, col="green", cex=1.2, lwd=2, pch=3)
            }
            if (VBtEstim == TRUE) {     #Point defining the intercept of the two linear fit
                points(point.coords$x, point.coords$y, col="orange", cex=3, lwd=2, pch=3)
            }
        } else if ((tab1 == 2) && (tab2==2) ){ ### tab VB Fit, NON-Linear Fit
            if (tclvalue(PLTF) == "residual" && hasFit(Object[[coreline]])) {
                XPSresidualPlot(Object[[coreline]])
            }
            if (VBtEstim == FALSE){
                plot(Object[[coreline]])
                points(point.coords, col="green", cex=1.2, lwd=2, pch=3)       #plots the point where to add the component
            }
            if (VBtEstim == TRUE) {
                plot(Object[[coreline]])
                points(point.coords, col="orange", cex=3, lwd=2, pch=3)  #plots the VB top
            }
        } else if ((tab1 == 2) && (tab2==3) ){ ### tab VB Fit, Hill Sigmoid Fit
            if (tclvalue(PLTF) == "residual" && hasFit(Object[[coreline]])) {
                XPSresidualPlot(Object[[coreline]])
            }
            if (VBtEstim == FALSE){
                plot(Object[[coreline]])
                points(point.coords, col="green", cex=1.2, lwd=2, pch=3)       #plots the point where to add the component
            }
            if (VBtEstim == TRUE) {
                plot(Object[[coreline]])
                points(point.coords, col="orange", cex=3, lwd=2, pch=3)    #plots the VB top
            }
        }
     }
  }

  LoadCoreLine <- function(h, ...){
     Object_name <- get("activeFName", envir=.GlobalEnv)
     Object <<- get(Object_name, envir=.GlobalEnv)  #load the XPSSample from the .Global Environment
     ComponentList <- names(slot(Object[[coreline]],"Components"))
     if (length(ComponentList)==0) {
         tkmessageBox(message="ATTENTION NO FIT FOUND: change coreline please!" , title = "WARNING",  icon = "warning")
         return()
     }
     replot()   #replot spectrum of the selected component
  }


  set.coreline <- function(h, ...) {
     CL <- tclvalue(CL)
     CL <- unlist(strsplit(CL, "\\."))   #select the NUMBER. before the CoreLine name
     coreline <<- as.integer(CL[1])

     if (coreline == 0) {    #coreline == "All spectra"
         tclvalue(PLTF) <- "normal"
         WidgetState(T1Frame1, "disabled")
         WidgetState(T21Frame1, "disabled")
         WidgetState(T22Frame1, "disabled")
         WidgetState(T22Frame2, "disabled")
         WidgetState(T23Frame1, "disabled")
         plot(Object)
         tkmessageBox(message="Please select a Valence Band spectrum", title="WRONG SPECTRUM", icon="error")
         return()
     } else {
         if (length(Object[[coreline]]@Components) > 0) {
             tkmessageBox(message="Analysis already present on this Coreline!", title = "WARNING: Analysis Done",  icon = "warning")
             return()
         }
         VBtEstim <<- FALSE
         WidgetState(T1Frame1, "normal")
         WidgetState(OK_btn1, "normal")
         WidgetState(OK_btn2, "disabled")

# Now computes the VB integral needed for the VBtop estimation by NON-Linear Fit
# By default a Shirley baseline is defined on the whole VB
         if (length(Object[[coreline]]@Baseline$x) != 0 ) {
             reset.baseline() 
         }
         Object[[coreline]]@RSF <<- 0 #set the VB sensitivity factor to zero to avoid error wornings

# reset zoom
         tclvalue(ZM) <- "0"
# if boundaries already defined
         if (hasBoundaries(Object[[coreline]])) {
             point.coords <<- slot(Object[[coreline]],"Boundaries")
         } else {
             reset.baseline()
         }
# enable notebook pages
         if (hasBaseline(Object[[coreline]]) ) {
             tcl(nbMain, "select", 0)  #select first page
         }
         if (hasComponents(Object[[coreline]]) ) {
             if (VBbkgOK==TRUE) { WidgetState(T2group1, "normal") }   #enable VB-fit tab
             tcl(nbMain, "select", 1)   #select second nbMain page
             tcl(nbVBfit, "select", 0)  #select first nbVBfit page

         }
     }
     ObjectBKP <<- Object[[coreline]]
     tcl(nbMain, "select", 0)  #when a coreline is selected, Baseline NB page is selected
     replot()
  }


  MakeBaseline <- function(deg,splinePoints, ...){
     if ( coreline != 0 && hasBoundaries(Object[[coreline]]) ) {
        Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
        Object[[coreline]] <<- XPSbaseline(Object[[coreline]], BType, deg, splinePoints )
        Object[[coreline]] <<- XPSsetRSF(Object[[coreline]])
        if (VBbkgOK==TRUE && VBlimOK==TRUE) { WidgetState(T2group1, "normal") }   #abilito VB-fit tab
        replot()
     }
     tcl(nbVBfit, "select", 0)  #select first nbVBfit page
  }


  reset.baseline <- function(h, ...) {
     if (coreline != 0) {   #coreline != "All spectra"
         LL <- length(Object[[coreline]]@.Data[[1]])
         if (BType == "Shirley"){
             Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
             Object[[coreline]]@Boundaries$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
             point.coords$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
             point.coords$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
             Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
             Object[[coreline]] <<- XPSbaseline(Object[[coreline]], "Shirley", deg, splinePoints )
             VBintg <<- sum(Object[[coreline]]@RegionToFit$y - Object[[coreline]]@Baseline$y)/LL #Integral of BKG subtracted VB / number of data == average intensity of VB points
         }
         if (BType == "linear"){
             Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
             Object[[coreline]]@Boundaries$y <<- c(Object[[coreline]]@.Data[[2]][LL], Object[[coreline]]@.Data[[2]][LL])
             point.coords$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
             point.coords$y <<- c(Object[[coreline]]@.Data[[2]][LL], Object[[coreline]]@.Data[[2]][LL])
             Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
             Object[[coreline]] <<- XPSbaseline(Object[[coreline]], "linear", deg, splinePoints )
         }
         WidgetState(T21Frame1, "disabled")
         WidgetState(T22Frame1, "disabled")
         WidgetState(T22Frame2, "disabled")
         WidgetState(T23Frame1, "disabled")
     }
  }


  update.outputArea <- function(...) {
     coreline <<- tclvalue(CL)
     coreline <<- unlist(strsplit(coreline, "\\."))   #drops the NUMBER. before the CoreLine name
     coreline <<- as.integer(coreline[1])
  }


#--- Functions, Fit and VB_Top estimation

  reset.LinRegions <- function(h, ...) {
     point.coords <<- list(x=NULL, y=NULL)
     Object[[coreline]]@Components <<- list()
     Object[[coreline]]@Fit <<- list()
     replot()
  }


  add.FitFunct <- function(h, ...) {
     ObjectBKP <<- Object[[coreline]]
     tab2 <- as.numeric(tclvalue(tcl(nbVBfit, "index", "current")))+1 #retrieve nbVBfit page index
     if (coreline != 0 && hasBaseline(Object[[coreline]])) {
         Xrange <- Object[[coreline]]@Boundaries$x
         Sigma <- abs(Xrange[2] - Xrange[1])/7
         if (!is.null(point.coords$x[1]) && tab2 == 2 ) {   #NON-Linear Fit
#Fit parameter are set in XPSAddComponent()
             Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = tclvalue(FITF),
                                             peakPosition = list(x = point.coords$x, y = point.coords$y), sigma=Sigma)
## to update fit remove Component@Fit and make the sum of Component@ycoor including the newone
             tmp <- sapply(Object[[coreline]]@Components, function(z) matrix(data=z@ycoor))  #create a matrix formed by ycoor of all the fit Components
             CompNames <<- names(Object[[coreline]]@Components)
             Object[[coreline]]@Fit$y <<- ( colSums(t(tmp)) - length(Object[[coreline]]@Components)*(Object[[coreline]]@Baseline$y)) #Subtract NComp*Baseline because for each Component a baseline was added
             point.coords <<- list(x=NULL,y=NULL)
             replot()
         }
         if (!is.null(point.coords$x[1]) && tab2 == 3 ) { #Hill Sigmoid Fit
#Fit parameter are set in XPSAddComponent()
             if(length(point.coords$x) > 3){
                tkmessageBox(message="Attention: more than the Max, Flex, Min points were defined. Only the first three points will be taken", title="WARNING", icon="warning")
                point.coords$x <<- point.coords$x[1:3]
                point.coords$y <<- point.coords$y[1:3]
             }
             if(Object[[coreline]]@Flags[[2]] == TRUE){ #Binding energy
                idx <- order(point.coords$x, decreasing=TRUE)
                point.coords$x <<- point.coords$x[idx]  #point.coords could be entered in sparse order
                point.coords$y <<- point.coords$y[idx]  #here we will have MAX, FLEX, MIN positions
             } else {                                   #Kinetic energy
                idx <- order(point.coords$x, decreasing=FALSE)
                point.coords$x <<- point.coords$x[idx]
                point.coords$y <<- point.coords$y[idx]
             }
             Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = "HillSigmoid",
                                             peakPosition = list(x = point.coords$x, y = point.coords$y), ...)
             Object[[coreline]]@Fit$y <<- Object[[coreline]]@Components[[1]]@ycoor-Object[[coreline]]@Baseline$y #subtract the Baseline
             point.coords <<- list(x=NULL,y=NULL)
             Object[[coreline]]@RegionToFit$x <- ObjectBKP@RegionToFit$x #restore original abscissas changed in XPSAddComponent()
             replot()
         }
     }
  }


  del.FitFunct <- function(h, ...) {  #title="DELETE COMPONENT KILLS CONSTRAINTS!!!"
     ObjectBKP <<- Object[[coreline]]
     answ <- tkmessageBox(message="Deleting fit function. Are you sure you want to proceed?",
                      type="yesno", title="DELETE", icon="warning") 
     if (tclvalue(answ == "yes")) {
         LL<-length(Object[[coreline]]@Components)
         for (ii in 1:LL) { #Rimuovo tutti i CONSTRAINTS
              Object[[coreline]] <<- XPSConstrain(Object[[coreline]],ii,action="remove",variable=NULL,value=NULL,expr=NULL)
         }
         if (coreline != 0 && hasComponents(Object[[coreline]])) {
             delWin <- tktoplevel()
             tkwm.title(delWin,"DELETE FIT COMPONENT")
             tkwm.geometry(delWin, "+200+200")   #position respect topleft screen corner
             FCgroup <- ttkframe(delWin, borderwidth=0, padding=c(0,0,0,0) )
             tkgrid(FCgroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
             txt <- c("Select the fit component to delete")
             tkgrid( ttklabel(FCgroup, text=txt, font="Serif, 11, normal"),
                     row = 1, column = 1, padx = 5, pady = 5, sticky="w")
             tkSep <- ttkseparator(FCgroup, orient="horizontal")
             tkgrid(tkSep, row = 2, column = 1, padx = 5, pady = 5, sticky="we")
             FitCompList <- c(names(Object[[coreline]]@Components), "All")
             compIdx <- tclVar()
             SelectFC <- ttkcombobox(FCgroup, width = 15, textvariable = compIdx, values = FitCompList)
             tkgrid(SelectFC, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

             OKBtn <- tkbutton(FCgroup, text="  OK  ", width=15, command=function(){
                            if (tclvalue(compIdx) != "All"){
                                indx <- as.numeric(tclvalue(compIdx))
                                Object[[coreline]] <<- XPSremove(Object[[coreline]], what="components", number=indx )
                                if (length(Object[[coreline]]@Components) > 0 ) {
                                    #to update the fit:
                                    tmp <- sapply(Object[[coreline]]@Components, function(z) matrix(data=z@ycoor))
                                    Object[[coreline]]@Fit$y <<- ( colSums(t(tmp)) - length(Object[[coreline]]@Components)*(Object[[coreline]]@Baseline$y))
                                }
                            } else {
                                Object[[coreline]] <<- XPSremove(Object[[coreline]], "components")
                            }
                            tclvalue(PLTF) <- "normal"
                            point.coords <<- list(x=NULL,y=NULL)
                            replot()
                            tkdestroy(delWin)
                        })
             tkgrid(OKBtn, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

             CloseBtn <- tkbutton(FCgroup, text=" CLOSE ", width=15, command=function(){
                            tkdestroy(delWin)
                        })
             tkgrid(CloseBtn, row = 4, column = 2, padx = 5, pady = 5, sticky="w")
         }
     }
  }

  Edit.FitParam <- function(h, ...) { #Edit Fit parameters to set constraints on fit components
     FitParam <- NULL
     newFitParam <- NULL
     indx <- NULL

     EditWin <- tktoplevel()
     tkwm.title(EditWin,"EDIT FIT PARAMETERS")
     tkwm.geometry(EditWin, "+200+200")   #position respect topleft screen corner
     FitCompframe <- ttklabelframe(EditWin, text = " Select the Fit Component To Edit ", borderwidth=2)
     tkgrid(FitCompframe, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     FitCompList <- names(Object[[coreline]]@Components)
     compIdx <- tclVar()
     SelectFC <- ttkcombobox(FitCompframe, width = 15, textvariable = compIdx, values = FitCompList)
     tkgrid(SelectFC, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
     tkbind(SelectFC, "<<ComboboxSelected>>", function(){
                          indx <- as.numeric(tclvalue(compIndx))
                          FitParam <-Object[[coreline]]@Components[[indx]]@param #Load parameters in a Dataframe correspondent to the selected coreline
                          VarNames <- rownames(FitParam)  #extract parameter names
                          FitParam <- as.matrix(FitParam) #transform the dataframe in a marix
                          FitParam <<- data.frame(cbind(FitParam), stringsAsFactors=FALSE)  #add varnames in the first column of the paramMatrix and make resave data in a Dataframe to enable editing
                          newFitParam <<- FitParam
                          ClearWidget(GDFGroup)
                          FitParam <<- DFrameTable(Data="FitParam", Title="Fit Comp. Parameters", ColNames=c("Start", "min", "max"),
                                                   RowNames=VarNames, Width=10, Env=environment(), parent=GDFGroup, Row=1, Column=1, Border=c(10, 10, 10, 10))
                    })

     GDFGroup <- ttkframe(FitCompframe, borderwidth=0, padding=c(0,0,0,0) )
     tkgrid(GDFGroup, row = 2, column = 1, padx = 0, pady = 0, sticky="w")
     tkgrid( ttklabel(GDFGroup, text=" \n \n \n"),   # just spaces
                     row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     ExitBtn <- tkbutton(EditWin, text=" EXIT ", width=15, command=function(){
                         tkdestroy(EditWin)
                    })
     tkgrid(ExitBtn, row = 2, column = 1, padx = 0, pady = 0, sticky="w")
  }


  MakeFit <- function(h, ...) {
     ObjectBKP <<- Object[[coreline]]
     FitRes <- NULL
     tab1 <- as.numeric(tclvalue(tcl(nbMain, "index", "current")))+1  #retrieve  nbMain page index
     tab2 <- as.numeric(tclvalue(tcl(nbVBfit, "index", "current")))+1 #retrieve nbVBfit page index

     if (coreline != 0 && tab2 == 1) {  #VB Linear Fit
         if (length(point.coords$x)<4) {
             tkmessageBox(message="4 points are needed for two Linear fits: please complete!", title = "WARNING: region limits lacking",  icon = "warning")
             return()
         }
         ###First Linear fit considered as component to compute the VB Top
         Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = "Linear",
                                             peakPosition = list(x = NA, y = NA), ...)
         #restrict the RegionToFit to the FIRST rengion selected with mouse for the linear fit
         idx1 <- findXIndex(Object[[coreline]]@RegionToFit$x, point.coords$x[1]) #Inside object@RegionToFit$x extract the region between selected points: limit1
         idx2 <- findXIndex(Object[[coreline]]@RegionToFit$x, point.coords$x[2]) #Inside object@RegionToFit$x extract the region between selected points: limit2
         tmp <- sort(c(idx1, idx2), decreasing=FALSE)   #maybe the definition of the fit region is from low to high BE
         idx1 <- tmp[1]
         idx2 <- tmp[2]
         X <- Object[[coreline]]@RegionToFit$x[idx1:idx2]
         Y <- Object[[coreline]]@RegionToFit$y[idx1:idx2]
         YpltLim <- max(range(Object[[coreline]]@RegionToFit$y))/5
         #Linear Fit
         Fit1 <- FitLin(X,Y)  #Linear Fit returns c(m, c) (see XPSUtilities.r)
         LL <- length(Object[[coreline]]@RegionToFit$x)
         for(ii in 1:LL){
             FitRes[ii] <- Fit1[1]*Object[[coreline]]@RegionToFit$x[ii]+Fit1[2]
             if (FitRes[ii] < -YpltLim) { FitRes[ii] <- NA  }   #to limit the Yrange to positive values in the plots
         }
         #store fit1 values
         Object[[coreline]]@Components[[1]]@param["m", "start"] <<- Fit1[1]
         Object[[coreline]]@Components[[1]]@param["c", "start"] <<- Fit1[2]
         Object[[coreline]]@Components[[1]]@ycoor <<- FitRes #-Object[[coreline]]@Baseline$y   #Baseline has to be subtracted to match the orig. data

         ###Second Linear fit considered as component to compute the VB Top
         Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = "Linear",
                                             peakPosition = list(x = NA, y = NA), ...)

         #restrict the RegionToFit to the SECOND rengion selected with mouse for the linear fit
         idx1 <- findXIndex(Object[[coreline]]@RegionToFit$x, point.coords$x[3]) #All-interno di object@RegionToFit$x estraggo la regione selezionata per i fit lineare: estremo1
         idx2 <- findXIndex(Object[[coreline]]@RegionToFit$x, point.coords$x[4]) #All-interno di object@RegionToFit$x estraggo la regione selezionata per i fit lineare: estremo2
         tmp <- sort(c(idx1, idx2), decreasing=FALSE)   #maybe the definition of the fit region is from low to high BE
         idx1 <- tmp[1]
         idx2 <- tmp[2]

         X <- Object[[coreline]]@RegionToFit$x[idx1:idx2]
         Y <- Object[[coreline]]@RegionToFit$y[idx1:idx2]
         #Linear Fit
         Fit2 <- FitLin(X,Y)  #Linear Fit returns c(m, c) (see XPSUtilities.r)
         LL <- length(Object[[coreline]]@RegionToFit$x)
         for(ii in 1:LL){
            FitRes[ii] <- Fit2[1]*Object[[coreline]]@RegionToFit$x[ii]+Fit2[2]
            if (FitRes[ii] < -YpltLim) { FitRes[ii] <- NA  }   #to limit the Yrange to positive values in the plots
         }
         #store  fit2 values
         Object[[coreline]]@Components[[2]]@param["m", "start"] <<- Fit2[1]
         Object[[coreline]]@Components[[2]]@param["c", "start"] <<- Fit2[2]
         Object[[coreline]]@Components[[2]]@ycoor <<- FitRes #-Object[[coreline]]@Baseline$y   #Baseline has to be subtracted to match the orig. data
         Object[[coreline]]@Fit$y <- FitRes
         replot()   #plot of the two linear fits
     }
     if (coreline != 0 && tab2 == 2) {  #VB NON-Linear Fit
         if (reset.fit==FALSE){
             NComp <- length(Object[[coreline]]@Components)
             for(ii in 1:NComp){
                 Object[[coreline]]@Components[[ii]]@param["sigma", "min"] <- 0.5 #limits the lower limit of component FWHM
             }
             Xbkp <- Object[[coreline]]@RegionToFit$x  #save the original X coords = RegionToFit$x
#Fit parameter are set in XPSAddComponent()
             Object[[coreline]] <<- XPSFitLM(Object[[coreline]], plt=FALSE, verbose=FALSE)  #Lev.Marq. fit returns all info stored in Object[[coreline]]
             Object[[coreline]]@RegionToFit$x <<- Xbkp
             replot()
         } else if (reset.fit==TRUE){
             Object[[coreline]] <<- XPSremove(Object[[coreline]],"fit")
             Object[[coreline]] <<- XPSremove(Object[[coreline]],"components")
             reset.fit <<- FALSE
             replot()
         }
     }
     if (coreline != 0 && tab2 == 3) {  #VB Hill Sigmoid
         if (reset.fit == FALSE){
#Fit parameter and new X coords are set in XPSAddComponent()
#HillSigmoid was defined using the new X coords
#New X coords must be used also for the fit

             LL <- length(Object[[coreline]]@RegionToFit$x)
             dx <- (Object[[coreline]]@RegionToFit$x[2]-Object[[coreline]]@RegionToFit$x[1])
             Xbkp <- Object[[coreline]]@RegionToFit$x  #save the original X coords

#Hill sigmoid defined only for positive X abscissas. Then (i)generate a temporary X array
#(ii)generate the  Hill sigmoid. (iii)Perform fitting (iv)restore the original X values
#compute the HillSigmoid position MU on the original abscissas
             Object[[coreline]]@RegionToFit$x <<- Object[[coreline]]@Fit$x  #Set sigmoid Xcoords as modified in XPSAddFitComp()

             Object[[coreline]] <<- XPSFitLM(Object[[coreline]], plt=FALSE, verbose=FALSE)   #Lev.Marq. fit returns all info stored in Object[[coreline]]
             FlexPos <- Object[[coreline]]@Components[[1]]@param[2,1] #MU = fitted flex position HillSigmoid position
             Object[[coreline]]@Fit$idx <<- FlexPos
#temporary abscissa X = seq(1,lenght(RegToFit))
#Observe that in the temporary abscissaa, each X represents both the value and the X-index
#FlexPos*dx represents how many dx are needed to reach MU starting form X[1]

             Object[[coreline]]@RegionToFit$x <<- Xbkp  #restore the original X coods
#now compute the HS position on the original X abscissas
             FlexPos <- Object[[coreline]]@RegionToFit$x[1]+dx*(FlexPos-1)
             Object[[coreline]]@Components[[1]]@param[2,1] <<- FlexPos #save Hill Sigmoid position
             replot()
         } else if (reset.fit == TRUE){
             Object[[coreline]] <<- XPSremove(Object[[coreline]],"fit")
             Object[[coreline]] <<- XPSremove(Object[[coreline]],"components")
             reset.fit <<- FALSE
             replot()
         }
     }
  }

  CalcVBTop <- function(h, ...) {
     tab1 <- as.numeric(tclvalue(tcl(nbMain, "index", "current")))+1  #retrieve  nbMain page index
     tab2 <- as.numeric(tclvalue(tcl(nbVBfit, "index", "current")))+1 #retrieve nbVBfit page index
     if ((tab1 == 2) && (tab2 == 1) ){ ##VB Fit tab, Linear Fit
         #recover linear fit1, fit2 parameters
         Fit2 <- Fit1 <- c(NULL, NULL)
         Fit1[1] <- Object[[coreline]]@Components[[1]]@param["m", "start"]
         Fit1[2] <- Object[[coreline]]@Components[[1]]@param["c", "start"]
         Fit2[1] <- Object[[coreline]]@Components[[2]]@param["m", "start"]
         Fit2[2] <- Object[[coreline]]@Components[[2]]@param["c", "start"]
         #Fit intersection occurs at x==
         VBtopX <- (Fit2[2]-Fit1[2])/(Fit1[1]-Fit2[1])
         idx1 <- findXIndex(Object[[coreline]]@RegionToFit$x,VBtopX)
         #estimation the value of VB corresponding to VBtopX:
         dX <- Object[[coreline]]@RegionToFit$x[idx1+1]-Object[[coreline]]@RegionToFit$x[idx1]
         dY <- Object[[coreline]]@RegionToFit$y[idx1+1]-Object[[coreline]]@RegionToFit$y[idx1]
         #VBtopX falls between RegToFit[idx1] and RegToFit[idx+1]: VBtopY found through proportionality relation
         VBtopY <- dY*(VBtopX-Object[[coreline]]@RegionToFit$x[idx1])/dX+Object[[coreline]]@RegionToFit$y[idx1]
         point.coords$x <<- VBtopX
         point.coords$y <<- VBtopY
         VBtopX <- round(VBtopX, 3)
         VBtopY <- round(VBtopY, 3)
         txt <- paste("Estimated position of VB top:  x=", VBtopX, "  y=", VBtopY, sep="  ")
         tkconfigure(StatusBar, text = txt)
         cat("\n",txt)
         #creation of component3 of type VBtop to store VBtop position
         Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = "VBtop", ...)
         LL <- length(Object[[coreline]]@Baseline$x)
         #VBtop is stored in component3  param mu
         Object[[coreline]]@Components[[3]]@param["mu", "start"] <<- VBtopX
         Object[[coreline]]@Components[[3]]@param["h", "start"] <<- VBtopY
         Object[[coreline]]@Info <<- paste("   ::: VBtop: x=", VBtopX,"  y=", VBtopY, sep="")
     }
     if ((tab1 == 2) && (tab2 == 2) ){ #VB Fit tab, NON-Linear Fit
         VBTop <<- TRUE #set the VBTop graphic mode (see draw.plot()
         if (length(Object[[coreline]]@Fit)==0 ) { #No fit present: Object[[coreline]]@Fit$y is lacking
             tkmessageBox(message="VB NON-Linear Fitting is missing!", title = "WARNING: VB NON-Linear FIT",  icon = "warning")
             return()
         } else if ( coreline != 0 && hasComponents(Object[[coreline]]) ) {
         ## Control on the extension of the VB above the Fermi

#             VBtresh <- VBintg/5   #define a treshold for VBtop estimation
             VBtresh <- (max(Object[[coreline]]@Fit$y)-min(Object[[coreline]]@Fit$y))/10
             LL <- length(Object[[coreline]]@Fit$y)
             for(idxTop in LL:1){ #scan the VBfit to find where the spectrum crosses the threshold
                if (Object[[coreline]]@Fit$y[idxTop] >= VBtresh) break
             }
             VBtopX <- Object[[coreline]]@RegionToFit$x[idxTop]  #abscissa from Region to Fit
             VBtopY <- Object[[coreline]]@RegionToFit$y[idxTop]  #ordinata from Fit
             point.coords$x <<- VBtopX
             point.coords$y <<- VBtopY
             replot()
             VBTop <<- FALSE
             VBtopX <- round(VBtopX, 3)
             VBtopY <- round(VBtopY, 3)
             txt <- paste("Estimated position of VB top:  x=", VBtopX, "  y=", VBtopY, sep="  ")
             tkconfigure(StatusBar, text = txt)
             cat("\n",txt)
             # now add a component to store VBtop Position in param mu
             Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = "VBtop", ...)
             LL <- length(Object[[coreline]]@Components)
             Object[[coreline]]@Components[[LL]]@param["mu", "start"] <<- VBtopX  # VBtop stored in param "mu"
             Object[[coreline]]@Components[[LL]]@param["h", "start"] <<- VBtopY  # VBtop stored in param "mu"
             Object[[coreline]]@Info <<- paste("   ::: VBtop: x=", VBtopX,"  y=", VBtopY, sep="")
         }
     }
     if ((tab1 == 2) && (tab2 == 3) ){ #VB Fit tab, Hill Sigmoid Fit
         LL <- length(Object)
         dx <- (Object[[coreline]]@RegionToFit$x[2]-Object[[coreline]]@RegionToFit$x[1])
         VBTop <<- TRUE      #set the VBTop graphic mode (see draw.plot()
         mu <- Object[[coreline]]@Components[[1]]@param[2,1]
         pow <- Object[[coreline]]@Components[[1]]@param[3,1]
         A <- Object[[coreline]]@Components[[1]]@param[4,1]
         B <- Object[[coreline]]@Components[[1]]@param[5,1]
         TmpMu <- Object[[coreline]]@Fit$idx   #MU position on the temporary X scale
#Now computes MU*(1-2/pow) = knee position of the HS curve. See Bartali et al Mater Int. (2014), 24, 287
#This position has to be computed on the temporary X scale (is positive and increasing)
         TmpVtop <- TmpMu*(1+2/pow)            #bottom knee position of the Hill sigmoid
         idx <- as.integer(TmpVtop)
         bgnd <- Object[[coreline]]@Baseline$y[idx] #baseline value at the TmpVtop point
         VBtopX <- Object[[coreline]]@RegionToFit$x[1]+dx*(TmpVtop-1) #knee position on the original scale
         VBtopY <- A - A*TmpVtop^pow/(TmpMu^pow + TmpVtop^pow)+bgnd   #ordinate correspondent to TmpVtop
         point.coords$x <<- VBtopX
         point.coords$y <<- VBtopY
         replot()
         VBTop <<- FALSE
         VBtopX <- round(VBtopX, 3)
         VBtopY <- round(VBtopY, 3)
         txt <- paste("Estimated position of VB top:  x=", VBtopX, "  y=", VBtopY, sep="  ")
         tkconfigure(StatusBar, text = txt)
         cat("\n",txt)
         # now add a component to store VBtop Position in param mu
         Object[[coreline]] <<- XPSaddComponent(Object[[coreline]], type = "VBtop", ...)
         LL <- length(Object[[coreline]]@Components)
         Object[[coreline]]@Components[[LL]]@param["mu", "start"] <<- VBtopX  # VBtop stored in param "mu"
         Object[[coreline]]@Components[[LL]]@param["h", "start"] <<- VBtopY  # VBtop stored in param "mu"
         Object[[coreline]]@Info <<- paste("   ::: VBtop: x=", VBtopX,"  y=", VBtopY, sep="")      
      }
      VBtEstim <<- TRUE
      replot()
  }
#----Set Default Variable Values

  ResetVars <- function(){
     Object[[coreline]] <<- XPSremove(Object[[coreline]],"all")

     LL <- length(Object[[coreline]]@.Data[[1]])
     Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
     Object[[coreline]]@Boundaries$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
     Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
     Object[[coreline]] <<- XPSbaseline(Object[[coreline]], "Shirley", deg, splinePoints )
     VBintg <<- sum(Object[[coreline]]@RegionToFit$y - Object[[coreline]]@Baseline$y)/LL #Integral of BKG subtracted VB / number of data == average intensity of VB points
     splinePoints <<- NULL
     
     VBbkgOK <<- FALSE
     VBlimOK <<- FALSE
     VBTop <<- FALSE
     VBtEstim <<- FALSE
     BType <<- "Shirley"
     LinFit <<- FALSE
     VBintg <<- NULL    #BKG subtracted VB integral
     CompNames <<- "   "
     compIndx <<- NULL
     reset.fit <<- FALSE
     MarkSym <<- 10
     SymSiz <<- 1.8
     point.coords$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
     point.coords$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
     tcl(nbMain, "select", 0)  #set FIRST the notebook page
     tcl(nbVBfit, "select", 0)
     WidgetState(T21Frame1, "disabled")
     WidgetState(T22Frame1, "disabled")
     WidgetState(T22Frame2, "disabled")
     WidgetState(T23Frame1, "disabled")
     WidgetState(OK_btn1, "normal")
     WidgetState(OK_btn2, "disabled")
     WidgetState(ResetBtn1, "normal")
  }

#-----VARIABLES---
  if (is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
  }

  Object <- get(activeFName,envir=.GlobalEnv)  #this is the XPS Sample
  Object_name <- get("activeFName", envir = .GlobalEnv) #XPSSample name
  ObjectBKP <- NULL   #CoreLine bkp to enable undo operation
  FNameList <- XPSFNameList() #list of XPSSamples
  SpectList <- XPSSpectList(activeFName) #list of XPSSample Corelines
  point.coords <- list(x=NULL, y=NULL)
  coreline <- 0
  plot_win <- as.numeric(get("XPSSettings", envir=.GlobalEnv)$General[4]) #the plot window dimension
  coords <- NA # for printing mouse coordinates on the plot
  deg <- 1 #per default setto a 1 il grado del polinomio per Baseline
  BType <- "Shirley" #defaul BKground
  splinePoints <- NULL
  PlotType <- c("normal", "residual")
  VBbkgOK <- FALSE
  VBlimOK <- FALSE
  VBTop <- FALSE
  VBtEstim <- FALSE
  LinFit <- FALSE
  VBintg <- NULL    #BKG subtracted VB integral
  FitFunct <- c("Gauss", "Voigt", "ExpDecay", "PowerDecay", "Sigmoid")
  CompNames <- "   "
  compIndx <- NULL
  reset.fit <- FALSE
  MarkSym <- 10
  SymSiz <- 1.8

  WinSize <- as.numeric(XPSSettings$General[4])


#----- Widget -----
  VBwindow <- tktoplevel()
  tkwm.title(VBwindow,"XPS ANALYSIS")
  tkwm.geometry(VBwindow, "+100+50")   #position respect topleft screen corner
  VBGroup <- ttkframe(VBwindow, borderwidth=0, padding=c(0,0,0,0) )
  tkgrid(VBGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

#---Core lines
  MainGroup <- ttkframe(VBGroup, borderwidth=0, padding=c(0,0,0,0) )
  tkgrid(MainGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

  SelectFrame <- ttklabelframe(MainGroup, text = " XPS Sample and Core line Selection ", borderwidth=2)
  tkgrid(SelectFrame, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
  XS <- tclVar(activeFName)
  XPS.Sample <- ttkcombobox(SelectFrame, width = 15, textvariable = XS, values = FNameList)
  tkbind(XPS.Sample, "<<ComboboxSelected>>", function(){
                      activeFName <<- tclvalue(XS)
                      Object <<- get(activeFName, envir=.GlobalEnv)
                      Object_name <<- activeFName
                      SpectList <<- XPSSpectList(activeFName)
#                      compIndx <<- grep("VB", SpectList)
#                      tkconfigure(Core.Lines, values=SpectList)
                      coreline <<- 0
                      VBbkgOK <<- FALSE
                      VBlimOK <<- FALSE
                      BType <<- "Shirley"
                      reset.baseline()
                      WidgetState(T21Frame1, "disabled")
                      WidgetState(T22Frame1, "disabled")
                      WidgetState(T22Frame2, "disabled")
                      WidgetState(T23Frame1, "disabled")
                      WidgetState(OK_btn2, "disabled")
                      replot()
                 })
  tkgrid(XPS.Sample, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  CL <- tclVar()
  Core.Lines <- ttkcombobox(SelectFrame, width = 15, textvariable = CL, values = SpectList)
  tkbind(Core.Lines, "<<ComboboxSelected>>", function(){
                      activeSpectName <<- tclvalue(CL)
                      compIndx <<- grep(activeSpectName, SpectList)
                      coreline <<- 0
                      VBbkgOK <<- FALSE
                      VBlimOK <<- FALSE
                      BType <<- "Shirley"
                      reset.baseline()
                      set.coreline()
                      WidgetState(T21Frame1, "disabled")
                      WidgetState(T22Frame1, "disabled")
                      WidgetState(T22Frame2, "disabled")
                      WidgetState(T23Frame1, "disabled")
                      WidgetState(OK_btn2, "disabled")
                      replot()
                      

                 })
  tkgrid(Core.Lines, row = 1, column = 2, padx = 5, pady = 5, sticky="w")


#----- Notebook -----
  nbMain <- ttknotebook(MainGroup)
  tkgrid(nbMain, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

#----- TAB1: Baseline -----
  T1group1 <- ttkframe(nbMain,  borderwidth=2, padding=c(5,5,5,5) )
  tkadd(nbMain, T1group1, text=" Baseline ")

  T1Frame1 <- ttklabelframe(T1group1, text = " Processing ", borderwidth=2)
  tkgrid(T1Frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
  T1Frame2 <- ttklabelframe(T1Frame1, text = " WARNING! ", borderwidth=2)
  tkgrid(T1Frame2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
  txt <- paste("Check the Shirley BKG and set it properly below the WHOLE VB\n",
               "Modify BKG Markers and press 'Define the VB Integral'", sep="")
  WarnLab <- ttklabel(T1Frame2, text=txt)
  tkgrid(WarnLab, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  T1group2 <- ttkframe(T1Frame2, borderwidth=0, padding=c(0,0,0,0) )
  tkgrid(T1group2, row = 2, column = 1, padx = 0, pady = 0, sticky="w")

  OK_btn1 <- tkbutton(T1group2, text=" Set the Baseline ", width=15, command=function(){
                      txt <- paste("Set the EXTENSION and LEVEL of the Background \nto Select the VBtop Region and Define the VB Integral\n",
                                   "Then press \n'Define the VB region proximal to the Fermi Edge' to proceed", sep="")
                      tkconfigure(WarnLab, text=txt)
                      LL <- length(Object[[coreline]]@.Data[[1]])
                      Object[[coreline]]@Boundaries$x <<- c(Object[[coreline]]@.Data[[1]][1], Object[[coreline]]@.Data[[1]][LL])
                      Object[[coreline]]@Boundaries$y <<- c(Object[[coreline]]@.Data[[2]][1], Object[[coreline]]@.Data[[2]][LL])
                      Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
                      Object[[coreline]] <<- XPSbaseline(Object[[coreline]], "Shirley", deg, splinePoints )
                      VBintg <<- sum(Object[[coreline]]@RegionToFit$y - Object[[coreline]]@Baseline$y)/LL #Integral of BKG subtracted VB / number of data == average intensity of VB points
                      tkmessageBox(message="Please set the background ends. \nAlways Left Button to Enter Positions Right Button to Stop", title="WARNING", icon="warning")
                      cat("\n Please set the background ends to define the VB integral ")
                      GetCurPos(SingClick=FALSE)   #activates locator to define the edges of the Baseline for VB background subtraction

                      VBbkgOK <<- TRUE
                      BType <<- "linear"
                      reset.baseline()  #reset baseline from Shirley to linear BKG
                      MarkSym <<- 9
                      SymSiz <<- 1.5
                      WidgetState(OK_btn1, "disabled")
                      WidgetState(ResetBtn1, "disabled")
                      WidgetState(OK_btn2, "normal")
                      replot()         })
  tkgrid(OK_btn1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  ResetBtn1 <- tkbutton(T1group2, text=" Reset Baseline ", width=15, command=function(){
                      reset.baseline()
                      ResetVars()
                      MarkSym <<- 10
                      SymSiz <<- 1.8
                      replot()
         })
  tkgrid(ResetBtn1, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

  T1Frame3 <- ttklabelframe(T1Frame1, text = " DEFINE THE ANALYSIS REGION ", borderwidth=2)
  tkgrid(T1Frame3, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

  OK_btn2 <- tkbutton(T1Frame3, text=" Define VB region proximal to the Fermi Edge ", width=45, command=function(){
                      txt <- paste(" Set the Extension of the VB-portion analyze \n in Proximity of the Fermi Level \n",
                                   " Extension of the VB must allow fitting the \n descending tail towards 0 eV", sep="")
                      tkconfigure(WarnLab, text=txt)
                      GetCurPos(SingClick=FALSE)   #Activates the locator to define the region proximal to the Fermi
                      Object[[coreline]]@Boundaries <<- point.coords
                      Object[[coreline]] <<- XPSsetRegionToFit(Object[[coreline]])  #Define RegionToFit see XPSClass.r
                      VBlimOK <<- TRUE
                      point.coords <<- list(x=NULL, y=NULL)
                      ObjectBKP <<- Object[[coreline]]
                      WidgetState(T21Frame1, "normal")
                      WidgetState(T22Frame1, "normal")
                      WidgetState(T22Frame2, "normal")
                      WidgetState(T23Frame1, "normal")
                      WidgetState(OK_btn2, "disabled")
                      tcl(nbMain, "select", 1)    #switch to the SECOND page (numbering start from 0)
                      replot()
         })
  tkgrid(OK_btn2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

  T1Frame4 <- ttklabelframe(T1Frame1, text = " Plot ", borderwidth=2)
  tkgrid(T1Frame4, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
  ZM <- tclVar("0")   #starts with cleared buttons
  Baseline.Zoom <- tkcheckbutton(T1Frame4, text="zoom Y scale", variable=ZM, onvalue=1, offvalue=0,
                      command=function(){replot() })
  tkgrid(Baseline.Zoom, row = 1, column = 1, padx=5, pady=3, sticky="w")

#----- plot type : Residual or simple
  PLTF <- tclVar("normal")
  for(ii in 1:2){
      plotFit <- ttkradiobutton(T1Frame4, text=PlotType[ii], variable=PLTF, value=PlotType[ii])
      tkgrid(plotFit, row = 2, column = ii, padx = 5, pady = 3, sticky="w")
  }



#----- TAB2: Fit Functions -----
  T2group1 <- ttkframe(nbMain,  borderwidth=2, padding=c(5,5,5,5) )
  tkadd(nbMain, T2group1, text=" VB Fit ")

#----- INNER NOTEBOOK ---
  nbVBfit <- ttknotebook(T2group1)
  tkgrid(nbVBfit, row = 2, column = 1, padx = 5, pady = 5, sticky="w")


#----- TAB21 Linear Fit subtab
  T21group <- ttkframe(nbVBfit,  borderwidth=2, padding=c(5,5,5,5) )
  tkadd(nbVBfit, T21group, text=" Linear Fit ")
  T21Frame1 <- ttklabelframe(T21group, text = " Linear Fit Regions ", borderwidth=2)
  tkgrid(T21Frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  T21group1 <- ttkframe(T21Frame1, borderwidth=0, padding=c(0,0,0,0) )
  tkgrid(T21group1, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
  tkgrid( ttklabel(T21group1, text="Left Mouse Butt. to Set Edges Right to Stop    ",
                   font="Sans 11 normal"),
          row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  Hlp21_btn1 <- tkbutton(T21group1, text="  ?  ", width=5, command=function(){
                      txt <- paste("Two regions must to be defined to perform the linear fits: \n",
                               "the first on the descending tail near to the Fermi edge and \n",
                               "the second on the flat background. Using the left mouse button,\n",
                               "define the two edges of the first and the second regions.\n",
                               "Green crosses will indicate the region boundaries. Then press the\n",
                               "button FIT and a linear fit will be performed in the selected\n",
                               "regions. Press ESTIMATE VB TOP button to obtain the abscissa\n",
                               "of to the fit intersection which is taken as position of the VBtop.")
                      tkmessageBox(message=txt, title="INFO", icon="info")
         })
  tkgrid(Hlp21_btn1, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

  SetPts21_Btn1 <- tkbutton(T21Frame1, text=" Set Linear Region Edges ", width=40, command=function(){
                      GetCurPos(SingClick=FALSE)
         })
  tkgrid(SetPts21_Btn1, row = 2, column = 1, padx = 5, pady = 3, sticky="w")

  Reset21_Btn1 <- tkbutton(T21Frame1, text=" Reset Fit Regions ", width=40, command=function(){
                      reset.LinRegions()
         })
  tkgrid(Reset21_Btn1, row = 3, column = 1, padx = 5, pady = 3, sticky="w")

  Fit21_btn1 <- tkbutton(T21Frame1, text=" Fit ", width=40, command=function(){
                      MakeFit()
         })
  tkgrid(Fit21_btn1, row = 4, column = 1, padx = 5, pady = 3, sticky="w")

  VBTop21_btn1 <- tkbutton(T21Frame1, text=" Estimate VB Top ", width=40, command=function(){
                      CalcVBTop()
         })
  tkgrid(VBTop21_btn1, row = 5, column = 1, padx = 5, pady = 3, sticky="w")

  Reset21_Btn2 <- tkbutton(T21Frame1, text=" Reset Analysis ", width=40, command=function(){
                      LL <- length(Object[[coreline]]@.Data[[1]])
                      Object[[coreline]] <<- ObjectBKP
                      point.coords <<- list(x=NULL, y=NULL)
                      VBTop <<- FALSE
                      MarkSym <<- 10
                      SymSiz <<- 1.8
                      tkconfigure(StatusBar, text= "Estimated position of VB top : ")
                      replot()
         })
  tkgrid(Reset21_Btn2, row = 6, column = 1, padx = 5, pady = 3, sticky="w")

  Reset21_btn3 <- tkbutton(T21Frame1, text=" Reset All ", width=40, command=function(){
                      ResetVars()
                      WidgetState(OK_btn1, "normal")
                      WidgetState(OK_btn2, "disabled")
                      replot()
         })
  tkgrid(Reset21_btn3, row = 7, column = 1, padx = 5, pady = 3, sticky="w")


#----- TAB22 NON-Linear Fit subtab
  T22group <- ttkframe(nbVBfit,  borderwidth=2, padding=c(5,5,5,5) )
  tkadd(nbVBfit, T22group, text=" NON-Linear Fit ")

  T22Frame1 <- ttklabelframe(T22group, text = " Fit Components ", borderwidth=2)
  tkgrid(T22Frame1, row = 2, column = 1, padx = 5, pady = 3, sticky="w")
  tkgrid( ttklabel(T22Frame1, text="Left Mouse Butt to Add Fit Comp. Right to Stop",
                   font="Sans 11 normal"),
          row = 1, column = 1, padx = 5, pady = 0, sticky="w")

  Hlp22_btn1 <- tkbutton(T22Frame1, text=" ? ", width=5, command=function(){
                      txt <- paste("The idea is to use the fit of the descending tail of the VB to \n",
                                  "get rid from noise and obtain a better estimate the VBtop.\n",
                                  "First select the desired component lineshape (Gaussian is suggested)\n",
                                  "Then click with the left mouse button in the positions to add fit components\n",
                                  "Click with the right mouse button to stop adding fit components\n",
                                  "Press DELETE FIT COMPONENT to delete a reduntant fit component\n",
                                  "Press RESET FIT to restart the procedure.\n",
                                  "Add as many components as needed to model the VB in the defined region\n",
                                  "Press the FIT button to make the fit which must correctly reproduce the VB tail\n",
                                  "Pressing the ESTIMATE VB TOP button, a predefined treshold based on the VB \n",
                                  "  integral intensity, is the utilized to estimate the VB top position \n",
                                  "Pressing the RESET ALL button one resets the whole analysis and restarts from very beginning")
                      tkmessageBox(message=txt, title="INFO", icon="info")
         })
  tkgrid(Hlp22_btn1, row = 1, column = 2, padx = 5, pady = 0, sticky="w")

  FITF <- tclVar("Gauss")
  SelectFitFunct <- ttkcombobox(T22Frame1, width = 15, textvariable = FITF, values = FitFunct)
  tkbind(SelectFitFunct, "<<ComboboxSelected>>", function(){
                      txt <- sprintf("Selected component type %s", tclvalue(FITF))
                      tkconfigure(StatusBar, text=txt)
         })
  tkgrid(SelectFitFunct, row = 2, column = 1, padx = 5, pady = 3, sticky="w")

  T22Frame2 <- ttklabelframe(T22group, text = " Options ", borderwidth=2)
  tkgrid(T22Frame2, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

  add22_btn1 <- tkbutton(T22Frame2, text=" Add Fit Component ", width=40, command=function(){
                      GetCurPos(SingClick=FALSE)
         })
  tkgrid(add22_btn1, row = 1, column = 1, padx = 5, pady = 3, sticky="we")

  del22_btn1 <- tkbutton(T22Frame2, text=" Delete Component ", command=function(){
                      del.FitFunct()
         })
  tkgrid(del22_btn1, row = 2, column = 1, padx = 5, pady = 3, sticky="we")

#  edit_btn2 <- tkbutton(T22Frame2, text=" Edit Fit Parameters ", command=function(){
#                      Edit.FitParam()
#         })
#  tkgrid(edit_btn2, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

  Fit22_btn1 <- tkbutton(T22Frame2, text=" Fit ", command=function(){
                      MakeFit()
         })
  tkgrid(Fit22_btn1, row = 3, column = 1, padx = 5, pady = 3, sticky="we")

  Reset22_btn1 <- tkbutton(T22Frame2, text=" Reset Fit ", command=function(){
                      MakeFit()
                      point.coords <<- list(x=NULL,y=NULL)
                      reset.fit <<- TRUE
                      MakeFit()
                      tkconfigure(StatusBar, text="Estimated position of VB top : ")
         })
  tkgrid(Reset22_btn1, row = 4, column = 1, padx = 5, pady = 3, sticky="we")

  VBTop22_btn1 <- tkbutton(T22Frame2, text=" Estimate VB Top ", command=function(){
                      CalcVBTop()
         })
  tkgrid(VBTop22_btn1, row = 5, column = 1, padx = 5, pady = 3, sticky="we")

  Reset22_btn2 <- tkbutton(T22Frame2, text=" Reset All ", command=function(){
                      ResetVars()
                      WidgetState(OK_btn1, "normal")
                      WidgetState(OK_btn2, "disabled")
                      replot()

         })
  tkgrid(Reset22_btn2, row = 6, column = 1, padx = 5, pady = 3, sticky="we")


#----- TAB13 HILL SIGMOID
  T23group <- ttkframe(nbVBfit,  borderwidth=2, padding=c(5,5,5,5) )
  tkadd(nbVBfit, T23group, text=" Hill Sigmoid Fit ")

  T23Frame1 <- ttklabelframe(T23group, text = " Options ", borderwidth=2)
  tkgrid(T23Frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  tkgrid( ttklabel(T23Frame1, text="Left Mouse Butt. to Set Sigmoid Max, \nFlex Point, Min.  Right Butt. to Stop    ",
                   font="Sans 11 normal"),
          row = 1, column = 1, padx = 5, pady = 5, sticky="w")
  Hlp23_btn1 <- tkbutton(T23Frame1, text=" ? ", width=5, command=function(){
                      txt <- paste("Three points are needed to define a Hill Sigmoid: the Sigmoid maximum M (max of the\n",
                                "  VB in the selected region, the sigmoid flex point FP in the middle of the descending\n",
                                "  tail and the sigmoid minimum m (background level).\n",
                                "Press 'Add Hill Sigmoid' and Click with the Left Mouse button to add the M, FP and m points\n",
                                "Click with the right mouse button to stop entering positions and add the ADD HILL SIGMOID\n",
                                "Press the FIT button to model the VB using the Hill Sigmoid",
                                "Press RESET FIT to restart the fitting procedure\n",
                                "Pressing the ESTIMATE VB TOP button, the VB top is determined matematically as\n",
                                "   the point with abscissa [FPx * (1-2/n)] where FPx is the abscissa of FP, \n",
                                "   n is the sigmoid power (see manual for more details).\n",
                                "Pressing RESET ALL button one resets all the analysis and restarts from very beginning")
                      tkmessageBox(message=txt, title="INFO", icon="info")
         })
  tkgrid(Hlp23_btn1, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

  add23_btn1 <- tkbutton(T23Frame1, text=" Add Hill Sigmoid ", width=40, command=function(){
                      GetCurPos(SingClick=FALSE)
                      add.FitFunct()
         })
  tkgrid(add23_btn1, row = 2, column = 1, padx = 5, pady = 3, sticky="w")

  Fit22_btn1 <- tkbutton(T23Frame1, text=" Fit ", width=40, command=function(){
                      MakeFit()
         })
  tkgrid(Fit22_btn1, row = 3, column = 1, padx = 5, pady = 3, sticky="w")

  Reset23_btn1 <- tkbutton(T23Frame1, text=" Reset Fit ", width=40, command=function(){
                      point.coords <<- list(x=NULL,y=NULL)
                      reset.fit <<- TRUE
                      MakeFit()
                      tkconfigure(StatusBar, text="Estimated position of VB top : ")
         })
  tkgrid(Reset23_btn1, row = 4, column = 1, padx = 5, pady = 3, sticky="w")

  VBTop23_btn1 <- tkbutton(T23Frame1, text=" Estimate VB Top ", width=40, command=function(){
                      CalcVBTop()
         })
  tkgrid(VBTop23_btn1, row = 5, column = 1, padx = 5, pady = 3, sticky="w")

  VBTop23_btn1 <- tkbutton(T23Frame1, text=" Reset All ", width=40, command=function(){
                      ResetVars()
                      WidgetState(OK_btn1, "normal")
                      WidgetState(OK_btn2, "disabled")
                      replot()
         })
  tkgrid(VBTop23_btn1, row = 6, column = 1, padx = 5, pady = 3, sticky="w")


#----- SAVE&CLOSE button -----
  BtnGroup <- ttkframe(VBGroup, borderwidth=0, padding=c(0,0,0,0) )
  tkgrid(BtnGroup, row = 3, column = 1, padx = 0, pady = 0, sticky="w")

  SaveBtn <- tkbutton(BtnGroup, text=" SAVE ", width=18,  command=function(){
                      tkdestroy(VBwindow)
                      if (VBtEstim == FALSE && length(Object[[coreline]]@Fit) > 0){  #VB fit done but VBtop estimation not
                          answ <- tkmessageBox(msg="VBtop estimation not performed. Would you proceed?",
                                               type="yeno", title="WARNING", icon="warning")
                          if (tclvalue(answ) == "no") { return() }
                      }
                      LL <- length(Object)
                      Object[[LL+1]] <<- Object[[coreline]]
                      Object@names[LL+1] <<- "VBt"
                      assign(Object_name, Object, envir = .GlobalEnv)
                      assign("activeSpectIndx", (LL+1), envir = .GlobalEnv)
                      assign("activeSpectName", "VBt", envir = .GlobalEnv)
                      replot()
                      XPSSaveRetrieveBkp("save")
         })
  tkgrid(SaveBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  SaveExitBtn <- tkbutton(BtnGroup, text=" SAVE & EXIT ", width=18,  command=function(){
                      tkdestroy(VBwindow)
                      if (VBtEstim == FALSE && length(Object[[coreline]]@Fit) > 0){  #VB fit done but VBtop estimation not
                          answ <- tkmessageBox(msg="VBtop estimation not performed. Would you proceed?",
                                               type="yeno", title="WARNING", icon="warning")
                          if (tclvalue(answ) == "no") { return() }
                      }
                      LL <- length(Object)
                      Object[[LL+1]] <<- Object[[coreline]]
                      Object@names[LL+1] <<- "VBt"
                      assign(Object_name, Object, envir = .GlobalEnv)
                      assign("activeSpectIndx", (LL+1), envir = .GlobalEnv)
                      assign("activeSpectName", "VBt", envir = .GlobalEnv)
                      tkdestroy(VBwindow)
                      plot(Object)
                      XPSSaveRetrieveBkp("save")
         })
  tkgrid(SaveExitBtn, row = 1, column = 2, padx = 5, pady = 5, sticky="we")

  ExitBtn <- tkbutton(BtnGroup, text=" EXIT ", width=18,  command=function(){
                      tkdestroy(VBwindow)
                      plot(Object)
         })
  tkgrid(ExitBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="we")



  StatusBar <- ttklabel(VBGroup, text=" Estimated position of VB top : ", relief="sunken", foreground="blue")
  tkgrid(StatusBar, row = 4, column = 1, padx = 5, pady = 5, sticky="we")

  WidgetState(OK_btn2, "disabled")
  WidgetState(T21Frame1, "disabled")
  WidgetState(T22Frame1, "disabled")
  WidgetState(T22Frame2, "disabled")
  WidgetState(T23Frame1, "disabled")

}
