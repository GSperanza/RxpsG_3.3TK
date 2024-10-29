#-----------------------------------------
# XPS Sprucing with gWidgets2 and tcltk
#-----------------------------------------

#'@title XPSSprucing
#'@description XPSSprucing function to correct original XPS spectral data
#'@return Returns the \code{Object} with the corrected spectrum.
#'@examples
#'\dontrun{
#' XPSSprucing()
#'}
#'@export
#'

XPSSprucing <- function() {

    GetCurPos <- function(SingClick){
       coords <<- NULL
       WidgetState(OptFrame, "disabled")   #prevent exiting Analysis if locatore active
       WidgetState(BtnGroup, "disabled")
       WidgetState(ExitFrame, "disabled")
       EXIT <- FALSE
       while(EXIT == FALSE){
            pos <- locator(n=1)
            if (is.null(pos)) {
                WidgetState(OptFrame, "normal")
                WidgetState(BtnGroup, "normal")
                WidgetState(ExitFrame, "normal")
                EXIT <- TRUE
            } else {
                if ( SingClick ){ 
                    coords <<- c(pos$x, pos$y)
                    WidgetState(OptFrame, "normal")
                    WidgetState(BtnGroup, "normal")
                    WidgetState(ExitFrame, "normal")
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
      point.coords$x[point.index] <<- coords[1]   #abscissa
      point.coords$y[point.index] <<- coords[2]   #ordinate
      if (point.index==1) {
         point.index <<- 2    #to modify the second edge of the selected area
         Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
         Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
      } else if (point.index==2) {
         Corners$x <<- c(point.coords$x[1],point.coords$x[1],point.coords$x[2],point.coords$x[2])
         Corners$y <<- c(point.coords$y[1],point.coords$y[2],point.coords$y[1],point.coords$y[2])
         point.index <<- 3
      } else if (point.index==3) {
         D <- vector("numeric", 4)
         Dmin<-((point.coords$x[3]-Corners$x[1])^2 + (point.coords$y[3]-Corners$y[1])^2)^0.5  #valore di inizializzazione
         for(ii in 1:4) {
             D[ii]<-((point.coords$x[3]-Corners$x[ii])^2 + (point.coords$y[3]-Corners$y[ii])^2)^0.5  #dist P0 P1
             if(D[ii] <= Dmin){
                Dmin <- D[ii]
                idx=ii
             }
         }
         if (idx==1){
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
            point.coords$x <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE) #ordina pos$x in ordine decrescente Corners$x[1]==Corners$x[2]
            point.coords$y <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
         } else {
            point.coords$x <<- sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE) #ordina pos$x in ordine crescente
            point.coords$y <<- sort(c(Corners$y[1],Corners$y[4]), decreasing=FALSE)
         }
      }
      replot()
  }

  undo.plot <- function(...){
      if (SelReg == 1) {
         reset.boundaries()
         replot()
      } else if (SelReg > 1) {
       Object[[coreline]]@Boundaries$x <<- OldCoords$x
       Ylimits <<- OldCoords$y
         replot()
      }
  }

  replot <- function(...) {
      if (coreline == 0) {     # coreline == "All spectra"
          plot(Object)
      } else {
         Xlimits <- Object[[coreline]]@Boundaries$x
         if (point.index <= 2) {
             plot(Object[[coreline]], xlim=Xlimits)
             points(point.coords, col="blue", cex=1.5, lwd=2, pch=3)
         } else if (point.index > 2){
             plot(Object[[coreline]], xlim=Xlimits, ylim=Ylimits)
             points(Corners, type="p", col="blue", cex=1.5, lwd=2, pch=3)
             rect(point.coords$x[1], point.coords$y[1], point.coords$x[2], point.coords$y[2])
         }
      }
  }

  reset.boundaries <- function(h, ...) {
      Object[[coreline]] <<- XPSremove(Object[[coreline]], "all")
      LL<-length(Object[[coreline]]@.Data[[1]])
      point.coords$x[1] <<- Object[[coreline]]@.Data[[1]][1] #ascissa primo estremo 1 del survey
      point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][1] #ordinata primo estremo 1 del survey
      point.coords$x[2] <<- Object[[coreline]]@.Data[[1]][LL] #ascissa  secondo estremo del survey
      point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][LL] #ordinata secondo estremo del survey
      slot(Object[[coreline]],"Boundaries") <<- point.coords
      Ylimits <<- c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
      OldCoords <<- point.coords #for undo
      Corners <- point.coords
      point.index <<- 1
      replot()
  }

  EditRegion <- function(h, ...){
              idx1 <<- point.coords$x[1]
              idx2 <<- point.coords$x[2]
              newcoreline <- Object[[coreline]]@.Data
              idx1 <<- findXIndex(newcoreline[[1]], point.coords$x[1])
              idx2 <<- findXIndex(newcoreline[[1]], point.coords$x[2])
              DataTblLngt <<- abs(idx1-idx2)+1
              newcoreline[[1]] <- newcoreline[[1]][idx1:idx2]
              newcoreline[[2]] <- newcoreline[[2]][idx1:idx2]
              tcl(TxtTbl, "delete", "0.0", "end")  #clear the TextWin
              tkconfigure(TxtTbl, foreground="black")
              tmp <- paste(newcoreline[[1]], "   ", newcoreline[[2]], "\n", collapse="")
              LL <- nchar(tmp)
              tmp <- strtrim(tmp, width=(LL-2))  #cut the last '\n'
              tcl(TxtTbl, "insert", "0.0", tmp)
              tkgrid.columnconfigure(EditGroup, 1, weight=2)
              addScrollbars(EditGroup, TxtTbl, type="y", Row = 1, Col = 1, Px=0, Py=0)
              WidgetState(BtnGroup, "normal")
  }


#--- Variables
  activeFName <- get("activeFName", envir = .GlobalEnv)
  if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
      tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
      return()
  }

  Object <- get(activeFName,envir=.GlobalEnv)
  FNameList <- XPSFNameList()
  SpectList <- XPSSpectList(activeFName)
  coreline <- 0 #get("activeSpectIndx",envir=.GlobalEnv)
  XPSSettings <- get("XPSSettings",envir=.GlobalEnv)
  WinSize <- as.numeric(XPSSettings$General[4])
  DataTable <- list()
  DataTblLngt <- NULL
  idx1 <- NULL
  idx2 <- NULL
  TabTxt <- list()

  point.coords <- list(x=NA,y=NA)
  point.index <- 1
  coords <- NA # for printing mouse coordinates on the plot
  XLimits <- Ylimits <- NULL
  OldCoords <- point.coords #for undo
  Corners <- point.coords
  xx <- NULL
  yy <- NULL
  SetChanges <- FALSE

  SelReg <- 0

  LL <- length(Object)
  if (LL == 1) { #XPSSample contains only one Core-Line
      coreline <- 1
      LL <- length(Object[[coreline]]@.Data[[1]])
      point.coords$x[1] <- Object[[coreline]]@.Data[[1]][1]  #ascissa primo estremo 1 del survey
      point.coords$y[1] <- Object[[coreline]]@.Data[[2]][1]  #ordinata primo estremo 1 del survey
      point.coords$x[2] <- Object[[coreline]]@.Data[[1]][LL] #ascissa  secondo estremo del survey
      point.coords$y[2] <- Object[[coreline]]@.Data[[2]][LL] #ordinata secondo estremo del survey
      Ylimits <-c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
      OldCoords <- point.coords #for undo
      Corners <- point.coords
      Object[[coreline]]@Boundaries$x <- point.coords$x
      Object[[coreline]]@Boundaries$y <- point.coords$y
  }


#--- Widget
  SPwin <- tktoplevel()
  tkwm.title(SPwin,"XPS SPRUCING GUI")
  tkwm.geometry(SPwin, "+100+50")   #position respect topleft screen corner
  MainGroup <- ttkframe(SPwin, borderwidth=0, padding=c(0,0,0,0) )
  tkgrid(MainGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

#--- Options
  XS.CLFrame <- ttklabelframe(MainGroup, text = " SELECT XPS SAMPLE and CORELINE ", borderwidth=2)
  tkgrid(XS.CLFrame, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
  XS <- tclVar(activeFName)
  XSCombo <- ttkcombobox(XS.CLFrame, width = 22, textvariable = XS, values = FNameList)
  tkbind(XSCombo, "<<ComboboxSelected>>", function(){
              XPSSample <- tclvalue(XS)
              Object <<- get(XPSSample, envir=.GlobalEnv)
              plot(Object)
              SpectList <<- XPSSpectList(XPSSample)
              tkconfigure(CLCombo, values=SpectList)
         })
  tkgrid(XSCombo, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  CL <- tclVar()
  CLCombo <- ttkcombobox(XS.CLFrame, width = 22, textvariable = CL, values = SpectList)
  tkbind(CLCombo, "<<ComboboxSelected>>", function(){
              XPSCL <- tclvalue(CL)
              coreline <<- grep(XPSCL, SpectList)
              if (length(Object[[coreline]]@Boundaries) == 0){
                  LL <- length(Object[[coreline]]@.Data[[1]])
                  point.coords$x[1] <<- Object[[coreline]]@.Data[[1]][1]  #ascissa primo estremo 1 del survey
                  point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][1]  #ordinata primo estremo 1 del survey
                  point.coords$x[2] <<- Object[[coreline]]@.Data[[1]][LL] #ascissa  secondo estremo del survey
                  point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][LL] #ordinata secondo estremo del survey
                  Object[[coreline]]@Boundaries$x <<- sort(point.coords$x, decreasing=FALSE)
                  Object[[coreline]]@Boundaries$y <<- sort(point.coords$y, decreasing=FALSE)
              } else if (length(Object[[coreline]]@RegionToFit) > 0){
                  LL <- length(Object[[coreline]]@RegionToFit$y)
                  point.coords$x <<- Object[[coreline]]@Boundaries$x
                  point.coords$y <<- c(Object[[coreline]]@RegionToFit$y[1], Object[[coreline]]@RegionToFit$y[LL])
                  if (Object[[coreline]]@Flags[1] == TRUE) { point.coords$y <<- rev(point.coords$y) }
              } else if (length(Object[[coreline]]@Boundaries) > 0){
                  point.coords$x <<- sort(Object[[coreline]]@Boundaries$x, decreasing=FALSE)
                  if (Object[[coreline]]@Flags[1] == TRUE) { point.coords$x <<- rev(point.coords$x) }
                  idx <- findXIndex(Object[[coreline]]@.Data[[1]], point.coords$x[1])
                  point.coords$y[1] <<- Object[[coreline]]@.Data[[2]][idx]
                  idx <- findXIndex(Object[[coreline]]@.Data[[1]], point.coords$x[2])
                  point.coords$y[2] <<- Object[[coreline]]@.Data[[2]][idx]
              }
              Ylimits <<- c(min(Object[[coreline]]@.Data[[2]]), max(Object[[coreline]]@.Data[[2]]))
              OldCoords <<- point.coords #for undo
              Corners <<- point.coords
              SelReg <<- 0
              cat("\n Please select the portion of the spectrum to control")
              replot()
              WidgetState(OptFrame, "normal")
              SetChanges <<- FALSE

         })
  tkgrid(CLCombo, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

#-----
  OptFrame <- ttklabelframe(MainGroup, text = " SELECT DATA ", borderwidth=2)
  tkgrid(OptFrame, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

  SelectBtn <- tkbutton(OptFrame, text=" SELECT REGION ", width=35, command=function(){
              OldCoords <<- Object[[coreline]]@Boundaries
              SelReg <<- SelReg+1
              if (point.coords$x[1] == Object[[coreline]]@Boundaries$x[1] && 
                  point.coords$x[2] == Object[[coreline]]@Boundaries$x[2]) {
                  txt <- paste("Left Mouse Button to Define the Region to Edit\n",
                               "Right Mouse Button to ZOOM\n",
                               "Then Optimize the Selected Region Clicking near Corners\n",
                               "When OK Right Mouse Button and then Press the EDIT REGION Button", sep="")
                  tkmessageBox(message=txt, title="WARNING", icon="warning")
              }
              GetCurPos(SingClick=FALSE)
              rngX <- range(point.coords$x)
              rngX <- (rngX[2]-rngX[1])/20
              rngY <- range(point.coords$y)
              rngY <- (rngY[2]-rngY[1])/20
              if (Object[[coreline]]@Flags[1]) { #Binding energy set
                 point.coords$x <- sort(point.coords$x, decreasing=TRUE) #ordina pos$x in ordine decrescente Corners$x[1]==Corners$x[2]
                 point.coords$x[1] <- point.coords$x[1]+rngX/20
                 point.coords$x[2] <- point.coords$x[2]-rngX/20
              } else {
                 point.coords$x <- sort(point.coords$x, decreasing=FALSE) #ordina pos$x in ordine decrescente Corners$x[1]==Corners$x[2]
                 point.coords$x[1] <- point.coords$x[1]-rngX/20
                 point.coords$x[2] <- point.coords$x[2]+rngX/20
              }
              point.coords$y <- sort(point.coords$y, decreasing=FALSE)
              Ylimits <<- c(point.coords$y[1]-rngY/10, point.coords$y[2]+rngY/10)
              Object[[coreline]]@Boundaries <<- point.coords
              replot()
              GetCurPos(SingClick=FALSE)
         })
  tkgrid(SelectBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")


  EditBtn <- tkbutton(OptFrame, text="  EDIT REGION  ", width=35, command=function(){
              EditRegion()
         })
  tkgrid(EditBtn, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

  UndoBtn <- tkbutton(OptFrame, text=" UNDO ", width=35, command=function(){
              undo.plot()
         })
  tkgrid(UndoBtn, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

  ResetBtn <- tkbutton(OptFrame, text=" RESET BOUNDARIES ", width=35, command=function(){
              reset.boundaries()
         })
  tkgrid(ResetBtn, row = 4, column = 1, padx = 5, pady = 5, sticky="w")



#-----

  EditFrame <- ttklabelframe(MainGroup, text = " DATA SPRUCING ", borderwidth=2)
  tkgrid(EditFrame, row = 3, column = 1, padx = 5, pady = 5, sticky="we")
  EditGroup <- ttkframe(EditFrame, borderwidth=0, padding=c(0,0,0,0) )
  tkgrid(EditGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

  TxtTbl <- tktext(EditGroup, height=7, width=24, background="white", foreground="grey")
  tkinsert(TxtTbl, "0.0", "Data to correct: \n \n \n")
  tkgrid(TxtTbl, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
#  tkgrid.columnconfigure(EditGroup, 1, weight=2)
#  addScrollbars(EditGroup, TxtTbl, type="y", Row = 1, Col = 1, Px=0, Py=0)

  BtnGroup <- ttkframe(EditFrame, borderwidth=0, padding=c(0,0,0,0) )
  tkgrid(BtnGroup, row = 1, column = 2, padx = 0, pady = 0, sticky="w")
  SetBtn <- tkbutton(BtnGroup, text=" SET CHANGES ", width=18, command=function(){
              tmp <- NULL
              tmp <- tclvalue(tkget(TxtTbl, "1.0", "end"))
              tmp <- unlist(strsplit(tmp, "\n"))
              LL <- length(tmp) #number of tmp text rows
              tmp <- unlist(strsplit(tmp, "   "))
              tmp <- as.numeric(tmp) #tmp is a sequence composed by x1, y1, x2, y2, ...xn, yn
              if (LL != DataTblLngt){
                  tkmessageBox(message=" ATTENTION: number of corrected data different from the original one.
                                \nPlease control data, avoid blank rows!", title="ERROR", icon="error")
                  return()
              }
              LL <- LL*2 #each tmp text row contains an x, y couple
              idx <- seq(from=1, to=LL-1, by=2)
              DataTable[[1]] <- tmp[idx] #extract abscissas
              idx <- idx+1
              DataTable[[2]] <- tmp[idx] #extract ordinate
              Object[[coreline]]@.Data[[1]][idx1:idx2] <<- DataTable[[1]]
              Object[[coreline]]@.Data[[2]][idx1:idx2] <<- DataTable[[2]]
              tcl(TxtTbl, "delete", "0.0", "end")  #clear the TextWin
              tkconfigure(TxtTbl, foreground="grey")
              tkinsert(TxtTbl, "0.0", "Data to correct: \n \n \n")
              reset.boundaries()
              replot(Object[[coreline]])
              SetChanges <<- TRUE
         })
  tkgrid(SetBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")


  RstBtn <- tkbutton(BtnGroup, text=" RESET ", width=18, command=function(){
              Object <<- get(activeFName,envir=.GlobalEnv)
              tcl(TxtTbl, "delete", "0.0", "end")  #clear the TextWin
              tkconfigure(TxtTbl, foreground="grey")
              tkinsert(TxtTbl, "0.0", "Data to correct: \n \n \n")
              reset.boundaries()
              replot(Object[[coreline]])
         })
  tkgrid(RstBtn, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
  WidgetState(BtnGroup, "disabled")


#=== CLOSE button ===
  ExitFrame <- ttklabelframe(MainGroup, text = " SAVE & EXIT ", borderwidth=2)
  tkgrid(ExitFrame, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

  SaveBtn <- tkbutton(ExitFrame, text=" SAVE ", width=16, command=function(){
              if (SetChanges == FALSE){
                  tkmessageBox(message=" No Changes to Data Detected. Please Control!", title="ERROR", icon="error")
                  return()
              }
              assign("activeFName", activeFName, envir = .GlobalEnv)
              assign(activeFName, Object, envir = .GlobalEnv)
              XPSSaveRetrieveBkp("save")
         })
  tkgrid(SaveBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  SaveExitBtn <- tkbutton(ExitFrame, text=" SAVE & EXIT ", width=16, command=function(){
              tkdestroy(SPwin)
              assign("activeFName", activeFName, envir = .GlobalEnv)
              assign(activeFName, Object, envir = .GlobalEnv)
              tkdestroy(SPwin)
              reset.boundaries()
              XPSSaveRetrieveBkp("save")
              UpdateXS_Tbl()
         })
  tkgrid(SaveExitBtn, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

  ExitBtn <- tkbutton(ExitFrame, text=" EXIT ", width=16, command=function(){
              tkdestroy(SPwin)
              XPSSaveRetrieveBkp("save")
              plot(Object)
         })
  tkgrid(ExitBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

  WidgetState(OptFrame, "disabled")    #prevent exiting Analysis if locatore active
  WidgetState(BtnGroup, "disabled")
  WidgetState(ExitFrame, "disabled")
}
