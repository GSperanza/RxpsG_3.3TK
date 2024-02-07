#CoreLine processing: adding/deleting corelines or Baseline and Fits to exiting Corelines


#' @title XPSProcessCoreLine
#' @description XPSProcessCoreLine function adds a CoreLine (with fit if existing)
#'   to an object of class XPSSample, or just a BaseLine and Fit to an existing CoreLine
#'   (class XPSCoreLine) or remove a CoreLine from an object of class XPSSample
#'   XPSProcessCoreLine may be used to perform some simple math operations on CoreLines
#'   (class XPSCoreLine) like addition, subtraction, multiplication by a constant value
#'    normalization, differentiation, combination of two CoreLines.
#' @examples
#' \dontrun{
#' 	XPSProcessCoreLine()
#' }
#' @export
#'

XPSProcessCoreLine <- function(){

#---

   CtrlRepCL <- function(destIndx, SpectName, DestSpectList){    #CTRL for repeated CL: search for core-lines with same name
      winCL <- tktoplevel()
      tkwm.title(winCL,"XPS ANALYSIS")
      tkwm.geometry(winCL, "+200+200")   #position respect topleft screen corner
      groupCL <- ttkframe(winCL, borderwidth=0, padding=c(0,0,0,0) )
      tkgrid(groupCL, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
      DestFile <- tclvalue(XS2)
      N.CL <- length(destIndx)
      msg <- paste("Found", N.CL, SpectName, "spectra in", DestFile,"\nPlease select the coreline \nto add Baseline and Fit", sep=" ")
      tkgrid( ttklabel(groupCL, text=msg, font="Serif, 12"),
              row = 1, column = 1, padx = 5, pady = 5, sticky="w")
      tkSep <- ttkseparator(groupCL, orient="horizontal")
      tkgrid(tkSep, row = 2, column = 1, padx = 5, pady = 5, sticky="we")

      selectCL <- tclVar()
      items <- DestSpectList[destIndx]
      LL <- length(DestSpectList[destIndx])
      for(ii in 1:LL){
          Radio <- ttkradiobutton(groupCL, text=items[ii], variable=selectCL, value=items[ii],
                                command=function(){
                                      CoreLine <- tclvalue(selectCL)
                                      CoreLine <- unlist(strsplit(CoreLine, "\\."))   #drop "NUMBER." at beginning of coreLine name
                                      RepCLidx <<- as.numeric(CoreLine[1])
                                })
          tkgrid(Radio, row = 3, column = ii, padx = 5, pady = 5, sticky="w")
      }

      exitBtn <- tkbutton(groupCL, text="  OK  ", width = 15, command=function(){
                                tkdestroy(winCL)
                 })
      tkgrid(exitBtn, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
      tkwait.window(winCL) #winCL modal mode
   }

   RenumCL <- function(SourceFile){
      CLnames <- names(SourceFile)
      LL <- length(SourceFile)
      for(ii in 1:LL){
          SourceFile[[ii]]@Symbol <- paste(ii,".",CLnames[ii], sep="")
      }
      return(SourceFile)
   }

   MakeBaseLine <- function(SourceFile, indx, DestFile, destIndx) {
        BLinfo <- SourceFile[[indx]]@Baseline$type
        BasLinType <- BLinfo[1]
        splinePoints <- NULL
        deg <- NULL
        Wgt <- NULL

        if (BasLinType == "shirley"){BasLinType <- "Shirley"}       #different names for old/new RXPSG packages
        if (BasLinType == "2p.shirley"){BasLinType <- "2P.Shirley"} #transform to new BaseLineNames.
        if (BasLinType == "3p.shirley"){BasLinType <- "3P.Shirley"} #Exact Baseline Names required to generate the Baseline see XPSClass
        if (BasLinType == "lp.shirley"){BasLinType <- "LP.Shirley"}
        if (BasLinType == "2p.tougaard"){BasLinType <- "2P.Tougaard"}
        if (BasLinType == "3p.tougaard"){BasLinType <- "3P.Tougaard"}
        if (BasLinType == "4p.tougaard"){BasLinType <- "4P.Tougaard"}

        if (BasLinType == "linear" ||
           BasLinType == "Shirley" || BasLinType == "2P.Shirley" || BasLinType == "2P.Tougaard" || BasLinType == "3P.Tougaard") {
           txt <- paste(BLinfo[1], " background found!\n  ==> Set the Baseline Limits")
           tkmessageBox(message=txt, title="HELP INFO", icon="info")
           plot(DestFile[[destIndx]])
           pos <- locator(n=2, type="p", col="red", lwd=2)
           DestFile[[destIndx]]@Boundaries$x <- pos$x
           DestFile[[destIndx]]@Boundaries$y <- pos$y
           DestFile[[destIndx]] <- XPSsetRegionToFit(DestFile[[destIndx]])
           DestFile[[destIndx]] <- XPSbaseline(DestFile[[destIndx]], BasLinType, deg, Wgt, splinePoints )
           DestFile[[destIndx]]@RSF <- SourceFile[[indx]]@RSF
        } else if (BasLinType == "polynomial") {
           deg <- as.numeric(BLinfo[2])
           tkmessageBox(message="Polynomial backgound found!\n ==> Set the Baseline Limits", title="HELP INFO", icon="info")
           plot(DestFile[[destIndx]])
           pos <- locator(n=2, type="p", col="red", lwd=2)
           DestFile[[destIndx]]@Boundaries$x <- pos$x
           DestFile[[destIndx]]@Boundaries$y <- pos$y
           DestFile[[destIndx]] <- XPSsetRegionToFit(DestFile[[destIndx]])
           DestFile[[destIndx]] <- XPSbaseline(DestFile[[destIndx]], BasLinType, deg, Wgt, splinePoints )
           DestFile[[destIndx]]@RSF <- SourceFile[[indx]]@RSF
        } else if (BasLinType == "spline") {
            splinePoints <- list(x=NULL, y=NULL)
            txt <- "Spline background found! \n ==> LEFT click to set spline points; RIGHT to exit"
            tkmessageBox(message=txt, title="HELP INFO", icon="info")
            plot(DestFile[[destIndx]])
            pos <- c(1,1) # only to enter in  the loop
            while (length(pos) > 0) {  #pos != NULL => mouse right button not pressed
                  pos <- locator(n=1, type="p", col=3, cex=1.5, lwd=2, pch=1)
                  if (length(pos) > 0) {
                      splinePoints$x <- c(splinePoints$x, pos$x)  # $x and $y must be separate to add new coord to splinePoints
                      splinePoints$y <- c(splinePoints$y, pos$y)
                  }
            }
            # Now make BaseLine
            decr <- FALSE #Kinetic energy set
            if (DestFile[[destIndx]]@Flags[1] == TRUE) { decr <- TRUE }
            idx <- order(splinePoints$x, decreasing=decr)
            splinePoints$x <- splinePoints$x[idx] #splinePoints$x in ascending order
            splinePoints$y <- splinePoints$y[idx] #following same order select the correspondent splinePoints$y
            LL <- length(splinePoints$x)

            DestFile[[destIndx]]@Boundaries$x <- c(splinePoints$x[1],splinePoints$x[LL]) #set the boundaries of the baseline
            DestFile[[destIndx]]@Boundaries$y <- c(splinePoints$y[1],splinePoints$y[LL])
            DestFile[[destIndx]] <- XPSsetRegionToFit(DestFile[[destIndx]])
            DestFile[[destIndx]] <- XPSbaseline(DestFile[[destIndx]], BasLinType, deg, Wgt, splinePoints )
            DestFile[[destIndx]]@RSF <- SourceFile[[indx]]@RSF
        } else if (BasLinType == "3P.Shirley" || BasLinType == "LP.Shirley" || BasLinType == "4P.Tougaard") {
            Wgt <- as.numeric(BLinfo[2])
            txt <- paste(BLinfo[1], " background found!\n  ==> Set the Baseline Limits")
            tkmessageBox(message=txt, title="HELP INFO", icon="info")
            plot(DestFile[[destIndx]])
            pos <- locator(n=2, type="p", col="red", lwd=2)
            DestFile[[destIndx]]@Boundaries$x <- pos$x
            DestFile[[destIndx]]@Boundaries$y <- pos$y
            DestFile[[destIndx]] <- XPSsetRegionToFit(DestFile[[destIndx]])
            DestFile[[destIndx]] <- XPSbaseline(DestFile[[destIndx]], BasLinType, deg, Wgt, splinePoints )
            DestFile[[destIndx]]@RSF <- SourceFile[[indx]]@RSF
        }

        plot(DestFile[[destIndx]])
        return(DestFile[[destIndx]])
   }



#---

   SaveSpectrum <- function(){
      activeFName <- tclvalue(XS2)
      assign(activeFName, DestFName, envir=.GlobalEnv) #save changes in the destinationfile
      assign("activeFName", activeFName, envir=.GlobalEnv) #set the Active XPSSample == DestinationFile
      assign("activeSpectName", activeSpectName,envir=.GlobalEnv) #Set the activeSpect == added coreline
      assign("activeSpectIndx", activeSpectIndx,envir=.GlobalEnv) #Set the active Index == index of added coreline
      plot(DestFName)
      XPSSaveRetrieveBkp("save")
      WidgetState(SaveBtn, "disabled")
      WidgetState(SaveNewSpect, "disabled")
      WidgetState(SaveExitBtn, "disabled")
   }

   SaveNewSpectrum <- function(){
      activeFName <- tclvalue(XS11) #name of the manipulated XPSSample (one of the math operations was performed)
      LL <- length(activeFName)
      if ( length(activeFName) > 0 ){     #Math operations performed on SourceFile11
         SourceFName <- DestFName
         DestFName <- get(activeFName, envir=.GlobalEnv) #retrieve original source XPSSample which will be the destination file
         SpectName <- activeSpectName
         SpectIndx <- activeSpectIndx
         Symbol <- paste(prefix,SpectName, sep="")
         CoreLineList <- names(DestFName)
         destIndx <- length(CoreLineList)+1
         DestFName[[destIndx]] <- SourceFName[[SpectIndx]]  #this is the manipulated core line
         DestFName[[destIndx]]@Symbol <- Symbol
         DestFName@names <- c(CoreLineList,Symbol)
         assign(activeFName, DestFName, envir=.GlobalEnv) #Save the changes in a new coreline in the destination file
         assign("activeFName", activeFName, envir=.GlobalEnv)
         assign("activeSpectName", SpectName,envir=.GlobalEnv)
         assign("activeSpectIndx", SpectIndx,envir=.GlobalEnv)
         msg <- paste(" Coreline: ",  Symbol, " saved in the XPS Sample ", DestFName@Filename, sep="")
         cat("\n ==> ", msg)
         plot(DestFName)
         WidgetState(SaveNewSpect, "disabled") #Save Destination file disabled if SourceFile OK
      } else {
         activeFName <- tclvalue(XS1)
         SourceFName <- DestFName
         DestFName <- get(activeFName, envir=.GlobalEnv) #retrieve original source XPSSample which will be the destination file
         SpectName <- activeSpectName
         SpectIndx <- activeSpectIndx
         CoreLineList <- names(DestFName)
         destIndx <- length(CoreLineList)+1
         DestFName[[destIndx]] <- SourceFName[[SpectIndx]]  #this is the manipulated core line
         DestFName[[destIndx]]@Symbol <- SpectName
         DestFName@names <- c(CoreLineList,SpectName)
         assign(activeFName, DestFName, envir=.GlobalEnv) #Save the changes in a new coreline in the destination file
         assign("activeFName", activeFName, envir=.GlobalEnv)
         assign("activeSpectName", SpectName,envir=.GlobalEnv)
         assign("activeSpectIndx", SpectIndx,envir=.GlobalEnv)
         msg <- paste(" Coreline: ",  SpectName, " saved in the XPS Sample ", DestFName@Filename, sep="")
         cat("\n ==> ", msg)
         plot(DestFName)
      }
      WidgetState(SaveBtn, "disabled")
      WidgetState(SaveNewSpect, "disabled")
      WidgetState(SaveExitBtn, "disabled")
   }


#----- variables -----

#---load list of file ID and correspondent FileNames
      FNameList <- XPSFNameList()
      DestFName <- NULL
      RepCLidx <- NULL
      activeSpectIndx <- NULL
      activeSpectName <- NULL
      SampID <- ""
      SpectList <- ""
      CullData <- NULL  #rangeX of the region to cull
      prefix <- ""
      SpectName <- ""
      
      tkmessageBox(message=" Remember to save data after each operation \n otherwise you will loss the results", title="SAVE RESULTS", icon="warning")

#####----main---
      ProcessWin <- tktoplevel()
      tkwm.title(ProcessWin,"CORELINE PROCESSING")
      tkwm.geometry(ProcessWin, "+100+50")   #position respect topleft screen corner

      MainGroup <- ttkframe(ProcessWin, borderwidth=0, padding=c(0,0,0,0) )
      tkgrid(MainGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

      NoteBK <- ttknotebook(MainGroup)
      tkgrid(NoteBK, row = 1, column = 1, padx = 5, pady = 5, sticky="we")

#--- TAB1
      T1group <- ttkframe(NoteBK,  borderwidth=2, padding=c(5,5,5,5) )
      tkadd(NoteBK, T1group, text="CORELINE PROCESSING")
      T1group1 <- ttkframe(T1group, borderwidth=0, padding=c(0,0,0,0) )
      tkgrid(T1group1, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

      AddFrame1 <- ttklabelframe(T1group1, text = "SELECT THE SOURCE XPS-SAMPLE", borderwidth=2)
      tkgrid(AddFrame1, row = 1, column = 1, padx = 5, pady = 3, sticky="we")
      XS1 <- tclVar()
      SourceFile1 <- ttkcombobox(AddFrame1, width = 25, textvariable = XS1, values = FNameList)
      tkgrid(SourceFile1, row = 1, column = 1, padx = 5, pady = 3, sticky="w")
      tkbind(SourceFile1, "<<ComboboxSelected>>", function(){
                               SelectedFName <- tclvalue(XS1)
                               SpectList <<- XPSSpectList(SelectedFName)
                               SourceFName <- get(SelectedFName,envir=.GlobalEnv)  #load the source XPSSample file
                               tkconfigure(SourceCoreline1, values=SpectList)
                               plot(SourceFName)
                               WidgetState(SourceCoreline1, "normal") #enable core line selection
                 })

      AddFrame2 <- ttklabelframe(T1group1, text = " SELECT CORELINE TO PROCESS ", borderwidth=2)
      tkgrid(AddFrame2, row = 2, column = 1, padx = 5, pady = 3, sticky="we")
      CL1 <- tclVar()
      SourceCoreline1 <- ttkcombobox(AddFrame2, width = 25, textvariable = CL1, values = SpectList)
      tkgrid(SourceCoreline1, row = 1, column = 1, padx = 5, pady = 3, sticky="w")
      tkbind(SourceCoreline1, "<<ComboboxSelected>>", function(){
                               WidgetState(DestFileName, "normal") # selection destination file enabled
                 })
      WidgetState(SourceCoreline1, "disabled")

      AddFrame3 <- ttklabelframe(T1group1, text = "SELECT THE DESTINATION FILE NAME", borderwidth=2)
      tkgrid(AddFrame3, row = 3, column = 1, padx = 5, pady = 3, sticky="we")
      XS2 <- tclVar()
      DestFileName <- ttkcombobox(AddFrame3, width = 25, textvariable = XS2, values = FNameList)
      tkgrid(DestFileName, row = 1, column = 1, padx = 5, pady = 3, sticky="w")
      tkbind(DestFileName, "<<ComboboxSelected>>", function(){
                               SourceFile <- tclvalue(XS1)
                               DestFile <- tclvalue(XS2)
                               SpectName <- tclvalue(CL1)
                               SpectName <- unlist(strsplit(SpectName, "\\."))   #split the string at character "."
                               SpectIndx <- as.integer(SpectName[1])
                               SpectName <- SpectName[2]
                               if (SourceFile == DestFile) {
                                  tkmessageBox(message="Warning: Destination File Name == Source File Name" , title = "WARNING: BAD DESTINATION FILE NAME!",  icon = "warning")
                               } else {
                                  SourceFile <- get(SourceFile, envir=.GlobalEnv)
                                  DestFile <- get(DestFile, envir=.GlobalEnv)
                                  destIndx <- grep(SpectName, names(DestFile))
                                  if (length(destIndx) == 0){
                                     txt <- paste("Warning: no ",SpectName, " core line present in the destination file. Continue?")
                                     answ <- tkmessageBox(message=txt , type="yesno", title = "WARNING: NO CORELINE PRESENT!",  icon = "warning")
                                     if (tclvalue(answ) == "no") {
                                        return()
                                     }
                                  } else if (length(destIndx) > 0){
                                     LL <- length(destIndx)
                                     for(ii in 1:LL){
                                         if (DestFile[[destIndx[ii] ]]@Flags[1] != SourceFile[[SpectIndx]]@Flags[1]) { #acquisitions made using different energy scale
                                             tkmessageBox(message="XPS-Samples have different Energy units. Operation stopped!" , title = "WARNING: BAD DESTINATION FILE!",  icon = "warning")
                                             return()
                                         }
                                     }
                                  }
                                  plot(DestFile)
                                  WidgetState(SaveBtn, "normal") # enable saving data if Dest File OK
                                  WidgetState(SaveExitBtn, "normal")
                               }
                 })
      WidgetState(DestFileName, "disabled") # selection of destination file blocked

      AddFrame4 <- ttklabelframe(T1group1, text = " ADD WHOLE CORELINE ", borderwidth=2)
      tkgrid(AddFrame4, row = 4, column = 1, padx = 5, pady = 3, sticky="we")
      Addbutton1 <- tkbutton(AddFrame4, text=" Add New Coreline and Fit ", width=35, command=function(){
                               SourceFile <- tclvalue(XS1)
                               SourceCoreline <- tclvalue(CL1)
                               DestFile <- tclvalue(XS2)
                               if (length(SourceCoreline) == 0) {   #No coreline selected
                                   tkmessageBox(message="Please select the source coreline please!" , title = "SOURCE CORELINE SELECTION",  icon = "warning")
                               } else if (length(DestFile) == 0) {   #No destination file selected
                                   tkmessageBox(message="Please select the destination file please!" , title = "DESTINATION FILE SELECTION",  icon = "warning")
                               } else {
                                   SourceFile <- get(SourceFile, envir=.GlobalEnv)
                                   DestFName <<- get(DestFile, envir=.GlobalEnv)
                                   SourceCoreline <- unlist(strsplit(SourceCoreline, "\\."))   #split string at character "."
                                   SpectName <- SourceCoreline[2]
                                   SpectIndx <- as.integer(SourceCoreline[1])
                                   CoreLineList <- names(DestFName)
                                   destIndx <- length(DestFName)+1
                                   DestFName[[destIndx]] <<- SourceFile[[SpectIndx]]
                                   DestFName@names <<- c(CoreLineList,SourceFile[[SpectIndx]]@Symbol)
                                   activeSpectIndx <<- destIndx
                                   activeSpectName <<- SpectName
                                   msg <- paste(" Coreline: ",  SourceCoreline[2], "added to XPS Sample. PLEASE SAVE DATA!", DestFName@Filename)
                                   cat("\n ==> ", msg)
                                   plot(DestFName)
                                   WidgetState(SaveBtn, "normal")  #Enable SaveSpectrum
                                   WidgetState(SaveExitBtn, "normal")
                               }
                 })
      tkgrid(Addbutton1, row = 1, column = 1, padx = 5, pady = 3, sticky="w")

      AddFrame5 <- ttklabelframe(T1group1, text = " ADD BASELINE AND FIT ", borderwidth=2)
      tkgrid(AddFrame5, row = 5, column = 1, padx = 5, pady = 3, sticky="we")
      Addbutton2 <- tkbutton(AddFrame5, text="Add Baseline and Fit to Original Coreline", width=35, command=function(){
                               SourceFile <- tclvalue(XS1)
                               SourceCoreline <- tclvalue(CL1)
                               DestFile <- tclvalue(XS2)
                               if (length(SourceCoreline) == 0) {   #no coreline selected
                                  tkmessageBox(message="Please select the source coreline please!" , title = "SOURCE CORELINE SELECTION",  icon = "warning")
                               } else if (length(DestFile) == 0) {  #no destination file selected
                                  tkmessageBox(message="Please select the destination file please!" , title = "DESTINATION FILE SELECTION",  icon = "warning")
                               } else {
                                  SourceFile <- get(SourceFile, envir=.GlobalEnv)
                                  DestSpectList <- XPSSpectList(DestFile)
                                  DestFName <<- get(DestFile, envir=.GlobalEnv)
                                  SourceCoreline <- unlist(strsplit(SourceCoreline, "\\."))
                                  SpectIndx <- as.integer(SourceCoreline[1])
                                  SpectName <- SourceCoreline[2]
                                  CoreLineList <- names(DestFName)
                                  destIndx <- grep(SpectName, CoreLineList) #The index of the coreline in the destinationFile can be different from that od sourceFile => grep()

                                  if (length(destIndx) > 1){                #The same coreline can be present more than one time
                                     CtrlRepCL(destIndx, SpectName, DestSpectList)
                                     destIndx <- RepCLidx
                                  }

                                  if (length(destIndx) == 0) {
                                      text <- paste(SpectName, "not present in the Destination File: Fit copy stopped")
                                      tkmessageBox(message=text , title = "FIT COPY TO DESTINATION",  icon = "warning")
                                  } else {
                                      if (length(SourceFile[[SpectIndx]]@RegionToFit) == 0){
                                          text <- paste("ATTENTION: NO fit No BaseLine found for ",SpectName, " of the Source XPS-Sample. Operation stopped.", sep="")
                                          tkmessageBox(message=text , title = "WARNING",  icon = "warning")
                                          return()
                                      }
                                      if (length(DestFName[[destIndx]]@Components) > 0) {  #a fit is present for the selected coreline
                                          text <- paste("ATTENTION: constraints of ",SpectName, " fit will be kept! Continue?", sep="")
                                          answ <- tkmessageBox(message=text, type="yesno", title = "WARNING!",  icon = "warning")
                                          if (tclvalue(answ) == "yes") {
                                              DestFName[[destIndx]] <<- XPSremove(DestFName[[destIndx]],"all")
                                          } else {
                                              return()
                                          }
                                      }
                                      DestFName[[destIndx]] <<- MakeBaseLine(SourceFile, SpectIndx, DestFName, destIndx)
                                      DestFName[[destIndx]]@Components <<- SourceFile[[SpectIndx]]@Components
                                      RescaleH <- max(DestFName[[destIndx]]@RegionToFit$y)/max(SourceFile[[SpectIndx]]@RegionToFit$y) #scale factor between source and destination corelines
                                      tmp <- NULL
                                      LL <- length(DestFName[[destIndx]]@Components)
                                      if (LL > 0) {
                                          for(ii in 1:LL) {
                                              varmu <- getParam(DestFName[[destIndx]]@Components[[ii]],variable="mu")
                                              DestFName[[destIndx]]@Components[[ii]] <<- setParam(DestFName[[destIndx]]@Components[[ii]], parameter=NULL, variable="mu", value=varmu)
                                              varh <- getParam(DestFName[[destIndx]]@Components[[ii]],variable="h")
                                              DestFName[[destIndx]]@Components[[ii]] <<- Ycomponent(DestFName[[destIndx]]@Components[[ii]], x=DestFName[[destIndx]]@RegionToFit$x, y=DestFName[[destIndx]]@Baseline$y/RescaleH) #Rescale Baseline Y values
                                              DestFName[[destIndx]]@Components[[ii]]@ycoor <<- RescaleH*DestFName[[destIndx]]@Components[[ii]]@ycoor  #Rescale Values of Y Components respect DestFName Y data
                                              varh$start <- max(DestFName[[destIndx]]@Components[[ii]]@ycoor-DestFName[[destIndx]]@Baseline$y)  #Substract Baseline
                                              varh$min <- 0
                                              varh$max <- 5*varh$start
                                              DestFName[[destIndx]]@Components[[ii]] <<- setParam(DestFName[[destIndx]]@Components[[ii]], parameter=NULL, variable="h", value=varh)
                                          }
                                          tmp <- sapply(DestFName[[destIndx]]@Components, function(z) matrix(data=z@ycoor))
                                          DestFName[[destIndx]]@Fit$y <<- (colSums(t(tmp)) - length(DestFName[[destIndx]]@Components)*(DestFName[[destIndx]]@Baseline$y))
                                      }                                           #transpose of tmp

                                      plot(DestFName[[destIndx]])
                                      activeSpectIndx <<- destIndx
                                      activeSpectName <<- SpectName
                                      msg <- paste(" Fit data added to ", SpectName, "PLASE SAVE DATA!", sep="")
                                      cat("\n ==> ", msg)
                                  }
                                  WidgetState(SaveBtn, "normal")  #Enable SaveSpectrum
                                  WidgetState(SaveExitBtn, "normal")
                               }
                 })
      tkgrid(Addbutton2, row = 1, column = 1, padx = 5, pady = 3, sticky="w")

      AddFrame6 <- ttklabelframe(T1group1, text = " ADD FIT ONLY ", borderwidth=2)
      tkgrid(AddFrame6, row = 6, column = 1, padx = 5, pady = 3, sticky="we")
      Addbutton3 <- tkbutton(AddFrame6, text=" Add Fit ", width=35, command=function(){
                               SourceFile <- tclvalue(XS1)
                               SourceCoreline <- tclvalue(CL1)
                               DestFile <- tclvalue(XS2)
                               if (length(SourceCoreline)==0) {   #No coreline has been selected in source File
                                  tkmessageBox(message="Please select the source coreline please!", title = "SOURCE CORELINE SELECTION",  icon = "warning")
                               } else if (length(DestFile)==0) {  #No Destination File has been selected
                                  tkmessageBox(message="Please select the destination file please!", title = "DESTINATION FILE SELECTION",  icon = "warning")
                               } else {
                                  SourceFile <- get(SourceFile, envir=.GlobalEnv)
                                  DestSpectList <- XPSSpectList(DestFile)
                                  DestFName <<- get(DestFile, envir=.GlobalEnv)
                                  SourceCoreline <- unlist(strsplit(SourceCoreline, "\\."))   #split string at char "."
                                  SpectName <- SourceCoreline[2]
                                  SpectIndx <- as.integer(SourceCoreline[1])
                                  CoreLineList <- names(DestFName)
                                  destIndx <- grep(SpectName, CoreLineList)  #The selected CoreLine name could be in any posiiton in the Destination XPSSample => source Samp Index could be different from Dest Samp index
                                  if (length(destIndx) == 0) {
                                      text <- paste(SpectName, "not present in the Destination File: Fit copy stopped")
                                      tkmessageBox(message=text, title = "FIT COPY TO DESTINATION STOPPED",  icon = "warning")
                                  } else {

                                     if (length(destIndx) > 1){                #The same coreline can be present more than one time
                                        CtrlRepCL(destIndx, SpectName, DestSpectList)
                                        destIndx <- RepCLidx
                                     }

                                     if (length(DestFName[[destIndx]]@Components) > 0) {  #Core line already fitted
                                          text <- paste("ATTENTION: fit present on ",SpectName, " New fit constaints will be kept! Continue?")
                                          answ <- tkmessageBox(message=text, type="yesno", title = "WARNING!",  icon = "warning")
                                          if (tclvalue(answ) ==  "yes") {
                                              DestFName[[destIndx]] <<- XPSremove(DestFName[[destIndx]],"all")
                                          } else {
                                              return()
                                          }
                                      }
                                      if (length(DestFName[[destIndx]]@Baseline) == 0) {  #baseline not present
                                         text <- paste("Baseline not present in the Destination File: Fit copy stopped")
                                         tkmessageBox(message=text, title = "FIT COPY TO DESTINATION STOPPED",  icon = "warning")
                                         return()
                                      }

                                      DestFName[[destIndx]] <<- XPSsetRSF(DestFName[[destIndx]])
                                      DestFName[[destIndx]]@Components <<- SourceFile[[SpectIndx]]@Components
                                      RescaleH <- max(DestFName[[destIndx]]@RegionToFit$y)/max(SourceFile[[SpectIndx]]@RegionToFit$y) #fattore di scala tra source coreline e destination coreline
                                      LL=length(DestFName[[destIndx]]@Components)

                                      for(ii in 1:LL) {
                                          varmu <- getParam(DestFName[[destIndx]]@Components[[ii]],variable="mu")
                                          DestFName[[destIndx]]@Components[[ii]] <<- setParam(DestFName[[destIndx]]@Components[[ii]], parameter=NULL, variable="mu", value=varmu)
                                          varh <- getParam(DestFName[[destIndx]]@Components[[ii]],variable="h")
                                          DestFName[[destIndx]]@Components[[ii]] <<- Ycomponent(DestFName[[destIndx]]@Components[[ii]], x=DestFName[[destIndx]]@RegionToFit$x, y=DestFName[[destIndx]]@Baseline$y/RescaleH) #calcola la Y ed aggiunge la baseline
                                          DestFName[[destIndx]]@Components[[ii]]@ycoor <<- RescaleH*DestFName[[destIndx]]@Components[[ii]]@ycoor
                                          varh$start <- max(DestFName[[destIndx]]@Components[[ii]]@ycoor-DestFName[[destIndx]]@Baseline$y)
                                          varh$min <- 0
                                          varh$max <- 5*varh$start
                                          DestFName[[destIndx]]@Components[[ii]] <<- setParam(DestFName[[destIndx]]@Components[[ii]], parameter=NULL, variable="h", value=varh)
                                      }
                                      tmp <- sapply(DestFName[[destIndx]]@Components, function(z) matrix(data=z@ycoor))
                                      DestFName[[destIndx]]@Fit$y <<- (colSums(t(tmp)) - length(DestFName[[destIndx]]@Components)*(DestFName[[destIndx]]@Baseline$y))
                                      plot(DestFName[[destIndx]])
                                      activeSpectIndx <<- destIndx
                                      activeSpectName <<- SpectName
                                      msg <- paste(" Fit data added to ", SpectName, "PLASE SAVE DATA!", sep="")
                                      cat("\n ==> ", msg)
                                      plot(DestFName)
                                      WidgetState(SaveBtn, "normal")  #Enable SaveSpectrum
                                      WidgetState(SaveExitBtn, "normal")
                                  }
                               }
                 })
      tkgrid(Addbutton3, row = 1, column = 1, padx = 5, pady = 3, sticky="w")

      AddFrame7 <- ttklabelframe(T1group1, text = " OVERWRITE A CORELINE ", borderwidth=2)
      tkgrid(AddFrame7, row = 7, column = 1, padx = 5, pady = 3, sticky="we")
      overbutton <- tkbutton(AddFrame7, text=" Overwrite ", width=35, command=function(){
                               SourceFile <- tclvalue(XS1)
                               SourceCoreline <- tclvalue(CL1)
                               DestFile <- tclvalue(XS2)
                               if (length(SourceCoreline) == 0) {   #No coreline selected
                                   tkmessageBox(message="Please select the source coreline please!" , title = "SOURCE CORELINE SELECTION",  icon = "warning")
                               } else if (length(DestFile) == 0) {   #No dest File selected
                                   tkmessageBox(message="Please select the destination file please!" , title = "DESTINATION FILE SELECTION",  icon = "warning")
                               } else {
                                  SourceFile <- get(SourceFile, envir=.GlobalEnv)
                                  DestFName <<- get(DestFile, envir=.GlobalEnv)
                                  DestSpectList <- XPSSpectList(DestFile)
                                  SourceCoreline <- unlist(strsplit(SourceCoreline, "\\."))
                                  SpectName <- SourceCoreline[2]
                                  SpectIndx <- as.integer(SourceCoreline[1])
                                  CoreLineList <- names(DestFName)
                                  destIndx <- grep(SpectName, CoreLineList)  #The selected CoreLine name could be in any posiiton in the Destination XPSSample => source Samp Index could be different from Dest Samp index
                                  if (length(destIndx) > 1){                 #The same coreline can be present more than one time
                                     CtrlRepCL(destIndx, SpectName, DestSpectList)
                                     destIndx <- RepCLidx
                                  }
                                  DestFName[[destIndx]] <<- SourceFile[[SpectIndx]]
                                  plot(DestFName[[destIndx]])
                                  activeSpectIndx <<- destIndx
                                  activeSpectName <<- SpectName
                                  msg <- paste("Coreline ", SpectName, "overwritten. PLASE SAVE DATA!")
                                  cat("\n ==> ", msg)
                                  plot(DestFName)
                                  WidgetState(SaveBtn, "normal")  #Enable SaveSpectrum
                                  WidgetState(SaveExitBtn, "normal")
                               }
                 })
      tkgrid(overbutton, row = 1, column = 1, padx = 5, pady = 3, sticky="w")

      AddFrame8 <- ttklabelframe(T1group1, text = " REMOVE/DUPLICATE CORELINE ", borderwidth=2)
      tkgrid(AddFrame8, row = 8, column = 1, padx = 5, pady = 3, sticky="we")
      delbutton <- tkbutton(AddFrame8, text=" Remove ", width=15, command=function(){
                               SourceFile <- tclvalue(XS1)
                               SourceCoreline <- tclvalue(CL1)
                               DestFile <- tclvalue(XS2)
                               SpectList <<- XPSSpectList(SourceFile)
                               if (length(SourceCoreline)==0) {   #No coreline selected
                                   tkmessageBox(message="Please select the source coreline please!" , title = "SOURCE CORELINE SELECTION",  icon = "warning")
                               }
                               if (length(SourceFile)==0) {   #No Source File selected
                                   tkmessageBox(message="Please select the Source File please!" , title = "SOURCE FILE SELECTION",  icon = "warning")
                               } else {
                                  WidgetState(SaveBtn, "normal") # enabling the Save options if the destination file name is OK
                                  WidgetState(SaveExitBtn, "normal")
                                  tclvalue(XS2) <- SourceFile
                                  SourceFile <- get(SourceFile, envir=.GlobalEnv)
                                  SourceCoreline <- unlist(strsplit(SourceCoreline, "\\."))
                                  SpectIndx <- as.integer(SourceCoreline[1])
                                  SpectName <- SourceCoreline[2]
                                  plot(SourceFile[[SpectIndx]])
                                  text <- paste("ATTENTION: are you sure to delete the",tclvalue(CL1), "core line ?", sep="")
                                  answ <- tkmessageBox(message=text, type="yesno", title="WARNING", icon="warning")
                                  if (tclvalue(answ) == "yes") {
                                     SourceFile[[SpectIndx]] <- NULL #this eliminates the coreline
                                     DestFName <<- SourceFile      #move updated XPSSample in the destinatin file for saving
                                     activeSpectIndx <<- 1
                                     activeSpectName <<- names(SourceFile)[1]
                                     msg <- paste("Coreline ", SpectName, "deleted. PLEASE SAVE DATA!")
                                     cat("\n ==> ", msg)
                                     plot(SourceFile)
                                  } else {
                                     return()  #do nothing
                                  }
                                  SpectList <<- names(SourceFile) #update the SpectrumList
                                  idx <- seq(1:length(SpectList)) #cannot use XPSSpectList because loads the SourceFile from the .GlobalEnv
                                  SpectList <<- paste(idx, SpectList, sep=".")
                                  tkconfigure(SourceCoreline1, values=SpectList)
                                  WidgetState(SaveBtn, "normal")  #Enable SaveSpectrum\
                                  WidgetState(SaveExitBtn, "normal")  #Enable SaveSpectrum
                              }
                 })
      tkgrid(delbutton, row = 1, column = 1, padx = 5, pady = 3, sticky="we")

      duplibutton <- tkbutton(AddFrame8, text=" Duplicate ", width=15, command=function(){
                               SourceFile <- tclvalue(XS1)
                               SourceCoreline <- tclvalue(CL1)
                               DestFile <- tclvalue(XS2)
                               if (length(SourceCoreline)==0) {   #No Coreline selected
                                   tkmessageBox(message="Please select the source coreline please!" , title = "SOURCE CORELINE SELECTION",  icon = "warning")
                               }
                               if (length(DestFile)==0) {   #Destination file not selected
                                   DestFile <- SourceFile
                               }
                               if (SourceFile != DestFile) {#Source different from Destination file
                                   DestFile <- SourceFile
                               }
                               if (length(SourceFile)==0) { #Source File not selected
                                   tkmessageBox(message="Please select the Source File please!" , title = "SOURCE FILE SELECTION",  icon = "warning")
                               } else {
                                  tclvalue(XS2) <- SourceFile
                                  DestFName <<- get(SourceFile, envir=.GlobalEnv)
                                  #--- eliminates the initial N. and reconstruct CLname also
                                  #    in presence of compex names as 10.D.2.ST.VB
                                  tmp <- unlist(strsplit(SourceCoreline, "\\."))
                                  SpectIndx <- as.integer(tmp[1])
                                  LL1 <- nchar(tmp[1])+2   # +2: substr(x,start,stop) start includes the character at 'start'
                                  LL2 <- nchar(SourceCoreline)
                                  SpectName <- substr(SourceCoreline, start=LL1, stop=LL2) #In case of XPSCLname="4.D1.C1s" SpectName must be "D1.C1s"
                                  CoreLineList <- names(DestFName)
                                  destIndx <- length(DestFName)+1
                                  DestFName[[destIndx]] <<- DestFName[[SpectIndx]]
                                  DestFName[[destIndx]]@Symbol <<- SpectName
                                  DestFName@names <<- c(CoreLineList,SpectName)
                                  activeSpectIndx <<- destIndx
                                  activeSpectName <<- SpectName
                                  msg <- paste(" Coreline: ",  SpectName, "added to XPS Sample", DestFName@Filename, ". PLEASE SAVE DATA!", sep="")
                                  cat("\n ==> ", msg)
                                  plot(DestFName)
                                  WidgetState(SaveBtn, "normal")  #Enable SaveSpectrum\
                                  WidgetState(SaveExitBtn, "normal")  #Enable SaveSpectrum
                               }
                 })
      tkgrid(duplibutton, row = 1, column = 2, padx = 5, pady = 3, sticky="we")

#---

      AddFrame9 <- ttklabelframe(T1group, text = " PICK UP DATA ", borderwidth=2)
      tkgrid(AddFrame9, row = 1, column = 2, padx = 5, pady = 3, sticky="we")

      XX1 <- tclVar("From ? ")  #sets the initial msg
      CullFrom <- ttkentry(AddFrame9, textvariable=XX1, width=20, foreground="grey")
      tkbind(CullFrom, "<FocusIn>", function(K){
                               tclvalue(XX1) <- ""
                               tkconfigure(CullFrom, foreground="red")
                 })
      tkbind(CullFrom, "<Key-Return>", function(K){
                               tkconfigure(CullFrom, foreground="black")
                               CullData[1] <<- as.numeric(tclvalue(XX1))
                 })
      tkgrid(CullFrom, row = 1, column = 1, padx = 5, pady = 3, sticky="w")

      XX2 <- tclVar("To ? ")  #sets the initial msg
      CullTo <- ttkentry(AddFrame9, textvariable=XX2, width=20, foreground="grey")
      tkbind(CullTo, "<FocusIn>", function(K){
                               tclvalue(XX2) <- ""
                               tkconfigure(CullTo, foreground="red")
                 })
      tkbind(CullTo, "<Key-Return>", function(K){
                               tkconfigure(CullTo, foreground="black")
                               CullData[2] <<- as.numeric(tclvalue(XX2))
                 })
      tkgrid(CullTo, row = 2, column = 1, padx = 5, pady = 3, sticky="w")

      MouseBtn1 <- tkbutton(AddFrame9, text=" MOUSE ", width=20, command=function(){
                               SourceFile <- tclvalue(XS1)
                               SourceCoreline <- tclvalue(CL1)
                               DestFile <- SourceFile
                               tclvalue(XX1) <<- "From ?"
                               tclvalue(XX2) <<- "To ?"
                               SourceCoreline <- unlist(strsplit(SourceCoreline, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                               SpectIndx <- as.integer(SourceCoreline[1])
                               SpectName <- SourceCoreline[2]
                               if (length(SourceFile)==0) {   #non e' stata selezionata alcuna coreline
                                   tkmessageBox(message="Please select the Source File please!" , title = "SOURCE FILE SELECTION",  icon = "warning")
                                   return()
                               }
                               if (length(SourceCoreline)==0) {   #non e' stata selezionata alcuna coreline
                                   tkmessageBox(message="Please select the source coreline please!" , title = "SOURCE CORELINE SELECTION",  icon = "warning")
                                   return()
                               } else {
                                  tclvalue(XS2) <<- SourceFile
                                  DestFName <<- get(SourceFile, envir=.GlobalEnv)
                                  SourceCoreline <- unlist(strsplit(SourceCoreline, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                                  SpectIndx <- as.integer(SourceCoreline[1])
                                  SpectName <- SourceCoreline[2]
                                  if (length(DestFName[[SpectIndx]]@RegionToFit)>0) {   #non e' stata selezionata alcuna coreline
                                     tkmessageBox(message="Baseline or Fit present. Reset analysis before culling data!" , title = " RESET ANALYSIS ",  icon = "warning")
                                     return()
                                  }
                                  plot(DestFName[[SpectIndx]])
                                  pos <- locator(n=2, type="p", col="red", lwd=2, pch=3)
                                  CullData[1] <<- pos$x[1]
                                  CullData[2] <<- pos$x[2]
                               }
                         })
      tkgrid(MouseBtn1, row = 3, column = 1, padx = 5, pady = 3, sticky="w")

      CullBtn <- tkbutton(AddFrame9, text=" SELECT DATA ", width=20, command=function(){
                               SourceFile <- tclvalue(XS1)
                               SourceCoreline <- tclvalue(CL1)
                               DestFile <- SourceFile
                               if (length(SourceFile)==0) {   #non e' stata selezionata alcuna coreline
                                  tkmessageBox(message="Please select the Source File please!" , title = "SOURCE FILE SELECTION",  icon = "warning")
                                  return()
                               }
                               if (length(SourceCoreline)==0) {   #non e' stata selezionata alcuna coreline
                                  tkmessageBox(message="Please select the source coreline please!" , title = "SOURCE CORELINE SELECTION",  icon = "warning")
                               } else {
                                  SourceCoreline <- unlist(strsplit(SourceCoreline, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                                  SpectIndx <- as.integer(SourceCoreline[1])
                                  SpectName <- SourceCoreline[2]
                                  DestFName <<- get(SourceFile, envir=.GlobalEnv)
                                  if (length(DestFName[[SpectIndx]]@RegionToFit)>0) {   #non e' stata selezionata alcuna coreline
                                      tkmessageBox(message="Baseline or Fit present. Reset analysis before culling data!" , title = " RESET ANALYSIS ",  icon = "warning")
                                      return()
                                  }
                                  if (is.null(CullData[1]) || is.null(CullData[2])) {
                                      tkmessageBox(message="From/To points NOT defined!" , title = " DEFINE FROM/TO ",  icon = "warning")
                                      return()
                                  }
                                  from <- findXIndex(DestFName[[SpectIndx]]@.Data[[1]], CullData[1])
                                  to <- findXIndex(DestFName[[SpectIndx]]@.Data[[1]], CullData[2])
                                  if (from > to) {
                                     tmp <- from
                                     from <- to
                                     to <- tmp
                                  }
                                  DestFName[[SpectIndx]]@.Data[[1]] <<- DestFName[[SpectIndx]]@.Data[[1]][from:to]
                                  DestFName[[SpectIndx]]@.Data[[2]] <<- DestFName[[SpectIndx]]@.Data[[2]][from:to]
                                  DestFName[[SpectIndx]]@.Data[[3]] <<- DestFName[[SpectIndx]]@.Data[[3]][from:to]
                                  activeSpectIndx <<- SpectIndx
                                  activeSpectName <<- SpectName
                                  plot(DestFName[[SpectIndx]])
                                  msg <- "Please SAVE Selected Data"
                                  cat("\n ==> ", msg)
                                  WidgetState(SaveNewSpect, "normal")
                                  WidgetState(SaveExitBtn, "normal")
                               }
                         })
      tkgrid(CullBtn, row = 4, column = 1, padx = 5, pady = 3, sticky="w")

      ResetBtn <- tkbutton(AddFrame9, text=" RESET ", width=20, command=function(){
                               SourceFile <- tclvalue(XS1)
                               SourceCoreline <- tclvalue(CL1)
                               tclvalue(XS2) <- SourceFile
                               tclvalue(XX1) <<- "From ?"
                               tclvalue(XX2) <<- "To ?"
                               DestFName <<- get(SourceFile, envir=.GlobalEnv)
                               SourceCoreline <- unlist(strsplit(SourceCoreline, "\\."))
                               SpectIndx <- as.integer(SourceCoreline[1])
                               plot(DestFName[[SpectIndx]])
                         })
      tkgrid(ResetBtn, row = 5, column = 1, padx = 5, pady = 3, sticky="w")



###---TAB2
      T2group <- ttkframe(NoteBK,  borderwidth=2, padding=c(5,5,5,5) )
      tkadd(NoteBK, T2group, text="CORELINE MATH")

      MathFrame1 <- ttklabelframe(T2group, text = "SELECT XPS-SAMPLE 1", borderwidth=2)
      tkgrid(MathFrame1, row = 1, column = 1, padx = 5, pady = 3, sticky="we")
      XS11 <- tclVar()
      SourceFile11 <- ttkcombobox(MathFrame1, width = 20, textvariable = XS11, values = FNameList)
      tkgrid(SourceFile11, row = 1, column = 1, padx = 5, pady = 3, sticky="w")
      tkbind(SourceFile11, "<<ComboboxSelected>>", function(){
                               tclvalue(XS2) <- SelectedFName <- tclvalue(XS11)
                               SpectList <<- XPSSpectList(SelectedFName)
                               SourceFName <- get(SelectedFName,envir=.GlobalEnv)  #load the source XPSSample file
                               tkconfigure(SourceCoreLine11, values=SpectList)
                               WidgetState(SourceCoreLine11, "normal")
                 })

      MathFrame11 <- ttklabelframe(T2group, text = " SELECT CORELINE 1 ", borderwidth=2)
      tkgrid(MathFrame11, row = 2, column = 1, padx = 5, pady = 3, sticky="we")
      SpectList <<- ""
      CL11 <- tclVar()
      SourceCoreLine11 <- ttkcombobox(MathFrame11, width = 20, textvariable = CL11, values = SpectList)
      tkgrid(SourceCoreLine11, row = 1, column = 1, padx = 5, pady = 3, sticky="w")
      tkbind(SourceCoreLine11, "<<ComboboxSelected>>", function(){
                               SourceFile <- tclvalue(XS11)
                               SourceFile <- get(SourceFile, envir=.GlobalEnv)
                               Coreline <- tclvalue(CL11)
                               Coreline <- unlist(strsplit(Coreline, "\\."))   #Split string at point
                               SpectIndx <- as.integer(Coreline[1])
                               Range <- range(SourceFile[[SpectIndx]]@.Data[1])
                               Range <- round(Range, 1)
                               tkconfigure(CLRange11, text=paste("Range", "  ", Coreline[2], "_X: ", Range[1], ",  ", Range[2], sep=""))
                               WidgetState(DestFileName, "normal") # Enable the choice of the destination file
                 })
      WidgetState(SourceCoreLine11, "disabled")

      CLRange11 <- ttklabel(MathFrame11, text="Range CoreLine1: ")
      tkgrid(CLRange11, row = 2, column = 1, padx = 5, pady = c(0, 3), sticky="w")

      MathFrame2 <- ttklabelframe(T2group, text = "SELECT XPS-SAMPLE 2", borderwidth=2)
      tkgrid(MathFrame2, row = 1, column = 2, padx = 5, pady = 3, sticky="we")
      XS22 <- tclVar()
      SourceFile22 <- ttkcombobox(MathFrame2, width = 20, textvariable = XS22, values = FNameList)
      tkgrid(SourceFile22, row = 1, column = 1, padx = 5, pady = 3, sticky="w")
      tkbind(SourceFile22, "<<ComboboxSelected>>", function(){
                               tclvalue(XS2) <- SelectedFName <- tclvalue(XS22)
                               if (length(tclvalue(CL11)) == 0) {
                                   tkmessageBox(message="Please select the CORELINE1 first", title = "WARNING: CORELINE1 NOT DEFINED ",  icon = "warning")
                                   tclvalue(XS22) <- NULL
                               } else {
                                   SpectList <<- XPSSpectList(SelectedFName)
                                   SourceFName <- get(SelectedFName,envir=.GlobalEnv)  #load the source XPSSample file
                                   tkconfigure(SourceCoreLine22, values=SpectList)
                                   WidgetState(SourceCoreLine22, "normal") # Enable the choice of the destination file
                               }
                 })

      MathFrame22 <- ttklabelframe(T2group, text = "SELECT CORELINE 2", borderwidth=2)
      tkgrid(MathFrame22, row = 2, column = 2, padx = 5, pady = 3, sticky="we")
      CL22 <- tclVar()
      SpectList <- ""
      SourceCoreLine22 <- ttkcombobox(MathFrame22, width = 20, textvariable = CL22, values = SpectList)
      tkgrid(SourceCoreLine22, row = 1, column = 1, padx = 5, pady = 3, sticky="w")
      tkbind(SourceCoreLine22, "<<ComboboxSelected>>", function(){
                               SourceFile <- tclvalue(XS22)
                               SourceFile <- get(SourceFile, envir=.GlobalEnv)
                               Coreline <- tclvalue(CL22)
                               Coreline <- unlist(strsplit(Coreline, "\\."))   #Split string at point
                               SpectIndx <- as.integer(Coreline[1])
                               Range <- range(SourceFile[[SpectIndx]]@.Data[1])
                               Range <- round(Range, 1)
                               tkconfigure(CLRange22, text=paste("Range", "  ", Coreline[2], "_X: ", Range[1], ",  ", Range[2], sep=""))

                 })
      WidgetState(SourceCoreLine22, "disabled") # Enable the choice of the destination file
      CLRange22 <- ttklabel(MathFrame22, text="Range CoreLine2: ")
      tkgrid(CLRange22, row = 2, column = 1, padx = 5, pady = c(0, 3), sticky="w")

      MathFrame3 <- ttklabelframe(T2group, text = "ADD A CONSTANT VALUE TO CORELINE1", borderwidth=2)
      tkgrid(MathFrame3, row = 3, column = 1, padx = 5, pady = 3, sticky="we")
      CC1 <- tclVar("Value ? ")
      AddValue <- ttkentry(MathFrame3, textvariable=CC1, foreground="grey")
      tkbind(AddValue, "<FocusIn>", function(K){
                               tclvalue(CC1) <- ""
                               tkconfigure(AddValue, foreground="red")
                        })
      tkbind(AddValue, "<Key-Return>", function(K){
                               tkconfigure(AddValue, foreground="black")
                               value <- as.numeric(tclvalue(CC1))
                               SourceFile <- tclvalue(XS2) <- tclvalue(XS11)
                               SourceCoreline <- tclvalue(CL11)
                               SourceFile <- get(SourceFile, envir=.GlobalEnv)
                               SourceCoreline <- unlist(strsplit(SourceCoreline, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                               SpectIndx <- as.integer(SourceCoreline[1])
                               SpectName <- SourceCoreline[2]
                               SourceFile[[SpectIndx]]@.Data[[2]] <- SourceFile[[SpectIndx]]@.Data[[2]]+value
                               if (length(SourceFile[[SpectIndx]]@RegionToFit) > 0) {
                                   SourceFile[[SpectIndx]]@RegionToFit[[2]] <- SourceFile[[SpectIndx]]@RegionToFit[[2]]+value
                               }
                               if (length(SourceFile[[SpectIndx]]@Baseline) > 0) {
                                   SourceFile[[SpectIndx]]@Baseline[[2]] <- SourceFile[[SpectIndx]]@Baseline[[2]]+value
                               }
                               LL <- length(SourceFile[[SpectIndx]]@Components)
                               if (LL > 0) {
                                   for (jj in c(1:LL)) {
		                                      SourceFile[[SpectIndx]]@Components[[jj]]@ycoor <- SourceFile[[SpectIndx]]@Components[[jj]]@ycoor+value
	                                  }
                               }
                               plot(SourceFile[[SpectIndx]])
                               DestFName <<- SourceFile
                               activeSpectIndx <<- SpectIndx
                               activeSpectName <<- SpectName
                               prefix <<- "M."
                               WidgetState(SaveBtn, "normal")
                               WidgetState(SaveNewSpect, "normal")
                               WidgetState(SaveExitBtn, "normal")
                               msg <- paste("Constant ",value, " added to CoreLine ", SpectName)
                               cat("\n ==> ", msg)
                     })
      tkgrid(AddValue, row = 1, column = 1, padx = 5, pady = 3, sticky="w")

      MathFrame33 <- ttklabelframe(T2group, text = "MULTIPLY CORELINE1 BY A CONSTANT", borderwidth=2)
      tkgrid(MathFrame33, row = 3, column = 2, padx = 5, pady = 3, sticky="we")
      CC2 <- tclVar("Value ? ")  #sets the initial msg
      MultValue <- ttkentry(MathFrame33, textvariable=CC2, foreground="grey")
      tkbind(MultValue, "<FocusIn>", function(K){
                               tclvalue(CC2) <- ""
                               tkconfigure(MultValue, foreground="red")
                        })
      tkbind(MultValue, "<Key-Return>", function(K){
                               tkconfigure(MultValue, foreground="black")
                               value <- as.numeric(tclvalue(CC2))
                               SourceFile <- tclvalue(XS2) <- tclvalue(XS11)
                               SourceCoreline <- tclvalue(CL11)
                               SourceFile <- get(SourceFile, envir=.GlobalEnv)
                               SourceCoreline <- unlist(strsplit(SourceCoreline, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                               SpectIndx <- as.integer(SourceCoreline[1])
                               SpectName <- SourceCoreline[2]
                               SourceFile[[SpectIndx]]@.Data[[2]] <- SourceFile[[SpectIndx]]@.Data[[2]]*value
                               if (length(SourceFile[[SpectIndx]]@RegionToFit) > 0){
                                   SourceFile[[SpectIndx]]@RegionToFit[[2]] <- SourceFile[[SpectIndx]]@RegionToFit[[2]]*value
                               }
                               if (length(SourceFile[[SpectIndx]]@Baseline) > 0){
                                   SourceFile[[SpectIndx]]@Baseline[[2]] <- SourceFile[[SpectIndx]]@Baseline[[2]]*value
                               }
                               LL <- length(SourceFile[[SpectIndx]]@Components)
                               if (LL > 0) {
                                   for (jj in 1:LL) {
	                                      SourceFile[[SpectIndx]]@Components[[jj]]@ycoor <- SourceFile[[SpectIndx]]@Components[[jj]]@ycoor*value
			                                }
			                                SourceFile[[SpectIndx]]@Fit$y <- SourceFile[[SpectIndx]]@Fit$y*value
                               }
                               plot(SourceFile[[SpectIndx]])
                               DestFName <<- SourceFile
                               activeSpectIndx <<- SpectIndx
                               activeSpectName <<- SpectName
                               WidgetState(SaveBtn, "normal")
                               WidgetState(SaveNewSpect, "normal")
                               WidgetState(SaveExitBtn, "normal")
                               msg <- paste("XX CoreLine ", SpectName, " multiplied by ", value)
                               cat("\n ==> ", msg)
                     })
      tkgrid(MultValue, row = 1, column = 1, padx = 5, pady = 3, sticky="w")

      MathFrame4 <- ttklabelframe(T2group, text = " DIFFERENTIATE ", borderwidth=2)
      tkgrid(MathFrame4, row = 4, column = 1, padx = 5, pady = 3, sticky="we")
      DiffBtn <- tkbutton(MathFrame4, text=" DIFFERENTIATE CORELINE1 ", command=function(){
                               SourceFile <- tclvalue(XS2) <- tclvalue(XS11)
                               CoreLine <- tclvalue(CL11)
                               SourceFile <- get(SourceFile1, envir=.GlobalEnv)
                               CoreLine <- unlist(strsplit(CoreLine, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                               SpectIndx <- as.integer(CoreLine[1])
                               SpectName <- CoreLine[2]
                               LL <- length(SourceFile[[SpectIndx]]@.Data[[1]])
                               SourceFile[[SpectIndx]]@RegionToFit <- list(x=NULL, y=NULL)
                               SourceFile[[SpectIndx]]@Baseline <- list(x=NULL, y=NULL, type=NULL)
                               SourceFile[[SpectIndx]]@Baseline$x <- SourceFile[[SpectIndx]]@RegionToFit$x <- SourceFile[[SpectIndx]]@.Data[[1]]
                               SourceFile[[SpectIndx]]@Baseline$y <- rep(0, LL) #creo una baseline fittizia nulla
                               SourceFile[[SpectIndx]]@Baseline$type <- "linear"
                               k <- max(SourceFile[[SpectIndx]]@.Data[[2]])-min(SourceFile[[SpectIndx]]@.Data[[2]])
                               for (ii in 2:LL){
                                    SourceFile[[SpectIndx]]@RegionToFit$y[ii] <- SourceFile[[SpectIndx]]@.Data[[2]][ii]-SourceFile[[SpectIndx]]@.Data[[2]][ii-1]
                               }
                               SourceFile[[SpectIndx]]@RegionToFit$y[1] <- SourceFile[[SpectIndx]]@RegionToFit$y[2]
                               Dmin <- min(SourceFile[[SpectIndx]]@RegionToFit$y)
                               Dmax <- max(SourceFile[[SpectIndx]]@RegionToFit$y)
                               SourceFile[[SpectIndx]]@RegionToFit$y <- k*(SourceFile[[SpectIndx]]@RegionToFit$y-Dmin)/(Dmax-Dmin)
                               matplot(x=matrix(c(SourceFile[[SpectIndx]]@.Data[[1]], SourceFile[[SpectIndx]]@RegionToFit$x), nrow=LL, ncol=2),
                                       y=matrix(c(SourceFile[[SpectIndx]]@.Data[[2]], SourceFile[[SpectIndx]]@RegionToFit$y), nrow=LL, ncol=2),
                                       type="l", lty=c(1,1), col=c("black","blue"),
                                       xlab=SourceFile[[SpectIndx]]@units[1], ylab=SourceFile[[SpectIndx]]@units[2])
                               DestFName <<- SourceFile
                               activeSpectIndx <<- SpectIndx
                               activeSpectName <<- SpectName
                               WidgetState(SaveNewSpect, "normal")
                               WidgetState(SaveExitBtn, "normal")
                               prefix <<- "D1."
                               msg <- paste(SpectName, " differentiated", sep="")
                               cat("\n ==> ", msg)
                     })
      tkgrid(DiffBtn, row = 1, column = 1, padx = 5, pady = 3, sticky="we")

      MathFrame44 <- ttklabelframe(T2group, text = " COMBINE CORELINE1 and CORELINE2 \n Coreline2 integrated in Coreline1", borderwidth=2)
      tkgrid(MathFrame44, row = 4, column = 2, padx = 5, pady = 3, sticky="we")
      CombineBtn <- tkbutton(MathFrame44, text=" COMBINE ", command=function(){
                               SourceFile1 <- tclvalue(XS11)
                               CoreLine1 <- tclvalue(CL11)
                               SourceFile1 <- get(SourceFile1, envir=.GlobalEnv)
                               CoreLine1 <- unlist(strsplit(CoreLine1, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                               SpectIndx1 <- as.integer(CoreLine1[1])
                               SpectName1 <- CoreLine1[2]

                               SourceFile2 <- tclvalue(XS22)
                               CoreLine2 <- tclvalue(CL22)
                               SourceFile2 <- get(SourceFile2, envir=.GlobalEnv)
                               CoreLine2 <- unlist(strsplit(CoreLine2, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                               SpectIndx2 <- as.integer(CoreLine2[1])
                               SpectName2 <- CoreLine2[2]
                               
                               if (length(SourceFile1[[SpectIndx1]]@RegionToFit)>0 || length(SourceFile2[[SpectIndx2]]@RegionToFit)){
                                   txt <- paste("Combine CoreLines  allowed ONLY on RAW data! RESET ANALYSIS")
                                   tkmessageBox(message=txt, title = "WARNING: FIT PRESENT ",  icon = "warning")
                               } else {
                                   if (SourceFile1[[SpectIndx1]]@Flags[1] == TRUE){
                                       SourceFile1[[SpectIndx1]]@.Data[[1]] <- rev(SourceFile1[[SpectIndx1]]@.Data[[1]])#scale in ascending order (KE)
                                       SourceFile1[[SpectIndx1]]@.Data[[2]] <- rev(SourceFile1[[SpectIndx1]]@.Data[[2]])#order also Y data
                                   }
                                   if (SourceFile2[[SpectIndx2]]@Flags[1] == TRUE){
                                       SourceFile2[[SpectIndx2]]@.Data[[1]] <- rev(SourceFile2[[SpectIndx2]]@.Data[[1]])
                                       SourceFile2[[SpectIndx2]]@.Data[[2]] <- rev(SourceFile2[[SpectIndx2]]@.Data[[2]])
                                   }
                                   Rng1 <- range(SourceFile1[[SpectIndx1]]@.Data[[1]])
                                   Rng2 <- range(SourceFile2[[SpectIndx2]]@.Data[[1]])
                                   MaxX1 <- max(Rng1)
                                   MaxX2 <- max(Rng2)
                                   MinX1 <- min(Rng1)
                                   MinX2 <- min(Rng2)
                                   LL1 <- length(SourceFile1[[SpectIndx1]]@.Data[[1]])
                                   LL2 <- length(SourceFile2[[SpectIndx2]]@.Data[[1]])
                                   if (MaxX1 > MaxX2) {
                                       if (MaxX2 < MinX1) {
                                           tkmessageBox(message="XPS-SAMPLE1 and XPS-SAMPLE2 are separated! \n
                                                        XPS-SAMPLE1 must overlap XPS-SAMPLE2 at least in ONE point", icon="warning")
                                           return()
                                       } else {
                                           Idx2 <- findXIndex(SourceFile2[[SpectIndx2]]@.Data[[1]], MinX1)
                                           DataToCombineX <- SourceFile2[[SpectIndx2]]@.Data[[1]][1:Idx2]
                                           DataToCombineY <- SourceFile2[[SpectIndx2]]@.Data[[2]][1:Idx2]
                                           yy1 <- SourceFile1[[SpectIndx1]]@.Data[[2]][1]    #average intensity of second block of data in the combination point
                                           yy2 <- SourceFile2[[SpectIndx2]]@.Data[[2]][Idx2] #average intensity of first block of data in the combination point
                                           Dy <- yy2-yy1 #difference of the intensities between block1, block2 od data in the combination point
                                           SourceFile1[[SpectIndx1]]@.Data[[1]] <- c(DataToCombineX, SourceFile1[[SpectIndx1]]@.Data[[1]])
                                           SourceFile1[[SpectIndx1]]@.Data[[2]] <- c(DataToCombineY-Dy, SourceFile1[[SpectIndx1]]@.Data[[2]])
                                           if (SourceFile1[[SpectIndx1]]@Flags[1] == TRUE){ #Restore original order
                                               SourceFile1[[SpectIndx1]]@.Data[[1]] <- rev(SourceFile1[[SpectIndx1]]@.Data[[1]])#scale in ascending order (KE)
                                               SourceFile1[[SpectIndx1]]@.Data[[2]] <- rev(SourceFile1[[SpectIndx1]]@.Data[[2]])#order also Y data
                                           }

                                           plot(SourceFile1[[SpectIndx1]])
                                           tclvalue(XS2) <- tclvalue(XS11)
                                           DestFName <<- SourceFile1
                                           activeSpectIndx <<- SpectIndx1
                                           activeSpectName <<- SpectName
                                       }
                                   } else if (MaxX2 > MaxX1) {
                                       if (MaxX1 < MinX2) {
                                           tkmessageBox(message="XPS-SAMPLE1 and XPS-SAMPLE2 are separated! \n
                                                        XPS-SAMPLE1 must overlap XPS-SAMPLE2 at least in ONE point", icon="warning")
                                           return()
                                       } else {
                                           MaxX1 <- SourceFile1[[SpectIndx1]]@.Data[[1]][LL1]
                                           Idx2 <- findXIndex(SourceFile2[[SpectIndx2]]@.Data[[1]], MaxX1)+1 #+1 needed to not start with the data next to MaxX1
                                           DataToCombineX <- SourceFile2[[SpectIndx2]]@.Data[[1]][Idx2:LL2]
                                           DataToCombineY <- SourceFile2[[SpectIndx2]]@.Data[[2]][Idx2:LL2]
                                           yy1 <- SourceFile1[[SpectIndx1]]@.Data[[2]][LL1] #average intensity of first block of data in the combination point
                                           yy2 <- SourceFile2[[SpectIndx2]]@.Data[[2]][Idx2] #average intensity of second block of data in the combination point
                                           Dy <- yy2-yy1 #difference of the intensities between block1, block2 od data in the combination point
                                           SourceFile1[[SpectIndx1]]@.Data[[1]] <- c(SourceFile1[[SpectIndx1]]@.Data[[1]], DataToCombineX)
                                           SourceFile1[[SpectIndx1]]@.Data[[2]] <- c(SourceFile1[[SpectIndx1]]@.Data[[2]], DataToCombineY-Dy)
                                           if (SourceFile1[[SpectIndx1]]@Flags[1] == TRUE){ #Restore original order
                                               SourceFile1[[SpectIndx1]]@.Data[[1]] <- rev(SourceFile1[[SpectIndx1]]@.Data[[1]])#scale in ascending order (KE)
                                               SourceFile1[[SpectIndx1]]@.Data[[2]] <- rev(SourceFile1[[SpectIndx1]]@.Data[[2]])#order also Y data
                                           }

                                           plot(SourceFile1[[SpectIndx1]])
                                           tclvalue(XS2) <- tclvalue(XS11)
                                           DestFName <<- SourceFile1
                                           activeSpectIndx <<- SpectIndx1
                                           activeSpectName <<- SpectName
                                       }
                                   }
                               }
                               WidgetState(SaveBtn, "normal")
                               WidgetState(SaveNewSpect, "normal")
                               WidgetState(SaveExitBtn, "normal")
                               prefix <<- "M."
                               msg <- paste(tclvalue(CL11)," combined with ", tclvalue(CL22), sep="")
                               cat("\n ==> ", msg)
                     })
      tkgrid(CombineBtn, row = 1, column = 1, padx = 5, pady = 3, sticky="we")

      MathFrame5 <- ttklabelframe(T2group, text = " ADD CORELINE2 TO CORELINE1 ", borderwidth=2)
      tkgrid(MathFrame5, row = 5, column = 1, padx = 5, pady = 3, sticky="we")
      AddBtn <- tkbutton(MathFrame5, text=" ADD SPECTRA ", command=function(){
                               SourceFile1 <- tclvalue(XS2) <- tclvalue(XS11)
                               CoreLine1 <- tclvalue(CL11)
                               SourceFile1 <- get(SourceFile1, envir=.GlobalEnv)
                               CoreLine1 <- unlist(strsplit(CoreLine1, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                               SpectIndx1 <- as.integer(CoreLine1[1])
                               SpectName1 <- CoreLine1[2]

                               SourceFile2 <- tclvalue(XS22)
                               CoreLine2 <- tclvalue(CL22)
                               SourceFile2 <- get(SourceFile2, envir=.GlobalEnv)
                               CoreLine2 <- unlist(strsplit(CoreLine2, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                               SpectIndx2 <- as.integer(CoreLine2[1])
                               SpectName2 <- CoreLine2[2]

                               if (hasBaseline(SourceFile1[[SpectIndx1]]) || hasBaseline(SourceFile2[[SpectIndx2]]) ){
                                   tkmessageBox(message="ADDITION CAN BE DONE ONLY ON RAW DATA: PLEASE REMOVE ANALYSIS", title = "WARNING!",  icon = "warning")
                               } else {
                                   txt="          ==> ADDITION WILL BE PERFOMED IN THE COMMON ENERGY RANGE\nSAVE AND SAVE&EXIT WILL OVERWRITE THE RESULT TO CORE LINE 1"
                                   answ <- tkmessageBox(message=txt, type="yesno", title = "WARNING!",  icon = "warning")
                                   if (tclvalue(answ) == "yes") {
                                       Range1 <- range(SourceFile1[[SpectIndx1]]@.Data[1])
                                       Range2 <- range(SourceFile2[[SpectIndx2]]@.Data[1])
                                       lim1 <- max(Range1[1], Range2[1])  #the greater of the lower limits
                                       lim2 <- min(Range1[2], Range2[2])  #the smaller of the higher limits
                                       idx1 <- findXIndex(SourceFile1[[SpectIndx1]]@.Data[[1]], lim1)
                                       idx2 <- findXIndex(SourceFile1[[SpectIndx1]]@.Data[[1]], lim2)
                                       if (SourceFile1[[SpectIndx1]]@Flags[1]) {  #Binding energy scale
                                           CoreLineX1 <- SourceFile1[[SpectIndx1]]@.Data[[1]][idx2:idx1]
                                           CoreLineY1 <- SourceFile1[[SpectIndx1]]@.Data[[2]][idx2:idx1]
                                           CoreLineSF1 <- SourceFile1[[SpectIndx1]]@.Data[[3]][idx2:idx1]    #Analyzer Transf Funct.
                                       } else {                               #Kinetic energy scale
                                           CoreLineX1 <- SourceFile1[[SpectIndx1]]@.Data[[1]][idx1:idx2]
                                           CoreLineY1 <- SourceFile1[[SpectIndx1]]@.Data[[2]][idx1:idx2]
                                           CoreLineSF1 <- SourceFile1[[SpectIndx1]]@.Data[[3]][idx1:idx2]    #Analyzer Transf Funct.
                                       }
                                       LL1 <- length(CoreLineY1)
                                       idx1 <- findXIndex(SourceFile2[[SpectIndx2]]@.Data[[1]], lim1)
                                       idx2 <- findXIndex(SourceFile2[[SpectIndx2]]@.Data[[1]], lim2)
                                       if (SourceFile2[[SpectIndx2]]@Flags[1]) {  #Binding energy scale
                                           CoreLineY2 <- SourceFile2[[SpectIndx2]]@.Data[[2]][idx2:idx1]
                                       } else {                               #Kinetic energy scale
                                          CoreLineY2 <- SourceFile2[[SpectIndx2]]@.Data[[2]][idx1:idx2]
                                       }
                                       LL2 <- length(CoreLineY2)
                                       if(LL1 > LL2){  #verify CoreLine1, CoreLine2 have same length
                                          kk <- LL1-LL2   #if CoreLine1 is longer
                                          CoreLineY2[LL2:(LL2+kk)] <- CoreLineY2[LL2]  #add lacking values to CoreLine2
                                       } else if (LL1 < LL2){  #if CoreLine2 longer
                                          kk <- LL2-LL1
                                          CoreLineY2 <- CoreLineY2[1:(LL2-kk)]  #drop exceeding values
                                       }

                                       Nswps1 <- strsplit(SourceFile1[[SpectIndx1]]@Info[2], "Sweeps:")     #split str in 2 parts: before and after  "sweeps:"
                                       Nsw1 <- as.numeric(substr(Nswps1[[1]][2], 1, 2))                 #keep the first 2 characters of the second part = Nsweeps and transform in integer
                                       Nswps2 <- strsplit(SourceFile1[[SpectIndx2]]@Info[2], "Sweeps:")
                                       Nsw2 <- as.numeric(substr(Nswps2[[1]][2], 1, 2))
                                       SourceFile1[[SpectIndx1]]@.Data[[1]] <- CoreLineX1
                                       SourceFile1[[SpectIndx1]]@.Data[[2]] <- (CoreLineY1*Nsw1+CoreLineY2*Nsw2)/(Nsw1+Nsw2) #renormalize data
                                       SourceFile1[[SpectIndx1]]@.Data[[3]] <- CoreLineSF1

                                       Nsw <- as.character(Nsw1+Nsw2)
                                       Nswps1 <- paste(Nswps1[[1]][1], "Sweeps: ",Nsw,substr(Nswps1[[1]][2], 3, nchar(Nswps1[[1]][2])), sep="")     #compose the new second part of the string with correct number of sweeps
                                       SourceFile1[[SpectIndx1]]@Info[2] <- Nswps1
                                       plot(SourceFile1[[SpectIndx1]])
                                       DestFName <<- SourceFile1
                                       activeSpectIndx <<- SpectIndx1
                                       activeSpectName <<- SpectName1
                                       WidgetState(SaveBtn, "normal")
                                       WidgetState(SaveNewSpect, "normal")
                                       WidgetState(SaveExitBtn, "normal")
                                       prefix <<- "M."
                                       msg <- paste(SpectName2,"-", tclvalue(XS22), " added to ", SpectName1, "-", tclvalue(XS11))
                                       cat("\n ==> ", msg)
                                   }
                               }
                     })
      tkgrid(AddBtn, row = 1, column = 1, padx = 5, pady = 3, sticky="we")

      MathFrame55 <- ttklabelframe(T2group, text = " SELECT NORMALIZATION MODE ", borderwidth=2)
      tkgrid(MathFrame55, row = 5, column = 2, padx = 5, pady = 3, sticky="we")
      NRMMOD <- tclVar()
      NormMode <- ttkcombobox(MathFrame55, width = 20, textvariable = NRMMOD, values = c("Normalize to the main Peak", "Normalize to Selected Peak"))
      tkbind(NormMode, "<<ComboboxSelected>>", function(){
                               SourceFile <- tclvalue(XS2) <- tclvalue(XS11)
                               CoreLine <- tclvalue(CL11)
                               CoreLine <- unlist(strsplit(CoreLine, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                               SpectIndx <- as.integer(CoreLine[1])
                               SpectName <- CoreLine[2]
                               if (length(SourceFile) == 0 || length(CoreLine) == 0){
                                   tkmessageBox(message="Source XPSSample or Source Core-Line not defined!", title="WARNING", icon="warning")
                                   return()
                               }
                               SourceFile <- get(SourceFile, envir=.GlobalEnv)
                               answ <- tclvalue(NRMMOD)
                               if (answ == "Normalize to the main Peak"){
                                   maxY <- max(SourceFile[[SpectIndx]]@.Data[[2]])
                                   minY <- min(SourceFile[[SpectIndx]]@.Data[[2]])
                               } else if (answ == "Normalize to Selected Peak"){
                                   tkmessageBox(message="Define the Reference Peak Edges", title="PEAK EDGES DEFINITION", icon="info")
                                   pos <- locator(n=2, type="p", col="red", cex=1.5, lwd=2, pch=1)
                                   LL <- length(SourceFile[[SpectIndx]]@.Data[[1]])
                                   idx <- NULL
                                   idx[1] <- findXIndex(SourceFile[[SpectIndx]]@.Data[[1]], pos$x[1])
                                   idx[2] <- findXIndex(SourceFile[[SpectIndx]]@.Data[[1]], pos$x[2])
                                   idx <- sort(idx, decreasing = FALSE)
	                                  maxY <- max(SourceFile[[SpectIndx]]@.Data[[2]][idx[1]:idx[2]])
	                                  minY <- min(SourceFile[[SpectIndx]]@.Data[[2]][idx[1]:idx[2]])
                               }
                               SourceFile[[SpectIndx]]@.Data[[2]] <- (SourceFile[[SpectIndx]]@.Data[[2]]-minY)/(maxY-minY)
                               if(length(SourceFile[[SpectIndx]]@RegionToFit) > 0){
                                   SourceFile[[SpectIndx]]@RegionToFit$y <- (SourceFile[[SpectIndx]]@RegionToFit$y-minY)/(maxY-minY)
                               }
                               if(length(SourceFile[[SpectIndx]]@Baseline) > 0){
                                   SourceFile[[SpectIndx]]@Baseline$y <- (SourceFile[[SpectIndx]]@Baseline$y-minY)/(maxY-minY)
                               }
                               LL <- length(SourceFile[[SpectIndx]]@Components)
                               if (LL > 0){
                                   for(ii in 1:LL){
                                       SourceFile[[SpectIndx]]@Components[[ii]]@ycoor <- (SourceFile[[SpectIndx]]@Components[[ii]]@ycoor-minY)/(maxY-minY)
                                   }
                                   SourceFile[[SpectIndx]]@Fit$y <- SourceFile[[SpectIndx]]@Fit$y/(maxY-minY)
                               }
                               SourceFile[[SpectIndx]]@Boundaries$y <- c(0, 1)
                               plot(SourceFile[[SpectIndx]])
                               DestFName <<- SourceFile
                               activeSpectIndx <<- SpectIndx
                               activeSpectName <<- SpectName
                               WidgetState(SaveBtn, "normal")
                               WidgetState(SaveNewSpect, "normal")
                               WidgetState(SaveExitBtn, "normal")
                               prefix <<- ""
                               msg <- paste(SpectName, " normalized", sep="")
                               cat("\n ==> ", msg)
                     })
      tkgrid(NormMode, row = 1, column = 1, padx = 5, pady = 3, sticky="we")

      MathFrame6 <- ttklabelframe(T2group, text = " SUBTRACT CORELINE2 FROM CORELINE1 ", borderwidth=2)
      tkgrid(MathFrame6, row = 6, column = 1, padx = 5, pady = 3, sticky="we")
      SubtrBtn <- tkbutton(MathFrame6, text=" SUBTRACT SPECTRA ", command=function(){
                               SourceFile1 <- tclvalue(XS2) <- tclvalue(XS11)
                               CoreLine1 <- tclvalue(CL11)
                               SourceFile1 <- get(SourceFile1, envir=.GlobalEnv)
                               CoreLine1 <- unlist(strsplit(CoreLine1, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                               SpectIndx1 <- as.integer(CoreLine1[1])
                               SpectName1 <- CoreLine1[2]

                               SourceFile2 <- tclvalue(XS22)
                               CoreLine2 <- tclvalue(CL22)
                               SourceFile2 <- get(SourceFile2, envir=.GlobalEnv)
                               CoreLine2 <- unlist(strsplit(CoreLine2, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                               SpectIndx2 <- as.integer(CoreLine2[1])
                               SpectName2 <- CoreLine2[2]

                               if (hasBaseline(SourceFile1[[SpectIndx1]]) || hasBaseline(SourceFile2[[SpectIndx2]]) ){
                                   tkmessageBox(message="SUBTRACTION CAN BE DONE ONLY ON RAW DATA: PLEASE REMOVE ANALYSIS" , title = "WARNING!",  icon = "warning")
                               } else {
                                   txt="          ==> SUBTRACTION WILL BE PERFOMED IN THE COMMON ENERGY RANGE\nSAVE AND SAVE&EXIT WILL OVERWRITE SUBTRACTION TO THE ORIGINAL CORE LINE"
                                   answ <- tkmessageBox(message=txt, type="yesno", title = "WARNING!",  icon = "warning")
                                   if (tclvalue(answ) == "yes") {
                                       Range1 <- range(SourceFile1[[SpectIndx1]]@.Data[1])
                                       Range2 <- range(SourceFile2[[SpectIndx2]]@.Data[1])
                                       lim1 <- max(Range1[1], Range2[1])  #il piu' grande dei limiti inferiori
                                       lim2 <- min(Range1[2], Range2[2])  #il piu' piccolo dei limiti superiori
                                       idx1 <- findXIndex(SourceFile1[[SpectIndx1]]@.Data[[1]], lim1)
                                       idx2 <- findXIndex(SourceFile1[[SpectIndx1]]@.Data[[1]], lim2)
                                       if (SourceFile1[[SpectIndx1]]@Flags[1]) {  #Binding energy scale
                                           CoreLineX1 <- SourceFile1[[SpectIndx1]]@.Data[[1]][idx2:idx1]
                                           CoreLineY1 <- SourceFile1[[SpectIndx1]]@.Data[[2]][idx2:idx1]
                                           CoreLineSF1 <- SourceFile1[[SpectIndx1]]@.Data[[3]][idx2:idx1]    #Analyzer Transf Funct.
                                       } else {                               #Kinetic energy scale
                                           CoreLineX1 <- SourceFile1[[SpectIndx1]]@.Data[[1]][idx1:idx2]
                                           CoreLineY1 <- SourceFile1[[SpectIndx1]]@.Data[[2]][idx1:idx2]
                                           CoreLineSF1 <- SourceFile1[[SpectIndx1]]@.Data[[3]][idx1:idx2]    #Analyzer Transf Funct.
                                       }
                                       LL1 <- length(CoreLineY1)
                                       idx1 <- findXIndex(SourceFile2[[SpectIndx2]]@.Data[[1]], lim1)
                                       idx2 <- findXIndex(SourceFile2[[SpectIndx2]]@.Data[[1]], lim2)
                                       if (SourceFile2[[SpectIndx2]]@Flags[1]) {  #Binding energy scale
                                           CoreLineY2 <- SourceFile2[[SpectIndx2]]@.Data[[2]][idx2:idx1]
                                       } else {                               #Kinetic energy scale
                                           CoreLineY2 <- SourceFile2[[SpectIndx2]]@.Data[[2]][idx1:idx2]
                                       }
                                       LL2 <- length(CoreLineY2)
                                       if (LL1 > LL2){  #verify CoreLine1, CoreLine2 have same length
                                           kk <- LL1-LL2   #if CoreLine1 is longer
                                           CoreLineY2[LL2:(LL2+kk)] <- CoreLineY2[LL2]  #add lacking values to CoreLine2
                                       } else if (LL1 < LL2){  #if CoreLine2 longer
                                           kk <- LL2-LL1
                                           CoreLineY2 <- CoreLineY2[1:(LL2-kk)]  #drop exceeding values
                                       }

                                       SourceFile1[[SpectIndx1]]@.Data[[1]] <- CoreLineX1
                                       SourceFile1[[SpectIndx1]]@.Data[[3]] <- CoreLineSF1

                                       Nswps1 <- strsplit(SourceFile1[[SpectIndx1]]@Info[2], "Sweeps:")    #taglia la str in 2 parti: ql prima e ql dopo "Sweeps:"
                                       Nsw1 <- as.numeric(substr(Nswps1[[1]][2], 1, 2))                 #della seconda parte prendo i primi due caratteri che indicano Nswps e trasforno in intero
                                       Nswps2 <- strsplit(SourceFile1[[SpectIndx2]]@Info[2], "Sweeps:")
                                       Nsw2 <- as.numeric(substr(Nswps2[[1]][2], 1, 2))
                                       if (Nsw1 >= Nsw2) {
                                           SourceFile1[[SpectIndx1]]@.Data[[2]] <- CoreLineY1-CoreLineY2
                                           Nsw1 <- as.character(Nsw1)
                                           Nswps1 <- paste(Nswps1[[1]][1], "Sweeps: ",Nsw1,substr(Nswps1[[1]][2], 3, nchar(Nswps1[[1]][2])), sep="")     #ricostruisco la secopnda parte della stinga
                                           SourceFile1[[SpectIndx1]]@Info[2] <- Nswps1
                                           plot(SourceFile1[[SpectIndx1]])
                                           DestFName <<- SourceFile1
                                           activeSpectIndx <<- SpectIndx1
                                           activeSpectName <<- SpectName1
                                           WidgetState(SaveBtn, "normal")
                                           WidgetState(SaveNewSpect, "normal")
                                           WidgetState(SaveExitBtn, "normal")
                                           prefix <<- "M."
                                           msg <- paste(SpectName2,"-", tclvalue(XS22), " subtracted from ", SpectName1, "-", tclvalue(XS11))
                                           cat("\n ==> ", msg)
                                       } else {
                                           tkmessageBox(message="Nsweeps CoreLine2 > Nsweeps CoreLine1: SUBTRACTION NOT ALLOWED" , title = "WARNING!",  icon = "warning")
                                       }
                                   }
                               }
                     })
      tkgrid(SubtrBtn, row = 1, column = 1, padx = 5, pady = 3, sticky="we")

      MathFrame66 <- ttklabelframe(T2group, text = " SUBTRACT THE BASELINE ", borderwidth=2)
      tkgrid(MathFrame66, row = 6, column = 2, padx = 5, pady = 3, sticky="we")
      tkgrid( ttklabel(MathFrame66, text=">>> WARNING: Data OUTSIDE the \n    BaseLine Range will be lost!"),
              row = 1, column = 1, padx = 5, pady = 3, sticky="w")
      SubtrBLBtn <- tkbutton(MathFrame66, text=" SUBTRACT ", command=function(){
                               answ <- tkmessageBox(message="ATTENTION: original data will be IRREVERSIBLY modified!", 
                                                    type="yesno", title = "BASELINE SUBTRACTION",  icon = "warning")
                               if (tclvalue(answ) == "yes"){
                                   SourceFile <- tclvalue(XS2) <- tclvalue(XS11)
                                   CoreLine <- tclvalue(CL11)
                                   SourceFile <- get(SourceFile, envir=.GlobalEnv)
                                   CoreLine <- unlist(strsplit(CoreLine, "\\."))   #tolgo il "NUMERO." all'inizio del nome coreline
                                   SpectIndx <- as.integer(CoreLine[1])
                                   SpectName <- CoreLine[2]
                                   if (length(SourceFile[[SpectIndx]]@RegionToFit)>0 && length(SourceFile[[SpectIndx]]@Baseline)>0) {
                                       SourceFile[[SpectIndx]]@RegionToFit[[2]] <- SourceFile[[SpectIndx]]@RegionToFit[[2]]-SourceFile[[SpectIndx]]@Baseline[[2]]
                                   }
                                   SourceFile[[SpectIndx]]@.Data <- SourceFile[[SpectIndx]]@RegionToFit
                                   LL <- length(SourceFile[[SpectIndx]]@Components)
                                   if (LL > 0) {
                                       for (ii in 1:LL) {
		                                          SourceFile[[SpectIndx]]@Components[[ii]]@ycoor <- SourceFile[[SpectIndx]]@Components[[ii]]@ycoor - SourceFile[[SpectIndx]]@Baseline[[2]]
	                                      }
                                   }
                                   SourceFile[[SpectIndx]]@Baseline[[2]] <- SourceFile[[SpectIndx]]@Baseline[[2]]*0 #annullo la baseline

                                   plot(SourceFile[[SpectIndx]])
                                   DestFName <<- SourceFile
                                   activeSpectIndx <<- SpectIndx
                                   activeSpectName <<- SpectName
                                   WidgetState(SaveBtn, "normal")
                                   WidgetState(SaveNewSpect, "normal")
                                   WidgetState(SaveExitBtn, "normal")
                                   msg <- paste("Baseline substracted")
                                   cat("\n ==> ", msg)
                               }
                     })
      tkgrid(SubtrBLBtn, row = 2, column = 1, padx = 5, pady = 3, sticky="we")



#---COMMON BUTTONS

      ButtGroup1 <- ttkframe(MainGroup, borderwidth=0, padding=c(0,0,0,0) )
      tkgrid(ButtGroup1, row = 2, column = 1, padx = 0, pady = 0, sticky="w")

      SaveBtn <- tkbutton(ButtGroup1, text="  SAVE  ", width = 12, command=function(){
                               SaveSpectrum()
                     })
      tkgrid(SaveBtn, row = 1, column = 1, padx = 5, pady = 3, sticky="w")
      WidgetState(SaveBtn, "disabled")

      SaveNewSpect <- tkbutton(ButtGroup1, text=" SAVE AS A NEW CORELINE ", width = 25, command=function(){
                               SaveNewSpectrum()
                     })
      tkgrid(SaveNewSpect, row = 1, column = 2, padx = 5, pady = 3, sticky="w")
      WidgetState(SaveNewSpect, "disabled")

      SaveExitBtn <- tkbutton(ButtGroup1, text=" SAVE & EXIT ", width = 15, command=function(){
                               if (prefix=="D1." || length(CullData > 0)){  #differentiation was performed
                                   CullData <<- NULL
                                   SaveNewSpectrum()   #for spectral differentiation or culled data saving is forced in a new coreline
                               } else {
                                   SaveSpectrum()
                               }
                               tkdestroy(ProcessWin)
                               XPSSaveRetrieveBkp("save")
                     })
      tkgrid(SaveExitBtn, row = 1, column = 3, padx = 5, pady = 3, sticky="w")
      WidgetState(SaveExitBtn, "disabled")

      ExitBtn <- tkbutton(ButtGroup1, text="  EXIT  ", width = 12, command=function(){
                               tkdestroy(ProcessWin)
                               XPSSaveRetrieveBkp("save")
                     })
      tkgrid(ExitBtn, row = 1, column = 4, padx = 5, pady = 3, sticky="w")
      WidgetState(ExitBtn, "disabled")

}

