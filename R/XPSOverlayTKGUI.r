# XPSOverlay function to superpose plots of CoreLine spectra

#' @title XPSOverlay
#' @description function to superpose plots of CoreLine spectra
#'   Provides a userfriendly interface to select XPS-Corelines to overlay
#'   and a selection of plotting options for a personalized data representation
#'   This function is based on the (\code{Lattice}) package.
#' @examples
#' \dontrun{
#' 	XPSOverlay()
#' }
#' @export
#'


XPSOverlay <- function(){

#---- calls the function to plot spectra following option selection -----
   CtrlPlot <- function(){
            if (SaveSelection == FALSE){
                tkmessageBox(message="Please Save Selection" , title = "WARNING: SELECTION NOT SAVED",  icon = "error")
                return()
            }
            if (tclvalue(LEGENDCK) == 1) { Plot_Args$auto.key <<- AutoKey_Args } #legends enabled
            Limits <- XPSovEngine(PlotParameters, Plot_Args, SelectedNames, Xlim, Ylim)
            Xlim <<- Limits[1:2]
            Ylim <<- Limits[3:4]
   }

#--- Routine for drawing Custom Axis
   CustomAx <- function(CustomDta){
#               AxItems <- c("AxMin", "AxMax", "Ntickd")
               axMin <- as.character(round(CustomDta[[1]], 2))
               axMax <- as.character(round(CustomDta[[2]], 2))
               axParam <- list(Data = c(axMin, axMax, "10"))
               axParam <- as.data.frame(axParam, stringsAsFactors=FALSE)
               Title <- "SET AXIS PARAMETERS"
               ColNames <- "PARAMETERS"
               RowNames <- c(paste("Xmin (min=", axMin, "): ", sep=""),
                             paste("Xmax (max=", axMax, "): ", sep=""),
                             "N. ticks")
               axParam <- DFrameTable(axParam, Title, ColNames=ColNames, RowNames=RowNames,
                                      Width=15, Modify=TRUE, Env=environment())
               axParam <- as.numeric(unlist(axParam[[1]]))
               axMin <- axParam[1]     #X or Y scale min value
               axMax <- axParam[2]     #X or Y scale max value
               NTicks <- axParam[3]
               rm(axParam)
               axRange <- sort(c(axMin, axMax))          #X or R scale range
               if (is.na(axMin*axMax)) {
                   tkmessageBox(message="ATTENTION: plase set all the min, max values!", title = "CHANGE X Y RANGE", icon = "error")
               }
               if (is.null(NTicks)){
                   tkmessageBox(message="Please N. Major Ticks  required!", icon="warning")
               } else {
                   dx <- (axMax-axMin)/NTicks
                   axStp <- seq(from=axMin, to=axMax, by=dx)
                   Ticklabels <- as.character(round(axStp,digits=1))
                   if (CustomDta[[3]] == "X") {
                       if (FName[[SpectIndx]]@Flags) {  #Binding energy set reverse X axis
                           axRange <- sort(c(axMin, axMax), decreasing=TRUE)
                       } else {
                           axRange <- sort(c(axMin, axMax))
                       }
                       Plot_Args$scales$x <<- list(at=axStp, labels=Ticklabels)
                       Plot_Args$xlim <<- axRange
                       Xlim <<- axRange
                   } else if (CustomDta[[3]] == "Y") {
                       Plot_Args$scales$y <<- list(at=axStp, labels=Ticklabels)
                       Plot_Args$ylim <<- axRange
                       Ylim <<- axRange
                   }
                   Plot_Args$scales$relation <<- "same"
                   CtrlPlot()
               }

   }

   SetLinesPoints <- function(){
         if ( tclvalue(SETLINES) == "OFF" && tclvalue(SETSYMBOLS) == "OFF") {
            Plot_Args$type <<- " "  #cancel line and symbols and legends
            AutoKey_Args$lines <<- FALSE
            AutoKey_Args$points <<- FALSE
            AutoKey_Args$col <<- rep("white", 20)
            PlotParameters$Colors <<- rep("white", 20)
            Plot_Args$lty <<- LType
            Plot_Args$par.settings$superpose.symbol$col <<- rep("white", 20)
            Plot_Args$par.settings$superpose.symbol$pch <<- rep(STypeIndx[1], 20)
            Plot_Args$par.settings$superpose.line$col <<- rep("white", 20)
         }

         if ( tclvalue(SETLINES) == "ON" && tclvalue(SETSYMBOLS) == "OFF") {
            Plot_Args$type <<- "l"  # lines only
            AutoKey_Args$type <<- "l"
            AutoKey_Args$lines <<- TRUE
            AutoKey_Args$points <<- FALSE
            AutoKey_Args$col <<- Colors
            PlotParameters$Colors <<- Colors
            Plot_Args$lty <<- rep(LType[1], 20)
            Plot_Args$pch <<- rep(STypeIndx[1], 20)
            Plot_Args$par.settings$superpose.line$col <<- Colors #Rainbow plot
            Plot_Args$par.settings$superpose.line$lty <<- rep("solid", 20)
            if (tclvalue(LINETYPE) == "Patterns") {
                Plot_Args$lty <<- LType
                Plot_Args$par.settings$superpose.line$lty <<- LType
            }
            if (tclvalue(BWCOL)=="Black/White") {
               PlotParameters$Colors <<- Colors[1]
               Plot_Args$lty <<- LType
               Plot_Args$par.settings$superpose.line$lty <<- LType
               Plot_Args$par.settings$superpose.line$col <<- rep(Colors[1], 20) #MonoChrome plot
               Plot_Args$par.settings$superpose.symbol$pch <<- rep(STypeIndx[1], 20)
               Plot_Args$par.settings$superpose.symbol$col <<- rep(Colors[1], 20)
               AutoKey_Args$col <<- rep(Colors[1], 20)
            }
         }

         if ( tclvalue(SETLINES) == "OFF" && tclvalue(SETSYMBOLS) == "ON") {
            Plot_Args$type <<- "p"  # symbols only
            AutoKey_Args$type <<- "p"
            AutoKey_Args$lines <<- FALSE
            AutoKey_Args$points <<- TRUE
            AutoKey_Args$pch <<- STypeIndx
            AutoKey_Args$col <<- Colors
            PlotParameters$Colors <<- Colors
            Plot_Args$lty <<- rep(LType[1], 20)
            Plot_Args$pch <<- rep(STypeIndx[1], 20)
            Plot_Args$par.settings$superpose.line$lty <<- rep(LType[1], 20)
            Plot_Args$par.settings$superpose.symbol$pch <<- rep(STypeIndx[1], 20)
            Plot_Args$par.settings$superpose.symbol$col <<- Colors
            if (tclvalue(SYMTYPE) == "Multi-Symbols") {
                Plot_Args$pch <<- STypeIndx
                Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
            }
            if (tclvalue(BWCOL) == "Black/White") {
               PlotParameters$Colors <<- Colors[1]
               Plot_Args$pch <<- STypeIndx
               Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
               if (tclvalue(SYMTYPE) == "Single-Symbols") {
                   Plot_Args$pch <<- rep(STypeIndx[1], 20)
                   Plot_Args$par.settings$superpose.symbol$pch <<- rep(STypeIndx[1], 20)
               }
               Plot_Args$par.settings$superpose.symbol$col <<- rep(Colors[1], 20)
               AutoKey_Args$col <<- rep(Colors[1], 20)
            }
         }

         if ( tclvalue(SETLINES) == "ON" && tclvalue(SETSYMBOLS) == "ON") {
            Plot_Args$type <<- "b"  #both: line and symbols
            AutoKey_Args$type <<- "b"
            AutoKey_Args$lines <<- TRUE
            AutoKey_Args$points <<- TRUE
            Plot_Args$lty <<- rep(LType[1], 20)
            Plot_Args$pch <<- rep(STypeIndx[1], 20)
            if (tclvalue(BWCOL)=="Black/White") {
               PlotParameters$Colors <<- Colors[1]
               Plot_Args$lty <<- LType
               Plot_Args$par.settings$superpose.line$lty <<- LType
               Plot_Args$par.settings$superpose.line$col <<- rep(Colors[1], 20) #MonoChrome plot
               Plot_Args$par.settings$superpose.symbol$pch <<- rep(STypeIndx[1], 20)
               Plot_Args$par.settings$superpose.symbol$col <<- rep(Colors[1], 20)
               AutoKey_Args$col <<- rep(Colors[1], 20)
               if (tclvalue(LINETYPE) == "Solid") {
                   Plot_Args$lty <<- rep(LType[1], 20)
                   Plot_Args$par.settings$superpose.line$lty <<- rep(LType[1], 20)
               }
               if (tclvalue(SYMTYPE) == "Multi-Symbols") {
                   Plot_Args$pch <<- STypeIndx
                   Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
               }
            } else if (tclvalue(BWCOL)=="RainBow") {
               Plot_Args$lty <<- rep(LType[1], 20)
               Plot_Args$pch <<- rep(STypeIndx[1], 20)
               PlotParameters$Colors <<- Colors
               Plot_Args$par.settings$superpose.line$lty <<- rep(LType[1], 20)
               Plot_Args$par.settings$superpose.line$col <<- Colors #Rainbow plot
               Plot_Args$par.settings$superpose.symbol$pch <<- rep(STypeIndx[1], 20)
               Plot_Args$par.settings$superpose.symbol$col <<- Colors
               AutoKey_Args$col <<- Colors
               if (tclvalue(LINETYPE) == "Patterns") {
                   Plot_Args$lty <<- LType
                   Plot_Args$par.settings$superpose.line$lty <<- LType
               }
               if (tclvalue(SYMTYPE) == "Multi-Symbols") {
                   Plot_Args$pch <<- STypeIndx
                   Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
               }
            }
         }
         CtrlPlot()
   }

   setFileCheckBox <- function(){
         FNameList <- tclvalue(XS1)
         if (FNameList == "0"){       #if the last FName is de-selected
             LL <- length(SelectedNames$XPSSample)  #remove the last FName from the list of selected files
             SelectedNames$XPSSample <<- SelectedNames$XPSSample[-LL]
             LL <- length(SelectedNames$CoreLines)
             if (SelectedNames$CoreLines[LL] == "-----") {
                 SelectedNames$CoreLines <<- SelectedNames$CoreLines[-LL] #Remove the last Coreline from the list of selected Corelines
             }
             if (length(SelectedNames$XPSSample)==0 && length(SelectedNames$CoreLines)==0){
                 clear_treeview(NameTable)
             } else {
                 updateTable(NameTable, items=SelectedNames)   #update the table of the selected FNAmes
             }
             NCoreLines <<- 0
             NamesList <<- SelectedNames  #restore names prior the last selection
             SaveSelection <<- TRUE  #previous selection saved: TRUE to avoid error messages
             ClearWidget(T1frameCoreLines) # when selection complete delete checkboxes
         } else if (SaveSelection == FALSE && tclvalue(CL1)=="0"){  #XPSSample changed without selection of a CoreLine
             FNameList <- tclvalue(XS1)
             LL <- length(FNameList)
             FNameList <- FNameList[-LL]
             tkmessageBox(message="Select a Core.Line Please" , title = "WARNING: CORE.LINE NOT SELECTED",  icon = "error")
         } else {
             SaveSelection <<- FALSE #previous selection saved control enabled
             FName <- tclvalue(XS1)

             if (length(FName) > 0) {
                 SelectedNames$XPSSample <<- append(SelectedNames$XPSSample, FName) #Add the selected FName to the list of selected XPSSamples
                 SelectedNames$CoreLines <<- append(SelectedNames$CoreLines, "-----")  #add "----" to have same number of rows in the table widget
             } else {
                 SelectedNames <<- NamesList
             }
             updateTable(NameTable, items=SelectedNames)   #update the table widgest with the selected names

             LL <- length(SelectedNames$XPSSample)         #update the widget to select XPSSample for changing spectrum intensity
             SelectedNames$Ampli <<- rep(1, LL)
             tkconfigure(objFunctAmpliXS, values=SelectedNames$XPSSample)

             NCoreLines <<- 0
             ClearWidget(T1frameCoreLines) #eliminates the T1frameFName to update corelines
             CoreLineList <- XPSSpectList(tclvalue(XS1))
             CL1 <<- tclVar(FALSE)   #starts with cleared buttons
             LL <- length(CoreLineList)
             NCol <- ceiling(LL/7) #ii runs on the number of columns
             for(ii in 1:NCol) {   #7 elements per column
                 NN <- (ii-1)*7    #jj runs on the number of column_rows
                 for (jj in 1:7) {
                      if((jj+NN) > LL) {break}
                          T1CoreLineCK <<- tkcheckbutton(T1frameCoreLines, text=CoreLineList[(jj+NN)], variable=CL1, onvalue = (jj+NN), offvalue = 0,
                               command=function(){
                                  NCoreLines <<- NCoreLines+1
                                  FName <- tclvalue(XS1)
                                  if (length(FName)==0) { #Error: a coreline is selected before the XPSSample is selected
                                      tkmessageBox(message="Please select an XPS-Sample" , title = "NO XPS-SAMPLE!",  icon = "warning")
                                      tclvalue(CL1) <- FALSE
                                      return()
                                  }
                                  NamesList <<- SelectedNames   #a temporary variable used to allow changes in the checkbox
                                  SpectList <- as.integer(tclvalue(CL1))
                                  SpectList <- CoreLineList[SpectList]
                                  if (SpectList == "0"){
                                      SpectList <- NULL
                                  }
                                  LL <- length(SpectList)
                                  LLL <- length(NamesList$CoreLines)
                                  if (LL == 0) SpectList <- "-----"  #all the checkboxes are unselected (nocoreline selected)
                                  if (NamesList$CoreLines[LLL] == "-----" && LL==1) { #XPSSample selected and the first coreline was selected
                                      NamesList$CoreLines[LLL] <<- SpectList #change ----- with Coreline-name
                                  } else {
                                      NamesList$CoreLines <<- paste(NamesList$CoreLines[1:LLL-1], SpectList, sep="") #select corelines >1 then add new coreline to the list
                                  }
                                  if (LL > 0) NamesList$XPSSample <<- paste(NamesList$XPSSample, rep(FName, LL-1), sep="")  #For each of the selected coreline add the parent FName to the list of selected XPSSamples
                                  updateTable(NameTable, items=NamesList)   #update the table widgest with the selected names
                               })
                      tclvalue(CL1) <- FALSE
                      tkgrid(T1CoreLineCK, row = jj, column = ii, padx = 5, pady = 3, sticky="w")
                 }
             }

             SaveSelectionBtn <- tkbutton(T1frameCoreLines, text=" SAVE SELECTION ", command=function(){
                           FName <- tclvalue(XS1)
                           SpectList <- tclvalue(CL1)
                           if (length(FName)==0 || length(SpectList)==0){
                               tkmessageBox(message="No XPS-Sample or CoreLine selected!" , title = "WARNING:",  icon = "error")
                               return()
                           }
                           SaveSelection <<- TRUE
                           tclvalue(XS1) <- FALSE
                           tclvalue(CL1) <- FALSE
                           SelectedNames <<- NamesList
                           activeFName <- SelectedNames[[1]][1]                               #if SelectedNames$CoreLines[1]= "5.KVV" it gives SpectList[1] = "5" spectList[2]="KVV"  but
                           SpectList <- unlist(strsplit(SelectedNames$CoreLines[1], "\\."))   #if SelectedNames$CoreLines[1]="5.D.1.KVV" first derivative of KVV how to get the coreline name?
                           assign("activeSpectIndx", as.numeric(SpectList[1]), envir=.GlobalEnv)  #SpectList[1] = 5 which is saved as activeSpectIndx
                           xxx <- paste(SpectList[1], ".", sep="")                            #this gives "5."
                           SpectList <- unlist(strsplit(SelectedNames$CoreLines[1], xxx, fixed=TRUE))     #this gives SpectList[1] = "" spectList[2]="D.1.KVV"  the correct coreline name
                           assign("activeFName", SelectedNames[[1]][1], envir=.GlobalEnv)
                           assign("activeSpectName", SpectList[2], envir=.GlobalEnv)

                    })
            tkgrid(SaveSelectionBtn, row = 8, column = 1, padx = 5, pady = 5, sticky="w")


         }
   }

   SetBWCol <- function(){
            CLPalette <<- data.frame(Colors=rep(Colors[1], 20), stringsAsFactors=FALSE)
            FitColors <<- data.frame(BaseColor=XPSSettings$BaseColor, CompColor=rep(XPSSettings$ComponentsColor[1],20),
                                                      FitColor=XPSSettings$FitColor, stringsAsFactors=FALSE)
            CLPalette$Colors <<- "black"
            PlotParameters$Colors <<- "black"
            Plot_Args$par.settings$superpose.symbol$col <<- "black"
            Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
            Plot_Args$par.settings$superpose.line$col <<- "black"
            Plot_Args$par.settings$superpose.line$lty <<- LType

            AutoKey_Args$col <<- "black"
#            Plot_Args$par.settings$superpose.symbol$col <<- "gray45"
#            Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
#            Plot_Args$par.settings$superpose.line$col <<- "gray45"
#            Plot_Args$par.settings$superpose.line$lty <<- LType
#            AutoKey_Args$col <<- "gray45"

            #Fit Color
            FTColor <- ttklabel(T3F_Colors, text=as.character(1), width=6, font="Serif 8", background=FitColors$FitColor[1])
            tkgrid(FTColor, row = 1, column = 4, padx = c(12,0), pady = 1, sticky="w")
            tkconfigure(FTColor, background="black")
            FitColors$FitColor <<- "black"
            PlotParameters$FitCol <<- "black"
            Plot_Args$lty <<- LType
            Plot_Args$pch <<- STypeIndx
            tclvalue(LEGTXTCOLOR) <- "MonoChrome"
            if (tclvalue(LINETYPE) == "OFF") tclvalue(SYMTYPE) <- "Multi-Symbols"
            if (tclvalue(SYMTYPE) == "OFF") tclvalue(LINETYPE) <- "Patterns"
   }

   SetRainbowCol <- function(){
#            CLPalette <<- data.frame(Colors=Colors, stringsAsFactors=FALSE)
            PlotParameters$Colors <<- Colors
            PlotParameters$FitCol$BaseColor <<- FitColors$BaseColor
            PlotParameters$FitCol$FitColor <<- FitColors$FitColor

            T3F_Colors <- ttklabelframe(T3F_Palette, text="C.Lines  Baseline  FitComp   Fit", borderwidth=2, padding=c(5,5,5,5))
            tkgrid(T3F_Colors, row = 1, column = 1, padx = 5, pady = 0, sticky="w")

            PlotParameters$Colors <<- Colors
            PlotParameters$FitCol$BaseColor <<- 
            Plot_Args$par.settings$strip.background$col <<- "lightskyblue1"
            AutoKey_Args$col <<- Colors
            tclvalue(LINETYPE) <- "solid"
            tclvalue(SYMTYPE) <- "Single-Symbol"
            tclvalue(LEGTXTCOLOR) <- "PolyChrome"

            if (tclvalue(SETLINES) == "ON") {
                LineType <- LType[1]
                if (tclvalue(LINETYPE) == "Patterns") LineType <- LType
                Plot_Args$lty <<- LineType
                Plot_Args$par.settings$superpose.line$lty <<- LineType
                Plot_Args$par.settings$superpose.line$col <<- Colors
            }
            if (tclvalue(SETSYMBOLS) == "ON"){
                Symtype <- STypeIndx[1]
                if ( tclvalue(SYMTYPE) == "multi-symbol") Symtype <- STypeIndx
                Plot_Args$par.settings$superpose.symbol$col <<- Colors
                Plot_Args$pch <<- Symtype
                Plot_Args$par.settings$superpose.symbol$pch <<- Symtype
                Plot_Args$par.settings$superpose.symbol$fill <<- Colors
            }
            tclvalue(LEGTXTCOLOR) <- "RainBow"
   }


#----- reset parameters to the initial values -----
   ResetPlot <- function(){
            tclvalue(XS1) <- FALSE
            ClearWidget(T1frameCoreLines)
            tkgrid( ttklabel(T1frameCoreLines, text="                "),
                    row = 1, column = 1, padx = 5, pady = 5, sticky="w")
            tclvalue(PLOTTYPE) <- "Spectrum"
            tclvalue(PLOTMODE) <- "Single-Panel"
            tclvalue(LIMITRTF) <- FALSE
            tclvalue(NORMALIZE) <- FALSE
            tclvalue(ALIGN) <- FALSE
            tclvalue(REVERSE) <- TRUE
            tclvalue(BE_KE) <- FALSE
            tclvalue(EE) <- "Peak position = "; tkconfigure(objFunctNormPeak, foreground="grey")
            tclvalue(XS2) <- ""
            tclvalue(CL2) <- ""
            tclvalue(SCALEfACT) <- ""
            WidgetState(objFunctFact, "disabled")
            tclvalue(XOFFSET) <- "X_Off="; tkconfigure(XOffsetobj, foreground="grey")
            tclvalue(YOFFSET) <- "X_Off="; tkconfigure(YOffsetobj, foreground="grey")
            tclvalue(PSEUDO3D) <- FALSE
            tclvalue(TRED) <- FALSE
            tclvalue(TREDASPECT) <- "1/1"
            tclvalue(AZROT) <<- 35
            tclvalue(ZNROT) <<- 15
            tclvalue(X1) <- "Xmin="; tkconfigure(X1, foreground="grey")
            tclvalue(X2) <- "Xmax="; tkconfigure(X2, foreground="grey")
            tclvalue(Y1) <- "Ymin="; tkconfigure(Y1, foreground="grey")
            tclvalue(Y2) <- "Ymax="; tkconfigure(Y2, foreground="grey")
            tclvalue(BWCOL) <- "MonoChrome"
            tclvalue(GRID) <- "Grid OFF"
            tclvalue(SETLINES) <- "ON"
            tclvalue(SETSYMBOLS) <- "OFF"
            tclvalue(LINETYPE) <- "Patterns"
            tclvalue(LINEWIDTH) <- "1"
            tclvalue(SYMTYPE) <- "Single-Symbol"
            tclvalue(SYMSIZE) <- "0.8"
            tclvalue(FCSTYLE) <- "Dotted"
            tclvalue(STRIPCOLOR) <- "grey"
            tclvalue(TICKPOS) <- "LeftBottom"
            tclvalue(XSCALETYPE) <- "Regular"
            tclvalue(YSCALETYPE) <- "Regular"
            tclvalue(TITLESIZE) <- "1.4"
            tclvalue(MAINTITLE) <- "New Title"; tkconfigure(T4_MainTitChange, foreground="grey")
            tclvalue(AXNUMSIZE) <- "1"
            tclvalue(AXLABELSIZE) <- "1"
            tclvalue(AXLABELORIENT) <- "Horizontal"
            tclvalue(XAXLABEL) <- "New X label"; tkconfigure(T4_XAxNameChange, foreground="grey")
            tclvalue(YAXLABEL) <- "New Y label"; tkconfigure(T4_YAxNameChange, foreground="grey")
            tclvalue(ZAXLABEL) <- "New Z label"; tkconfigure(T4_ZAxNameChange, foreground="grey")
            tclvalue(XSTEP) <- "Xstep"; tkconfigure(T4_XStep, foreground="grey")
            tclvalue(YSTEP) <- "Ystep"; tkconfigure(T4_XStep, foreground="grey")
            tclvalue(X1RANGE) <- "Xmin="; tkconfigure(xx1, foreground="grey")
            tclvalue(X2RANGE) <- "Xmax="; tkconfigure(xx2, foreground="grey")
            tclvalue(Y1RANGE) <- "Ymin="; tkconfigure(yy1, foreground="grey")
            tclvalue(Y2RANGE) <- "Xmax="; tkconfigure(yy2, foreground="grey")
            tclvalue(LEGENDCK) <- FALSE
            tclvalue(LEGNAMECK) <- FALSE
            tclvalue(LEGENDPOS) <- "OutsideTop"
            tclvalue(LEGENCOLUMNS) <- "N.Col="; tkconfigure(T5_LegColNum, foreground="grey")
            tclvalue(LEGENDSIZE) <- "0.4"
            tclvalue(LEGENDDIST) <- "0.08"
            tclvalue(LEGLINWIDTH) <- "1"
            tclvalue(LEGTXTCOLOR) <<- "Reinbow"
            XPSSettings <<- get("XPSSettings", envir=.GlobalEnv)
            Colors <<- XPSSettings$Colors
            LType <<- XPSSettings$LType
            SType <<- XPSSettings$Symbols
            STypeIndx <<- XPSSettings$SymIndx
            if (XPSSettings$General[8] == "PolyChromeFC"){
                FitColors <- data.frame(BaseColor=XPSSettings$BaseColor, CompColor=XPSSettings$ComponentsColor,
                                  FitColor=XPSSettings$FitColor, stringsAsFactors=FALSE)
            } else if (XPSSettings$General[8] == "MonoChromeFC"){
                FitColors <- data.frame(BaseColor=XPSSettings$BaseColor, CompColor=rep(XPSSettings$ComponentsColor[1],20),
                                  FitColor=XPSSettings$FitColor, stringsAsFactors=FALSE)
            }
            CLPalette <<- data.frame(Colors=Colors, stringsAsFactors=FALSE)

            PlotParameters <<- DefaultPlotParameters

            Plot_Args <<- list( x=formula("y ~ x"), data=NULL, PanelTitles=list(), groups=NULL,layout=NULL,
                                  xlim=NULL,ylim=NULL,
                                  pch=STypeIndx,cex=1,lty="solid",lwd=1,type="l",
                                  background="transparent",  col=Colors,
                                  main=list(label=NULL,cex=1.5),
                                  xlab=list(label=NULL, rot=0, cex=1.2),
                                  ylab=list(label=NULL, rot=90, cex=1.2),
                                  zlab=NULL,
                                  scales=list(cex=1, tck=c(1,0), alternating=c(1), relation="same",
                                              x=list(log=FALSE), y=list(log=FALSE), axs="i"),
                                  xscale.components = xscale.components.subticks,
                                  yscale.components = yscale.components.subticks,
                                  las=0,
                                  par.settings = list(superpose.symbol=list(pch=STypeIndx, fill="black"), #set the symbol fill color
                                        superpose.line=list(lty=LType, col=Colors), #needed to set the legend colors
                                        par.strip.text=list(cex=1),
                                        strip.background=list(col="lightskyblue1") ),
                                  auto.key = FALSE,
                                  grid = FALSE
                             )

            AutoKey_Args <<- list(space="top",
                                  text=get("activeSpectName", envir=.GlobalEnv),
                                  cex = 1,
                                  type= "l",
                                  lines=TRUE,
                                  points=FALSE,
                                  col="black",
                                  columns=1,   #leggendsorganized in a column
                                  list(corner=NULL,x=NULL,y=NULL)
                             )
            Xlim <<- NULL #reset Xlim
            Ylim <<- NULL #reset Ylim
   }


#----- Variables -----
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FName <- get(activeFName, envir=.GlobalEnv)   #load the active FName
   SpectIndx <- 1
   activeSpectName <<- names(FName)[1]

   SpectList <- XPSSpectList(activeFName)   #list of the active XPSSample core lines
   NCoreLines <- NULL
   NComp <- length(FName[[SpectIndx]]@Components)
   FitComp1 <- ""
   for (ii in 1:NComp){
      FitComp1[ii] <- paste("C",ii, sep="")
   }
   FNameListTot <- as.array(XPSFNameList())     #List of all XPSSamples loaded in the workspace
   LL=length(FNameListTot)
   jj <- 1
   SelectedNames <- list(XPSSample=NULL, CoreLines=NULL, Ampli=NULL)
   NamesList <- list(XPSSample=NULL, CoreLines=NULL)
   SpectName <- ""

   T1CoreLineCK <- NULL

   plot.new()  #reset graphical window

   # list of graphical variables
   PatternList <- NULL
   FontSize <- c(0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)
   AxLabOrient <- c("Horizontal", "Rot-20", "Rot-45", "Rot-70", "Vertical", "Parallel", "Normal")
   XPSSettings <- get("XPSSettings", envir=.GlobalEnv)
   Colors <- XPSSettings$Colors
   LType <- XPSSettings$LType
#LType <- c("dashed", "Solid", "dotted", "dotdash")
   SType <- XPSSettings$Symbols
   STypeIndx <- XPSSettings$SymIndx
   CLPalette <- data.frame(Colors=Colors, stringsAsFactors=FALSE)
   if (XPSSettings$General[8] == "PolyChromeFC"){
       CLPalette <- data.frame(Colors=Colors, stringsAsFactors=FALSE)
       FitColors <- data.frame(BaseColor=XPSSettings$BaseColor, CompColor=XPSSettings$ComponentsColor,
                           FitColor=XPSSettings$FitColor, stringsAsFactors=FALSE)
   } else if (XPSSettings$General[8] == "MonoChromeFC"){
       CLPalette <- data.frame(Colors=rep(Colors[1], 20), stringsAsFactors=FALSE)
       FitColors <- data.frame(BaseColor=XPSSettings$BaseColor, CompColor=rep(XPSSettings$ComponentsColor[1], 20),
                           FitColor=XPSSettings$FitColor, stringsAsFactors=FALSE)
   }
#-------------------------------------------------------------------------------------------------
#   LType <- c("solid", "dashed", "dotted", "dotdash", "longdash",     #definisco 20 tipi divesi di line pattern
#            "twodash", "F8", "431313", "22848222", "12126262",
#            "12121262", "12626262", "52721272", "B454B222", "F313F313",
#            "71717313", "93213321", "66116611", "23111111", "222222A2" )
#
#   SType <- c("VoidCircle", "VoidSquare", "VoidTriangleUp", "VoidTriangleDwn",  "Diamond",
#            "X", "Star", "CrossSquare", "CrossCircle", "CrossDiamond",
#            "SolidSquare", "SolidCircle", "SolidTriangleUp", "SolidTriangleDwn", "SolidDiamond",
#            "DavidStar", "SquareCross", "SquareTriang", "CircleCross", "Cross")
#   STypeIndx <- c(1,  0,  2,  6,  5,
#                4,  8,  7,  10, 9,
#                15, 16, 17, 25, 18,
#                11, 12, 14, 13, 3)
#
#   Colors <- c("black", "red", "limegreen", "blue", "magenta", "orange", "cadetblue", "sienna",
#             "darkgrey", "forestgreen", "gold", "darkviolet", "greenyellow", "cyan", "lightcoral",
#             "turquoise", "deeppink3", "wheat", "thistle", "grey40")
#-------------------------------------------------------------------------------------------------
   LWidth <- c(1,1.25,1.5,1.75,2,2.25,2.5,3, 3.5,4)
   SymSize <- c(0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2) #lattice prende indici simboli piuttosto che nomesimbolo
   PanelTitles <- NULL
   LegPos <- c("OutsideTop","OutsideRight","OutsideLeft", "OutsideBottom",
             "InsideTopRight","InsideTopLeft","InsideBottomRight","InsideBottomLeft")
   LegOrient <- c("Vertical", "Horizontal")
   LegLineWdh <- c(1,1.5,2,2.5,3,3.5,4,4.5,5)
   LegTxtCol <- c("RainBow", "Black")
   LegTxtSize <- c(0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)
   LegDist <- c(0,0.01,0.02,0.04,0.08,0.1,0.12,0.14,0.16,0.18,0.2)
   ColorList <- NULL
   exit <- NULL
   Xlim <- NULL
   Ylim <- NULL

   T1FNameListCK <- NULL
   T1frameCoreLines <- list()
   CLcolor <- list()  #to store widget ID
   FCcolor <- list()  #to store widget ID
   FTcolor <- list()  #to store widget ID

#--- general options
   PlotParameters <- list()
   PlotParameters$Align <- FALSE
   PlotParameters$RTFLtd <- FALSE #restrict plot to RTF region
   PlotParameters$Normalize <- FALSE
   PlotParameters$NormPeak <- 0
   PlotParameters$Reverse <- TRUE #reversed X axes for Bind. Energy
   PlotParameters$SwitchE <- FALSE
   PlotParameters$XOffset <- 0
   PlotParameters$YOffset <- 0
   PlotParameters$OverlayType <- "Spectrum"
   PlotParameters$OverlayMode <- "Single-Panel"
   PlotParameters$Colors <- Colors
   PlotParameters$CompLty <- "solid"
   PlotParameters$FitCol <- FitColors
#--- legend options
   PlotParameters$Labels <- NULL
   PlotParameters$Legenda <- FALSE
   PlotParameters$LegPos <- "topright"
   PlotParameters$LegLineWdh <- 1
   PlotParameters$LegTxtCol <- "RainBow"
   PlotParameters$LegTxtSize <- 1
   PlotParameters$LegDist <- 0
#--- 3D OPTIONS
   PlotParameters$Pseudo3D <- FALSE
   PlotParameters$TreD <- FALSE
   PlotParameters$TreDAspect <- c(1,1)
   PlotParameters$AzymuthRot <- 35
   PlotParameters$ZenithRot <- 15
   PlotParameters$AxOffset <- NULL

   PlotParameters$Annotate <- FALSE
   DefaultPlotParameters <- PlotParameters

#--- commands for lattice -----
   Plot_Args <- list( x=formula("y ~ x"), data=NULL, PanelTitles=list(), groups=NULL,layout=NULL,
                    xlim=NULL, ylim=NULL,
                    pch=STypeIndx,cex=1,lty="solid",lwd=1,type="l",
                    background = "transparent", col=Colors,
                    main=list(label=NULL,cex=1.5),
                    xlab=list(label=NULL, rot=0, cex=1.2),
                    ylab=list(label=NULL, rot=90, cex=1.2),
                    zlab=NULL,
                    scales=list(cex=1, tck=c(1,0), alternating=c(1), tick.number=5, relation="same",
                                x=list(log=FALSE), y=list(log=FALSE), axs="i"),
                    xscale.components = xscale.components.subticks,
                    yscale.components = yscale.components.subticks,
                    las=0,
                    par.settings = list(superpose.symbol=list(pch=STypeIndx,fill=Colors), #set symbol fill colore
                                        superpose.line=list(lty=LType, col=Colors), #needed to set legend colors
                                        par.strip.text=list(cex=1),
                                        strip.background=list(col="lightskyblue1") ),
                    auto.key = FALSE,
                    grid = FALSE
                  )

   AutoKey_Args <- list( auto.key = FALSE,
                         space="top",
                         text= "",
                         cex = 1,
                         type= "l",
                         pch = 1,
                         lines=TRUE,
                         points=FALSE,
                         col="black",
                         columns=1,  #legends organized in a column
                         list(corner=NULL,x=NULL,y=NULL)
                       )

   SaveSelection <- TRUE #at beginning force the control of the selection to TRUE to avoid error messages

#----- Reset Grafic window -----

   plot.new()
   assign("MatPlotMode", FALSE, envir=.GlobalEnv)  #basic matplot function used to plot data

#----- NoteBook -----

   OverlayWindow <- tktoplevel()
   tkwm.title(OverlayWindow,"OVERLAY XPS SPECTRA")
   tkwm.geometry(OverlayWindow, "+100+50")   #position respect topleft screen corner

   MainGroup <- ttkframe(OverlayWindow, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(MainGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   Tab <- tclVar(1)
   NB <- ttknotebook(MainGroup)
   tkgrid(NB, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   RSTOptBtn <- tkbutton(MainGroup, text=" RESET OPTIONS ", width=18, command=function(){
                           LL=length(SelectedNames$XPSSample)
                           SelectedNames$Ampli <<- rep(1, LL)
                           ResetPlot()
                           CtrlPlot()
                  })
   tkgrid(RSTOptBtn, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

   ExitBtn <- tkbutton(MainGroup, text=" EXIT ", width=15, command=function(){
                           ResetPlot()
                           CtrlPlot()
		                         tkdestroy(OverlayWindow)
                  })
   tkgrid(ExitBtn, row = 2, column = 1, padx = 150, pady = 5, sticky="w")

# --- TAB1 ---
#XPS Sample selecion & representation options

     T1group1 <- ttkframe(NB,  borderwidth=0, padding=c(0,0,0,0) )
     tkadd(NB, T1group1, text=" XPS SAMPLE SELECTION ")

     T1groupSelection <- ttkframe(T1group1, borderwidth=0, padding=c(0,0,0,0))
     tkgrid(T1groupSelection, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T1frameOvType <- ttklabelframe(T1groupSelection, text = "SELECT PLOT OPTIONS", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T1frameOvType, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     Items1 <- c("Spectrum", "Spectrum+Baseline", "Spectrum+Fit")
     PLOTTYPE <- tclVar("Spectrum")   #starts with cleared buttons
     for(ii in 1:3){
         T1OvTypeCK1 <- ttkradiobutton(T1frameOvType, text=Items1[ii], variable=PLOTTYPE, value=Items1[ii],
                           command=function(){
                           if (tclvalue(TRED) == 1 && tclvalue(PLOTTYPE) != "Spectrum" ){  #3D plot active
                              tkmessageBox(message="3d plot active: only SPECTRUM mode allowed", title = "Warning 3D active", icon = "warning")
                           } else {
                              if(tclvalue(BWCOL) == "Black/White") {
                                 CLPalette$Colors <<- rep("black", 20)
                                 Plot_Args$par.settings$superpose.symbol$col <<- "black"
                                 Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                 Plot_Args$par.settings$superpose.line$col <<- "black"
                                 Plot_Args$par.settings$superpose.line$lty <<- LType
                                 PlotParameters$Colors <<- rep("black", 20)
                                 FitColors$CompColor <<- rep("gray45", 20)
                                 Plot_Args$par.settings$superpose.symbol$col <<- "gray45"
                                 Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                 Plot_Args$par.settings$superpose.line$col <<- "gray45"
                                 Plot_Args$par.settings$superpose.line$lty <<- LType
                                 AutoKey_Args$col <<- "gray45"
                                 PlotParameters$FitCol <<- FitColors
                                 Plot_Args$lty <<- LType
                                 Plot_Args$pch <<- STypeIndx
                                 if (tclvalue(LINETYPE) == "OFF") tclvalue(SYMTYPE) <- "Multi-Symbols"
                                 if (tclvalue(SYMTYPE) == "OFF") tclvalue(LINETYPE) <- "Patterns"
                                 Plot_Args$par.settings$superpose.symbol$col <<- rep("black", 20)
                                 Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                 Plot_Args$par.settings$superpose.line$col <<- rep("black", 20)
                                 Plot_Args$par.settings$superpose.line$lty <<- LType
                                 Plot_Args$par.settings$strip.background$col <<- "grey90"
                                 AutoKey_Args$col <<- "black"
                              } else if (tclvalue(BWCOL) == "RainBow"){
                                 CLPalette$Colors <<- Colors
                                 Plot_Args$par.settings$superpose.symbol$col <<- Colors
                                 Plot_Args$par.settings$superpose.symbol$pch <<- rep(STypeIndx[[1]], 20)
                                 Plot_Args$par.settings$superpose.line$col <<- Colors
                                 Plot_Args$par.settings$superpose.line$lty <<- rep("solid", 20)
                                 PlotParameters$Colors <<- Colors
                                 if (XPSSettings$General[8] == "MonoChromeFC"){
                                     FitColors$CompColor <<- rep("gray45", 20)
                                     Plot_Args$par.settings$superpose.symbol$col <<- "gray45"
                                     Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                     Plot_Args$par.settings$superpose.line$col <<- "gray45"
                                     Plot_Args$par.settings$superpose.line$lty <<- LType
                                     AutoKey_Args$col <<- "gray45"
                                     PlotParameters$FitCol <<- FitColors
                                     Plot_Args$lty <<- LType
                                     Plot_Args$pch <<- STypeIndx
                                     if (tclvalue(LINETYPE) == "OFF") tclvalue(SYMTYPE) <- "Multi-Symbols"
                                     if (tclvalue(SYMTYPE) == "OFF") tclvalue(LINETYPE) <- "Patterns"
                                     Plot_Args$par.settings$superpose.symbol$col <<- rep("black", 20)
                                     Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                     Plot_Args$par.settings$superpose.line$col <<- rep("black", 20)
                                     Plot_Args$par.settings$superpose.line$lty <<- LType
                                     Plot_Args$par.settings$strip.background$col <<- "grey90"
                                     AutoKey_Args$col <<- rep("black", 20)
                                 } else if (XPSSettings$General[8] == "PolyChromeFC"){
                                     FitColors$CompColor <<- rep("gray45", 20)
                                     Plot_Args$par.settings$superpose.symbol$col <<- Colors
                                     Plot_Args$par.settings$superpose.symbol$pch <<- rep(1, 20)
                                     Plot_Args$par.settings$superpose.line$col <<- Colors
                                     Plot_Args$par.settings$superpose.line$lty <<- rep("solid", 20)
                                     AutoKey_Args$col <<- Colors
                                     PlotParameters$FitCol <<- FitColors
                                     Plot_Args$lty <<- rep("solid", 20)
                                     Plot_Args$pch <<- rep(1, 20)
                                     if (tclvalue(LINETYPE) == "OFF") tclvalue(SYMTYPE) <- "Multi-Symbols"
                                     if (tclvalue(SYMTYPE) == "OFF") tclvalue(LINETYPE) <- "Patterns"
                                     Plot_Args$par.settings$superpose.symbol$col <<- Colors
                                     Plot_Args$par.settings$superpose.symbol$pch <<- rep(1, 20)
                                     Plot_Args$par.settings$superpose.line$col <<- Colors
                                     Plot_Args$par.settings$superpose.line$lty <<- rep("solid", 20)
                                     Plot_Args$par.settings$strip.background$col <<- "grey90"
                                     AutoKey_Args$col <<- Colors
                                 }
                                 FitColors$FitColor <<- XPSSettings$FitColor
                                 PlotParameters$Colors <<- Colors
                                 lty <- tclvalue(LINETYPE)
                                 if (tclvalue(SETLINES) == "ON") {
                                     if (lty == "Solid") {
                                         Plot_Args$par.settings$superpose.line$lty <<- rep("solid", 20)
                                     } else if (lty == "Patterns") {
                                         Plot_Args$par.settings$superpose.line$lty <<- LType
                                     }
                                     Plot_Args$par.settings$superpose.line$col <<- Colors
                                 }
                                 if (tclvalue(SETSYMBOLS) == "ON"){
                                     symty <- tclvalue(SYMTYPE)
                                     if (symty == "Single-Symbol") {
                                         Plot_Args$par.settings$superpose.symbol$pch <<- rep(1, 20)
                                     } else if (symty == "multi-symbol") {
                                         Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                     }
                                     Plot_Args$par.settings$superpose.symbol$col <<- Colors
                                     Plot_Args$par.settings$superpose.symbol$fill <<- Colors
                                 }
                                 PlotParameters$Colors <<- Colors
                                 Plot_Args$lty <<- rep("solid", 20)
                                 Plot_Args$pch <<- rep(STypeIndx[1], 20)
                                 Plot_Args$par.settings$strip.background$col <<- "lightskyblue1"
                                 AutoKey_Args$col <<- Colors
                                 tclvalue(LINETYPE) <- "Solid"
                                 tclvalue(SYMTYPE) <- "Single-Symbol"
                                 tclvalue(LEGTXTCOLOR) <- "PolyChrome"
                              }
                              PlotParameters$OverlayType <<- tclvalue(PLOTTYPE)
                              CtrlPlot()
                           }
                   })
         tkgrid(T1OvTypeCK1, row = ii, column = 1, padx = 5, pady = 5, sticky="w")
     }

     Items2 <- c("Single-Panel", "Multi-Panel")
     PLOTMODE <- tclVar("Single-Panel")   #starts with cleared buttons
     for(ii in 1:2){
         T1OvTypeCK2 <- ttkradiobutton(T1frameOvType, text=Items2[ii], variable=PLOTMODE, value=Items2[ii],
                          command=function(){
                            LL <- length(SelectedNames$XPSSample)
                            if (LL == 0) {
                                tkmessageBox(message="Spectra to Plot are Missing. Select XPS Samples and Core-Lines Please.", title = "WARNING", icon = "warning")
                                tclvalue(PLOTMODE) <- "Single-Panel"
                                return()
                            }
                            PlotParameters$OverlayMode <<- tclvalue(PLOTMODE)
                            if (tclvalue(PLOTMODE) == "Single-Panel") {
                               WidgetState(T4_PanelTitles, "disabled")
                               Plot_Args$scales$relation <<- "same"
                            }
                            if (tclvalue(PLOTMODE) == "Multi-Panel") {
                               WidgetState(T4_PanelTitles, "normal")
                               Plot_Args$scales$relation <<- "free"
                               PanelTitles <<- NULL
                               for (ii in 1:LL){
                                   SpectName <- unlist(strsplit(SelectedNames$CoreLines[ii], "\\."))   #skip the number of the Coreline-name
                                   SpectName <- SpectName[2]
                                   PanelTitles <<- c(PanelTitles, paste(SpectName, SelectedNames$XPSSample[ii], sep=" ")) #List of Titles for the Multipanel
                               }
                               Plot_Args$PanelTitles <<- PanelTitles
                            }
                            CtrlPlot()
                   })
         tkgrid(T1OvTypeCK2, row = ii, column = 2, padx = 5, pady = 5, sticky="w")
     }

     LIMITRTF <- tclVar()
     T1LimitRTF <- tkcheckbutton(T1frameOvType, text="Limit Plot To Fit Region", variable=LIMITRTF, onvalue = 1, offvalue = 0,
                           command=function(){
                            PlotParameters$RTFLtd <<- as.logical(as.numeric(tclvalue(LIMITRTF)))
                            CtrlPlot() 
                   })
     tclvalue(LIMITRTF) <- FALSE
     tkgrid(T1LimitRTF, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
     T1frameFName <- ttklabelframe(T1groupSelection, text = "SELECT XPS-SAMPLE", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T1frameFName, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
     XS1 <- tclVar(FALSE)   #starts with cleared buttons
     LL <- length(FNameListTot)
     NCol <- ceiling(LL/7) #ii runs on the number of columns
     for(ii in 1:NCol) {   #7 elements per column
         NN <- (ii-1)*7    #jj runs on the number of column_rows
         for (jj in 1:7) {
              if((jj+NN) > LL) {break}
              T1FNameListCK <- tkcheckbutton(T1frameFName, text=FNameListTot[(jj+NN)], variable=XS1, onvalue = FNameListTot[(jj+NN)], offvalue = 0,
                                           command=function(){
                                               setFileCheckBox()
                   })
              tclvalue(XS1) <- FALSE
              tkgrid(T1FNameListCK, row = jj, column = ii, padx = 5, pady = 3, sticky="w")
         }
     }

     T1frameCoreLines <- ttklabelframe(T1groupSelection, text = "SELECT CORE.LINE", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T1frameCoreLines, row = 1, column = 3, padx = 5, pady = 5, sticky="w")
     CL1 <- tclVar(FALSE)   #starts with cleared buttons
     tkgrid( ttklabel(T1frameCoreLines, text="                "),
             row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T1groupButtons <- ttkframe(T1group1, borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T1groupButtons, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

     PlotBtn <- tkbutton(T1groupButtons, text="   PLOT   ", command=function(){
                           CtrlPlot()
                    })
     tkgrid(PlotBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     ClrBtn <- tkbutton(T1groupButtons, text=" CLEAR LAST XPS-SAMPLE ", command=function(){
                           LL <- length(NamesList$XPSSample) #NamesList$XPSSample and NamesList$CoreLines have the same length
                           if (SaveSelection == FALSE){
                              tkmessageBox(message="Please Save Selection" , title = "WARNING: SELECTION NOT SAVED",  icon = "error")
                              return()
                           } else if (NCoreLines == LL) {
                              NamesList <<- list(XPSSample="   ", CoreLines="   ")  #dummy lists to begin: NB each lcolumn contains 2 element otherwise error
                           } else {
                              NamesList <<- list(XPSSample=NamesList[[1]][1:(LL-NCoreLines)], CoreLines=NamesList[[2]][1:(LL-NCoreLines)])
                           }
                           SpectList <- tclvalue(CL1)
                           LL <- length(SpectList)
                           SpectList <- SpectList[-LL]
                           updateTable(NameTable, items=NamesList)   #update the table with the name of the selected FNames
                           SelectedNames <<- NamesList
                           LL=length(SelectedNames$XPSSample)
                           SelectedNames$Ampli <<- rep(1, LL)
                           tkconfigure(objFunctAmpliXS, values=SelectedNames$XPSSample)
                           tclvalue(XS2) <- FALSE
                           tkconfigure(objFunctAmpliCL, values="     ")
                           tclvalue(CL2) <- FALSE
                    })
     tkgrid(ClrBtn, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

     CLRLstBtn <- tkbutton(T1groupButtons, text=" RESET LIST ", command=function(){
                           tclvalue(XS1) <- FALSE
                           tclvalue(CL1) <- FALSE   #starts with cleared buttons
                           ClearWidget(T1frameCoreLines)
                           tkgrid( ttklabel(T1frameCoreLines, text="                "),
                                   row = 1, column = 1, padx = 5, pady = 5, sticky="w")
                           NamesList <<- list(XPSSample=NULL, CoreLines=NULL)
                           SelectedNames <<- list(XPSSample=c(" ", " "),CoreLines=c(" "," "))  #dummy lists to begin: NB each lcolumn contains 2 element otherwise error
                           updateTable(NameTable, items=SelectedNames)   #update the table with the name of the selected FNames
                           SelectedNames <<- list(XPSSample=NULL,CoreLines=NULL,Ampli=NULL )   #dummy lists to begin: NB each lcolumn contains 2 element otherwise error
                           tkconfigure(objFunctAmpliXS, values="       ")
                           tclvalue(XS2) <- FALSE
                           tkconfigure(objFunctAmpliCL, values="       ")
                           tclvalue(CL2) <- FALSE
                           tclvalue(PLOTMODE) <- "Single-Panel"
                           tclvalue(PLOTTYPE) <- "Spectrum"
                           tclvalue(LEGENDCK) <- FALSE
                           tclvalue(LEGNAMECK) <- FALSE
                           Plot_Args$auto.key <<- FALSE
                           SaveSelection <<- TRUE
                           plot.new()
                    })
     tkgrid(CLRLstBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

     RSTBtn <- tkbutton(T1groupButtons, text=" RESET PLOT ", command=function(){
                           tclvalue(XS1) <- FALSE
                           tclvalue(CL1) <- FALSE   #starts with cleared buttons
                           ClearWidget(T1frameCoreLines)
                           tkgrid( ttklabel(T1frameCoreLines, text="                "),
                                   row = 1, column = 1, padx = 5, pady = 5, sticky="w")
                           SelectedNames <<- list(XPSSample=c(" ", " "),CoreLines=c(" "," "))  #dummy lists to begin: NB each lcolumn contains 2 element otherwise error
                           updateTable(NameTable, items=SelectedNames)   #update the table with the name of the selected FNames
                           SelectedNames <<- list(XPSSample=NULL,CoreLines=NULL,Ampli=NULL )   #dummy lists to begin: NB each lcolumn contains 2 element otherwise error
                           tkconfigure(objFunctAmpliXS, values="       ")
                           tclvalue(XS2) <- FALSE
                           tkconfigure(objFunctAmpliCL, values="       ")
                           tclvalue(CL2) <- FALSE
#                           tclvalue(PLOTMODE) <- "Single-Panel"
#                           tclvalue(PLOTTYPE) <- "Spectrum"
                           SaveSelection <<- TRUE
                           ResetPlot()
                           plot.new()
                    })
     tkgrid(RSTBtn, row = 1, column = 4, padx = 5, pady = 5, sticky="w")

     UPDateBtn <- tkbutton(T1groupButtons, text=" UPDATE XPS-SAMPLE LIST ", command=function(){
                           FName <<- get(activeFName, envir=.GlobalEnv)
                           SpectIndx <<- get("activeSpectIndx", envir=.GlobalEnv)
                           SpectList <<- XPSSpectList(activeFName)   #sCoreLine list of the XPSSample
                           NComp <<- length(FName[[SpectIndx]]@Components)
                           NCoreLines <<- NULL
                           FitComp1 <<- ""  #build vector containing names of the fit components on the Active Spectrum
                           for (ii in 1:NComp){
                               FitComp1[ii] <- paste("C",ii, sep="")
                           }
                           # LISTE DEI NOMI ALTRI SPETTRI
                           FNameListTot <- as.array(XPSFNameList())     #list of all XPSSample in Envir=.GlobalEnv
                           LL=length(FNameListTot)
                           jj <- 1
                           SelectedNames <<- list(XPSSample=NULL, CoreLines=NULL, Ampli=NULL)
                           NamesList <<- list(XPSSample=NULL, CoreLines=NULL)
                           SaveSelection <<- TRUE
                           ClearWidget(T1frameFName)
                           LL <- length(FNameListTot)
                           NCol <- ceiling(LL/7) #ii runs on the number of columns
                           for(ii in 1:NCol) {   #7 elements per column
                               NN <- (ii-1)*7    #jj runs on the number of column_rows
                               for (jj in 1:7) {
                                   if((jj+NN) > LL) {break}
                                      T1FNameListCK <- tkcheckbutton(T1frameFName, text=FNameListTot[(jj+NN)], variable=XS1, onvalue = FNameListTot[(jj+NN)], offvalue = 0,
                                                         command=function(){
                                                           setFileCheckBox()
                                             })
                                      tclvalue(XS1) <- FALSE
                                      tkgrid(T1FNameListCK, row = jj, column = ii, padx = 5, pady = 5, sticky="w")
                               }
                           }
                           ClearWidget(T1frameCoreLines)
                           tkgrid( ttklabel(T1frameCoreLines, text="                "),
                                   row = 1, column = 1, padx = 5, pady = 5, sticky="w")
                           tkconfigure(objFunctAmpliXS, values="       ")
                           tclvalue(XS2) <- FALSE
                           tkconfigure(objFunctAmpliCL, values="       ")
                           tclvalue(CL2) <- FALSE
                           updateTable(NameTable, items=dummy)
                           ResetPlot()
                           plot.new()
                    })
     tkgrid(UPDateBtn, row = 1, column = 5, padx = 5, pady = 5, sticky="w")

     T1frameTable <- ttklabelframe(T1group1, text = " FUNCTIONS ", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T1frameTable, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
     dummy <<- list(XPSSample=c("   ", "  "),CoreLines=c("   ", "  "))   #dummy list to begin: NB each column has 2 initial element otherwise error...
     dummy$XPSSample <- encodeString(dummy$XPSSample, width=20, justify="right")
     dummy$CoreLines <- encodeString(dummy$CoreLines, width=20, justify="right")
     NameTable <- XPSTable(parent=T1frameTable, items=dummy, NRows=5, ColNames=c("XPSSample","CoreLines"), Width=300)
     tkgrid.columnconfigure(T1frameTable, 1, weight=4)
     addScrollbars(parent=T1frameTable, widget=NameTable, type = "y", Row=1, Col=1, Px=0, Py=0)

# --- TAB2 ---

     T2group1 <- ttkframe(NB,  borderwidth=0, padding=c(0,0,0,0) )
     tkadd(NB, T2group1, text=" FUNCTIONS ")

     T2frame2 <- ttklabelframe(T2group1, text = " FUNCTIONS ", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T2frame2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

#---Funct1: NORMALIZE
     NORMALIZE <- tclVar(FALSE)
     objFunctNorm <- tkcheckbutton(T2frame2, text="Normalize", variable=NORMALIZE, onvalue = 1, offvalue = 0,
                        command=function(){
                           PlotParameters$Normalize <<- as.logical(as.numeric(tclvalue(NORMALIZE)))
                           FName <- get(SelectedNames$XPSSample[1], envir=.GlobalEnv) #retrieve a generic XPSSample from the selected ones
                           SpectName <- unlist(strsplit(SelectedNames$CoreLines[1], "\\."))  #retrieve a generic coreline from the list of selected ones
                           indx <- as.numeric(SpectName[1])
                           Plot_Args$ylab$label <<- FName[[indx]]@units[2]   #retrieve the Y axis label
                           if (tclvalue(NORMALIZE) == "1") {   #Normalize option TRUE
                               Plot_Args$ylab$label <<- "Intensity [a.u.]"
                           }
                           CtrlPlot()
                    })
     tkgrid(objFunctNorm, row = 1, column = 1, padx = 5, pady=5, sticky="w")

#---Funct2: Y-Align
     ALIGN <- tclVar(FALSE)
     objFunctAlign <- tkcheckbutton(T2frame2, text="Align bkg to 0", variable=ALIGN, onvalue = 1, offvalue = 0,
                           command=function(){
                           PlotParameters$Align <<- as.logical(as.numeric(tclvalue(ALIGN)))
                           CtrlPlot()
                    })
     tkgrid(objFunctAlign, row = 1, column = 2, padx = 5, pady=5, sticky="w")

#---Funct3: Switch Binding to Kinetic Energy scale
     BE_KE <- tclVar(FALSE)
     objFunctSwitch <- tkcheckbutton(T2frame2, text="Switch BE/KE Scale", variable=BE_KE, onvalue = 1, offvalue = 0,
                           command=function(){
                           PlotParameters$SwitchE <<- as.logical(as.numeric(tclvalue(BE_KE)))
                           CtrlPlot()
                    })
     tkgrid(objFunctSwitch, row = 1, column = 3, padx = 5, pady=5, sticky="w")


#---Funct4: Reverse X axis
     REVERSE <- tclVar(TRUE)
     objFunctRev <- tkcheckbutton(T2frame2, text="Rev. Xaxis", variable=REVERSE, onvalue = 1, offvalue = 0,
                           command=function(){
                           PlotParameters$Reverse <<- as.logical(as.numeric(tclvalue(REVERSE)))
                           CtrlPlot()
                    })
     tkgrid(objFunctRev, row = 1, column = 4, padx = 5, pady=5, sticky="w")

#---Funct5: Normalize to a selected peak
     tkgrid( ttklabel(T2frame2, text="Normalize to Peak:"),
             row = 2, column = 1, padx = 5, pady = 5, sticky="w")
     EE <- tclVar("Peak position = ")  #sets the initial msg
     objFunctNormPeak <- ttkentry(T2frame2, textvariable=EE, width=15, foreground="grey")
     #receiving focus ttkentry clears the initialmsg and set black color
     tkbind(objFunctNormPeak, "<FocusIn>", function(K){
                           tclvalue(EE) <- ""
                           tkconfigure(objFunctNormPeak, foreground="red")
                    })
     #now ttkentry waits for a return to read the entry_value
     tkbind(objFunctNormPeak, "<Key-Return>", function(K){
                           PlotParameters$Normalize <<- TRUE
                           tkconfigure(objFunctNormPeak, foreground="black")
                           PlotParameters$NormPeak <<- as.numeric(tclvalue(EE))
                           if ( is.na(PlotParameters$NormPeak) ) PlotParameters$NormPeak <<- 0
                           FName <- get(SelectedNames$XPSSample[1], envir=.GlobalEnv) #retrieve a generic XPSSample from the selected ones
                           SpectName <- unlist(strsplit(SelectedNames$CoreLines[1], "\\."))  #retrieve a generic coreline from the list of selected ones
                           indx <- as.numeric(SpectName[1])
                           Plot_Args$ylab$label <<- FName[[indx]]@units[2]   #retrieve the Y axis label
                           Plot_Args$ylab$label <<- "Intensity [a.u.]"
                           CtrlPlot()
                    })
     tkgrid(objFunctNormPeak, row = 2, column = 2, padx = 5, pady = 5, sticky="w")

#---Funct5: Amplify
     tkgrid( ttklabel(T2frame2, text="XPS Sample:"),
             row = 3, column = 1, padx = 5, pady = 5, sticky="e")
     XS2 <- tclVar("XPSSample")
     objFunctAmpliXS <- ttkcombobox(T2frame2, width = 15, textvariable = XS2, values = "    ", foreground="grey")
     tkbind(objFunctAmpliXS, "<<ComboboxSelected>>", function(){
                              XSamp <- tclvalue(XS2)
                              CLlist <- XPSSpectList(XSamp)
                              tkconfigure(objFunctAmpliCL, values=CLlist)
                    })
     tkgrid(objFunctAmpliXS, row = 3, column = 2, padx = 5, pady = 5, sticky="w")

#     tkgrid( ttklabel(T2frame2, text="Core.Line"),
#             row = 3, column = 3, padx = 5, pady = 5, sticky="e")
     CL2 <- tclVar("Core.Line")
     objFunctAmpliCL <- ttkcombobox(T2frame2, width = 15, textvariable = CL2, values = "    ", foreground="grey")
     tkbind(objFunctAmpliCL, "<<ComboboxSelected>>", function(){
                              WidgetState(objFunctFact, "normal")
                    })
     tkgrid(objFunctAmpliCL, row = 3, column = 3, padx = 5, pady = 5, sticky="w")

     tkgrid( ttklabel(T2frame2, text="Scale Factor:"),
             row = 3, column = 4, padx = 5, pady = 5, sticky="e")
     SCALEfACT <- tclVar("? ")  #sets the initial msg
     objFunctFact <- ttkentry(T2frame2, textvariable=SCALEfACT, width=7, foreground="grey")
     tkbind(objFunctFact, "<FocusIn>", function(K){
                           tclvalue(SCALEfACT) <- ""
                           tkconfigure(objFunctFact, foreground="red")
                    })
     tkbind(objFunctFact, "<Key-Return>", function(K){
                           tkconfigure(objFunctFact, foreground="black")
                           indx <- grep(tclvalue(CL2), SelectedNames$CoreLines)
print(SelectedNames)
                           SelectedNames$Ampli[indx] <<- as.numeric(tclvalue(SCALEfACT))
                           CtrlPlot()
                    })
     tkgrid(objFunctFact, row = 3, column = 5, padx = 5, pady = 5, sticky="w")

#---Funct6: X, Y offset
     tkgrid( ttklabel(T2frame2, text="X, Y Offsets:"),
             row = 4, column = 1, padx = 5, pady = 5, sticky="e")
     XOFFSET <- tclVar("X_Off=")  #sets the initial msg
     XOffsetobj <- ttkentry(T2frame2, textvariable=XOFFSET, width=15, foreground="grey")
     tkbind(XOffsetobj, "<FocusIn>", function(K){
                           tclvalue(XOFFSET) <- ""
                           tkconfigure(XOffsetobj, foreground="red")
                    })
     tkbind(XOffsetobj, "<Key-Return>", function(K){
                           tkconfigure(XOffsetobj, foreground="black")
                           xx <- as.numeric(tclvalue(XOFFSET))
                           if ( is.na(xx) ) xx <- 0
                           PlotParameters$XOffset <<- xx
                           CtrlPlot()
                    })
     tkgrid(XOffsetobj, row = 4, column = 2, padx = 5, pady = 5, sticky="w")

     YOFFSET <- tclVar("Y_Off=")  #sets the initial msg
     YOffsetobj <- ttkentry(T2frame2, textvariable=YOFFSET, width=15, foreground="grey")
     tkbind(YOffsetobj, "<FocusIn>", function(K){
                           tclvalue(YOFFSET) <- ""
                           tkconfigure(YOffsetobj, foreground="red")
                    })
     tkbind(YOffsetobj, "<Key-Return>", function(K){
                           tkconfigure(YOffsetobj, foreground="black")
                           yy <- as.numeric(tclvalue(YOFFSET))
                           if ( is.na(yy) ) yy <- 0
                           PlotParameters$YOffset <<- yy
                           CtrlPlot()
                    })
     tkgrid(YOffsetobj, row = 4, column = 3, padx = 5, pady = 5, sticky="w")

#---Funct7: 3D
     T2frame3 <- ttklabelframe(T2group1, text = " 3D PLOT ", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T2frame3, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

     T2frame33 <- ttkframe(T2frame3, borderwidth=0, padding=c(0,0,0,0))
     tkgrid(T2frame33, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     PSEUDO3D <- tclVar(FALSE)
     objFunctPseudo3D <- tkcheckbutton(T2frame33, text="Pseudo-3D", variable=PSEUDO3D, onvalue = 1, offvalue = 0,
                         command=function(){
                           PlotParameters$Pseudo3D <<- as.logical(as.numeric(tclvalue(PSEUDO3D)))
                           CtrlPlot()
                    })
     tkgrid(objFunctPseudo3D, row = 1, column = 1, padx = c(1,15), pady=5, sticky="w")

     TRED <- tclVar(FALSE)
     objFunctTreD <- tkcheckbutton(T2frame33, text="3D ", variable=TRED, onvalue = 1, offvalue = 0,
                         command=function(){
                           PlotParameters$TreD <<- as.logical(as.numeric(tclvalue(TRED)))
                           OvType <- tclvalue(PLOTTYPE)
                           if (OvType != "Spectrum") {
                              tkmessageBox(message="3D plot allowed only for plot mode SPECTRUM" , title = "WARNING: WRONG MODE PLOT",  icon = "warning")
                              tclvalue(TRED) <- PlotParameters$TreD <<- FALSE
                           } else {
                              if (PlotParameters$TreD == TRUE) {
                                  PlotParameters$OverlayMode <<- "TreD"
                                  Plot_Args$ylab$label <<- "Sample"
                                  Plot_Args$zlab$label <<- "Intensity [cps]"
                                  if ( tclvalue(NORMALIZE) == "1") {
                                     Plot_Args$zlab$label <<- "Intensity [a.u.]"
                                  }
#                                  Plot_Args$main$label <<- tclvalue(MAINTITLE)
#                                  if (tclvalue(MAINTITLE) == ""){
#                                      Plot_Args$main$label <<- activeSpectName
#                                  }
                                  WidgetState(T4_ZAxNameChange, "normal")
                              } else {
                                  PlotParameters$OverlayMode <<- FALSE
                                  PlotParameters$OverlayMode <<- "Single-Panel"
                                  Plot_Args$ylab$label <<- "Intensity [cps]"   #restore Y axis label for the 2D graphic mode
                                  Plot_Args$zlab$label <<- NULL
                                  if ( tclvalue(NORMALIZE) == "1") {
                                      Plot_Args$ylab$label <<- "Intensity [a.u.]"
                                  }
                                  WidgetState(T4_ZAxNameChange, "disabled")
                              }
                           }
                           CtrlPlot()
                    })
     tkgrid(objFunctTreD, row = 1, column = 2, padx = c(1,15), pady=5, sticky="w")

     AXOFFSET <- tclVar("Ax_Off=")  #sets the initial msg
     AxOffsetobj <- ttkentry(T2frame33, textvariable=AXOFFSET, width = 7, foreground="grey")
     tkbind(AxOffsetobj, "<FocusIn>", function(K){
                           tclvalue(AXOFFSET) <- ""
                           tkconfigure(AxOffsetobj, foreground="red")
                    })
     tkbind(AxOffsetobj, "<Key-Return>", function(K){
                           tkconfigure(AxOffsetobj, foreground="black")
                           dd <- as.numeric(tclvalue(AXOFFSET))
                           if ( is.na(dd) ) dd <- NULL
                           PlotParameters$AxOffset <<- dd
                           CtrlPlot()
                    })
     tkgrid(AxOffsetobj, row = 1, column = 3, padx = 5, pady = 5, sticky="w")
     tkgrid( ttklabel(T2frame33, text="Ax.Name Offset "),
             row = 1, column = 4, padx = c(1,15), pady = 5, sticky="e")

     TREDASPECT <- tclVar()
     objTreDAspect <- ttkcombobox(T2frame33, width = 7, textvariable = TREDASPECT, values = c("3|1", "2|1","1|1", "1|2", "1|3"))
     tkbind(objTreDAspect, "<<ComboboxSelected>>", function(){
                           indx <- grep(tclvalue(TREDASPECT), c("3|1", "2|1","1|1", "1|2", "1|3"), fixed=TRUE)
                           aspect <- matrix(c(0.3,0.5,1,2,3,   1,1,1,1,1), nrow=5) # this are the corresponding values to set the 3d aspect
                           PlotParameters$TreDAspect <<- as.vector(aspect[indx,])
                           CtrlPlot()
                    })
     tkgrid(objTreDAspect, row = 1, column = 5, padx = 5, pady = 5, sticky="w")
     tkgrid( ttklabel(T2frame33, text="X|Y aspect ratio"),
             row = 1, column = 6, padx = c(1,5), pady = 5, sticky="e")

     T2frame333 <- ttkframe(T2frame3, borderwidth=0, padding=c(0,0,0,0))
     tkgrid(T2frame333, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
     tkgrid( ttklabel(T2frame333, text="Azymuth rotation:"),
             row = 1, column = 1, padx = 5, pady = 5, sticky="w")
     AZROT <- tclVar()
     T2AzymutRot <- ttkscale(T2frame333, from=0, to=90, value=35, variable=AZROT, orient="horizontal", length=180)
     tkbind(T2AzymutRot, "<ButtonRelease>", function(K){
                           PlotParameters$AzymuthRot <<- as.numeric(tclvalue(AZROT))
                           CtrlPlot()
                    })
     tkgrid(T2AzymutRot, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

     tkgrid( ttklabel(T2frame333, text="Zenith rotation:"),
             row = 1, column = 3, padx = 5, pady = 5, sticky="w")
     ZNROT <- tclVar()
     T2ZenithRot <- ttkscale(T2frame333, from=0, to=90, value=15, variable=ZNROT, orient="horizontal", length=180)
     tkbind(T2ZenithRot, "<ButtonRelease>", function(K){
                           PlotParameters$ZenithRot <<- as.numeric(tclvalue(ZNROT))
                           CtrlPlot()
                    })
     tkgrid(T2ZenithRot, row = 1, column = 4, padx = 5, pady = 5, sticky="w")

#---Funct8: Zoom
     T2frame4 <- ttklabelframe(T2group1, text=" ZOOM & CURSOR POSITION ", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T2frame4, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

     tkgrid( ttklabel(T2frame4, text="Set zoom area corners with SX mouse button"),
             row = 1, column = 1, padx = 5, pady = 1, sticky="w")

     T2group5 <- ttkframe(T2frame4, borderwidth=0, padding=c(0,0,0,0))
     tkgrid(T2group5, row = 2, column = 1, padx = 5, pady = c(1, 5), sticky="w")

     LimitsBtn <- tkbutton(T2group5, text="  Set Zoom Limits  ", command=function(){
                           if (PlotParameters$OverlayMode=="Multi-Panel") {
                               tkmessageBox(message="ZOOM option" , title = "WARNING: zoom option not available in MULTI-PANEL mode",  icon = "warning")
                           } else {
                             FName <- get(activeFName, envir=.GlobalEnv)
                             CLname <- get("activeSpectName", envir=.GlobalEnv)
                             SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv)

                             trellis.focus("panel", 1, 1, highlight=FALSE)
                             pos <- list(x=NULL, y=NULL)   #initial values to enter in the while loop
                             pos1 <- list(x=NULL, y=NULL)
                             pos2 <- list(x=NULL, y=NULL)
                             X1 <<- min(Xlim)
                             X2 <<- max(Xlim)
                             RangeX <- Xlim[2]-Xlim[1]
                             Y1 <- min(Ylim)
                             RangeY <- Ylim[2]-Ylim[1]
                             width <- max(convertX(unit(Xlim, "native"), "points", TRUE))
                             height <- max(convertY(unit(Ylim, "native"), "points", TRUE))
                             RevAx <- as.logical(as.numeric(tclvalue(REVERSE)))   #the X axis reversed?

                             #First zoom area corner
                             pos <- grid::grid.locator(unit = "points")
                             if (FName[[CLname]]@Flags[1] && RevAx==TRUE) { #Binding energy set
                                pos1$x <- X2-as.numeric(pos$x)*RangeX/width    #reversed scale
                             } else if (! FName[[CLname]]@Flags[1] && RevAx==FALSE) { #Kinetic energy scale
                                pos1$x <- X1+as.numeric(pos$x)*RangeX/width           #not reversed scale
                             }
                             if (FName[[CLname]]@Flags[1] && RevAx==FALSE) { #Binding energy set
                                pos1$x <- X1 + as.numeric(pos$x)*RangeX/width   #not reversed scale
                             } else if (! FName[[CLname]]@Flags[1] && RevAx==TRUE) {#Kinetic energy scale
                                pos1$x <- X2-as.numeric(pos$x)*RangeX/width         #reversed scale
                             }
                             pos1$y <- as.numeric(pos$y)*RangeY/height+Y1
                             #shows the first marker
                             panel.superpose(x=pos1$x,y=pos1$y,subscripts=c(1,1),groups=1, type="p", pch=3, cex=1, lwd=1.5, col="blue")

                             #Second zoom area corner
                             pos <- grid::grid.locator(unit = "points")
                             if (FName[[CLname]]@Flags[1] && RevAx==TRUE) { #Binding energy set
                                pos2$x <- X2-as.numeric(pos$x)*RangeX/width    #reversed scale
                             }  else if (! FName[[CLname]]@Flags[1] && RevAx==FALSE) { #Kinetic energy scale
                                pos2$x<- X1+as.numeric(pos$x)*RangeX/width           #not reversed scale
                             }
                             if (FName[[CLname]]@Flags[1] && RevAx==FALSE) { #Binding energy set
                                 pos2$x <- X1 + as.numeric(pos$x)*RangeX/width       #not reversed scale
                             } else if (! FName[[CLname]]@Flags[1] && RevAx==TRUE) {#Kinetic energy scale
                                 pos2$x <- X2-as.numeric(pos$x)*RangeX/width         #reversed scale
                             }
                             pos2$y <- as.numeric(pos$y)*RangeY/height+Y1
                             #shows the second marker
                             panel.superpose(x=pos2$x,y=pos2$y,subscripts=c(1,1),groups=1, type="p", pch=3, cex=0.8, lwd=1.8, col="red")

                             #define zoom area with a rectangle
                             pos$x <- c(pos1$x, pos2$x, pos2$x, pos1$x, pos1$x)
                             pos$y <- c(pos1$y, pos1$y, pos2$y, pos2$y, pos1$y)
                             panel.superpose(x=pos$x,y=pos$y,subscripts=c(1,1),groups=1, type="l", lwd=1, col="black")

                             trellis.unfocus()
                             if (FName[[CLname]]@Flags) { #Binding energy set
                                 Plot_Args$xlim <<- sort(c(pos1$x, pos2$x), decreasing=TRUE)
                                 Plot_Args$ylim <<- sort(c(pos1$y, pos2$y))
                             } else {
                                 Plot_Args$xlim <<- sort(c(pos1$x, pos2$x))
                                 Plot_Args$ylim <<- sort(c(pos1$y, pos2$y))
                             }

                           }
                    })
     tkgrid(LimitsBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     OKBtn1 <- tkbutton(T2group5, text="    OK    ", command=function(){
                           CtrlPlot()
                    })
     tkgrid(OKBtn1, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

     RSTScaleBtn <- tkbutton(T2group5, text="  RESET SCALES  ", command=function(){
                           Plot_Args$xlim <<- NULL    #xlim set in XPSOverlayEngine
                           Plot_Args$ylim <<- NULL    #ylim set in XPSOverlayEngine
                           CtrlPlot()
                    })
     tkgrid(RSTScaleBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

     tkgrid( ttklabel(T2frame4, text="Exact Range Values:"),
             row = 3, column = 1, padx = 5, pady = c(5, 1), sticky="w")
     T2group6 <- ttkframe(T2frame4, borderwidth=0, padding=c(0,0,0,0))
     tkgrid(T2group6, row = 4, column = 1, padx = 5, pady =c(1, 5), sticky="w")

     ZMXmin <- tclVar("Xmin =")  #sets the initial msg
     X1 <- ttkentry(T2group6, textvariable=ZMXmin, width=12, foreground="grey")
     tkbind(X1, "<FocusIn>", function(K){
                           tclvalue(ZMXmin) <- ""
                           tkconfigure(X1, foreground="red")
                    })
     tkbind(X1, "<Key-Return>", function(K){
                           tkconfigure(X1, foreground="black")
                    })
     tkgrid(X1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     ZMXmax <- tclVar("Xmax =")  #sets the initial msg
     X2 <- ttkentry(T2group6, textvariable=ZMXmax, width=12, foreground="grey")
     tkbind(X2, "<FocusIn>", function(K){
                           tclvalue(ZMXmax) <- ""
                           tkconfigure(X2, foreground="red")
                    })
     tkbind(X2, "<Key-Return>", function(K){
                           tkconfigure(X2, foreground="black")
                    })
     tkgrid(X2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

     ZMYmin <- tclVar("Ymin =")  #sets the initial msg
     Y1 <- ttkentry(T2group6, textvariable=ZMYmin, width=12, foreground="grey")
     tkbind(Y1, "<FocusIn>", function(K){
                           tclvalue(ZMYmin) <- ""
                           tkconfigure(Y1, foreground="red")
                    })
     tkbind(Y1, "<Key-Return>", function(K){
                           tkconfigure(Y1, foreground="black")
                    })
     tkgrid(Y1, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

     ZMYmax <- tclVar("Ymax =")  #sets the initial msg
     Y2 <- ttkentry(T2group6, textvariable=ZMYmax, width=12, foreground="grey")
     tkbind(Y2, "<FocusIn>", function(K){
                           tclvalue(ZMYmax) <- ""
                           tkconfigure(Y2, foreground="red")
                    })
     tkbind(Y2, "<Key-Return>", function(K){
                           tkconfigure(Y2, foreground="black")
                    })
     tkgrid(Y2, row = 1, column = 4, padx = 5, pady = c(1, 5), sticky="w")

     OKBtn2 <- tkbutton(T2group6, text="    OK    ", command=function(){
                           x1 <- as.numeric(tclvalue(ZMXmin))
                           x2 <- as.numeric(tclvalue(ZMXmax))
                           y1 <- as.numeric(tclvalue(ZMYmin))
                           y2 <- as.numeric(tclvalue(ZMYmax))
                           Xlim <<- c(x1, x2)
                           Ylim <<- c(y1, y2)
                           if (tclvalue(REVERSE)=="TRUE") { #Reverse axis
                               Plot_Args$xlim <<- sort(c(x1, x2), decreasing=TRUE)
                               Xlim <<- sort(c(x1, x2), decreasing=TRUE)
                               Plot_Args$ylim <<- sort(c(y1, y2))
                           } else {
                               Plot_Args$xlim <<- sort(c(x1, x2))
                               Xlim <<- sort(c(x1, x2))
                               Plot_Args$ylim <<- sort(c(y1, y2))
                           }
                           CtrlPlot()
                    })
     tkgrid(OKBtn2, row = 1, column = 5, padx = 5, pady = 5, sticky="w")

     RSTScaleBtn <- tkbutton(T2group6, text=" RESET SCALES ", command=function(){
                           Plot_Args$xlim <<- NULL    #xlim set in XPSOverlayEngine
                           Plot_Args$ylim <<- NULL    #ylim set in XPSOverlayEngine
                           tclvalue(ZMXmin) <- "Xmin="
                           tkconfigure(X1, foreground="grey")
                           tclvalue(ZMXmax) <- "Xmax="
                           tkconfigure(X2, foreground="grey")
                           tclvalue(ZMYmin) <- "Ymin="
                           tkconfigure(Y1, foreground="grey")
                           tclvalue(ZMYmax) <- "Ymax="
                           tkconfigure(Y2, foreground="grey")
                           CtrlPlot()
                    })
     tkgrid(RSTScaleBtn, row = 1, column = 6, padx = 5, pady = 5, sticky="w")

     tkgrid( ttklabel(T2frame4, text="Position Left button    EXIT Right button"),
             row = 5, column = 1, padx = 5, pady = c(5, 1), sticky="w")

     T2group7 <- ttkframe(T2frame4, borderwidth=0, padding=c(0,0,0,0))
     tkgrid(T2group7, row = 6, column = 1, padx = 5, pady = c(1, 5), sticky="w")
     CurPosBtn <- tkbutton(T2group7, text=" Get Cursor Position ", command=function(){
                            if (PlotParameters$OverlayMode=="Multi-Panel") {
                                tkmessageBox(message="CURSOR POSITION option not available in MULTI-PANEL mode" , title = "WARNING: CURSOR POSITION OPTION NOT AVAILABLE IN MULTI-PANEL MODE",  icon = "warning")
                            } else {
                                FName <- get(activeFName, envir=.GlobalEnv)
                                SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv)
                                trellis.focus("panel", 1, 1, highlight=FALSE)
                                pos <- list(x=NULL, y=NULL)   #initial pos values to wenter in the while loop
                                xx <- Plot_Args$xlim
                                if (is.null(xx)) {  #xlim null => no zoom
                                    xx <- Xlim
                                    yy <- Ylim
                                } else {
                                    xx <- Plot_Args$xlim
                                    yy <- Plot_Args$ylim
                                }
                                X1 <- min(xx)
                                X2 <- max(xx)
                                RangeX <- abs(xx[2]-xx[1])
                                Y1 <- min(yy)
                                RangeY <- yy[2]-yy[1]

                                RevAx <- as.logical(as.numeric(tclvalue(REVERSE)))
                                width <- max(convertX(unit(xx, "native"), "points", TRUE))
                                height <- max(convertY(unit(yy, "native"), "points", TRUE))
                                while (! is.null(pos)) {
                                      pos <- grid::grid.locator(unit = "points")
                                      if (is.null(pos)) break ## non-left click
                                      if (FName[[SpectIndx]]@Flags[1] && RevAx==TRUE) { #Binding energy set
                                          pos$x <- X2-as.numeric(pos$x)*RangeX/width      #reversed scale
                                      } else if (! FName[[SpectIndx]]@Flags[1] && RevAx==FALSE) { #Kinetic energy scale
                                          pos$x <- X1+as.numeric(pos$x)*RangeX/width              #not reversed scale
                                      } else if (FName[[SpectIndx]]@Flags[1] && RevAx==FALSE) { #Binding energy set
                                          pos$x <- X1 + as.numeric(pos$x)*RangeX/width          #not reversed scale
                                      } else if (! FName[[SpectIndx]]@Flags[1] && RevAx==TRUE) {#Kinetic energy scale
                                          pos$x <- X2-as.numeric(pos$x)*RangeX/width            #reversed scale
                                      }
                                      pos$y <- as.numeric(pos$y)*RangeY/height+Y1
                                      pos$x <- round(pos$x,digits=2)
                                      pos$y <- round(pos$y,digits=2)
                                      txt <- paste("X: ", as.character(pos$x), ", Y: ", as.character(pos$y), sep="")
                                      tkconfigure(ZMXYpos, text=txt)
                                      tcl("update", "idletasks")  # forces the txt to be flushed in glabel
                                }
                                trellis.unfocus()
                           }
                           CtrlPlot()
                    })
     tkgrid(CurPosBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     ZMXYpos <- ttklabel(T2group7, text=" X, Y :")
     tkgrid(ZMXYpos, row = 1, column = 2, padx = 5, pady = 5, sticky="w")


# --- TAB3 ---

# Rendering options
     T3group1 <- ttkframe(NB,  borderwidth=0, padding=c(0,0,0,0) )
     tkadd(NB, T3group1, text=" RENDERING ")

     T3group2 <- ttkframe(T3group1, borderwidth=0, padding=c(0,0,0,0))
     tkgrid(T3group2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
     T3group3 <- ttkframe(T3group1, borderwidth=0, padding=c(0,0,0,0))
     tkgrid(T3group3, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

     tkgrid( ttklabel(T3group2, text="Double click to change colors"),
             row = 1, column = 1, padx = 5, pady = 5)

     T3F_Palette <- ttkframe(T3group2, borderwidth=0, padding=c(0,0,0,0) )
     tkgrid(T3F_Palette, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

     T3F_Colors <- ttklabelframe(T3F_Palette, text="C.Lines  Baseline  FitComp   Fit", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T3F_Colors, row = 1, column = 1, padx = 5, pady = 0, sticky="w")
     #building the widget to change CL colors
     for(ii in 1:20){ #column1 colors 1 - 20
         CLcolor[[ii]] <- ttklabel(T3F_Colors, text=as.character(ii), width=6, font="Serif 8", background=Colors[ii])
         tkgrid(CLcolor[[ii]], row = ii, column = 1, padx = c(5,0), pady = 1, sticky="w")
         tkbind(CLcolor[[ii]], "<Double-1>", function( ){
                         X <- as.numeric(tkwinfo("pointerx", OverlayWindow))
                         Y <- as.numeric(tkwinfo("pointery", OverlayWindow))
                         WW <- tkwinfo("containing", X, Y)
                         BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                         BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                         colIdx <- grep(BKGcolor, Colors) #
                         BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                         Colors[colIdx] <<- BKGcolor
                         tkconfigure(CLcolor[[colIdx]], background=Colors[colIdx])
                         PlotParameters$Colors <<- Colors
                         CtrlPlot()
               })
     }

     BLColor <- ttklabel(T3F_Colors, text=as.character(1), width=6, font="Serif 8", background=FitColors$BaseColor[1])
     tkgrid(BLColor, row = 1, column = 2, padx = c(12,0), pady = 1, sticky="w")
     tkbind(BLColor, "<Double-1>", function( ){
                      FitColors$BaseColor[1] <<- as.character(.Tcl('tk_chooseColor'))
                      FitColors$BaseColor <<- rep(FitColors$BaseColor[1], 20)
                      tkconfigure(BLColor, background=FitColors$BaseColor[1])
                      PlotParameters$FitCol <<- FitColors
                      CtrlPlot()
                   })

     #If FitComp = Multicolor building the widget to change FitComp colors
     if (XPSSettings$General[8] == "PolyChromeFC"){
         for(ii in 1:20){
             FCcolor[[ii]] <- ttklabel(T3F_Colors, text=as.character(ii), width=6, font="Serif 8", background=FitColors$CompColor[ii])
             tkgrid(FCcolor[[ii]], row = ii, column = 3, padx = c(12,0), pady = 1, sticky="w")
             tkbind(FCcolor[[ii]], "<Double-1>", function( ){
                          X <- as.numeric(tkwinfo("pointerx", OverlayWindow))
                          Y <- as.numeric(tkwinfo("pointery", OverlayWindow))
                          WW <- tkwinfo("containing", X, Y)
                          BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                          BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                          colIdx <- grep(BKGcolor, FitColors$CompColor) #index of the selected color
                          BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                          tkconfigure(FCcolor[[colIdx]], background=BKGcolor)
                          FitColors$CompColor[colIdx] <<- BKGcolor
                          PlotParameters$FitCol <<- FitColors
                          CtrlPlot()
                       })
         }
     }
     #If FitComp = Singlecolor building the widget to change the Baseline FitComp and Fit colors
     if (XPSSettings$General[8] == "MonoChromeFC"){
          FCcolor <- ttklabel(T3F_Colors, text=as.character(1), width=6, font="Serif 8", background=FitColors$CompColor[1])
          tkgrid(FCcolor, row = 1, column = 3, padx = c(12,0), pady = 1, sticky="w")
          tkbind(FCcolor, "<Double-1>", function( ){
                           FitColors$CompColor[1] <<- as.character(.Tcl('tk_chooseColor'))
                           tkconfigure(FCcolor, background=FitColors$CompColor[1])
                           FitColors$CompColor <<- rep(FitColors$CompColor[1], 20)
                           PlotParameters$FitCol <<- FitColors
                           CtrlPlot()
                       })
     }
     #Fit Color
     FTColor <- ttklabel(T3F_Colors, text=as.character(1), width=6, font="Serif 8", background=FitColors$FitColor[1])
     tkgrid(FTColor, row = 1, column = 4, padx = c(12,0), pady = 1, sticky="w")
     tkbind(FTColor, "<Double-1>", function( ){
                      FitColors$FitColor[1] <<- as.character(.Tcl('tk_chooseColor'))
                      tkconfigure(FTColor, background=FitColors$FitColor[1])
                      FitColors$FitColor <<- rep(FitColors$FitColor[1], 20)
                      PlotParameters$FitCol <<- FitColors
                      CtrlPlot()
                   })

     T3F_BW_Col <- ttklabelframe(T3group3, text="COLOR", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T3F_BW_Col, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     BWCOL <- tclVar("RainBow")
     T3_BW_Col <- ttkcombobox(T3F_BW_Col, width = 15, textvariable = BWCOL, values = c("Black/White", "RainBow"))
     tkbind(T3_BW_Col, "<<ComboboxSelected>>", function(){
                           if(tclvalue(BWCOL) == "Black/White") {
                              tclvalue(LINETYPE) <<- "Patterns"
                              ClearWidget(T3F_Palette)
                              #now generate MonoChrome palette
                              T3F_Colors <- ttklabelframe(T3F_Palette, text="C.Lines  Baseline  FitComp   Fit", borderwidth=2, padding=c(5,5,5,5))
                              tkgrid(T3F_Colors, row = 1, column = 1, padx = 5, pady = 0, sticky="w")

                              CLcolor[[1]] <<- ttklabel(T3F_Colors, text=" ", width=6, font="Serif 8") #Column of empty cells
                              tkgrid(CLcolor[[1]], row = 1, column = 1, padx = 5, pady = 1, sticky="w")
                              tkconfigure(CLcolor[[1]], background="black")

                              BLColor <<- ttklabel(T3F_Colors, text=as.character(1), width=6, font="Serif 8", background="black")
                              tkgrid(BLColor, row = 1, column = 2, padx = c(12, 0), pady = 1, sticky="w")
                              tkconfigure(BLColor, background="black")
                              FitColors$BaseColor <<- rep("black", 20)

                              FCcolor[[1]] <<- ttklabel(T3F_Colors, text=" ", width=6, font="Serif 8", background="black")
                              tkgrid(FCcolor[[1]], row = 1, column = 3, padx = c(12, 0), pady = 1, sticky="w")
                              tkconfigure(FCcolor[[1]], background="gray45")
                              FitColors$CompColor <<- rep("gray45", 20)
                              SetBWCol()
                           } else if (tclvalue(BWCOL) == "RainBow"){
                              ClearWidget(T3F_Palette)
                              #now generate MonoChrome palette
                              T3F_Colors <- ttklabelframe(T3F_Palette, text="C.Lines  Baseline  FitComp   Fit", borderwidth=2, padding=c(5,5,5,5))
                              tkgrid(T3F_Colors, row = 1, column = 1, padx = 5, pady = 0, sticky="w")
                              for(ii in 1:20){ #column1 colors 1 - 20
                                  CLcolor[[ii]] <- ttklabel(T3F_Colors, text=as.character(ii), width=6, font="Serif 8", background=Colors[ii])
                                  tkgrid(CLcolor[[ii]], row = ii, column = 1, padx = c(5,0), pady = 1, sticky="w")
                                  tkbind(CLcolor[[ii]], "<Double-1>", function( ){
                                                      X <- as.numeric(tkwinfo("pointerx", OverlayWindow))
                                                      Y <- as.numeric(tkwinfo("pointery", OverlayWindow))
                                                      WW <- tkwinfo("containing", X, Y)
                                                      BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                                                      BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                                                      colIdx <- grep(BKGcolor, Colors) #
                                                      BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                                                      tkconfigure(CLcolor[[colIdx]], background=BKGcolor)
                                                      Colors[colIdx] <<- BKGcolor
                                                      PlotParameters$Colors <<- Colors

                                                      if (tclvalue(SETLINES) == "ON") {
                                                          lty <- tclvalue(LINETYPE)
                                                          if (lty == "Solid") {
                                                              Plot_Args$par.settings$superpose.line$lty <<- rep("solid", 20)
                                                          } else if (lty == "Patterns") {
                                                              Plot_Args$par.settings$superpose.line$lty <<- LType
                                                          }
                                                          Plot_Args$par.settings$superpose.line$col <<- Colors
                                                      }
                                                      if (tclvalue(SETSYMBOLS) == "ON"){
                                                          symty <- tclvalue(SYMTYPE)
                                                          if (symty == "Single-Symbol") {
                                                              Plot_Args$par.settings$superpose.symbol$pch <<- rep(1, 20)
                                                          } else if (symty == "multi-symbol") {
                                                              Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                                          }
                                                          Plot_Args$par.settings$superpose.symbol$col <<- Colors
                                                          Plot_Args$par.settings$superpose.symbol$fill <<- Colors
                                                      }
                                                      CtrlPlot()
                                         })
                              }
                              BLColor <- ttklabel(T3F_Colors, text=as.character(1), width=6, font="Serif 8", background=FitColors$BaseColor[1])
                              tkgrid(BLColor, row = 1, column = 2, padx = c(12,0), pady = 1, sticky="w")
                              tkbind(BLColor, "<Double-1>", function( ){
                                              BaseLinColors <- as.character(.Tcl('tk_chooseColor'))
                                              tkconfigure(BLColor, background=BaseLinColors)
                                              FitColors$BaseColor <<- rep(BaseLinColors, 20)
                                         })

                              if (XPSSettings$General[8] == "MonoChromeFC"){
                                  FCcolor <- ttklabel(T3F_Colors, text=as.character(1), width=6, font="Serif 8", background=FitColors$CompColor[1])
                                  tkgrid(FCcolor, row = 1, column = 3, padx = c(12,0), pady = 1, sticky="w")
                                  tkbind(FCcolor, "<Double-1>", function( ){
                                             FitColors$CompColor[1] <<- as.character(.Tcl('tk_chooseColor'))
                                             tkconfigure(FCcolor, background=FitColors$CompColor[1])
                                             FitColors$CompColor <<- rep(FitColors$CompColor[1], 20)
                                             PlotParameters$FitCol <<- FitColors
                                         })
                              } else if (XPSSettings$General[8] == "PolyChromeFC")
                                 for(ii in 1:20){
                                     FCcolor[[ii]] <- ttklabel(T3F_Colors, text=as.character(ii), width=6, font="Serif 8", background=FitColors$CompColor[ii])
                                     tkgrid(FCcolor[[ii]], row = ii, column = 3, padx = c(12,0), pady = 1, sticky="w")
                                     tkbind(FCcolor[[ii]], "<Double-1>", function( ){
                                                         X <- as.numeric(tkwinfo("pointerx", OverlayWindow))
                                                         Y <- as.numeric(tkwinfo("pointery", OverlayWindow))
                                                         WW <- tkwinfo("containing", X, Y)
                                                         BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                                                         BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                                                         colIdx <- grep(BKGcolor, FitColors$CompColor) #index of the selected color
                                                         BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                                                         tkconfigure(FCcolor[[colIdx]], background=BKGcolor)
                                                         FitColors$CompColor[colIdx] <<- BKGcolor
                                                         PlotParameters$FitCol <<- FitColors
                                         })
                              }

                              FTColor <- ttklabel(T3F_Colors, text=as.character(1), width=6, font="Serif 8", background=FitColors$FitColor[1])
                              tkgrid(FTColor, row = 1, column = 4, padx = c(12,0), pady = 1, sticky="w")
                              tkbind(FTColor, "<Double-1>", function( ){
                                                FitColors$FitColor[1] <<- as.character(.Tcl('tk_chooseColor'))
                                                FitColors$FitColor <<- rep(FitColors$FitColor[1], 20)
                                                tkconfigure(FTColor, background=FitColors$FitColor[1])
                                                PlotParameters$FitCol <<- FitColors
                                         })
                              SetRainbowCol()
                           }
                           CtrlPlot()
                    })
     tkgrid(T3_BW_Col, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_Grid <- ttklabelframe(T3group3, text="GRID", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T3F_Grid, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
     GRID <- tclVar()
     T3_Grid <- ttkcombobox(T3F_Grid, width = 15, textvariable = GRID, values = c("Grid ON", "Grid OFF"))
     tkbind(T3_Grid, "<<ComboboxSelected>>", function(){
                           if(tclvalue(GRID) == "Grid ON") {
                              Plot_Args$grid <<- TRUE
                           } else {
                              Plot_Args$grid <<- FALSE
                           }
                           CtrlPlot()
                    })
     tkgrid(T3_Grid, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_SetLines <- ttklabelframe(T3group3, text="SET LINES", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T3F_SetLines, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
     SETLINES <- tclVar("ON")
     Items <- c("ON", "OFF")
     for(ii in 1:2){
         T3_SetLines <- ttkradiobutton(T3F_SetLines, text=Items[ii], variable=SETLINES, value=Items[ii],
                        command=function(){
                           SetLinesPoints()
                    })
         tkgrid(T3_SetLines, row = 1, column = ii, padx = 5, pady = 5, sticky="w")
     }

     T3F_SetSymbols <- ttklabelframe(T3group3, text="SET SYMBOLS", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T3F_SetSymbols, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
     SETSYMBOLS <- tclVar("OFF")
     Items <- c("ON", "OFF")
     for(ii in 1:2){
         T3_SetSymbols <- ttkradiobutton(T3F_SetSymbols, text=Items[ii], variable=SETSYMBOLS, value=Items[ii],
                          command=function(){
                            tclvalue(LINETYPE) <- "Solid"
                            SetLinesPoints()
                    })
         tkgrid(T3_SetSymbols, row = 1, column = ii, padx = 5, pady = 5, sticky="w")
     }

     T3F_LineType <- ttklabelframe(T3group3, text="LINE TYPE", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T3F_LineType, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
     LINETYPE <- tclVar("Solid")
     T3_LineType <- ttkcombobox(T3F_LineType, width = 15, textvariable = LINETYPE, values = c("Solid", "Patterns"))
     tkbind(T3_LineType, "<<ComboboxSelected>>", function(){
                           Plot_Args$type <<- "l"
                           palette <- tclvalue(BWCOL)
#                           if (tclvalue(LINETYPE) == "Solid") {
#                              Plot_Args$lty <<- "solid"
#                              Plot_Args$pch <<- STypeIndx[1]
#                              tclvalue(BWCOL) <- "RainBow"
#                              tclvalue(LEGTXTCOLOR) <- "RainBow"
#                              PlotParameters$Colors <<- Colors
#                              Plot_Args$par.settings$superpose.symbol$fill <<- Colors
#                              Plot_Args$par.settings$superpose.line$col <<- Colors
#                              Plot_Args$par.settings$superpose.line$lty <<- "solid"
#                              Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx[1]
#                              Plot_Args$par.settings$strip.background$col <<- "lightskyblue"
#                              AutoKey_Args$col <<- Colors
#                           }
#                           if (tclvalue(LINETYPE) == "Patterns") {
#                              Plot_Args$lty <<- LType
#                              Plot_Args$pch <<- STypeIndx
#                              Plot_Args$par.settings$superpose.symbol$col <<- "black"
#                              Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
#                              Plot_Args$par.settings$superpose.line$col <<- "black"
#                              Plot_Args$par.settings$superpose.line$lty <<- LType
#                              Plot_Args$par.settings$strip.background$col <<- "gray90"
#                              AutoKey_Args$col <<- "black"
#                           }
                           SetLinesPoints()
#                           CtrlPlot()
                    })
     tkgrid(T3_LineType, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_LinWidth <- ttklabelframe(T3group3, text="LINE WIDTH", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T3F_LinWidth, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
     LINEWIDTH <- tclVar("1")
     T3_LinWidth <- ttkcombobox(T3F_LinWidth, width = 15, textvariable = LINEWIDTH, values = LWidth)
     tkbind(T3_LinWidth, "<<ComboboxSelected>>", function(){
                           Plot_Args$lwd <<- as.numeric(tclvalue(LINEWIDTH))
                           CtrlPlot()
                    })
     tkgrid(T3_LinWidth, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_SymbolType <- ttklabelframe(T3group3, text="SYMBOLS", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T3F_SymbolType, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
     SYMTYPE <- tclVar("Single-Symbol")
     T3_SymType <- ttkcombobox(T3F_SymbolType, width = 15, textvariable = SYMTYPE, values = c("Single-Symbol", "Multi-Symbols"))
     tkbind(T3_SymType, "<<ComboboxSelected>>", function(){
                           Plot_Args$type <<- "p"
                           palette <- tclvalue(BWCOL)
#                           if (tclvalue(SYMTYPE)=="Single-Symbol") {
#                              tclvalue(BWCOL) <- "RainBow"
#                              tclvalue(LINETYPE) <- "Patterns"
#                              Plot_Args$lty <<- LType
#                              Plot_Args$pch <<- STypeIndx[1]
#                              PlotParameters$Colors <<- Colors
#                              Plot_Args$par.settings$superpose.symbol$fill <<- Colors
#                              Plot_Args$par.settings$superpose.line$col <<- Colors
#                              Plot_Args$par.settings$superpose.line$lty <<- LType
#                              Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx[1]
#                              Plot_Args$par.settings$strip.background$col <<- "lightskyblue"
#                              AutoKey_Args$col <<- Colors
#                           }
#                           if (tclvalue(SYMTYPE)=="Multi-Symbols") {
#                              PlotParameters$Colors <<- "black"
#                              Plot_Args$lty <<- LType
#                              Plot_Args$pch <<- STypeIndx
#                              Plot_Args$par.settings$superpose.symbol$col <<- "black"
#                              Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
#                              Plot_Args$par.settings$superpose.line$col <<- "black"
#                              Plot_Args$par.settings$superpose.line$lty <<- LType
#                              Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
#                              Plot_Args$par.settings$strip.background$col <<- "gray90"
#                              AutoKey_Args$col <<- "black"
#                           }
                           SetLinesPoints()
#                           CtrlPlot()
                    })
     tkgrid(T3_SymType, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_SymSize <- ttklabelframe(T3group3, text="SYMSIZE", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T3F_SymSize, row = 4, column = 2, padx = 5, pady = 5, sticky="w")
     SYMSIZE <- tclVar("0.8")
     T3_SymSize <- ttkcombobox(T3F_SymSize, width = 15, textvariable = SYMSIZE, values = SymSize)
     tkbind(T3_SymSize, "<<ComboboxSelected>>", function(){
                           Plot_Args$cex <<- as.numeric(tclvalue(SYMSIZE))
                           CtrlPlot()
                    })
     tkgrid(T3_SymSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_FitCompStyle <- ttklabelframe(T3group3, text="FIT COMPONENT LINESTYLE", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T3F_FitCompStyle, row = 5, column = 1, padx = 5, pady = 5, sticky="w")
     FCSTYLE <- tclVar("Dotted")
     T3_FitCompStyle <- ttkcombobox(T3F_FitCompStyle, width = 15, textvariable = FCSTYLE, values = c("Dotted", "Solid", "Dashed"))
     tkbind(T3_FitCompStyle, "<<ComboboxSelected>>", function(){
                           if (tclvalue(FCSTYLE) == "Solid") {
                               PlotParameters$CompLty <<- "solid"
                           } else if (tclvalue(FCSTYLE) == "Dashed") {
                               PlotParameters$CompLty <<- "dashed"
                           } else if (tclvalue(FCSTYLE) == "Dotted") {
                               PlotParameters$CompLty <<- "dotted"
                           }
                           CtrlPlot()
                    })
     tkgrid(T3_FitCompStyle, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_PanStripCol <- ttklabelframe(T3group3, text="PANEL STRIP COLOR", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T3F_PanStripCol, row = 5, column = 2, padx = 5, pady = 5, sticky="w")
     STRIPCOLOR <- tclVar("grey")
     T3_PanStripCol <- ttkcombobox(T3F_PanStripCol, width = 15, textvariable = STRIPCOLOR,
                           values = c("white","grey", "darkgrey","lightblue","blue","darkblue","deepskyblue","lightbeige","beige","darkbeige","lightpink","pink","darkpink","lightgreen","green","darkgreen"))
     tkbind(T3_PanStripCol, "<<ComboboxSelected>>", function(){
                           StripCol <- tclvalue(STRIPCOLOR)
                           if(StripCol=="grey"){ StripCol <- "grey90" }
                           else if (StripCol=="darkgrey") { StripCol <- "gray60" }
                           else if (StripCol=="lightblue") { StripCol <- "lightskyblue1" }
                           else if(StripCol=="blue") { StripCol <- "lightskyblue3" }
                           else if(StripCol=="darkblue") { StripCol <- "steelblue3" }
                           else if (StripCol=="lightbeige") { StripCol <- "beige" }
                           else if(StripCol=="beige") { tripCol <- "bisque2" }
                           else if(StripCol=="darkbeige") { StripCol <- "navajowhite4" }
                           else if (StripCol=="pink") { StripCol <- "lightpink2" }
                           else if(StripCol=="darkpink") { StripCol <- "lightpink4" }
                           else if (StripCol=="lightgreen") { StripCol <- "darkseagreen1" }
                           else if(StripCol=="green")  { StripCol <- "darkseagreen2" }
                           else if(StripCol=="darkgreen") { StripCol <- "mediumseagreen" }
                           Plot_Args$par.settings$strip.background$col <<- StripCol
                           CtrlPlot()
                    })
     tkgrid(T3_PanStripCol, row = 1, column = 1, padx = 5, pady = 5, sticky="w")


# --- TAB4 ---

# Axis Rendering options

     T4group1 <- ttkframe(NB,  borderwidth=0, padding=c(0,0,0,0) )
     tkadd(NB, T4group1, text=" AXES ")

     T4AxOptgroup <- ttkframe(T4group1, borderwidth=0, padding=c(0,0,0,0))
     tkgrid(T4AxOptgroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

     T4F_TickPos <- ttklabelframe(T4AxOptgroup, text="TICKS", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T4F_TickPos, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
     TICKPOS <- tclVar("LeftBottom")
     T4_TickPos <- ttkcombobox(T4F_TickPos, width = 15, textvariable = TICKPOS, values = c("LeftBottom", "TopRight", "Both", "Custom X", "Custom Y"))
     tkbind(T4_TickPos, "<<ComboboxSelected>>", function(){
                             if (tclvalue(TICKPOS) == "LeftBottom") {
                                Plot_Args$scales$tck <<- c(1,0)
                                Plot_Args$scales$alternating <<- c(1)
                             } else if (tclvalue(TICKPOS) == "TopRight") {
                                Plot_Args$scales$tck <<- c(0,1)
                                Plot_Args$scales$alternating <<- c(2)
                             } else if (tclvalue(TICKPOS) == "Both") {
                                Plot_Args$scales$tck <<- c(1,1)
                                Plot_Args$scales$alternating <<- c(3)
                             } else if (tclvalue(TICKPOS) == "Custom X"){
                                Plot_Args$scales$tck <<- c(1,0)
                                Plot_Args$scales$alternating <<- c(1)
                                Plot_Args$scales$relation <<- "free"
                                CustomDta <- list(Xlim[1], Xlim[2], "X")
                                CustomAx(CustomDta)
                             } else if (tclvalue(TICKPOS) == "Custom Y") {
                                Plot_Args$scales$tck <<- c(1,0)
                                Plot_Args$scales$alternating <<- c(1)
                                Plot_Args$scales$relation <<- "free"
                                CustomDta <- list(Ylim[1], Ylim[2], "Y")
                                CustomAx(CustomDta)
                             }
                             CtrlPlot()
                    })
     tkgrid(T4_TickPos, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T4F_XScale <- ttklabelframe(T4AxOptgroup, text="X SCALE", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T4F_XScale, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
     XSCALETYPE <- tclVar("Standard")
     T4_XScale <- ttkcombobox(T4F_XScale, width = 15, textvariable = XSCALETYPE, values = c("Standard", "Power", "Log.10", "Log.e", "X E10", "X^10"))
     tkbind(T4_XScale, "<<ComboboxSelected>>", function(){
                             xx1 <- as.numeric(tclvalue(X1RANGE))
                             xx2 <- as.numeric(tclvalue(X2RANGE))
                             yy1 <- as.numeric(tclvalue(Y1RANGE))
                             yy2 <- as.numeric(tclvalue(Y2RANGE))
                             if (is.na(xx1*xx2*yy1*yy2) == FALSE) {
                                 tkmessageBox(message="Only 'standard' Scale Allowed on Custom X,Y Range", title="WARNING", icon="warning")
                                 return()
                             }
                             Plot_Args$xlab$label <<- FName[[SpectIndx]]@units[1]
                             if (tclvalue(XSCALETYPE) == "Standard") {
                                tclvalue(YSCALETYPE) <- "Standard"
                                Plot_Args$scales <<- list(cex=1, tck=c(1,0), alternating=c(1), tick.number=5, relation="same",
                                                     x=list(log=FALSE), y=list(log=FALSE), axs="i")
                                Xlabel <- FName[[SpectIndx]]@units[1]
                                Plot_Args$xlab <<- list(label=Xlabel, rot=0, cex=1.2)
                                Plot_Args$xscale.components <<- xscale.components.subticks
                             } else if (tclvalue(XSCALETYPE) == "Power") {
                                if (PlotParameters$TreD == TRUE){
                                    tkmessageBox(message="In 3D only standard scales allowed", title="WARNING", icon="warning")
                                    return()
                                }
                                Plot_Args$scales$x$log <<- 10    # 10^ power scale
                                Plot_Args$xscale.components <<- xscale.components.logpower
                             } else if (tclvalue(XSCALETYPE) == "Log.10") {
                                Xlim <- sort(Xlim)
                                if (tclvalue(REVERSE) == "1"){
                                    tkmessageBox(message="X-axis inverted. Please uncheck Reverse_Xaxis", title="Xaxis REVERSED", icon="warning")
                                    return()
                                }
                                if (Xlim[1] <= 0) {
                                    tkmessageBox(message="Cannot plot negatige or zero X-values !", title="WRONG X VALUES", icon="warning")
                                    return()
                                }
                                Plot_Args$xlab$label <<- paste("Log(",Xlabel, ")", sep="")
                                Plot_Args$scales$x$log <<- 10    # log10 scale
                                Plot_Args$xscale.components <<- xscale.components.log10ticks
                             } else if (tclvalue(XSCALETYPE) == "Log.e") {
                                if (tclvalue(REVERSE) == "1"){
                                    tkmessageBox(message="X-axis inverted. Please uncheck Reverse_Xaxis", title="Xaxis REVERSED", icon="warning")
                                    return()
                                }
                                Xlim <- sort(Xlim)
                                if (Xlim[1] <= 0) {
                                    tkmessageBox(message="Cannot plot negatige or zero X-values !", title="WRONG X VALUES", icon="warning")
                                    return()
                                }
                                Plot_Args$xlab$label <<- paste("Ln(",Xlabel, ")", sep="")
                                Plot_Args$scales$x$log <<- "e"   # log e scale
                                Plot_Args$xscale.components <<- xscale.components.subticks
                             } else if (tclvalue(XSCALETYPE) == "X E10"){ #X E+n
                                if (PlotParameters$TreD == TRUE){
                                    tkmessageBox(message="In 3D only standard scales allowed", title="WARNING", icon="warning")
                                    return()
                                }
                                Xlim <- sort(Xlim)
                                x_at <- NULL
                                x_labels <- NULL
                                xscl <- NULL
                                xscl <- xscale.components.default(
                                                 lim=Xlim, packet.number = 0,
                                                 packet.list = NULL, right = TRUE
                                        )
                                x_at <- xscl$bottom$labels$at
                                x_labels <- formatC(x_at, digits = 1, format = "e")
                                Plot_Args$scales$x <<- list(at = x_at, labels = x_labels)
                             } else if (tclvalue(XSCALETYPE) == "X^10") { #X^10 scale
                                if (PlotParameters$TreD == TRUE){
                                    tkmessageBox(message="In 3D only standard scales allowed", title="WARNING", icon="warning")
                                    return()
                                }
                                Plot_Args$scales$x$rot <<- 0
                                Plot_Args$scales$y$rot <<- 90
                                Xlim <- sort(Xlim)
                                x_at <- x_labels <- xscl <- NULL
                                xscl <- xscale.components.default(
                                                 lim=Xlim, packet.number = 0,
                                                 packet.list = NULL, right = TRUE
                                        )
                                x_at <- xscl$bottom$labels$at
                                eT <- floor(log10(abs(x_at)))# at == 0 case is dealt with below
                                mT <- x_at / 10 ^ eT
                                ss <- lapply(seq(along = x_at),
                                             function(jj) {
                                                    if (x_at[jj] == 0){
                                                        quote(0)
                                                    } else {
                                                        substitute(A %*% 10 ^ E, list(A = mT[jj], E = eT[jj]))
                                                    }
                                             })
                                xscl$left$labels$labels <- do.call("expression", ss)
                                x_labels <- xscl$left$labels$labels
                                Plot_Args$scales$x <<- list(at = x_at, labels = x_labels)
                             }
                             CtrlPlot()
                    })
     tkgrid(T4_XScale, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T4F_YScale <- ttklabelframe(T4AxOptgroup, text="Y SCALE", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T4F_YScale, row = 1, column = 3, padx = 5, pady = 5, sticky="w")
     YSCALETYPE <- tclVar("Standard")
     T4_YScale <- ttkcombobox(T4F_YScale, width = 15, textvariable = YSCALETYPE, values = c("Standard", "Power", "Log.10", "Log.e", "Y E10", "Y^10"))
     tkbind(T4_YScale, "<<ComboboxSelected>>", function(){
                             if (PlotParameters$TreD == TRUE){
                                 tkmessageBox(message="In 3D only 'standard' Scale Allowed", title="WARNING", icon="warning")
                                 Plot_Args$scales <<- list(cex=1, tck=c(1,0), tick.number=5, alternating=c(1), relation="same",
                                                           y=list(log=FALSE), axs="i")
                                 Plot_Args$yscale.components <<- yscale.components.subticks
                                 return()
                             }
                             xx1 <- as.numeric(tclvalue(X1RANGE))
                             xx2 <- as.numeric(tclvalue(X2RANGE))
                             yy1 <- as.numeric(tclvalue(Y1RANGE))
                             yy2 <- as.numeric(tclvalue(Y2RANGE))
                             if (is.na(xx1*xx2*yy1*yy2) == FALSE) {
                                 tkmessageBox(message="Only 'standard' Scale Allowed on Custom X,Y Range", title="WARNING", icon="warning")
                                 return()
                             }
                             Plot_Args$ylab$label <<- FName[[SpectIndx]]@units[2]
                             if (tclvalue(YSCALETYPE) == "Standard") {
                                tclvalue(XSCALETYPE) <- "Standard"
                                Ylabel <- FName[[SpectIndx]]@units[1]
                                Plot_Args$ylab <<- list(label=Ylabel, rot=90, cex=1.2)
                                Plot_Args$scales <<- list(cex=1, tck=c(1,0), tick.number=5, alternating=c(1), relation="same",
                                                          y=list(log=FALSE), axs="i")
                                Plot_Args$yscale.components <<- yscale.components.subticks
                             } else if (tclvalue(YSCALETYPE) == "Power") {
                                Plot_Args$scales$y$log <<- 10
                                Plot_Args$yscale.components <<- yscale.components.logpower
                             } else if (tclvalue(YSCALETYPE) == "Log.10") {
                                Ylim <- sort(Ylim)
                                if (Ylim[1] <= 0) {
                                    tkmessageBox(message="Cannot plot negative or zero Y-values !", title="WRONG Y VALUES", icon="warning")
                                    return()
                                }
                                Plot_Args$ylab$label <<- paste("Log(",Ylabel, ")", sep="")
                                Plot_Args$scales$y$log <<- 10
                                Plot_Args$yscale.components <<- yscale.components.log10ticks
                             } else if (tclvalue(YSCALETYPE) == "Log.e") { #log e scale
                                Ylim <- sort(Ylim)
                                if (Ylim[1] <= 0) {
                                    tkmessageBox(message="Cannot plot negative or zero Y-values !", title="WRONG Y VALUES", icon="warning")
                                    return()
                                }
                                Plot_Args$ylab$label <<- paste("Ln(",Ylabel, ")", sep="")
                                Plot_Args$scales$y$log <<- "e"
                                Plot_Args$yscale.components <<- yscale.components.subticks
                             } else if (tclvalue(YSCALETYPE) == "Y E10"){ #Y E+n
                                Ylim <- sort(Ylim)
                                y_at <- NULL
                                y_labels <- NULL
                                yscl <- NULL
                                yscl <- yscale.components.default(
                                                 lim=Ylim, packet.number = 0,
                                                 packet.list = NULL, right = TRUE
                                        )
                                y_at <- yscl$left$labels$at
                                y_labels <- formatC(y_at, digits = 1, format = "e")
                                Plot_Args$scales$y <<- list(at = y_at, labels = y_labels)
                             } else if (tclvalue(YSCALETYPE) == "Y^10") { #Y^10 scale
                                Plot_Args$scales$x$rot <<- 0
                                Plot_Args$scales$y$rot <<- 90
                                Ylim <- sort(Ylim)
                                y_at <- y_labels <- yscl <- NULL
                                yscl <- yscale.components.default(
                                                  lim=Ylim, packet.number = 0,
                                                  packet.list = NULL, right = TRUE
                                         )
                                y_at <- yscl$left$labels$at
                                eT <- floor(log10(abs(y_at)))# at == 0 case is dealt with below
                                mT <- y_at / 10 ^ eT
                                ss <- lapply(seq(along = y_at),
                                                  function(jj) {
                                                       if (y_at[jj] == 0){
                                                           quote(0)
                                                       } else {
                                                           substitute(A %*% 10 ^ E, list(A = mT[jj], E = eT[jj]))
                                                       }
                                                  })
                                yscl$left$labels$labels <- do.call("expression", ss)
                                y_labels <- yscl$left$labels$labels
                                Plot_Args$scales$y <<- list(at = y_at, labels = y_labels)
                             }
                             CtrlPlot()
                    })
     tkgrid(T4_YScale, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T4F_TitSize <- ttklabelframe(T4AxOptgroup, text="TITLE SIZE", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T4F_TitSize, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
     TITLESIZE <- tclVar("1.4")
     T4_TitSize <- ttkcombobox(T4F_TitSize, width = 15, textvariable = TITLESIZE, values = FontSize)
     tkbind(T4_TitSize, "<<ComboboxSelected>>", function(){
                           if (PlotParameters$OverlayMode == "Single-Panel" || PlotParameters$OverlayMode == "TreD") {
                               Plot_Args$main$cex <<- tclvalue(TITLESIZE)
                           } else if (PlotParameters$OverlayMode=="Multi-Panel") {
                               Plot_Args$par.strip.text$cex <<- as.numeric(tclvalue(TITLESIZE))
                           }
                           CtrlPlot()
                    })
     tkgrid(T4_TitSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T4F_MainTitChange <- ttklabelframe(T4AxOptgroup, text="CHANGE SINGLE-PANEL TITLE", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T4F_MainTitChange, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
     MAINTITLE <- tclVar("New Title")
     T4_MainTitChange <- ttkentry(T4F_MainTitChange, textvariable=MAINTITLE, foreground="grey")
     tkbind(T4_MainTitChange, "<FocusIn>", function(K){
                           tclvalue(MAINTITLE) <- ""
                           tkconfigure(T4_MainTitChange, foreground="red")
                    })
     tkbind(T4_MainTitChange, "<Key-Return>", function(K){
                           tkconfigure(T4_MainTitChange, foreground="black")
                           if (tclvalue(MAINTITLE) == ""){return()}
                           if (PlotParameters$OverlayMode == "Single-Panel") {
                               Plot_Args$scales$relation <<- "same"
                               Plot_Args$main$label <<- tclvalue(MAINTITLE)
                               CtrlPlot()
                           } else {
                               return()
                           }
                    })
     tkgrid(T4_MainTitChange, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T4F_PanelTitles <- ttklabelframe(T4AxOptgroup, text="CHANGE MULTI-PANEL TITLES", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T4F_PanelTitles, row = 2, column = 3, padx = 5, pady = 5, sticky="w")
     T4_PanelTitles <- tkbutton(T4F_PanelTitles, text="  CHANGE  ", command=function(){
                           PTitles <- as.data.frame(PanelTitles, stringsAsFactors=FALSE)
                           Title <- "CHANGE PANEL TITLE"
                           ColNames <- "Titles"
                           RowNames <- ""
                           PTitles <- DFrameTable(PTitles, Title, ColNames="", RowNames="",
                                                                 Width=25, Modify=TRUE, Env=environment(),
                                                                 Border=c(3,3,3,3))
                           PTitles <- unname(unlist(PTitles))
                           Plot_Args$PanelTitles <<- PTitles
                           CtrlPlot()
                    })
     tkgrid(T4_PanelTitles, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T4F_AxNumSize <- ttklabelframe(T4AxOptgroup, text="AXIS NUMBERS SIZE", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T4F_AxNumSize, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
     AXNUMSIZE <- tclVar("1.2")
     T4_AxNumSize <- ttkcombobox(T4F_AxNumSize, width = 15, textvariable = AXNUMSIZE, values = FontSize)
     tkbind(T4_AxNumSize, "<<ComboboxSelected>>", function(){
                           Plot_Args$scales$cex <<- tclvalue(AXNUMSIZE)
                           CtrlPlot()
                    })
     tkgrid(T4_AxNumSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T4F_AxLabSize <- ttklabelframe(T4AxOptgroup, text="AXIS LABEL SIZE", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T4F_AxLabSize, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
     AXLABELSIZE <- tclVar("1.2")
     T4_AxLabSize <- ttkcombobox(T4F_AxLabSize, width = 15, textvariable = AXLABELSIZE, values = FontSize)
     tkbind(T4_AxLabSize, "<<ComboboxSelected>>", function(){
                           Plot_Args$xlab$cex <<- tclvalue(AXLABELSIZE)
                           Plot_Args$ylab$cex <<- tclvalue(AXLABELSIZE)
                           CtrlPlot()
                    })
     tkgrid(T4_AxLabSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T4F_AxLabOrient <- ttklabelframe(T4AxOptgroup, text="AXIS LABEL ORIENTATION", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T4F_AxLabOrient, row = 3, column = 3, padx = 5, pady = 5, sticky="w")
     AXLABELORIENT <- tclVar("Horizontal")
     T4_AxLabOrient <- ttkcombobox(T4F_AxLabOrient, width = 15, textvariable = AXLABELORIENT, values = AxLabOrient)
     tkbind(T4_AxLabOrient, "<<ComboboxSelected>>", function(){
                           LabOrient <- tclvalue(AXLABELORIENT)
                           if (LabOrient == "Horizontal"){Plot_Args$scales$x$rot <<- Plot_Args$scales$y$rot <<- 0}
                           if (LabOrient == "Rot-20"){Plot_Args$scales$x$rot <<- Plot_Args$scales$y$rot <<- 20}
                           if (LabOrient == "Rot-45"){Plot_Args$scales$x$rot <<- Plot_Args$scales$y$rot <<- 45}
                           if (LabOrient == "Rot-70"){Plot_Args$scales$x$rot <<- Plot_Args$scales$y$rot <<- 70}
                           if (LabOrient == "Vertical"){Plot_Args$scales$x$rot <<- Plot_Args$scales$y$rot <<- 90}
                           if (LabOrient == "Parallel"){
                               Plot_Args$scales$x$rot <<- 0
                               Plot_Args$scales$y$rot <<- 90
                           }
                           if (LabOrient == "Normal"){
                               Plot_Args$scales$x$rot <<- 90
                               Plot_Args$scales$y$rot <<- 0
                           }
                           CtrlPlot()
                    })
     tkgrid(T4_AxLabOrient, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T4F_XAxNameChange <- ttklabelframe(T4AxOptgroup, text="CHANGE X-LABEL", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T4F_XAxNameChange, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
     XAXLABEL <- tclVar("New X label")
     T4_XAxNameChange <- ttkentry(T4F_XAxNameChange, textvariable=XAXLABEL, foreground="grey")
     tkbind(T4_XAxNameChange, "<FocusIn>", function(K){
                           tclvalue(XAXLABEL) <- ""
                           tkconfigure(T4_XAxNameChange, foreground="red")
                    })
     tkbind(T4_XAxNameChange, "<Key-Return>", function(K){
                           tkconfigure(T4_XAxNameChange, foreground="black")
                           if (tclvalue(XAXLABEL) == ""){return()}
                           if (PlotParameters$OverlayMode == "Single-Panel") {
                               Plot_Args$xlab$label <<- tclvalue(XAXLABEL)
                               CtrlPlot()
                           } else {
                              return()
                           }
                    })
     tkgrid(T4_XAxNameChange, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T4F_YAxNameChange <- ttklabelframe(T4AxOptgroup, text="CHANGE Y-LABEL", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T4F_YAxNameChange, row = 4, column = 2, padx = 5, pady = 5, sticky="w")
     YAXLABEL <- tclVar("New Y label")
     T4_YAxNameChange <- ttkentry(T4F_YAxNameChange, textvariable=YAXLABEL, foreground="grey")
     tkbind(T4_YAxNameChange, "<FocusIn>", function(K){
                           tclvalue(YAXLABEL) <- ""
                           tkconfigure(T4_YAxNameChange, foreground="red")
                    })
     tkbind(T4_YAxNameChange, "<Key-Return>", function(K){
                           tkconfigure(T4_YAxNameChange, foreground="black")
                           if (tclvalue(YAXLABEL) == ""){return()}
                           if (PlotParameters$OverlayMode == "Single-Panel") {
                               Plot_Args$ylab$label <<- tclvalue(YAXLABEL)
                               CtrlPlot()
                           } else {
                               return()
                           }
                    })
     tkgrid(T4_YAxNameChange, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T4F_ZAxNameChange <- ttklabelframe(T4AxOptgroup, text="CHANGE Z-LABEL", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T4F_ZAxNameChange, row = 4, column = 3, padx = 5, pady = 5, sticky="w")
     ZAXLABEL <- tclVar("New Z label")
     T4_ZAxNameChange <- ttkentry(T4F_ZAxNameChange, textvariable=ZAXLABEL, foreground="grey")
     tkbind(T4_ZAxNameChange, "<FocusIn>", function(K){
                           tclvalue(ZAXLABEL) <- ""
                           tkconfigure(T4_ZAxNameChange, foreground="red")
                    })
     tkbind(T4_ZAxNameChange, "<Key-Return>", function(K){
                           tkconfigure(T4_ZAxNameChange, foreground="black")
                           if (tclvalue(ZAXLABEL) == ""){return()}
                           if (PlotParameters$OverlayMode == "Single-Panel") {
                               Plot_Args$zlab$label <<- tclvalue(ZAXLABEL)
                               CtrlPlot()
                           } else {
                               return()
                           }
                    })
     tkgrid(T4_ZAxNameChange, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T4F_XStep <- ttklabelframe(T4AxOptgroup, text="CHANGE X-STEP", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T4F_XStep, row = 5, column = 1, padx = 5, pady = 5, sticky="w")
     XSTEP <- tclVar("Xstep")
     T4_XStep <- ttkentry(T4F_XStep, textvariable=XSTEP, foreground="grey")
     tkbind(T4_XStep, "<FocusIn>", function(K){
                           tclvalue(XSTEP) <- ""
                           tkconfigure(T4_XStep, foreground="red")
                    })
     tkbind(T4_XStep, "<Key-Return>", function(K){
                           tkconfigure(T4_XStep, foreground="black")
                           if (tclvalue(XSTEP) == ""){return()}
                           dx <- as.numeric(tclvalue(XSTEP))
                           Nticks <- as.integer((Xlim[2]-Xlim[1])/dx)
                           Plot_Args$scales$x$tick.number <<- Nticks
                           CtrlPlot()
                    })
     tkgrid(T4_XStep, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T4F_YStep <- ttklabelframe(T4AxOptgroup, text="CHANGE Y-STEP", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T4F_YStep, row = 5, column = 2, padx = 5, pady = 5, sticky="w")
     YSTEP <- tclVar("Ystep")
     T4_YStep <- ttkentry(T4F_YStep, textvariable=YSTEP, foreground="grey")
     tkbind(T4_YStep, "<FocusIn>", function(K){
                           tclvalue(YSTEP) <- ""
                           tkconfigure(T4_YStep, foreground="red")
                    })
     tkbind(T4_YStep, "<Key-Return>", function(K){
                           tkconfigure(T4_YStep, foreground="black")
                           if (tclvalue(YSTEP) == ""){return()}
                           dy <- as.numeric(tclvalue(YSTEP))
                           Nticks <- as.integer((Ylim[2]-Ylim[1])/dy)
                           Plot_Args$scales$y$tick.number <<- Nticks
                           CtrlPlot()
                    })
     tkgrid(T4_YStep, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T4F_XYrange <- ttklabelframe(T4group1, text="CHANGE X, Y RANGE", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T4F_XYrange, row = 6, column = 1, padx = 5, pady = 5, sticky="w")
     X1RANGE <- tclVar("Xmin=")
     xx1 <- ttkentry(T4F_XYrange, textvariable=X1RANGE, foreground="grey")
     tkbind(xx1, "<FocusIn>", function(K){
                           tclvalue(X1RANGE) <- ""
                           tkconfigure(xx1, foreground="red")
                    })
     tkbind(xx1, "<Key-Return>", function(K){
                           tkconfigure(xx1, foreground="black")
                    })
     tkgrid(xx1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     X2RANGE <- tclVar("Xmax=")
     xx2 <- ttkentry(T4F_XYrange, textvariable=X2RANGE, foreground="grey")
     tkbind(xx2, "<FocusIn>", function(K){
                           tclvalue(X2RANGE) <- ""
                           tkconfigure(xx2, foreground="red")
                    })
     tkbind(xx2, "<Key-Return>", function(K){
                           tkconfigure(xx2, foreground="black")
                    })
     tkgrid(xx2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

     Y1RANGE <- tclVar("Ymin=")
     yy1 <- ttkentry(T4F_XYrange, textvariable=Y1RANGE, foreground="grey")
     tkbind(yy1, "<FocusIn>", function(K){
                           tclvalue(Y1RANGE) <- ""
                           tkconfigure(yy1, foreground="red")
                    })
     tkbind(yy1, "<Key-Return>", function(K){
                           tkconfigure(yy1, foreground="black")
                    })
     tkgrid(yy1, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

     Y2RANGE <- tclVar("Ymax=")
     yy2 <- ttkentry(T4F_XYrange, textvariable=Y2RANGE, foreground="grey")
     tkbind(yy2, "<FocusIn>", function(K){
                           tclvalue(Y2RANGE) <- ""
                           tkconfigure(yy2, foreground="red")
                    })
     tkbind(yy2, "<Key-Return>", function(K){
                           tkconfigure(yy2, foreground="black")
                    })
     tkgrid(yy2, row = 1, column = 4, padx = 5, pady = 5, sticky="w")

     T4_OKBtn <- tkbutton(T4F_XYrange, text=" OK ", width=12, command=function(){
                           xx1 <- as.numeric(tclvalue(X1RANGE))
                           xx2 <- as.numeric(tclvalue(X2RANGE))
                           yy1 <- as.numeric(tclvalue(Y1RANGE))
                           yy2 <- as.numeric(tclvalue(Y2RANGE))
                           if (is.na(xx1*xx2*yy1*yy2)) {
                               tkmessageBox(message="ATTENTION: plase set all the xmin, xmax, ymin, ymax values!", title = "CHANGE X Y RANGE", icon = "error")
                               return()
                           } else {
                               if (FName[[SpectIndx]]@Flags) { #Binding energy set
                                   Plot_Args$xlim <<- Xlim <<- sort(c(xx1, xx2), decreasing=TRUE)
                                   Plot_Args$ylim <<- Ylim <<- sort(c(yy1, yy2))
                               } else {
                                   Plot_Args$xlim <<- Xlim <<- sort(c(xx1, xx2))
                                   Plot_Args$ylim <<- Ylim <<- sort(c(yy1, yy2))
                               }
                           }
                           CtrlPlot()
                    })
     tkgrid(T4_OKBtn, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

     T4_OKBtn <- tkbutton(T4F_XYrange, text=" RESET ", width=12, command=function(){
                           Plot_Args$xlim <<- NULL    #xlim set in XPSOverlayEngine
                           Plot_Args$ylim <<- NULL    #ylim set in XPSOverlayEngine
                           CtrlPlot()
                    })
     tkgrid(T4_OKBtn, row = 2, column = 2, padx = 5, pady = 5, sticky="w")


# --- TAB5 ---

### LEGEND SETTINGS

     T5group1 <- ttkframe(NB,  borderwidth=0, padding=c(0,0,0,0) )
     tkadd(NB, T5group1, text=" LEGEND ")

     T5F_legendCK <- ttklabelframe(T5group1, text="Enable Legend ON/OFF", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T5F_legendCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
     LEGENDCK <- tclVar(FALSE)
     T5_legendCK <- tkcheckbutton(T5F_legendCK, text="Enable Legend ON/OFF", variable=LEGENDCK, onvalue = 1, offvalue = 0,
                        command=function(){
                           Legends <- NULL
                           Legends <- SelectedNames$CoreLines
                           for(ii in seq_along(Legends)){
                              tmp <- unlist(strsplit(Legends[ii], "\\."))   #skip the number at beginning coreline name
                              Legends[ii] <- tmp[2]
                           }
                           AutoKey_Args$text <<- Legends  #load the Legends in the slot of the AutoKey_Args = List of parameters defining legend properties
                           if (tclvalue(LEGENDCK) == "1") {
		           	                 Plot_Args$auto.key <<- AutoKey_Args  #Save the AutoKey_Args list of par in Plot_Args$auto.key
                               if (tclvalue(SETLINES) == "ON") {
                                  Plot_Args$par.settings$superpose.line$col <<- Colors[1] #MonoChrome plot
                                  Plot_Args$par.settings$superpose.line$lty <<- Plot_Args$lty
                                  if (PlotParameters$OverlayMode == "Multi-Panel") {
                                     Plot_Args$par.settings$superpose.line$lty <<- "solid"
                                     Plot_Args$scales$relation <<- "free"
                                  }
 		           	                   if (tclvalue(BWCOL) == "RainBow") {                    #COLOR plot
                                     Plot_Args$par.settings$superpose.line$col <<- Colors
                                     Plot_Args$par.settings$superpose.line$lty <<- Plot_Args$lty
                                  }
                               }
                               if (tclvalue(SETSYMBOLS) == "ON") {   #selezionate SIMBOLI
                                  Plot_Args$par.settings$superpose.symbol$col <<- Colors[1]  #MonoChrome plot
                                  Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                  if (PlotParameters$OverlayMode == "Multi-Panel") {
                                     Plot_Args$par.settings$superpose.symbol$pch <<- 1
                                     Plot_Args$par.settings$superpose.symbol$col <<- Colors[1]
                                     Plot_Args$scales$relation <<- "free"
                                  }
 		           	                if (tclvalue(BWCOL) == "RainBow") {                       #COLOR plot
                                     Plot_Args$par.settings$superpose.symbol$col <<- Colors
                                     Plot_Args$par.settings$superpose.symbol$pch <<- 1
                                  }
                               }
                           } else {
		           	                 Plot_Args$auto.key <<- FALSE
	           	              }
                           SetLinesPoints()
                    })
     tkgrid(T5_legendCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T5F_LegFNameCK <- ttklabelframe(T5group1, text="Complete Legend", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T5F_LegFNameCK, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
     LEGNAMECK <- tclVar(FALSE)
     T5_LegFNameCK <- tkcheckbutton(T5F_LegFNameCK, text="Add XPSSamp Name", variable=LEGNAMECK, onvalue = 1, offvalue = 0,
                        command=function(){
                           if (is.logical(Plot_Args$auto.key)){
                               tkmessageBox(message="Please enable LEGENDS", icon="warning")
                               tclvalue(LEGNAMECK) <- "0"
                           } else {
                              Legends <- NULL
                              if (tclvalue(LEGNAMECK) == "1") {
                                  Legends <- SelectedNames$CoreLines
                                  for (ii in seq_along(SelectedNames$XPSSample)){
                                      tmp <- unlist(strsplit(Legends[ii], "\\."))  #skip the number at beginning coreline name
                                      Legends[ii] <- paste(tmp[2], "_", SelectedNames$XPSSample[ii], sep="")
                                  }
                                  AutoKey_Args$text <<- Legends
                              } else {
                                  Legends <- SelectedNames$CoreLines
                                  for(ii in seq_along(Legends)){
                                      tmp <- unlist(strsplit(Legends[ii], "\\."))   #skip the number at beginning coreline name
                                      Legends[ii] <- tmp[2]
                                  }
                                  AutoKey_Args$text <<- Legends
                              }
                           }
                           SetLinesPoints()
                    })
     tkgrid(T5_LegFNameCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T5F_LegPosCK <- ttklabelframe(T5group1, text="Legend Position", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T5F_LegPosCK, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
     LEGENDPOS <- tclVar("OutsideTop")
     T5_LegPosCK <- ttkcombobox(T5F_LegPosCK, width = 15, textvariable = LEGENDPOS, values = LegPos)
     tkbind(T5_LegPosCK, "<<ComboboxSelected>>", function(){
                           if (PlotParameters$OverlayMode=="Multi-Panel"||PlotParameters$OverlayMode=="TreD") {
                               tkmessageBox(message="WARNING: Legend position option NOT available for MULTIPANEL or 3D-Plot", title = "Legend Position",  icon = "warning")
                           } else {
	                              switch(tclvalue(LEGENDPOS),
                                  "OutsideTop" = { AutoKey_Args$space <<- "top" },
				                              "OutsideRight" = { AutoKey_Args$space <<- "right" },
				                              "OutsideLeft"  = { AutoKey_Args$space <<- "left" },
			                               "OutsideBottom" = { AutoKey_Args$space <<- "bottom" },
				                              "InsideTopRight" = { AutoKey_Args$space <<- NULL
                                                       AutoKey_Args$corner <<-c(1,1)
                                                       AutoKey_Args$x <<- 0.95
                                                       AutoKey_Args$y <<- 0.95 },
				                              "InsideTopLeft" =  { AutoKey_Args$space <<- NULL
                                                       AutoKey_Args$corner <<-c(0,1)
                                                       AutoKey_Args$x <<- 0.05
                                                       AutoKey_Args$y <<- 0.95 },
                                  "InsideBottomRight" = { AutoKey_Args$space <<- NULL
                                                       AutoKey_Args$corner <<-c(1,0)
                                                       AutoKey_Args$x <<- 0.95
                                                       AutoKey_Args$y <<- 0.05 },
				                              "InsideBottomLeft"  = {	AutoKey_Args$space <<- NULL
                                                       AutoKey_Args$corner <<-c(0,0)
                                                       AutoKey_Args$x <<- 0.05
                                                       AutoKey_Args$y <<- 0.05 },
                                )
                           }
                           CtrlPlot()
                    })
     tkgrid(T5_LegPosCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T5F_LegColNum <- ttklabelframe(T5group1, text="N. Legend Columns", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T5F_LegColNum, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
     LEGENCOLUMNS <- tclVar("N.Col=")
     T5_LegColNum <- ttkentry(T5F_LegColNum, textvariable=LEGENCOLUMNS, foreground="grey")
     tkbind(T5_LegColNum, "<FocusIn>", function(K){
                           tclvalue(LEGENCOLUMNS) <- ""
                           tkconfigure(T5_LegColNum, foreground="red")
                    })
     tkbind(T5_LegColNum, "<Key-Return>", function(K){
                           tkconfigure(T5_LegColNum, foreground="black")
                           Plot_Args$auto.key$columns <<- as.numeric(tclvalue(LEGENCOLUMNS))
                           CtrlPlot()
                    })
     tkgrid(T5_LegColNum, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T5F_LegSize <- ttklabelframe(T5group1, text="Text Size", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T5F_LegSize, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
     LEGENDSIZE <- tclVar("1")
     T5_LegSize <- ttkcombobox(T5F_LegSize, width = 15, textvariable = LEGENDSIZE, values = LegTxtSize)
     tkbind(T5_LegSize, "<<ComboboxSelected>>", function(){
		           	             Plot_Args$auto.key$cex <<- as.numeric(tclvalue(LEGENDSIZE))
                           AutoKey_Args$cex <<- as.numeric(tclvalue(LEGENDSIZE))
                           CtrlPlot()
                    })
     tkgrid(T5_LegSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T5F_LegDist <- ttklabelframe(T5group1, text="Distance from Margin", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T5F_LegDist, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
     LEGENDDIST <- tclVar("0.08")
     T5_LegDist <- ttkcombobox(T5F_LegDist, width = 15, textvariable = LEGENDDIST, values = LegDist)
     tkbind(T5_LegDist, "<<ComboboxSelected>>", function(){
                           if (PlotParameters$OverlayMode=="Multi-Panel" || PlotParameters$TreD==TRUE) {
                               tkmessageBox(message="WARNING: Legend position option Not available for MULTIPANEL or 3D-Plot", title = "Legend Position",  icon = "warning")
                               return()
                           } else {
                              Dist <- as.numeric(tclvalue(LEGENDDIST))
                              if (tclvalue(LEGENCOLUMNS) == "N.Col=" ) {
                                  Ncol <- 1
                              } else {
                                  Ncol <- as.numeric(tclvalue(LEGENCOLUMNS))
                              }
			                           switch(tclvalue(LEGENDPOS),
                              "OutsideTop"     = { AutoKey_Args$space <<- NULL
                                                   AutoKey_Args$corner <<- c(1,0)
                                                   AutoKey_Args$x <<- 1.1 - 1/(Ncol+1)
                                                   AutoKey_Args$y <<- 1+Dist },
				                          "OutsideRight"   = { AutoKey_Args$space <<- "right"
#                                                   Plot_Args$par.settings$layout.widths$right.padding <<- 8-Dist*40},
                                                   Plot_Args$par.settings$layout.widths$key.right <<- 1-10*Dist },
				                          "OutsideLeft"    = { AutoKey_Args$space <<- "left"
#                                                   Plot_Args$par.settings$layout.widths$left.padding <<- 8-Dist*40
                                                   Plot_Args$par.settings$layout.widths$key.left <<- Dist*10 },
			                           "OutsideBottom"  = { AutoKey_Args$space <<- NULL
                                                   AutoKey_Args$corner <<- c(0,0)
                                                   AutoKey_Args$x <<- 1.1 - 1/(Ncol+1)
                                                   AutoKey_Args$y <<- -1 - Dist },
				                          "InsideTopRight" = { AutoKey_Args$space <<- NULL
                                                   AutoKey_Args$corner <<- c(1,1)
                                                   AutoKey_Args$x <<- 1-Dist
                                                   AutoKey_Args$y <<- 1-Dist },
				                          "InsideTopLeft"  = { AutoKey_Args$space <<- NULL
                                                   AutoKey_Args$corner <<- c(0,1)
                                                   AutoKey_Args$x <<- Dist
                                                   AutoKey_Args$y <<- 1-Dist },
                              "InsideBottomRight" = { AutoKey_Args$space <<- NULL
                                                      AutoKey_Args$corner <<- c(1,0)
                                                      AutoKey_Args$x <<- 1-Dist
                                                      AutoKey_Args$y <<- Dist },
				                          "InsideBottomLeft"  = {	AutoKey_Args$space <<- NULL
                                                      AutoKey_Args$corner <<- c(0,0)
                                                      AutoKey_Args$x <<- 1.1 - 1/(Ncol+1)
                                                      AutoKey_Args$y <<- 1-Dist }                              )
                           }
                           CtrlPlot()
                    })
     tkgrid(T5_LegDist, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T5F_LegLineWdh <- ttklabelframe(T5group1, text="Line/Symbol weight", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T5F_LegLineWdh, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
     LEGLINWIDTH <- tclVar("1")
     T5F_LegLineWdh <- ttkcombobox(T5F_LegLineWdh, width = 15, textvariable = LEGLINWIDTH, values = LWidth)
     tkbind(T5F_LegLineWdh, "<<ComboboxSelected>>", function(){
                           weight <- as.numeric(tclvalue(LEGLINWIDTH))
                           if (tclvalue(SETLINES)=="ON") {   #Lines selected
                              Plot_Args$par.settings$superpose.line$lwd <<- weight
                           }
                           if (tclvalue(SETSYMBOLS)=="ON") {   #Symbol selected
                              Plot_Args$par.settings$superpose.symbol$cex <<- weight
                           }
                           CtrlPlot()
                    })
     tkgrid(T5F_LegLineWdh, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T5F_LegendCol <- ttklabelframe(T5group1, text="Legend text Color", borderwidth=2, padding=c(5,5,5,5))
     tkgrid(T5F_LegendCol, row = 4, column = 2, padx = 5, pady = 5, sticky="w")
     LEGTXTCOLOR <- tclVar("1")
     T5_LegendCol <- ttkcombobox(T5F_LegendCol, width = 15, textvariable = LEGTXTCOLOR, values = c("MonoChrome", "RainBow"))
     tkbind(T5_LegendCol, "<<ComboboxSelected>>", function(){
                           if (tclvalue(LEGTXTCOLOR) == "MonoChrome"){
                               AutoKey_Args$col <<- Colors[1]
                           } else {
                               AutoKey_Args$col <<- Colors
                           }
                           CtrlPlot()
                    })
     tkgrid(T5_LegendCol, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T5_ChngLegBtn <- tkbutton(T5group1, text=" Change Legend ", width=20, command=function(){
                           Legends <- ColNames <- RowNames <- NULL
                           LL <- length(SelectedNames$XPSSample)
                           Legends <- list(rep("?", LL))
                           Legends <- as.data.frame(Legends, stringsAsFactors=FALSE)
                           Title <- "CHANGE LEGENDS"
                           ColNames <- "LEGENDS"
                           for(ii in 1:LL){
                               txt <- paste("Leg. ", ii, sep="")
                               RowNames <- c(RowNames, txt)
                           }
                           Legends <- DFrameTable(Legends, Title=Title,
                                                            ColNames=ColNames, RowNames=RowNames,
                                                            Width=15, Modify=TRUE, Env=environment())
                           AutoKey_Args$text <<- unname(unlist(Legends))
                           CtrlPlot()
                    })
     tkgrid(T5_ChngLegBtn, row = 5, column = 1, padx = 5, pady = 5, sticky="w")

     T5_AnnotateBtn <- tkbutton(T5group1, text=" Annotate ", width=20, command=function(){
                           PlotParameters$Annotate <<- TRUE
                           CtrlPlot()
                           PlotParameters$Annotate <<- FALSE
                    })
     tkgrid(T5_AnnotateBtn, row = 5, column = 2, padx = 5, pady = 5, sticky="w")


#----- END NOTEBOOK -----

   WidgetState(objFunctFact, "disabled")
   WidgetState(T4_ZAxNameChange, "disabled")
   WidgetState(T4_PanelTitles, "disabled")

}
