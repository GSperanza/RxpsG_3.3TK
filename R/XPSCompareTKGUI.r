#XPSCompare() function to create plots in multi-panel mode
#'
#' @title XPSCompare is used to compare Core-Line spectra
#' @description XPSCompare provides a userfriendly interface to select
#'   the XPS-Core-Lines together with their best fit to be plotted in 
#'   individual panels. XPSCompare permits the selection of plotting options
#'   for a personalized data representation.
#'   This function is based on the (/code{Lattice}) Package.
#' @examples
#' \dontrun{
#' 	XPSCompare()
#' }
#' @export
#'


XPSCompare <- function(){

#---- calls the function to plot spectra folloowing option selection -----
   CtrlPlot <- function(){
            N.XS <- length(FNamesCoreLines$XPSSample)
            N.CL <- length(FNamesCoreLines$CoreLines)
            if (N.XS == 0) {
                tkmessageBox(message="Please select XPS Samples!", title="NO XPS SAMPLES SELECTED", icon="warning")
                return()
            }

            if (length(FNamesCoreLines$Ampli) == 0){
                N <- N.XS * N.CL
                FNamesCoreLines$Ampli <<- rep(1,N)
            }

            if (N.CL == 0) {
                tkmessageBox(message="Please select The Core Lines to plot!", title="NO CORE-LINES SELECTED", icon="warning")
                return()
            }
            if (length(PanelTitles) == 0) {
               for (ii in 1:N.CL){
                    PanelTitles <- c(PanelTitles, FNamesCoreLines$CoreLines[ii]) #List of Titles for the Multipanel
               }
            } else {
               PanelTitles <- FNamesCoreLines$CoreLines
            }
            
            Plot_Args$PanelTitles <<- PanelTitles
            Limits <- XPScompEngine(PlotParameters, Plot_Args, FNamesCoreLines, Xlim, Ylim)
   }


#--- Routine for drawing Custom Axis
   CustomAx <- function(CustomDta){
               axMin <- as.character(round(CustomDta[[1]], 2))
               axMax <- as.character(round(CustomDta[[2]], 2))
               axParam <- list(Data = c(axMin, axMax, "10"))
               axParam <- as.data.frame(axParam, stringsAsFactors=FALSE)
               Title <- "SET AXIS PARAMETERS"
               ColNames <- "PARAMETERS"
               RowNames <- c(paste("Xmin (min=", axMin, "): ", sep=""),
                             paste("Xmax (max=", axMax, "): ", sep=""),
                             "N. ticks")
               axParam <- DFrameTable(axParam, Title, ColNames=ColNames, RowNames=RowNames, Width=15, Env=environment())
               axParam <- as.numeric(unlist(axParam[[1]]))
               axMin <- axParam[1]     #X or Y scale min value
               axMax <- axParam[2]     #X or Y scale max value
               NTicks <- axParam[3]

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
         if (tclvalue(SetLines) == "OFF" && tclvalue(SetSymbols) == "OFF") {
            Plot_Args$type <<- "l"  #cancel line and symbols and legends
            AutoKey_Args$lines <<- FALSE
            AutoKey_Args$points <<- FALSE
            AutoKey_Args$col <<- rep("white", 20)
            PlotParameters$Colors <<- rep("white", 20)
            Plot_Args$lty <<- LType
            Plot_Args$par.settings$superpose.symbol$col <<- rep("white", 20)
            Plot_Args$par.settings$superpose.symbol$pch <<- rep(STypeIndx[1], 20)
            Plot_Args$par.settings$superpose.line$col <<- rep("white", 20)
         }

         if (tclvalue(SetLines) == "ON" && tclvalue(SetSymbols) == "OFF") {
            Plot_Args$type <<- "l"  # lines only
            AutoKey_Args$lines <<- TRUE
            AutoKey_Args$points <<- FALSE
            AutoKey_Args$col <<- Colors
            PlotParameters$Colors <<- Colors
            Plot_Args$lty <<- LType
            Plot_Args$par.settings$superpose.line$col <<- Colors #Rainbow plot
            Plot_Args$par.settings$superpose.line$lty <<- rep("solid", 20)
            if (tclvalue(SetLines) == "Patterns") {
                Plot_Args$par.settings$superpose.line$lty <<- LType
            }
            if (tclvalue(ColBW) == "B/W") {
               AutoKey_Args$col <<- rep("black", 20)
               PlotParameters$Colors <<- "black"
               Plot_Args$par.settings$superpose.line$col <<- rep("black", 20) #B/W plot
               Plot_Args$par.settings$superpose.line$lty <<- LType
            }
         }

         if (tclvalue(SetLines) == "OFF" && tclvalue(SetSymbols) == "ON") {
            Plot_Args$type <<- "p"  # symbols only
            AutoKey_Args$lines <<- FALSE
            AutoKey_Args$points <<- TRUE
            AutoKey_Args$col <<- Colors
            PlotParameters$Colors <<- Colors
            Plot_Args$pch <<- rep(STypeIndx[1], 20)
            Plot_Args$par.settings$superpose.symbol$pch <<- rep(STypeIndx[1], 20)
            Plot_Args$par.settings$superpose.symbol$col <<- Colors
            if (tclvalue(SetSymbols) == "multi-symbols") {
                Plot_Args$pch <<- STypeIndx
                Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
            }
            if (tclvalue(ColBW) == "B/W") {
               PlotParameters$Colors <<- "black"
               Plot_Args$pch <<- STypeIndx
               Plot_Args$par.settings$superpose.symbol$col <<- rep("black", 20)
               Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
               AutoKey_Args$col <<- rep("black", 20)
            }
         }

         if (tclvalue(SetLines) == "ON" && tclvalue(SetSymbols) == "ON") {
            Plot_Args$type <<- "b"  #both: line and symbols
            AutoKey_Args$lines <<- TRUE
            AutoKey_Args$points <<- TRUE
            Plot_Args$lty <<- LType
            Plot_Args$pch <<- STypeIndx
            if (tclvalue(ColBW)=="B/W") {
               AutoKey_Args$col <<- rep("black", 20)
               PlotParameters$Colors <<- "black"
               Plot_Args$par.settings$superpose.line$lty <<- LType
               Plot_Args$par.settings$superpose.line$col <<- rep("black", 20) #B/W plot
               Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
               Plot_Args$par.settings$superpose.symbol$col <<- rep("black", 20)
            } else {
               AutoKey_Args$col <<- Colors
               PlotParameters$Colors <<- Colors
               Plot_Args$par.settings$superpose.line$col <<- Colors #Rainbow plot
               Plot_Args$par.settings$superpose.line$lty <<- rep("solid", 20)
               if (tclvalue(SetLines) == "Patterns") {
                   Plot_Args$par.settings$superpose.line$lty <<- LType
               }
               Plot_Args$par.settings$superpose.symbol$col <<- Colors
               Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx[1]
               if (tclvalue(SetSymbols) == "multi-symbols") {
                   Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
               }
            }
         }
         CtrlPlot()
   }

   ChkCommCL <- function(){
       XpSamp <- get(FNamesCoreLines$XPSSample[1], envir=.GlobalEnv)
#       CommonCL <- gsub("[\\.0-9]+", "", CLlist[[1]])  #set the CL of the first XPSSample as a reference (names without numbers and dots
       CommonCL <- strsplit(CLlist[[1]], "\\.")         #splits CL_Symbol at the dot
       CommonCL <- unlist(lapply(CommonCL, function(x) x <- x[2])) #takes the Symbol, remove the number
       N.CL <- length(CommonCL)                        #N. Corelines in the reference XPS-Sample (first selected sample)
       N.XS <- length(FNamesCoreLines$XPSSample)       #Number of N. XPSSpectra selected
       RngX <- RngY <- list(CLName=NULL, min=NULL, max=NULL)
       for(jj in 1:N.CL){ #Range of CoreLines of XPSSample1
          RngX[[1]][jj] <- CommonCL[jj]
          RngX[[2]][jj] <- range(XpSamp[[CommonCL[jj]]]@.Data[1])[1] #MinX
          RngX[[3]][jj] <- range(XpSamp[[CommonCL[jj]]]@.Data[1])[2] #MaxX
          RngY[[1]][jj] <- CommonCL[jj]
          RngY[[2]][jj] <- range(XpSamp[[CommonCL[jj]]]@.Data[2])[1] #MinY
          RngY[[3]][jj] <- range(XpSamp[[CommonCL[jj]]]@.Data[2])[2] #MaxY
       }
#Here we load selected XPSSample to see which CoreLines contains.
#The X and Y range OK for all the common corelines of all XPSSamples
       if (N.XS > 1) {
           for (ii in 2:N.XS){ #the first XPSSamp is compared starting from the second XPSSamp
                XpSamp <- get(FNamesCoreLines$XPSSample[ii], envir=.GlobalEnv)
#                CoreLines <- gsub("[\\.0-9]+", "", CLlist[[ii]]) #set the CL of the next XPSSample to intersect
                CoreLines <- strsplit(CLlist[[ii]], "\\.")         #splits CL_Symbol at the dot
                CoreLines <- unlist(lapply(CoreLines, function(x) x <- x[2])) #takes the Symbol, remove the number
                CommonCL <- intersect(CommonCL, CoreLines)
                for (jj in N.CL:1){
                    #work out the X-range Y-range common to the selected Core Lines
                    RngXmin <- range(XpSamp[[CommonCL[jj]]]@.Data[1])[1]
                    RngXmax <- range(XpSamp[[CommonCL[jj]]]@.Data[1])[2]
                    if (RngXmin < RngX[[2]][jj]) {RngX[[2]][jj] <- RngXmin}
                    if (RngXmax > RngX[[3]][jj]) {RngX[[3]][jj] <- RngXmax}
                    RngYmin <- range(XpSamp[[CommonCL[jj]]]@.Data[2])[1]
                    RngYmax <- range(XpSamp[[CommonCL[jj]]]@.Data[2])[2]
                    if (RngYmin < RngY[[2]][jj]) {RngY[[2]][jj] <- RngYmin}
                    if (RngYmax > RngY[[3]][jj]) {RngY[[3]][jj] <- RngYmax}
                }
           }
       }
       FNamesCoreLines$CoreLines <<- CommonCL
       Xlim <<- RngX
       Ylim <<- RngY
   }

   CtrlRepCL <- function(ii){    #CTRL for repeated CL: search for core-lines with same name
       LL <- length(CLlist[[ii]])
       jj <- 1
       while(jj < LL){
           SpectName <- CLlist[[ii]][jj]
           SpectName <- unlist(strsplit(SpectName, "\\."))
           SpectName <- SpectName[2]
           Indx <- grep(SpectName, CLlist[[ii]])  #The selected CoreLine name could be in any position in the Destination XPSSample => source Samp Index could be different from Dest Samp index
           if (length(Indx) > 1){                 #The same coreline is present more than one time
               CLWindow <- tktoplevel()
               tkwm.title(CLWindow,"CORE LINE SELECTION")
               N.CL <- length(Indx)
               msg <- paste(" Found ", N.CL," ",SpectName, "spectra.\n Please select the coreline to compare")
               tkgrid(ttklabel(CLWindow, text=msg, font="Helvetica 12 normal"), row=1, column=1, padx=5, pady=5)
               tkgrid(ttkseparator(CLWindow, orient="horizontal"), row=2, column=1, padx=5, pady=5)
               CLGroup <- ttkframe(CLWindow, borderwidth=2, padding=c(5,5,5,5) )
               tkgrid(CLGroup, row=3, column=1, sticky="w")
               RCL <- tclVar()
               CLRadio <- NULL
               for(kk in Indx){
                   CLRadio <- ttkradiobutton(CLGroup, text=CLlist[[ii]][kk], variable=RCL, value=CLlist[[ii]][kk],
                              command=function(){
                                  zz <- grep(tclvalue(RCL), CLlist[[ii]][Indx])
                                  Indx <<- Indx[-zz] #in Indx remains the component to eliminiate
                                  CLlist[[ii]] <<- CLlist[[ii]][-Indx] #eliminate the repeated spectra
                                  LL <- length(CLlist[[ii]]) #update CLlist length
                              })
                   tkgrid(CLRadio, row=1, column=kk, padx=5, pady=5)
               }

               ExitBtn <- tkbutton(CLWindow, text="  OK  ", width=15, command=function(){
                                  tkdestroy(CLWindow)
                              })
               tkgrid(ExitBtn, row=4, column=1, padx=5, pady=5)

               tkwait.window(CLWindow)  #nothing else can be done while WinCL is active
           }                     #to allow selection/eliminaiton of repeated CL
           jj <- jj+1
       }
   }

#----- reset parameters to the initial values -----
   ResetPlot <- function(){
            LL <- length(FNameListTot)
            for(ii in 1:LL){
                tclvalue(FNameListTot[ii]) <- FALSE
            }
            CLlist <<- list()
            SelectedCL <<- FALSE
            childID <- tclvalue(tkwinfo("children", T1frameCLine)) #Remove CL_checkbox
            if (length(childID) > 0){
                sapply(unlist(strsplit(childID, " ")), function(x) {
                       tcl("grid", "remove", x)
                })
            }
            NamesList <<- list(XPSSample=NULL, CoreLines=NULL)
            FNamesCoreLines <<- list(XPSSample=NULL,CoreLines=NULL, Ampli=NULL)  #dummy lists to begin: NB each lcolumn contains 2 element otherwise error
            updateTable(widget=NameTable, items=list(FNamesCoreLines$XPSSample, FNamesCoreLines$CoreLines))
            tkconfigure (objCLineOff, values="")
            tkconfigure (objXSampAmpl, values="")
            tkconfigure (objCLineAmpl, values="")
            SaveSelection <<- TRUE
            sapply(FNameListTot, function(x) {tclvalue(x) <- FALSE}) #reset T1XSampListCK checkbuttons
            tclvalue(RevAx) <- TRUE #set reverse axis
            tclvalue(BEKE) <- FALSE #set BE units
            tclvalue(Nrm) <- FALSE #reset normalization
            tclvalue(Algn ) <- FALSE #reset alignment
            tkconfigure(objCLineOff, textvariable="")
            tkconfigure(objXSampAmpl, textvariable="")
            tkconfigure(objCLineAmpl, textvariable="")
            tkconfigure(CLineCustXY, values=" ")
            tclvalue(SF) <- "" #reset scale factor
            tkconfigure(objScaleFact, state="disabled")
            tclvalue(XOff) <- "" #reset x-offset
            tclvalue(YOff) <- "" #reset x-offset
            tclvalue(Xmn) <- "Xmin" #reset xmin
            tclvalue(Xmx) <- "Xmax" #reset xmax
            tclvalue(Ymn) <- "Ymin" #reset ymin
            tclvalue(Ymx) <- "Ymax" #reset ymax
            tclvalue(ColBW) <- "B/W" #set colors to B/W ymax
            tclvalue(GridOnOff) <- "Grid OFF" #set plot_grid to OFF
            tclvalue(SetLines ) <- "ON" #set lines ON
            tclvalue(SetSymbols) <- "OFF" #set symbols OFF
            tclvalue(LineType) <- "Patterns" #set symbols OFF
            tclvalue(LineWidth) <- "1" #set symbols OFF
            tclvalue(SymbolType) <- "Single-Symbol"
            tclvalue(SymbolSize) <- "0.8"
            tclvalue(StripColor) <- "" #color of the panel title_strip
            tclvalue(xScale) <- "Standard"
            tclvalue(yScale) <- "Standard"
            tclvalue(TitleSize) <- "1.4"
            tclvalue(AxisNumSize) <- "1"
            tclvalue(AxisLabelSize) <- "1"
            tclvalue(XAxNamChange) <- ""
            tclvalue(YAxNamChange) <- ""
            tclvalue(LegendOnOff ) <- "1" #sets legend OFF
            tclvalue(ColumnNumber) <- "" #sets number of legend columns to ""
            tclvalue(LegTxtSize) <- "" #sets number of legend text size ""
            tclvalue(LineWidthCK) <- "" #sets the legend linewidth to ""
            tclvalue(TxtColCK) <- "B/W" #sets the legend color to "B/W"

            XPSSettings <<- get("XPSSettings", envir=.GlobalEnv)
            Colors <<- XPSSettings$Colors
            LType <<- XPSSettings$LType
            SType <<- XPSSettings$Symbols
            STypeIndx <<- XPSSettings$SymIndx
            FitColors <<- c(XPSSettings$BaseColor[1], XPSSettings$ComponentsColor[1], XPSSettings$FitColor[1])
            CLPalette <<- data.frame(Colors=Colors, stringsAsFactors=FALSE)
            FitPalette <<- data.frame(FitColors=FitColors, stringsAsFactors=FALSE)

            PlotParameters <<- DefaultPlotParameters

            Plot_Args <<- list( x=formula("y ~ x"), data=NULL, PanelTitles=list(), groups=NULL,layout=NULL,
                                  xlim=NULL,ylim=NULL,
                                  pch=STypeIndx,cex=1,lty=LType,lwd=1,type="l",
                                  background="transparent",  col="black",
                                  main=list(label=NULL,cex=1.5),
                                  xlab=list(label=NULL, rot=0, cex=1.2),
                                  ylab=list(label=NULL, rot=90, cex=1.2),
                                  zlab=NULL,
                                  PanelTitles=NULL,
                                  scales=list(cex=1, tck=c(1,0), alternating=c(1), tick.number=NULL, relation="free",
                                              x=list(log=FALSE), y=list(log=FALSE), axs="i"),
                                  xscale.components = xscale.components.subticks,
                                  yscale.components = yscale.components.subticks,
                                  las=0,
                                  par.settings = list(superpose.symbol=list(pch=STypeIndx, fill="black"), #set the symbol fill color
                                        superpose.line=list(lty=LType, col="black"), #needed to set the legend colors
                                        par.strip.text=list(cex=1),
                                        strip.background=list(col="grey90") ),
                                  auto.key = TRUE,
                                  grid = FALSE
                             )

            AutoKey_Args <<- list(space="top",
                                  text=" ",
                                  cex = 1,
                                  type= "l",
                                  lines=TRUE,
                                  points=FALSE,
                                  col="black",
                                  columns=1,   #leggends organized in a column
                                  list(corner=NULL,x=NULL,y=NULL)
                             )
            Xlim <<- NULL #reset Xlim
            Ylim <<- NULL #reset Ylim
            plot.new()
   }


#----- Variables -----
   if (exists("activeFName", envir=.GlobalEnv)==FALSE){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FName <- get(activeFName, envir=.GlobalEnv)   #load the active FName
   ActiveFName <- get("activeFName", envir=.GlobalEnv)  #load the name of the active FNamw (string)
   SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv)#index of the active coreline
   SpectList <- XPSSpectList(ActiveFName)   #list of the active XPSSample core lines
   FNameListTot <- as.array(XPSFNameList())     #List of all XPSSamples loaded in the workspace
   LL <- NULL
#   jj <- 1
   FNamesCoreLines <- list(XPSSample=NULL, CoreLines=NULL, Ampli=NULL)
   NamesList <- list(XPSSample=NULL, CoreLines=NULL)
   CLlist <- list()
   SpectName <- ""
   SelectedCL <- FALSE


#--- list of graphical variables
   PatternList <- NULL
   FontSize <- c(0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)
   AxLabOrient <- c("Horizontal", "Rot-20", "Rot-45", "Rot-70", "Vertical", "Parallel")
   XPSSettings <- get("XPSSettings", envir=.GlobalEnv)
   Colors <- XPSSettings$Colors
   LType <- XPSSettings$LType
   SType <- XPSSettings$Symbols
   STypeIndx <- XPSSettings$SymIndx
   CLPalette <- as.matrix(Colors)
   CLPalette <- data.frame(Colors=CLPalette, stringsAsFactors=FALSE)
   FitColors <- as.matrix(c(XPSSettings$BaseColor[1], XPSSettings$ComponentsColor[1], XPSSettings$FitColor[1]))
   VarNames <- c("BasLnCol", "CompCol", "FitCol")
   FitPalette <- data.frame(Object=VarNames, Colors=FitColors, stringsAsFactors=FALSE)
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
   Xlim <- NULL
   Ylim <- NULL
   CLcolor <- list()

#--- general options
   PlotParameters <- list()
   PlotParameters$Align <- FALSE
   PlotParameters$RTFLtd <- FALSE #restrict plot to RTF region
   PlotParameters$Normalize <- NULL
   PlotParameters$Reverse <- TRUE #reversed X axes for Bind. Energy
   PlotParameters$SwitchE <- FALSE
   PlotParameters$XOffset <- list(CL=NA, Shift=0)
   PlotParameters$YOffset <- list(CL=NA, Shift=0)
   PlotParameters$ScaleFact <- list(XS=NA, CL=NA, ScFact=0)
   PlotParameters$CustomXY <- NULL
   PlotParameters$OverlayType <- "Compare.CoreLines" #Compare.Corelines  and  Multi-Panel are fixed options
   PlotParameters$OverlayMode <- "Multi-Panel"
   PlotParameters$Colors <- "B/W"
#--- legend options
   PlotParameters$Labels <- NULL
   PlotParameters$Legenda <- FALSE
   PlotParameters$LegPos <- "left"  #Out side left legend position
   PlotParameters$LegLineWdh <- 1
   PlotParameters$LegTxtCol <- "RainBow"
   PlotParameters$LegTxtSize <- 1
   PlotParameters$LegDist <- 0           

   DefaultPlotParameters <- PlotParameters

#--- comands for Lattice options
   Plot_Args <- list( x=formula("y ~ x"), data=NULL, PanelTitles=list(), groups=NULL,layout=NULL,
                    xlim=NULL, ylim=NULL,
                    pch=STypeIndx,cex=1,lty=LType,lwd=1,type="l",
                    background="transparent", col="black",
                    main=list(label=NULL,cex=1.5),
                    xlab=list(label=NULL, rot=0, cex=1.2),
                    ylab=list(label=NULL, rot=90, cex=1.2),
                    zlab=NULL,
                    PanelTitles=NULL,
                    scales=list(cex=1, tck=c(1,0), alternating=c(1), tick.number=NULL, relation="free",
                                x=list(log=FALSE), y=list(log=FALSE), axs="i"),
                    xscale.components = xscale.components.subticks,
                    yscale.components = yscale.components.subticks,
                    las=0,
                    par.settings = list(superpose.symbol=list(pch=STypeIndx,fill="black"), #set symbol filling color
                                        superpose.line=list(lty=LType, col="black"), #needed to set legend colors
                                        par.strip.text=list(cex=1),
                                        strip.background=list(col="grey90") ),
                    auto.key = TRUE,
                    grid = FALSE
                  )


   AutoKey_Args <- list( space="top",
                         text=" ",
                         cex = 1,
                         type= "l",
                         lines=TRUE,
                         points=FALSE,
                         col="black",
                         columns=1,  #legends organized in a column
                         list(corner=NULL,x=NULL,y=NULL)
                       )

   SaveSelection <- TRUE #at beginning force the control of the selection to TRUE to avoid error messages

#--- Reset graphical window
   plot.new()
   assign("MatPlotMode", FALSE, envir=.GlobalEnv)  #basic matplot function used to plot data


#===== NoteBook =====
   CompareWindow <- tktoplevel()
   tkwm.title(CompareWindow," COMPARE SPECTRA ")
   tkwm.geometry(CompareWindow, "+100+50")   #SCREEN POSITION from top-left corner

   MainGroup <- ttkframe(CompareWindow,  borderwidth = 5, padding = c(5,5,5,5))
   tkgrid(MainGroup, row = 1, column=1, sticky="w")

   NB <- ttknotebook(MainGroup)
   tkgrid(NB, row = 2, column = 1, padx = 5, pady = 5)


# --- TAB1 ---
#XPS Sample/Coreline selection
   T1group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
   tkadd(NB, T1group1, text=" XPS SAMPLE SELECTION ")

   T1frameInfoT1 <- ttklabelframe(T1group1, text = "INFO", borderwidth=2)
   tkgrid(T1frameInfoT1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   txt <- paste(" (1) Select the XPS.Sample to compare \n",
                "(2) Select the spectra to compare \n",
                "(3) Press the PLOT button", collapse="")
   tkgrid( ttklabel(T1frameInfoT1, text=txt), row = 1, column = 1, padx = 5, pady = 5, sticky="w")


   T1frameSelect <- ttkframe(T1group1, borderwidth=0, padding=c(0,0,0,0))
   tkgrid(T1frameSelect, row = 2, column = 1, padx = 0, pady = 0, sticky="w")
   
   T1frameButtT1 <- ttklabelframe(T1frameSelect, text = "PLOT", borderwidth=2)
   tkgrid(T1frameButtT1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   Button1 <- tkbutton(T1frameButtT1, text="PLOT", width=22, command=function(){
                           if (SelectedCL == FALSE){
                               tkmessageBox(message="Please Select the Core Lines to plot.", title="ERROR", icon="error")
                               return()
                           }
                           CtrlPlot() #plot selected XPS-SAmples

                     })
   tkgrid(Button1, row = 1, column = 1, padx = 10, pady = 5, sticky="w")

   Button2 <- tkbutton(T1frameButtT1, text="RESET PLOT", width=22, command=function(){
                           ResetPlot()
                     })
   tkgrid(Button2, row = 2, column = 1, padx = 10, pady = 5, sticky="w")

   Button3 <- tkbutton(T1frameButtT1, text="UPDATE XPSSAMPLE LIST", command=function(){
                           #Remove XS_checkbox
                           childID <- tclvalue(tkwinfo("children", T1frameFName))
                           if (length(childID) > 0){
                                sapply(unlist(strsplit(childID, " ")), function(x) {
                                              tcl("grid", "remove", x)
                                      })
                           }
                           #Remove CL_checkbox
                           childID <- tclvalue(tkwinfo("children", T1frameCLine)) #Remove XS_checkbox
                           if (length(childID) > 0){
                                sapply(unlist(strsplit(childID, " ")), function(x) {
                                              tcl("grid", "remove", x)
                                      })
                           }

                           #Regenerate the XS ccheckbox
                           FNameListTot <<- as.array(XPSFNameList())
                           for(ii in 1:length(FNameListTot)){
                               T1XSampListCK <- tkcheckbutton(T1frameFName, text=FNameListTot[ii], variable=FNameListTot[ii],
                                                   onvalue = FNameListTot[ii], offvalue = 0, command=function(){
                                                          for(jj in length(FNameListTot):1){
                                                              FNamesCoreLines$XPSSample[jj] <<- tclvalue(FNameListTot[jj])
                                                              if (FNamesCoreLines$XPSSample[jj] == "0") { FNamesCoreLines$XPSSample <<- FNamesCoreLines$XPSSample[-jj] }
                                                          }
                                                          if (length(FNamesCoreLines$XPSSample) == 0 || is.na(FNamesCoreLines$XPSSample)){
                                                              FNamesCoreLines$XPSSample <<- NULL
                                                              FNamesCoreLines$CoreLines <<- NULL
                                                              FNamesCoreLines$Ampli <<- NULL
                                                              Plot_Args$PanelTitles <<- list()
                                                              CLlist <<- list()
                                                              updateTable(widget=NameTable, items=list(FNamesCoreLines$XPSSample, FNamesCoreLines$CoreLines))
                                                          } else {

                                                              FNamesCoreLines$CoreLines <<- NULL
                                                              updateTable(widget=NameTable, items=list(FNamesCoreLines$XPSSample, FNamesCoreLines$CoreLines))
                                                              LL <- length(FNamesCoreLines$XPSSample)
                                                              if (LL > 3) {  #prepare for plotting the legends
                                                                  Plot_Args$auto.key$columns <<- 3
                                                                  AutoKey_Args$columns <<- 3
                                                              }
                                                              Plot_Args$auto.key$text <<- unlist(FNamesCoreLines$XPSSample)
                                                              #Define only the CLlist for the new selected XPSSample
                                                              CLlist[[LL]] <<- XPSSpectList(FNamesCoreLines$XPSSample[LL])

                                                              CtrlRepCL(LL)  #controls if same spectra are repeated in CLlist[[LL]]
                                                              ChkCommCL()   #check for corelines common to selected XPSSamples
                                                          }

                                                          tkconfigure(objXSampAmpl, values=FNamesCoreLines$XPSSample)
                                                          childID <- tclvalue(tkwinfo("children", T1frameCLine)) #Remove CL_checkbox
                                                          if (length(childID) > 0){
                                                              sapply(unlist(strsplit(childID, " ")), function(x) {
                                                                 tcl("grid", "remove", x)
                                                              })
                                                          }
                                                          CLines <- FNamesCoreLines$CoreLines
                                                          if (length(CLines) > 0){
                                                             for(jj in 1:length(CLines)){
                                                                T1CLineListCK <- tkcheckbutton(T1frameCLine, text=CLines[jj], variable=CLines[jj],
                                                                onvalue = CLines[jj], offvalue = 0, command=function(){
                                                                    for(ll in length(CLines):1){
                                                                       FNamesCoreLines$CoreLines[ll] <<- tclvalue(CLines[ll])
                                                                       if (FNamesCoreLines$CoreLines[ll] == "0") {
                                                                          SelectedCL <<- FALSE
                                                                          FNamesCoreLines$CoreLines <<- FNamesCoreLines$CoreLines[-ll]
                                                                          if (length(FNamesCoreLines$CoreLines) > 0) { SelectedCL <<- TRUE }
                                                                       }
                                                                    }
                                                                    tkconfigure(objCLineOff, values=FNamesCoreLines$CoreLines)
                                                                    tkconfigure(objCLineAmpl, values=FNamesCoreLines$CoreLines)
                                                                    tkconfigure(CLineCustXY, values=FNamesCoreLines$CoreLines)
                                                                    updateTable(widget=NameTable, items=list(FNamesCoreLines$XPSSample, FNamesCoreLines$CoreLines))
                                                                })
                                                                tclvalue(CLines[jj]) <- FALSE
                                                                tkgrid(T1CLineListCK, row = jj, column = 1, padx = 5, pady = 5, sticky="w")                                                               
                                                             }
                                                          }                                                          
                                                          tclvalue(SelXSampAmpl) <- ""
                                                          tkconfigure(objXSampAmpl, values=FNamesCoreLines$XPSSample)
                                                      })
                              tkgrid(T1XSampListCK, row = ii, column = 1, padx = 5, pady = 5, sticky="w")
                              tclvalue(FNameListTot[ii]) <- FALSE
                           }
                           LL <- length(FNameListTot)
                           tkconfigure(objCLineOff, values=" ")
                           tkconfigure(objXSampAmpl, values=FNamesCoreLines$XPSSample)  #reset objXSampAmpl
                           tkconfigure(objCLineAmpl, values=" ")
                           tkconfigure(CLineCustXY, values=" ")
                 })

   tkgrid(Button3, row = 3, column = 1, padx = 10, pady = 5, sticky="w")

   Button4 <- tkbutton(T1frameButtT1, text="EXIT", width=22, command=function(){
                           XPSSaveRetrieveBkp("save")
                           tkdestroy(CompareWindow)
                     })
   tkgrid(Button4, row = 4, column = 1, padx = 10, pady = 5, sticky="w")


   T1frameFName <- ttklabelframe(T1frameSelect, text = "SELECT XPS-SAMPLES", borderwidth=2)
   tkgrid(T1frameFName, row = 1, column = 2, padx = 10, pady = 5, sticky="w")

   btns <- NULL
   for(ii in 1:length(FNameListTot)){
       T1XSampListCK <- tkcheckbutton(T1frameFName, text=FNameListTot[ii], variable=FNameListTot[ii],
                        onvalue = FNameListTot[ii], offvalue = 0, command=function(){
                           for(jj in length(FNameListTot):1){
                               FNamesCoreLines$XPSSample[jj] <<- tclvalue(FNameListTot[jj])
                               if (FNamesCoreLines$XPSSample[jj] == "0") { FNamesCoreLines$XPSSample <<- FNamesCoreLines$XPSSample[-jj] }
                           }
                           if (length(FNamesCoreLines$XPSSample) == 0 || is.na(FNamesCoreLines$XPSSample)){
                               FNamesCoreLines$XPSSample <<- NULL
                               FNamesCoreLines$CoreLines <<- NULL
                               FNamesCoreLines$Ampli <<- NULL
                               Plot_Args$PanelTitles <<- list()
                               CLlist <<- list()
                               updateTable(widget=NameTable, items=list(FNamesCoreLines$XPSSample, FNamesCoreLines$CoreLines))
                           } else {
                               FNamesCoreLines$CoreLines <<- NULL
                               updateTable(widget=NameTable, items=list(FNamesCoreLines$XPSSample, FNamesCoreLines$CoreLines))
                               LL <- length(FNamesCoreLines$XPSSample)
                               if (LL > 3) {  #prepare for plotting the legends
                                   Plot_Args$auto.key$columns <<- 3
                                   AutoKey_Args$columns <<- 3
                               }
                               Plot_Args$auto.key$text <<- unlist(FNamesCoreLines$XPSSample)
                               #Define only the CLlist for the new selected XPSSample
                               CLlist[[LL]] <<- XPSSpectList(FNamesCoreLines$XPSSample[LL])

                               CtrlRepCL(LL)  #controls if same spectra are repeated in CLlist[[LL]]
                               ChkCommCL()   #check for corelines common to selected XPSSamples
                            }
                            tkconfigure(objXSampAmpl, values=FNamesCoreLines$XPSSample)
                            childID <- tclvalue(tkwinfo("children", T1frameCLine)) #Remove CL_checkbox
                            if (length(childID) > 0){
                                sapply(unlist(strsplit(childID, " ")), function(x) {
                                              tcl("grid", "remove", x)
                                      })
                            }

                            CLines <- FNamesCoreLines$CoreLines
                            if (length(CLines) > 0){
                               ##Common Core-Line Checkbox
                               for(jj in 1:length(CLines)){
                                   T1CLineListCK <- tkcheckbutton(T1frameCLine, text=CLines[jj], variable=CLines[jj],
                                                     onvalue = CLines[jj], offvalue = 0, command=function(){
                                                        for(ll in length(CLines):1){
                                                            FNamesCoreLines$CoreLines[ll] <<- tclvalue(CLines[ll])
                                                            if (FNamesCoreLines$CoreLines[ll] == "0") {
                                                                SelectedCL <<- FALSE
                                                                FNamesCoreLines$CoreLines <<- FNamesCoreLines$CoreLines[-ll]
                                                                if (length(FNamesCoreLines$CoreLines) > 0) { SelectedCL <<- TRUE }
                                                            }
                                                        }
                                                        tkconfigure(objCLineOff, values=FNamesCoreLines$CoreLines)
                                                        tkconfigure(objCLineAmpl, values=FNamesCoreLines$CoreLines)
                                                        tkconfigure(CLineCustXY, values=FNamesCoreLines$CoreLines)
                                                        updateTable(widget=NameTable, items=list(FNamesCoreLines$XPSSample, FNamesCoreLines$CoreLines))
                                                     })
                                   tclvalue(CLines[jj]) <- FALSE
                                   tkgrid(T1CLineListCK, row = jj, column = 1, padx = 5, pady = 5, sticky="w")
                               }
                            }
                            tclvalue(SelXSampAmpl) <- ""
                            tkconfigure(objXSampAmpl, values=FNamesCoreLines$XPSSample)

                       })
       tkgrid(T1XSampListCK, row = ii, column = 1, padx = 5, pady = 5, sticky="w")
       tclvalue(FNameListTot[ii]) <- FALSE
   }

   T1frameCLine <- ttklabelframe(T1frameSelect, text = "SELECT SPECTRA", borderwidth=2)
   tkgrid(T1frameCLine, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

   T1CLineListCK <- ttklabel(T1frameCLine, text="      \n      ")
   tkgrid(T1CLineListCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")  #just to add space into the T1frameCLine

   T1frameTab <- ttklabelframe(T1group1, text = "SELECTED XPS-SAMPLES AND COMMON CORE LINES", borderwidth=2)
   tkgrid(T1frameTab, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

   NameTable <- ttktreeview(T1frameTab,
                           columns = c("XPS Samples", "Core Lines"),
                           displaycolumns=c(0, 1),
                           show = "headings",
                           height = 5,
                           selectmode = "browse"
   )
   tcl(NameTable, "heading", 0, text="XPS Samples")
   tcl(NameTable, "column", 0, width=200)
   tcl(NameTable, "heading", 1, text="Core Lines")
   tcl(NameTable, "column", 1, width=200)
   tkgrid(NameTable, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkgrid.columnconfigure(T1frameTab, 1, weight=4)
   addScrollbars(T1frameTab, NameTable, type="y", Row=1, Col=1, Px=0, Py=0)
   updateTable(widget=NameTable, items=FNameListTot)


# --- TAB2 ---
#Plotting options
   T2group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
   tkadd(NB, T2group1, text=" PLOT FUNCTIONS ")

   FunctionGroup <- ttkframe(T2group1, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(FunctionGroup, row = 1, column = 1, padx = 0, pady = 5, sticky="w")

   T2frame1 <- ttklabelframe(FunctionGroup, text = "FUNCTIONS", borderwidth=2)
   tkgrid(T2frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

###Function Reverse X axis
   ChkBoxGroup <- ttkframe(T2frame1, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(ChkBoxGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")


   RevAx <- tclVar("")
   objFunctRev <- tkcheckbutton(ChkBoxGroup, text="Reverse X axis", variable=RevAx,
                        onvalue = 1, offvalue = 0, command=function(){
                           if (tclvalue(RevAx) == "1"){ PlotParameters$Reverse <<- TRUE }
                           if (tclvalue(RevAx) == "0"){ PlotParameters$Reverse <<- FALSE }
                           CtrlPlot() ####PLOT FOLLOWING SELECTIONS
                 })
   tclvalue(RevAx) <- TRUE
   tkgrid(objFunctRev, row = 1, column = 1, padx = 5, pady = 5, sticky="w")


###Function Switch Binding to Kinetic Energy scale
   BEKE <- tclVar("")
   objFunctSwitch <- tkcheckbutton(ChkBoxGroup, text="Switch BE to KE scale", variable=BEKE,
                        onvalue = 1, offvalue = 0, command=function(){
                           if (tclvalue(BEKE) == "1"){ PlotParameters$SwitchE <<- TRUE }
                           if (tclvalue(BEKE) == "0"){ PlotParameters$SwitchE <<- FALSE }
                           CtrlPlot() ####PLOT FOLLOWING SELECTIONS
                 })
   tclvalue(BEKE) <- FALSE
   tkgrid(objFunctSwitch, row = 1, column = 2, padx = 5, pady = 5, sticky="w")


###Normalize block
   Nrm <- tclVar("")
   objFunctNorm <- tkcheckbutton(ChkBoxGroup, text="Normalize", variable=Nrm,
                        onvalue = 1, offvalue = 0, command=function(){
                           if (tclvalue(Nrm) == "0") { PlotParameters$Normalize <<- NULL }
                           if (tclvalue(Nrm) == "1"){
#---------Normalizatino ToplevelWin -----------
                               CLMainWindow <- tktoplevel()
                               tkwm.geometry(CLMainWindow, "+200+150")   #SCREEN POSITION from top-left corner
                               tkwm.title(CLMainWindow,"SELECT CORE LINE")

                               CLMainGroup <- ttkframe(CLMainWindow, borderwidth=0, padding=c(5,5,5,5) )
                               tkgrid(CLMainGroup, row=1, column=1, sticky="w")

                               CLFrame <- ttklabelframe(CLMainGroup, text = "Select the Core-Lines to Normalize", borderwidth=2)
                               tkgrid(CLFrame, row = 2, column=1, pady=5, sticky="w")

                               Selected <- NULL
                               NCL <- length(FNamesCoreLines$CoreLines)
                               for(ii in 1:NCL) {
                                   SelCLChckBtn <- tkcheckbutton(CLFrame, text=FNamesCoreLines$CoreLines[ii], variable=FNamesCoreLines$CoreLines[ii],
                                                           onvalue=ii, offvalue = 0, command=function(){
                                                                Selected <<- sapply(FNamesCoreLines$CoreLines, function(x) as.numeric(tclvalue(x)))
                                                                Selected <<- unname(unlist(Selected))
                                                                for(ll in NCL:1){
                                                                    if (Selected[ll] == "0") Selected <<- Selected[-ll]
                                                                }
                                                   })
                                   tclvalue(FNamesCoreLines$CoreLines[ii]) <- FALSE   #initial cehckbutton setting
                                   tkgrid(SelCLChckBtn, row = 3, column = ii, pady=5, sticky="w")
                               }
                               SelButt <- tkbutton(CLMainGroup, text="SELECT", width=15, command=function(){
                                                   tkdestroy(CLMainWindow)
                                          })
                               tkgrid(SelButt, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
                               tkwait.window(CLMainWindow)
#---------Normalization ToplevelWin end--------

                               PlotParameters$Normalize <<- Selected #reads the selected CL to normalize
                               Plot_Args$ylab$label <<- "Intensity [a.u.]"
                               if (length(PlotParameters$Normalize) == 0) {
                                   PlotParameters$Normalize <<- NULL
                                   Plot_Args$ylab$label <<- "Intensity [cps]"
                               }
                    }
                    CtrlPlot() ####PLOT FOLLOWING SELECTIONS
                 })
   tclvalue(Nrm) <- FALSE
   tkgrid(objFunctNorm, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

###Function Y-Align to background
   Algn <- tclVar("")
   objFunctAlign <- tkcheckbutton(ChkBoxGroup, text="Align bkg to 0", variable=Algn,
                        onvalue = 1, offvalue = 0, command=function(){
                           if (tclvalue(Algn) == "1"){ PlotParameters$Align <<- TRUE }
                           if (tclvalue(Algn) == "0"){ PlotParameters$Align <<- FALSE }
                           CtrlPlot() ####PLOT FOLLOWING SELECTIONS
                 })
   tclvalue(Algn) <- FALSE
   tkgrid(objFunctAlign, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
   
###Functions X, Y offset
   OffsetGroup <- ttkframe(T2frame1, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(OffsetGroup, row = 3, column = 1, padx = 0, pady = 0, sticky="w")

   SelCLineOff <- tclVar("")
   objCLineOff <- ttkcombobox(OffsetGroup, width = 10, textvariable = SelCLineOff, values = c("  "))
   tkbind(objCLineOff, "<<ComboboxSelected>>", function(){
                           PlotParameters$XOffset$CL <- tclvalue(SelCLineOff)
                 })
   tkgrid(objCLineOff, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   tkgrid( ttklabel(OffsetGroup, text="Xoffset"),
                    row = 1, column = 2, padx = 5, pady = 5, sticky="w")
   XOff <- tclVar("")  #sets the initial msg
   XOffsetobj <- ttkentry(OffsetGroup, textvariable=XOff, width=5)
   tkbind(XOffsetobj, "<FocusIn>", function(K){
                           tkconfigure(XOffsetobj, foreground="red")
                 })
   tkbind(XOffsetobj, "<Key-Return>", function(K){
                           tkconfigure(XOffsetobj, foreground="black")
                           CL <- tclvalue(SelCLineOff) #reads selection from
                           CL <- grep(CL, FNamesCoreLines$CoreLines)
                           if (length(CL) == 0){
                               tkmessageBox(message="Please select the Core-Line first!", title="WARNING", icon="warning")
                               return()
                           }
                           PlotParameters$XOffset$CL <<- as.integer(CL)
                           PlotParameters$XOffset$Shift <<- as.numeric(tclvalue(XOff))
                           if (is.na(PlotParameters$XOffset)){
                               tkmessageBox(message="Please enter a numeric value for the X-shift", title="WARNING", icon="warning")
                               PlotParameters$XOffset$Shift <<- 0
                               return()
                           }
                           CtrlPlot()
                     })
   tkgrid(XOffsetobj, row = 1, column = 3,  padx = 5, pady = 5, sticky="w")

   tkgrid( ttklabel(OffsetGroup, text="Yoffset"),
                    row = 1, column = 4, padx = 5, pady = 5, sticky="w")
   YOff <- tclVar("")  #sets the initial msg
   YOffsetobj <- ttkentry(OffsetGroup, textvariable=YOff, width=5)
   tkgrid(YOffsetobj, row = 1, column = 5, padx = 5, pady = 5, sticky="w")
   tkbind(YOffsetobj, "<FocusIn>", function(K){
                           tkconfigure(YOffsetobj, foreground="red")
                     })
   tkbind(YOffsetobj, "<Key-Return>", function(K){
                           tkconfigure(YOffsetobj, foreground="black")
                           CL <- tclvalue(SelCLineOff) #reads selection from CL
                           CL <- grep(CL, FNamesCoreLines$CoreLines)
                           if (length(CL)== 0){
                               tkmessageBox(message="Please select the XPSSample and the Core-Line", 
                                             title="WARNING", icon=", warning")
                               return() 
                           }
                           PlotParameters$YOffset$CL <<- as.integer(CL) 
                           PlotParameters$YOffset$Shift <<- as.numeric(tclvalue(YOff)) 
                           if (is.na(PlotParameters$YOffset)){
                               tkmessageBox(message="Please enter a numeric value for the Y-shift", 
                                            title="WARNING", icon="warning") 
                               PlotParameters$YOffset$Shift <<- 0
                               return() 
                           }
                           CtrlPlot() 
                     })

###Amplify block
   T2frame2 <- ttklabelframe(FunctionGroup, text = "AMPLIFY", borderwidth=2)
   tkgrid(T2frame2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   tkgrid(ttklabel(T2frame2, text="XPS Sample"),
          row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   SelXSampAmpl <- tclVar("")
   objXSampAmpl <- ttkcombobox(T2frame2, width = 15, textvariable = SelXSampAmpl, values = FNameListTot)
   tkbind(objXSampAmpl, "<<ComboboxSelected>>", function(){
                        if(tclvalue(SelCLineAmpl) != ""){
                           tkconfigure(objScaleFact, state="normal")
                        }
                 })
   tkgrid(objXSampAmpl, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   tkgrid(ttklabel(T2frame2, text="Core-Line  "),
          row = 2, column = 1, padx = 5, pady = 5, sticky="w")

   SelCLineAmpl <- tclVar("")
   objCLineAmpl <- ttkcombobox(T2frame2, width = 15, textvariable = SelCLineAmpl, values = c("  "))
   tkbind(objCLineAmpl, "<<ComboboxSelected>>", function(){
                           if(tclvalue(SelXSampAmpl) != ""){
                              tkconfigure(objScaleFact, state="normal")
                           }
                 })
   tkgrid(objCLineAmpl, row = 2, column = 2, padx = 5, pady = 5, sticky="w")

   tkgrid( ttklabel(T2frame2, text="ScaleFact."),
                    row = 3, column = 1, padx = 5, pady = 5, sticky="w")

   SF <- tclVar("")  #sets the initial msg
   objScaleFact <- ttkentry(T2frame2, textvariable=SF, width=8)
   tkgrid(objScaleFact, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
   tkbind(objScaleFact, "<FocusIn>", function(K){
                           tkconfigure(objScaleFact, foreground="red")
                     })
   tkbind(objScaleFact, "<Key-Return>", function(K){
                           tkconfigure(objScaleFact, foreground="black")
                           XS <- tclvalue(SelXSampAmpl) #reads XPSSample selection
                           CL <- tclvalue(SelCLineAmpl) #reads CoreLine selection
                           if (XS == 0 || CL == 0){
                               tkmessageBox(message="Please select the XPSSample and the Core-Line", title="WARNING", icon="warning")
                               return()
                           }
                           XS <- grep(XS, FNamesCoreLines$XPSSample)
                           CL <- grep(CL, FNamesCoreLines$CoreLines)
                           SF <- as.numeric(tclvalue(SF))
                           PlotParameters$ScaleFact$XS <<- as.integer(XS)
                           PlotParameters$ScaleFact$CL <<- as.integer(CL)
                           PlotParameters$ScaleFact$ScFact <<- SF
                           CtrlPlot()
                     })
   tkconfigure(objScaleFact, state="disabled")

###Funct8: Custom XY scale
   T2frame3 <- ttklabelframe(T2group1, text="EXACT X, Y RANGE", borderwidth=2, padding=c(5,5,5,5) )
   tkgrid(T2frame3, row = 4, column = 1, padx=5, pady=5, sticky="w")

   tkgrid( ttklabel(T2frame3, text="Select the Spectrum"),
           row = 1, column = 1, padx = 5, pady = 2, sticky="w")
   CLCustXY <- tclVar()        
   CLineCustXY <- ttkcombobox(T2frame3, width = 15, textvariable = CLCustXY, values = "  ")
   tkgrid(CLineCustXY, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

   T2XYgroup <- ttkframe(T2frame3, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(T2XYgroup, row = 3, column = 1, padx = 0, pady = 0, sticky="w")

   Xmn <- tclVar("Xmin ")  #sets the initial msg
   XX1 <- ttkentry(T2XYgroup, textvariable=Xmn, width=15, foreground="grey")
   tkgrid(XX1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(XX1, "<FocusIn>", function(K){
                          tkconfigure(XX1, foreground="red")
                          tclvalue(Xmn) <- ""
                      })
   tkbind(XX1, "<Key-Return>", function(K){
                          tkconfigure(XX1, foreground="black")
                          OO <- as.numeric(tclvalue(Xmn))
                      })

   Xmx <- tclVar("Xmax ")  #sets the initial msg
   XX2 <- ttkentry(T2XYgroup, textvariable=Xmx, width=15, foreground="grey")
   tkgrid(XX2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
   tkbind(XX2, "<FocusIn>", function(K){
                          tkconfigure(XX2, foreground="red")
                          tclvalue(Xmx) <- ""
                      })
   tkbind(XX2, "<Key-Return>", function(K){
                          tkconfigure(XX2, foreground="black")
                          OO <- as.numeric(tclvalue(Xmx))
                      })

   Ymn <- tclVar("Ymin ")  #sets the initial msg
   YY1 <- ttkentry(T2XYgroup, textvariable=Ymn, width=15, foreground="grey")
   tkgrid(YY1, row = 1, column = 3, padx = 5, pady = 5, sticky="w")
   tkbind(YY1, "<FocusIn>", function(K){
                          tkconfigure(YY1, foreground="red")
                          tclvalue(Ymn) <- ""
                      })
   tkbind(YY1, "<Key-Return>", function(K){
                          tkconfigure(YY1, foreground="black")
                          OO <- as.numeric(tclvalue(Ymn))
                      })

   Ymx <- tclVar("Ymax ")  #sets the initial msg
   YY2 <- ttkentry(T2XYgroup, textvariable=Ymx, width=15, foreground="grey")
   tkgrid(YY2, row = 1, column = 4, padx = 5, pady = 5, sticky="w")
   tkbind(YY2, "<FocusIn>", function(K){
                          tkconfigure(YY2, foreground="red")
                          tclvalue(Ymx) <- ""
                      })
   tkbind(YY2, "<Key-Return>", function(K){
                          tkconfigure(YY2, foreground="black")
                          OO <- as.numeric(tclvalue(Ymx))
                      })

   OKButt <- tkbutton(T2XYgroup, text="OK", width=15, command=function(h){
                   CL <- tclvalue(CLCustXY)
                   CL <- grep(CL, FNamesCoreLines$CoreLines)

                   x1 <- as.numeric(tclvalue(Xmn))
                   x2 <- as.numeric(tclvalue(Xmx))
                   y1 <- as.numeric(tclvalue(Ymn))
                   y2 <- as.numeric(tclvalue(Ymx))
                   if (is.na(x1*x2*y1*y2)) {
                       tkmessageBox(message="ATTENTION: plase set all the xmin, xmax, ymin, ymax values!", title = "CHANGE X Y RANGE", icon = "error")
                   }
                   PlotParameters$CustomXY <<- c(CL, x1, x2, y1, y2)
                   CtrlPlot() 
                 })
   tkgrid(OKButt, row = 2, column = 1, padx=5, pady=10, sticky="w")

   ResetButt2 <- tkbutton(T2XYgroup, text="RESET PLOT", width=15,  command=function(){
                   ResetPlot()
                   CtrlPlot()
                 })
   tkgrid(ResetButt2, row = 2, column = 2, padx=5, pady=10, sticky="w")

   ExitButt2 <- tkbutton(T2XYgroup, text="EXIT", width=15, command=function(){
				               XPSSaveRetrieveBkp("save")
                   tkdestroy(CompareWindow)
                 })
   tkgrid(ExitButt2, row = 2, column = 3, padx=5, pady=10, sticky="w")




# --- TAB3 ---
# Rendering options
   T3group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
   tkadd(NB, T3group1, text=" RENDERING ")

   T3frame1 <- ttklabelframe(T3group1, text = "SET PALETTE", borderwidth=2)
   tkgrid(T3frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   tkgrid( ttklabel(T3frame1, text="Double click to change color"),
           row = 1, column = 1, padx = 5, pady = 0)

   T3frame11 <- ttklabelframe(T3frame1, text = "", borderwidth=10)
   tkgrid(T3frame11, row = 2, column = 1, padx = 5, pady = 0, sticky="w")

   for(ii in 1:10){
       CLcolor[[ii]] <- ttklabel(T3frame11, text=as.character(ii), width=6, font="Serif 8", background=Colors[ii])
       tkgrid(CLcolor[[ii]], row = ii, column=1, padx = c(10,0), pady = 1, sticky="w")
       tkbind(CLcolor[[ii]], "<Double-1>", function( ){
                    X <- as.numeric(tkwinfo("pointerx", CompareWindow)) #coordinates of the mouse pointer referred to the root window
                    Y <- tkwinfo("pointery", CompareWindow)
                    WW <- Y <- tkwinfo("containing", X, Y) #WW is the widget containinf the X,Y coords
                    BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                    BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                    colIdx <- grep(BKGcolor, Colors) #retrieve the index of the clicked tktext()
                    BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                    Colors[colIdx] <<- BKGcolor
                    tkconfigure(CLcolor[[colIdx]], background=Colors[colIdx])
                    PlotParameters$Colors <<- Colors
                    CtrlPlot()
                 })
       jj <- ii+10
       CLcolor[[jj]] <- ttklabel(T3frame11, text=as.character(jj), width=6, font="Serif 8", background=Colors[jj])
       tkgrid(CLcolor[[jj]], row = ii, column=2, padx = 15, pady = 1, sticky="w")
       tkbind(CLcolor[[jj]], "<Double-1>", function(){
                    X <- as.numeric(tkwinfo("pointerx", CompareWindow))
                    Y <- tkwinfo("pointery", CompareWindow)
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

   ColorButt <- tkbutton(T3frame1, text="Save Colors as Default", command=function(){
                           XPSSettings$Colors <- Colors
                           ColNames <- names(XPSSettings)
                           Ini.pthName <- system.file("extdata/XPSSettings.ini", package="RxpsG")
                           write.table(XPSSettings, file = Ini.pthName, sep=" ",
                                       eol="\n", row.names=FALSE, col.names=ColNames)
                           assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                 })
   tkgrid(ColorButt, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

   T3frame2 <- ttklabelframe(T3group1, text = "PLOT OPTIONS", borderwidth=2)
   tkgrid(T3frame2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   T3F_BW_Col <- ttklabelframe(T3frame2, text = "COLOR", borderwidth=2)
   tkgrid(T3F_BW_Col, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   ColBW <- tclVar("B/W")
   T3_BW_Col <- ttkcombobox(T3F_BW_Col, width = 15, textvariable = ColBW, values = c("B/W", "RainBow"))
   tkbind(T3_BW_Col, "<<ComboboxSelected>>", function(){
                             Plot_Args$type <<- "l"
                             if(tclvalue(SetSymbols) == "ON") {Plot_Args$type <<- "b"}
                             if(tclvalue(ColBW)=="B/W") {
                                tclvalue(SetLines) <- "ON"
                                tclvalue(LineType) <- "Patterns"
                                tclvalue(SymbolType) <- "Multi-Symbols"
                                tclvalue(TxtColCK) <- "B/W"
                                PlotParameters$Colors <<- "black"
                                Plot_Args$lty <<- rep("solid", 20)
                                Plot_Args$pch <<- STypeIndx
                                Plot_Args$cex <<- as.numeric(tclvalue(SymbolSize))
                                Plot_Args$par.settings$superpose.symbol$col <<- "black"
                                Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                Plot_Args$par.settings$superpose.line$col <<- "black"
                                Plot_Args$par.settings$superpose.line$lty <<- rep("solid", 20)
                                Plot_Args$par.settings$strip.background$col <<- "grey90"
                                if(tclvalue(LineType) == "Patterns"){
                                   Plot_Args$lty <<- LType
                                   Plot_Args$par.settings$superpose.line$lty <<- LType
                                }
                                AutoKey_Args$col <<- "black"
                             } else {
                                tclvalue(TxtColCK) <- "RainBow"
                                Plot_Args$lty <<- rep("solid", 20)
                                Plot_Args$pch <<- rep(STypeIndx[1], 20)
                                Plot_Args$cex <<- as.numeric(tclvalue(SymbolSize))
                                PlotParameters$Colors <<- Colors
                                Plot_Args$par.settings$superpose.line$col <<- Colors
                                Plot_Args$par.settings$superpose.line$lty <<- rep("solid", 20)
                                Plot_Args$par.settings$superpose.symbol$col <<- Colors
                                Plot_Args$par.settings$superpose.symbol$pch <<- rep(STypeIndx[1], 20)
                                Plot_Args$par.settings$strip.background$col <<- "lightskyblue1"
                                AutoKey_Args$col <<- Colors
                                if(tclvalue(LineType) == "Patterns"){
                                   Plot_Args$lty <<- LType
                                   Plot_Args$par.settings$superpose.line$lty <<- LType
                                }
                                if(tclvalue(SymbolType) == "Multi-Symbols"){
                                   Plot_Args$pch <<- STypeIndx
                                   Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                }
                             }
                             CtrlPlot()
                 })
   tkgrid(T3_BW_Col, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T3F_Grid <- ttklabelframe(T3frame2, text = "GRID", borderwidth=2)
   tkgrid(T3F_Grid, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
   GridOnOff <- tclVar("Grid OFF")
   T3_Grid <- ttkcombobox(T3F_Grid, width = 15, textvariable = GridOnOff, values = c("Grid ON", "Grid OFF"))
   tkbind(T3_Grid, "<<ComboboxSelected>>", function(){
                             if(tclvalue(GridOnOff) == "Grid ON") {
                                Plot_Args$grid <<- TRUE
                             } else {
                                Plot_Args$grid <<- FALSE
                             }
                             CtrlPlot()
                 })
   tkgrid(T3_Grid, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T3F_SetLines <- ttklabelframe(T3frame2, text = "SET LINES", borderwidth=2)
   tkgrid(T3F_SetLines, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
   choices <- c("ON", "OFF")
   SetLines <- tclVar("ON")
   for(ii in 1:2){
       T3_SetLines <- ttkradiobutton(T3F_SetLines, text=choices[ii], variable=SetLines,
                                 value=choices[ii], command = function(){
                             SetLinesPoints()
                 })
       tkgrid(T3_SetLines, row = 1, column = ii, padx = 5, pady = 5, sticky = "w")
   }

   T3F_SetSymbols <- ttklabelframe(T3frame2, text = "SET SYMBOLS", borderwidth=2)
   tkgrid(T3F_SetSymbols, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
   SetSymbols <- tclVar("OFF")
   for(ii in 1:2){
       T3_SetSymbols <- ttkradiobutton(T3F_SetSymbols, text=choices[ii], variable=SetSymbols,
                                 value=choices[ii], command = function(){
                             SetLinesPoints()
                 })
       tkgrid(T3_SetSymbols, row = 1, column = ii, padx = 5, pady = 5, sticky = "w")
   }

   T3F_LineType <- ttklabelframe(T3frame2, text = "LINE TYPE", borderwidth=2)
   tkgrid(T3F_LineType, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
   LineType <- tclVar("Patterns")
   T3_LineType <- ttkcombobox(T3F_LineType, width = 15, textvariable = LineType, values = c("Solid", "Patterns"))
   tkbind(T3_LineType, "<<ComboboxSelected>>", function(){
                             Plot_Args$type <<- "l"
                             if(tclvalue(SetSymbols) == "ON") {Plot_Args$type <<- "b"}
                             palette <- tclvalue(ColBW )

                             if (tclvalue(LineType) == "Solid") {
                                tclvalue(ColBW) <- "RainBow"
                                Plot_Args$lty <<- rep("solid", 20)
                                Plot_Args$pch <<- rep(STypeIndx[1], 20)
                                Plot_Args$cex <<- as.numeric(tclvalue(SymbolSize))
                                PlotParameters$Colors <<- Colors
                                Plot_Args$par.settings$superpose.line$col <<- Colors
                                Plot_Args$par.settings$superpose.line$lty <<- rep("solid", 20)
                                Plot_Args$par.settings$superpose.symbol$col <<- Colors
                                Plot_Args$par.settings$superpose.symbol$pch <<- rep(STypeIndx[1], 20)
                                Plot_Args$par.settings$strip.background$col <<- "lightskyblue"
                                AutoKey_Args$col <<- Colors
                                if (tclvalue(SetSymbols) == "multi-symbols"){
                                    Plot_Args$pch <<- STypeIndx
                                    Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                }
                             }
                             if (tclvalue(LineType)=="Patterns") {
                                ColStyle <- tclvalue(ColBW)
                                PlotParameters$Colors <<- "black"
                                Plot_Args$lty <<- LType
                                Plot_Args$pch <<- rep(STypeIndx[1], 20)
                                Plot_Args$cex <<- as.numeric(tclvalue(SymbolSize))
                                Plot_Args$par.settings$superpose.line$col <<- rep("black", 20)
                                Plot_Args$par.settings$superpose.line$lty <<- LType
                                Plot_Args$par.settings$superpose.symbol$col <<- rep("black", 20)
                                Plot_Args$par.settings$superpose.symbol$pch <<- rep(STypeIndx[1], 20)
                                Plot_Args$par.settings$strip.background$col <<- "gray90"
                                AutoKey_Args$col <<- rep("black", 20)
                                if (ColStyle == "RainBow"){
                                    PlotParameters$Colors <<- Colors
                                    Plot_Args$par.settings$superpose.symbol$col <<- Colors
                                    Plot_Args$par.settings$superpose.line$col <<- Colors
                                    Plot_Args$par.settings$strip.background$col <<- "lightskyblue"
                                    AutoKey_Args$col <<- Colors
                                }
                                if (tclvalue(SymbolType) == "Multi-Symbols"){
                                    Plot_Args$pch <<- STypeIndx
                                    Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                }
                             }
                             CtrlPlot()
                 })
   tkgrid(T3_LineType, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T3F_LinWidth <- ttklabelframe(T3frame2, text = "LINE WIDTH", borderwidth=2)
   tkgrid(T3F_LinWidth, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
   LineWidth <- tclVar("1")
   T3_LinWidth <- ttkcombobox(T3F_LinWidth, width = 15, textvariable = LineWidth, values = LWidth)
   tkbind(T3_LinWidth, "<<ComboboxSelected>>", function(){
                              Plot_Args$lwd <<- as.numeric(tclvalue(LineWidth))
                              CtrlPlot()
                 })
   tkgrid(T3_LinWidth, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T3F_SymType <- ttklabelframe(T3frame2, text = "SYMBOLS", borderwidth=2)
   tkgrid(T3F_SymType, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
   SymbolType <- tclVar("Multi-Symbols")
   T3_SymType<- ttkcombobox(T3F_SymType, width = 15, textvariable = SymbolType, values = c("Single-Symbol", "Multi-Symbols"))
   tkbind(T3_SymType, "<<ComboboxSelected>>", function(){
                             Plot_Args$type <<- "p"
                             if(tclvalue(SetLines) == "ON") {Plot_Args$type <<- "b"}
                             if (tclvalue(SymbolType) == "Single-Symbol") {
                                tclvalue(ColBW) <- "RainBow"
                                Plot_Args$lty <<- rep("solid", 20)
                                Plot_Args$pch <<- rep(STypeIndx[1], 20)
                                Plot_Args$cex <<- as.numeric(tclvalue(SymbolSize))
                                PlotParameters$Colors <<- Colors
                                Plot_Args$par.settings$superpose.line$col <<- Colors
                                Plot_Args$par.settings$superpose.line$lty <<- rep("solid", 20)
                                Plot_Args$par.settings$superpose.symbol$col <<- Colors
                                Plot_Args$par.settings$superpose.symbol$pch <<- rep(STypeIndx[1], 20)
                                Plot_Args$par.settings$strip.background$col <<- "lightskyblue"
                                AutoKey_Args$col <<- Colors
                                if(tclvalue(LineType) == "Patterns"){
                                   Plot_Args$lty <<- LType
                                   Plot_Args$par.settings$superpose.line$lty <<- LType
                                }
                             }
                             if (tclvalue(SymbolType) == "Multi-Symbols") {
                                PlotParameters$Colors <<- "black"
                                Plot_Args$lty <<- rep("solid", 20)
                                Plot_Args$pch <<- STypeIndx
                                Plot_Args$cex <<- as.numeric(tclvalue(SymbolSize))
                                Plot_Args$par.settings$superpose.line$col <<- rep("black", 20)
                                Plot_Args$par.settings$superpose.line$lty <<- rep("solid", 20)
                                Plot_Args$par.settings$superpose.symbol$col <<- rep("black", 20)
                                Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                Plot_Args$par.settings$strip.background$col <<- "gray90"
                                AutoKey_Args$col <<- rep("black", 20)
                                ColStyle <- tclvalue(ColBW)
                                if (ColStyle == "RainBow"){
                                    PlotParameters$Colors <<- Colors
                                    Plot_Args$par.settings$superpose.symbol$col <<- Colors
                                    Plot_Args$par.settings$superpose.line$col <<- Colors
                                    Plot_Args$par.settings$strip.background$col <<- "lightskyblue"
                                    AutoKey_Args$col <<- Colors
                                }
                                if(tclvalue(LineType) == "Patterns"){
                                   Plot_Args$lty <<- LType
                                   Plot_Args$par.settings$superpose.line$lty <<- LType
                                }
                             }
                             CtrlPlot()
                 })
   tkgrid(T3_SymType, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T3F_SymSize <- ttklabelframe(T3frame2, text = "SYMSIZE", borderwidth=2)
   tkgrid(T3F_SymSize, row = 4, column = 2, padx = 5, pady = 5, sticky="w")
   SymbolSize <- tclVar("0.8")
   T3_SymSize <- ttkcombobox(T3F_SymSize, width = 15, textvariable = SymbolSize, values = SymSize)
   tkbind(T3_SymSize, "<<ComboboxSelected>>", function(){
                              Plot_Args$cex <<- as.numeric(tclvalue(SymbolSize))
                              CtrlPlot()
                 })
   tkgrid(T3_SymSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T3F_PanStripCol <- ttklabelframe(T3frame2, text = "PANEL STRIP COLOR", borderwidth=2)
   tkgrid(T3F_PanStripCol, row = 5, column = 1, padx = 5, pady = 5, sticky="w")
   StripColor <- tclVar()
   T3_PanStripCol <- ttkcombobox(T3F_PanStripCol, width = 15, textvariable = StripColor, values = c("white","grey",
                              "darkgrey","lightblue","blue","darkblue","deepskyblue","lightbeige",
                              "beige","darkbeige","lightpink","pink","darkpink","lightgreen","green",
                              "darkgreen"))
   tkbind(T3_PanStripCol, "<<ComboboxSelected>>", function(){
                             StripCol <- tclvalue(StripColor)
                             if(StripCol=="grey")               { StripCol <- "grey90"
                             } else if (StripCol=="darkgrey")   { StripCol <- "gray60"

                             } else if (StripCol=="lightblue")  { StripCol <- "lightskyblue1"
                             } else if(StripCol=="blue")        { StripCol <- "lightskyblue3"
                             } else if(StripCol=="darkblue")    { StripCol <- "steelblue3"

                             } else if (StripCol=="lightbeige") { StripCol <- "beige"
                             } else if(StripCol=="beige")       { StripCol <- "bisque2"
                             } else if(StripCol=="darkbeige")   { StripCol <- "navajowhite4"

                             } else if (StripCol=="pink")       { StripCol <- "lightpink2"
                             } else if(StripCol=="darkpink")    { StripCol <- "lightpink4"

                             } else if (StripCol=="lightgreen") { StripCol <- "darkseagreen1"
                             } else if(StripCol=="green")       { StripCol <- "darkseagreen2"
                             } else if(StripCol=="darkgreen")   { StripCol <- "mediumseagreen"
                             }
                             Plot_Args$par.settings$strip.background$col <<- StripCol
                             CtrlPlot() 
                 })
   tkgrid(T3_PanStripCol, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   ResetButt3 <- tkbutton(T3group1, text=" RESET PLOT ", width=15, command=function(){
                             ResetPlot()
                             CtrlPlot()
                 })
   tkgrid(ResetButt3, row = 6, column = 1, padx = 5, pady = 5, sticky="w")

   ExitButt3 <- tkbutton(T3group1, text=" EXIT ", width=15, command=function(){
				                         XPSSaveRetrieveBkp("save")
                             tkdestroy(CompareWindow)
                 })
   tkgrid(ExitButt3, row = 6, column = 2, padx = 5, pady = 5, sticky="w")
   

# --- TAB4 ---
# Axis Rendering options
   T4group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
   tkadd(NB, T4group1, text=" AXES ")

   T4F_XScale <- ttklabelframe(T4group1, text = "X SCALE", borderwidth=2)
   tkgrid(T4F_XScale, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   ScaleTypes <- c("Standard", "Power", "Log.10", "Log.e", "Y*10^n", "Ye+0n")
   xScale <- tclVar("Standard")
   T4_XScale <- ttkcombobox(T4F_XScale, width = 15, textvariable = xScale, values = ScaleTypes)
   tkbind(T4_XScale, "<<ComboboxSelected>>", function(){
                             idx <- charmatch(tclvalue(yScale),ScaleTypes) #charmatch does not interporet math symbols
                             if (idx == 1) {
                                Plot_Args$scales$x$log <<- FALSE
                                Plot_Args$xscale.components <<- xscale.components.subticks
                             } else if (idx == 2) {
                                Plot_Args$scales$x$log <<- 10    # 10^ power scale
                                Plot_Args$xscale.components <<- xscale.components.logpower
                             } else if (idx == 3) {
                                Plot_Args$scales$x$log <<- 10    # log10 scale
                                Plot_Args$xscale.components <<- xscale.components.log10ticks
                             } else if (idx == 4) {
                                Plot_Args$scales$x$log <<- "e"   # log e scale
                                Plot_Args$xscale.components <<- xscale.components.subticks
                             } else if (idx == 5) {
                                Plot_Args$scales$x$log <<- "Xpow10"
                                Plot_Args$scales$x$rot <<- 0
                                Plot_Args$scales$y$rot <<- 90
                             } else if (idx == 6) {
                                Plot_Args$scales$x$log <<- "Xe+0n"
                                Plot_Args$scales$x$rot <<- 0
                                Plot_Args$scales$y$rot <<- 90
                             }
                             CtrlPlot()
                 })
   tkgrid(T4_XScale, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T4F_YScale <- ttklabelframe(T4group1, text = "Y SCALE", borderwidth=2)
   tkgrid(T4F_YScale, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
   ScaleTypes <- c("Standard", "Power", "Log.10", "Log.e", "Y*10^n", "Ye+0n")
   yScale <- tclVar("Standard")
   T4_YScale <- ttkcombobox(T4F_YScale, width = 15, textvariable = yScale, values = ScaleTypes)
   tkbind(T4_YScale, "<<ComboboxSelected>>", function(){
                             idx <- charmatch(tclvalue(yScale),ScaleTypes)
cat("\n Scales", tclvalue(yScale), idx)
                             if (idx == 1) {
                                Plot_Args$scales$y$log <<- FALSE
                                Plot_Args$yscale.components <<- yscale.components.subticks
                             } else if (idx == 2) {
                                Plot_Args$scales$y$log <<- 10
                                Plot_Args$yscale.components <<- yscale.components.logpower
                             } else if (idx == 3) {
                                Plot_Args$scales$y$log <<- 10
                                Plot_Args$yscale.components <<- yscale.components.log10ticks
                             } else if (idx == 4) {
                                Plot_Args$scales$y$log <<- "e"
                                Plot_Args$yscale.components <<- yscale.components.subticks
                             } else if (idx == 5) {
                                Plot_Args$scales$y$log <<- "Ypow10"
                                Plot_Args$scales$x$rot <<- 0
                                Plot_Args$scales$y$rot <<- 90
                             } else if (idx == 6) {
                                Plot_Args$scales$y$log <<- "Ye+0n"
                                Plot_Args$scales$x$rot <<- 0
                                Plot_Args$scales$y$rot <<- 90
                             }
                             CtrlPlot()
                 })
   tkgrid(T4_YScale, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T4F_XStep <- ttklabelframe(T4group1, text = "X STEP", borderwidth=2)
   tkgrid(T4F_XStep, row = 1, column = 3, padx = 5, pady = 5, sticky="w")
   T4_XStep <- tkbutton(T4F_XStep, text="Custom X ticks", width = 15, command=function(){
                             RngXmin <- round(unlist(Xlim[[2]]), digits=0) #I need identify XRange components using the CoreLine names
                             RngXmax <- round(unlist(Xlim[[3]]), digits=0)
                             names(RngXmin) <- Xlim[[1]]
                             names(RngXmax) <- Xlim[[1]]
                             NCL <- length(FNamesCoreLines$CoreLines)
                             Col2 <- rep("?", NCL)
                             Tick.Increment <- list(C1=FNamesCoreLines$CoreLines,
                                                    C2=Col2)
                             Tick.Increment <- as.data.frame(Tick.Increment, stringsAsFactors=FALSE)
                             Tick.Increment <- DFrameTable(Data="Tick.Increment", Title="SET X TICK INCREMENT",
                                                    ColNames=c("Core-Lines", "Increment"), RowNames="",
                                                    Width=18, Env=environment())
                             Tick.Increment <- as.numeric(unlist(Tick.Increment[2])) #first element of Tick.Increment is the CL-names
                             x_at <- list()
                             x_labels <- list()
                             for(ii in 1:NCL){
                                 if (is.na(Tick.Increment[ii])){
                                    tkmessageBox(message="Missing Major-Tick Step. Please Enter Values for all Spectra", title="ERROR", icon="error")
                                    return()
                                 }
                                 x_at[[ii]] <- seq(from=RngXmin[[FNamesCoreLines$CoreLines[ii]]],
                                                   to=RngXmax[[FNamesCoreLines$CoreLines[ii]]],
                                                   by=Tick.Increment[ii])
                                 x_labels[[ii]] <- as.character(x_at[[ii]])
                             }
                             Plot_Args$scales$x <<- list(at = x_at, labels = x_labels, log=FALSE)
                             CtrlPlot()
                 })
   tkgrid(T4_XStep, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T4F_TitSize <- ttklabelframe(T4group1, text = "TITLE SIZE", borderwidth=2)
   tkgrid(T4F_TitSize, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
   TitleSize <- tclVar("1.4")
   T4_TitSize <- ttkcombobox(T4F_TitSize, width = 15, textvariable = TitleSize, values = FontSize)
   tkbind(T4_TitSize, "<<ComboboxSelected>>", function(){
                             if (PlotParameters$OverlayMode=="Single-Panel" || PlotParameters$OverlayMode=="TreD") {
                                 Plot_Args$main$cex <<- as.numeric(tclvalue(TitleSize))
                             } else if (PlotParameters$OverlayMode=="Multi-Panel") {
                                 Plot_Args$par.strip.text$cex <<- as.numeric(tclvalue(TitleSize))
                             }
                             CtrlPlot()
                 })
   tkgrid(T4_TitSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T4F_PanelTitles <- ttklabelframe(T4group1, text = "CHANGE PANEL TITLES", borderwidth=2)
   tkgrid(T4F_PanelTitles, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
   T4_PanelTitles <- tkbutton(T4F_PanelTitles, text="Change Titles", width = 15, command=function(){
#                                  TitleMainWindow <- tktoplevel()
#                                  tkwm.title(TitleMainWindow,"EDIT TITLES")
#                                  tkwm.geometry(TitleMainWindow, "+300+300")   #SCREEN POSITION from top-left corner
#                                  TitleMainGroup <- ttkframe(TitleMainWindow, borderwidth=2, padding=c(5,5,5,5) )
#                                  tkgrid(TitleMainGroup, row=1, column=1, pady=5, sticky="w")
#                                  tkgrid(tklabel(TitleMainGroup, text="  EDIT TITLES "),
#                                         row=2, column=1, pady=5, sticky="w")
                             PanelTitles <- as.data.frame(FNamesCoreLines$CoreLines)
                             Plot_Args$PanelTitles <<- DFrameTable(Data=PanelTitles, Title="EDIT TITLES",
                                                           ColNames="Titles", RowNames="", Width=20, Env=environment())
                 })
   tkgrid(T4_PanelTitles, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T4F_YStep <- ttklabelframe(T4group1, text = "Y STEP", borderwidth=2)
   tkgrid(T4F_YStep, row = 2, column = 3, padx = 5, pady = 5, sticky="w")
   T4_YStep <- tkbutton(T4F_YStep, text="Custom Y ticks", width = 15, command=function(){
                             RngYmin <- round(unlist(Ylim[[2]]), digits=0) #I need identify XRange components using the CoreLine names
                             RngYmax <- round(unlist(Ylim[[3]]), digits=0)
                             names(RngYmin) <- Ylim[[1]]
                             names(RngYmax) <- Ylim[[1]]
                             NCL <- length(FNamesCoreLines$CoreLines)
                             Col2 <- rep("?", NCL)
                             Tick.Increment <- list(C1=FNamesCoreLines$CoreLines,
                                                    C2=Col2)
                             Tick.Increment <- as.data.frame(Tick.Increment, stringsAsFactors=FALSE)
                             Tick.Increment <- DFrameTable(Data="Tick.Increment", Title="SET Y TICK INCREMENT",
                                                    ColNames=c("Core-Lines", "Increment"), RowNames="",
                                                    Width=18, Env=environment())
                             Tick.Increment <- as.numeric(unlist(Tick.Increment[2])) #first element of Tick.Increment is the CL-names
                             y_at <- list()
                             y_labels <- list()
                             for(ii in 1:NCL){  #reshape the Y limits using rounded values
                                 if (is.na(Tick.Increment[ii])){
                                    tkmessageBox(message="Missing Major-Tick Step. Please Enter Values for all Spectra", title="ERROR", icon="error")
                                    return()
                                 }
                                 CL <- FNamesCoreLines$CoreLines[ii]
                                 Ndigit <- nchar(RngYmin[[CL]])
                                 if (Ndigit == 1){
                                     Y1 <- 0
                                 } else {
                                     Y1 <- as.integer(RngYmin[[CL]]/(10^(Ndigit-1)))* 10^(Ndigit-1)
                                 }
                                 Ndigit <- nchar(RngYmax[[CL]])
                                 Y2 <- as.integer(RngYmax[[CL]]/(10^(Ndigit-2)))* 10^(Ndigit-2)+10^(Ndigit-2)
                                 y_at[[ii]] <- seq(from=Y1, to=Y2, by=Tick.Increment[ii])
                                 y_labels[[ii]] <- as.character(y_at[[ii]])
                                 if (Ndigit > 5){
                                     y_labels[[ii]] <- as.character(sprintf("%1.1e",y_at[[ii]]))
                                 }
                             }
                             Plot_Args$scales$y <<- list(at = y_at, labels = y_labels, log=FALSE)
                             CtrlPlot()
                 })
   tkgrid(T4_YStep, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T4F_AxNumSize <- ttklabelframe(T4group1, text = "AXIS NUMBER SIZE", borderwidth=2)
   tkgrid(T4F_AxNumSize, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
   AxisNumSize <- tclVar("1")
   T4_AxNumSize <- ttkcombobox(T4F_AxNumSize, width = 15, textvariable = AxisNumSize, values = FontSize)
   tkbind(T4_AxNumSize, "<<ComboboxSelected>>", function(){
                             Plot_Args$scales$cex <<- tclvalue(AxisNumSize)
                             CtrlPlot() 
                 })
   tkgrid(T4_AxNumSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T4F_AxLabSize <- ttklabelframe(T4group1, text = "AXIS LABEL SIZE", borderwidth=2)
   tkgrid(T4F_AxLabSize, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
   AxisLabelSize <- tclVar("1")
   T4_AxLabSize <- ttkcombobox(T4F_AxLabSize, width = 15, textvariable = AxisLabelSize, values = FontSize)
   tkbind(T4_AxLabSize, "<<ComboboxSelected>>", function(){
                             Plot_Args$xlab$cex <<- tclvalue(AxisLabelSize)
                             Plot_Args$ylab$cex <<- tclvalue(AxisLabelSize)
                             CtrlPlot()
                 })
   tkgrid(T4_AxLabSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T4F_AxLabOrient <- ttklabelframe(T4group1, text = "AXIS NUMBER ORIENTATION", borderwidth=2)
   tkgrid(T4F_AxLabOrient, row = 3, column = 3, padx = 5, pady = 5, sticky="w")

   AxisLabelOrient <- tclVar()
   T4_AxLabOrient <- ttkcombobox(T4F_AxLabOrient, width = 15, textvariable = AxisLabelOrient, values = AxLabOrient)
   tkbind(T4_AxLabOrient, "<<ComboboxSelected>>", function(){
                             LabOrient <- tclvalue(AxisLabelOrient)
                             #reset scales to the default
                             Plot_Args$scales <<- list(cex=1, tck=c(1,0), alternating=c(1), tick.number=NULL, relation="free",
                                                   x=list(log=FALSE), y=list(log=FALSE), axs="i")
                             if (LabOrient == "Horizontal"){Plot_Args$scales$rot <<- 0}
                             if (LabOrient == "Rot-20"){Plot_Args$scales$rot <<- 20}
                             if (LabOrient == "Rot-45"){Plot_Args$scales$rot <<- 45}
                             if (LabOrient == "Rot-70"){Plot_Args$scales$rot <<- 70}
                             if (LabOrient == "Vertical"){Plot_Args$scales$rot <<- 90}
                             if (LabOrient == "Parallel"){
                                 Plot_Args$scales$relation <<- "free"  #needed to set x, y rot values
                                 Plot_Args$scales$x$rot <<- 0
                                 Plot_Args$scales$y$rot <<- 90
                             }
                             CtrlPlot()
                 })
   tkgrid(T4_AxLabOrient, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T4F_XAxNameFrame <- ttklabelframe(T4group1, text = "CHANGE X-LABEL", borderwidth=2)
   tkgrid(T4F_XAxNameFrame, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

   XAxNamChange <- tclVar()
   T4_XAxNameChange <- ttkentry(T4F_XAxNameFrame, textvariable=XAxNamChange)
   tkgrid(T4_XAxNameChange, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(T4_XAxNameChange, "<FocusIn>", function(K){
                             tkconfigure(T4_XAxNameChange, foreground="red")
                 })
   tkbind(T4_XAxNameChange, "<Key-Return>", function(K){
                             tkconfigure(T4_XAxNameChange, foreground="black")
                             if(tclvalue(XAxNamChange)==""){return()}
                             Plot_Args$xlab$label <<- tclvalue(XAxNamChange) 
                             CtrlPlot()
                 })

   T4F_YAxNameFrame <- ttklabelframe(T4group1, text = "CHANGE Y-LABEL", borderwidth=2)
   tkgrid(T4F_YAxNameFrame, row = 4, column = 2, padx = 5, pady = 5, sticky="w")

   YAxNamChange <- tclVar()
   T4_YAxNameChange <- ttkentry(T4F_YAxNameFrame, textvariable=YAxNamChange)
   tkgrid(T4_YAxNameChange, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(T4_YAxNameChange, "<FocusIn>", function(K){
                             tkconfigure(T4_YAxNameChange, foreground="red")
                 })
   tkbind(T4_YAxNameChange, "<Key-Return>", function(K){
                             tkconfigure(T4_YAxNameChange, foreground="black")
                             if(tclvalue(YAxNamChange)=="") {return()}
                             Plot_Args$ylab$label <<- tclvalue(YAxNamChange) # in 2D Y is the vertical axis
                             CtrlPlot()     
                 })


   ResetButt4 <- tkbutton(T4group1, text=" RESET PLOT ", width=15, command=function(){
                             ResetPlot()
                             CtrlPlot()
                 })
   tkgrid(ResetButt4, row = 5, column = 1, padx = 5, pady = 5, sticky="w")

   ExitButt4 <- tkbutton(T4group1, text=" EXIT ", width=15, command=function(){
				                         XPSSaveRetrieveBkp("save")
                             tkdestroy(CompareWindow)
                 })
   tkgrid(ExitButt4, row = 5, column = 2, padx = 5, pady = 5, sticky="w")


# --- TAB5 ---
### LEGEND SETTINGS
   T5group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
   tkadd(NB, T5group1, text=" LEGEND ")

   T5F_legendCK <- ttklabelframe(T5group1, text = "ENABLE LEGEND", borderwidth=2)
   tkgrid(T5F_legendCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   LegendOnOff <- tclVar("1")
   T5_legendCK <- tkcheckbutton(T5F_legendCK, text="Enable Legend ON/OFF", variable=LegendOnOff, 
                             onvalue = 1, offvalue = 0, command=function(){
                             AutoKey_Args$text <<- unlist(FNamesCoreLines$XPSSample)  #load the Legends in the slot of the AutoKey_Args = List of parameters defining legend properties
	                	           Plot_Args$auto.key <<- AutoKey_Args #Save the AutoKey_Args list of par in Plot_Args$auto.key
                             if (tclvalue(LegendOnOff)==1) {
                                 if (tclvalue(SetLines)=="ON") {    #selezionate LINEE
                                     Plot_Args$par.settings$superpose.line$col <<- "black" #B/W plot
                                     Plot_Args$par.settings$superpose.line$lty <<- LType
                                     Plot_Args$scales$relation <<- "free"
     		           	                  if (tclvalue(ColBW)=="RainBow") {                    #COLOR plot
                                         Plot_Args$par.settings$superpose.line$col <<- Colors
                                     }
                                 }
                                 if (tclvalue(SetSymbols)=="ON") {   #selezionate SIMBOLI
                                     Plot_Args$par.settings$superpose.symbol$col <<- "black"  #B/W plot
                                     Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                     Plot_Args$par.settings$superpose.symbol$pch <<- 1
                                     Plot_Args$scales$relation <<- "free"
 		           	                      if (tclvalue(ColBW)=="RainBow") {                    #COLOR plot
                                         Plot_Args$par.settings$superpose.symbol$col <<- Colors
                                     }
                                 }
                             } else {
		           	                   Plot_Args$auto.key <<- FALSE
	           	                }
                             CtrlPlot()
                 })
   tkgrid(T5_legendCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T5F_ChangLeg <- ttklabelframe(T5group1, text = "MODIFY THE LEGEND TEXT", borderwidth=2)
   tkgrid(T5F_ChangLeg, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
   T5_ChangLeg <- tkbutton(T5F_ChangLeg, text=" Change Legend ", width=15, command=function(){
                             NXS <- length(FNamesCoreLines$XPSSample)
                             Legends <- list(XPSSamples=FNamesCoreLines$XPSSample, legends=rep("-", LL))
                             Legends <- as.data.frame(Legends, stringsAsFactors=FALSE)
                             Legends <- DFrameTable(Data="Legends", Title="XPS SAMPLE LEGENDS",
                                                    ColNames=c("Core-Lines", "Legends"), RowNames="",
                                                    Width=18, Env=environment())
                             Legends <- unlist(Legends[2]) #first element of Legends is the XS-names
                             Plot_Args$auto.key$text <<- Legends
                             CtrlPlot()
                 })
   tkgrid(T5_ChangLeg, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T5F_TSizeCK <- ttklabelframe(T5group1, text = "TEXT SIZE", borderwidth=2)
   tkgrid(T5F_TSizeCK, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
   LegTxtSize <- tclVar()
   T5_TSizeCK <- ttkcombobox(T5F_TSizeCK, width = 15, textvariable = LegTxtSize, values = FontSize)
   tkbind(T5_TSizeCK, "<<ComboboxSelected>>", function(){
		           	               Plot_Args$auto.key$cex <<- as.numeric(tclvalue(LegTxtSize))
                             CtrlPlot()
                  })
   tkgrid(T5_TSizeCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T5F_LegColCK <- ttklabelframe(T5group1, text = "ORGANIZE LEGEND IN COLUMNS", borderwidth=2)
   tkgrid(T5F_LegColCK, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
   T5_LegLab <- ttklabel(T5F_LegColCK, text="N. Legend Columns")
   tkgrid(T5_LegLab, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   ColumnNumber <- tclVar()
   T5_LegColCK <- ttkentry(T5F_LegColCK, textvariable=ColumnNumber, width=8)
   tkgrid(T5_LegColCK, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
   tkbind(T5F_LegColCK, "<FocusIn>", function(K){
                             tkconfigure(T5F_LegColCK, foreground="red")
                  })
   tkbind(T5_LegColCK, "<Key-Return>", function(K){
                             tkconfigure(T5F_LegColCK, foreground="black")
                             Plot_Args$auto.key$columns <<- as.numeric(tclvalue(ColumnNumber))
                             CtrlPlot()
                  })

   T5F_LineWdhCK <- ttklabelframe(T5group1, text = "LINE/SYMBOL WEIGHT", borderwidth=2)
   tkgrid(T5F_LineWdhCK, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
   LineWidthCK <- tclVar()
   T5_LineWdhCK <- ttkcombobox(T5F_LineWdhCK, width = 15, textvariable = LineWidthCK, values = LWidth)
   tkbind(T5_LineWdhCK, "<<ComboboxSelected>>", function(){
                             weight <- as.numeric(tclvalue(LineWidthCK))
                             if (tclvalue(SetLines)=="ON") {   #Lines selected
                                Plot_Args$par.settings$superpose.line$lwd <<- weight
                             }
                             if (tclvalue(SetSymbols)=="ON") {   #Symbol selected
                                Plot_Args$par.settings$superpose.symbol$cex <<- weight
                             }
                             CtrlPlot()
                  })
   tkgrid(T5_LineWdhCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T5F_TxtColCK <- ttklabelframe(T5group1, text = "LEGEND TEXT COLOR", borderwidth=2)
   tkgrid(T5F_TxtColCK, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
   TxtColCK <- tclVar()
   T5_TxtColCK <- ttkcombobox(T5F_TxtColCK, width = 15, textvariable = TxtColCK, values = c("B/W", "RainBow"))
   tkbind(T5_TxtColCK, "<<ComboboxSelected>>", function(){
                             if(tclvalue(TxtColCK)=="B/W") {
                                tclvalue(SetLines) <- "ON"
                                tclvalue(LineType) <- "Patterns"
                                tclvalue(SymbolType) <- "Multi-Symbols"
                                tclvalue(TxtColCK) <- "B/W"
                                PlotParameters$Colors <<- "black"
                                Plot_Args$lty <<- rep("solid", 20)
                                Plot_Args$pch <<- STypeIndx
                                Plot_Args$cex <<- as.numeric(tclvalue(SymbolSize))
                                Plot_Args$par.settings$superpose.symbol$col <<- "black"
                                Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                Plot_Args$par.settings$superpose.line$col <<- "black"
                                Plot_Args$par.settings$superpose.line$lty <<- rep("solid", 20)
                                Plot_Args$par.settings$strip.background$col <<- "grey90"
                                if(tclvalue(LineType) == "Patterns"){
                                   Plot_Args$lty <<- LType
                                   Plot_Args$par.settings$superpose.line$lty <<- LType
                                }
                                AutoKey_Args$col <<- "black"
                             } else {
                                tclvalue(TxtColCK) <- "RainBow"
                                Plot_Args$lty <<- rep("solid", 20)
                                Plot_Args$pch <<- rep(STypeIndx[1], 20)
                                Plot_Args$cex <<- as.numeric(tclvalue(SymbolSize))
                                PlotParameters$Colors <<- Colors
                                Plot_Args$par.settings$superpose.line$col <<- Colors
                                Plot_Args$par.settings$superpose.line$lty <<- rep("solid", 20)
                                Plot_Args$par.settings$superpose.symbol$col <<- Colors
                                Plot_Args$par.settings$superpose.symbol$pch <<- rep(STypeIndx[1], 20)
                                Plot_Args$par.settings$strip.background$col <<- "lightskyblue1"
                                AutoKey_Args$col <<- Colors
                                if(tclvalue(LineType) == "Patterns"){
                                   Plot_Args$lty <<- LType
                                   Plot_Args$par.settings$superpose.line$lty <<- LType
                                }
                                if(tclvalue(SymbolType) == "Multi-Symbols"){
                                   Plot_Args$pch <<- STypeIndx
                                   Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx
                                }
                             }
                             CtrlPlot()
                 })
   tkgrid(T5_TxtColCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   ResetButt5 <- tkbutton(T5group1, text=" RESET PLOT ", width=15, command=function(){
                             ResetPlot()
                             CtrlPlot()
                 })
   tkgrid(ResetButt5, row = 5, column = 1, padx = 5, pady = 5, sticky="w")

   ExitButt5 <- tkbutton(T5group1, text=" EXIT ", width=15, command=function(){
				                         XPSSaveRetrieveBkp("save")
                             tkdestroy(CompareWindow)
                 })
   tkgrid(ExitButt5, row = 5, column = 2, padx = 5, pady = 5, sticky="w")


#----- END NOTEBOOK -----
   tcl("update", "idletasks")

}
