#XPSCustomPlot function to produce customized plots
#
#' @title Function to generate personalized plot
#' @description XPSCustomPlot allows a full control of the various
#'   parameters forplotting data. Through a user friendly interface it
#'   is possible to set colors, lines or symbols, their weight,  modify
#'   title, X,Y labels and their dimensions, add/modify legend
#'   annotate the plot.This GUI is based on the Lattice package.
#' @examples
#' \dontrun{
#' 	XPSCustomPlot()
#' }
#' @export
#'

XPSCustomPlot <- function(){

   CtrlPlot <- function(){

            logTicks <- function (lim, loc = c(1, 5)) {
               ii <- floor(log10(range(lim))) + c(-1, 2)
               main <- 10^(ii[1]:ii[2])
               r <- as.numeric(outer(loc, main, "*"))
               r[lim[1] <= r & r <= lim[2]]
            }


            if ( tclvalue(LINEONOFF) == "ON" && tclvalue(SYMONOFF) == "ON") {  # symbols OFF
               Plot_Args$type <<- "b"  # both: line and symbols
            }  #conditions on lines and symbols see above (T2obj4==ON   T2obj7==ON)

            idx <- charmatch(tclvalue(SCALETY), ScaleTypes) #Charmatch does not interpret math symbols

            LL <- length(SpectName)
            if (LL > 0){
	              graph <<- do.call(xyplot, args = Plot_Args)
	              plot(graph)
            }

#--- if VBtop or VBFermi are plotted draw the VBtop or VBFermi position
            if (tclvalue(FITONOFF) == "1") {
                if (FName[[SpectIndx]]@Symbol == "VBt" ||  #check if we are plotting VBtop
                    FName[[SpectIndx]]@Symbol == "VBf") {  #or VBFermi
                    TestName <- NULL
                    TestName <- sapply(FName[[SpectIndx]]@Components, function(z) c(TestName, z@funcName))
                    pos <- list(x=NULL, y=NULL)
                    if(length(idx <- grep("VBtop", TestName)) == 0) { #if TestName does not contains "VBtop"
                       idx <- grep("VBFermi", TestName) # TestName must contain VBFermi"
                    }
                    pos$x <- FName[[SpectIndx]]@Components[[idx]]@param["mu", "start"]
                    pos$y <- FName[[SpectIndx]]@Components[[idx]]@param["h", "start"]
                    graph <<- update(graph, panel=function(...){
                                    # replot the graph
                                    panel.xyplot(...)
                                    # now add the marker
                                    panel.xyplot(pos$x, pos$y, type="p", lwd=2, col="orange", pch="+", cex=5)
                             })
                    plot(graph)
                }
            }

            if (tclvalue(FCLBLONOFF) == "1") {   #Fit component label ON/OFFn
                LL <- length(FName[[SpectIndx]]@Components)
                RngX <- range(FName[[SpectIndx]]@RegionToFit$x)
                RngY <- range(FName[[SpectIndx]]@RegionToFit$y)
                yspan <- max(RngY)/20
                LabPosX <- LabPosY <- CompLbl <- NULL
                for(ii in 1:LL){                    #Control mu != NA  (see linear fit in VBTop
                    LabPosX <- c(LabPosX, FName[[SpectIndx]]@Components[[ii]]@param["mu", "start"])
                    CompLbl <- c(CompLbl, as.character(ii))
                    BaseLevl <- findY(FName[[SpectIndx]]@Baseline, LabPosX)
                    if (is.na(LabPosX)==FALSE){   #in VBtop Lin Fit there is not a value for mu
                        if (LabPosX <= max(RngX) && LabPosX >= min(RngX)){   #Lab only if inside X-range
                            if (FName[[SpectIndx]]@Components[[ii]]@param["h", "start"] > BaseLevl) {
                                LabPosY <- c(LabPosY, FName[[SpectIndx]]@Components[[ii]]@param["h", "start"]+yspan)
                            }
                            if (max(FName[[SpectIndx]]@Components[[ii]]@ycoor) < BaseLevl) {
                                LabPosY <- c(LabPosY, FName[[SpectIndx]]@Components[[ii]]@param["h", "start"]+1.5*yspan)
                            }
                        }
                    }
                }
                graph <<- graph + update(graph, panel=function(...){
                                               panel.text(x=LabPosX,y=LabPosY, labels=CompLbl, col="black") #draws component labels
                                            })
                plot(graph)
            }

            if ( ErrData == TRUE && length(SetErrBars) > 0 ) {  #Request to plot Error bars
               PlotErrorBar()
            }
   }

   PlotErrorBar <- function(){
            x1 <- as.numeric(tclvalue(XMIN))
            y1 <- as.numeric(tclvalue(YMIN))
            x2 <- as.numeric(tclvalue(XMAX))
            y2 <- as.numeric(tclvalue(YMAX))


            MinX <- min(SampData$x)
            MaxX <- max(SampData$x)
            Xlim <<- c(MinX, MaxX)
            MaxY <- max(SampData$y+SampData$err)
            MinY <- min(SampData$y-SampData$err)
            MaxY <- MaxY + (MaxY-MinY)/15
            MinY <- MinY - (MaxY-MinY)/15
            Ylim <<- c(MinY, MaxY)
            if (is.na(x1) && is.na(x2) && is.na(y1) && is.na(y2)){
                Plot_Args$ylim <<- Ylim  <<- c(MinY, MaxY)
            } else {
                if (!is.na(x1) && !is.na(x2)){ # --- Set X Range
                    if (tclvalue(REVAX) == "1") { #Binding energy set
                        Plot_Args$xlim  <<- Xlim <<- sort(c(x1, x2), decreasing=TRUE)
                    } else {
                        Plot_Args$xlim  <<- Xlim <<- sort(c(x1, x2))
                    }
                }
                if (!is.na(y1) && !is.na(y2)){ # --- Set Y Range
                    Plot_Args$ylim  <<- Ylim <<- sort(c(y1, y2))
                }
            }
            NData <- length(SampData$x)
            EndLngth <- as.numeric(tclvalue(ERRENDS)) #Amplitude of the limiting ending bars
            LinWdt <- tclvalue(SPECTLWD)
            graph <<- do.call(xyplot, args = Plot_Args)
            plot(graph)
            #following bar selection, compose the sequence of errorbars to plot
            xx1 <- NULL
            xx2 <- NULL
            yy1 <- NULL
            yy2 <- NULL
            NOpt <- length(SetErrBars)
            for(ii in 1:NOpt){
                if (SetErrBars[ii] == 1){ #plot the upper part of the stanbdard error
                    xx1 <- c(xx1, SampData$x)
                    xx2 <- c(xx2, SampData$x)
                    yy1 <- c(yy1, SampData$y)
                    yy2 <- c(yy2, SampData$y + SampData$err)
                }
                if (SetErrBars[ii] == 2){ #lower part of the stanbdard error
                    xx1 <- c(xx1, SampData$x)
                    xx2 <- c(xx2, SampData$x)
                    yy1 <- c(yy1, SampData$y)
                    yy2 <- c(yy2, SampData$y - SampData$err)
                }
                if (SetErrBars[ii] == 3){ #upper limiting bar
                    xx1 <- c(xx1, SampData$x - EndLngth)
                    xx2 <- c(xx2, SampData$x + EndLngth)
                    yy1 <- c(yy1, SampData$y + SampData$err)
                    yy2 <- c(yy2, SampData$y + SampData$err)
                }
                if (SetErrBars[ii] == 4){ #lower limiting bar
                    xx1 <- c(xx1, SampData$x - EndLngth)
                    xx2 <- c(xx2, SampData$x + EndLngth)
                    yy1 <- c(yy1, SampData$y - SampData$err)
                    yy2 <- c(yy2, SampData$y - SampData$err)
                }
            }
            if (tclvalue(SCALETY) == "Log10 Y") { # Y-log scale selected
                yy1 <- log10(yy1)
                yy2 <- log10(yy2)
            }
#            panel = function(x=SampData$x, y=SampData$y,
#                                         col=SpectCol, lwd=LinWdt,...){
#                   panel.xyplot(x, y, type=Plot_Args$type,  pch=Plot_Args$pch, cex=Plot_Args$cex, col=SpectCol, lwd=LinWdt)
#                   panel.segments(x, yy1, x, yy2, col=SpectCol, lwd=LinWdt) #, col=SpectCol, lwd=LinWdt) #, col=SpectCol, lwd=LinWdt) #plots the upper part of the segment
#            }
#            Plot_Args$panel <<- panel  #This is important to correctly update Plot_Args
#            graph <<- do.call(xyplot, args = Plot_Args)
            graph <<- update(graph, panel=function(x=SampData$x, y=SampData$y,
                            col=SpectCol, lwd=LinWdt,...){
                            panel.xyplot(x, y, type=Plot_Args$type,  pch=Plot_Args$pch, cex=Plot_Args$cex, col=SpectCol, lwd=LinWdt)
                            panel.segments(xx1, yy1, xx2, yy2, col=SpectCol, lwd=LinWdt) #, col=SpectCol, lwd=LinWdt) #, col=SpectCol, lwd=LinWdt) #plots the upper part of the segment
            })
            plot(graph)
   }

   CustomPltAnnotate <- function(){

       MakePlot <- function(){
               x1 <- y1 <- x0 <- y0 <- cex <- col <- adj <- NULL
               graph <<- AcceptedGraph
               plot(graph)
               if (! is.null(TextPosition$x) && ! is.null(TextPosition$y)){
                   AnnotationData <- list(x0 = TextPosition$x,
                                                y0 = TextPosition$y,
                                                pos = 0,
                                                adj = c(-1, -1),
                                                offset=2,
                                                labels = AnnotateText,
                                                cex = TextSize,
                                                col = TextColor
                                         )

                   graph <<- graph + latticeExtra::layer(data = AnnotationData,
                                                panel.text(x=x0, y=y0, pos=0, adj=adj,
                                                labels=labels, cex=cex, col=col),
                                                force=TRUE)
               }

               if (! is.null(ArrowPosition0$x) && ! is.null(ArrowPosition1$y)){
                   AnnotationData <- list(x0 = ArrowPosition0$x,
                                                y0 = ArrowPosition0$y,
                                                x1 = ArrowPosition1$x,
                                                y1 = ArrowPosition1$y,
                                                col = TextColor)
                   graph <<- graph + latticeExtra::layer(data = AnnotationData, panel.points(x = x0, y = y0, type="p", cex=1.1, pch=20, col=col))
                   graph <<- graph + latticeExtra::layer(data = AnnotationData, panel.arrows(x0 = x0, y0 = y0,
                                                   x1 = x1, y1 = y1, length = 0.07, col = col))
                   ArrowPosition0 <<- ArrowPosition1 <<- list(x=NULL, y=NULL)
               }
               trellis.unfocus()
               plot(graph)
       }

       ConvertCoords <- function(pos){
               X1 <- min(Xlim)
               if ( tclvalue(REVAX) == 1) {
                   X1 <- max(Xlim)   #Binding Energy Set
               }
               RangeX <- abs(Xlim[2]-Xlim[1])
               Y1 <- min(Ylim)
               RangeY <- Ylim[2]-Ylim[1]
               PosX <- max(convertX(unit(Xlim, "native"), "points", TRUE))
               PosY <- max(convertY(unit(Ylim, "native"), "points", TRUE))
               if ( tclvalue(REVAX) == 1) {
                   pos$x <- X1-as.numeric(pos$x)*RangeX/PosX
               } else {
                   pos$x <- X1+as.numeric(pos$x)*RangeX/PosX
               }
               pos$y <- Y1+as.numeric(pos$y)*RangeY/PosY
               return(pos)
      }

#--- variables CustomPltAnnotate ---
       Colors <- XPSSettings$Colors
       FontSize <- c(0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)
       FontCol <- XPSSettings$Colors

       TextPosition <- list(x=NULL, y=NULL)
       ArrowPosition0 <- list(x=NULL, y=NULL)
       ArrowPosition1 <- list(x=NULL, y=NULL)
       TextSize <- 1
       TextColor <- "black"
       SpectColor <- "black"
       AnnotateText <- "?"
       OrigGraph <- graph
       AcceptedGraph <- graph   #save the plot before Annotation to make UNDO
#       SampData <- setAsMatrix(FName[[SpectIndx]],"matrix") #store spectrum baseline etc in a matrix
       graph <<- NULL

    #----- Annotate Widget -----
       AnnWin <- tktoplevel()
       tkwm.title(AnnWin,"ANNOTATE")
       tkwm.geometry(AnnWin, "+100+50")   #SCREEN POSITION from top-left corner

       AnnGroup <- ttkframe(AnnWin,  borderwidth=0, padding=c(0,0,0,0))
       tkgrid(AnnGroup, row = 1, column = 1, sticky="we")

       INFOframe <- ttklabelframe(AnnGroup, text = " HELP ", borderwidth=2)
       tkgrid(INFOframe, row = 2, column = 1, padx = 5, pady = 5, sticky="we")
       tkgrid( ttklabel(INFOframe, text="1. Mouse-locate Position and Define the Label"),
               row = 1, column=1, pady=2, sticky="w")
       tkgrid( ttklabel(INFOframe, text="2. Change Size and Color if Needed"),
               row = 2, column=1, pady=2, sticky="w")
       tkgrid( ttklabel(INFOframe, text="3. If Label OK ACCEPT or UNDO to the Previous Plot "),
               row = 3, column=1, pady=2, sticky="w")

       Anframe1 <- ttklabelframe(AnnGroup, text = " Text ", borderwidth=2)
       tkgrid(Anframe1, row = 3, column = 1, padx = 5, pady = 5, sticky="we")
       tkgrid( ttklabel(Anframe1, text=" Text to Annotate: "),
               row = 1, column = 1, padx = 5, pady = 5, sticky="we")
       ANNTXT <- tclVar("Label?")
       AnnEntry1 <- ttkentry(Anframe1, textvariable=ANNTXT, foreground="grey")
       tkgrid(AnnEntry1, row = 1, column = 2, padx=5, pady=5, sticky="we")
       tkbind(AnnEntry1, "<FocusIn>", function(K){
                              tclvalue(ANNTXT) <- ""
                              tkconfigure(AnnEntry1, foreground="red")
                       })
       tkbind(AnnEntry1, "<Key-Return>", function(K){
                              tkconfigure(AnnEntry1, foreground="black")
                              AnnotateText <<- tclvalue(ANNTXT)
                       })

       Anframe2 <- ttklabelframe(AnnGroup, text = "  Set Text Position ", borderwidth=2)
       tkgrid(Anframe2, row = 5, column = 1, padx = 5, pady = 5, sticky="we")
       TxtButt <- tkbutton(Anframe2, text=" TEXT POSITION ", command=function(){
                            if (is.null(AnnotateText)){
                                tkmessageBox(message="Set Label Text Please", title="WARNING", icon="warning")
                            }
                            trellis.focus("panel", 1, 1, clip.off=TRUE, highlight=FALSE)
                            pos <- list(x=NULL, y=NULL)
                            WidgetState(Anframe1, "disabled")
                            WidgetState(Anframe2, "disabled")
                            WidgetState(Anframe3, "disabled")
                            WidgetState(BtnGroup, "disabled")
                            pos <- grid.locator(unit = "points")
                            TextPosition <<- ConvertCoords(pos)
                            if (is.null(TextPosition$x) && is.null(TextPosition$x))  {
                               return()
                            }
                            TextSize <<- as.numeric(tclvalue(TXTSIZE))
                            if (is.na(TextSize)) {TextSize <<- 1}
                            TextColor <<- tclvalue(TCOLOR)
                            if (is.na(TextColor)) {TextColor <<- "black"}

                            tkdestroy(AnnotePosition)
                            txt <- paste("Text Position: X = ", round(TextPosition$x, 1), "  Y = ", round(TextPosition$y, 1), sep="")
                            AnnotePosition <- ttklabel(Anframe2, text=txt)
                            tkgrid(AnnotePosition, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
                            WidgetState(Anframe1, "normal")
                            WidgetState(Anframe2, "normal")
                            WidgetState(Anframe3, "normal")
                            WidgetState(BtnGroup, "normal")
                            MakePlot()
                       })
       tkgrid(TxtButt, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       AnnotePosition <- ttklabel(Anframe2, text="Text Position                ")
       tkgrid(AnnotePosition, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

       Anframe3 <- ttklabelframe(AnnGroup, text = " Text Size & Color ", borderwidth=2)
       tkgrid(Anframe3, row = 6, column = 1, padx = 5, pady =c(2, 5), sticky="we")
       tkgrid( ttklabel(Anframe3, text=" Size "),
               row = 1, column = 1, padx = 5, pady = 2, sticky="w")
       tkgrid( ttklabel(Anframe3, text=" Color "),
               row = 1, column = 2, padx = 5, pady = 2, sticky="w")

       TXTSIZE <- tclVar("1")  #TEXT SIZE
       AnnoteSize <- ttkcombobox(Anframe3, width = 15, textvariable = TXTSIZE, values = FontSize)
       tkbind(AnnoteSize, "<<ComboboxSelected>>", function(){
                            if (is.null(TextPosition$x) || is.null(TextPosition$y)) {
                                tkmessageBox(message="Please set the Label Position first!", title="WARNING: position lacking", icon="warning")
                            } else {
                                TextSize <<- as.numeric(tclvalue(TXTSIZE))
                                trellis.focus("panel", 1, 1, clip.off=TRUE, highlight=FALSE)
                                MakePlot()
                            }
                       })
       tkgrid(AnnoteSize, row = 2, column = 1, padx = 5, pady =c(2, 5), sticky="w")

       TCOLOR <- tclVar("black") #TEXT COLOR
       AnnoteColor <- ttkcombobox(Anframe3, width = 15, textvariable = TCOLOR, values = FontCol)
       tkbind(AnnoteColor, "<<ComboboxSelected>>", function(){
                            if (is.null(TextPosition$x) || is.null(TextPosition$y)) {
                                tkmessageBox(message="Please set the Label Position first!", title="WARNING: position lacking", icon="warning")
                            } else {
                                TextColor <<- tclvalue(TCOLOR)
                                trellis.focus("panel", 1, 1, clip.off=TRUE, highlight=FALSE)
                                MakePlot()
                            }
                       })
       tkgrid(AnnoteColor, row = 2, column = 2, padx = 5, pady =c(2, 5), sticky="w")

       BtnGroup <- ttkframe(AnnWin, borderwidth=0, padding=c(0,0,0,0))
       tkgrid(BtnGroup, row = 7, column = 1, sticky="we")
       tkgrid.columnconfigure(BtnGroup, 1, weight = 1)  #needed to extend buttons with sticky="we"

       AddArwButt <- tkbutton(BtnGroup, text=" ADD ARROW ", command=function(){
                            TextColor <- tclvalue(TCOLOR)
                            WidgetState(Anframe1, "disabled")
                            WidgetState(Anframe2, "disabled")
                            WidgetState(Anframe3, "disabled")
                            WidgetState(BtnGroup, "disabled")
                            trellis.focus("panel", 1, 1, clip.off=TRUE, highlight=FALSE)
                            pos <- grid.locator(unit = "points")
                            ArrowPosition0 <<- ConvertCoords(pos)
                            panel.points(x = ArrowPosition0$x, y = ArrowPosition0$y, type="p", cex=1.1, pch=20, col=TextColor)
                            pos <- grid.locator(unit = "points") #first mark the arrow start point
                            ArrowPosition1 <<- ConvertCoords(pos)
                            WidgetState(Anframe1, "normal")
                            WidgetState(Anframe2, "normal")
                            WidgetState(Anframe3, "normal")
                            WidgetState(BtnGroup, "normal")
                            MakePlot()
                       })
       tkgrid(AddArwButt, row = 1, column = 1, padx = 5, pady = 5, sticky="we")

       AcceptButt <- tkbutton(BtnGroup, text=" ACCEPT ", command=function(){
                            AcceptedGraph <<- graph  #accept the new annotation
                            TextPosition <<- list(x=NULL, y=NULL)
                            AnnotateText <<- NULL
                            TextSize <<- 1
                            TextColor <<- "black"
                       })
       tkgrid(AcceptButt, row = 2, column = 1, padx = 5, pady = 5, sticky="we")

       UndoButt <- tkbutton(BtnGroup, text=" UNDO ", command=function(){
                            #Restore the last accepted plot
                            TextPosition <<- list(x=NULL, y=NULL)
                            AnnotateText <<- NULL
                            TextSize <<- 1
                            TextColor <<- "black"
                            plot(AcceptedGraph)
                            trellis.unfocus()
                       })
       tkgrid(UndoButt, row = 3, column = 1, padx = 5, pady = 5, sticky="we")

       RestoreButt <- tkbutton(BtnGroup, text=" REFRESH PLOT ", command=function(){
                            #Restore the initial original plot
                            AcceptedGraph <<- OrigGraph   #restore graph at the previous annotation step
                            TextPosition <<- list(x=NULL, y=NULL)
                            AnnotateText <<- NULL
                            TextSize <<- 1
                            TextColor <<- "black"
                            plot(AcceptedGraph)
                            trellis.unfocus()
                       })
       tkgrid(RestoreButt, row = 4, column = 1, padx = 5, pady = 5, sticky="we")

       ExitButt <- tkbutton(BtnGroup, text="  EXIT  ", command=function(){
                            trellis.unfocus()
                            tkdestroy(AnnWin)
                       })
       tkgrid(ExitButt, row = 5, column = 1, padx = 5, pady = 5, sticky="we")
   }
#---- End CustomPltAnnotate ----

   setRange <- function(){
               x1 <- as.numeric(tclvalue(XMIN))
               y1 <- as.numeric(tclvalue(YMIN))
               x2 <- as.numeric(tclvalue(XMAX))
               y2 <- as.numeric(tclvalue(YMAX))

               if (!is.na(x1) && !is.na(x2)){
                   if (tclvalue(REVAX ) == "1") { #Binding energy set
                       Xlim <<- sort(c(x1, x2), decreasing=TRUE)
                   } else {
                       Xlim <<- sort(c(x1, x2))
                   }
                   Ylim <<- sort(c(y1, y2))
               } else {
                   if (tclvalue(BLINEONOFF) == "1" ||
                       tclvalue(FCOMPONOFF) == "1" ||
                       tclvalue (FITONOFF) == "1"){
                       Xlim <<- range(FName[[SpectIndx]]@RegionToFit$x)
                       wdth <- Xlim[2]-Xlim[1]
                       Xlim[1] <<- Xlim[1]-wdth/15
                       Xlim[2] <<- Xlim[2]+wdth/15
                       Ylim <<- range(FName[[SpectIndx]]@RegionToFit$y)
                       wdth <- Ylim[2]-Ylim[1]
                       Ylim[1] <<- Ylim[1]-wdth/15
                       Ylim[2] <<- Ylim[2]+wdth/15
                       if (tclvalue(REVAX) == "1") { #Binding energy set
                           Xlim <<- sort(Xlim, decreasing=TRUE)
                       }
                       Ylim <<- sort(Ylim)
                   } else {
                       Xlim <<- range(FName[[SpectIndx]]@.Data[[1]])
                       wdth <- Xlim[2]-Xlim[1]
                       Xlim[1] <<- Xlim[1]-wdth/15
                       Xlim[2] <<- Xlim[2]+wdth/15
                       Ylim <<- range(FName[[SpectIndx]]@.Data[[2]])
                       wdth <- Ylim[2]-Ylim[1]
                       Ylim[1] <<- Ylim[1]-wdth/15
                       Ylim[2] <<- Ylim[2]+wdth/15
                       if (tclvalue(REVAX ) == "1") { #Binding energy set
                           Xlim <<- sort(Xlim, decreasing=TRUE)
                       }
                       Ylim <<- sort(Ylim)
                   }
               }
               Plot_Args$xlim <<- Xlim
               Plot_Args$ylim <<- Ylim
   }

#--- Routine for drawing Custom Axis
   CustomAx <- function(CustomDta){
               AxItems <- c("Ticks starting from:", "Tick step: ", "Number of Ticks: ")
               AxisData <- list(Info=AxItems, Data = rep("?", 3))
               AxisData <- as.data.frame(AxisData, , stringsAsFactors=FALSE)
               RowNames <- c("Start", "Step", "N.Ticks")
               ColNames <- c("Info", "Data")
               Title <- "CUSTOM SCALE"
               AxisData <- DFrameTable(Data=AxisData, Title=Title, ColNames=ColNames, RowNames=RowNames,
                                       Width=c(20, 10), Modify=TRUE, Env=environment())
               AxisData <- as.numeric(unlist(AxisData[[2]]))

               if (AxisData[1]=="?" || AxisData[1]=="" || AxisData[1]==" " || is.na(AxisData[1])){
                   tkmessageBox(message="Please Start value required!", title="WARNING", icon="warning")
                   return()
               }
               if (AxisData[2]=="?" || AxisData[2]=="" || AxisData[2]==" " || is.na(AxisData[2])){
                   tkmessageBox(message="Please Step value required!", title="WARNING", icon="warning")
                   return()
               if (AxisData[3]=="?" || AxisData[3]=="" || AxisData[3]==" " || is.na(AxisData[3])){
                   tkmessageBox(message="Please N. Major Ticks  required!", title="WARNING", icon="warning")
                   return()
               }
               } else {
                   End <- AxisData[1]+AxisData[2]*AxisData[3]
                   AXstp <- seq(from=AxisData[1], to=End, by=AxisData[2])
                   Ticklabels <- as.character(round(AXstp,digits=1))
                   if (CustomDta[[3]] == "X") {
                       Plot_Args$scales$x <<- list(at=AXstp, labels=Ticklabels)
                   } else if (CustomDta[[3]] == "Y") {
                       Plot_Args$scales$y <<- list(at=AXstp, labels=Ticklabels)
                   }
                   CtrlPlot()
                   Plot_Args$scales$relation <<- "same"
               }
   }

   SetXYplotData <- function() {
               NComp <- length(FName[[SpectIndx]]@Components)
               select <- ""
               code <- vector()
               if (tclvalue(RNGIDX) == "Original XYrange") {
                   select <- "MAIN"   #plot raw data
                   setRange()
               } else {
                   select <- "RTF"    #plot RegionToFit
                   setRange()
               }
               code <- 1   #code is the label identifying the group of data possessing
#                          #same properties (linetype, lwd, color etc...)
#                          #and at the same time in which order the style options have to be applied

               if (tclvalue(BLINEONOFF) == "1") {
                   select <- c(select, "BASE")
                   code <- c(code, 2)
                   setRange()
               }
               if (tclvalue(FCOMPONOFF) == "1") {
                   select <- c(select, "COMPONENTS")
                   code <- c(code, (3:(NComp+2)))
                   setRange()
               }
               if (tclvalue(FITONOFF) == "1") {
                   select <- c(select, "FIT")
                   code <- c(code, (NComp+3))
                   setRange()
               } else {
                   Plot_Args$ylab$label <<- FName[[SpectIndx]]@units[2]
                   setRange()
                   Plot_Args$ylim <<- c(Ylim[1]-(Ylim[2]-Ylim[1])/10, Ylim[2]+(Ylim[2]-Ylim[1])/10)
               }


               if (tclvalue(NORM) == "1"){
                   if (length(grep("[cps]", FName[[SpectIndx]]@units[2], fixed=TRUE)) > 0) {  #if original Ylab="Intensity [cps]"
                       Plot_Args$ylab$label <<- "Intensity [a.u.]"
                   }
                   #Now normalize the .Data[[2]], RegionToFit$y, Baseline, Components and Fit
                   RngY <- range(FName[[SpectIndx]]@.Data[[2]])
                   FName[[SpectIndx]]@.Data[[2]] <- (FName[[SpectIndx]]@.Data[[2]]-RngY[1])/(RngY[2]-RngY[1])
                   if (hasBaseline(FName[[SpectIndx]])){
                       FName[[SpectIndx]]@RegionToFit$y <- (FName[[SpectIndx]]@RegionToFit$y-RngY[1])/(RngY[2]-RngY[1])
                       FName[[SpectIndx]]@Baseline$y <- (FName[[SpectIndx]]@Baseline$y-RngY[1])/(RngY[2]-RngY[1])
                   }
                   if (hasComponents(FName[[SpectIndx]])){
                       for(ii in 1:NComp){
                           FName[[SpectIndx]]@Components[[ii]]@ycoor <- (FName[[SpectIndx]]@Components[[ii]]@ycoor-RngY[1])/(RngY[2]-RngY[1])
                       }
                       FName[[SpectIndx]]@Fit$y <- FName[[SpectIndx]]@Fit$y/(RngY[2]-RngY[1])
                   }
                   Plot_Args$ylim <<- c(-0.05,1.05)  #normalized limits: slightly larger than [0,1]
               }

               tmp <- asList(FName[[SpectIndx]],select=select) #from coreline FName[[SpectIndx]] extract the selecteed regions
               X <- tmp$x # x list
               Y <- tmp$y # y list

               if (length(Xlim)==0 || length(Ylim)==0){
                  Xlim  <<- sort(range(X, na.rm=TRUE))
                  Ylim  <<- sort(range(Y, na.rm=TRUE))
                  wdth <- Xlim[2]-Xlim[1]
                  Xlim[1] <<- Xlim[1]-wdth/15
                  Xlim[2] <<- Xlim[2]+wdth/15
                  wdth <- Ylim[2]-Ylim[1]
                  Ylim[1] <<- Ylim[1]-wdth/15
                  Ylim[2] <<- Ylim[2]+wdth/15
                  if (tclvalue(REVAX) == "1") {   #reverse scale if checkbox TRUE
                      Xlim <<- sort(Xlim, decreasing=TRUE)
                      Plot_Args$xlim <<- Xlim
                  } else {
                      Xlim <<- sort(Xlim, decreasing=FALSE)
                      Plot_Args$xlim <<- Xlim
                  }
               }
               Ylength <- lapply(Y, length)
               Ylength <- as.array(as.integer(Ylength))
               grps <- list()
               levelX <- list()
               NN <- length(Ylength)
               for (ii in 1:NN){
                   grps[[ii]] <- rep(code[ii], times=Ylength[ii])
               }

               df <- data.frame(x = unname(unlist(X)), y = unname(unlist(Y)) )
               Plot_Args$data <<- df
               Plot_Args$groups <<- unlist(grps)
               if (tclvalue(LINEONOFF) == "ON") {
                  LTy <- grep(tclvalue(SPECTLTY), LineTypes)
                  Plot_Args$type <<- "l"
                  Plot_Args$col <<- SpectCol
                  Plot_Args$lty <<- LTy
                  Plot_Args$lwd <<- as.numeric(tclvalue(SPECTLWD))
                  if (tclvalue(BLINEONOFF) == "1") {
                     LTy <- grep(tclvalue(BLINELTY), LineTypes)
                     Plot_Args$col <<- c(Plot_Args$col, BLineCol)
                     Plot_Args$lty <<- c(Plot_Args$lty, LTy)
                     Plot_Args$lwd <<- c(Plot_Args$lwd, as.numeric(tclvalue(BLINELWD)))
                  }
                  if (tclvalue (FCOMPONOFF) == "1") {
                     LTy <- grep(tclvalue(FCOMPLTY), LineTypes)
                     Plot_Args$col <<- c(Plot_Args$col, FCompCol)
                     Plot_Args$lty <<- c(Plot_Args$lty, rep(LTy, NComp))
                     Plot_Args$lwd <<- c(Plot_Args$lwd, rep(as.numeric(tclvalue(FCOMPLWD)), NComp))
                  }
                  if (tclvalue(FITONOFF) == "1") {
                     LTy <- grep(tclvalue(FITLTY), LineTypes)
                     Plot_Args$col <<- c(Plot_Args$col, FitCol)
                     Plot_Args$lty <<- c(Plot_Args$lty, LTy)
                     Plot_Args$lwd <<- c(Plot_Args$lwd, as.numeric(tclvalue(FITLWD)))
                  }
                  if (tclvalue(LEGONOFF) == "1"){
                      Plot_Args$auto.key <<-AutoKey_Args
                      Plot_Args$par.settings$superpose.line$col <<- SpectCol
                      Plot_Args$par.settings$superpose.line$lty <<- grep(tclvalue(SPECTLTY), LineTypes)
                  }
               }
               if ( tclvalue(SYMONOFF) == "ON") {
                  Plot_Args$type  <<-"p"
                  Plot_Args$col <<- SpectCol
                  Plot_Args$pch <<- STypeIndx[grep(tclvalue(SPECTSYM),SType)]
                  Plot_Args$cex <<- as.numeric(tclvalue(SPECTSYMSIZE))
                  if (tclvalue(BLINEONOFF)=="1") {
                     Plot_Args$col <<- c(Plot_Args$col, BLineCol)
                     Plot_Args$pch <<- c(Plot_Args$pch, STypeIndx[grep(tclvalue(BLINESYM), SType)])
                     Plot_Args$cex <<- c(Plot_Args$cex, as.numeric(tclvalue(BLSYMSIZE)))
                  }
                  if (tclvalue(FCOMPONOFF)=="1") {
                     Plot_Args$col <<- c(Plot_Args$col, FCompCol)
                     Plot_Args$pch <<- c(Plot_Args$pch, rep(STypeIndx[grep(tclvalue(FCOMPSYM), SType)], NComp))
                     Plot_Args$cex <<- c(Plot_Args$cex, rep(tclvalue(FCSYMSIZE), NComp))
                  }
                  if (tclvalue(FITONOFF)=="1") {
                     Plot_Args$col <<- c(Plot_Args$col, FitCol)
                     Plot_Args$pch <<- c(Plot_Args$pch, STypeIndx[grep(tclvalue(FITSYM), SType)])
                     Plot_Args$cex <<- c(Plot_Args$cex, as.numeric(tclvalue(FITSYMSIZE)))
                  }
                  if (tclvalue(LEGONOFF) == "1"){
                      Plot_Args$auto.key <<-AutoKey_Args
                      Plot_Args$par.settings$superpose.symbol$col <<- SpectCol
                      Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx[grep(tclvalue(SPECTSYM),SType)]
                  }
               }

               if ( tclvalue(LINEONOFF) == "ON" && tclvalue(SYMONOFF) == "ON") {
                  Plot_Args$type <<- "b"
               }

               if ( tclvalue(LINEONOFF) == "OFF" && tclvalue(SYMONOFF) == "OFF") {
                     Plot_Args$type <<- "x"   #linetype not defined figure cancelled!
                     Plot_Args$auto.key <<- FALSE
               }
               CtrlPlot()
   }

   ResetPlot <- function(){
               tclvalue(REVAX) <- TRUE
               tclvalue(NORM) <- FALSE
               tclvalue(RNGIDX) <- "Original XYrange"
               tclvalue(BLINEONOFF) <- FALSE
               tclvalue(FCOMPONOFF) <- FALSE
               tclvalue(FITONOFF) <- FALSE
               tclvalue(FCLBLONOFF) <- FALSE
               tclvalue(LEGONOFF) <- FALSE

               NComp <<- length(FName[[SpectIndx]]@Components)
               SampData <<- OrigData
               NColS <<- ncol(SampData)
               SetErrBars <<- NULL
               
               SpectCol <<- "black"
               BLineCol <<- "sienna"
               FCompCol <<- XPSSettings$ComponentsColor
               FitCol <<- "red"


	              Plot_Args$x	<<- formula("y ~ x")
               Plot_Args$data <<- SampData
               Plot_Args$groups	<<- rep(1, length(SampData$x))
               Xlim <<- sort(range(SampData$x))
               Ylim <<- sort(range(SampData$y))
               wdth <- Xlim[2]-Xlim[1]
               Xlim[1] <<- Xlim[1]-wdth/15
               Xlim[2] <<- Xlim[2]+wdth/15
               wdth <- Ylim[2]-Ylim[1]
               Ylim[1] <<- Ylim[1]-wdth/15
               Ylim[2] <<- Ylim[2]+wdth/15
               if (tclvalue(REVAX) == "1") {   #reverse scale if checkbox TRUE
                   Xlim <<- sort(Xlim, decreasing=TRUE)
                   Plot_Args$xlim <<- Xlim
               }
               Plot_Args$ylim <<- Ylim
               Plot_Args$type <<-"l"
               Plot_Args$pch <<- 1
               Plot_Args$cex <<- 1
               Plot_Args$lty <<- "solid"
               Plot_Args$lwd <<- 1
               Plot_Args$type <<- "l"
               Plot_Args$background <<- "transparent"
               Plot_Args$col <<- "black"
               Plot_Args$main <<- list(label=SpectName,cex=1.4)
               Plot_Args$xlab <<- list(label=FName[[SpectIndx]]@units[1], rot=0, cex=1.2)
               Plot_Args$ylab <<- list(label=FName[[SpectIndx]]@units[2], rot=90, cex=1.2)
               Plot_Args$scales <<- list(cex=1, tck=c(1,0), alternating=c(1), relation="same",
                                         x=list(log=FALSE), y=list(log=FALSE))
               Plot_Args$xscale.components <<- xscale.components.subticks
               Plot_Args$yscale.components <<- yscale.components.subticks
               Plot_Args$las <<- 0
               Plot_Args$auto.key <<- FALSE
               Plot_Args$grid <<- FALSE

               Xlabel <<- FName[[SpectIndx]]@units[1]
               Ylabel <<- FName[[SpectIndx]]@units[2]
               AutoKey_Args <<- list(space="top",
                                    text=SpectName,
                                    cex = 1,
                                    type= "l",
                                    lines=TRUE,
                                    points=FALSE,
                                    border=FALSE,
                                    list(corner=NULL,x=NULL,y=NULL)
                                   )
               tclvalue(TITSIZE) <<- "1.4"      #Titile size
               tclvalue(AXNUMSIZE) <<- "1"     #axis label size
               tclvalue(LBSIZE) <<- "1.2"       #axis scale size
  }

  LoadCoreLine <- function(){
               SpName <- tclvalue(CL)
               SpName <- unlist(strsplit(SpName, "\\."))
               SpectIndx <<- as.numeric(SpName[1])
               SpectName <<- SpName[2]
               assign("activeSpectName",SpectName,envir=.GlobalEnv)
               assign("activeSpectIndx",SpectIndx,envir=.GlobalEnv)
               OrigData <<- data.frame(x=FName[[SpectIndx]]@.Data[[1]],
                                       y=FName[[SpectIndx]]@.Data[[2]])
               ResetPlot()
               NColS <<- ncol(SampData)
   }


#===== VARIABLES =====

   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FNameList <- XPSFNameList()  #list of the XPSSample loaded in the Global Env
   if (length(FNameList) == 0){
       tkmessageBox(message="No XPS Samples found. Please load XPS Data", title="WARNING", icon="warning")
       return()
   }
   FName <- get(activeFName, envir=.GlobalEnv)
   activeFName <- get("activeFName", envir=.GlobalEnv)
   SpectList <- XPSSpectList(activeFName)      #list of all the corelines of the activeXPSSample
   SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv)
   SpectName <- get("activeSpectName", envir=.GlobalEnv)
   if(length(activeSpectName)==0 || is.null(activeSpectName) || is.na(activeSpectName)){
      activeSpectName <<- SpectList[1]
      activeSpectIndx <<- 1
   }
   FNameList <- XPSFNameList()
   ErrData <- FALSE
   NonNull <- which(!sapply(FName[[1]]@.Data, is.null))
   if (length(NonNull) < 4){
       OrigData <- data.frame(x=FName[[SpectIndx]]@.Data[[1]],
                              y=FName[[SpectIndx]]@.Data[[2]])
   }
   if (length(NonNull) == 4){
       OrigData <- data.frame(x=FName[[SpectIndx]]@.Data[[1]],
                              y=FName[[SpectIndx]]@.Data[[2]],
                              err=FName[[SpectIndx]]@.Data[[4]])
       ErrData <- TRUE
   }
   SampData <- OrigData

   NComp <- length(FName[[SpectIndx]]@Components)
   NColS <- ncol(SampData)
   Xlim <- sort(range(SampData[,1]))
   Ylim <- sort(range(SampData[,2]))
   Xlabel <- FName[[SpectIndx]]@units[1]
   Ylabel <- FName[[SpectIndx]]@units[2]
   Normalize <- FALSE
   SetErrBars <- NULL
   graph <- NULL
   
   Colors <- c("black", "red3", "limegreen", "blue", "magenta", "orange", "cadetblue", "sienna",
             "darkgrey", "forestgreen", "gold", "darkviolet", "greenyellow", "cyan", "lightcoral",
             "turquoise", "deeppink3", "wheat", "thistle", "grey40")
   LType <- c("solid", "dashed", "dotted", "dotdash", "longdash",     #definisco 20 tipi divesi di line pattern
            "twodash", "F8", "431313", "22848222", "12126262",
            "12121262", "12626262", "52721272", "B454B222", "F313F313",
            "71717313", "93213321", "66116611", "23111111", "222222A2" )
   LineTypes <- c("Solid", "Dashed", "Dotted", "Dotdash", "Longdash",     #definisco 20 tipi divesi di line pattern
            "Twodash", "F8", "431313", "22848222", "12126262",
            "12121262", "12626262", "52721272", "B454B222", "F313F313",
            "71717313", "93213321", "66116611", "23111111", "222222A2" )
   SType <- c("VoidCircle", "VoidSquare", "VoidTriangleUp", "VoidTriangleDwn",  "Diamond",
            "X", "Star", "CrossSquare", "CrossCircle", "CrossDiamond",
            "SolidSquare", "SolidCircle", "SolidTriangleUp", "SolidTriangleDwn", "SolidDiamond",
            "DavidStar", "SquareCross", "SquareTriang", "CircleCross", "Cross")
   STypeIndx <- c(1,  0,  2,  6,  5,
                4,  8,  7,  10, 9,
                15, 16, 17, 25, 18,
                11, 12, 14, 13, 3)
   LWidth <- c(1,1.25,1.5,1.75,2,2.25,2.5,3, 3.5,4)
   SymSize <- c(0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2)
   LCol <- "black"
   LW <- 1
   FontSize <- c(0.6, 0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)
   LegPos <- c("OutsideCenterTop", "OutsideTopRight", "OutsideTopLeft", "OutsideCenterRight", "OutsideCenterLeft",
             "OutsideCenterBottom", "InsideTopRight", "InsideTopLeft", "InsideBottomRight", "InsideBottomLeft")
   Orient <- c("Vertical", "Horizontal")
   LineWdh <- c(1,1.5,2,2.5,3,3.5,4,4.5,5)
   TxtCol <- c("Color", "Black")
   TxtSize <- c(0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)
   Dist <- c(0.01,0.02,0.04,0.06,0.08,0.1,0.12,0.14,0.16,0.18,0.2)
   
#--- Default values
   SpectCol <- "black"
   BLineCol <- "sienna"
   FCompCol <- XPSSettings$ComponentsColor
   FitCol <- "red"
   FitCmpClr <- list()
#---tclVars
   ANNTXT <- NULL
   TXTSIZE <- NULL
   TCOLOR <- NULL
   XS <- NULL
   CL <- NULL
   TKs <- NULL
   SCALETY <- NULL
   TITSIZE <- NULL
   NEWTITLE <- NULL
   AXNUMSIZE <- NULL
   LBSIZE <- NULL
   NEWXAXLAB <- NULL
   NEWYAXLAB <- NULL
   REVAX <- NULL
   LBORI <- NULL
   RNGIDX <- NULL
   NORM <- NULL
   XMIN <- NULL
   XMAX <- NULL
   YMIN <- NULL
   YMAX <- NULL
   LINEONOFF <- NULL
   SYMONOFF <- NULL
   SPECTLTY <- NULL
   SPECTLWD <- NULL
   SPECTSYM <- NULL
   SPECTSYMSIZE <- NULL
   ERRENDS <- NULL
   BLINEONOFF <- NULL
   BLINELTY <- NULL
   BLINELWD <- NULL
   BLINESYM <- NULL
   BLSYMSIZE <- NULL
   FCOMPONOFF <- NULL
   FCLBLONOFF <- NULL
   FCOMPLTY <- NULL
   FCOMPLWD <- NULL
   FCOMPSYM <- NULL
   FCSYMSIZE <- NULL
   FITONOFF <- NULL
   FITLTY <- NULL
   FITLWD <- NULL
   FITSYM <- NULL
   FITSYMSIZE <- NULL
   LEGONOFF <- NULL
   LEGENDPOS <- NULL
   LEGTXTSIZE <- NULL
   LEGTXTCOL <- NULL
   NEWLEG <- NULL
#---
   Plot_Args <- list(x=formula("y ~ x"), data=NULL, groups=NULL, layout=NULL,
                     xlim=NULL,ylim=NULL,
                     pch=1,cex=1,lty="solid",lwd=1,type="l",               #default settings for point and lines
                     background="transparent", col="black",
                     main=list(label=NULL,cex=1.4),
                     xlab=list(label=NULL, rot=0, cex=1.2),
                     ylab=list(label=NULL, rot=90, cex=1.2),
                     scales=list(cex=1, tck=c(1,0), alternating=c(1), relation="same",
                                 x=list(log=FALSE), y=list(log=FALSE)),
                     xscale.components = xscale.components.subticks,
                     yscale.components = yscale.components.subticks,
                     las = 0,
                     par.settings = list(superpose.line=list(lty="solid", col=Colors)), #needed to set colors
                     auto.key = FALSE,
                     grid = FALSE
                   )

   AutoKey_Args <- list( space="top",
                         text=SpectName,
                         cex = 1,
                         type= "l",
                         lines=TRUE,
                         points=FALSE,
                         border=FALSE,
                         list(corner=NULL,x=NULL,y=NULL)
                       )

#===== Reset graphic window =====

   plot.new()
   assign("MatPlotMode", FALSE, envir=.GlobalEnv)  #basic matplot function used to plot data


#===== NoteBook =====
   CustomWindow <- tktoplevel()
   tkwm.title(CustomWindow,"CUSTOM PLOT")
   tkwm.geometry(CustomWindow, "+100+50")   #SCREEN POSITION from top-left corner

   CustMainGp <- ttkframe(CustomWindow, borderwidth=2, padding=c(5,5,5,5) )
   tkgrid(CustMainGp, row = 1, column = 1, sticky="w")

   NoteBk <- ttknotebook(CustMainGp)
   tkgrid(NoteBk, row = 1, column = 1, sticky="w")

# --- Tab1: Axes Options ---
       T1group1 <- ttkframe(NoteBk,  borderwidth=2, padding=c(5,5,5,5) )
       tkadd(NoteBk, T1group1, text=" AXES ")

#---  AxGroup groups first and second columns of widgets
       AxGroup <- ttkframe(T1group1, borderwidth=0, padding=c(0,0,0,0))
       tkgrid(AxGroup, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

       T1frame1 <- ttklabelframe(AxGroup, text = "XPS SAMP. SELECTION", borderwidth=3)
       tkgrid(T1frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       XS <- tclVar(activeFName)
       T1obj1 <- ttkcombobox(T1frame1, width = 15, textvariable = XS, values = FNameList)
       tkgrid(T1obj1, row = 1, column = 1, padx = 5, pady = 5, sticky="we")
       tkbind(T1obj1, "<<ComboboxSelected>>", function(){
                             plot.new()
                             ResetPlot()
                             SelectedFName <- tclvalue(XS)
                             FName <<- get(SelectedFName,envir=.GlobalEnv)  #load the XPSSample
                             SpectList <<- XPSSpectList(SelectedFName)
                             SpectIndx <<- 1
                             SampData <<- setAsMatrix(FName[[SpectIndx]],"matrix")
                             tkconfigure(T1obj2, values=SpectList)
                 })

       T1frame2 <- ttklabelframe(AxGroup, text = "CORE LINE SELECTION", borderwidth=3)
       tkgrid(T1frame2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
       CL <- tclVar()
       T1obj2 <- ttkcombobox(T1frame2, width = 15, textvariable = CL, values = SpectList)
       tkgrid(T1obj2, row = 1, column = 1, padx = 5, pady = 5, sticky="we")
       tkbind(T1obj2, "<<ComboboxSelected>>", function(){
                             LoadCoreLine()
                             CtrlPlot()
                 })

       T1frame6 <- ttklabelframe(AxGroup, text = "TICKS", borderwidth=3)
       tkgrid(T1frame6, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
       AxTicks <- c("Left & Bottom", "Top & Right", "Both", "Custom X", "Custom Y")
       TKs <- tclVar("Left & Bottom")
       T1obj6 <- ttkcombobox(T1frame6, width = 15, textvariable = TKs, values = AxTicks)
       tkgrid(T1obj6, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T1obj6, "<<ComboboxSelected>>", function(){
                             idx <- grep(tclvalue(TKs), AxTicks)
                             Plot_Args$scales$relation <<- "same"
                             if (idx==1) {
                                Plot_Args$scales$tck <<- c(1,0)
                                Plot_Args$scales$alternating <<- 1
                             } else if (idx==2) {
                                Plot_Args$scales$tck <<- c(0,1)
                                Plot_Args$scales$alternating <<- 2
                             } else if (idx==3) {
                                Plot_Args$scales$tck <<- c(1,1)
                                Plot_Args$scales$alternating <<- 3
                             } else if (idx==4 || idx==5) {
                                Plot_Args$scales$relation <<- "free"
                                if (tclvalue(TKs)=="Custom X") {
                                   CustomDta <- list(Xlim[1], Xlim[2], "X")
                                   CustomAx(CustomDta)
                                }
                                if (tclvalue(TKs)=="Custom Y") {
                                   txt1="1) Ymin, Ymax and the number of ticks on the Y axis: es. Ymin=0, Ymax=35, Nticks=7"
                                   txt2="2) Set Tick-Labels (as many labels as the ticks): es. Tick Labels= 0,5, ,15,20, ,30"
                                   CustomDta <- list(Ylim[1], Ylim[2], "Y")
                                   CustomAx(CustomDta)
                                }
                             }
                             CtrlPlot()
                 })

       T1frame7 <- ttklabelframe(AxGroup, text = "SCALE", borderwidth=3)
       tkgrid(T1frame7, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
       ScaleTypes <- c("Standard", "Xpower", "Ypower", "Log10 X", "Log10 Y", "Ln X", "Ln Y", "X E10", "Y E10", "X ^10", "Y ^10")
       SCALETY <- tclVar("Standard")
       T1obj7 <- ttkcombobox(T1frame7, width = 15, textvariable = SCALETY, values = ScaleTypes)
       tkgrid(T1obj7, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T1obj7, "<<ComboboxSelected>>", function(){
                             Xlabel <<- FName[[SpectIndx]]@units[1]
                             Ylabel <<- FName[[SpectIndx]]@units[2]
                             idx <- charmatch(tclvalue(SCALETY),ScaleTypes) #charmatch does not interporet math symbols
                             if (idx == 1) {   #Standard
                                 Plot_Args$xlab$label <<- Xlabel
                                 Plot_Args$ylab$label <<- Ylabel
                                 Plot_Args$scales$x$log <<- FALSE
                                 Plot_Args$xscale.components <<- xscale.components.subticks
                                 Plot_Args$scales$y$log <<- FALSE
                                 Plot_Args$yscale.components <<- yscale.components.subticks
                             } else if (idx == 2) {  # X power scale
                                 Plot_Args$scales$x$log <<- 10
                                 Plot_Args$xscale.components <<- xscale.components.logpower
                             } else if (idx == 3) {  # Y power scale
                                 Plot_Args$scales$y$log <<- 10
                                 Plot_Args$yscale.components <<- yscale.components.logpower
                             } else if (idx == 4) {  #Log10 X
                                 if (tclvalue(REVAX) == "1"){
                                     tkmessageBox(message="X-axis inverted. Please uncheck Reverse_Xaxis", title="Xaxis REVERSED", icon="warning")
                                     return()
                                 }
                                 Xlim <- sort(Xlim)
                                 if (Xlim[1] <= 0) {
                                     tkmessageBox(message="Cannot plot negative or zero X-values !", title="WRONG X VALUES", icon="warning")
                                     return()
                                 }
                                 Plot_Args$xlab$label <<- paste("Log(",Xlabel, ")", sep="")
                                 Plot_Args$scales$x$log <<- 10
                                 Plot_Args$xscale.components <<- xscale.components.log10ticks
                             } else if (idx == 5) {  #Log10 Y
                                 Ylim <- sort(Ylim)
                                 if (Ylim[1] <= 0) {
                                     tkmessageBox(message="Cannot plot negative or zero Y-values !", title="WRONG Y VALUES", icon="warning")
                                     return()
                                 }
                                 Plot_Args$ylab$label <<- paste("Log(",Ylabel, ")", sep="")
                                 Plot_Args$scales$y$log <<- 10
                                 Plot_Args$yscale.components <<- yscale.components.log10ticks
                             } else if (idx == 6) {  #Ln X
                                 if (tclvalue(REVAX) == "1"){
                                     tkmessageBox(message="X-axis inverted. Please uncheck Reverse_Xaxis", title="Xaxis REVERSED", icon="warning")
                                     return()
                                 }
                                 Xlim <- sort(Xlim)
                                 if (Xlim[1] <= 0) {
                                     tkmessageBox(message="Cannot plot negatige or zero X-values !", title="WRONG X VALUES", icon="warning")
                                     return()
                                 }
                                 Plot_Args$xlab$label <<- paste("Ln(",Xlabel, ")", sep="")
                                 Plot_Args$scales$x$log <<- "e"
                                 Plot_Args$xscale.components <<- xscale.components.subticks
                             } else if (idx == 7) {  #Ln Y"
                                 Ylim <- sort(Ylim)
                                 if (Ylim[1] <= 0) {
                                     tkmessageBox(message="Cannot plot negatige or zero Y-values !", title="WRONG Y VALUES", icon="warning")
                                     return()
                                 }
                                 Plot_Args$ylab$label <<- paste("Ln(",Ylabel, ")", sep="")
                                 Plot_Args$scales$y$log <<- "e"
                                 Plot_Args$yscale.components <<- yscale.components.subticks
                             } else if (idx == 8){ #X E+n
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
                             } else if (idx == 9){ #Y E+n
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
                             } else if (idx == 10){ #X ^10"
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
                             } else if (idx == 11){ #Y ^10
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

       T1frame9 <- ttklabelframe(AxGroup, text = "TITLE SIZE", borderwidth=3)
       tkgrid(T1frame9, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
       TITSIZE <- tclVar("1.4")
       T1obj9 <- ttkcombobox(T1frame9, width = 15, textvariable = TITSIZE, values = FontSize)
       tkgrid(T1obj9, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T1obj9, "<<ComboboxSelected>>", function(){
                             Plot_Args$main$cex <<- as.numeric(tclvalue(TITSIZE))
                             CtrlPlot()
                  })

       T1frame10 <- ttklabelframe(AxGroup, text = "CHANGE TITLE", borderwidth=3)
       tkgrid(T1frame10, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
       NEWTITLE <- tclVar("")  #sets the initial msg
       EnterTitle <- ttkentry(T1frame10, textvariable=NEWTITLE, width=20)
       tkgrid(EnterTitle, row = 1, column = 1, padx = 5, pady = 5, sticky="we")
       #now ttkentry waits for a return to read the entry_value
       tkbind(EnterTitle, "<FocusIn>", function(K){
                             tkconfigure(EnterTitle, foreground="red")
                  })
       tkbind(EnterTitle, "<Key-Return>", function(K){
                             tkconfigure(EnterTitle, foreground="black")
                             Plot_Args$main$label <<- tclvalue(NEWTITLE)
                             CtrlPlot()
                  })

       T1frame11 <- ttklabelframe(AxGroup, text = "AXIS SCALE SIZE", borderwidth=3)
       tkgrid(T1frame11, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
       AXNUMSIZE <- tclVar(1)
       T1obj11 <- ttkcombobox(T1frame11, width = 15, textvariable = AXNUMSIZE, values = FontSize)
       tkgrid(T1obj11, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T1obj11, "<<ComboboxSelected>>", function(){
                             Plot_Args$scales$cex <<- as.numeric(tclvalue(AXNUMSIZE))
                             CtrlPlot()
                  })

       T1frame12 <- ttklabelframe(AxGroup, text = "AXIS LABEL SIZE", borderwidth=3)
       tkgrid(T1frame12, row = 4, column = 2, padx = 5, pady = 5, sticky="w")
       LBSIZE <- tclVar(1)
       T1obj12 <- ttkcombobox(T1frame12, width = 15, textvariable = LBSIZE, values = FontSize)
       tkgrid(T1obj12, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T1obj12, "<<ComboboxSelected>>", function(){
                             Plot_Args$xlab$cex <<- as.numeric(tclvalue(LBSIZE))
                             Plot_Args$ylab$cex <<- as.numeric(tclvalue(LBSIZE))
                             CtrlPlot()
                  })

       T1frame14 <- ttklabelframe(AxGroup, text = "CHANGE X-LABEL", borderwidth=3)
       tkgrid(T1frame14, row = 5, column = 1, padx = 5, pady = 5, sticky="w")
       NEWXAXLAB <- tclVar()
       T1obj14 <- ttkentry(T1frame14, textvariable=NEWXAXLAB, width=20)
       tkgrid(T1obj14, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T1obj14, "<FocusIn>", function(K){
                             tkconfigure(T1obj14, foreground="red")
                  })
       tkbind(T1obj14, "<Key-Return>", function(K){
                             tkconfigure(T1obj14, foreground="black")
                             Plot_Args$xlab$label <<- tclvalue(NEWXAXLAB)
                             Xlabel <<- tclvalue(NEWXAXLAB)
                             CtrlPlot()
                  })

       T1frame15 <- ttklabelframe(AxGroup, text = "CHANGE Y-LABEL", borderwidth=3)
       tkgrid(T1frame15, row = 5, column = 2, padx = 5, pady = 5, sticky="w")
       NEWYAXLAB <- tclVar()
       T1obj15 <- ttkentry(T1frame15, textvariable=NEWYAXLAB, width=20)
       tkgrid(T1obj15, row = 1, column = 1, padx = 5, pady = 5, sticky="we")
       tkbind(T1obj15, "<FocusIn>", function(K){
                             tkconfigure(T1obj15, foreground="red")
                  })
       tkbind(T1obj15, "<Key-Return>", function(K){
                             tkconfigure(T1obj15, foreground="black")
                             Plot_Args$ylab$label <<- tclvalue(NEWYAXLAB)
                             Ylabel <<- tclvalue(NEWYAXLAB)
                             CtrlPlot()
                  })
                  
       PlotButt <- tkbutton(AxGroup, text="  PLOT  ", width=15, command=function(){
                             LoadCoreLine()
                             CtrlPlot()
                  })
       tkgrid(PlotButt, row = 6, column = 1, padx = 5, pady = 5, sticky="w")


#--- tab1 OptnGroup groups third column widgets
       OptnGroup <- ttkframe(T1group1, borderwidth=0, padding=c(0,0,0,0))
       tkgrid(OptnGroup, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

       T1frame3 <- ttklabelframe(OptnGroup, text = "REVERSE X-axis", borderwidth=3)
       tkgrid(T1frame3, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       REVAX <- tclVar(TRUE)   #starts with cleared buttons
       RevAxis <- tkcheckbutton(T1frame3, text="Reverse X AXIS", variable=REVAX, onvalue = 1, offvalue = 0,
                           command=function(){
                             if ( tclvalue(REVAX) == 1) {   #reverse scale if checkbox TRUE
                                Xlim <<- sort(Xlim, decreasing=TRUE)
                                Plot_Args$xlim <<- Xlim
                             } else {
                                Xlim <<- sort(Xlim, decreasing=FALSE)
                                Plot_Args$xlim <<- Xlim
                             }
                             CtrlPlot()
                 })
       tkgrid(RevAxis, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

       FrameAxLabOrient <- ttklabelframe(OptnGroup, text = "AXIS LABEL ORIENTATION", borderwidth=3)
       tkgrid(FrameAxLabOrient, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
       LBOrient <- c("Horizontal","Rot-20","Rot-45","Rot-70","Vertical","Parallel","Normal")
       LBORI <- tclVar("Horizontal")
       AxLabOrient <- ttkcombobox(FrameAxLabOrient, width = 15, textvariable = LBORI, values = LBOrient)
       tkgrid(AxLabOrient, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(AxLabOrient, "<<ComboboxSelected>>", function(){
                             LabOrient <- tclvalue(LBORI)
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

       T1frame13 <- ttklabelframe(OptnGroup, text = "XY RANGE", borderwidth=3)
       tkgrid(T1frame13, row = 3, column = 1, padx = 5, pady = 1, sticky="w")
       RNGIDX <- tclVar()
       items <- c("Original XYrange", "Fitted XY range")
       for(ii in 1:2){
           T1obj13 <- ttkradiobutton(T1frame13, text=items[ii], variable=RNGIDX, value=items[ii],
                             command=function(){
                             if (length(tclvalue(RNGIDX)) == 0) {
                                tkmessageBox(meggage="Please Select the Core Line!" , title = "No spectral Data selected",  icon = "warning")
                                return()
                             } else {
#                                tclvalue(RngIdx) <- "Fitted XY range"
                                SetXYplotData()
                             }
                             CtrlPlot()
                  })
           tkgrid(T1obj13, row = ii, column = 1, padx = 5, pady = 1, sticky="w")
       }

       NORM <- tclVar(FALSE)
       objFunctNorm <- tkcheckbutton(T1frame13, text="Normalize", variable=NORM, onvalue = 1, offvalue = 0,
                           command=function(){
                             if (tclvalue(NORM) == "1") {   #reverse scale if checkbox TRUE
                                 Normalize <<- TRUE
                             }
                             SetXYplotData()
                  })
       tkgrid(objFunctNorm, row = 3, column = 1, padx = 5, pady = 1, sticky="w")


       T1frame16 <- ttklabelframe(OptnGroup, text = "EXACT RANGE VALUES", borderwidth=3)
       tkgrid(T1frame16, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
       XMIN <- tclVar("Xmin= ")
       XX1 <- ttkentry(T1frame16, textvariable=XMIN, width=10, foreground="grey")
       tkbind(XX1, "<FocusIn>", function(K){
                         tkconfigure(XX1, foreground="red")
                         tclvalue(XMIN) <- ""
                     })
       tkbind(XX1, "<Key-Return>", function(K){
                         tkconfigure(XX1, foreground="black")
                         Xmin <- as.numeric(tclvalue(XMIN))
                     })
       tkgrid(XX1, row = 1, column = 1, padx=5, pady=5, sticky="w")

       XMAX <- tclVar("Xmax= ")
       XX2 <- ttkentry(T1frame16, textvariable=XMAX, width=10, foreground="grey")
       tkbind(XX2, "<FocusIn>", function(K){
                         tkconfigure(XX2, foreground="red")
                         tclvalue(XMAX) <- ""
                     })
       tkbind(XX2, "<Key-Return>", function(K){
                         tkconfigure(XX2, foreground="black")
                         Xmax <- as.numeric(tclvalue(XMAX))
                     })
       tkgrid(XX2, row = 1, column = 2, padx=5, pady=5, sticky="w")

       YMIN <- tclVar("Ymin= ")
       YY1 <- ttkentry(T1frame16, textvariable=YMIN, width=10, foreground="grey")
       tkbind(YY1, "<FocusIn>", function(K){
                         tkconfigure(YY1, foreground="red")
                         tclvalue(YMIN) <- ""
                     })
       tkbind(YY1, "<Key-Return>", function(K){
                         tkconfigure(YY1, foreground="black")
                         Ymin <- as.numeric(tclvalue(YMIN))
                     })
       tkgrid(YY1, row = 2, column = 1, padx=5, pady=5, sticky="w")

       YMAX <- tclVar("Ymax= ")
       YY2 <- ttkentry(T1frame16, textvariable=YMAX, width=10, foreground="grey")
       tkbind(YY2, "<FocusIn>", function(K){
                         tkconfigure(YY2, foreground="red")
                         tclvalue(YMAX) <- ""
                     })
       tkbind(YY2, "<Key-Return>", function(K){
                         tkconfigure(YY2, foreground="black")
                         Ymax <- as.numeric(tclvalue(YMAX))
                     })
       tkgrid(YY2, row = 2, column=2, padx=5, pady=5, sticky="w")

       OKButtn <- tkbutton(T1frame16, text=" OK ", width=12, command=function(){
                             setRange()
                             CtrlPlot()
                     })
       tkgrid(OKButtn, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

# --- Tab2: Spectrum Options ---
       T2group1 <- ttkframe(NoteBk,  borderwidth=2, padding=c(5,5,5,5) )
       tkadd(NoteBk, T2group1, text="SPECTRUM OPTIONS")

       T2frame1 <- ttklabelframe(T2group1, text = "SET SPECTRUM COLOR", borderwidth=3)
       tkgrid(T2frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

       tkgrid( ttklabel(T2frame1, text="Double click to change colors"),
             row = 1, column = 1, padx = 5, pady = 5)

       #building the widget to change CL colors
       T2SpctCol <- ttklabel(T2frame1, text=as.character(1), width=6, font="Serif 8", background="black")
       tkgrid(T2SpctCol, row = ii, column = 1, padx = c(5,0), pady = 1, sticky="w")
       tkbind(T2SpctCol, "<Double-1>", function( ){
                             X <- as.numeric(tkwinfo("pointerx", CustomWindow))
                             Y <- as.numeric(tkwinfo("pointery", CustomWindow))
                             WW <- tkwinfo("containing", X, Y)
                             BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                             BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                             BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                             tkconfigure(T2SpctCol, background=BKGcolor)
                             SpectCol <<- BKGcolor
                             Plot_Args$col <<- BKGcolor
                             SetXYplotData()
                     })

       T2GroupOptn <- ttkframe(T2group1, borderwidth=0, padding=c(0,0,0,0) )
       tkgrid(T2GroupOptn, row = 1, column = 2, padx = 0, pady = 0, sticky="w")

       T2frame4 <- ttklabelframe(T2GroupOptn, text = "SET LINES", borderwidth=3)
       tkgrid(T2frame4, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       LINEONOFF <- tclVar("ON")
       items <- c("ON", "OFF")
       for(ii in 1:2){
           T2obj4 <- ttkradiobutton(T2frame4, text=items[ii], variable=LINEONOFF, value=items[ii],
                              command=function(){
                                  SetXYplotData()
                              })
           tkgrid(T2obj4, row = 1, column = ii, padx = 5, pady=5, sticky="w")
       }

       T2frame7 <- ttklabelframe(T2GroupOptn, text = "SET SYMBOLS", borderwidth=3)
       tkgrid(T2frame7, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
       SYMONOFF <- tclVar("OFF")
       items <- c("ON", "OFF")
       for(ii in 1:2){
           T2obj7 <- ttkradiobutton(T2frame7, text=items[ii], variable=SYMONOFF, value=items[ii],
                              command=function(){
                                  SetXYplotData()
                              })
           tkgrid(T2obj7, row = 1, column = ii, padx = 5, pady=5, sticky="w")
       }

       T2frame2 <- ttklabelframe(T2GroupOptn, text = "LINE TYPE", borderwidth=3)
       tkgrid(T2frame2, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
       SPECTLTY <- tclVar("Solid")
       T2obj2 <- ttkcombobox(T2frame2, width = 15, textvariable = SPECTLTY, values = LineTypes)
       tkgrid(T2obj2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T2obj2, "<<ComboboxSelected>>", function(){
#                              Plot_Args$type <<- "l"
#                              Plot_Args$lty <<- tclvalue(SPECTLTY)
#                              Plot_Args$par.settings = list(superpose.line=list(col=Colors, lwd=1)) #needed to set legend colors
                              AutoKey_Args$lines <<- TRUE
                              AutoKey_Args$points <<- FALSE
                              if (tclvalue(LINEONOFF) == "ON") SetXYplotData()
                     })

       T2frame3 <- ttklabelframe(T2GroupOptn, text = "LINE WIDTH", borderwidth=3)
       tkgrid(T2frame3, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
       SPECTLWD <- tclVar("1")
       T2obj3 <- ttkcombobox(T2frame3, width = 15, textvariable = SPECTLWD, values = LWidth)
       tkgrid(T2obj3, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T2obj3, "<<ComboboxSelected>>", function(){
                              Plot_Args$lwd <<- as.numeric(tclvalue(SPECTLWD))
                              if (tclvalue(LINEONOFF) == "ON") SetXYplotData()
                     })


       T2frame5 <- ttklabelframe(T2GroupOptn, text = "SYMBOL", borderwidth=3)
       tkgrid(T2frame5, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
       SPECTSYM <- tclVar("VoidCircle")
       T2obj5 <- ttkcombobox(T2frame5, width = 15, textvariable = SPECTSYM, values = SType)
       tkgrid(T2obj5, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T2obj5, "<<ComboboxSelected>>", function(){
                              Plot_Args$type <<- "p"
                              idx <- grep(tclvalue(SPECTSYM), SType)
                              Plot_Args$pch <<- STypeIndx[idx]
                              Plot_Args$par.settings = list(superpose.symbol=list(col=Colors))
                              AutoKey_Args$lines <<- FALSE
                              AutoKey_Args$points <<- TRUE
                              if (tclvalue(SYMONOFF)=="ON") SetXYplotData()
                     })

       T2frame6 <- ttklabelframe(T2GroupOptn, text = "SYMSIZE", borderwidth=3)
       tkgrid(T2frame6, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
       SPECTSYMSIZE <- tclVar("0.8")
       T2obj6 <- ttkcombobox(T2frame6, width = 15, textvariable = SPECTSYMSIZE, values = SymSize)
       tkgrid(T2obj6, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T2obj6, "<<ComboboxSelected>>", function(){
                              Plot_Args$cex <<- as.numeric(tclvalue(SPECTSYMSIZE))
                              if (tclvalue(SYMONOFF)=="ON") SetXYplotData()
                     })

       if (ErrData == TRUE){
           T2frame8 <- ttklabelframe(T2GroupOptn, text = "ERROR BARS", borderwidth=3)
           tkgrid(T2frame8, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
           txt <- paste("Only generic X, Y data with Standard Errors loaded with XPSImport.asciiGUI()\n",
                        "can be plotted using the error options", sep="")
           tkgrid( ttklabel(T2frame8, text=txt), row = 1, column = 1, padx = 5, pady=5, sticky="w")
           T2ckboxGroup <- ttkframe(T2frame8, borderwidth=0, padding=c(0,0,0,0))
           tkgrid(T2ckboxGroup, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
           txt <- c("Upper Bar", "Lower Bar", "Upper End", "Lower End")
           for(ii in 1:length(txt)) {
               ErrBar <- tkcheckbutton(T2ckboxGroup, text=txt[ii], variable=txt[ii], onvalue = ii, offvalue = 0,
                                   command=function(){
                                   for(jj in length(txt):1){
                                       SetErrBars[jj] <<- as.numeric(tclvalue(txt[jj]))
                                       if (SetErrBars[jj] == 0) { SetErrBars <<- SetErrBars[-jj] }
                                   }
                                   CtrlPlot()
                           })
               tclvalue(txt[ii]) <- FALSE   #initial cehckbutton setting
               tkgrid(ErrBar, row = 1, column = ii,  padx = 5, pady = 5, sticky="w")
           }
           tkgrid( ttklabel(T2frame8, text="Error Ends Amplitude"),row = 3, column = 1,  padx = 5, pady =c(5, 0), sticky="w")

           ERRENDS <- tclVar("End Extension = 0.5")  #sets the initial msg
           T2obj9 <- ttkentry(T2frame8, textvariable=ERRENDS, width=15, foreground="grey")
           tkbind(T2obj9, "<FocusIn>", function(K){
                                   tkconfigure(T2obj9, foreground="red")
                                   tclvalue(ERRENDS) <- ""
                         })
           tkbind(T2obj9, "<Key-Return>", function(K){
                                   tkconfigure(T2obj9, foreground="black")
                                   CtrlPlot()
                         })
           tkgrid(T2obj9, row = 4, column=1, padx = 5, pady =c(0, 5), sticky="w")
       }

# --- Tab3: BaseLine Options ---

       T3group1 <- ttkframe(NoteBk,  borderwidth=2, padding=c(5,5,5,5) )
       tkadd(NoteBk, T3group1, text="BASELINE OPTIONS")

       T3CKframe <- ttklabelframe(T3group1, text = "Set BaseLine", borderwidth=3)
       tkgrid(T3CKframe, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       BLINEONOFF <- tclVar(FALSE)
       BaseLineCK <- tkcheckbutton(T3CKframe, text="Base Line ON/OFF", variable=BLINEONOFF, onvalue = 1, offvalue = 0,
                               command=function(){
                                  if (tclvalue(BLINEONOFF) == 1) {
                                      if (hasBaseline(FName[[SpectIndx]])==FALSE){
                                          tkmessageBox(message="Sorry No Baseline Found!" , title = "PLOTTING BASELINE INTERRUPTED",  icon = "warning")
                                          tclvalue(BLINEONOFF) <- 0
                                          return()
                                      }
                                      WidgetState(T3frame2, "normal")
                                      WidgetState(T3frame3, "normal")
                                      WidgetState(T3frame4, "normal")
                                      WidgetState(T3frame5, "normal")
                                  } else if (tclvalue(BLINEONOFF) == 0) {
                                      WidgetState(T3frame2, "disabled")
                                      WidgetState(T3frame3, "disabled")
                                      WidgetState(T3frame4, "disabled")
                                      WidgetState(T3frame5, "disabled")
                                  }
                                  SetXYplotData()
                     })
       tkgrid(BaseLineCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

       T3frame1 <- ttklabelframe(T3group1, text = "SET BASELINE COLOR", borderwidth=3)
       tkgrid(T3frame1, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

       tkgrid( ttklabel(T3frame1, text="Double click to change colors"),
             row = 1, column = 1, padx = 5, pady = 5)

       #building the widget to change CL colors
       T3BsLnCol <- ttklabel(T3frame1, text=as.character(1), width=6, font="Serif 8", background="sienna")
       tkgrid(T3BsLnCol, row = ii, column = 1, padx = c(5,0), pady = 1, sticky="w")
       tkbind(T3BsLnCol, "<Double-1>", function( ){
                             X <- as.numeric(tkwinfo("pointerx", CustomWindow))
                             Y <- as.numeric(tkwinfo("pointery", CustomWindow))
                             WW <- tkwinfo("containing", X, Y)
                             BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                             BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                             BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                             tkconfigure(T3BsLnCol, background=BKGcolor)
                             BLineCol <<- BKGcolor
                             SetXYplotData()
                     })

       T3frame2 <- ttklabelframe(T3group1, text = "LINE TYPE", borderwidth=3)
       tkgrid(T3frame2, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
       BLINELTY <- tclVar("Solid")
       T3obj2 <- ttkcombobox(T3frame2, width = 15, textvariable = BLINELTY, values = LineTypes)
       tkgrid(T3obj2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T3obj2, "<<ComboboxSelected>>", function(){
                               Plot_Args$type <<- "l"
                               SetXYplotData() })

       T3frame3 <- ttklabelframe(T3group1, text = "LINE WIDTH", borderwidth=3)
       tkgrid(T3frame3, row = 2, column = 3, padx = 5, pady = 5, sticky="w")
       BLINELWD <- tclVar("1")
       T3obj3 <- ttkcombobox(T3frame3, width = 15, textvariable = BLINELWD, values = LWidth)
       tkgrid(T3obj3, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T3obj3, "<<ComboboxSelected>>", function(){
                               SetXYplotData()
                     })

       T3frame4 <- ttklabelframe(T3group1, text = "SYMBOL", borderwidth=3)
       tkgrid(T3frame4, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
       BLINESYM <- tclVar("VoidCircle")
       T3obj4 <- ttkcombobox(T3frame4, width = 15, textvariable = BLINESYM, values = SType)
       tkgrid(T3obj4, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T3obj4, "<<ComboboxSelected>>", function(){
                               Plot_Args$type <<- "p"
                               SetXYplotData()
                     })

       T3frame5 <- ttklabelframe(T3group1, text = "SYMSIZE", borderwidth=3)
       tkgrid(T3frame5, row = 3, column = 3, padx = 5, pady = 5, sticky="w")
       BLSYMSIZE <- tclVar("0.8")
       T3obj5 <- ttkcombobox(T3frame5, width = 15, textvariable = BLSYMSIZE, values = SymSize)
       tkgrid(T3obj5, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T3obj5, "<<ComboboxSelected>>", function(){
                               Plot_Args$cex <<- c(tclvalue(BLSYMSIZE), tclvalue(SPECTSYM))
                               SetXYplotData()
                     })

# --- Tab4: Fit Components Options ---

       T4group1 <- ttkframe(NoteBk,  borderwidth=2, padding=c(5,5,5,5) )
       tkadd(NoteBk, T4group1, text="FIT COMPONENT OPTIONS")

       FitCompGroup1 <- ttkframe(T4group1, borderwidth=0, padding=c(0,0,0,0) )
       tkgrid(FitCompGroup1, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

       T4FitCmpOKframe <- ttklabelframe(FitCompGroup1, text = "Set Components", borderwidth=3)
       tkgrid(T4FitCmpOKframe, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       FCOMPONOFF <- tclVar(FALSE)
       ComponentCK <- tkcheckbutton(T4FitCmpOKframe, text="Components ON/OFF", variable=FCOMPONOFF, onvalue = 1, offvalue = 0,
                               command=function(){
                                  if (tclvalue(FCOMPONOFF) == 1) {
                                      if (hasComponents(FName[[SpectIndx]])==FALSE) {
                                         tkmessageBox(message="Sorry No Fit Found!" , title = "PLOTTING FIT INTERRUPTED",  icon = "warning")
                                         tclvalue(FCOMPONOFF) <- 0
                                         return()
                                      }
                                      WidgetState(T4frame2, "normal")
                                      WidgetState(T4frame3, "normal")
                                      WidgetState(T4frame4, "normal")
                                      WidgetState(T4frame5, "normal")
                                  } else if (tclvalue(BLINEONOFF) == 0) {
                                      WidgetState(T4frame2, "disabled")
                                      WidgetState(T4frame3, "disabled")
                                      WidgetState(T4frame4, "disabled")
                                      WidgetState(T4frame5, "disabled")
                                  }
                                  SetXYplotData()
                     })
       tkgrid(ComponentCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

       T4FCColframe <- ttklabelframe(FitCompGroup1, text = "Fit Comp. Color", borderwidth=3)
       tkgrid(T4FCColframe, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
       MonoPolyCOL <- tclVar("PolyChrome")
       T4_MonoPoly_Col <- ttkcombobox(T4FCColframe, width = 15, textvariable = MonoPolyCOL, values = c("MonoChrome", "PolyChrome"))
       tkbind(T4_MonoPoly_Col, "<<ComboboxSelected>>", function(){
                            if(tclvalue(MonoPolyCOL) == "MonoChrome") {
                               FCompCol <<- rep("blue", NComp)
                               SetXYplotData()
                               tkdestroy(T4frame1)
                               T4frame1 <<- ttklabelframe(FitCompGroup2, text = "SET FIT COOMPONENT PALETTE", borderwidth=3)
                               tkgrid(T4frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

                               tkgrid( ttklabel(T4frame1, text="Double click to change colors"),
                                       row = 1, column = 1, padx = 5, pady = 5)
                               #building the widget to change CL colors
                               FitCmpClr[[1]] <- ttklabel(T4frame1, text="1", width=6, font="Serif 8", background="blue")
                               tkgrid(FitCmpClr[[1]], row = 2, column = 1, padx = c(5,0), pady = 1, sticky="w")
                               tkbind(FitCmpClr[[1]], "<Double-1>", function( ){
                                                     X <- as.numeric(tkwinfo("pointerx", CustomWindow))
                                                     Y <- as.numeric(tkwinfo("pointery", CustomWindow))
                                                     WW <- tkwinfo("containing", X, Y)
                                                     BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                                                     BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                                                     BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                                                     tkconfigure(FitCmpClr[[1]], background=BKGcolor)
                                                     FCompCol <<- rep(BKGcolor, NComp)
                                                     SetXYplotData()
                                         })
                            } else if(tclvalue(MonoPolyCOL) == "PolyChrome") {
                               FCompCol <<- XPSSettings$ComponentsColor
                               SetXYplotData()
                               tkdestroy(T4frame1)
                               T4frame1 <<- ttklabelframe(FitCompGroup2, text = "SET FIT COOMPONENT PALETTE", borderwidth=3)
                               tkgrid(T4frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

                               tkgrid( ttklabel(T4frame1, text="Double click to change colors"),
                                       row = 1, column = 1, padx = 5, pady = 5)
                               #building the widget to change CL colors
                               FCompCol <<- XPSSettings$ComponentsColor
                               SetXYplotData()
                               for(ii in 1:10){ #column1 colors 1 - 20
                                   FitCmpClr[[ii]] <- ttklabel(T4frame1, text=as.character(ii), width=6, font="Serif 8", background=FCompCol[ii])
                                   tkgrid(FitCmpClr[[ii]], row = (ii+1), column = 1, padx = c(5,0), pady = 1, sticky="w")
                                   tkbind(FitCmpClr[[ii]], "<Double-1>", function( ){
                                                     X <- as.numeric(tkwinfo("pointerx", CustomWindow))
                                                     Y <- as.numeric(tkwinfo("pointery", CustomWindow))
                                                     WW <- tkwinfo("containing", X, Y)
                                                     BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                                                     BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                                                     colIdx <- grep(BKGcolor, FCompCol) #
                                                     BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                                                     tkconfigure(FitCmpClr[[colIdx]], background=BKGcolor)
                                                     FCompCol[colIdx] <<- BKGcolor
                                                     Plot_Args$col <<- FCompCol
                                                     SetXYplotData()
                                         })
                               }
                               for(ii in 1:10){ #column1 colors 1 - 20
                                   FitCmpClr[[(ii+10)]] <- ttklabel(T4frame1, text=as.character(ii+10), width=6, font="Serif 8", background=FCompCol[(ii+10)])
                                   tkgrid(FitCmpClr[[(ii+10)]], row = (ii+1), column = 1, padx = c(80,0), pady = 1, sticky="w")
                                   tkbind(FitCmpClr[[(ii+10)]], "<Double-1>", function( ){
                                                     X <- as.numeric(tkwinfo("pointerx", CustomWindow))
                                                     Y <- as.numeric(tkwinfo("pointery", CustomWindow))
                                                     WW <- tkwinfo("containing", X, Y)
                                                     BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                                                     BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                                                     colIdx <- grep(BKGcolor, FCompCol) #
                                                     BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                                                     tkconfigure(FitCmpClr[[colIdx]], background=BKGcolor)
                                                     FCompCol[colIdx] <<- BKGcolor
                                                     Plot_Args$col <<- FCompCol
                                                     SetXYplotData()
                                         })
                               }
                            }                     })
       tkgrid(T4_MonoPoly_Col, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

       T4Lblframe <- ttklabelframe(FitCompGroup1, text = "Component Labels", borderwidth=3)
       tkgrid(T4Lblframe, row = 1, column = 3, padx = 5, pady = 5, sticky="w")
       FCLBLONOFF <- tclVar(FALSE)
       LabelCK <- tkcheckbutton(T4Lblframe, text="Labels ON/OFF", variable=FCLBLONOFF, onvalue = 1, offvalue = 0,
                               command=function(){
                                  if (tclvalue(FCLBLONOFF) == 1 && tclvalue(FCOMPONOFF) == 0) {
                                      tkmessageBox(message="Please Enable Plotting the Fit Components" , title = "PLOTTING LABELS INTERRUPTED",  icon = "warning")
                                      return()
                                  }
                                  SetXYplotData()
                     })
       tkgrid(LabelCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

       FitCompGroup2 <- ttkframe(T4group1, borderwidth=0, padding=c(0,0,0,0) )
       tkgrid(FitCompGroup2, row = 2, column = 1, padx = 0, pady = 0, sticky="w")

       T4frame1 <- ttklabelframe(FitCompGroup2, text = "SET FIT COOMPONENT PALETTE", borderwidth=3)
       tkgrid(T4frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

       tkgrid( ttklabel(T4frame1, text="Double click to change colors"),
             row = 1, column = 1, padx = 5, pady = 5)

       #building the widget to change CL colors
       for(ii in 1:10){ #column1 colors 1 - 20
           FitCmpClr[[ii]] <- ttklabel(T4frame1, text=as.character(ii), width=6, font="Serif 8", background=FCompCol[ii])
           tkgrid(FitCmpClr[[ii]], row = (ii+1), column = 1, padx = c(5,0), pady = 1, sticky="w")
           tkbind(FitCmpClr[[ii]], "<Double-1>", function(){
                             X <- as.numeric(tkwinfo("pointerx", CustomWindow))
                             Y <- as.numeric(tkwinfo("pointery", CustomWindow))
                             WW <- tkwinfo("containing", X, Y)
                             BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                             BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                             colIdx <- grep(BKGcolor, FCompCol)
                             BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                             tkconfigure(FitCmpClr[[colIdx]], background=BKGcolor)
                             FCompCol[colIdx] <<- BKGcolor
                             Plot_Args$col <<- FCompCol
                             SetXYplotData()
                     })
       }
       for(ii in 1:10){ #column1 colors 1 - 20
           FitCmpClr[[(ii+10)]] <- ttklabel(T4frame1, text=as.character(ii+10), width=6, font="Serif 8", background=FCompCol[(ii+10)])
           tkgrid(FitCmpClr[[(ii+10)]], row = (ii+1), column = 1, padx = c(80,0), pady = 1, sticky="w")
           tkbind(FitCmpClr[[(ii+10)]], "<Double-1>", function( ){
                             X <- as.numeric(tkwinfo("pointerx", CustomWindow))
                             Y <- as.numeric(tkwinfo("pointery", CustomWindow))
                             WW <- tkwinfo("containing", X, Y)
                             BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                             BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                             colIdx <- grep(BKGcolor, FCompCol) #
                             BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                             tkconfigure(FitCmpClr[[colIdx]], background=BKGcolor)
                             FCompCol[colIdx] <<- BKGcolor
                             Plot_Args$col <<- FCompCol
                             SetXYplotData()
                     })
       }

       T4LynSymframe <- ttkframe(FitCompGroup2, borderwidth=0, padding=c(0,0,0,0) )
       tkgrid(T4LynSymframe, row = 1, column = 1, padx = c(230, 0), pady = 0, sticky="w")

       T4frame2 <- ttklabelframe(T4LynSymframe, text = "LINE TYPE", borderwidth=3)
       tkgrid(T4frame2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       FCOMPLTY <- tclVar("Solid")
       T4obj2 <- ttkcombobox(T4frame2, width = 15, textvariable = FCOMPLTY, values = LineTypes)
       tkgrid(T4obj2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T4obj2, "<<ComboboxSelected>>", function(){
                                  Plot_Args$type <<- "l"
                                  SetXYplotData()
                     })

       T4frame3 <- ttklabelframe(T4LynSymframe, text = "LINE WIDTH", borderwidth=3)
       tkgrid(T4frame3, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
       FCOMPLWD <- tclVar("1")
       T4obj3 <- ttkcombobox(T4frame3, width = 15, textvariable = FCOMPLWD, values = LWidth)
       tkgrid(T4obj3, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T4obj3, "<<ComboboxSelected>>", function(){
                                  SetXYplotData()
                     })

       T4frame4 <- ttklabelframe(T4LynSymframe, text = "SYMBOL", borderwidth=3)
       tkgrid(T4frame4, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
       FCOMPSYM <- tclVar("VoidCircle")
       T4obj4 <- ttkcombobox(T4frame4, width = 15, textvariable = FCOMPSYM, values = SType)
       tkgrid(T4obj4, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T4obj4, "<<ComboboxSelected>>", function(){
#                                  Plot_Args$type <<- "p"
                                  SetXYplotData()
                     })

       T4frame5 <- ttklabelframe(T4LynSymframe, text = "SYMSIZE", borderwidth=3)
       tkgrid(T4frame5, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
       FCSYMSIZE <- tclVar("0.8")
       T4obj5 <- ttkcombobox(T4frame5, width = 15, textvariable = FCSYMSIZE, values = SymSize)
       tkgrid(T4obj5, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T4obj5, "<<ComboboxSelected>>", function(){
#                                  Plot_Args$type <<- as.numeric(tclvalue(FCSYMSIZE))
                                  SetXYplotData()
                     })

# --- Tab5: Fit Options ---

       T5group1 <- ttkframe(NoteBk,  borderwidth=2, padding=c(5,5,5,5) )
       tkadd(NoteBk, T5group1, text="FIT OPTIONS")

       T5CKframe <- ttklabelframe(T5group1, text = "Set Components", borderwidth=3)
       tkgrid(T5CKframe, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       FITONOFF <- tclVar(FALSE)
       FitLineCK <- tkcheckbutton(T5CKframe, text="Fit ON/OFF", variable=FITONOFF, onvalue = 1, offvalue = 0,
                               command=function(){
                                  if (tclvalue(FITONOFF) == "1" ) {
                                      if (hasFit(FName[[SpectIndx]])==FALSE) {
                                         tkmessageBox(message="Sorry None Fit Found!" , title = "PLOTTING FIT INTERRUPTED",  icon = "warning")
                                         tclvalue(FITONOFF) <- 0
                                         return()
                                      }
                                      WidgetState(T5frame2, "normal")
                                      WidgetState(T5frame3, "normal")
                                      WidgetState(T5frame4, "normal")
                                      WidgetState(T5frame5, "normal")
                                  } else if (tclvalue(BLINEONOFF) == 0) {
                                      WidgetState(T5frame2, "disabled")
                                      WidgetState(T5frame3, "disabled")
                                      WidgetState(T5frame4, "disabled")
                                      WidgetState(T5frame5, "disabled")
                                  }
                                  SetXYplotData()
                     })
       tkgrid(FitLineCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

       T5frame1 <- ttklabelframe(T5group1, text = "COLOR", borderwidth=3)
       tkgrid(T5frame1, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

       tkgrid( ttklabel(T5frame1, text="Double click to change colors"),
             row = 1, column = 1, padx = 5, pady = 5)

       #building the widget to change CL colors
       T5FitCol <- ttklabel(T5frame1, text=as.character(1), width=6, font="Serif 8", background=FitCol)
       tkgrid(T5FitCol, row = ii, column = 1, padx = c(5,0), pady = 1, sticky="w")
       tkbind(T5FitCol, "<Double-1>", function( ){
                             X <- as.numeric(tkwinfo("pointerx", CustomWindow))
                             Y <- as.numeric(tkwinfo("pointery", CustomWindow))
                             WW <- tkwinfo("containing", X, Y)
                             BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                             BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                             BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                             tkconfigure(T5FitCol, background=BKGcolor)
                             FitCol <<- BKGcolor
                             SetXYplotData()
                     })

       T5frame2 <- ttklabelframe(T5group1, text = "LINE TYPE", borderwidth=3)
       tkgrid(T5frame2, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
       FITLTY <- tclVar("Solid")
       T5obj2 <- ttkcombobox(T5frame2, width = 15, textvariable = FITLTY, values = LineTypes)
       tkgrid(T5obj2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T5obj2, "<<ComboboxSelected>>", function(){
                                  SetXYplotData()
                     })

       T5frame3 <- ttklabelframe(T5group1, text = "LINE WIDTH", borderwidth=3)
       tkgrid(T5frame3, row = 2, column = 3, padx = 5, pady = 5, sticky="w")
       FITLWD <- tclVar("1")
       T5obj3 <- ttkcombobox(T5frame3, width = 15, textvariable = FITLWD, values = LWidth)
       tkgrid(T5obj3, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T5obj3, "<<ComboboxSelected>>", function(){
                                  SetXYplotData()
                     })

       T5frame4 <- ttklabelframe(T5group1, text = "SYMBOL", borderwidth=3)
       tkgrid(T5frame4, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
       FITSYM <- tclVar("VoidCircle")
       T5obj4 <- ttkcombobox(T5frame4, width = 15, textvariable = FITSYM, values = SType)
       tkgrid(T5obj4, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T5obj4, "<<ComboboxSelected>>", function(){
                                  SetXYplotData()
                     })

       T5frame5 <- ttklabelframe(T5group1, text = "SYMSIZE", borderwidth=3)
       tkgrid(T5frame5, row = 3, column = 3, padx = 5, pady = 5, sticky="w")
       FITSYMSIZE <- tclVar("0.8")
       T5obj5 <- ttkcombobox(T5frame5, width = 15, textvariable = FITSYMSIZE, values = SymSize)
       tkgrid(T5obj5, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(T5obj5, "<<ComboboxSelected>>", function(){
                                  SetXYplotData()
                     })


# --- Tab6: Legend Options ---

       T6group1 <- ttkframe(NoteBk,  borderwidth=2, padding=c(5,5,5,5) )
       tkadd(NoteBk, T6group1, text="LEGEND")

       Lframe1 <- ttklabelframe(T6group1, text = "Set Components", borderwidth=3)
       tkgrid(Lframe1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       LEGONOFF <- tclVar(FALSE)
       legendCK <- tkcheckbutton(Lframe1, text="Legend ON/OFF", variable=LEGONOFF, onvalue = 1, offvalue = 0,
                               command=function(){
                                  if (tclvalue(LEGONOFF) == "1") {
		           	                        Plot_Args$auto.key <<-AutoKey_Args
                                      if (tclvalue(LINEONOFF) == "ON") {
                                         Plot_Args$par.settings$superpose.line$col <<- tclvalue(SpectCol)
                                         Plot_Args$par.settings$superpose.line$lty <<- grep(tclvalue(SPECTLTY), LineTypes)
                                      }
                                      if (tclvalue(SYMONOFF) == "ON") {
                                         Plot_Args$par.settings$superpose.symbol$col <<- tclvalue(SpectCol)
                                         Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx[grep(tclvalue(SPECTSYM),SType)]
                                      }
                                  } else {
		                                    Plot_Args$auto.key <<- FALSE
	           	                     }
                                  CtrlPlot()
                     })
       tkgrid(legendCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

       Lframe2 <- ttklabelframe(T6group1, text = "Legend Position", borderwidth=3)
       tkgrid(Lframe2, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
       LEGENDPOS <- tclVar()
       LegPosCK <- ttkcombobox(Lframe2, width = 15, textvariable = LEGENDPOS, values = LegPos)
       tkgrid(LegPosCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(LegPosCK, "<<ComboboxSelected>>", function(){
			                               switch(tclvalue(LEGENDPOS),
                                      "OutsideCenterTop"    = { Plot_Args$auto.key$space <<-"top" },
				                                  "OutsideTopRight"     = { Plot_Args$auto.key$space <<-NULL
                                                                Plot_Args$auto.key$corner <<- c(1,1)
                                                                Plot_Args$auto.key$x <<-0.95
                                                                Plot_Args$auto.key$y <<-1.05 },
				                                  "OutsideTopLeft"      = { Plot_Args$auto.key$space <<-NULL
                                                                Plot_Args$auto.key$corner <<- c(0,1)
                                                                Plot_Args$auto.key$x <<-0.05
                                                                Plot_Args$auto.key$y <<-1.05 },
				                                  "OutsideCenterRight"  = { Plot_Args$auto.key$space <<-"right" },
				                                  "OutsideCenterLeft"   = { Plot_Args$auto.key$space <<-"left" },
			                                   "OutsideCenterBottom" = { Plot_Args$auto.key$space <<-"bottom" },
				                                  "InsideTopRight"      = { Plot_Args$auto.key$space <<-NULL
                                                                Plot_Args$auto.key$corner <<- c(1,1)
                                                                Plot_Args$auto.key$x <<-0.95
                                                                Plot_Args$auto.key$y <<-0.95 },
				                                  "InsideTopLeft"       = { Plot_Args$auto.key$space <<-NULL
                                                                Plot_Args$auto.key$corner <<- c(0,1)
                                                                Plot_Args$auto.key$x <<-0.05
                                                                Plot_Args$auto.key$y <<-0.95 },
                                      "InsideBottomRight"   = { Plot_Args$auto.key$space <<-NULL
                                                                Plot_Args$auto.key$corner <<- c(1,0)
                                                                Plot_Args$auto.key$x <<-0.95
                                                                Plot_Args$auto.key$y <<-0.05 },
				                                  "InsideBottomLeft"    = {	Plot_Args$auto.key$space <<-NULL
                                                                Plot_Args$auto.key$corner <<- c(0,0)
                                                                Plot_Args$auto.key$x <<-0.05
                                                                Plot_Args$auto.key$y <<-0.05 },
                                  )
                                  CtrlPlot()
                     })

       Lframe3 <- ttklabelframe(T6group1, text = "Legend Border", borderwidth=3)
       tkgrid(Lframe3, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
       LegBorder <- tclVar("No")
       BorderCK <- ttkcombobox(Lframe3, width = 15, textvariable = LegBorder, values = c("No", "Yes"))
       tkgrid(BorderCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(BorderCK, "<<ComboboxSelected>>", function(){
                                  if (tclvalue(LegBorder) == "No"){
                                      Plot_Args$auto.key$border <<- FALSE
                                  } else if (tclvalue(LegBorder) == "Yes"){
                                      Plot_Args$auto.key$border <<- TRUE
                                  }
                                  CtrlPlot()
                     })

       Lframe4 <- ttklabelframe(T6group1, text = "Line/Symbol weight", borderwidth=3)
       tkgrid(Lframe4, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
       LegWeigth <- tclVar("1")
       LineWdhCK <- ttkcombobox(Lframe4, width = 15, textvariable = LegWeigth, values = LineWdh)
       tkgrid(LineWdhCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(LineWdhCK, "<<ComboboxSelected>>", function(){
                                  weight <- as.numeric(tclvalue(LegWeigth))
                                  if (tclvalue(LINEONOFF) == "ON") {   #Selected Lines for plotting
                                      Plot_Args$par.settings$superpose.line$lty <<- grep(tclvalue(SPECTLTY), LineTypes)
                                      Plot_Args$par.settings$superpose.line$lwd <<- weight
                                  } else {
                                      Plot_Args$par.settings$superpose.symbol$pch <<- STypeIndx[grep(tclvalue(SPECTSYM), SType)]
                                      Plot_Args$par.settings$superpose.symbol$cex <<- weight
                                  }
                                  CtrlPlot()
                     })

       Lframe5 <- ttklabelframe(T6group1, text = "Distance from Margin", borderwidth=3)
       tkgrid(Lframe5, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
       MargDist <- tclVar("0.08")
       DistCK <- ttkcombobox(Lframe5, width = 15, textvariable = MargDist, values = Dist)
       tkgrid(DistCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(DistCK, "<<ComboboxSelected>>", function(){
                                  LegDist <- as.numeric(tclvalue(MargDist))
			                               switch(tclvalue(LEGENDPOS),
                                     "OutsideTop"         = { Plot_Args$auto.key$space <<-"top"
                                                              Plot_Args$auto.key$y <<-1+LegDist },
				                                 "OutsideTopRight"    = { Plot_Args$auto.key$space <<-NULL
                                                              Plot_Args$auto.key$corner <<- c(1,1)
                                                              Plot_Args$auto.key$x <<-0.95
                                                              Plot_Args$auto.key$y <<-1+LegDist },
				                                 "OutsideTopLeft"     = { Plot_Args$auto.key$space <<-NULL
                                                              Plot_Args$auto.key$corner <<- c(0,1)
                                                              Plot_Args$auto.key$x <<-0.05
                                                              Plot_Args$auto.key$y <<-1+LegDist },
				                                 "OutsideCenterRight" = { Plot_Args$auto.key$space <<-"right"
                                                              Plot_Args$par.settings$layout.widths$right.padding <<- 8-LegDist*40
                                                              Plot_Args$par.settings$layout.widths$key.right <<- LegDist*10 },
				                                 "OutsideCenterLeft"  = { Plot_Args$auto.key$space <<-"left"
                                                              Plot_Args$par.settings$layout.widths$left.padding <<- 8-LegDist*40
                                                              Plot_Args$par.settings$layout.widths$key.left <<- LegDist*10 },
			                                  "OutsideBottom"      = { Plot_Args$auto.key$space <<-"bottom"
                                                              Plot_Args$auto.key$y <<-1-LegDist },
				                                 "InsideTopRight"     = { Plot_Args$auto.key$space <<-NULL
                                                              Plot_Args$auto.key$corner <<- c(1,1)
                                                              Plot_Args$auto.key$x <<-1-LegDist
                                                              Plot_Args$auto.key$y <<-1-LegDist },
				                                 "InsideTopLeft"      = { Plot_Args$auto.key$space <<-NULL
                                                              Plot_Args$auto.key$corner <<- c(0,1)
                                                              Plot_Args$auto.key$x <<-LegDist
                                                              Plot_Args$auto.key$y <<-1-LegDist },
                                     "InsideBottomRight"  = { Plot_Args$auto.key$space <<-NULL
                                                              Plot_Args$auto.key$corner <<- c(1,0)
                                                              Plot_Args$auto.key$x <<-1-LegDist
                                                              Plot_Args$auto.key$y <<-LegDist },
				                                 "InsideBottomLeft"   = {	Plot_Args$auto.key$space <<-NULL
                                                              Plot_Args$auto.key$corner <<- c(0,0)
                                                              Plot_Args$auto.key$x <<-LegDist
                                                              Plot_Args$auto.key$y <<-LegDist },
                                 )
                                 CtrlPlot()
                     })

       Lframe6 <- ttklabelframe(T6group1, text = "Text Size", borderwidth=3)
       tkgrid(Lframe6, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
       LEGTXTSIZE <- tclVar("0.4")
       TSizeCK <- ttkcombobox(Lframe6, width = 15, textvariable = LEGTXTSIZE, values = TxtSize)
       tkgrid(TSizeCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(TSizeCK, "<<ComboboxSelected>>", function(){
		           	                    Plot_Args$auto.key$cex <<- as.numeric(tclvalue(LEGTXTSIZE))
                                  CtrlPlot()
                     })

       Lframe7 <- ttklabelframe(T6group1, text = "Text Color", borderwidth=3)
       tkgrid(Lframe7, row = 4, column = 2, padx = 5, pady = 5, sticky="w")
       LEGTXTCOL <- tclVar("B/W")
       TxtColCK <- ttkcombobox(Lframe7, width = 15, textvariable = LEGTXTCOL, values = c("B/W", "Color"))
       tkgrid(TxtColCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkbind(TxtColCK, "<<ComboboxSelected>>", function(){
                                  if  (tclvalue(LEGTXTCOL)=="B/W"){
                                       Plot_Args$auto.key$col <<- "black"
                                  } else {
                                       Plot_Args$auto.key$col <<- tclvalue(SpectCol)
                                  }
                                  CtrlPlot()
                     })

       Lframe8 <- ttklabelframe(T6group1, text = "Change Legend", borderwidth=3)
       tkgrid(Lframe8, row = 5, column = 1, padx = 5, pady = 5, sticky="w")

       NEWLEG <- tclVar("New Label= ")
       NewLegend <- ttkentry(Lframe8, textvariable=NEWLEG, width=15, foreground="grey")
       tkbind(NewLegend, "<FocusIn>", function(K){
                                  tkconfigure(NewLegend, foreground="red")
                                  tclvalue(NEWLEG) <- ""
                     })
       tkbind(NewLegend, "<Key-Return>", function(K){
                                  tkconfigure(NewLegend, foreground="black")
                                  Plot_Args$auto.key$text <<- tclvalue(NEWLEG)
                                  CtrlPlot()
                     })
       tkgrid(NewLegend, row = 1, column=1, padx=5, pady=5, sticky="w")

       AnnotateButt <- tkbutton(T6group1, text=" Annotate ", width=15, command=function(){
                                CustomPltAnnotate()
                     })
       tkgrid(AnnotateButt, row = 5, column = 2, padx = 5, pady = 5, sticky="w")



#--- Common buttons
   ButtGroup <- ttkframe(CustMainGp, borderwidth=2, padding=c(5,5,5,5) )
   tkgrid(ButtGroup, row = 2, column = 1, padx=5, pady=5, sticky="w")

   RefreshBtn <- tkbutton(ButtGroup, text="  REFRESH  ", width=20, command=function(){
                                  SetXYplotData()
                     })
   tkgrid(RefreshBtn, row = 1, column = 1, padx=5, pady=5, sticky="w")

   ResetBtn <- tkbutton(ButtGroup, text="  RESET PLOT  ", width=20, command=function(){
                                  ResetPlot()
                                  CtrlPlot()
                     })
   tkgrid(ResetBtn, row = 1, column = 2, padx=5, pady=5, sticky="w")

   ExitBtn <- tkbutton(ButtGroup, text="  EXIT  ", width=20, command=function(){
                                  tkdestroy(CustomWindow)
                     })
   tkgrid(ExitBtn, row = 1, column = 3, padx=5, pady=5, sticky="w")


   WidgetState(T3obj2, "disabled")
   WidgetState(T3obj3, "disabled")
   WidgetState(T4obj2, "disabled")
   WidgetState(T4obj3, "disabled")
   WidgetState(T5obj2, "disabled")
   WidgetState(T5obj3, "disabled")
   WidgetState(T3obj4, "disabled")
   WidgetState(T3obj5, "disabled")
   WidgetState(T4obj4, "disabled")
   WidgetState(T4obj5, "disabled")
   WidgetState(T5obj4, "disabled")
   WidgetState(T5obj5, "disabled")




}



