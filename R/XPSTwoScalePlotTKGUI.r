#function to perform plots using two Y scales

#' @title XPSTwoScalePlot
#' @description XPSTwoScalePlot performs plots overlapping two different
#'   object with different Y scales
#'   Provides a userfriendly interface to select Objects to superpose
#'   and a selection of major plotting options for a personalized data representation
#'   Data are represented using different Left and Right Y scales
#' @examples
#' \dontrun{
#' 	XPSTwoScalePlot()
#' }
#' @export
#'


XPSTwoScalePlot <- function(){

   CtrlPlot <- function(){
#---- calls the macro which makes the plot following options
            if (tclvalue(XR) == "1") {   #reverse X scale==TRUE
               Xlim <- sort(Xlim, decreasing=TRUE)
               Plot_Args$xlim <<- Xlim
            } else {
               Xlim <- sort(Xlim, decreasing=FALSE)
               Plot_Args$xlim <<- Xlim
            }

            Plot_Args$ylim <<- sort(Ylim1, decreasing=FALSE)
            Plot_Args$col <<- tclvalue(XSC1)
            Plot_Args$lty <<- tclvalue(LT1)
            idx <- which(SType == tclvalue(SYT1))
            Plot_Args$pch <<- STypeIndx[idx]
            Plot_Args$data	<<- df1

            par(mar=c(5,5,5,5)) #set plot margins first
            with(df1, plot(x, y, xlim=Plot_Args$xlim, ylim=Plot_Args$ylim, axes=FALSE, ann=FALSE,
                           pch=Plot_Args$pch, cex=Plot_Args$cex, col=Plot_Args$col, #using dataframe df1 plot following parameters
                           lty=Plot_Args$lty, lwd=Plot_Args$lwd, type=Plot_Args$type,
#                           main=Plot_Args$main$label, cex.main=Plot_Args$main$cex,
#                           lines=Plot_Args$lines, point=Plot_Args$point, background=Plot_Args$background, col=Plot_Args$col,
#                           xlab=Plot_Args$xlab$label, cex.lab=Plot_Args$xlab$cex,
#                           ylab=Plot_Args$ylab$label, cex.lab=Plot_Args$ylab$cex,
#                           cex.axis=Plot_Args$scales$cex,
#                           xscale.components=Plot_Args$xscale.components, xscale.components=Plot_Args$xscale.components,
                           par.settings=Plot_Args$par.settings, grid=FALSE)
                )
            box() #frame around the plot
            mtext(side=3, line=1.5, text=Plot_Args$main$label, cex=Plot_Args$main$cex, col="black") # Now write the title

#--- draw X axis
            axis(side=1, labels=FALSE, col="black") #now plot X axis with ticks
            tks <- axTicks(1) # Use axTicks() to get default tick posiitons and numbers
            mtext(side=1, line=1, text=tks, at=tks, cex=Plot_Args$scales$cex, col="black") #now plot X numbers at distance line=1 (margins distance is in lines)
            mtext(side=1, line=2.5, text=Plot_Args$xlab$label, cex=Plot_Args$xlab$cex, col="black") # Now draw x label at distance = line=3

#--- draw Y1 axis
            axis(side=2, labels=FALSE, col=Plot_Args$col) #now plot left axis with color of dataset1
            tks <- axTicks(2) #Use axTicks() to get default tick posiitons and numbers
            mtext(side=2, line=1, text=tks, at=tks, cex=Plot_Args$scales$cex,     #now plot Y1 numbers at distance line=1 (margins distance is in lines)
                  col=Plot_Args$col, col.ticks=Plot_Args$col, col.axis=Plot_Args$col)
            mtext(side=2, line=2.7, text=Plot_Args$ylab$label, cex=Plot_Args$ylab$cex, col=Plot_Args$col) # Now draw Y1 label at distance = line=3

#--- plot XPSSamp2 ----
            Plot_Args$ylim <<- sort(Ylim2, decreasing=FALSE)
            Plot_Args$col <<- tclvalue(XSC2)
            Plot_Args$lty <<- tclvalue(LT2)
            idx <- which(SType == tclvalue(SYT2))
            Plot_Args$pch <<- STypeIndx[idx]
            Plot_Args$data <<- df2

            par(new=T) #superpose new plot
            with(df2, plot(x, y, xlim=Plot_Args$xlim, ylim=Plot_Args$ylim, axes=FALSE, ann=FALSE,
                           pch=Plot_Args$pch, cex=Plot_Args$cex, col=Plot_Args$col, #using dataframe df1 plot following parameters
                           lty=Plot_Args$lty, lwd=Plot_Args$lwd, type=Plot_Args$type,
#                           main=Plot_Args$main$label, cex.main=Plot_Args$main$cex,
#                           lines=Plot_Args$lines, point=Plot_Args$point, background=Plot_Args$background,
#                           ylab=Plot_Args$ylab.right$label, cex.lab=Plot_Args$ylab.right$cex,
#                           main=Plot_Args$main, xlab=Plot_Args$xlab, ylab=Plot_Args$ylab,
#                           scales=Plot_Args$scales,
#                           xscale.components=Plot_Args$xscale.components, xscale.components=Plot_Args$xscale.components,
                           par.settings=Plot_Args$par.settings, grid=FALSE)
                )
            axis(side=4, labels=FALSE, col=Plot_Args$col) #now plot right axis with color of dataset2
            tks <- axTicks(2) # Use axTicks() to get default tick posiitons and numbers
            mtext(side=4, line=1, text=tks, at=tks, cex=Plot_Args$scales$cex,  #now plot Y2 numbers at distance line=1 (margins distance is in lines)
                  col=Plot_Args$col, col.ticks=Plot_Args$col, col.axis=Plot_Args$col)
            mtext(side=4, line=2.7, text=Plot_Args$ylab.right$label, cex=Plot_Args$ylab.right$cex, col=Plot_Args$col) # Now draw Y2 label at distance = line=3

            if(Plot_Args$grid) { grid() } #add grid to plot

            LTyp <- Sym <- NULL
            LW <- 0
            LegPos <- tclvalue(PLBL)
            if(LegPos != "OFF"){ #enable legend upon selction
               if (Plot_Args$type == "l" || Plot_Args$type=="b") {
                  LTyp <- c(tclvalue(LT1), tclvalue(LT2))
                  LW <- Plot_Args$lwd
               }
               if (Plot_Args$type == "p" || Plot_Args$type=="b") {
                   idx1 <- which(SType == tclvalue(SYT1))
                   idx2 <- which(SType == tclvalue(SYT2))
                   Sym <- c(STypeIndx[idx1], STypeIndx[idx2])
               }
               legend (LegPos, ncol=1, bty="n",    #position center, no box
                      legend <- c(tclvalue(NWLEG1), tclvalue(NWLEG2)),
                      lty=LTyp, lwd=LW, pt.lwd=Plot_Args$lwd, pch=Sym, pt.cex=Plot_Args$cex,
                      col=c(tclvalue(XSC1), tclvalue(XSC2)))
            }
   }

#----- reset parametri ai valori iniziali -----

   ResetPlot <- function(){
            tclvalue(XS1) <- FALSE
            tclvalue(CL1) <- FALSE
            tclvalue(XS2) <- FALSE
            tclvalue(CL2) <- FALSE
            tclvalue(XR) <- FALSE
            tclvalue(XSC1) <- Colors[1]
            tclvalue(XSC2) <- Colors[2]
            tclvalue(GRD) <- "OFF"
            tclvalue(SETL) <- "ON"
            tclvalue(SETS) <- "OFF"
            tclvalue(LT1) <- LType[1]
            tclvalue(LT2) <- LType[2]
            tclvalue(LWD) <- LWidth[1]
            tclvalue(SYT1) <- SType[1]
            tclvalue(SYT2) <- SType[2]
            tclvalue(SYSZ) <- SymSize[4]
            tclvalue(TZ) <- FontSize[5]
            tclvalue(TZ) <- FontSize[5]
            tclvalue(NWT) <- ""
            tclvalue(AXSZ) <- FontSize[3]
            tclvalue(LBLS ) <- FontSize[4]
            tclvalue(NWXLBL) <- "New X LBL"
            tclvalue(NWYLBL) <- "New Left LBL"
            tclvalue(NWYLBL) <- "New Right LBL"
            tclvalue(PLBL ) <- LegPos[1]
            tclvalue(NWLEG1) <- "New Legend"
            tclvalue(NWLEG2) <- "New Legend"
            tclvalue(XMIN) <- "Xmin"
            tclvalue(XMAX) <- "Xmax"
            tclvalue(YMIN) <- "Ymin Lft"
            tclvalue(YMAX) <- "Ymax Lft"
            tclvalue(YMIN) <- "Ymin Rgt"
            tclvalue(YMAX) <- "Ymax Rgt"
            Xlim <- NULL
            Ylim1 <- NULL
            Ylim2 <- NULL

            Plot_Args<<-list( x=formula("y ~ x"), data=NULL, groups=NULL,layout=NULL,
                    xlim=NULL, ylim=NULL,
                    pch=STypeIndx,cex=1,lty=LType,lwd=1,type="l",
                    lines=TRUE, point=FALSE,
                    background="transparent", col="black",
                    main=list(label=NULL,cex=1.4),
                    xlab=list(label="X", rot=0, cex=1),
                    ylab=list(label="Y1", rot=90, cex=1),
                    ylab.right=list(label="Y2", rot=90, cex=1),
                    par.settings = list(superpose.symbol=list(pch=STypeIndx,fill="black"), #setta colore riempimento simboli
                                        superpose.line=list(lty=LType, col="black")), #necessario per settare colori legende
                    grid = FALSE
                  )
            LegTxt<<-NULL
            CtrlPlot()
   }

#----- Variabili -----
   plot.new()                               #reset graphic window
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FNameListTot <- as.array(XPSFNameList())     #XPS Sample List in GlobalEnv
   FName1 <- get(activeFName, envir=.GlobalEnv)
   SpectList1 <- XPSSpectList(activeFName)      #CorfeLines of active XPSSample
   df1 <- data.frame(x=NULL, y=NULL)
   df2 <- data.frame(x=NULL, y=NULL)
   NCorelines <- NULL
   Xlim <- NULL
   Ylim1 <- NULL
   Ylim2 <- NULL

   SelectedNames <- list(XPSSample=NULL, CoreLines=NULL)
   NamesList=list(XPSSample=NULL, CoreLines=NULL)
   FontSize <- c(0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)
   LWidth <- c(1,1.25,1.5,1.75,2,2.25,2.5,3, 3.5,4)
   SymSize <- c(0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2)
   LegTxt <- NULL
   LegPos <- c("OFF", "topleft", "top", "topright", "left",  "center", "right", "bottomleft", "bottom", "bottomright")
   OnOff <- c("ON", "OFF")

   XPSSettings <- get("XPSSettings", envir=.GlobalEnv)
   Colors <- XPSSettings$Colors
   LType <- XPSSettings$LType
   SType <- XPSSettings$Symbols
   STypeIndx <- XPSSettings$SymIndx

   Plot_Args<-list( x=formula("y ~ x"), data=NULL, groups=NULL,layout=NULL,
                    xlim=NULL, ylim=NULL,
                    pch=STypeIndx,cex=1,lty=LType,lwd=1,type="l",
                    lines=TRUE, point=FALSE,
                    background="transparent", col="black",
                    main=list(label=NULL,cex=1.2),
                    xlab=list(label="X", rot=0, cex=1),
                    ylab=list(label="Y1", rot=90, cex=1),
                    ylab.right=list(label="Y2", rot=90, cex=1),
                    par.settings = list(superpose.symbol=list(pch=STypeIndx,fill="black"), #set symbol fill color
                                        superpose.line=list(lty=LType, col="black")), #needed to set colors and legends
                    grid = FALSE
                  )

#----- GUI -----
   TwoSWindow <- tktoplevel()
   tkwm.title(TwoSWindow," TWO Y-SCALE PLOT ")
   tkwm.geometry(TwoSWindow, "+100+50")   #position respect topleft screen corner
   MainTwoSGroup <- ttkframe(TwoSWindow, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(MainTwoSGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   TwoSNB <- ttknotebook(MainTwoSGroup)
   tkgrid(TwoSNB, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

# --- TAB1 ---
#XPS Sample selecion & representation options
     T1group1 <- ttkframe(TwoSNB,  borderwidth=2, padding=c(5,5,5,5) )
     tkadd(TwoSNB, T1group1, text="XPS SAMPLE SELECTION")

     T1F_FName1 <- ttklabelframe(T1group1, text = "SELECT XPS-SAMPLE 1", borderwidth=2)
     tkgrid(T1F_FName1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
     XS1 <- tclVar()
     T1FNameListCK1 <- ttkcombobox(T1F_FName1, width = 15, textvariable = XS1, values = FNameListTot)
     tkbind(T1FNameListCK1, "<<ComboboxSelected>>", function(){
                       SelectedNames$XPSSample[1] <<- tclvalue(XS1)
                       SpectList1 <- XPSSpectList(SelectedNames$XPSSample[1])
                       tkconfigure(T1_CoreLineCK1, values=SpectList1)
                       WidgetState(T1_CoreLineCK1, "normal")
                 })
     tkgrid(T1FNameListCK1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T1F_CoreLines1 <- ttklabelframe(T1group1, text = "SELECT CORE_LINE 1", borderwidth=2)
     tkgrid(T1F_CoreLines1, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
     CL1 <- tclVar()
     T1_CoreLineCK1 <- ttkcombobox(T1F_CoreLines1, width = 15, textvariable = CL1, values = "     ")
     tkbind(T1_CoreLineCK1, "<<ComboboxSelected>>", function(){
                       SelectedNames$CoreLines[1] <<- tclvalue(CL1)
                       FName1 <- get(SelectedNames$XPSSample[1], envir=.GlobalEnv)
                       SpectList1 <- XPSSpectList(SelectedNames$XPSSample[1])
                       idx <- grep(SelectedNames$CoreLines[1], SpectList1)
                       Xlim <<- range(FName1[[idx]]@.Data[[1]])
                       Ylim1 <<- range(FName1[[idx]]@.Data[[2]])
                       Plot_Args$xlab$label <<- FName1[[idx]]@units[1]
                       Plot_Args$ylab$label <<- FName1[[idx]]@units[2]
                       df1 <<- data.frame(x=unlist(FName1[[idx]]@.Data[1]), y=unlist(FName1[[idx]]@.Data[2]))
                 })
     tkgrid(T1_CoreLineCK1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T1F_FName2 <- ttklabelframe(T1group1, text = "SELECT XPS-SAMPLE 2", borderwidth=2)
     tkgrid(T1F_FName2, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
     XS2 <- tclVar()
     T1FNameListCK2 <- ttkcombobox(T1F_FName2, width = 15, textvariable = XS2, values = FNameListTot)
     tkbind(T1FNameListCK2, "<<ComboboxSelected>>", function(){
                       SelectedNames$XPSSample[2] <<- tclvalue(XS2)
                       SpectList2 <- XPSSpectList(SelectedNames$XPSSample[2])
                       tkconfigure(T1_CoreLineCK2, values=SpectList2)
                       WidgetState(T1_CoreLineCK2, "normal")
                 })
     tkgrid(T1FNameListCK2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T1F_CoreLines2 <- ttklabelframe(T1group1, text = "SELECT CORE_LINE 2", borderwidth=2)
     tkgrid(T1F_CoreLines2, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
     CL2 <- tclVar()
     T1_CoreLineCK2 <- ttkcombobox(T1F_CoreLines2, width = 15, textvariable = CL2, values = "     ")
     tkbind(T1_CoreLineCK2, "<<ComboboxSelected>>", function(){
                       SelectedNames$CoreLines[2] <<- tclvalue(CL2)
                       FName2 <- get(SelectedNames$XPSSample[2], envir=.GlobalEnv)
                       SpectList2 <- XPSSpectList(SelectedNames$XPSSample[2])
                       idx <- grep(SelectedNames$CoreLines[2], SpectList2)
                       Xlim2 <- range(FName2[[idx]]@.Data[[1]])
                       Xlim <<- range(Xlim, Xlim2)
                       Ylim2 <<- range(FName2[[idx]]@.Data[[2]])
#                       Plot_Args$xlab$label <<- FName2[[idx]]@units[1]
                       Plot_Args$ylab.right$label <<- FName2[[idx]]@units[2]
                       df2 <<- data.frame(x=unlist(FName1[[idx]]@.Data[1]), y=unlist(FName1[[idx]]@.Data[2]))
                 })
     tkgrid(T1_CoreLineCK2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T1F_ReverseX <- ttklabelframe(T1group1, text = "REVERSE X-axis", borderwidth=2)
     tkgrid(T1F_ReverseX, row = 3, column = 1, padx = 5, pady = 5, sticky="we")
     XR <- tclVar("0")
     T1_ReverseX <- tkcheckbutton(T1F_ReverseX, text="Reverse X axis", variable=XR, onvalue=1, offvalue=0,
                      command=function(){
                           CtrlPlot()
                 })
     tkgrid(T1_ReverseX, row = 1, column = 1, padx=10, pady = 5, sticky="w")


# --- TAB2 ---
# Rendering options
     T2group1 <- ttkframe(TwoSNB,  borderwidth=2, padding=c(5,5,5,5) )
     tkadd(TwoSNB, T2group1, text="RENDERING")

     T2F_Col1 <- ttklabelframe(T2group1, text = " COLOR XPSSample1 ", borderwidth=2)
     tkgrid(T2F_Col1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
     XSC1 <- tclVar(Colors[1])
     T2_Col1 <- ttkcombobox(T2F_Col1, width = 15, textvariable = XSC1, values = Colors)
     tkbind(T2_Col1, "<<ComboboxSelected>>", function(){
                      Plot_Args$col <<- c(tclvalue(XSC1),tclvalue(XSC2))
                      Plot_Args$par.settings$superpose.line$col <<- c(tclvalue(XSC1),tclvalue(XSC2))
                      Plot_Args$par.settings$superpose.symbol$col <<- c(tclvalue(XSC1),tclvalue(XSC2))
                      CtrlPlot()
                 })
     tkgrid(T2_Col1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T2F_Col2 <- ttklabelframe(T2group1, text = " COLOR XPSSample2 ", borderwidth=2)
     tkgrid(T2F_Col2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
     XSC2 <- tclVar(Colors[2])
     T2_Col2 <- ttkcombobox(T2F_Col2, width = 15, textvariable = XSC2, values = Colors)
     tkbind(T2_Col2, "<<ComboboxSelected>>", function(){
                      Plot_Args$col <<- c(tclvalue(XSC1),tclvalue(XSC2))
                      Plot_Args$par.settings$superpose.line$col <<- c(tclvalue(XSC1),tclvalue(XSC2))
                      Plot_Args$par.settings$superpose.symbol$col <<- c(tclvalue(XSC1),tclvalue(XSC2))
                      CtrlPlot()
                 })
     tkgrid(T2_Col2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T2F_Grid <- ttklabelframe(T2group1, text = " GRID ", borderwidth=2)
     tkgrid(T2F_Grid, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

     GRD <- tclVar("OFF")   #starts with cleared buttons
     for(ii in 1:2){
        T2_Grid <- ttkradiobutton(T2F_Grid, text=OnOff[ii], variable=GRD, value=OnOff[ii], command=function(){
                      if(tclvalue(GRD) == "ON") {
                         Plot_Args$grid <<- TRUE
                      } else {
                         Plot_Args$grid <<- FALSE
                      }
                      CtrlPlot()
                 })
        tkgrid(T2_Grid, row = 1, column = ii, padx = 15, pady = 5, sticky="we")
     }

     T2F_SetLines <- ttklabelframe(T2group1, text = " SET LINES ", borderwidth=2)
     tkgrid(T2F_SetLines, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
     SETL <- tclVar("ON")   #starts with cleared buttons
     for(ii in 1:2){
        T2_SetLines <- ttkradiobutton(T2F_SetLines, text=OnOff[ii], variable=SETL, value = OnOff[ii], command=function(){
                      if (tclvalue(SETL)=="ON" && tclvalue(SETS)=="OFF"){
                          Plot_Args$type <<- "l"
                      } else if (tclvalue(SETL)=="OFF" && tclvalue(SETS)=="OFF"){
                          Plot_Args$type <<- NA
                      } else if (tclvalue(SETL)=="OFF" && tclvalue(SETS)=="ON"){
                          Plot_Args$type <<- "p"
                      } else if (tclvalue(SETL)=="ON" && tclvalue(SETS)=="ON"){
                          Plot_Args$type <<- "b"
                      }
                      CtrlPlot()
                 })
        tkgrid(T2_SetLines, row = 1, column = ii, padx = 15, pady = 5, sticky="we")
     }

     T2F_SetSymbols <- ttklabelframe(T2group1, text = " SET SYMBOLS ", borderwidth=2)
     tkgrid(T2F_SetSymbols, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
     SETS <- tclVar("OFF")   #starts with cleared buttons
     for(ii in 1:2){
        T2_SetSymbols <- ttkradiobutton(T2F_SetSymbols, text=OnOff[ii], variable=SETS, value = OnOff[ii], command=function(){
                      if (tclvalue(SETS)=="ON" && tclvalue(SETL)=="OFF"){
                          Plot_Args$type <<- "p"
                      } else if (tclvalue(SETS)=="OFF" && tclvalue(SETL)=="OFF"){
                          Plot_Args$type <<- NA
                      } else if (tclvalue(SETS)=="OFF" && tclvalue(SETL)=="ON"){
                          Plot_Args$type <<- "l"
                      } else if (tclvalue(SETS)=="ON" && tclvalue(SETL)=="ON"){
                          Plot_Args$type <<- "b"
                      }
                      CtrlPlot()
                 })
        tkgrid(T2_SetSymbols, row = 1, column = ii, padx = 15, pady = 5, sticky="we")
     }

     T2F_SetLines1 <- ttklabelframe(T2group1, text = " LINE TYPE XPSSamp1 ", borderwidth=2)
     tkgrid(T2F_SetLines1, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
     LT1 <- tclVar(LType[1])
     T2_LineType1 <- ttkcombobox(T2F_SetLines1, width = 15, textvariable = LT1, values = LType)
     tkbind(T2_LineType1, "<<ComboboxSelected>>", function(){
                      Plot_Args$par.settings$superpose.line$lty <<- c(tclvalue(LT1),tclvalue(LT2))
                      CtrlPlot()
                 })
     tkgrid(T2_LineType1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T2F_SetLines2 <- ttklabelframe(T2group1, text = " LINE TYPE XPSSamp2 ", borderwidth=2)
     tkgrid(T2F_SetLines2, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
     LT2 <- tclVar(LType[2])
     T2_LineType2 <- ttkcombobox(T2F_SetLines2, width = 15, textvariable = LT2, values = LType)
     tkbind(T2_LineType2, "<<ComboboxSelected>>", function(){
                      Plot_Args$par.settings$superpose.line$lty <<- c(tclvalue(LT1),tclvalue(LT2))
                      CtrlPlot()
                 })
     tkgrid(T2_LineType2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T2F_LinWidth <- ttklabelframe(T2group1, text = " LINE WIDTH ", borderwidth=2)
     tkgrid(T2F_LinWidth, row = 3, column = 3, padx = 5, pady = 5, sticky="w")
     LWD <- tclVar(LWidth[1])
     T2_LinWidth <- ttkcombobox(T2F_LinWidth, width = 15, textvariable = LWD, values = LWidth)
     tkbind(T2_LinWidth, "<<ComboboxSelected>>", function(){
                      Plot_Args$lwd <<- as.numeric(tclvalue(LWD))
                      CtrlPlot()
                 })
     tkgrid(T2_LinWidth, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T2F_SymType1 <- ttklabelframe(T2group1, text = " SYMBOLS XPSSamp1 ", borderwidth=2)
     tkgrid(T2F_SymType1, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
     SYT1 <- tclVar(SType[1])
     T2_SymType1 <- ttkcombobox(T2F_SymType1, width = 15, textvariable = SYT1, values = SType)
     tkbind(T2_SymType1, "<<ComboboxSelected>>", function(){
                      Plot_Args$par.settings$superpose.symbol$pch <<- c(tclvalue(SYT1),tclvalue(SYT2))
                      CtrlPlot()
#                      CtrlPlot()
                 })
     tkgrid(T2_SymType1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T2F_SymType2 <- ttklabelframe(T2group1, text = " SYMBOLS XPSSamp2 ", borderwidth=2)
     tkgrid(T2F_SymType2, row = 4, column = 2, padx = 5, pady = 5, sticky="w")
     SYT2 <- tclVar(SType[2])
     T2_SymType2 <- ttkcombobox(T2F_SymType2, width = 15, textvariable = SYT2, values = SType)
     tkbind(T2_SymType2, "<<ComboboxSelected>>", function(){
print(c(tclvalue(SYT1),tclvalue(SYT2)))
                      Plot_Args$par.settings$superpose.symbol$pch <<- c(tclvalue(SYT1),tclvalue(SYT2))
                      CtrlPlot()
#                      CtrlPlot()
                 })
     tkgrid(T2_SymType2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T2F_SymSize <- ttklabelframe(T2group1, text = " SYMSIZE ", borderwidth=2)
     tkgrid(T2F_SymSize, row = 4, column = 3, padx = 5, pady = 5, sticky="w")
     SYSZ <- tclVar(SymSize[4])
     T2_SymSize <- ttkcombobox(T2F_SymSize, width = 15, textvariable = SYSZ, values = SymSize)
     tkbind(T2_SymSize, "<<ComboboxSelected>>", function(){
                      Plot_Args$cex <<- as.numeric(tclvalue(SYSZ))
                      CtrlPlot()
                 })
     tkgrid(T2_SymSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     ResetBtn <- tkbutton(T2group1, text=" RESET ", width=20, command=function(){
                      ResetPlot()
                      CtrlPlot()
                 })
     tkgrid(ResetBtn, row = 5, column = 1, padx = 5, pady = 5, sticky="w")


# --- TAB3 ---
# Axis Rendering options
     T3group1 <- ttkframe(TwoSNB,  borderwidth=2, padding=c(5,5,5,5) )
     tkadd(TwoSNB, T3group1, text="AXES")

     T3F_TitSize <- ttklabelframe(T3group1, text = " TITLE SIZE ", borderwidth=2)
     tkgrid(T3F_TitSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
     TSZ <- tclVar(FontSize[5])
     T3_TitSize <- ttkcombobox(T3F_TitSize, width = 15, textvariable = TSZ, values = FontSize)
     tkbind(T3_TitSize, "<<ComboboxSelected>>", function(){
                      Plot_Args$main$cex <<- as.numeric(tclvalue(TSZ))
                      CtrlPlot()
                 })
     tkgrid(T3_TitSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_MainTitChange <- ttklabelframe(T3group1, text = " CHANGE MAIN TITLE ", borderwidth=2)
     tkgrid(T3F_MainTitChange, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
     NWT <- tclVar("New Title")  #sets the initial msg
     T3F_MainTitChange <- ttkentry(T3F_MainTitChange, textvariable=NWT, width=20, foreground="grey")
     tkbind(T3F_MainTitChange, "<FocusIn>", function(K){
                            tclvalue(NWT) <- ""
                            tkconfigure(T3F_MainTitChange, foreground="red")
                        })
     tkbind(T3F_MainTitChange, "<Key-Return>", function(K){
                            tkconfigure(T3F_MainTitChange, foreground="black")
                            Plot_Args$main$label <<- tclvalue(NWT)
                            CtrlPlot()
                        })
     tkgrid(T3F_MainTitChange, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_AxNumSize <- ttklabelframe(T3group1, text = " AXIS NUMBER SIZE ", borderwidth=2)
     tkgrid(T3F_AxNumSize, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
     AXSZ <- tclVar(FontSize[3])
     T3_AxNumSize <- ttkcombobox(T3F_AxNumSize, width = 15, textvariable = AXSZ, values = FontSize)
     tkbind(T3_AxNumSize, "<<ComboboxSelected>>", function(){
                      Plot_Args$scales$cex <<- as.numeric(tclvalue(AXSZ))  #controls the size X axis only
                      CtrlPlot()
                 })
     tkgrid(T3_AxNumSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_AxLabSize <- ttklabelframe(T3group1, text = " AXIS LABEL SIZE ", borderwidth=2)
     tkgrid(T3F_AxLabSize, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
     LBLS <- tclVar(FontSize[4])
     T3_AxLabSize <- ttkcombobox(T3F_AxLabSize, width = 15, textvariable = LBLS, values = FontSize)
     tkbind(T3_AxLabSize, "<<ComboboxSelected>>", function(){
                      Plot_Args$xlab$cex <<- as.numeric(tclvalue(LBLS))
                      Plot_Args$ylab$cex <<- as.numeric(tclvalue(LBLS))
                      Plot_Args$ylab.right$cex <<- as.numeric(tclvalue(LBLS))
                      CtrlPlot()
                 })
     tkgrid(T3_AxLabSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_XAxNameChange <- ttklabelframe(T3group1, text = " CHANGE X-LABEL ", borderwidth=2)
     tkgrid(T3F_XAxNameChange, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
     NWXLBL <- tclVar("New X LBL")  #sets the initial msg
     T3_XAxNameChange <- ttkentry(T3F_XAxNameChange, textvariable=NWXLBL, width=20, foreground="grey")
     tkbind(T3_XAxNameChange, "<FocusIn>", function(K){
                      tclvalue(NWXLBL) <- ""
                      tkconfigure(T3_XAxNameChange, foreground="red")
                 })
     tkbind(T3_XAxNameChange, "<Key-Return>", function(K){
                      tkconfigure(T3_XAxNameChange, foreground="black")
                      Plot_Args$xlab$label <<- tclvalue(NWXLBL)
                      CtrlPlot()
                 })
     tkgrid(T3_XAxNameChange, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_LYAxNameChange <- ttklabelframe(T3group1, text = " CHANGE LEFT Y-LABEL ", borderwidth=2)
     tkgrid(T3F_LYAxNameChange, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
     NWLYLBL <- tclVar("New Left LBL")  #sets the initial msg
     T3_LYAxNameChange <- ttkentry(T3F_LYAxNameChange, textvariable=NWLYLBL, width=20, foreground="grey")
     tkbind(T3_LYAxNameChange, "<FocusIn>", function(K){
                      tclvalue(NWLYLBL) <- ""
                      tkconfigure(T3_LYAxNameChange, foreground="red")
                 })
     tkbind(T3_LYAxNameChange, "<Key-Return>", function(K){
                      tkconfigure(T3_LYAxNameChange, foreground="black")
                      Plot_Args$ylab$label <<- tclvalue(NWLYLBL)
                      CtrlPlot()
                 })
     tkgrid(T3_LYAxNameChange, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_RYAxNameChange <- ttklabelframe(T3group1, text = " CHANGE RIGHT Y-LABEL ", borderwidth=2)
     tkgrid(T3F_RYAxNameChange, row = 3, column = 3, padx = 5, pady = 5, sticky="w")
     NWRYLBL <- tclVar("New Right LBL")  #sets the initial msg
     T3_RYAxNameChange <- ttkentry(T3F_RYAxNameChange, textvariable=NWRYLBL, width=20, foreground="grey")
     tkbind(T3_RYAxNameChange, "<FocusIn>", function(K){
                      tclvalue(NWRYLBL) <- ""
                      tkconfigure(T3_RYAxNameChange, foreground="red")
                 })
     tkbind(T3_RYAxNameChange, "<Key-Return>", function(K){
                      tkconfigure(T3_RYAxNameChange, foreground="black")
                      Plot_Args$ylab.right$label <<- tclvalue(NWRYLBL)
                      CtrlPlot()
                 })
     tkgrid(T3_RYAxNameChange, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_Legend1 <- ttklabelframe(T3group1, text = " LEGEND POSITION ", borderwidth=2)
     tkgrid(T3F_Legend1, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
     PLBL <- tclVar(LegPos[1])
     T3_LegPos1 <- ttkcombobox(T3F_Legend1, width = 15, textvariable = PLBL, values = LegPos)
     tkbind(T3_LegPos1, "<<ComboboxSelected>>", function(){
                      CtrlPlot()
                 })
     tkgrid(T3_LegPos1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_Legend2 <- ttklabelframe(T3group1, text = " XPSSample1 LEGEND ", borderwidth=2)
     tkgrid(T3F_Legend2, row = 4, column = 2, padx = 5, pady = 5, sticky="w")
     NWLEG1 <- tclVar("New Legend")  #sets the initial msg
     T3_Legend2 <- ttkentry(T3F_Legend2, textvariable=NWLEG1, width=20, foreground="grey")
     tkbind(T3_Legend2, "<FocusIn>", function(K){
                      tclvalue(NWLEG1) <- ""
                      tkconfigure(T3_Legend2, foreground="red")
                 })
     tkbind(T3_Legend2, "<Key-Return>", function(K){
                      tkconfigure(T3_Legend2, foreground="black")
                      LegTxt <<- c(tclvalue(NWLEG1), tclvalue(NWLEG2))
                      CtrlPlot()
                 })
     tkgrid(T3_Legend2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_Legend3 <- ttklabelframe(T3group1, text = " XPSSample2 LEGEND ", borderwidth=2)
     tkgrid(T3F_Legend3, row = 4, column = 3, padx = 5, pady = 5, sticky="w")
     NWLEG2 <- tclVar("New Legend")  #sets the initial msg
     T3_Legend3 <- ttkentry(T3F_Legend3, textvariable=NWLEG2, width=20, foreground="grey")
     tkbind(T3_Legend3, "<FocusIn>", function(K){
                      tclvalue(NWLEG2) <- ""
                      tkconfigure(T3_Legend3, foreground="red")
                 })
     tkbind(T3_Legend3, "<Key-Return>", function(K){
                      tkconfigure(T3_Legend3, foreground="black")
                      LegTxt <<- c(tclvalue(NWLEG1), tclvalue(NWLEG2))
                      CtrlPlot()
                 })
     tkgrid(T3_Legend3, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

#--- to modify the default X, Y1, Y2 scales

     T3F_X <- ttklabelframe(T3group1, text = " X range ", borderwidth=2)
     tkgrid(T3F_X, row = 5, column = 1, padx = 5, pady = 5, sticky="w")
     XMIN <- tclVar("Xmin")  #sets the initial msg
     T3_Xmin <- ttkentry(T3F_X, textvariable=XMIN, width=20, foreground="grey")
     tkbind(T3_Xmin, "<FocusIn>", function(K){
                      tclvalue(XMIN) <- ""
                      tkconfigure(T3_Xmin, foreground="red")
                 })
     tkbind(T3_Xmin, "<Key-Return>", function(K){
                      tkconfigure(T3_Xmin, foreground="black")
                      Xmin <- as.numeric(tclvalue(XMIN))
                      Xlim <<- c(Xmin, max(Xlim))
                      CtrlPlot()
                 })
     tkgrid(T3_Xmin, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     XMAX <- tclVar("Xmax")  #sets the initial msg
     T3_Xmax <- ttkentry(T3F_X, textvariable=XMAX, width=20, foreground="grey")
     tkbind(T3_Xmax, "<FocusIn>", function(K){
                      tclvalue(XMAX) <- ""
                      tkconfigure(T3_Xmax, foreground="red")
                 })
     tkbind(T3_Xmax, "<Key-Return>", function(K){
                      tkconfigure(T3_Xmax, foreground="black")
                      Xmax <- as.numeric(tclvalue(XMAX))
                      Xlim <<- c(min(Xlim), Xmax)
                      CtrlPlot()
                 })
     tkgrid(T3_Xmax, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_Y1 <- ttklabelframe(T3group1, text = " Y1 range ", borderwidth=2)
     tkgrid(T3F_Y1, row = 5, column = 2, padx = 5, pady = 5, sticky="w")
     YMIN1 <- tclVar("Ymin Left")  #sets the initial msg
     T3_Ymin1 <- ttkentry(T3F_Y1, textvariable=YMIN1, width=20, foreground="grey")
     tkbind(T3_Ymin1, "<FocusIn>", function(K){
                      tclvalue(YMIN1) <- ""
                      tkconfigure(T3_Ymin1, foreground="red")
                 })
     tkbind(T3_Ymin1, "<Key-Return>", function(K){
                      tkconfigure(T3_Ymin1, foreground="black")
                      Ymin1 <- as.numeric(tclvalue(YMIN1))
                      Ylim1 <<- c(Ymin1, max(Ylim1))
                      CtrlPlot()
                 })
     tkgrid(T3_Ymin1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     YMAX1 <- tclVar("Ymax Left")  #sets the initial msg
     T3_Ymax1 <- ttkentry(T3F_Y1, textvariable=YMAX1, width=20, foreground="grey")
     tkbind(T3_Ymax1, "<FocusIn>", function(K){
                      tclvalue(YMAX1) <- ""
                      tkconfigure(T3_Ymax1, foreground="red")
                 })
     tkbind(T3_Ymax1, "<Key-Return>", function(K){
                      tkconfigure(T3_Ymax1, foreground="black")
                      Ymax1 <- as.numeric(tclvalue(YMAX1))
                      Ylim1 <- c(min(Ylim1), Ymax1)
                      CtrlPlot()
                 })
     tkgrid(T3_Ymax1, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

     T3F_Y2 <- ttklabelframe(T3group1, text = " Y1 range ", borderwidth=2)
     tkgrid(T3F_Y2, row = 5, column = 3, padx = 5, pady = 5, sticky="w")
     YMIN2 <- tclVar("Ymin Right")  #sets the initial msg
     T3_Ymin2 <- ttkentry(T3F_Y2, textvariable=YMIN2, width=20, foreground="grey")
     tkbind(T3_Ymin2, "<FocusIn>", function(K){
                      tclvalue(YMIN2) <- ""
                      tkconfigure(T3_Ymin2, foreground="red")
                 })
     tkbind(T3_Ymin2, "<Key-Return>", function(K){
                      tkconfigure(T3_Ymin2, foreground="black")
                      Ymin2 <- as.numeric(tclvalue(YMIN2))
                      Ylim2 <<- c(Ymin2, max(Ylim2))
                      CtrlPlot()
                 })
     tkgrid(T3_Ymin2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     YMAX2 <- tclVar("Ymax Right")  #sets the initial msg
     T3_Ymax2 <- ttkentry(T3F_Y2, textvariable=YMAX2, width=20, foreground="grey")
     tkbind(T3_Ymax2, "<FocusIn>", function(K){
                      tclvalue(YMAX2) <- ""
                      tkconfigure(T3_Ymax2, foreground="red")
                 })
     tkbind(T3_Ymax2, "<Key-Return>", function(K){
                      tkconfigure(T3_Ymax2, foreground="black")
                      Ymin2 <- as.numeric(tclvalue(YMAX2))
                      Ylim2 <<- c(Ymin2, max(Ylim2))
                      CtrlPlot()
                 })
     tkgrid(T3_Ymax2, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

     ResetBtn <- tkbutton(T3group1, text="  RESET  ", width=20, command=function(){
                      ResetPlot()
                      CtrlPlot()
                 })
     tkgrid(ResetBtn, row = 12, column = 1, padx = 5, pady = 5, sticky="w")


#--- COMMON
     PlotBtn <- tkbutton(MainTwoSGroup, text="  PLOT  ", width=30, command=function(){
                      if (SelectedNames$XPSSample[1]==SelectedNames$XPSSample[2]){
                          answ <- tkmessageBox(message="XPSSample1 = XPSSample2! Proceed anyway?", type="yesno",
                                               title = "WARNING: SAME XPSSAMPLE DATA SELECTED",  icon = "warning")
                          if (tclvalue(answ) == "1") {
                              return()
                          } else {
                              CtrlPlot()
                          }
                      } else {
                          CtrlPlot()
                      }
                 })
     tkgrid(PlotBtn, row = 2, column = 1, padx = 5, pady = c(5,10), sticky="w")
     WW <- as.numeric(tkwinfo("reqwidth", PlotBtn))+20


     exitBtn <- tkbutton(MainTwoSGroup, text="  EXIT  ", width=30, command=function(){
                      tkdestroy(TwoSWindow)
                 })
     tkgrid(exitBtn, row = 2, column = 1, padx = c(WW,5), pady = c(5,10), sticky="w")



   WidgetState(T1_CoreLineCK1, "disabled")
   WidgetState(T1_CoreLineCK2, "disabled")
}
