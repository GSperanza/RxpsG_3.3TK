#XPSCustomPlot function to produce customized plots
#
#' @title Function to generate BAR plots
#' @description XPSBarPlot allows a full control of the various
#'   parameters forplotting data. Through a user friendly interface it
#'   is possible to set colors, lines or symbols, their weight,  modify
#'   title, X,Y labels and their dimensions, add/modify legend
#'   annotate the plot.This GUI is based on the Lattice package.
#' @examples
#' \dontrun{
#' 	XPSBarPlot()
#' }
#' @export
#'

XPSBarPlot <- function(){


   CtrlPlot <- function(){
        setRange()
        barplot(height=BarData$y,
            xlim=Plot_Args$xlim, ylim=Plot_Args$ylim,
            xlab=Plot_Args$xlab, ylab=Plot_Args$ylab, main=Plot_Args$main,
            axisnames=TRUE, names.arg=Plot_Args$BarNames,
            cex.lab=Plot_Args$cex.lab, cex.main=Plot_Args$cex.main,
            cex.axis = Plot_Args$cex.axis, cex.names=Plot_Args$cex.names,
            beside=Plot_Args$beside,
            axes=TRUE, axis.lty = 1,
            col=Plot_Args$col, legend.text=Plot_Args$legend.text
        )
        box()
   }

   setRange <- function(){
        x1 <- as.numeric(tclvalue(XMIN))
        y1 <- as.numeric(tclvalue(YMIN))
        x2 <- as.numeric(tclvalue(XMAX))
        y2 <- as.numeric(tclvalue(YMAX))
        LL <- length(FName[[SpectIndx]]@.Data[[2]])
        Xlim <<- Ylim <<- NULL

        if (is.na(x1) == FALSE && is.na(x2) == FALSE &&
            is.na(y1) == FALSE && is.na(y2) == FALSE) {
                Xlim <<- sort(c(x1, x2))
                Ylim <<- sort(c(y1, y2))
        } else {
            NRow <- length(SelectedCL)
            LL <- length(FName[[SelectedCL[1]]]@.Data[[2]])
            Xlim[1] <<- 0
            Xlim[2] <<- (LL + LL*Plot_Args$space)*NRow  #*Plot_Args$width + LL*Plot_Args$space

            wdth <- Xlim[2]-Xlim[1]
            Xlim[1] <<- Xlim[1]-wdth/15
            Xlim[2] <<- Xlim[2]+wdth/15
            if (NRow == 1){
                Ylim <<- range(FName[[SelectedCL[1]]]@.Data[[2]])
            } else {
                Ylim <<- range(sapply(SelectedCL, function(x) FName[[x]]@.Data[[2]]))
            }
            wdth <- Ylim[2]-Ylim[1]
            Ylim[1] <<- Ylim[1]-wdth/15
            Ylim[2] <<- Ylim[2]+wdth/15
            Ylim <<- sort(Ylim)
        }
        if (sum(Ylim) == 0) {Ylim <<- c(0, 1.1)}
        Plot_Args$xlim <<- Xlim
        Plot_Args$ylim <<- Ylim
   }

   LoadData <- function(){
        NRow <<- length(SelectedCL)  #Nrow represents the number of experiments
        OrigData <<- NULL
        if (NRow == 1) { #just one Dataset selected i.e. one experiment
            SpectIndx <<- grep(SelectedCL, XPSSpectList(tclvalue(XS)))
            SpectName <<- names(FName)[SpectIndx] #to correctly get the coreline names in  case of filtered, differentiated...corelines
            BarData <<- list(x=FName[[SpectIndx]]@.Data[[1]], y=FName[[SpectIndx]]@.Data[[2]])
            NCol <<- length(BarData$y)
            BarNames <<- rep("?", NCol)
            Plot_Args$BarNames <<- BarNames #list(items=BarNames)
            assign("activeSpectName",SpectName,envir=.GlobalEnv)
            assign("activeSpectIndx",SpectIndx,envir=.GlobalEnv)
        }
        if (NRow > 1) {
            NCol <<- length(FName[[SelectedCL[1]]]@.Data[[2]])  #LL represents the N.Data of each experiment
            sapply(SelectedCL, function(x, NCol) { #Sapply sees ONLY local variables
                         if(length(FName[[x]]@.Data[[2]]) != NCol){   #NCol is not seen inside this function
                            tkmessageBox(message="Data to Plot Have Different Length! Cannot continue..", title="ERROR", icon="error")
                            return()
                         }
                   }, NCol)  #Adding LL is a workaround to solve the problem
            BarData$x <<- FName[[SelectedCL[1]]]@.Data[[1]]
            #Data must be organized as follows"
            #
            #       | experim1_S1 experim1_S2 experim1_S1 experim1_S3 |
            #Data = | experim1_S1 experim1_S2 experim1_S1 experim1_S3 |
            #       | experim1_S1 experim1_S2 experim1_S1 experim1_S3 |
            #
            for(ii in 1:NRow){
                OrigData <<- rbind(OrigData, FName[[SelectedCL[ii]]]@.Data[[2]])
            }
            BarNames <<- rep("?", NCol)
            Plot_Args$BarNames <<- BarNames
            #Data organized as 3 Experiments generating Data1, Data2, Data3 values
            #with besides = TRUE, the BarPlot is composed by 3 groups of bars:
            #1) the Data1 of Experim1, Experim2, Experim3   with Color1
            #2) the Data2 of Experim1, Experim2, Experim3   with Color2
            #3) the Data3 of Experim1, Experim2, Experim3   with Color3
            #Then:
            if (Plot_Args$group == "Data"){
                Plot_Args$col <<- Colors[1:NRow]  #NRows runs on the Experiments
                Plot_Args$BarNames <<- rep("?", NCol)
                BarData$y <<- matrix(data=OrigData, nrow=NRow, ncol=NCol)
            } else if (Plot_Args$group == "Acquisition"){
                Plot_Args$col <<- Colors[1:NCol]  #NRows runs on the Data
                Plot_Args$BarNames <<- rep("?", NRow)
                BarData$y <<- t(matrix(data=OrigData, nrow=NRow, ncol=NCol))
            }
            assign("activeSpectName",SpectName,envir=.GlobalEnv)
            assign("activeSpectIndx",SpectIndx,envir=.GlobalEnv)
        }
   }
   
   ShowDatasets <- function(){
        LL <- length(CLNames)
        NColumn <- ceiling((LL+1)/5) #ii runs on the number of rows made of 5 columns
        for(ii in 1:NColumn){
            NN <- (ii-1)*5    #jj runs on 5 column per row
            for(jj in 1:5) {
                if ((jj+NN) > LL) {break} #exit loop if all CL are described
                SelCL <- tkcheckbutton(CLframe, text=CLNames[((ii-1)*5+jj)], variable=CLNames[((ii-1)*5+jj)],
                                 onvalue = CLNames[((ii-1)*5+jj)], offvalue = 0,
                                 command=function(){
                                     SelectedCL <<- sapply(CLNames, function(x){ tclvalue(x) })
                                     SelectedCL <<- intersect(SelectedCL, CLNames) #drop the zeros
                                     if (length(SelectedCL) > 1 ) {
                                         WidgetState(BarGroupFrame, "normal")
                                         WidgetState(BarWidthFrame, "disabled")
                                     } else {
                                         WidgetState(BarGroupFrame, "disabled")
                                         WidgetState(BarGroupFrame, "normal")
                                     }
                                     SetBarColor()
                      })
                tclvalue(CLNames[((ii-1)*5+jj)]) <- FALSE   #initial cehckbutton setting
                tkgrid(SelCL, row = ii, column = jj, padx = 5, pady=5, sticky="w")
            }
        }
   }

   SetBarColor <- function(){
        tkdestroy(ColFrame)
        ColFrame  <<- ttklabelframe(OptnGroup, text = "SET BAR COLORS", borderwidth=3)
        tkgrid(ColFrame , row = 1, column = 1, padx = 0, pady = 0, sticky="w")
        tkgrid( ttklabel(ColFrame , text="Double click to change colors"),
                row = 1, column = 1, padx = 5, pady = c(5, 10))

        #building the widget to change CL colors
        LL <- length(SelectedCL)
        if (LL == 0) { return() }
        
        ColFrame2  <- ttkframe(ColFrame, borderwidth=3, padding=c(0,0,0,0))
        tkgrid(ColFrame2 , row = 2, column = 1, padx = 0, pady = 0, sticky="w")

        RR <- ceiling((LL+1)/5) #ii runs on the number of rows made of 5 columns
        for(ii in 1:RR){
            NN <- (ii-1)*5    #jj runs on 5 column per row
            for(jj in 1:5) {
                if ((jj+NN) > LL) {break} #exit loop if all FitComp are in tkcheckbutton
                     kk <- (ii-1)*5+jj
                     BClr[[kk]] <<- ttklabel(ColFrame2 , text=as.character(kk), width=6, font="Serif 8", background=Colors[kk])
                     tkgrid(BClr[[kk]], row = jj, column = ii, padx = 7, pady = 1, sticky="w")
                     tkbind(BClr[[kk]], "<Double-1>", function( ){
                            X <- as.numeric(tkwinfo("pointerx", BarWindow))
                            Y <- as.numeric(tkwinfo("pointery", BarWindow))
                            WW <- tkwinfo("containing", X, Y)
                            colIdx <- as.numeric(tclvalue(tcl(WW, "cget", "-text")))
                            BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                            tkconfigure(BClr[[colIdx]], background=BKGcolor)
                            Colors[colIdx] <<- BKGcolor
                            Plot_Args$col <<- BKGcolor
                     })
           }
        }
   }

   ResetPlot <- function(){
        activeFName <<- tclvalue(XS)
        FName <<- get(activeFName, envir=.GlobalEnv)
        SpectList <<- XPSSpectList(activeFName)      #list of all the corelines of the activeXPSSample
        SpectList <<- unname(sapply(SpectList, function(x) gsub(" ", "", x)))
        SpectIndx <<- 1
        activeSpectName <<- SpectList[1]
        FNameList <<- XPSFNameList()
        CLNames <<- names(FName)
        SelectedCL <<- NULL

        Xlim <<- sort(range(FName[[SpectIndx]]@.Data[[1]]))
        Ylim <<- sort(range(FName[[SpectIndx]]@.Data[[2]]))
        Xlabel <<- FName[[SpectIndx]]@units[1]
        Ylabel <<- FName[[SpectIndx]]@units[2]
        OrigData <<- NULL
        BarData <<- list(x=NULL, y=NULL)
        NCol <<- NRow <<- NULL
        BarNames <<- NULL
        
        SelectedCL <<- NULL
        BClr <<- list()

        tclvalue(CL) <<- ""
        for (ii in 1:length(CLNames)){
             tclvalue(CLNames[ii]) <<- FALSE
        }
        tclvalue(TITSIZE) <<- 1.6
        tclvalue(NEWTITLE) <<- ""
        tclvalue(LBSIZE) <<- 1.2
        tclvalue(AXNUMSIZE) <<- 1.2
        tclvalue(BNAMSIZE) <<- 1.4
        tclvalue(XAXLAB) <<- ""
        tclvalue(YAXLAB) <<- ""
        tclvalue(BARNAMES) <<- ""
        tclvalue(NORM) <<- NULL
        tclvalue(XMIN) <<- NULL
        tclvalue(YMIN) <<- NULL
        tclvalue(XMAX) <<- NULL
        tclvalue(YMAX) <<- NULL
        tclvalue(BARGRP) <<- "Group Data"
        tclvalue(BARWDTH) <<- 1
        tclvalue(BARSPACING) <<- 0.5

        Plot_Args <<- list(height=NULL, width=1, space=0.5, names.arg=BarNames, beside=TRUE,
                     xlim=NULL,ylim=NULL, main=" ", xlab=" ", ylab=" ", cex.main=1.6,
                     cex.names=1.6, cex.axis=1.2, cex.lab=1.2,
                     background="transparent", col="black",
                     group="Data", legend.text=FALSE
                   )
        answ <- tkmessageBox(message="Reset also Colors?", type="yesno", title="WARNING", icon="warning")
        if (tclvalue(answ) == "yes"){
            Colors <<- c("black", "red3", "limegreen", "blue", "magenta", "orange", "cadetblue", "sienna",
                        "darkgrey", "forestgreen", "gold", "darkviolet", "greenyellow", "cyan", "lightcoral",
                        "turquoise", "deeppink3", "wheat", "thistle", "grey40")
        }

        tkdestroy(ColFrame)
        ColFrame  <<- ttklabelframe(OptnGroup, text = "SET FIT COOMPONENT PALETTE", borderwidth=3)
        tkgrid(ColFrame , row = 1, column = 1, padx = 0, pady = 0, sticky="w")
        tkgrid( ttklabel(ColFrame , text="Double click to change colors"),
                row = 1, column = 1, padx = 5, pady = 5)

        plot.new()
   }

#----- VARIABLES -----

   activeFName <- get("activeFName", envir = .GlobalEnv)
   FNameList <- XPSFNameList()  #list of the XPSSample loaded in the Global Env
   if (length(FNameList) == 0){
       tkmessageBox(message="No XPS Samples found. Please load XPS Data", title="WARNING", icon="warning")
       return()
   }
   FName <- get(activeFName, envir=.GlobalEnv)
   SpectList <- XPSSpectList(activeFName)      #list of all the corelines of the activeXPSSample
   SpectList <- unname(sapply(SpectList, function(x) gsub(" ", "", x)))
   SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv)
   SpectName <- get("activeSpectName", envir=.GlobalEnv)
   if(length(activeSpectName)==0 || is.null(activeSpectName) || is.na(activeSpectName)){
      activeSpectName <<- SpectList[1]
      activeSpectIndx <<- 1
   }
   FNameList <- XPSFNameList()
   CLNames <- names(FName)
   SelectedCL <- NULL

   Xlim <- sort(range(FName[[SpectIndx]]@.Data[[1]]))
   Ylim <- sort(range(FName[[SpectIndx]]@.Data[[2]]))
   Xlabel <- FName[[SpectIndx]]@units[1]
   Ylabel <- FName[[SpectIndx]]@units[2]
   OrigData <- NULL
   BarData <- list(x=NULL, y=NULL)
   NCol <- NRow <- NULL
   BarNames <- NULL
   BClr <- list()

   BGroup <- c("Group by Dataset", "Group by Acquisitions")
   BWidth <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
   Spacing <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2)
   TxtSize <- c(0,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)
   Colors <- c("black", "red3", "limegreen", "blue", "magenta", "orange", "cadetblue", "sienna",
             "darkgrey", "forestgreen", "gold", "darkviolet", "greenyellow", "cyan", "lightcoral",
             "turquoise", "deeppink3", "wheat", "thistle", "grey40")

   XS <- NULL
   CL <- NULL
   TITSIZE <- NULL
   NEWTITLE <- NULL
   LBSIZE <- NULL
   AXNUMSIZE <- NULL
   BNAMSIZE <- NULL
   XAXLAB <- NULL
   YAXLAB <- NULL
   BARNAMES <- NULL
   NORM <- NULL
   XMIN <- NULL
   YMIN <- NULL
   XMAX <- NULL
   YMAX <- NULL
   BARGRP <- NULL
   BARWDTH <- NULL
   BARSPACING <- NULL

   Plot_Args <- list(height=NULL, width=1, space=0.5, names.arg=BarNames, beside=TRUE,
                     xlim=NULL, ylim=NULL, main=" ", xlab=" ", ylab=" ", cex.main=1.6,
                     cex.names=1.6, cex.axis=1.2, cex.lab=1.2,
                     background="transparent", col="black",
                     group="Data", legend.text=FALSE
                   )

   par(mar = c(5.1, 5.1, 4.1, 2.1) ) # increase the left margin, default is c( 5.1 4.1 4.1 2.1)
   plot.new()

#---- GUI ---
   BarWindow <- tktoplevel()
   tkwm.title(BarWindow,"BAR PLOT")
   tkwm.geometry(BarWindow, "+100+50")   #SCREEN POSITION from top-left corner

   BarGroup <- ttkframe(BarWindow, borderwidth=2, padding=c(5,5,5,5) )
   tkgrid(BarGroup, row = 1, column = 1, sticky="w")

# --- Graph Options ---

#---  AxGroup groups first and second columns of widgets
   XsClGroup <- ttkframe(BarGroup, borderwidth=0, padding=c(0,0,0,0))
   tkgrid(XsClGroup, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   XSframe <- ttklabelframe(XsClGroup, text = "SELECT DATA FILE")
   tkgrid(XSframe, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   XS <- tclVar(activeFName)
   XSobj <- ttkcombobox(XSframe, width = 15, textvariable = XS, values = FNameList)
   tkgrid(XSobj, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(XSobj, "<<ComboboxSelected>>", function(){
                        ResetPlot()
                        ClearWidget(CLframe)
                        ShowDatasets()
                        tclvalue(CL) <- ""
                 })

   CLframe <- ttklabelframe(XsClGroup, text = "SELECT ACQUISITIONS")
   tkgrid(CLframe, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
   ShowDatasets()

   AxGroup <- ttkframe(BarGroup, borderwidth=0, padding=c(0,0,0,0))
   tkgrid(AxGroup, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

   NewTitFrame <- ttklabelframe(AxGroup, text = "CHANGE TITLE", borderwidth=3)
   tkgrid(NewTitFrame, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
   NEWTITLE <- tclVar("")  #sets the initial msg
   EnterTitle <- ttkentry(NewTitFrame, textvariable=NEWTITLE, width=18)
   tkgrid(EnterTitle, row = 1, column = 1, padx = 5, pady = 5, sticky="we")
   #now ttkentry waits for a return to read the entry_value
   tkbind(EnterTitle, "<FocusIn>", function(K){
                        tkconfigure(EnterTitle, foreground="red")
                 })
   tkbind(EnterTitle, "<Key-Return>", function(K){
                        tkconfigure(EnterTitle, foreground="black")
                        Plot_Args$main <<- tclvalue(NEWTITLE)
                        CtrlPlot()
                 })

   TitSizeFrame <- ttklabelframe(AxGroup, text = "TITLE SIZE", borderwidth=3)
   tkgrid(TitSizeFrame, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
   TITSIZE <- tclVar("1.4")
   T1obj9 <- ttkcombobox(TitSizeFrame, width = 15, textvariable = TITSIZE, values = TxtSize)
   tkgrid(T1obj9, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(T1obj9, "<<ComboboxSelected>>", function(){
                        Plot_Args$cex.main <<- as.numeric(tclvalue(TITSIZE))
                        CtrlPlot()
                 })

   HelpFrame <- ttklabelframe(AxGroup, text = "HELP", borderwidth=3)
   tkgrid(HelpFrame, row = 2, column = 3, padx = 5, pady = 5, sticky="w")
   HelpButt <- tkbutton(HelpFrame, text=" Get Info.  ", width=10, command=function(){
                        txt <- paste("=> The SELECT ACQUISITIONS shows you the available datasets for the selected File of Data \n",
                                     "   The ACQUISITION represents an Experiment performed to acquire a series of values or Dataset \n",
                                     "   The ACQUISITION contains a series of Datasets representing the experimental data. \n",
                                     "   For example: Acquisition = C1s, (O1s, N1s...), Datasets = how the C1s (O1s, N1s ...) concentration \n",
                                     "   changes in sample1 sample2, sample3, sample4 upon increasing deposition temperature T. \n",
                                     "   For C1s, data are organized as follows: \n",
                                     "   ACQUISITION = C1s  \n",
                                     "   Dataset1 (Sample 1): C1s%(T=100, 300, 600) = 67, 73, 81 \n",
                                     "   Dataset2 (Sample 2): C1s%(T=100, 300, 600) = 45, 51, 55 \n",
                                     "   Dataset3 (Sample 3): C1s%(T=100, 300, 600) = 53, 59, 57 \n",
                                     "   Dataset4 (Sample 4): C1s%(T=100, 300, 600) = 82, 77, 49 \n",
                                     "   --- \n",
                                     "=> GUI options allow setting title, axis labels, and BarNames and their character size. \n",
                                     "    Data are plotted using the default colors. They can be changed by the user. \n",
                                     "=> GROUP BARS: bars can be plotted grouping data in two different ways: \n",
                                     "     (1) Group Data: data of same category are grouped throughout the acquisition: \n",
                                     "         C1s conc. from ALL depositions at 100C (group1), from ALL depositions at 300C (group2) and ALL at 600C (group3);\n",
                                     "         The plot is formed 3 groups composed by 4 bars. \n",
                                     "     (2) Group Acquisition: data from the individual acquisition are grouped: \n",
                                     "         the Dataset1 (group1), Dataset2 (group2), the Dataset3 (group3) and the Dataset4 (group4)\n",
                                     "         The plot is formed 4 groups composed by 3 bars. \n",
                                     sep="")
                        tkmessageBox(message=txt, title="HELP INFO", icon="info")
                 })
   tkgrid(HelpButt, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   CngBNamFrame <- ttklabelframe(AxGroup, text = "-", borderwidth=3)
   tkgrid(CngBNamFrame, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
   CngBNamButtn <- tkbutton(CngBNamFrame, text="CHANGE BAR NAMES", width=18, command=function(){
                         BNames <- data.frame(Plot_Args$BarNames, stringsAsFactors=FALSE)
                         assign("BNames", BNames, envir=.GlobalEnv)
                         BNames <- DFrameTable(Data=BNames, Title="", ColNames="    Bar Names   ", RowNames="",
                                               Width=15, Modify=TRUE, Env=.GlobalEnv, parent=NULL,
                                               Row=1, Column=1, Border=c(3, 3, 3, 3))
                         Plot_Args$BarNames <<- unname(unlist(BNames))
                         CtrlPlot()
                  })
   tkgrid(CngBNamButtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   CngXLabFrame <- ttklabelframe(AxGroup, text = "CHANGE X-LABEL", borderwidth=3)
   tkgrid(CngXLabFrame, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
   XAXLAB <- tclVar()
   CngXLabObj <- ttkentry(CngXLabFrame, textvariable=XAXLAB, width=18)
   tkgrid(CngXLabObj, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(CngXLabObj, "<FocusIn>", function(K){
                        tkconfigure(CngXLabObj, foreground="red")
                 })
   tkbind(CngXLabObj, "<Key-Return>", function(K){
                        tkconfigure(CngXLabObj, foreground="black")
                        Plot_Args$xlab <<- tclvalue(XAXLAB)
                        CtrlPlot()
                 })

   CngYLabFrame <- ttklabelframe(AxGroup, text = "CHANGE Y-LABEL", borderwidth=3)
   tkgrid(CngYLabFrame, row = 3, column = 3, padx = 5, pady = 5, sticky="w")
   YAXLAB <- tclVar()
   CngYLabObj <- ttkentry(CngYLabFrame, textvariable=YAXLAB, width=18)
   tkgrid(CngYLabObj, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(CngYLabObj, "<FocusIn>", function(K){
                        tkconfigure(CngYLabObj, foreground="red")
                 })
   tkbind(CngYLabObj, "<Key-Return>", function(K){
                        tkconfigure(CngYLabObj, foreground="black")
                        Plot_Args$ylab <<- tclvalue(YAXLAB)
                        CtrlPlot()
                 })

   BarNamSize <- ttklabelframe(AxGroup, text = "BAR LABEL SIZE", borderwidth=3)
   tkgrid(BarNamSize, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
   BNAMSIZE <- tclVar(1)
   AxLabObj <- ttkcombobox(BarNamSize, width = 15, textvariable = BNAMSIZE, values = TxtSize)
   tkgrid(AxLabObj, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(AxLabObj, "<<ComboboxSelected>>", function(){
                        Plot_Args$cex.names <<- as.numeric(tclvalue(BNAMSIZE))
                        CtrlPlot()
                 })

   AxNumFrame <- ttklabelframe(AxGroup, text = "AXIS SCALE SIZE", borderwidth=3)
   tkgrid(AxNumFrame, row = 4, column = 2, padx = 5, pady = 5, sticky="w")
   AXNUMSIZE <- tclVar(1)
   AxNumObj <- ttkcombobox(AxNumFrame, width = 15, textvariable = AXNUMSIZE, values = TxtSize)
   tkgrid(AxNumObj, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(AxNumObj, "<<ComboboxSelected>>", function(){
                        Plot_Args$cex.axis <<- as.numeric(tclvalue(AXNUMSIZE))
                        CtrlPlot()
                 })

   AxLabSize <- ttklabelframe(AxGroup, text = "AXIS LABEL SIZE", borderwidth=3)
   tkgrid(AxLabSize, row = 4, column = 3, padx = 5, pady = 5, sticky="w")
   LBSIZE <- tclVar(1)
   AxLabObj <- ttkcombobox(AxLabSize, width = 15, textvariable = LBSIZE, values = TxtSize)
   tkgrid(AxLabObj, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(AxLabObj, "<<ComboboxSelected>>", function(){
                        Plot_Args$cex.lab <<- as.numeric(tclvalue(LBSIZE))
                        CtrlPlot()
                 })

   BarWidthFrame <- ttklabelframe(AxGroup, text = "BAR WIDTH", borderwidth=3)
   tkgrid(BarWidthFrame, row = 5, column = 1, padx = 5, pady = 5, sticky="w")
   BARWDTH <- tclVar("1")
   BarWidth <- ttkcombobox(BarWidthFrame, width = 15, textvariable = BARWDTH, values = BWidth)
   tkgrid(BarWidth, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(BarWidth, "<<ComboboxSelected>>", function(){
                        Plot_Args$width <<- as.numeric(tclvalue(BARWDTH))
                        CtrlPlot()
                 })

   BarSpaceFrame <- ttklabelframe(AxGroup, text = "BAR SPACING", borderwidth=3)
   tkgrid(BarSpaceFrame, row = 5, column = 2, padx = 5, pady = 5, sticky="w")
   BARSPACING <- tclVar("0.5")
   BarSpace <- ttkcombobox(BarSpaceFrame, width = 15, textvariable = BARSPACING, values = Spacing)
   tkgrid(BarSpace, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(BarSpace, "<<ComboboxSelected>>", function(){
                        Plot_Args$space <<- as.numeric(tclvalue(BARSPACING))
                        CtrlPlot()
                 })

   BarGroupFrame <- ttklabelframe(AxGroup, text = "GROUP BARS", borderwidth=3)
   tkgrid(BarGroupFrame, row = 5, column = 3, padx = 5, pady = 5, sticky="w")
   BARGRP <- tclVar("Group Data")
   BGroupSel <- ttkcombobox(BarGroupFrame, width = 15, textvariable = BARGRP, values = BGroup)
   tkgrid(BGroupSel, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(BGroupSel, "<<ComboboxSelected>>", function(){
                        if (tclvalue(BARGRP) == "Group by Dataset"){
                            Plot_Args$group <<- "Data"
                            Plot_Args$col <<- Colors[1:NRow]  #NRows runs on the Experiments
                            Plot_Args$BarNames <<- rep("?", NCol)
                            Plot_Args$legend.text <<- FALSE
                            BarData$y <<- matrix(data=OrigData, nrow=NRow, ncol=NCol)
                            WidgetState(CngLegFrame, "disabled")
                        }
                        if (tclvalue(BARGRP) == "Group by Acquisition"){
                            Plot_Args$col <<- Colors[1:NCol]  #NRows runs on the Data
                            Plot_Args$BarNames <<- rep("?", NRow)
                            Plot_Args$group <<- "Acquisition"
                            Plot_Args$legend.text <<- rep("?", NCol)
                            BarData$y <<- t(matrix(data=OrigData, nrow=NRow, ncol=NCol)) #Transpose data
                            WidgetState(CngLegFrame, "normal")
                        }
                        CtrlPlot()
                 })
   WidgetState(BarGroupFrame, "disabled")


#--- Second group of options: OptnGroup
   OptnGroup <- ttkframe(BarGroup, borderwidth=0, padding=c(0,0,0,0))
   tkgrid(OptnGroup, row = 2, column = 2, padx = 5, pady = 5, sticky="w")

   Spacer <- ttkframe(OptnGroup, borderwidth=3, padding=c(0,0,0,0)) #reserve space for ColFrame Widget
   tkgrid(Spacer, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkgrid( ttklabel(Spacer , text=" "),
           row = 1, column = 1, padx = 5, pady = 5)  #five empty rows to contain the
   tkgrid( ttklabel(Spacer , text=" "),              #color elements
           row = 2, column = 1, padx = 5, pady = 5)
   tkgrid( ttklabel(Spacer , text=" "),
           row = 3, column = 1, padx = 5, pady = 5)
   tkgrid( ttklabel(Spacer , text=" "),
           row = 4, column = 1, padx = 5, pady = 5)
   tkgrid( ttklabel(Spacer , text=" "),
           row = 5, column = 1, padx = 5, pady = 5)
   ColFrame <- ttklabelframe(Spacer, text = "SET BAR COLOR", borderwidth=3)
   tkgrid(ColFrame, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   SetBarColor()

   CngLegFrame <- ttklabelframe(OptnGroup, text = "-", borderwidth=3)
   tkgrid(CngLegFrame, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
   CngLegButtn <- tkbutton(CngLegFrame, text="CHANGE BAR GROUPS", width=18, command=function(){
                         LL <- length(Plot_Args$col) #The number of legend elements == numbe of colors
                         LegText <- rep("?", LL)
                         LegText <- data.frame(LegText, stringsAsFactors=FALSE)
                         assign("LegText", LegText, envir=.GlobalEnv)
                         LegText <- DFrameTable(Data=LegText, Title="", ColNames=" Legend Text ", RowNames="",
                                               Width=15, Modify=TRUE, Env=.GlobalEnv, parent=NULL,
                                               Row=1, Column=1, Border=c(3, 3, 3, 3))
                         Plot_Args$legend.text <<- unname(unlist(LegText))
                         CtrlPlot()
                 })
   tkgrid(CngLegButtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   WidgetState(CngLegFrame, "disabled")


   PlotButt <- tkbutton(AxGroup, text="  PLOT  ", width=18, command=function(){
                        LoadData()
                        CtrlPlot()
                 })
   tkgrid(PlotButt, row = 6, column = 1, padx = 5, pady = 5, sticky="w")

   ResetButt <- tkbutton(AxGroup, text="  RESET  ", width=18, command=function(){
                        ResetPlot()
                        plot.new()
                 })
   tkgrid(ResetButt, row = 6, column = 2, padx = 5, pady = 5, sticky="w")


   ExitButt <- tkbutton(AxGroup, text="  EXIT  ", width=18, command=function(){
                        assign("activeSpectName", names(FName)[1], envir=.GlobalEnv)
                        assign("activeSpectIndx", 1, envir=.GlobalEnv)
                        tkdestroy(BarWindow)
                        par(mar = c(5.1, 4.1, 4.1, 2.1))
                 })
   tkgrid(ExitButt, row = 6, column = 3, padx = 5, pady = 5, sticky="w")

}

