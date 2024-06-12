# XPSElemTab builds the element tables for assigning elements in a survey spectrum

#' @title XPSElemTab
#' @description XPSElemTab constructs the CoreLine and Auger Transition tables
#'   this function helps the identification of chemical elements in Survey spectra.
#' @examples
#' \dontrun{
#' 	XPSElemTab()
#' }
#' @export
#'

XPSElemTab <-function() {

ShowLines <- function(){
              LL <- length(Elmt)
              if (Elmt == "" || LL == 0) { return() }
              if (tclvalue(HLD) == 0){
                  plot(Object[[SpectIndx]])   #refresh plot
              }
              if (tclvalue(SCL) == "1") {
                 idx <- grep(Elmt, ElmtList1[,1])
                 for (ii in seq_along(idx)){
                     xx <- ElmtList1[idx[ii],3]
                     lines(x=c(xx, xx), y=rangeY, col="red")   #plot corelines of the selected elements
                 }
              }
              if (tclvalue(SAug) == "1") {
                  idx <- grep(Elmt, ElmtList2[,1])
                  for (ii in seq_along(idx)){
                      xx <- ElmtList2[idx[ii],3]
                      lines(x=c(xx, xx), y=rangeY, col="blue") #plot corelines of the selected elements
                  }
              }
   }


#---- Var-Initialization
   plot.new()
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   Object <- get(activeFName,envir=.GlobalEnv)
   FNameList <- XPSFNameList() #list of XPSSamples
   SpectList <- XPSSpectList(activeFName)
   SpectIndx <- grep("Survey",SpectList)[1]   #switch to the survey spectrum
   if (length(SpectIndx) == 0 || is.na(SpectIndx)){
       SpectIndx <- grep("survey",SpectList)[1]
   }
   rangeX <- range(Object[[SpectIndx]]@.Data[2])
   rangeY <- range(Object[[SpectIndx]]@.Data[2])
   plot(Object[[SpectIndx]])
   RecPlot <- recordPlot()   #save graph for UNDO
   if (Object[[SpectIndx]]@Flags[3]) {
       ftype <- "scienta" #scienta filetype
   } else {
       ftype <- "kratos"  #kratos filetype
   }
   ElmtList1 <- ReadElmtList("CoreLines") #reads the CoreLine Table see XPSSurveyUtilities()
   ElmtList1 <- format(ElmtList1, justify="centre", width=10)
   ElmtList1 <- as.data.frame(ElmtList1,  stringsAsFactors = FALSE)
   ElmtList2 <- ReadElmtList("AugerTransitions") #reads the Auger Table see XPSSurveyUtilities()
   ElmtList2 <- format(ElmtList2, justify="centre", width=10)
   ElmtList2 <- as.data.frame(ElmtList2,  stringsAsFactors = FALSE)
   Elmt <- ""
   Transition <- ""

#----- GUI -----
   mainWin <- tktoplevel()
   tkwm.title(mainWin,"CORE LINES AND AUGER ELEMENT TABLES")
   tkwm.geometry(mainWin, "+100+50")

   TblGroup <- ttkframe(mainWin, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(TblGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   Tblframe1 <- ttklabelframe(TblGroup, text="Peak Table", borderwidth=2, padding=c(5,5,5,5) )
   tkgrid(Tblframe1, row = 1, column = 1, padx = 5, pady = c(20, 5), sticky="we")
#--- Insert Core Line Header
   CLheader <- format(c("Element", "Orbital ", "   BE ", "   KE   ", "RSF_K   ", "RSF_S    "),
                       justify = "centre", width = 11) #let header as it is to be aligned with ElementTbl
   CLHeaderList <- tklistbox(Tblframe1, selectmode = "single", height=1, font="Courier 10 bold",
                             width = 65, background="#E0E0E0",borderwidth=0)
   tkgrid(CLHeaderList, row = 1, column = 1, padx = 5, pady = 0, sticky="w")
   tcl(CLHeaderList, "insert", "end", paste(CLheader, collapse = ""))
#--- Core Line Element Table
   ElementTbl <- tklistbox(Tblframe1, selectmode = "single", height=13, font="Courier 10 normal",
                           width = 65, borderwidth=0)
   tkgrid(ElementTbl, row = 2, column = 1, padx = 5, pady =c(0,5), sticky="w")
   LL <- length(ElmtList1[[1]])
#--- Insert data rows
   for (ii in 1:LL) {
        row_data <- paste(ElmtList1[ii, ], collapse="")
        tcl(ElementTbl, "insert", "end", row_data)
   }
   tkbind(ElementTbl, "<Double-1>", function() {
                    Elmt <<- as.numeric(tclvalue(tcl(ElementTbl, "curselection")))+1
                    Elmt <<- ElmtList1[[1]][Elmt] #get the selected element symbol
                    Elmt <<- trimws(Elmt, which="left") #remove white spaces at beginning
                    tclvalue(ELMT) <- Elmt
                    idx <- grep(Elmt, ElmtList1[[1]])
                    Elmt <<- ElmtList1[[1]][idx]
                    if (Elmt == "") return()
                    RecPlot <<- recordPlot()   #save the graph for UNDO option
                    ShowLines()
          })
   tkgrid.columnconfigure(Tblframe1, 1, weight=3)

#---------
   Tblframe2 <- ttklabelframe(TblGroup, text="Auger Transitions", borderwidth=2, padding=c(5,5,5,5) )
   tkgrid(Tblframe2, row = 2, column = 1, padx = 5, pady = c(20, 5), sticky="we")
#--- Insert Auger Header
   Augerheader <- format(c("Element", "Transition", "   BE ", "  KE   ", "RSF_K   ", "RSF_S    "),
                         justify = "centre", width = 11) #let header as it is to be aligned with ElementTbl
   AugerHeaderList <- tklistbox(Tblframe2, selectmode = "single", height=1, font="Courier 10 bold",
                             width = 65, background="#E0E0E0",borderwidth=0)
   tkgrid(AugerHeaderList, row = 1, column = 1, padx = 5, pady = 0, sticky="w")
   tcl(AugerHeaderList, "insert", "end", paste(Augerheader, collapse = ""))
#--- Auger Transition Table
   TransitionTbl <- tklistbox(Tblframe2, selectmode = "single", height=13, font="Courier 10 normal",
                           width = 65, borderwidth=0)
   tkgrid(TransitionTbl, row = 2, column = 1, padx = 5, pady =c(0,5), sticky="w")
   LL <- length(ElmtList2[[1]])
#--- Insert data rows
   for (ii in 1:LL) {
        row_data <- paste(ElmtList2[ii, ], collapse="")
        tcl(TransitionTbl, "insert", "end", row_data)
   }
   tkbind(TransitionTbl, "<Double-1>", function() {
                    Elmt <<- as.numeric(tclvalue(tcl(TransitionTbl, "curselection")))+1
                    Elmt <<- ElmtList2[[1]][Elmt] #get the selected element symbol
                    Elmt <<- trimws(Elmt, which="left") #remove white spaces at beginning
                    tclvalue(ELMT) <- Elmt
                    idx <- grep(Elmt, ElmtList2[[1]])
                    Elmt <<- ElmtList2[[1]][idx]
                    if (Elmt == "") return()
                    RecPlot <<- recordPlot()   #save the graph for UNDO option
                    ShowLines()
          })
   tkgrid.columnconfigure(Tblframe2, 1, weight=3)

   Tblframe3 <- ttklabelframe(mainWin, text="Options", borderwidth=2, padding=c(5,5,5,5) )
   tkgrid(Tblframe3, row = 1, column = 2, padx = 5, pady = 5, sticky="we")

#   OptnGroup <- ttkframe(Tblframe3, borderwidth=0, padding=c(0,0,0,0) )
#   tkgrid(OptnGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   XS <- tclVar(activeFName)
   XPS.Sample <- ttkcombobox(Tblframe3, width = 20, textvariable = XS, values = FNameList)
   tkgrid(XPS.Sample, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(XPS.Sample, "<<ComboboxSelected>>", function(){
                    activeFName <<- tclvalue(XS)
                    Object <<- get(activeFName, envir=.GlobalEnv)
                    SpectList <<- XPSSpectList(activeFName)
                    tkconfigure(Core.Lines, values=SpectList)
                    rangeX <<- range(Object[[SpectIndx]]@.Data[2])
                    rangeY <<- range(Object[[SpectIndx]]@.Data[2])
                    plot(Object[[SpectIndx]])
                    RecPlot <<- recordPlot()    #save graph for UNDO
#                    WidgetState(Core.Lines, "normal")
          })

   CL <- tclVar()
   Core.Lines <- ttkcombobox(Tblframe3, width = 20, textvariable = CL, values = SpectList)
   tkgrid(Core.Lines, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(Core.Lines, "<<ComboboxSelected>>", function(){
                    SpectName <- tclvalue(CL)
                    SpectIndx <<- grep(SpectName, SpectList)
                    if (length(SpectIndx) == 0) {
                        tkmessageBox(message="Please Select a Survey Spectrum Please", title="WARNING", icon="warning")
                    }
                    rangeX <<- range(Object[[SpectIndx]]@.Data[2])
                    rangeY <<- range(Object[[SpectIndx]]@.Data[2])
                    plot(Object[[SpectIndx]])
                    RecPlot <<- recordPlot()    #save graph for UNDO
          })
#   WidgetState(Core.Lines, "disabled")

   ELMT <- tclVar("Element? ")  #sets the initial msg
   Search <- ttkentry(Tblframe3, textvariable=ELMT, width=10, foreground="grey")
   tkbind(Search, "<FocusIn>", function(K){
                    tkconfigure(Search, foreground="red")
                    tclvalue(ELMT) <- ""
          })
   tkbind(Search, "<Key-Return>", function(K){
                    tkconfigure(Search, foreground="black")
                    Elmt <- tclvalue(ELMT)
                    if (Elmt=="") return()
                    Elmt <<- paste(Elmt, " ", sep="")
                    RecPlot <<- recordPlot()   #save the graph for UNDO option
                    ShowLines()
                    tclvalue(ELMT) <- ""
          })
   tkgrid(Search, row = 3, column = 1, padx = 7, pady = 5, sticky="w")

   SCL <- tclVar("1")
   ShowCL <- tkcheckbutton(Tblframe3, text="Core Lines", variable=SCL, onvalue=1, offvalue=0,
                    command=function(){  ShowLines() })
   tkgrid(ShowCL, row = 4, column = 1, padx = 7, pady = 5, sticky="w")

   SAug <- tclVar("0")
   ShowAuger <- tkcheckbutton(Tblframe3, text="Auger Transitions", variable=SAug, onvalue=1, offvalue=0,
                    command=function(){  ShowLines() })
   tkgrid(ShowAuger, row = 5, column = 1, padx = 7, pady = 5, sticky="w")

   HLD <- tclVar(FALSE)
   HoldPlot <- tkcheckbutton(Tblframe3, text="Hold plot", variable=HLD, onvalue=1, offvalue=0,
                    command=function(){
                         Elmt <<- ""
                         ShowLines()
          })
   tkgrid(HoldPlot, row = 6, column = 1, padx = 7, pady = 5, sticky="w")

   CurGroup <- ttklabelframe(Tblframe3, text="Position", borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(CurGroup, row = 7, column = 1, padx = 5, pady = 5, sticky="w")

   CurBtn <- tkbutton(CurGroup, text="CURSOR", width=10, command=function(){
                    tkmessageBox(message="LEFT click to move marker's position; RIGHT to exit" , title = "WARNING",  icon = "warning", parent=mainWin)
                    RecPlot <<- recordPlot()   #save the graph for UNDO option
                    pos <- c(1,1) # only to enter in  the loop
                    while (length(pos) > 0) {  #pos != NULL => mouse right button not pressed
                        pos <- locator(n=1, type="p", pch=3, cex=1.5, lwd=1.8, col="red")
                        if (length(pos) > 0) { #right mouse button not pressed
                           replayPlot(RecPlot) #refresh graph  to cancel previous cursor markers
                           points(pos, type="p", pch=3, cex=1.5, lwd=1.8, col="red")
                           pos <- round(x=c(pos$x, pos$y), digits=2)
                           txt <- paste("X: ", as.character(pos[1]), ", Y: ", as.character(pos[2]), sep="")
                           tkconfigure(CurPos, text=txt)
                           tcl("update", "idletasks")
                        }
                    }
                    replayPlot(RecPlot) #refresh graph  to cancel previous cursor markers

          })
   tkgrid(CurBtn, row = 1, column = 1, padx = 7, pady = 5, sticky="w")

   CurPos <- ttklabel(CurGroup, text="X, Y:                      ")
   tkgrid(CurPos, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

#   ButtGroup <- ttkframe(Tblframe3, borderwidth=0, padding=c(0,0,0,0) )
#   tkgrid(ButtGroup, row = 5, column = 1, padx = 0, pady = 0, sticky="w")

   UndoBtn <- tkbutton(Tblframe3, text="UNDO", width=15, command=function(){
                    Elmt <<- ""
                    replayPlot(RecPlot)
          })
   tkgrid(UndoBtn, row = 8, column = 1, padx = c(5, 10) , pady = 5, sticky="we")

   UndoBtn <- tkbutton(Tblframe3, text="REFRESH", width=15, command=function(){
                    Elmt <<- ""
                    plot(Object[[SpectIndx]])
                    RecPlot <<- recordPlot()   #save graph for UNDO
          })
   tkgrid(UndoBtn, row = 9, column = 1, padx = c(5, 10), pady = 5, sticky="we")

   exitBtn <- tkbutton(Tblframe3, text="EXIT", width=15, command=function(){
                    tkdestroy(mainWin)
         })
   tkgrid(exitBtn, row = 10, column = 1, padx = c(5, 10), pady = 5, sticky="we")
   tcl("update", "idletasks")
   
   addScrollbars(Tblframe1, ElementTbl, type="y", Row = 2, Col = 1, Px=0, Py=0)
   addScrollbars(Tblframe2, TransitionTbl, type="y", Row = 2, Col = 1, Px=0, Py=0)

}



