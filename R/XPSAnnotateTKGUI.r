# XPSAnnotate function to add labels and text to lattice graphs

#' @title XPSAnnotate is graphic function to add text to lattice plots
#' @description XPSAnnotate() adds text to spectra plotted using (\code{matplot})base function.
#' @seealso \link{matplot}, \link{plot}
#' @examples
#' \dontrun{
#' 	XPSAnnotate()
#' }
#' @export
#'


XPSAnnotate <- function(){

   CtrlPlot <- function(){
      if (exists("RxpsGGraph")){
         plot.new()
         RxpsGGraph <- get("RxpsGGraph", envir=.GlobalEnv)
         plot(RxpsGGraph)
      } else {
         replayPlot(InitialGraph)
      }
      text(x=TextParam$TxtPos$x, y=TextParam$TxtPos$y,    #add the full array of annotations in the selected
           adj=c(-0.05,-0.05), labels=TextParam$Txt,      #positions with the selected colors and sizes
           cex=TextParam$TxtSize, col=TextParam$TxtCol)
      points(ArrPos1$x, ArrPos1$y,pch=20, col="black") #first mark the arrow start point
      arrows(ArrPos1$x, ArrPos1$y, ArrPos2$x, ArrPos2$y, length = 0.05, col = "black")
   }


#--- Refresh original plot
    if (exists("RxpsGGraph")){
       plot.new()
       RxpsGGraph <- get("RxpsGGraph", envir=.GlobalEnv)
       plot(RxpsGGraph)
    } else {
       graph <- recordPlot()
       InitialGraph <- graph
    }
#--- variables
   if (is.na(activeFName)){
       tkmessageBox("No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }

   FName <- get(activeFName, envir=.GlobalEnv)   #Load the active XPSSample
   ActiveFName <- get("activeFName", envir=.GlobalEnv)  #Load the XPSSample name
   SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv)#index of Active CoreLine
   SpectList <- XPSSpectList(ActiveFName)                   #List of all Corelines in the XPSSample

   FontColor <- XPSSettings$Colors
   FontSize <- c(0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,3)
   TextParam <- list(Txt="", TxtPos=list(x=NA, y=NA), TxtSize=1, TxtCol="black")
   ArrPos1 <- list(x=NA, y=NA)
   ArrPos2 <- list(x=NA, y=NA)
   idx <- 1
   AnnotateText <- "?"
   Accepted <- FALSE



#===== GUI =====
     AnnWin <- tktoplevel()
     tkwm.title(AnnWin,"ANNOTATE")
     tkwm.geometry(AnnWin, "+100+50")   #SCREEN POSITION from top-left corner

     AnnGroup <- ttkframe(AnnWin,  borderwidth=0, padding=c(0,0,0,0))
     tkgrid(AnnGroup, row = 1, column=1, sticky="we")

     INFOframe <- ttklabelframe(AnnGroup, text = " HELP ", borderwidth=2)
     tkgrid(INFOframe, row = 2, column = 1, padx = 5, pady = 5, sticky="we")
     tkgrid( ttklabel(INFOframe, text="1. Input the label and and then locate the position"),
             row = 1, column=1, pady=2, sticky="w")
     tkgrid( ttklabel(INFOframe, text="2. Change Size and Color if Needed"),
             row = 2, column=1, pady=2, sticky="w")
     tkgrid( ttklabel(INFOframe, text="3. ACCEPT if Label OK or UNDO to the Previous Plot "),
             row = 3, column=1, pady=2, sticky="w")

     Anframe1 <- ttklabelframe(AnnGroup, text = " Text ", borderwidth=2)
     tkgrid(Anframe1, row = 3, column = 1, padx = 5, pady = 5, sticky="we")
     tkgrid( ttklabel(Anframe1, text=" Text to Annotate: "),
             row = 1, column = 1, padx = 5, pady = 5, sticky="we")
     AnnTxt <- tclVar("Label?")
     AnnEntry1 <- ttkentry(Anframe1, textvariable=AnnTxt, foreground="grey")
     tkgrid(AnnEntry1, row = 1, column = 2, padx=5, pady=5, sticky="we")
     tkbind(AnnEntry1, "<FocusIn>", function(K){
                            tclvalue(AnnTxt) <- ""
                            tkconfigure(AnnEntry1, foreground="red")
                     })
     tkbind(AnnEntry1, "<Key-Return>", function(K){
                            if (TextParam$Txt[[idx]] != "" && Accepted == FALSE){
                                answ <- tclVar()
                                answ <- tkmessageBox(message="Save the Previous Annotation?", type="yesno", title="WARNING", icon="warning")
                                if (tclvalue(answ) == "yes"){
                                    idx <<- idx+1
                                    TextParam$Txt[idx] <<- ""
                                    TextParam$TxtPos$x[idx] <<- NA
                                    TextParam$TxtPos$y[idx] <<- NA
                                    TextParam$TxtSize[idx] <<- 1
                                    TextParam$TxtCol[idx] <<- "black"
                                    ArrPos1 <- list(x=NA, y=NA)
                                    ArrPos2 <- list(x=NA, y=NA)
#                                    Accepted <<- TRUE
                                }
                            }
                            TextParam$Txt[idx] <<- tclvalue(AnnTxt)  #add new text to annotattion array
                            tclvalue(AnnTxt) <- ""
                            Accepted <<- FALSE
                     })


     Anframe2 <- ttklabelframe(AnnGroup, text = "  Set Text Position ", borderwidth=2)
     tkgrid(Anframe2, row = 5, column = 1, padx = 5, pady = 5, sticky="we")
     TxtButt <- tkbutton(Anframe2, text=" TEXT POSITION ", command=function(){
                            if (TextParam$Txt[[idx]] == "" ) {
                                tkmessageBox(message="Please set the Label Text or Position first!", title="WARNING: position lacking", icon="warning")
                                return()
                            }
                            WidgetState(Anframe1, "disabled")
                            WidgetState(Anframe2, "disabled")
                            WidgetState(Anframe3, "disabled")
                            WidgetState(BtnGroup, "disabled")
                            pos <- locator(n=1)                 #each of the TextParam$Txt[idx]
                            if (length(pos) == 0)  {            #has  its X, Y position on the graph
                               return()
                            }
                            TextParam$TxtPos$x[idx] <<- pos$x   #save position of the correspondent TextParam$Txt[idx]
                            TextParam$TxtPos$y[idx] <<- pos$y
                            CtrlPlot()
                            WidgetState(Anframe1, "normal")
                            WidgetState(Anframe2, "normal")
                            WidgetState(Anframe3, "normal")
                            WidgetState(BtnGroup, "normal")
                            txt <- round(x=c(TextParam$TxtPos$x[idx], TextParam$TxtPos$y[idx]), digits=2)
                            txt <- paste("X: ", as.character(txt[1]), ", Y: ", as.character(txt[2]), sep="")
                            tkconfigure(AnnotePosition, text=txt)
                     })
     tkgrid(TxtButt, row = 1, column = 1, padx = 5, pady = 5, sticky="we")

     AnnotePosition <- ttklabel(Anframe2, text=" Text Position                ")
     tkgrid(AnnotePosition, row = 1, column = 2, padx = 5, pady = 5, sticky="we")

     Anframe3 <- ttklabelframe(AnnGroup, text = " Text Size & Color ", borderwidth=2)
     tkgrid(Anframe3, row = 7, column = 1, padx = 5, pady =c(2, 5), sticky="we")
     tkgrid( ttklabel(Anframe3, text=" Size "),
             row = 1, column = 1, padx = 5, pady = 2, sticky="w")
     tkgrid( ttklabel(Anframe3, text=" Color "),
             row = 1, column = 2, padx = 5, pady = 2, sticky="w")
     TSIZE <- tclVar("1")
     AnnoteSize <- ttkcombobox(Anframe3, width = 15, textvariable = TSIZE, values = FontSize)
     tkbind(AnnoteSize, "<<ComboboxSelected>>", function(){
                            if (is.na(TextParam$TxtPos$x[idx]) || TextParam$Txt[idx]=="" ) {
                                tkmessageBox(message="Please set the Label Text and Position first!", title="WARNING: position lacking", icon="warning")
                            } else {
                                TextParam$TxtSize[idx] <<- as.numeric(tclvalue(TSIZE)) #each of the TextParam$Txt[idx] has its Size
                                TextParam$TxtCol[idx] <<- tclvalue(TCOLOR)             #each of the TextParam$Txt[idx] has its Color
                                CtrlPlot()
                            }
                     })
     tkgrid(AnnoteSize, row = 2, column = 1, padx = 5, pady =c(2, 5), sticky="w")

     TCOLOR <- tclVar("black")
     AnnoteColor <- ttkcombobox(Anframe3, width = 15, textvariable = TCOLOR, values = FontColor)
     tkbind(AnnoteColor, "<<ComboboxSelected>>", function(){
                            if (is.na(TextParam$TxtPos$x[idx]) || TextParam$Txt[idx]=="") {
                                tkmessageBox(message="Please set the Label Text and Position first!", title="WARNING: position lacking", icon="warning")
                            } else {
                                TextParam$TxtSize[idx] <<- as.numeric(tclvalue(TSIZE)) #each of the TextParam$Txt[idx] has its Size
                                TextParam$TxtCol[idx] <<- tclvalue(TCOLOR)             #each of the TextParam$Txt[idx] has its Color
                                CtrlPlot()
                            }
                     })
     tkgrid(AnnoteColor, row = 2, column = 2, padx = 5, pady =c(2, 5), sticky="w")

     BtnGroup <- ttkframe(AnnWin,  borderwidth=0, padding=c(0,0,0,0))
     tkgrid(BtnGroup, row = 8, column = 1, sticky="we")
     tkgrid.columnconfigure(BtnGroup, 1, weight = 1)  #needed to extend buttons with sticky="we"

     AddArwButt <- tkbutton(BtnGroup, text=" ADD ARROW ", command=function(){
                            WidgetState(Anframe2, "disabled")
                            WidgetState(Anframe3, "disabled")
                            WidgetState(BtnGroup, "disabled")
                            pos <- locator(n=1, type="p", pch=20, col="black") #first mark the arrow start point
                            ArrPos1$x[idx] <<- pos$x
                            ArrPos1$y[idx] <<- pos$y
                            pos <- locator(n=1, type="n") #the arrow ending point
                            ArrPos2$x[idx] <<- pos$x
                            ArrPos2$y[idx] <<- pos$y
                            CtrlPlot()
                            WidgetState(Anframe1, "normal")
                            WidgetState(Anframe2, "normal")
                            WidgetState(Anframe3, "normal")
                            WidgetState(BtnGroup, "normal")
                     })
     tkgrid(AddArwButt, row = 1, column = 1, padx = 5, pady = 5, sticky="we")

     AcceptButt <- tkbutton(BtnGroup, text=" ACCEPT ", command=function(){
                            idx <<- idx+1
                            TextParam$Txt[idx] <<- ""
                            TextParam$TxtPos$x[idx] <<- NA
                            TextParam$TxtPos$y[idx] <<- NA
                            TextParam$TxtSize[idx] <<- 1
                            TextParam$TxtCol[idx] <<- "black"
                            ArrPos1$x[idx] <<- NA
                            ArrPos1$y[idx] <<- NA
                            ArrPos2$x[idx] <<- NA
                            ArrPos2$y[idx] <<- NA
                            Accepted <<- TRUE
                     })
     tkgrid(AcceptButt, row = 2, column = 1, padx = 5, pady = 5, sticky="we")

     UndoButt <- tkbutton(BtnGroup, text=" UNDO ", command=function(){
                            RxpsGGraph <- get("RxpsGGraph", envir=.GlobalEnv)
                            plot(RxpsGGraph)
                            #eliminate last label
                            TextParam$Txt[idx] <<- ""
                            TextParam$TxtPos$x[idx] <<- NA
                            TextParam$TxtPos$y[idx] <<- NA
                            TextParam$TxtSize[idx] <<- 1
                            TextParam$TxtCol[idx] <<- "black"
                            ArrPos1$x[idx] <<- NA
                            ArrPos1$y[idx] <<- NA
                            if (idx > 1) {
                                CtrlPlot()
                            }
                     })
     tkgrid(UndoButt, row = 3, column = 1, padx = 5, pady = 5, sticky="we")

     UndoButt <- tkbutton(BtnGroup, text=" RESET PLOT ", command=function(){
                            if (exists("RxpsGGraph")){
                                RxpsGGraph <- get("RxpsGGraph", envir=.GlobalEnv)
                                plot(RxpsGGraph)
                            } else {
                                replayPlot(InitialGraph)
                            }
                     })
     tkgrid(UndoButt, row = 4, column = 1, padx = 5, pady = 5, sticky="we")


     ExitButt <- tkbutton(BtnGroup, text="  EXIT  ", command=function(){
                            tkdestroy(AnnWin)
                     })
     tkgrid(ExitButt, row = 5, column = 1, padx = 5, pady = 5, sticky="we")

}

