# GUI to split an XPSSample in groups its corelines
# The GUI shows a list of the XPSSamples loaded
# For the selected XPSSample the GUI shows the list of Corelines acquired
# A group of corelines can be chosen through a checkbox
# The chosen corelines may be saved in a new XPSSample.

#' @title XPSSplit
#' @description XPSSplit to select split multiple acquisitions performed
#    with the ScientaGammadata or other instruments
#'   Acquisitions on multiple samples may be included in a single .PXT file
#'   This function allows splitting spectra corresponding to each sample in
#'   individual files.
#' @examples
#' \dontrun{
#' 	XPSSplit()
#' }
#' @export
#'




XPSSplit <- function() {

      CKSelCL <- function(){ #Check which are the selected corelines in a column of checkboxes
             SelectedCL <<- NULL
             SelectedIdx <<- NULL
             jj <- 1
             for(ii in 1:length(CoreLines)){
                 item <- tclvalue(CL[[ii]])
                 if (item != "0"){
                     SelectedCL[jj] <<- tclvalue(CL[[ii]])
                     tkconfigure(CoreLineCK[[ii]], foreground="red")
                     SelectedIdx[jj] <<- ii
                     jj <- jj+1
                 }
             }
      }

      MakeSplitCL <- function(){  #construct the list of checkboxes describing all the XPSSample corelines
             CL <<- NULL
             CoreLineCK <<- list()
             CoreLines <<- XPSSpectList(activeFName)
             LL <- length(CoreLines)
             N.CKboxGrps <- ceiling(LL/15) #ii runs on the number of columns
             for(ii in 1:N.CKboxGrps){
                 NN <- (ii-1)*15    #jj runs on the number of column_rows
                 for(jj in 1:15) {
                    CL[(jj+NN)] <<- jj+NN #'variable' MUST be initialized with different values
                     if ((jj+NN) > LL) {break} #exit loop if all FitComp are in RadioBtn
                     CoreLineCK[[(jj+NN)]] <<- tkcheckbutton(T1frameCoreLines, text=CoreLines[(jj+NN)],
                                                 variable=CL[(jj+NN)],  onvalue=CoreLines[(jj+NN)],
                                                 offvalue = 0, command=function(){
                                                        SelectedCL <<- NULL
                                                        SelectedIdx <<- NULL
                                                        jj <- 1
                                                        for(ii in 1:length(CoreLines)){
                                                            item <- tclvalue(CL[[ii]])
                                                            if (item != "0"){
                                                                SelectedCL[jj] <<- tclvalue(CL[[ii]])
                                                                tkconfigure(CoreLineCK[[ii]], foreground="red")
                                                                SelectedIdx[jj] <<- ii
                                                                jj <- jj+1
                                                            }
                                                            if (item == "0"){
                                                                tkconfigure(CoreLineCK[[ii]], foreground="black")
                                                            }
                                                        }
                                             })
                     tclvalue(CL[(jj+NN)]) <<- FALSE
                     tkgrid(CoreLineCK[[(jj+NN)]], row = jj, column = ii, padx = 5, pady=5, sticky="w")
                 }
             }
      }



#--- Variables
    CoreLines <- NULL
    CoreLineList <- list()   #define a list for the XPSSample corelines
    CoreLineCK <- list()     #define a list of the Gwidget
    N.CKboxGrps <- NULL      #N. of checkboxgroups used to descibe the XPSSample corelines
    SelectedCL <- NULL
    SelectedIdx <- NULL
    XS <- NULL
    CL <- NULL
   
    CLidx <- NULL
    if (exists("activeFName") == FALSE){
        tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
        return()
    }

    FName <- get(activeFName, envir=.GlobalEnv)   #load the active XPSSample in memory
    activeFName <- get("activeFName", envir=.GlobalEnv)  #carico il nome XPSSample (stringa)
    FNameList <- XPSFNameList()     #list of all loaded XPSSamples in .GlobalEnv
    FNameIdx <- grep(activeFName, FNameList)

#--- GUI
    SplitWin <- tktoplevel()
    tkwm.title(SplitWin,"XPS ANALYSIS")
    tkwm.geometry(SplitWin, "+100+50")   #position respect topleft screen corner
    SplitGroup1 <- ttkframe(SplitWin, borderwidth=0, padding=c(0,0,0,0) )
    tkgrid(SplitGroup1, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

    SplitGroup2 <- ttkframe(SplitGroup1, borderwidth=0, padding=c(0,0,0,0) )
    tkgrid(SplitGroup2, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

    T1frameFName <- ttklabelframe(SplitGroup2, text = "Select the XPS-SAMPLE", borderwidth=2)
    tkgrid(T1frameFName, row = 1, column = 1, padx = 5, pady = 5, sticky="we")
    XS <- tclVar()
    for(ii in 1:length(FNameList)){
        XSRadio <- ttkradiobutton(T1frameFName, text=FNameList[ii], variable=XS, value=FNameList[ii],
                        command=function(){
                            activeFName <<- tclvalue(XS)
                            assign("activeFName", activeFName, envir=.GlobalEnv)
                            FName <<- get(activeFName, envir=.GlobalEnv)
                            activeFName <- get("activeFName", envir=.GlobalEnv)  #load the active XPSSample in memory
                            FNameList <<- XPSFNameList()     #list of all loaded XPSSamples in .GlobalEnv
                            FNameIdx <<- grep(activeFName, FNameList)
                            clear_widget(T1frameCoreLines)
                            MakeSplitCL()
                            plot(FName)
                        })
        tkgrid(XSRadio, row = ii, column = 1, padx = 5, pady = 2, sticky="w")
    }

    T1frameCoreLines <- ttklabelframe(SplitGroup1, text = "Select the CORE LINES to export", borderwidth=2)
    tkgrid(T1frameCoreLines, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
    MakeSplitCL()

    ExportBtn <- tkbutton(SplitGroup2, text=" SAVE SELECTED SPECTRA ", width=25, command=function(){
                            spectNames <- NULL
                            CLname <- NULL
                            CLidx <<- NULL
	                           NewFName <- new("XPSSample")
                            LL <- length(SelectedCL)
                            mm <- length(FName)
                            for (jj in 1:LL){
	                               NewFName[[jj]] <- new("XPSCoreLine")
                                CLname <- unlist(strsplit(SelectedCL[jj], "\\."))   #skip the number at coreline name beginning
                                CLidx[jj] <<- as.integer(CLname[1])
                                spectNames[jj] <- CLname[2]
                                NewFName[[jj]] <- FName[[CLidx[jj]]]
                                ii <- SelectedIdx[jj]
                                WidgetState(CoreLineCK[[ii]], "disabled")
                            }
                            NewFName@Project <- FName@Project
                            NewFName@Sample <- FName@Sample
                            NewFName@Comments <- FName@Comments
                            NewFName@User <- FName@User
                            NewFName@names <- spectNames
                            plot(NewFName)

                            PathFile <- tclvalue(tkgetSaveFile())
                            if( length(PathFile) == 0 ){ return() }
                            PathName <- dirname(PathFile)
                            FileName <- basename(PathFile) #extract the filename from complete
                            FileName <- unlist(strsplit(FileName, "\\."))
                            if (is.na(FileName[2]) || FileName[2] != "RData"){
                               tkmessageBox(message="Extension of the destination file forced to .RData!" , title = "DESTINATION FILE EXTENSION",  icon = "warning")
                            }
                            PathFile <- paste(PathName, "/", FileName[1], ".", "RData", sep="")
                            NewFName@Sample <- PathFile
                            FileName <- paste(FileName[1], ".", "RData", sep="")
                            NewFName@Filename <- FileName
                            assign(FileName,NewFName, envir=.GlobalEnv) #salvo il nuovo XPSSample nel GlobalEnvir
                            command=paste("save('", FileName,"', file='",PathFile, "', compress=TRUE)", sep="")
                            eval(parse(text=command),envir=.GlobalEnv)
                            cat("\n Data saved in: ", PathFile)
                            XPSSaveRetrieveBkp("save")
                        })
    tkgrid(ExportBtn, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

    ClearBtn <- tkbutton(SplitGroup2, text=" CLEAR SELECTIONS ", width=25, command=function(){
#                           clear_widget(T1frameCoreLines)
                           LL <- length(CoreLines)
                           N.CKboxGrps <- ceiling(LL/15) #ii runs on the number of columns
                           for(ii in 1:N.CKboxGrps){
                               NN <- (ii-1)*15    #jj runs on the number of column_rows
                               for(jj in 1:15) {
                                   if ((jj+NN) > LL) {break} #exit loop if all FitComp are in RadioBtn
                                   if (tclvalue(CL[[(jj+NN)]]) != "0"){ #coreline jj+NN selected
                                       tclvalue(CL[[(jj+NN)]]) <- "0"   #reset selection
                                       tkconfigure(CoreLineCK[[jj+NN]], foreground="black")
                                   }
                               }
                           }
                        })
    tkgrid(ClearBtn, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

    exitBtn <- tkbutton(SplitGroup2, text="  EXIT  ", width=25, command=function(){
                           tkdestroy(SplitWin)
    	                      return(1)
                        })
    tkgrid(exitBtn, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

#     tkwait.window(SplitWin)
}


