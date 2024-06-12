#XPSCoreLineFitInfo function display coreline fit information and component parameters

#' @title XPSCoreLineFitInfo to describe a Core-Line best fit
#' @description XPSCoreLineFitInfo provides the list of components and 
#    the relative fitting parameters for a selected XPSCoreLine.
#' @examples
#' \dontrun{
#' 	XPSCoreLineFitInfo()
#' }
#' @export
#'


XPSCoreLineFitInfo <- function() {

   updateObj <- function(h,...){
      SelectedFName <- tclvalue(XS)
      FName <- get(SelectedFName,envir=.GlobalEnv) #carico in SampID il relativo XPSSAmple
      SpectList <- XPSSpectList(SelectedFName)
      tkconfigure(InfoObj2, values = SpectList)
      clear_treeview(FitTbl)
      tcl(ShowParam, "delete", "0.0", "end")
      plot(FName)
   }


   SetSpectrum <- function(h,...){
      activeFName <<- tclvalue(XS)
      assign("activeFName", activeFName,envir=.GlobalEnv) #loas activeXPSSample
      coreline <- tclvalue(CL)
      if (coreline != ""){
          coreline <- unlist(strsplit(coreline, "\\."))   #"number." and "CL name" are separated
          activeSpectName <<- coreline[2]
          assign("activeSpectName",coreline[2], envir=.GlobalEnv)
          Indx <<- as.integer(coreline[1])
          activeSpectIndx <<- Indx
          assign("activeSpectIndx", Indx, envir=.GlobalEnv)
          FName <- get(activeFName, envir=.GlobalEnv)
          plot(FName[[Indx]])
      } else {
          plot(FName)
      }
   }


   SetDataFrame <- function() {
      activeFName <- get("activeFName", envir=.GlobalEnv)
      FName <- get(activeFName, envir=.GlobalEnv)
      Indx <- get("activeSpectIndx", envir=.GlobalEnv)
      N_Comp <- length(FName[[Indx]]@Components)
      CompNames <- names(FName[[Indx]]@Components)

      fnName <- sapply(FName[[Indx]]@Components, function(x)  x@funcName) #was VBtop analysis performed on coreline?
#--- Fit Quantification sul FIT ---
      RSF <- FName[[Indx]]@RSF
      if (RSF==0) RSF <- 1
      sumCoreLine <- sum(FName[[Indx]]@Fit$y)/RSF #Fit contribution
      for(jj in 1:N_Comp){    #jj runs on the fit components
          RSF <- FName[[Indx]]@Components[[jj]]@rsf
          if (RSF==0) { #RSF not defined(es. Auger, VB spectra...): cannot normalize
              sumComp[jj] <- sum(FName[[Indx]]@Components[[jj]]@ycoor-FName[[Indx]]@Baseline$y)
          } else {
              sumComp[jj] <- sum(FName[[Indx]]@Components[[jj]]@ycoor-FName[[Indx]]@Baseline$y)/RSF  #contributo della singola componente
          }
      }          
#---Set DataFrame Table
      RSF <- NULL
      for(jj in 1:N_Comp){ #jj runs on the FitComponents
#     CompNames <- rbind(CompNames, paste("C", jj, sep=""))
          if (FName[[Indx]]@Components[[jj]]@funcName=="VBtop"){
              CompNames[jj] <- paste(CompNames[jj], "VBtop", sep=" ")
              FitFnctn <- rbind(FitFnctn, "//")
              Area <- rbind(Area, "//")
              FWHM <- rbind(FWHM, "//")
              RSF <- rbind(RSF, "//")
              BE <- rbind(BE,round(FName[[Indx]]@Components[[jj]]@param["mu","start"], 2)) #BE component jj
              Conc  <- rbind(Conc, "//")
          } else {
              FitFnctn <- rbind(FitFnctn, unlist(FName[[Indx]]@Components[[jj]]@funcName))
              Area <- rbind(Area,round(sumComp[jj], 2))
              FWHM <- rbind(FWHM,round(FName[[Indx]]@Components[[jj]]@param[3,1], 2)) #FWHM component jj
              RSF <- rbind(RSF,unlist(FName[[Indx]]@Components[[jj]]@rsf)) #RSF component jj
              BE <- rbind(BE,round(FName[[Indx]]@Components[[jj]]@param["mu","start"], 2)) #BE component jj
              Conc <- rbind(Conc,round(100*sumComp[jj]/sumCoreLine, 2))  #Concentration component jj
          }
      }
#      CompNames <- encodeString(CompNames, width=10, justify="centre")
#      FitFnctn <- encodeString(FitFnctn, width=10, justify="centre")
#      Area <- encodeString(Area, width=10, justify="right")
#      FWHM <- encodeString(FWHM, width=10, justify="right")
#      RSF <- encodeString(RSF, width=10, justify="right")
#      BE <- encodeString(BE, width=10, justify="right")
#      Conc <- encodeString(Conc, width=10, justify="right")
      fitParam <<- data.frame(CompNames, FitFnctn, Area, FWHM, RSF, BE, Conc, stringsAsFactors=FALSE)
      return(fitParam)
   }


#----- variabili -----

   FNameList <- XPSFNameList()  #list of the XPSSample loaded in the Global Env
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if(length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
      tkmessageBox(message="Please Select an XPS Sample to Proceed", title="WARNING", icon="warning")
      return()
   }
   FName <- get(activeFName, envir = .GlobalEnv)
   Indx <- activeSpectIndx
   if (length(Indx) == 0) { Indx <- 1 }
   activeSpectName <- FName[[Indx]]@Symbol
   N_comp=length(FName[[Indx]]@Components)
   SpectList <- XPSSpectList(activeFName)
   sumCoreLine <- 0
   sumComp <- array(0,dim=N_comp)  #define a dummy vector of zeros
   CompNames <- NULL
   FitFnctn <- NULL
   Area <- NULL
   FWHM <- NULL
   BE <- NULL
   RSF <- NULL
   Conc <- NULL
   fitParam <- NULL
   CLTable <- list()
   SpectTbl <- list()
   FitTbl <- list()

#--- CTRL on Fit
   if (length(FName[[Indx]]@Baseline$x) == 0){  #no information se il Baseline non presente
      txt <- paste("No Baseline Found for", activeFName," - ", activeSpectName, sep=" ")
      tkmessageBox(message=txt, title = "CORE LINE INFO",  icon = "warning")
      return()
   }

#---Set Data.Frame of fit parameters on active XPS Sample

   InfoFwin <- tktoplevel()
   tkwm.title(InfoFwin,"CORE LINE FIT INFO")
   tkwm.geometry(InfoFwin, "+100+50")   #SCREEN POSITION from top-left corner

   InfoGroup1 <- ttkframe(InfoFwin,  borderwidth=2, padding=c(5,5,5,5))
   tkgrid(InfoGroup1, row = 1, column = 1, padx= 5, pady = 5, sticky="w")

# --- Spect-Selection ---
   InfoGroup2 <- ttkframe(InfoGroup1,  borderwidth=2, padding=c(5,5,5,5))
   tkgrid(InfoGroup2, row = 1, column = 1, padx= 5, pady = 5, sticky="w")

   Infoframe1 <- ttklabelframe(InfoGroup2, text = " Select the XPSsample ", borderwidth=2)
   tkgrid(Infoframe1, row = 1, column = 1, padx = 20, pady = 5, sticky="w")
   XS <- tclVar(activeFName)
   InfoObj1 <- ttkcombobox(Infoframe1, width = 25, textvariable = XS, values = FNameList)
   tkgrid(InfoObj1, row = 1, column=1, padx = 5, pady = 5, sticky="w")
   tkbind(InfoObj1, "<<ComboboxSelected>>", function(){
                          tclvalue(CL) <- ""
                          SetSpectrum()
                          SpectList <- XPSSpectList(activeFName)
                          tkconfigure(InfoObj2, value=SpectList)
         })

   Infoframe2 <- ttklabelframe(InfoGroup2, text = " Select the CoreLine ", borderwidth=2)
   tkgrid(Infoframe2, row = 1, column = 2, padx = 20, pady = 5, sticky="w")
   CL <- tclVar(activeSpectName)
   InfoObj2 <- ttkcombobox(Infoframe2, width = 25, textvariable = CL, values = SpectList)
   tkgrid(InfoObj2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(InfoObj2, "<<ComboboxSelected>>", function(){
                          SetSpectrum()
                          fitrng1 <- fitrng2 <- c("  ", "  ")
                          if (length(FName[[Indx]]@RegionToFit$x) > 0) {
                              fitrng1 <- round(range(FName[[Indx]]@RegionToFit$x),2)
                              fitrng2 <- round(range(FName[[Indx]]@RegionToFit$y),2)
                          }
                          EStep <- abs(FName[[Indx]]@.Data[[1]][1]- FName[[Indx]]@.Data[[1]][2])
                          EStep <- round(EStep,3)
                          BLtype <- FName[[Indx]]@Baseline$type[1]
                          txt <- ""
                          txt <- paste(" Source Data: ", FName@Filename, "\n",
                                       "Core-Line:  ",activeSpectName, "\n",
                                       "Fit Parameters: \n",
                                       "Extension of the fit region:  ", fitrng1[1], " - ", fitrng1[2], "\n",
                                       "Energy Step:  ", EStep, "\n",
                                       "Spectral Intensity Range:  ",fitrng2[1],"-", fitrng2[2], "\n",
                                       "Base Line Type:  ", BLtype, collapse="")
                          tcl(SpectTbl, "delete", "0.0", "end") #cancel previous comments
                          tkinsert(SpectTbl, "0.0", txt) #cancel previous comments

                          if (length(FName[[Indx]]@Components) == 0){  #no information if Baseline not defined
                             clear_treeview(FitTbl)
                             tcl(ShowParam, "delete", "0.0", "end")
                             txt <- paste("No Fit found for", activeSpectName, sep=" ")
                             tkmessageBox(message=txt, title = "CORE LINE INFO",  icon = "warning")
                          } else {
                             CLTable <<- SetDataFrame()
                             tcl(ShowParam, "delete", "0.0", "end")
                             updateTable(FitTbl, CLTable) #update Fit Info Tbl
                          }
         })
#--- Spectrum Info
   Infoframe3 <- ttklabelframe(InfoGroup1, text="Spectrum Info", borderwidth=0, padding=c(5,5,5,5))
   tkgrid(Infoframe3, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
   SpectTbl <- tktext(Infoframe3, width=72, height=5)
   tkgrid(SpectTbl, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   fitrng1 <- fitrng2 <- c("  ", "  ")
   if (length(FName[[Indx]]@RegionToFit$x) > 0) {
       fitrng1 <- round(range(FName[[Indx]]@RegionToFit$x),2)
       fitrng2 <- round(range(FName[[Indx]]@RegionToFit$y),2)
   }
   EStep <- abs(FName[[Indx]]@.Data[[1]][1]- FName[[Indx]]@.Data[[1]][2])
   EStep <- round(EStep,3)
   BLtype <- FName[[Indx]]@Baseline$type[1]
   txt <- ""
   txt <- paste(" Source Data: ", FName@Filename, "\n",
                "Core-Line:  ", activeSpectName, "\n",
                "Fit Parameters: \n",
                "Extension of the fit region:  ", fitrng1[1], " - ", fitrng1[2], "\n",
                "Energy Step:  ", EStep, "\n",
                "Spectral Intensity Range:  ",fitrng2[1],"-", fitrng2[2], "\n",
                "Base Line Type:  ", BLtype, collapse="")
   tkinsert(SpectTbl, "0.0", txt) #write report in SpectTbl Win

#--- Fit Component Info
   Infoframe4 <- ttklabelframe(InfoGroup1, text="Component Parameteters", borderwidth=0, padding=c(5,5,5,5))
   tkgrid(Infoframe4, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

   if (length(FName[[Indx]]@Components) > 0){
       CLTable <- SetDataFrame()
       FitTbl <- XPSTable(parent=Infoframe4, items=CLTable, NRows=5,
                          ColNames=c("C Names", "FitFnct", "Area", "FWHM", "RSF", "BE", "Conc."),
                          Width=c(80, 110, 150, 60, 80, 80, 80) )

       tkbind(FitTbl, "<Double-1>", function() {  #bind the table elements to the LEFT mouse but doubleClick
                          if (length(FName[[Indx]]@Components) > 0 ){
                              selItems <- tclvalue(tcl(FitTbl, "selection" ))  # Get the selected items
                              selRow <- lapply(selItems, function(x) {
                                                tclvalue(tcl(FitTbl, "index", x))
                                         })
                              selRow <- as.numeric(unlist(selRow))+1 #treeview row index start from 0
                              selComp <- CLTable[[1]][selRow]
                              CompIndx <- grep(selComp, names(FName[[Indx]]@Components))
                              activeFName <- get("activeFName", envir=.GlobalEnv) #set the activeSpectrum be equal to the last loaded file
                              FName <- get(activeFName, envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                              Indx <- get("activeSpectIndx", envir=.GlobalEnv)
                              FunctName <- FName[[Indx]]@Components[[CompIndx]]@funcName
                              if (FunctName == "VBtop"){
                                 tkmessageBox(message="No Additional Information for VBtop Analysis", title="INFO", icon="info")
                                 return()
                              }
                              tcl(ShowParam, "delete", "0.0", "end") #cancel the SwhoParam table

                              fitParam <<- NULL
                              FP <- FName[[Indx]]@Components[[CompIndx]]@param
                              FP <- round(FP, 3)
                              VarNames <- rownames(FName[[Indx]]@Components[[CompIndx]]@param)

                              options(stringsAsFactors=FALSE) #Without this option the class(fitParam$VarNames)== FACTOR e non CHARACTER
                              fitParam <<- c(fitParam, paste("Fit Component: ", CompIndx, "\n"))
                              fitParam <<- c(fitParam, paste("Parameter", "        Start", "            Min", "            Max", sep=""), "\n")
                              LL <- length(FP[[1]])
                              for (ii in 1:LL){
                                   fitParam <<- c(fitParam, paste(encodeString(VarNames[ii], width=7, justify="right"),
                                                                 encodeString(FP$start[ii], width=15, justify="right"),
                                                                 encodeString(FP$min[ii], width=15, justify="right"),
                                                                 encodeString(FP$max[ii], width=15, justify="right"),sep=""), "\n")
                              }

                              fitParam <<- paste(fitParam, collapse="") #eliminates brakets {} from fitParam
                              tkinsert(ShowParam, "0.0", fitParam) #write report in ShowParam Win
                          }
             })
   } else {
       txt <- paste("No Fit found for", activeSpectName, sep=" ")
       tkmessageBox(message=txt, title = "CORE LINE INFO",  icon = "warning")
       CLTable <- data.frame(list("   ", "   ", "   ", "   ", "   ", "   ", "   ", "   "), stringsAsFactors=FALSE)
       names(CLTable) <- c("C Names", "Fit", "Fnct", "Area", "FWHM", "RSF", "BE", "Conc.")
       #generates an empty table
       FitTbl <- XPSTable(parent=Infoframe4, items=CLTable,
                          ColNames=c("C Names", "Fit", "Fnct", "Area", "FWHM", "RSF", "BE", "Conc."),
                          NRows=5, Width=c(80, 110, 150, 60, 80, 80, 80))
   }
   addScrollbars(parent=Infoframe4, widget=FitTbl, type="y", Row=1, Col=1, Px=0, Py=0)

   CopyBtn1 <- tkbutton(InfoGroup1, text=" Copy Table ", command=function(){
                          SpectData <- data.frame(A="File Name: ", B=FName@Filename, stringsAsFactors=FALSE)
                          SpectData <- rbind(SpectData, c(" ", " "))
                          rownames(SpectData) <- NULL
                          colnames(SpectData) <- NULL
                          for(ii in 1:length(CLTable)){
                              NChr <- max(nchar(CLTable[[ii]]))
                              CLTable[[ii]] <<- encodeString(CLTable[[ii]], width=NChr, quote="", justify=c("right"))
                              if (NChr < 6 || is.na(NChr)) { #NChr <- 5 }
                                  CLTable[[ii]] <<- paste(CLTable[[ii]],"", sep="\t")
                              }
                          }

                          ClipBoard <- file(description="clipboard")
                          open(ClipBoard, "w")
                          write.table(SpectData, file=ClipBoard, append=TRUE, eol="\n", sep="\t", na="    ",
                                      dec=".", quote=FALSE, row.names=FALSE, col.names=TRUE )
                          write.table(CLTable, file=ClipBoard, append=TRUE, eol="\n", sep="   ", na="    ",
                                      dec=".", quote=FALSE, row.names=FALSE, col.names=TRUE )
                          flush(ClipBoard)
                          close(ClipBoard)
                          cat("\n Table copied to clipboard")
             })
   tkgrid(CopyBtn1, row = 5, column = 1, padx = 5, pady = 5, sticky="w")

   Infoframe5 <- ttklabelframe(InfoGroup1, text="Fit Component Info", borderwidth=0, padding=c(5,5,5,5))
   tkgrid(Infoframe5, row = 6, column = 1, padx = 5, pady = 5, sticky="w")
   ShowParam <- tktext(Infoframe5, width=72, height=5)
   tkgrid(ShowParam, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   CopyBtn2 <- tkbutton(InfoGroup1, text=" Copy Table ", command=function(){
                          fitParam <<- strsplit(fitParam, "\n")
                          fitParam <<- data.frame(A=fitParam, stringsAsFactors=FALSE)
                          rownames(fitParam) <<- NULL
                          colnames(fitParam) <<- NULL
                          ClipBoard <- file(description="clipboard")
                          open(ClipBoard, "w")
                          write.table(fitParam, file=ClipBoard, append=TRUE, eol="\n", sep="\t", na="    ",
                                      dec=".", quote=FALSE, row.names=FALSE, col.names=TRUE )
                          flush(ClipBoard)
                          close(ClipBoard)
                          cat("\n Table copied to clipboard")
             })
   tkgrid(CopyBtn2, row = 7, column = 1, padx = 5, pady = 5, sticky="w")

   ExitBtn <- tkbutton(InfoGroup1, text=" EXIT ", width=20, command=function(){
                          tkdestroy(InfoFwin)
         })
   tkgrid(ExitBtn, row = 8, column = 1, padx = 5, pady=5, sticky="w")
}
