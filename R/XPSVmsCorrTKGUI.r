#function to correct VAMAS-data for the analyzer transmission factor

#' @title XPSVmsCorr
#' @description XPSVmsCorr function corrects Vamas_type data for the analyzer transmission
#'   This routine divides raw data intensity for the value of the analyzer transmission
#'   The routine applies the correction to the selected coreline or to all the XPSSample
#'   spectra, baselines and fit components when present.
#'   The transmission function must be provided by the XPS-Instrument manufacturer
#' @examples
#' \dontrun{
#' 	XPSVmsCorr()
#' }
#' @export
#'


XPSVmsCorr <- function(){

   if (is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FName <- get(activeFName, envir=.GlobalEnv)
   FNameList <- XPSFNameList()
   SpectList <- c(XPSSpectList(activeFName), "All")
   NSpect <- NULL
   plot(FName)

   MainWindow <- tktoplevel()
   tkwm.title(MainWindow,"DATA TRANSMISSION CORRECTION")
   tkwm.geometry(MainWindow, "+100+50")   #position respect topleft screen corner

   MainGroup <- ttkframe(MainWindow, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(MainGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
   SelectGroup <- ttkframe(MainGroup, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(SelectGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   XSFrame <- ttklabelframe(SelectGroup, text = "Select the XPS Sample", borderwidth=2)
   tkgrid(XSFrame, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   XS <- tclVar(activeFName)
   SelectXS <- ttkcombobox(XSFrame, width = 15, textvariable = XS, values = FNameList)
   tkgrid(SelectXS, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(SelectXS, "<<ComboboxSelected>>", function(){
                   SelectedFName <- tclvalue(XS)
                   FName <<- get(SelectedFName,envir=.GlobalEnv)
                   activeFName <<- get("activeFName",envir=.GlobalEnv)  #lead the XPSSample identifier
                   SpectList <<- c(XPSSpectList(SelectedFName), "All")
                   NSpect <<- length(FName)
                   tkconfigure(SelectCL, values=SpectList)
                   plot(FName)
                   msg=""
                   for(ii in 1:NSpect) {
                       LL=length(FName[[ii]]@Flags)
                       if (LL==3){  #In old files XPSSample@Flags[4] absent => no correction done
                           FName[[ii]]@Flags <<- c(FName[[ii]]@Flags, FALSE)  #Force flag transm.Funct correction to FALSE
                       }
                       if (FName[[ii]]@Flags[[4]]==FALSE) {   #Flag FALSE when old files loaded or when data are deliberately transformed in raw uncorrected data
                           msg <- paste("CRTL of Raw Spectrum", FName[[ii]]@Symbol, ": => OK for correction", collapse="")
                       }
                       if (FName[[ii]]@Flags[[4]]==TRUE) {
                           msg <- paste("Raw Spectrum", FName[[ii]]@Symbol, ": => WARNING correction ALREADY DONE!", collapse="")
                       }
                   }
                   tkconfigure(StatusBar, text=msg)
         })

   CLFrame <- ttklabelframe(SelectGroup, text = "Select the Spectrum to Correct", borderwidth=2)
   tkgrid(CLFrame, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
   CL <- tclVar()
   SelectCL <- ttkcombobox(CLFrame, width = 15, textvariable = CL, values = SpectList)
   tkgrid(SelectCL, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   tkbind(SelectCL, "<<ComboboxSelected>>", function(){
                   WidgetState(CorrectBtn, "normal")
                   WidgetState(ReplaceBtn, "normal")
         })

   CorrFrame <- ttklabelframe(MainGroup, text = "Correction for the Analyzer Transmission", borderwidth=2)
   tkgrid(CorrFrame, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
   CorrectBtn <- tkbutton(CorrFrame, text=" Correct Data ", width=15, command=function(){
                   msg <- ""
                   Spect <- tclvalue(CL)
                   if (Spect=="All") {
                       for (ii in 1:NSpect) {
                           if (FName[[ii]]@Flags[[4]]==FALSE) {
                               FName[[ii]]@.Data[[2]] <<- FName[[ii]]@.Data[[2]]/FName[[ii]]@.Data[[3]] # correction of RAW DATA for transmission
                               LL=length(FName[[ii]]@RegionToFit$x)
                               if (LL>0) {
                                   idx1=findXIndex(FName[[ii]]@.Data[[1]], FName[[ii]]@RegionToFit$x[1])
                                   idx2=findXIndex(FName[[ii]]@.Data[[1]], FName[[ii]]@RegionToFit$x[LL])
                                   TrasmFact <- FName[[ii]]@.Data[[3]][idx1:idx2]                    #these are the transmission function data
                                   FName[[ii]]@RegionToFit$y <<- FName[[ii]]@RegionToFit$y/TrasmFact #now correction for the transmission function
                                   FName[[ii]]@Baseline$y <<- FName[[ii]]@Baseline$y/TrasmFact
                                   FName[[ii]]@Fit$y <<- FName[[ii]]@Fit$y/TrasmFact
                                   NComp <- length(FName[[ii]]@Components)
                                   if (NComp>0) {
                                       for (jj in 1:NComp) {
                                            FName[[ii]]@Components[[jj]]@ycoor<<- FName[[ii]]@Components[[jj]]@ycoor/TrasmFact #correction of components anf fit data
                                            tmp <- as.matrix(FName[[ii]]@Components[[jj]]@param)  #correction fit parameneters
                                            tmp[1,] <- tmp[1,]/mean(TrasmFact)                    #use the average value of the transf funct data
                                            FName[[ii]]@Components[[jj]]@param<<- as.data.frame(tmp)
                                            FName[[ii]]@Flags[[4]] <<- TRUE                       #now flag correction set to TRUE
                                       }
                                   }
                               }
                               msg <- paste("Raw Spectrum", FName[[ii]]@Symbol, "Corrected!", collapse="")
                               tkconfigure(StatusBar, text=msg)
                           } else {
                               msg <- paste("Skip", FName[[ii]]@Symbol, "Correction!", collapse="")
                               tkconfigure(StatusBar, text=msg)
                           }
                       }
                   } else {
                       SourceCoreline <- unlist(strsplit(Spect, "\\."))   #skip the number at beginning of corelinename
                       idx <- as.integer(SourceCoreline[1])
                       if (FName[[idx]]@Flags[[4]]==FALSE) {
                           FName[[idx]]@.Data[[2]] <<- FName[[idx]]@.Data[[2]]/FName[[idx]]@.Data[[3]]
                           LL=length(FName[[idx]]@RegionToFit$x)
                           if (LL > 0) {
                               idx1 <- findXIndex(FName[[idx]]@.Data[[1]], FName[[idx]]@RegionToFit$x[1])
                               idx2 <- findXIndex(FName[[idx]]@.Data[[1]], FName[[idx]]@RegionToFit$x[LL])
                               TrasmFact <- FName[[idx]]@.Data[[3]]
                               TrasmFact <- TrasmFact[idx1:idx2]
                               FName[[idx]]@RegionToFit$y <<- FName[[idx]]@RegionToFit$y/TrasmFact
                               FName[[idx]]@Baseline$y <<- FName[[idx]]@Baseline$y/TrasmFact
                               FName[[idx]]@Fit$y <<- FName[[idx]]@Fit$y/TrasmFact
                               NComp <- length(FName[[idx]]@Components)
                               if (NComp > 0) {
                                   for (jj in 1:NComp) {
                                        FName[[idx]]@Components[[jj]]@ycoor <<- FName[[idx]]@Components[[jj]]@ycoor/TrasmFact
                                        tmp <- as.matrix(FName[[idx]]@Components[[jj]]@param)
                                        tmp[1,] <- tmp[1,]/mean(TrasmFact)
                                        FName[[idx]]@Components[[jj]]@param <<- as.data.frame(tmp)
                                   }
                               }
                           }
                           FName[[idx]]@Flags[[4]] <<- TRUE
                           msg <- paste("Raw Spectrum", FName[[idx]]@Symbol, "Corrected!", collapse="")
                           tkconfigure(StatusBar, text=msg)
                       } else {
                           msg <- paste("Skipped", FName[[idx]]@Symbol, "Correction!", collapse="")
                           tkconfigure(StatusBar, text=msg)
                       }
                   }
         })
   tkgrid(CorrectBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   ReplaceFrame <- ttklabelframe(MainGroup, text = "Raw Data Generation", borderwidth=2)
   tkgrid(ReplaceFrame, row = 3, column = 1, padx = 5, pady = 5, sticky="w")
   ReplaceBtn <- tkbutton(ReplaceFrame, text=" Replace Raw Data ", width=15, command=function(){
                   msg <- ""
                   Spect <- tclvalue(CL)
                   if (Spect == "All") {
                       for (ii in 1:NSpect) {
                            if (FName[[ii]]@Flags[[4]]==TRUE) {
                                FName[[ii]]@.Data[[2]] <<- FName[[ii]]@.Data[[2]]*FName[[ii]]@.Data[[3]] #elimination correction for tranf.function: back to raw data
                                LL=length(FName[[ii]]@RegionToFit$x)
                                if (LL > 0) {
                                    idx1=findXIndex(FName[[ii]]@.Data[[1]], FName[[ii]]@RegionToFit$x[1])
                                    idx2=findXIndex(FName[[ii]]@.Data[[1]], FName[[ii]]@RegionToFit$x[LL])
                                    TrasmFact <- FName[[ii]]@.Data[[3]][idx1:idx2]
                                    FName[[ii]]@RegionToFit$y <<- FName[[ii]]@RegionToFit$y*TrasmFact
                                    FName[[ii]]@Baseline$y <<- FName[[ii]]@Baseline$y*TrasmFact
                                    FName[[ii]]@Fit$y <<- FName[[ii]]@Fit$y*TrasmFact
                                    NComp <- length(FName[[ii]]@Components)
                                    if (NComp > 0) {
                                        for (jj in 1:NComp) {
                                             FName[[ii]]@Components[[jj]]@ycoor <<- FName[[ii]]@Components[[jj]]@ycoor*TrasmFact
                                             tmp <- as.matrix(FName[[ii]]@Components[[jj]]@param)
                                             tmp[1,] <- tmp[1,]*mean(TrasmFact)
                                             FName[[ii]]@Components[[jj]]@param <<- as.data.frame(tmp)
                                             FName[[ii]]@Flags[[4]] <<- FALSE
                                        }
                                    }
                                }
                                msg <- paste("Spectrum", FName[[ii]]@Symbol, "Original Raw Data Replaced!", collapse="")
                                tkconfigure(StatusBar, text=msg)
                            } else {
                                msg <- paste("Skipped", FName[[ii]]@Symbol, "Correction!", collapse="")
                                tkconfigure(StatusBar, text=msg)
                            }
                       }
                   } else {
                       SourceCoreline <- unlist(strsplit(Spect, "\\."))
                       idx <- as.integer(SourceCoreline[1])
                       if (FName[[idx]]@Flags[[4]]==TRUE) {
                           FName[[idx]]@.Data[[2]] <<- FName[[idx]]@.Data[[2]]*FName[[idx]]@.Data[[3]]
                           LL=length(FName[[idx]]@RegionToFit$x)
                           if (LL > 0) {
                               idx1 <- findXIndex(FName[[idx]]@.Data[[1]], FName[[idx]]@RegionToFit$x[1])
                               idx2 <- findXIndex(FName[[idx]]@.Data[[1]], FName[[idx]]@RegionToFit$x[LL])
                               TrasmFact <- FName[[idx]]@.Data[[3]][idx1:idx2]
                               FName[[idx]]@RegionToFit$y <<- FName[[idx]]@RegionToFit$y*TrasmFact
                               FName[[idx]]@Baseline$y <<- FName[[idx]]@Baseline$y*TrasmFact
                               FName[[idx]]@Fit$y <<- FName[[idx]]@Fit$y*TrasmFact
                               NComp <- length(FName[[idx]]@Components)
                               if (NComp > 0) {
                                   for (jj in 1:NComp) {
                                        FName[[idx]]@Components[[jj]]@ycoor <<- FName[[idx]]@Components[[jj]]@ycoor*TrasmFact
                                        tmp <- as.matrix(FName[[idx]]@Components[[jj]]@param)
                                        tmp[1,] <- tmp[1,]*mean(TrasmFact)
                                        FName[[idx]]@Components[[jj]]@param <<- as.data.frame(tmp)
                                   }
                               }
                           }
                           FName[[idx]]@Flags[[4]] <<- TRUE
                           msg <- paste("Spectrum", FName[[idx]]@Symbol, ": original Raw Data Replaced!", collapse="")
                           tkconfigure(StatusBar, text=msg)
                       } else {
                           msg <- paste("Skipped", FName[[idx]]@Symbol, "Replacement!", collapse="")
                           tkconfigure(StatusBar, text=msg)
                       }
                   }
         })
   tkgrid(ReplaceBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   BtnFrame <- ttklabelframe(MainGroup, text = "Options: ", borderwidth=2)
   tkgrid(BtnFrame, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

   SaveBtn <- tkbutton(BtnFrame, text=" SAVE ", width=20, command=function(){
                   activeFName <- FName@Filename
                   activeSpectIndx <- 1
                   activeSpectName <-   FName[[1]]@Symbol
                   assign(activeFName, FName, envir=.GlobalEnv)
                   assign("activeFName", activeFName, envir=.GlobalEnv)
                   assign("activeSpectName", activeSpectName,envir=.GlobalEnv)
                   assign("activeSpectIndx", activeSpectIndx,envir=.GlobalEnv)
                   XPSSaveRetrieveBkp("save")
                   plot(FName)
         })
   tkgrid(SaveBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   SaveExitBtn <- tkbutton(BtnFrame, text=" SAVE & EXIT ", width=20, command=function(){
                   activeFName <- FName@Filename
                   activeSpectIndx <- 1
                   activeSpectName <-   FName[[1]]@Symbol
                   assign(activeFName, FName, envir=.GlobalEnv)
                   assign("activeFName", activeFName, envir=.GlobalEnv)
                   assign("activeSpectName", activeSpectName,envir=.GlobalEnv)
                   assign("activeSpectIndx", activeSpectIndx,envir=.GlobalEnv)
                   XPSSaveRetrieveBkp("save")
                   plot(FName)
                   tkdestroy(MainWindow)
         })
   tkgrid(SaveExitBtn, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   tkSep <- ttkseparator(MainGroup, orient="horizontal")
   tkgrid(tkSep, row = 5, column = 1, padx = 5, pady = 10, sticky="we")
   StatusBar <- ttklabel(MainGroup, text="Status : ", relief="sunken", foreground="blue3")
   tkgrid(StatusBar, row = 6, column = 1, padx = 5, pady = 5, sticky="we")


   WidgetState(CorrectBtn, "disabled")
   WidgetState(ReplaceBtn, "disabled")
}
