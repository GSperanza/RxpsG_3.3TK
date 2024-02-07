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
      clear_treeview(InfoTable)
      tcl(ShowParam, "delete", "0.0", "end")
      plot(FName)
   }


   SetSpectrum <- function(h,...){
      activeFName <- tclvalue(XS)
      assign("activeFName", activeFName,envir=.GlobalEnv) #loas activeXPSSample
      coreline <- tclvalue(CL)
      if (coreline != ""){
          coreline <- unlist(strsplit(coreline, "\\."))   #"number." and "CL name" are separated
          assign("activeSpectName",coreline[2], envir=.GlobalEnv)
          Indx <<- as.integer(coreline[1])
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
      fitParam <- data.frame(CompNames, FitFnctn, Area, FWHM, RSF, BE, Conc, stringsAsFactors=FALSE)
      return(fitParam)
   }


#----- variabili -----

   FNameList <- XPSFNameList()  #list of the XPSSample loaded in the Global Env
   if(is.na(activeFName) || is.null(activeFName) || length(activeFName)==0){
      tkmessageBox(message="Please Select an XPS Sample to Proceed", title="WARNING", icon="warning")
      return()
   }
   FName <- get(activeFName, envir = .GlobalEnv)
   Indx <- activeSpectIndx
   if(is.na(Indx) || is.null(Indx) || length(Indx)==0){
      tkmessageBox(message="Please Select a Core Line to Proceed", title="WARNING", icon="warning")
      return()
   }
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
   InfoTable <- list()

#--- CTRL on Fit
   if (length(FName[[Indx]]@Components) == 0){  #no information se il Baseline non presente
      txt <- paste("No Fit found for", activeFName," - ", activeSpectName, sep=" ")
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
                          tkconfigure(SpectInfo1, text="") #cancel previous comments
                          tkconfigure(SpectInfo3, text="")
                          tkconfigure(SpectInfo4, text="")
                          tkconfigure(SpectInfo5, text="")

                          txt=paste(FName@Filename, "     Core-Line:  ",activeSpectName, sep="")
                          SpectInfo1 <- ttklabel(InfoGroup1, text=txt)
                          tkgrid(SpectInfo1, row = 4, column = 1, padx = 20, pady=5, sticky="w")

                          fitrng <- round(range(FName[[Indx]]@RegionToFit$x),2)
                          txt <- paste("Extension of the fit region:  ", fitrng[1], "-", fitrng[2], sep="")
                          SpectInfo3 <- ttklabel(InfoGroup1, text=txt)
                          tkgrid(SpectInfo3, row = 6, column = 1, padx = 20, pady=5, sticky="w")

                          fitrng <- round(range(FName[[Indx]]@RegionToFit$y),2)
                          txt <- paste("Intensity of the fitted spectrum:  ",fitrng[1],"-", fitrng[2])
                          SpectInfo4 <- ttklabel(InfoGroup1, text=txt)
                          tkgrid(SpectInfo4, row = 7, column = 1, padx = 20, pady=5, sticky="w")

                          BLtype <- FName[[Indx]]@Baseline$type
                          txt <- paste("Base Line Type:  ",BLtype)
                          SpectInfo5 <- ttklabel(InfoGroup1, text=txt)
                          tkgrid(SpectInfo5, row = 8, column = 1, padx = 20, pady=5, sticky="w")

                          if (length(FName[[Indx]]@Components) == 0){  #no information if Baseline not defined
                             clear_treeview(InfoTable)
                             tcl(ShowParam, "delete", "0.0", "end")

                             txt <- paste("No Fit found for", activeSpectName, sep=" ")
                             tkmessageBox(message=txt, title = "CORE LINE INFO",  icon = "warning")
                          } else {
                             CLTable <- SetDataFrame()
                             tcl(ShowParam, "delete", "0.0", "end")
                             updateTable(InfoTable, CLTable)
                          }
         })

   SpectInfo0 <- ttklabel(InfoGroup1, text="Source Data: ")
   tkgrid(SpectInfo0, row = 3, column = 1, padx = 20, pady=5, sticky="w")

   txt=paste(FName@Filename, "     Core-Line:  ",activeSpectName, sep="")
   SpectInfo1 <- ttklabel(InfoGroup1, text=txt)
   tkgrid(SpectInfo1, row = 4, column = 1, padx = 20, pady=5, sticky="w")

   SpectInfo2 <- ttklabel(InfoGroup1, text="Fit Parameters: ")
   tkgrid(SpectInfo2, row = 5, column = 1, padx = 20, pady=5, sticky="w")

   fitrng <- round(range(FName[[Indx]]@RegionToFit$x),2)
   EStep <- abs(FName[[Indx]]@.Data[[1]][1]- FName[[Indx]]@.Data[[1]][2])
   EStep <- round(EStep,3)
   txt <- paste("Extension of the fit region:  ", fitrng[1], "-", fitrng[2],
                "     Energy Step:  ", EStep, sep="")
   SpectInfo3 <- ttklabel(InfoGroup1, text=txt)
   tkgrid(SpectInfo3, row = 6, column = 1, padx = 20, pady=5, sticky="w")

   fitrng <- round(range(FName[[Indx]]@RegionToFit$y),2)
   txt <- paste("Intensity of the fitted spectrum:  ",fitrng[1],"-", fitrng[2])
   SpectInfo4 <- ttklabel(InfoGroup1, text=txt)
   tkgrid(SpectInfo4, row = 7, column = 1, padx = 20, pady=5, sticky="w")

   BLtype <- FName[[Indx]]@Baseline$type[1]
   txt <- paste("Base Line Type:  ",BLtype)
   SpectInfo5 <- ttklabel(InfoGroup1, text=txt)
   tkgrid(SpectInfo5, row = 8, column = 1, padx = 20, pady=5, sticky="w")

   InfoGroup3 <- ttklabelframe(InfoGroup1, text="Component Parameteters", borderwidth=0, padding=c(0,0,0,0))
   tkgrid(InfoGroup3, row = 9, column = 1, padx = 5, pady = 5, sticky="w")

   if (length(FName[[Indx]]@Components) > 0){
       CLTable <- SetDataFrame()
       InfoTable <- XPSTable(parent=InfoGroup3, items=CLTable, NRows=5,
                          ColNames=c("C Names", "FitFnct", "Area", "FWHM", "RSF", "BE", "Conc."),
                          Width=c(80, 110, 150, 60, 80, 80, 80) )

       tkbind(InfoTable, "<Double-1>", function() {  #bind the table elements to the LEFT mouse but doubleClick
                          selItems <- tclvalue(tcl(InfoTable, "selection" ))  # Get the selected items
                          selRow <- lapply(selItems, function(x) {
                                            tclvalue(tcl(InfoTable, "index", x))
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

                          fitParam <- NULL
                          FP <- FName[[Indx]]@Components[[CompIndx]]@param
                          FP <- round(FP, 3)
                          VarNames <- rownames(FName[[Indx]]@Components[[CompIndx]]@param)

                          options(stringsAsFactors=FALSE) #Without this option the class(fitParam$VarNames)== FACTOR e non CHARACTER
                          fitParam <- c(fitParam, paste("      Parameter", "          Start", "            Min", "            Max", sep=""), "\n")
                          LL <- length(FP[[1]])
                          for (ii in 1:LL){
                               fitParam <- c(fitParam, paste(encodeString(VarNames[ii], width=15, justify="right"),
                                                             encodeString(FP$start[ii], width=15, justify="right"),
                                                             encodeString(FP$min[ii], width=15, justify="right"),
                                                             encodeString(FP$max[ii], width=15, justify="right"),sep=""), "\n")
                          }

                          fitParam <- paste(fitParam, collapse="") #eliminates brakets {} from fitParam
                          tkinsert(ShowParam, "0.0", fitParam) #write report in ShowParam Win
             })

   } else {
       txt <- paste("No Fit found for", activeSpectName, sep=" ")
       tkmessageBox(message=txt, title = "CORE LINE INFO",  icon = "warning")
       CLTable <<- as.data.frame(list("   ", "    ", "    ", "    ", "    ", "    ", "    ", "    "))
       names(CLTable) <- c("C Names", "Fit", "Fnct", "Area", "FWHM", "RSF", "BE", "Conc.")
       #generates an empty table
       InfoTable <<- XPSTable(parent=InfoGroup3, items=CLTable,
                          ColNames=c("C Names", "Fit", "Fnct", "Area", "FWHM", "RSF", "BE", "Conc."),
                          NRows=5, Width=c(80, 110, 150, 60, 80, 80, 80) )
   }
   addScrollbars(parent=InfoGroup3, widget=InfoTable, type="y", Row=1, Col=1, Px=0, Py=0)


   ShowParam <- tktext(InfoGroup1, width=72, height=5)
   tkgrid(ShowParam, row = 10, column = 1, padx = 5, pady = 5, sticky="w")

   ExitBtn <- tkbutton(InfoGroup1, text=" EXIT ", width=20, command=function(){
                          tkdestroy(InfoFwin)
         })
   tkgrid(ExitBtn, row = 11, column = 1, padx = 50, pady=5, sticky="w")


}
