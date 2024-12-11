#XPSAnalReport() function to make a report containing the list of fitting components
#and relative abundance for the selected coreline

#' @title XPSAnalReport provides information about a Core Line fit
#' @description XPSAnalReport() makes the computation of the integral intensity of
#'  each of the fitting components of a given coreline. The sum of the component 
#'  integral intensities =100% (it is the best fit integral intensity).
#' @examples
#' \dontrun{
#'	  XPSReport()
#' }
#' @export
#'


XPSAnalReport <- function(){


   RetrivePE <- function(CoreLine){  #Retrieve the PE value from the CL coreline information slot
      info <- CoreLine@Info[1]   #retrieve info containing PE value
      xxx <- strsplit(info, "Pass energy")  #extract PE value
      PE <- strsplit(xxx[[1]][2], "Iris") #PE value
      PE <- as.integer(PE[[1]][1])
      return(PE)
   }

   FindFittedCL <- function(){
      Fidx <- which(sapply(XPSSample, function(x) hasBaseline(x))==TRUE)  #extract all CoreLines with BaseLine
      if (length(Fidx) == 0){
         tkmessageBox(message="No fitted Core-Lines in the selected XPS Sample", title="WARNING", icon="warning")
         return()
      }
      SpecialComp <- c("VBtop", "VBFermi", "Derivative", "FitProfile",
                   "HillSigmoid", "HillSigmoid.KE", "Sigmoid",
                   "PowerDecay", "ExpDecay", "Linear")
#now eliminate from FittedCL list the SpecialComponents
      NN <- length(Fidx)
      CL <- Fidx  #CL temporary vector with indexes of FittedCL
      for(ii in NN:1){
          LL <- length(XPSSample[[CL[ii]]]@Components)
          if (LL > 0){  #Fit components must be present not only a baseline
              for(jj in 1:LL){
                  funcName <- XPSSample[[CL[ii]]]@Components[[jj]]@funcName
                  if (funcName %in% SpecialComp) {
                      Fidx <- Fidx[-ii]
                      break
                  }
              }
          }
      }
      return(Fidx)
   }

   ReportSelection <- function(){
      tkconfigure(ReprtWin, font=MyFont2)
      TabTxt <<- ""
      TabTxt <<- c(TabTxt, paste ("===> File Name:", XPSSample@Filename), "\n\n")  #Filename
#----Call STANDARD Report
      RprtOpt <- tclvalue(StdRprt) #Make STANDARD Report?
      if (RprtOpt == 1){
          if(sum(SelectedCL2) == 0){ #no selected Unfitted CoreLines
             tkmessageBox(message="Standard Report needs selection of 'Other Core-Lines' ", title="ERROR", icon="error")
             tclvalue(StdRprt) <- FALSE
             return()
          }
          for(ii in length(NonFittedCL):1){
              if(SelectedCL2[ii] == 0){
                 SelectedCL2 <<- SelectedCL2[-ii]
              }
          }
          TabTxt <<- c(TabTxt, paste("STANDARD REPORT \n\n"))
          MakeStdrdReport()
          TabTxt <<- c(TabTxt, "\n")
      }
#----Call FIT Report
      RprtOpt <- tclvalue(FRprt)   #Make FIT Report?
      if (RprtOpt == 1){
          if(sum(SelectedCL1) == 0){ #NO selected Fitted CoreLines
             tkmessageBox(message="Fit Report requires selection of FITTED Core-Lines", title="ERROR", icon="error")
             tclvalue(FRprt) <- FALSE
             return()
          }
          for(ii in length(FittedCL):1){
              if(SelectedCL1[ii] == 0){
                 SelectedCL1 <<- SelectedCL1[-ii]
              }
          }
          TabTxt <<- c(TabTxt, paste("FIT REPORT \n\n"))
          for(ii in SelectedCL1){  #Make fit Report for the selected CoreLines
              MakeFitReport(XPSSample[[ii]])
          }
          TabTxt <<- c(TabTxt, "\n")
      }
#----call QUANTIFICATION Report
      RprtOpt <- tclvalue(QRprt) #Make QUANTIFICATION Report?
      if (RprtOpt == 1){
          if(sum(SelectedCL1) == 0){ #NO selected CoreLines to Quantify
             tkmessageBox(message="Quantification Report requires selection of FITTED Core-Lines", title="ERROR", icon="error")
             tclvalue(QRprt) <- FALSE
             return()
          }
          for(ii in length(FittedCL):1){
              if(SelectedCL1[ii] == 0){
                 SelectedCL1 <<- SelectedCL1[-ii] #eliminate the non-selected FittedCL
              }
          }
          TabTxt <<- c(TabTxt, "QUANTIFICATION REPORT \n\n")
          MakeQuantReport()
          TabTxt <<- c(TabTxt, "\n")
      }

      if(tclvalue(QRprt) == FALSE && tclvalue(FRprt) == FALSE && tclvalue(StdRprt) == FALSE){
         tkmessageBox(message="Please Select the Reporting Model", title="ERROR", icon="error")
         return()
      }
      if(sum(SelectedCL1) == 0 && sum(SelectedCL2) == 0){   #SelectedCL1 or SelectedCL2 can contain 0 corresponding to unselected elements
         tkmessageBox(message="Please Select the Core-Lines to Report", title="ERROR", icon="error")
         return()
      }

#-------
      TabTxt <<- paste(TabTxt, collapse="")
      tkinsert(ReprtWin, "0.0", TabTxt) #write report in ReprtWin
      XScroll <<- addScrollbars(RGroup1, ReprtWin, type="x", Row = 16, Col=1, Px=0, Py=0)
      YScroll <<- addScrollbars(RGroup1, ReprtWin, type="y", Row = 16, Col=1, Px=0, Py=0)

#-------
   }


##====MAKE Reports
##FIT Report
   MakeFitReport <- function(CoreLine){
      CompNames <- names(CoreLine@Components)
      sumCoreLine <- 0
      N_comp <- length(CoreLine@Components) #this is the number of fit components
      sumComp <- array(0,dim=N_comp)  #array if zerros
      RSF <- CoreLine@RSF
      E_stp <- round(abs(CoreLine@.Data[[1]][2]-CoreLine@.Data[[1]][1]), 2) #energy step

      if (length(CoreLine@Baseline)==0) {
#--- No Baseline No Fit ---
         TabTxt <<- c(TabTxt, paste("*** ", CoreLine@Symbol, ": no fit present", sep=" "), "\n")
         PE <- RetrivePE(CoreLine)
         TabTxt <<- c(TabTxt, paste("Pass Energy: ", PE, "   Energy Step: ",E_stp, sep=""), "\n\n")
      } else if (length(CoreLine@Baseline) > 0 && N_comp==0) {
#--- Baseline only ---
         TabTxt <<- c(TabTxt, paste("*** ", CoreLine@Symbol, "Coreline only Baseline ", CoreLine@Baseline$type[1], " present: ", sep=" "), "\n")
         PE <- RetrivePE(CoreLine)
         TabTxt <<- c(TabTxt, paste("Pass Energy: ", PE, "   Energy Step: ",E_stp, sep=""), "\n")
         if (RSF==0){
            sumCoreLine <- sum(CoreLine@RegionToFit$y-CoreLine@Baseline$y*E_stp) #Spectral area of the core-line
         } else {
            sumCoreLine <- sum(CoreLine@RegionToFit$y-CoreLine@Baseline$y)*E_stp/RSF #normalized spectral area of the core-line
         }

         area <- sprintf("%1.2f",sumCoreLine)  #converto
         txt <- paste("  ", CoreLine@Symbol,"peak area: ", area, sep=" ")
         CellLength <- nchar(txt)
         TabTxt <<- c(TabTxt,printCell("label",txt,cellB,CellLength,"left"), "\n")
         TabTxt <<- c(TabTxt, "\n")

      } else if (N_comp > 0) {
#--- Baseline + Fit ---
         #VBtop analysis
         fnName <- sapply(CoreLine@Components, function(x)  x@funcName) #was VBtop analysis performed on coreline?
         if ("VBtop" %in% fnName){
             TabTxt <<- c(TabTxt, paste("***  ", CoreLine@Symbol, " Core-Line Fit Info: ", sep=""), "\n")
             PE <- RetrivePE(CoreLine)
             TabTxt <<- c(TabTxt, paste("Pass Energy: ", PE, "   Energy Step: ",E_stp, sep=""), "\n")
             TabTxt <<- c(TabTxt, " \n")
             TabTxt <<- c(TabTxt, paste("     BaseLine applied: ",CoreLine@Baseline$type[1], sep=""), "\n")
             TabTxt <<- c(TabTxt, " \n")
             CellLength <- c(10, 12, 8)
             cellB <- " "
             txt <- c("Components", "Fit Funct.", "Position")
             TabTxt <<- c(TabTxt,printCell("tableRow", txt, cellB, CellLength, "center"), "\n")
             for(jj in 1:N_comp){ #jj runs on CoreLines, jj runs on Fit components
                 Function <- sprintf("%s",CoreLine@Components[[jj]]@funcName) #Fit Funct name
                 BE <- "//"
                 if (Function=="VBtop"){
                     BE <- sprintf("%1.2f",CoreLine@Components[[jj]]@param[1,1]) #Component BE
                 }
                 txt <- c(CompNames[jj], Function, BE) #make string to print
                 TabTxt <<- c(TabTxt,printCell("tableRow", txt, cellB, CellLength, "center"), "\n") #print in mode TABLEROW
             }
         } else {
             sumCoreLine <- 0
             TotArea <- 0
             for(jj in 1:N_comp){    #jj runs on the core-line fit components
                RSF <- CoreLine@Components[[jj]]@rsf
                if (RSF==0) { #RSF not defined
                   sumComp[jj] <- sum(CoreLine@Components[[jj]]@ycoor-CoreLine@Baseline$y)*E_stp
                } else {
                   sumComp[jj] <- sum(CoreLine@Components[[jj]]@ycoor-CoreLine@Baseline$y)*E_stp/RSF  #controbution of the single FITcomponent
                }
                sumCoreLine <- sumCoreLine + sumComp[jj]
             }
             TotArea <- TotArea + sum(CoreLine@Fit$y)/RSF #Contributo del Fit

# width of colum<ns("Components", "FitFunct.", "Area(cps)", "Intensity", "FWHM", "BE(eV)", "TOT.(%)")
             TabTxt <<- c(TabTxt, paste("***  ", CoreLine@Symbol, " Core-Line Fit Info: ", sep=""), "\n")
             PE <- RetrivePE(CoreLine)
             TabTxt <<- c(TabTxt, paste("Pass Energy: ", PE, "   Energy Step: ",E_stp, sep=""), "\n")
             TabTxt <<- c(TabTxt, " \n")
             TabTxt <<- c(TabTxt, paste("     BaseLine applied: ",CoreLine@Baseline$type[1], sep=""), "\n")
             TabTxt <<- c(TabTxt, " \n")
             CellLength <- c(10, 10, 15, 10, 6, 8, 12)
             cellB <- " "

#Columns names
             txt <- c("Components", "Fit Funct.", "Area (cps)", "Intensity", "FWHM", "Position", "Weight(%)")
             TabTxt <<- c(TabTxt,printCell("tableRow", txt, cellB, CellLength, "center"), "\n")


#Rows describing Fit Components
             for(jj in 1:N_comp){ #jj runs on CoreLines, jj runs on Fit components
                Function <- sprintf("%s",CoreLine@Components[[jj]]@funcName) #Fit Funct name
                if (length(grep("DoniachSunjic", Function))){
                    Function <- gsub("DoniachSunjic", "D.S.",Function) #extract the additional part after DoniachSunjic
                }
                Area <- sprintf("%1.2f",sumComp[jj]) #area componente jj linea di core ii
                Intensity <- sprintf("%1.2f",CoreLine@Components[[jj]]@param[1,1]) #Component Intensity componente
                FWHM <- sprintf("%1.2f",CoreLine@Components[[jj]]@param[3,1]) #Component FWHM
                BE <- sprintf("%1.2f",CoreLine@Components[[jj]]@param[2,1]) #Component BE
                Conc <- sprintf("%1.2f",100*sumComp[jj]/sumCoreLine)  #Core-Line Relative Component Concentrations: Sum(components%)=100
                txt <- c(CompNames[jj], Function, Area, Intensity, FWHM, BE, Conc) #make string to print
                TabTxt <<- c(TabTxt,printCell("tableRow", txt, cellB, CellLength, "center"), "\n") #print in mode TABLEROW
             }
         }
         TabTxt <<- c(TabTxt, "\n")
      }
   }

##STANDARD Report
   MakeStdrdReport <- function() {
      for(ii in SelectedCL2){  #Make fit Report for the selected CoreLines
          LL <- length(XPSSample[[ii]]@.Data[[1]])
          Bnd1 <- XPSSample[[ii]]@.Data[[1]][1]
          Bnd2 <- XPSSample[[ii]]@.Data[[1]][LL]
          TabTxt <<- c(TabTxt, paste("*** ", XPSSample[[ii]]@Symbol, ": no fit present", sep=" "), "\n")
          PE <- RetrivePE(XPSSample[[ii]])
          E_stp <- round(abs(XPSSample[[ii]]@.Data[[1]][2] - XPSSample[[ii]]@.Data[[1]][1]), 2)
          TabTxt <<- c(TabTxt, paste("Core Line  : ",slot(XPSSample[[ii]],"Symbol"), sep=""), "\n")
          TabTxt <<- c(TabTxt, paste("E-range    : ",round(Bnd1, 2)," - ", round(Bnd2, 2), sep=""), "\n")
          TabTxt <<- c(TabTxt, paste("N. data    : ",length(XPSSample[[ii]]@.Data[[1]])), "\n")
          TabTxt <<- c(TabTxt, paste("Pass Energy: ", PE, sep=""), "\n")
          TabTxt <<- c(TabTxt, paste("Energy Step: ",E_stp, sep=""), "\n")
          TabTxt <<- c(TabTxt, paste("Baseline   : ",ifelse(hasBaseline(XPSSample[[ii]]),XPSSample[[ii]]@Baseline$type[1], "NONE"), sep=""), "\n")
          TabTxt <<- c(TabTxt, "Info: \n")
          TabTxt <<- c(TabTxt, paste(XPSSample[[ii]]@Info, "\n", sep=""))

### this block NOT NECESSARY all information in ...@Info
#          if(XPSSample[[ii]]@Symbol == "VBf"){
#             idx <- sapply(XPSSample[[ii]]@Components, function(x) which(x@funcName == "VBtop"))
#             idx <- which(idx > 0) #index of the component having 'VBtop' name
#             TabTxt <<- c(TabTxt, paste("VBFermi:", XPSSample[[ii]]@Components[[idx]]@param["mu", "start"], sep=""), "\n")
#          }
#          if("d.D." %in% XPSSample[[ii]]@Symbol || "\U0394." %in% XPSSample[[ii]]@Symbol){ #MaxMinDistance
#             idx <- sapply(XPSSample[[ii]]@Components, function(x) which(x@funcName == "Derivative"))
#             idx <- which(idx > 0) #index of the component having 'VBtop' name
#             MaxMinD <- abs(XPSSample[[ii]]@Components[[idx]]@param["mu", "min"] -
#                            XPSSample[[ii]]@Components[[idx]]@param["mu", "max"])
#             TabTxt <<- c(TabTxt, paste("MaxMin Dist.:", MaxMinD, sep=""), "\n")
#          }
          TabTxt <<- c(TabTxt, "\n")
          if (hasComponents(XPSSample[[ii]])){
              fnName <- sapply(XPSSample[[ii]]@Components, function(x)  x@funcName) #was VBtop analysis performed on coreline?
              if ("Derivative" %in% fnName){
                 TabTxt <<- c(TabTxt, paste("***  ", XPSSample[[ii]]@Symbol, " Core-Line Deriavtive Info: ", sep=""), "\n")
                 PE <- RetrivePE(XPSSample[[ii]])
                 TabTxt <<- c(TabTxt, " \n")
                 TabTxt <<- c(TabTxt, paste("     BaseLine applied: ",XPSSample[[ii]]@Baseline$type[1], sep=""), "\n")
                 DiffDeg <- unlist(strsplit(XPSSample[[ii]]@Symbol, ".", fixed=TRUE))
                 jj <- which(DiffDeg == "D")
                 DiffDeg <- as.integer(DiffDeg[(jj+1)])
                 TabTxt <<- c(TabTxt, paste("     Differentiation Degree: ",DiffDeg, sep=""), "\n")
                 jj <- which(fnName %in% "Derivative")  #Component having funcName=="Derivative"
                 if (XPSSample[[ii]]@Flags[[1]]) { #BE scale set
                     MidPos <- round(XPSSample[[ii]]@Components[[jj]]@param[1,1], 2) #Component Intensity componente
                     PosMin <- round(XPSSample[[ii]]@Components[[jj]]@param[1,2], 2) #Component Intensity componente
                     PosMax <- round(XPSSample[[ii]]@Components[[jj]]@param[1,3], 2) #Component Intensity componente
                 } else {
                     MidPos <- round(XPSSample[[ii]]@Components[[jj]]@param[1,1], 2) #Component Intensity componente
                     PosMax <- round(XPSSample[[ii]]@Components[[jj]]@param[1,2], 2) #Component Intensity componente
                     PosMin <- round(XPSSample[[ii]]@Components[[jj]]@param[1,3], 2) #Component Intensity componente
                 }
                 TabTxt <<- c(TabTxt, paste("     Max Derivative Position : ",PosMax, sep=""), "\n")
                 TabTxt <<- c(TabTxt, paste("     MidPoint Derivative Position : ",MidPos, sep=""), "\n")
                 TabTxt <<- c(TabTxt, paste("     Min Derivative Position : ",PosMin, sep=""), "\n")
                 TabTxt <<- c(TabTxt, paste("     Max : Min Distance : ",round(abs(PosMax-PosMin), 2), sep=""), "\n")
                 TabTxt <<- c(TabTxt, " \n")
              }
          }
      }
   }

##QUANTIFICATION Report
   MakeQuantReport <- function(){
      sumCoreLine <- 0
      TotArea <- 0
      CellLength <- c(10, 15, 5, 8, 8, 9)
      cellB <- " "
      #Columns names

      AreaComp <- list()
      NormAreaComp <- list()
      for(ii in SelectedCL1){  #SelectedCL1 contains the analyzed CoreLines excluding VB Auger spectra
          AreaComp[[ii]] <- vector()
          NormAreaComp[[ii]] <- vector()
          N_Comp <- length(XPSSample[[ii]]@Components)
          E_stp <- abs(XPSSample[[ii]]@.Data[[1]][2]-XPSSample[[ii]]@.Data[[1]][1]) #energy step
          if(N_Comp == 0){  #Contribution of NON-fitted BKG-subtracted Core-Lines
             RSF <- XPSSample[[ii]]@RSF
             AreaComp[[ii]][1] <- sum(XPSSample[[ii]]@RegionToFit$y-XPSSample[[ii]]@Baseline$y)*E_stp
             NormAreaComp[[ii]][1] <- AreaComp[[ii]][1]/RSF
          } else {
             for(jj in 1:N_Comp){    #ii runs on the Core-Lines, jj runs on core-line-fit-components
                 RSF <- XPSSample[[ii]]@Components[[jj]]@rsf
                 AreaComp[[ii]][jj] <- sum(XPSSample[[ii]]@Components[[jj]]@ycoor-XPSSample[[ii]]@Baseline$y)*E_stp  #single FITcomponent contribution
                 if (RSF==0) { #RSF not defined
                     NormAreaComp[[ii]][jj] <- AreaComp[[ii]][jj]
                 } else {
                     NormAreaComp[[ii]][jj] <- AreaComp[[ii]][jj]/RSF
                 }
             }
          }
          TotArea <- TotArea + sum(NormAreaComp[[ii]]) #Contributo del Fit
      }

      for(ii in SelectedCL1){
          if (length(grep(":::", XPSSample[[ii]]@Info) > 0)){
              TabTxt <<- c(TabTxt, XPSSample[[ii]]@Info, "\n")
          }
          #First Table Row
          CellLength <- c(12, 15, 5, 5, 8, 10)
          txt <- c("Components", "Area(cps)", "FWHM", "RSF", "BE(eV)", "TOT.(%)")
          TabTxt <<- c(TabTxt,printCell("tableRow", txt, CellB=" ", CellLength, "center"), "\n")
          N_Comp <- length(XPSSample[[ii]]@Components)
          CompNames <- names(XPSSample[[ii]]@Components)

          #Concentration of CoreLine as a whole
          RSF <- XPSSample[[ii]]@RSF
          Area <- sprintf("%1.2f", sum(AreaComp[[ii]]))  #round to 2 decimals and transform to string
          Conc <- sprintf("%1.2f",100*sum(NormAreaComp[[ii]])/TotArea)
          txt <- c(XPSSample[[ii]]@Symbol, Area, RSF, " ", " ", Conc )  #core-line concentrations + FitFunct, FWHM and BE
          TabTxt <<- c(TabTxt,printCell("tableRow", txt, cellB, CellLength, "center"), "\n")

          #Rows describing  FitComponent concentration
          if(N_Comp > 0){  #Contribution of Fit Components
             for(jj in 1:N_Comp){ #jj runs on CoreLines, jj runs on Fit components
                 Area <- sprintf("%1.2f",AreaComp[[ii]][jj]) #area componente jj linea di core ii
                 FWHM <- sprintf("%1.2f",XPSSample[[ii]]@Components[[jj]]@param[3,1]) #Component FWHM
                 RSF <- sprintf("%1.3f",XPSSample[[ii]]@Components[[jj]]@rsf) #Component RSF
                 BE <- sprintf("%1.2f",XPSSample[[ii]]@Components[[jj]]@param[2,1]) #Component BE
                 Conc <- sprintf("%1.2f",100*NormAreaComp[[ii]][jj]/TotArea)  #Concentration of componente
                 txt <- c(CompNames[jj], Area, FWHM, RSF, BE, Conc) #make string to print
                 TabTxt <<- c(TabTxt,printCell("tableRow", txt, cellB, CellLength, "center"), "\n") #print in mode TABLEROW
             }
          }
          TabTxt <<- c(TabTxt, " \n")
      }
   }

   ResetVars <- function(){
      SelectedCL1 <<- NULL
      SelectedCL2 <<- NULL
      sapply(FittedCL, function(x) tclvalue(x) <- FALSE)
      sapply(NonFittedCL, function(x) tclvalue(x) <- FALSE)
      tclvalue(StdRprt) <- FALSE
      tclvalue(FRprt) <- FALSE
      tclvalue(QRprt) <- FALSE
      TabTxt <<- NULL
      tcl(ReprtWin, "delete", "0.0", "end")

   }

#----- variabili -----
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
   XPSSample <- get(activeFName, envir = .GlobalEnv)
   XPSSettings <- get("XPSSettings", envir=.GlobalEnv)
   NCorelines <- length(XPSSample)
   SelectedCL1 <- NULL
   SelectedCL2 <- NULL
   FittedIdx <- FindFittedCL()
   if(length(FittedIdx) == 0){
      FittedIdx <- 0
      FittedCL <- NULL
      NonFittedCL <- XPSSpectList(activeFName)
      NonFittedIdx <- seq(1:NCorelines)
   } else {
      FittedCL <- XPSSpectList(activeFName)[FittedIdx]  #names of the analyzed CoreLines
      NonFittedCL <- XPSSpectList(activeFName)[-FittedIdx]   #names of the NON fitted Core-Lines
      NonFittedIdx <- seq(1:NCorelines)[-FittedIdx]
   }
   TabTxt <- NULL
   XScroll <- list()
   YScroll <- list()
   Font <- XPSSettings$General[1]
   FStyle <- XPSSettings$General[2]
   FSize <- XPSSettings$General[3]


#---Widget
   CkBxGroup1 <- tclVar()  #container needed to regenerate che checkboxbutton children
   CkBxGroup2 <- tclVar()  #container needed to regenerate che checkboxbutton children
   ChkCL1 <- tclVar()  #container needed to regenerate che checkboxbutton children
   ChkCL2 <- tclVar()  #container needed to regenerate che checkboxbutton children
   MyFont1 <- tclVar()
   MyFont1 <- tcl("font", "create", MyFont1, family="helvetica", size=11, weight="bold")
   MyFont2 <- tclVar()
   MyFont2 <- tcl("font", "create", MyFont2, 
                   family = XPSSettings$General[1],
                   size = XPSSettings$General[3],
                   weight = XPSSettings$General[2])

   txtWin <- tktoplevel()
   tkwm.title(txtWin,"XPS SAMPLE REPORT")
   tkwm.geometry(txtWin, "+100+50")   #SCREEN POSITION from top-left corner

   RGroup1 <- ttkframe(txtWin,  borderwidth=2, padding=c(5,5,20,20))  #padding values=20 to allow scrollbars
   tkgrid(RGroup1, row = 1, column=1, sticky="w")
   RFrame1 <- ttklabelframe(RGroup1, text = " SELECT the XPSsample ", borderwidth=2)
   tkgrid(RFrame1, row = 2, column=1, sticky="w")

   XS <- tclVar("")
   ChkXSamp <- ttkcombobox(RFrame1, width = 25, textvariable = XS, values = FNameList)
   tclvalue(XS) <- activeFName
   tkgrid(ChkXSamp, row = 3, column=1, padx=10, pady=7, sticky="w")
   tkbind(ChkXSamp, "<<ComboboxSelected>>", function(){
                         ResetVars()
                         activeFName <<- tclvalue(XS)
                         XPSSample <<- get(activeFName, envir = .GlobalEnv)
                         LL <- length(FittedCL)    #delete checkbox for Fitted and NON Fitted CoreLines
                         FittedCL <<- NonFittedCL <<- NULL
                         FittedIdx <<- FindFittedCL()
                         LL <- length(FittedCL)    #N. Fitted CL in the new XPSSample
                         if(length(FittedIdx) == 0){
                            FittedIdx <<- 0
                            FittedCL <<- NULL
                            NonFittedCL <<- XPSSpectList(activeFName)
                            NonFittedIdx <<- seq(1:NCorelines)
                         } else {
                            FittedCL <<- XPSSpectList(activeFName)[FittedIdx]  #names of the analyzed CoreLines
                            NonFittedCL <<- XPSSpectList(activeFName)[-FittedIdx]   #names of the NON fitted Core-Lines
                            NonFittedIdx <<- seq(1:NCorelines)[-FittedIdx]
                         }

                         #regenerate ChkCL1 with the new fittedCL list
                         ClearWidget(CkBxGroup1)
                         SelectedCL1 <<- NULL
                         ChkCL1 <- NULL
                         CboxRow <- 0
                         jj <- 0
                         LL <- length(FittedCL)
                         if (LL > 0) {
                             for(ii in 1:LL){
                                 ChkCL1 <- tkcheckbutton(CkBxGroup1, text=FittedCL[ii], variable=FittedCL[ii], onvalue = FittedIdx[ii], offvalue = 0,
                                                         command=function(){
                                                                 SelectedCL1 <<- sapply(FittedCL, function(x) tclvalue(x))
                                                                 SelectedCL1 <<- as.integer(SelectedCL1)
                                                         })
                                 tkgrid(ChkCL1, row = 1+CboxRow, column=1, padx = c(5+jj*85, 10), pady=1, sticky="w")
                                 jj <- jj+1        #increases the padding to make a row of max 7 checkboxes
                                 if (ii == 7) {
                                     CboxRow <- 1  #after 7 checkboxes return from beginning in a second row
                                     jj <- 0
                                 }
                                 if (ii == 14) {   #after 14 checkboxes return from beginning in a third row
                                     CboxRow <- 2
                                     jj <- 0
                                 }
                                 tclvalue(FittedCL[ii]) <- FALSE
                             }
                         } else {
                             tkgrid( ttklabel(CkBxGroup1, text="   "), row = 1, column=1, padx=5, pady=1, sticky="w")
                         }
                         #regenerate ChkCL1 with the new NonfittedCL list
                         ClearWidget(CkBxGroup2)
                         SelectedCL2 <<- NULL
                         ChkCL2 <- NULL
                         CboxRow <- 0
                         jj <- 0
                         LL <- length(NonFittedCL)
                         if (LL > 0) {
                             for(ii in 1:LL){
                                 ChkCL2 <- tkcheckbutton(CkBxGroup2, text=NonFittedCL[ii], variable=NonFittedCL[ii], onvalue = NonFittedIdx[ii], offvalue = 0,
                                                         command=function(){
                                                                 SelectedCL2 <<- sapply(NonFittedCL, function(x) tclvalue(x))
                                                                 SelectedCL2 <<- as.integer(SelectedCL2)
                                                         })
                                 tkgrid(ChkCL2, row = 1+CboxRow, column = 1, padx = c(5+jj*85, 0), pady=1, sticky="w")
                                 jj <- jj+1        #increases the padding to make a row of max 7 checkboxes
                                 if (ii == 7) {
                                     CboxRow <- 1  #after 7 checkboxes return from beginning in a second row
                                     jj <- 0
                                 }
                                 if (ii == 14) {   #after 14 checkboxes return from beginning in a third row
                                     CboxRow <- 2
                                     jj <- 0
                                 }
                                 tclvalue(NonFittedCL[ii]) <- FALSE
                             }
                         } else {
                             tkgrid( ttklabel(CkBxGroup2, text="   "), row = 1, column=1, padx=5, pady=1, sticky="w")
                         }
                         plot(XPSSample)
                     })

   RFrame2 <- ttklabelframe(RGroup1, text = " Select Core-Lines and the Report Format ", borderwidth=2)
   tkgrid(RFrame2, row = 4, column=1, sticky="w")

   tkgrid( ttklabel(RFrame2, text="Fitted Core-Lines: "), row = 5, column=1, padx=5, pady=1, sticky="w")

   CkBxGroup1 <- ttkframe(RFrame2, borderwidth=0, padding=c(0,0,0,0))
   tkgrid(CkBxGroup1,  row = 6, column=1, padx=0, pady=0, sticky="w")

   LL <- length(FittedCL)
   CboxRow <- 0
   if (LL > 0){
       jj <- 0
       for(ii in 1:LL){
           ChkCL1 <- tkcheckbutton(CkBxGroup1, text=FittedCL[ii], variable=FittedCL[ii], onvalue = FittedIdx[ii], offvalue = 0,
                                   command=function(){
                                              SelectedCL1 <<- sapply(FittedCL, function(x) tclvalue(x))
                                              SelectedCL1 <<- as.integer(SelectedCL1)
                                          })

           tkgrid(ChkCL1, row = 1+CboxRow, column=1, padx = c(5+jj*85, 10), pady=1, sticky="w")
           jj <- jj+1        #increases the padding to make a row of max 7 checkboxes
           if (ii == 7) {
               CboxRow <- 1  #after 7 checkboxes return from beginning in a second row
               jj <- 0
           }
           if (ii == 14) {   #after 14 checkboxes return from beginning in a third row
               CboxRow <- 2
               jj <- 0
           }
           tclvalue(FittedCL[ii]) <- FALSE
       }
   } else {
       tkgrid( ttklabel(CkBxGroup1, text="  "), row = 1, column=1, padx=5, pady=1, sticky="w")
   }

   FRprt <- tclVar(FALSE)
   FitRprt <- tkcheckbutton(RFrame2, text="Fit report", variable=FRprt, onvalue = TRUE, offvalue = FALSE)
   tkgrid(FitRprt, row = 7, column=1, padx = 5, pady=3, sticky="w")
   tclvalue(FRprt) <- FALSE
   tkconfigure(FitRprt, font=MyFont1)
   xx <- as.integer(tkwinfo("reqwidth", FitRprt))+30

   QRprt <- tclVar(FALSE)
   QuantRprt <- tkcheckbutton(RFrame2, text="Quantification report", variable=QRprt, onvalue = TRUE, offvalue = FALSE)
   tkgrid(QuantRprt, row = 7, column=1, padx = c(xx, 20), pady=3, sticky="w")
   tclvalue(QRprt) <- FALSE
   tkconfigure(QuantRprt, font=MyFont1)
   xx <- xx + as.integer(tkwinfo("reqwidth", QuantRprt))+30

   MakeRprtB1 <- tkbutton(RFrame2, text="  MAKE REPORT  ", command=function(){
                         if (tclvalue(StdRprt) == "0" && tclvalue(QRprt) == "0" && tclvalue(FRprt) == "0") {
                             tkmessageBox(message="Please Select Which Report to Display", title="WARNING", icon="warning")
                         }
                         ReportSelection()
                     })
   tkgrid(MakeRprtB1, row = 7, column = 1, padx = c(xx, 0), pady = 3, sticky="w")
   xx <- xx + as.integer(tkwinfo("reqwidth", MakeRprtB1))+30

   ResetB1 <- tkbutton(RFrame2, text="     RESET     ", command=function(){
                         ResetVars()
                     })
   tkgrid(ResetB1, row = 7, column = 1, padx = c(xx,10), pady=3, sticky="w")

   tkSep <- ttkseparator(RFrame2, orient="horizontal")
   tkgrid(tkSep, row = 9, column=1, pady = c(10,5), sticky="we")

   tkgrid( ttklabel(RFrame2, text="Other Core-Lines: "), row = 10, column = 1, padx = 5, pady = 1, sticky="w")

   CkBxGroup2 <- ttkframe(RFrame2, borderwidth=0, padding=c(0,0,0,0))
   tkgrid(CkBxGroup2,  row = 11, column=1, padx=0, pady=0, sticky="w")
   CboxRow <- 0
   jj <- 0
   LL <- length(NonFittedCL)
   if (LL > 0){
       for(ii in 1:LL){
           ChkCL2 <- tkcheckbutton(CkBxGroup2, text=NonFittedCL[ii], variable=NonFittedCL[ii], onvalue = NonFittedIdx[ii], offvalue = 0,
                                   command=function(){
                                              SelectedCL2 <<- sapply(NonFittedCL, function(x) tclvalue(x))
                                              SelectedCL2 <<- as.integer(SelectedCL2)
                                           })
           tkgrid(ChkCL2, row = 1+CboxRow, column = 1, padx = c(5+jj*85, 0), pady=1, sticky="w")
           jj <- jj+1        #increases the padding to make a row of max 7 checkboxes
           if (ii == 7) {
               CboxRow <- 1  #after 7 checkboxes return from beginning in a second row
               jj <- 0
           }
           if (ii == 14) {   #after 14 checkboxes return from beginning in a third row
               CboxRow <- 2
               jj <- 0
           }
           tclvalue(NonFittedCL[ii]) <- FALSE
       }
   } else {
       tkgrid( ttklabel(CkBxGroup2, text="    "), row = 1, column = 1, padx = 5, pady = 1, sticky="w")
   }
   StdRprt <- tclVar()
   StandRprt <- tkcheckbutton(RFrame2, text="Standard report", variable=StdRprt, onvalue = TRUE, offvalue = FALSE)
   tkgrid(StandRprt, row = 12, column = 1, padx=5, sticky="w")
   tclvalue(StdRprt) <- FALSE
   tkconfigure(StandRprt, font=MyFont1)
   xx <- as.integer(tkwinfo("reqwidth", StandRprt))+30

   MakeRprtB2 <- tkbutton(RFrame2, text="  MAKE REPORT  ", command=function(){
                         if (tclvalue(StdRprt) == "0" && tclvalue(QRprt) == "0" && tclvalue(FRprt) == "0") {
                             tkmessageBox(message="Please Select Which Report to Display", title="WARNING", icon="warning")
                         }
                         ReportSelection()
                     })
   tkgrid(MakeRprtB2, row = 12, column = 1, padx = c(xx, 0), pady = 5, sticky="w")
   xx <- xx + as.integer(tkwinfo("reqwidth", MakeRprtB2))+30

   ResetB2 <- tkbutton(RFrame2, text="     RESET     ", command=function(){
                         ResetVars()
                     })
   tkgrid(ResetB2, row = 12, column = 1, padx=c(xx, 0), pady = 5, sticky="w")

   RFrame3 <- ttklabelframe(RGroup1, text = " Save & Exit ", borderwidth=2)
   tkgrid(RFrame3, row = 14, column=1, sticky="w")

   RFrame2 <- ttklabelframe(RGroup1, text = " Select Core-Lines and the Report Format ", borderwidth=2)
   tkgrid(RFrame2, row = 4, column=1, sticky="w")

   SaveButt <- tkbutton(RFrame3, text="   SAVE TO FILE   ", command=function(){
                         TabTxt <- as.data.frame(TabTxt)
                         filename <- tclvalue(tkgetSaveFile(initialdir = getwd(),
                                          initialfile = "", title = "SAVE FILE"))
                         filename <- unlist(strsplit(filename, "\\."))
                         if( is.na(filename[2])) {        #if extension not given, .txt by default
                            filename[2] <- ".txt"
                         } else {
                            filename[2] <- paste(".", filename[2], sep="")
                         }
                         filename <- paste(filename[1], filename[2], sep="")
                         write.table(x=TabTxt, file = filename, sep=" ", eol="\n",
                                     dec=".", row.names=FALSE, col.names=FALSE)
                         XPSSaveRetrieveBkp("save")
                     })
   tkgrid(SaveButt, row = 15, column = 1, padx = 5, pady = 5, sticky="w")

   ExitButt <- tkbutton(RFrame3, text="     EXIT     ", command=function(){
                        tkdestroy(txtWin)
                        XPSSaveRetrieveBkp("save")
                     })
   tkgrid(ExitButt, row = 15, column = 1, padx = c(150, 5), pady = 5, sticky="w")

   ReprtWin <- tktext(RGroup1, height=15)
   tkgrid(ReprtWin, row = 16, column = 1, padx = c(0,0), pady = c(0,0), sticky="w")
   tkconfigure(ReprtWin, font=MyFont2)
   #in Rgroup1 padding=c(5,5,20,20): 20 needed to have space for the scrollbars


   cat(" ")
}
