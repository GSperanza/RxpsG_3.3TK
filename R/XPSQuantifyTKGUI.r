#Function to perform quantifications on XPS spectra
#allowing selection of corelines and fit components
#for the computation of the atomic concentrations.
#XPSquantify and XPScalc used only by XPSMoveComponent.

#' @title XPSQuant
#' @description XPSQuant() performs the elemental quantification 
#'   for a selected XPS-Sample. Provides a userfriendly interface 
#'   with the list of Corelines of the selected XPS-Sample
#'   Each Coreline can be count/omit from the elemental quantification.
#'   If peak fitting is present also each of the fitting component 
#'   can be count/omit from the elemental quantification.
#'   Finally also relative RSF of the coreline or individual fitting 
#'   components can be modified.
#' @examples
#' \dontrun{
#' 	XPSQuant()
#' }
#' @export
#'

XPSQuant <- function(){

#Extract PE from each slot @Info of the XPSSample spectra
   RetrivePE <- function(CL=NULL){  #Retrieve the PE value from the CL coreline information slot
      PEnergy <- NULL
      if(is.null(CL)){
         LL <- length(XPSSample)
         for(ii in 1:LL){
             info <- XPSSample[[ii]]@Info[1]   #retrieve info containing PE value
             xxx <- unlist(strsplit(info, "Pass energy"))[2]  #extract PE value
             PEnergy[ii] <- as.integer(gsub("\\D","", xxx)) # extract numeric characters from xxx = PE value
             if(is.na(PEnergy[ii])){
                PEwin <- tktoplevel()
                tkwm.title(PEwin,"INPUT CORE-LINE PASS ENERGY")
                tkwm.geometry(PEwin, "+200+200")
                PEgroup1 <- ttkframe(PEwin, borderwidth=0, padding=c(0,0,0,0) )
                tkgrid(PEgroup1, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
                txt <- paste("CoreLine ", XPSSample[[ii]]@Symbol, ": \nPass Energy unknown! Please provide the Pass Energy value", sep="")
                tkgrid( ttklabel(PEgroup1, text=txt, font="Serif 12"),
                        row = 1, column = 1, padx = 5, pady = 5, sticky="w")
                CLPE1 <- tclVar("CoreLine Pass Energy ")  #sets the initial msg
                CL.PassE <- ttkentry(PEgroup1, textvariable=CLPE1, foreground="grey")
                tkgrid(CL.PassE, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
                tkbind(CL.PassE, "<FocusIn>", function(K){
                         tclvalue(CLPE1) <- ""
                         tkconfigure(CL.PassE, foreground="red")
                     })
                tkbind(CL.PassE, "<Key-Return>", function(K){
                         tkconfigure(CL.PassE, foreground="black")
                     })
                tkSep <- ttkseparator(PEgroup1, orient="horizontal")
                tkgrid(tkSep, row = 3, column = 1, sticky="we")

                PEgroup2 <- ttkframe(PEwin, borderwidth=0, padding=c(0,0,0,0) )
                tkgrid(PEgroup2, row = 4, column = 1, padx = 0, pady = 0, sticky="w")
                OKBtn <- tkbutton(PEgroup2, text=" OK ", width=12,  command=function(){
                         PEnergy[ii] <<- as.integer(tclvalue(CLPE1))
                         XPSSample[[ii]]@Info[1] <<- paste(XPSSample[[ii]]@Info, " Pass energy ", PEnergy[ii], sep="")
                         tkdestroy(PEwin)
                     })
                tkgrid(OKBtn, row = 1, column = 1, padx=5, pady=5, sticky="w")
                CancelBtn <- tkbutton(PEgroup2, text=" Cancel ", width=12,  command=function(){
                         tkdestroy(PEwin)
                     })
                tkgrid(CancelBtn, row = 1, column = 2, padx=5, pady=5, sticky="w")
                tkwait.window(PEwin)
            }
         }
      } else {
         info <- CL@Info[1]   #retrieve info containing PE value
         xxx <- unlist(strsplit(info, "Pass energy"))[2]  #extract PE value
         PEnergy <- as.integer(gsub("\\D","", xxx)) # extract numeric characters from xxx = PE value
      }
      return(PEnergy)
   }

   CorrectPE <- function(CheckedCL, CK.PassE, SurPE, CLinePE){
      idx <- which(CK.PassE == SurPE)   #select all the corelines with PE==160. It is supposed that survey cannot be used for quantification. Only CL extracted from survey can be used
      LL <- length(idx)
      for (ii in 1:LL){
          jj <- CheckedCL[idx[ii]]
          info <- XPSSample[[jj]]@Info[1]   #retrieve info containing PE value
          xxx <- strsplit(info, "Pass energy")  #extract PE value
          PE <- strsplit(xxx[[1]][2], "Iris") #PE value
          info <- paste(xxx[[1]][1],"Pass energy ", CLinePE, "   Iris", PE[[1]][2], sep="")
          XPSSample[[jj]]@Info[1] <<- info
          assign(activeFName, XPSSample, envir=.GlobalEnv)
      }
   }


   CalcNormCoeff <- function(CheckedCL, CK.PassE, SurIdx){
# CheckedCL = CoreLines selected for quantification
# CK.PassE = PE of CheckedCL
# SurIdx = index of the survey
# It is supposed that a coreline was extracted from the survey
# For elements extracted from survey at high PE the photoelectron collection efficiency
# is different from that of the corelines with low PE (higher energy resolution)
# Spectra acquired at different PE need a normalization coefficient
# Let us consider C1s extracted from survey(PE=160eV in KratosXPS) and C1s coreline (PE=20eV)
# Different PE leads to markedly different signal intensities.
# Let A1 = area of C1s at PE=160,  A2 = area of C1s at PE=20
# It has discovered that a simple normalization for the PE: A1/160, A2/20   does not work
# In htis function a high resolution-Coreline (lower PE) is compared with the same spectrum 
# from the survey and a proportion coeff. is computed
# This proportion coeff. is used to normalize Corelines acquired at different PE.

#variables
      PassCL <- NULL
      BaseLine <- NULL
      X <- list()
      Y <- list()
      Area1 <- Area2 <- Area3 <- NULL
#------

      CLinePE <- min(CK.PassE)
      txt <- paste("Please Confirm if the Pass Energy of the High-Resolution Core Lines is ", as.character(CLinePE), sep="")
      answ <- tkmessageBox(message=txt, type="yesno", title="CORE LINE PASS ENERGY", icon="info")
      if (tclvalue(answ) == "no"){
          CLwin <- tktoplevel()
          tkwm.title(CLwin,"CORE LINE PASS ENERGY")
          tkwm.geometry(CLwin, "+200+200")
          CLgroup <- ttkframe(CLwin, borderwidth=0, padding=c(0,0,0,0) )
          tkgrid(CLgroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
          tkgrid( ttklabel(CLgroup, text="Please input the Pass Energy of the Core-Lines"),
                  row = 1, column = 1, padx = 5, pady = 5, sticky="w")

          CLPE2 <- tclVar("CoreLine Pass Energy ")  #sets the initial msg
          CL.PassE <- ttkentry(CLgroup, textvariable=CLPE2, foreground="grey")
          tkgrid(CL.PassE, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
          tkbind(CL.PassE, "<FocusIn>", function(K){
                   tclvalue(CLPE2) <- ""
                   tkconfigure(CL.PassE, foreground="red")
               })
          tkbind(CL.PassE, "<Key-Return>", function(K){
                   tkconfigure(CL.PassE, foreground="black")
               })
          OKBtn <- tkbutton(CLgroup, text=" OK ", width=12,  command=function(){
                   CLinePE <<- as.integer(tclvalue(CLPE2))
                   tkdestroy(CLwin)
               })
          tkgrid(OKBtn, row = 3, column = 1, padx=5, pady=5, sticky="w")
          tkwait.window(CLwin)
      }
      
      idx <- which(CK.PassE == CLinePE) #select all the corelines acquired at lower PE
      CLidx <- CheckedCL[idx]  #CL.PE will contains CL indexes which could be in sparse order
      LL <- length(CLidx)
      MaxI <- NULL

#Now find the coreline with max intensity and compare this coreline
#with the same in the survey to compute the normalization factor
      for(ii in 1:LL){ #Among the selected corelines finds the one with max intensity
          MaxI[ii] <- max(XPSSample[[CLidx[ii]]]@.Data[[2]])  #find the coreline with max intensity
      }
      idx <- which(MaxI == max(MaxI))  #index of the coreline with max intensity
      idx <- CLidx[idx]   #index of the CL with max internsity and acquired with min PE

      if(XPSSample[[SurIdx]]@units[1] != XPSSample[[idx]]@units[1]){
         tkmessageBox(message="WARNING: Wide Spectrum and Core-Lines energy scale units are different. Check please!", title="WARNING", icon="warning")
         NormCoeff <<- -1
         return()
      }
      CLName <- XPSSample[[idx]]@Symbol
      X[[1]] <- unlist(XPSSample[[idx]]@RegionToFit$x) #resume the abscissa
      Y[[1]] <- unlist(XPSSample[[idx]]@RegionToFit$y)
      LL <- length(X[[1]])
      DY <- (Y[[1]][LL] - Y[[1]][1])/(LL-1)   #energy step
      for (ii in 1:LL){
          BaseLine[ii] <- Y[[1]][1]+(ii-1)*DY #this is the linear baseline
      }
      Y[[1]] <- Y[[1]]-BaseLine
      E_stp <- abs(X[[1]][2]-X[[1]][1])
      Area1 <- sum(Y[[1]])*E_stp    #Area of high resolution coreline

      Xlim <- range(X[[1]])  #X range of the selected CL
      Xlim <- sort(Xlim, decreasing=FALSE)
      Xlim[1] <- Xlim[1]-2   #extend the CL X-range for higher PE (maybe PE=160eV)
      Xlim[2] <- Xlim[2]+2   #extend the CL X-range for higher PE
      if (XPSSample[[idx]]@Flags[1]==TRUE) {Xlim <- c(Xlim[2], Xlim[1])} #Binding energy scale
      idx1 <- findXIndex(XPSSample[[SurIdx]]@.Data[[1]], Xlim[1])
      idx2 <- findXIndex(XPSSample[[SurIdx]]@.Data[[1]], Xlim[2])
      X[[2]] <- unlist(XPSSample[[SurIdx]]@.Data[[1]][idx1:idx2])  #X values of coreline extracted from survey
      Y[[2]] <- unlist(XPSSample[[SurIdx]]@.Data[[2]][idx1:idx2])  #Y values of coreline extracted from survey
      Ylim <- range(sapply(Y, sapply, range))
      LL <- length(X[[2]])

      BaseLine <- NULL
      DY <- (Y[[2]][LL] - Y[[2]][1])/(LL-1)   #energy step
      for (ii in 1:LL){
          BaseLine[ii] <- Y[[2]][1]+(ii-1)*DY #this is the linear baseline
      }

      Y[[2]] <- Y[[2]]-BaseLine
      E_stp <- abs(X[[2]][2]-X[[2]][1])
      Area2 <- sum(Y[[2]])*E_stp              #Area of the coreline extracted from survey

      NormCoeff <<- Area2/Area1
      X[[3]] <- X[[2]]
      Y[[3]] <- Y[[2]]/NormCoeff
      Area3 <- sum(Y[[3]])*E_stp
      cat("\n => Normalization coefficient: ", round(NormCoeff, 3))
#----- Graphics
      if (XPSSample[[SurIdx]]@Flags[[1]]){        #Binding energy set
          Xlim <- sort(Xlim, decreasing=TRUE)
      } else {
          Xlim <- sort(Xlim, decreasing=FALSE)
      }
      XLabel <- XPSSample[[SurIdx]]@units[1]
      YLabel <- XPSSample[[SurIdx]]@units[2]
      LL <- max(sapply(X, length))
      X <- sapply(X, function(x) {length(x)<-LL   #insert NAs if the length of X[] < LL
                                  return(x)})
      Y <- sapply(Y, function(x) {length(x)<-LL   #insert NAs if the length of Y[] < LL
                                  return(x)})
      X <- matrix(unname(unlist(X)), ncol=3)      #transform list in matrix
      Y <- matrix(unname(unlist(Y)), ncol=3)
      txt <- paste("Normalization coeff. computed on ", CLName, sep="")
      matplot(x=X, y=Y, xlim=Xlim, type="l", lty=1, lwd=1, col=c("black","green","red"), main=txt, xlab=XLabel, ylab=YLabel)
      txt <- c("High res. CL", "Extracted CL ", "Normalized CL")
      legend(x=Xlim[1], y=Ylim[2]/1.2, legend=txt, text.col=c("black","green","red"))
      txt <- paste(" Check the graph. The Integral Area of RED Normalized Spectrum \n must be EQUAL to the Integral Area of and BLACK Spectrum", sep="")
      answ <- tkmessageBox(message=txt, type="yesno", title="Compare HighRes and Normalized Spectra", icon="warning")
      if (tclvalue(answ) == "yes") {
          SurPE <- RetrivePE(XPSSample[[SurIdx]]) # Retrieve PE used for the survey
          idx <- which(CK.PassE == SurPE)
          idx <- CheckedCL[idx] #retrieve the index of the selected corelines having PE=SurPE.
           for(ii in idx){
              XPSSample[[ii]]@.Data[[2]] <<- XPSSample[[ii]]@.Data[[2]]/NormCoeff
              XPSSample[[ii]]@RegionToFit$y <<- XPSSample[[ii]]@RegionToFit$y/NormCoeff
              XPSSample[[ii]]@Baseline$y <<- XPSSample[[ii]]@Baseline$y/NormCoeff
              XPSSample[[ii]]@RegionToFit$NormCoeff <<- NormCoeff
              if(hasComponents(XPSSample[[ii]]) == TRUE) {
                 XPSSample[[ii]]@Components <<- sapply(XPSSample[[ii]]@Components, function(x) {
                                                              x@ycoor <- x@ycoor/NormCoeff
                                                              return(x) } )
                 XPSSample[[ii]]@Fit$y <<- XPSSample[[ii]]@Fit$y/NormCoeff
              }
          }
          CorrectPE(CheckedCL, CK.PassE, SurPE, CLinePE)
      }
      return()
}


#XPScalc() function cannot be applied since here you can select fit components and their RSF
   quant <- function(CoreLineComp){
      N_CL <- length(CoreLineComp)
      maxFitComp <- 0

      for(ii in 1:length(CoreLineComp)){
         LL <- length(CoreLineComp[[ii]])
         if (LL > maxFitComp) { maxFitComp <- LL }
      }
      AreaCL <- rep(0,N_CL)
      NormAreaCL <- rep(0,N_CL)
      sumComp <- matrix(0,nrow=N_CL,ncol=maxFitComp)  #define a zero matrix
      AreaComp <- matrix(0,nrow=N_CL,ncol=maxFitComp)
      maxNchar <- 0
      TabTxt <<- ""
      QTabTxt <<- ""

      for(ii in 1:N_CL){
         AreaCL[ii] <- 0
         indx <- CoreLineIndx[ii]
         if (CLCK[[ii]] == "TRUE") {   #if a coreline is selected
            N_comp <- length(CoreLineComp[[ii]])   #this is the number of fit components
            RSF <- XPSSample[[indx]]@RSF               #Sensitivity factor of the coreline
            E_stp <- abs(XPSSample[[indx]]@.Data[[1]][2]-XPSSample[[indx]]@.Data[[1]][1]) #energy step
            if (RSF != 0) {  #Sum is made only on components with RSF != 0 (Fit on Auger or VB not considered)
               AreaCL[ii] <- sum(XPSSample[[indx]]@RegionToFit$y-XPSSample[[indx]]@Baseline$y)*E_stp      #Integral undeer coreline spectrum
               NormAreaCL[ii] <- AreaCL[ii]/RSF  #Coreline contribution corrected for the relative RSF
            } else {
               AreaCL[ii] <- sum(XPSSample[[indx]]@RegionToFit$y-XPSSample[[indx]]@Baseline$y)*E_stp #if RSF not defined the integral under the coreline is considered
               NormAreaCL[ii] <- AreaCL[ii]
            }
            txt <- as.character(round(AreaCL[ii], 2))

            if (hasComponents(XPSSample[[indx]])) {   #is fit present on the coreline?
               for(jj in 1:N_comp){    #ii runs on CoreLines, jj runs on coreline fit components
                  comp <- CoreLineComp[[ii]][jj]
                  RSF <- XPSSample[[indx]]@Components[[comp]]@rsf
                  if (RSF!=0) { #if the RSF is lacking(es. Auger, VB spectra...) : it is not possible to make correction for the RSF...
                     sumComp[ii,jj] <- (sum(XPSSample[[indx]]@Components[[comp]]@ycoor)-sum(XPSSample[[indx]]@Baseline$y))*E_stp       #simple area of the coreline
                     AreaComp[ii,jj] <- sumComp[ii,jj]/RSF  #(area of the single component -  area backgroound) corrected for the RSF
                  } else {
                     sumComp[ii,jj] <- (sum(XPSSample[[indx]]@Components[[comp]]@ycoor)-sum(XPSSample[[indx]]@Baseline$y))*E_stp    #if RSF is not defined the simple spectral integral is computed
                     AreaComp[ii,jj] <- sumComp[ii,jj]      #Component spectral integral
                  }
                  txt <- as.character(round(sumComp[ii,jj], 2))
                  Nch <- nchar(txt)
                  if (Nch > maxNchar) { maxNchar <- Nch } #calculate the max number of characters of numbers describing component areas
               }
               if (RSF != 0) {  #summation only on components with RSF !=0 (no fit on Auger o VB
                   NormAreaCL[ii] <- sum(AreaComp[ii,])
                   AreaCL[ii] <- sum(sumComp[ii, ])
               }
            }
         }
      }

      AreaTot <- sum(NormAreaCL)
      sumTot <- sum(AreaCL)
      txt <- as.character(round(sumTot, 2))  #print original integral area witout RSF corrections
      maxNchar <- max(c(10,nchar(txt)+2))

      lgth <- c(10, maxNchar, 8, 8, 8, 9)    #width of table columns "Components", "Area", ", FWHM", "BE(eV)", "RSF", "TOT%"
      totLgth <- sum(lgth)+1
      cat("\n")

      QTabTxt <<- sprintf("%s %s \n", "   File Name: ", XPSSample@Filename)
      TabTxt <<- c(TabTxt,"   File Name: ", XPSSample@Filename,"\n")

      txt <- "-"  #separator
      cell <- printCell("separator", "-","",totLgth,"left")     #call cellprint in modality SEPARATOR  alignment LEFT
      TabTxt <<- c(TabTxt, cell)

      QTabTxt <<- sprintf("%s \n", QTabTxt)
      TabTxt <<- c(TabTxt,"\n")

      txt <- c("Comp.", "Area(cps)", "FWHM", "RSF", "BE(eV)", "TOT.(%)")
      cell <- printCell("tableRow", txt, CellB=" ", lgth, "center")
      QTabTxt <<- sprintf("%s %s \n", QTabTxt, cell)
#      CBrd2 <<- data.frame(Comp="Comp.", Area="Area(cps)", FWHM="FWHM ", RSF="RSF", BE="BE", TOT="TOT(%)", stringsAsFactors=FALSE)
      CBrd2 <<- data.frame(Comp=" ", Area=" ", FWHM=" ", RSF=" ", BE=" ", TOT=" ", stringsAsFactors=FALSE)
      CBrd2 <<- rbind(CBrd2, c(" ", " ", " ", " ",  " ", " "))

      TabTxt <<- c(TabTxt,cell,"\n")
      cell <- printCell("separator", "-","",totLgth,"left")
      TabTxt <<- c(TabTxt, cell)

      QTabTxt <<- sprintf("%s \n", QTabTxt)
      TabTxt <<- c(TabTxt,"\n")

  	   kk <- 2
      for(ii in 1:N_CL){
          indx <- CoreLineIndx[ii]
          if (CLCK[[ii]] == "TRUE") {
              #PEAK data
              Comp <- names(CoreLineComp)[ii]
              Area <- sprintf("%1.2f", AreaCL[ii])  #round number to 2 decimals and trasform in string
              RSF <- sprintf("%1.3f", XPSSample[[indx]]@RSF)
              Mpos <- NULL
              Mpos <- findMaxPos(XPSSample[[indx]]@RegionToFit)    #Mpos[1]==position of spectrum max,    Mpos[2]==spectrum max value
              BE <- sprintf("%1.2f", Mpos[1])
              if (RSF=="0.000") { #RSF not defined (es. Auger, VB spectra...) : cannot make correction for RSF
                 Conc <- sprintf("%1.2f",0)
              } else {
                 Conc <- sprintf("%1.2f",100*NormAreaCL[ii]/AreaTot)
              }
 		           txt <- c(Comp, Area, " ", RSF, BE, Conc )   #total concentration relative to the coreline: FWHM e BE not print
              CBrd2 <<- rbind(CBrd2, as.character(txt))
              cell <- printCell("tableRow", txt, CellB=" ", lgth, "center")
              QTabTxt <<- sprintf("%s %s \n", QTabTxt, cell)                  #this QTabTxt will appear in the Quant widget
              cell <- printCell("tableRow", txt, CellB="|", lgth, "center")
              TabTxt <<- c(TabTxt,cell,"\n")                    #this TabTxt will appear in the R consolle

              #FIT COMPONENT's data
              if (hasComponents(XPSSample[[indx]])) {
                 N_comp <- length(CoreLineComp[[ii]]) #number of fit components
                 for(jj in 1:N_comp){ #ii runs on the corelines, jj runs on the fit components
                    Comp <- CoreLineComp[[ii]][jj]
                    Area <- sprintf("%1.2f",sumComp[ii,jj]) #area of component jj coreline ii
                    FWHM <- sprintf("%1.2f",XPSSample[[indx]]@Components[[Comp]]@param[3,1]) #FWHM component ii
                    RSF <- sprintf("%1.3f",XPSSample[[indx]]@Components[[Comp]]@rsf) #RSF component ii
                    BE <- sprintf("%1.2f",XPSSample[[indx]]@Components[[Comp]]@param[2,1]) #BE component ii
                    if (RSF=="0.000") {
                       Conc <- sprintf("%1.2f",0)
                    } else {
                       Conc <- sprintf("%1.2f",100*AreaComp[ii,jj]/AreaTot)  #Concentration component ii
                    }
     		             txt <- c(Comp, Area, FWHM, RSF, BE, Conc) #make string to print
                    CBrd2 <<- rbind(CBrd2, as.character(txt))
                    cell <- printCell("tableRow", txt, CellB=" ", lgth, "center") #print string in modality TABLEROW
                    QTabTxt <<- sprintf("%s %s \n", QTabTxt, cell)
                    cell <- printCell("tableRow", txt, CellB="|", lgth, "center") #print string in modality TABLEROW
                    TabTxt <<- c(TabTxt,cell,"\n")
                 }
                 CBrd2 <<- rbind(CBrd2, c(" ", " ", " ", " ",  " ", " "))
              }
              QTabTxt <<- sprintf("%s \n",QTabTxt)
              TabTxt <<- c(TabTxt,"\n")
          }
      }

      tcl(QTable, "delete", "0.0", "end") #clears the quant_window
      tkinsert(QTable, "0.0", QTabTxt) #quantification report in QTable window

#      tkconfigure(QTable, font=MyFont)
      cat("\n", TabTxt)
   }

   SetRSF <- function(ii){
       for(ii in 1:NCoreLines){
           Eq.RSF <- TRUE  #Eq.RSF==TRUE means all RSF are equal
           indx <- CoreLineIndx[ii]
           LL <- length(unlist(OrigCoreLinComp[[ii]])) #the list vectors may have different lengths (different N. Fit components)
           if (LL > 0){                                #The RSF may be changed then control the RSF of all the fit components
              refRSF <- sapply(RSFCK[[ii]][1], function(x) tclvalue(x))   #set the referenceRSF as the first of the CorelineComponent
              refRSF <- as.numeric(refRSF)
              if (Eq.RSF == TRUE){                     #if all Component RSF are equal set same value also in the Coreline RSF-Slot
                  XPSSample[[indx]]@RSF <<- refRSF
              }
              lapply(seq_along(OrigCoreLinComp[[ii]]), function(jj){
                 newRSF <- sapply(RSFCK[[ii]][jj], function(x) tclvalue(x))
                 newRSF <- as.numeric(newRSF)
                 if (newRSF != refRSF) { Eq.RSF <- FALSE } #at end of for Eq.RSF==TRUE means all RSF are equal
                 XPSSample[[indx]]@Components[[jj]]@rsf <<- newRSF #load the new RSFR in the relative slot of the XPSSample
              })
           }
           if (LL == 0) {
              newRSF <- sapply(RSFCK[[ii]][1], function(x) tclvalue(x))
              XPSSample[[indx]]@RSF <<- as.numeric(newRSF) #load the new RSFR in the relative slot of the XPSSample
           }
       }
   }

   ResetComp <- function(ii){
       indx <- CoreLineIndx[ii]
       if (CLCK[[ii]] == FALSE) {   #if the coreline is not selected the fit component are disabled
          if (hasComponents(XPSSample[[indx]])) {     #Does the coreline possess fitting components?
              sapply(SelCMP[[ii]], function(x) tclvalue(x) <- FALSE) #deselects all fit components
              CLCK[[ii]] <<- FALSE
              CMPCK[[ii]] <<- ""    #All fitcomp of coreline ii are unchecked
          }
       }
       if (CLCK[[ii]] == TRUE) {    #if the coreline is selected all the fitting components are selected

          if (hasComponents(XPSSample[[indx]])) {
              sapply(SelCMP[[ii]], function(x) tclvalue(x) <- TRUE) #deselects all fit components
              CLCK[[ii]] <<- TRUE
              CMPCK[[ii]] <<- CoreLineComp[[ii]]  #All fitcomp of coreline ii are checked
          }
       }

   }

   CKHandlers <- function(){
         for(ii in 1:NCoreLines){
#----HANDLER on Widget-CoreLineCK to call ResetComp()
            if (CLCK[[ii]] != CLChecked[ii] && length(CLChecked[ii]) > 0) {
               CLChecked[ii] <<- CLCK[[ii]]
               ResetComp(ii)     #set/reset fit components
            }
#----HANDLER on Widget-ComponentCK
            NComp <- length(CoreLineComp[[ii]])
            tmp1 <- CMPCK[[ii]]
            tmp2 <- unlist(CoreLineComp[[ii]])
            if(CLCK[[ii]] == FALSE && is.null(tmp2) == TRUE) {  #Coreline has only baseline is un-selected
               tmp1 <- ""
               tmp2 <- ""
            } else if(length(tmp1) == 0 && length(tmp2) > 0) {  #Coreline with fit is un-selected
               CLChecked[ii] <<- FALSE
               sapply(CLChecked[[ii]], function(x) tclvalue(x) <- FALSE) #uncheck CL checkbox
               tmp1 <- ""
               CMPCK[[ii]] <- ""
               sapply(SelCMP[[ii]], function(x) tclvalue(x) <- FALSE) #deselects all fit components
            } else {
               sapply(CLChecked[[ii]], function(x) tclvalue(x) <- TRUE) #CL checkbox marked
               CLChecked[ii] <<- TRUE  #set CL checkbox marked
            }
            if(is.null(tmp2) == TRUE) {
               tmp2 <- ""
            }  # the correspondent coreline does NOT possess fitting components but only the BaseLine
            if (all(tmp1 == tmp2) == FALSE) {    #if the two vector contain different elements
#               CoreLineComp[[ii]] <<- tmp1 #modify component [ii] of the list following the checkbox component selections
#               if (nchar(tmp1) == 0) {
#                  names(CoreLineComp[[ii]]) <<- ""
#                  CLChecked[ii] <<- ""
#               }
#               if (nchar(tmp1) > 0) {
#                  CoreLineComp <<- setNames(CoreLineComp, CoreLineNames)
#               }
            }
         }
   }


#----MakeNb makes the notebook: a coreline for each notebook page
   MakeNb <- function(){
      for(ii in 1:NCoreLines){
          Qgroup[[ii]] <<- ttkframe(QNB, borderwidth=0, padding=c(0,0,0,0) )
          tkadd(QNB, Qgroup[[ii]], text=CoreLineNames[ii])
          NoComp <- "FALSE"
          indx <- CoreLineIndx[ii]
          tmp <- names(XPSSample[[indx]]@Components)
          if (is.null(tmp)) { NoComp <- "TRUE" }
          CoreLineComp <<- c(CoreLineComp, list(tmp))   #create a list containing the fit components of corelines
          txt <- paste(" CORE LINE", ii, " " , sep="")
          CLframe[[ii]] <<- ttklabelframe(Qgroup[[ii]], text=txt, borderwidth=2)
          tkgrid(CLframe[[ii]], row = 1, column = 1, padx = 5, pady = 5, sticky="w")
          SelCL[[ii]] <<- TRUE #initialize the selected coreline to TRUE
          CLCK[[ii]] <<- TRUE
          CoreLineCK[[ii]] <<- lapply(CoreLineNames[[ii]], function(x) {  #CoreLine[[ii]] contains just an element but Lapply creates the correct pointer
                                     CL_var <- tclVar(1) # 0 unchecked  1 checked
                                     SelCL[[ii]] <<- list(CL_var)
                                     CLboxBtn[[ii]] <<- ttkcheckbutton(CLframe[[ii]], text=as.character(x), variable=CL_var,
                                               command=function(){
                                                  ii <- as.integer(tcl(QNB, "index", "current"))+1 #ii+1= index of active notebook page starting from 0
                                                  CLCK[[ii]] <<- sapply(SelCL[[ii]], function(x) tclvalue(x)) #sapply needed to correctly make the tclvalue()
                                                  CLCK[[ii]] <<- as.logical(as.integer(CLCK[[ii]]))
                                                  CKHandlers()
                                               })
                                     tkgrid(CLboxBtn[[ii]], row=1, column=1, padx=5, pady=5)
                                 })
          CMPCK[[ii]] <<- ""
          if (NoComp == "FALSE"){   #Coreline has fit
              CMPframe[[ii]] <- ttklabelframe(Qgroup[[ii]], text=" COMPONENTS ", borderwidth=2)
              tkgrid(CMPframe[[ii]], row = 1, column = 2, padx=5, pady=5, sticky="w")

              SelCMP[[ii]] <<- list() #SelCMP[[ii]]is a list containing the pointers to fit_comp checkbox ON/OFF values for each coreline ii
              CMPCK[[ii]] <<- CoreLineComp[[ii]]  #initialize CMPCK setting all fitcomponents checked
              ComponentCK[[ii]] <<- lapply(seq_along(CoreLineComp[[ii]]), function(jj) {  #CoreLine[[ii]] contains just an element but Lapply creates the correct pointer
                                         txt <- unlist(CoreLineComp[[ii]][jj])
                                         CMP_var <- tclVar(1) # 0 unchecked  1 checked
                                         SelCMP[[ii]][jj] <<- list(CMP_var) #list() needed to correctly save CMP_var in SelCMP
                                         CMPboxBtn[[ii]] <<- ttkcheckbutton(CMPframe[[ii]], text=as.character(txt), variable=CMP_var,
                                               command=function(){
                                                  ii <- as.integer(tcl(QNB, "index", "current"))+1 #ii+1= index of active notebook page starting from 0
                                                  CMPCK[[ii]] <<- sapply(SelCMP[[ii]], function(x) tclvalue(x))
                                                  for(ll in length(CMPCK[[ii]]):1){
                                                      if(CMPCK[[ii]][ll] == "0"){
                                                         CMPCK[[ii]] <<- CMPCK[[ii]][-ll] #remove unchecked fit component
                                                      } else {
                                                         CMPCK[[ii]][ll] <<- CoreLineComp[[ii]][ll] #set checked fit components
                                                      }
                                                  }
                                                  CKHandlers()
                                               })
                                         tkgrid(CMPboxBtn[[ii]], row=jj, column=1, padx=5, pady=5)
                                 })
          }
          RSFframe[[ii]] <- ttklabelframe(Qgroup[[ii]], text=" RSF ", borderwidth=2)
          tkgrid(RSFframe[[ii]], row = 1, column = 3, padx=5, pady=5, sticky="w")

          RelSensFactCK[[ii]] <<- list()
          RSFCK[[ii]] <<- list()
          RSF[[ii]] <<- list()
          if (NoComp == "TRUE"){
              OldRSF <- XPSSample[[indx]]@RSF
              RSF[[ii]][1] <<- OldRSF
              RSF_var <- tclVar(OldRSF)  #sets the initial msg
              RSFCK[[ii]][1] <<- list(RSF_var)  #save the pointer to RSF_var
              RelSensFactCK[[ii]][1] <<- ttkentry(RSFframe[[ii]], textvariable=RSF_var, foreground="grey")
              tkgrid(unlist(RelSensFactCK[[ii]][1]), row = 1, column = 1, padx = 5, pady = 5, sticky="w")

              tkbind(unlist(RelSensFactCK[[ii]][1]), "<FocusIn>", function(K){
                         ii <- as.integer(tcl(QNB, "index", "current"))+1 #ii+1= index of active notebook page starting from 0
                         tclvalue(RSFCK[[ii]][1]) <- ""
                         tkconfigure(unlist(RelSensFactCK[[ii]][1]), foreground="red")
                     })
              tkbind(unlist(RelSensFactCK[[ii]][1]), "<Key-Return>", function(K){
                         ii <- as.integer(tcl(QNB, "index", "current"))+1 #ii+1= index of active notebook page starting from 0
                         tkconfigure(unlist(RelSensFactCK[[ii]][1]), foreground="black")
                            RSF[[ii]][1] <<- sapply(RSFCK[[ii]][1], function(x) tclvalue(x)) #sapply needed to correctly get tclvalue()
                            RSF[[ii]][1] <<- as.numeric(RSF[[ii]][1])
                     })
         } else {
               lapply(seq_along(CoreLineComp[[ii]]), function(jj){  #for(jj 1:LL){ does not work correctly
                   OldRSF <- XPSSample[[indx]]@Components[[jj]]@rsf
                   RSF[[ii]][jj] <<- OldRSF
                   RSF_var <- tclVar(OldRSF)  #sets the initial msg
                   RSFCK[[ii]][jj] <<- list(RSF_var)  #save the pointer to RSF_var
                   RelSensFactCK[[ii]][jj] <<- ttkentry(RSFframe[[ii]], textvariable=RSF_var, foreground="grey")
                   tkgrid(unlist(RelSensFactCK[[ii]][jj]), row = jj, column = 1, padx = 5, pady = 5, sticky="w")
                   tkbind(unlist(RelSensFactCK[[ii]][jj]), "<FocusIn>", function(K){
                            ii <- as.integer(tcl(QNB, "index", "current"))+1 #ii+1= index of active notebook page starting from 0
                            tclvalue(RSFCK[[ii]][jj]) <- ""
                            tkconfigure(unlist(RelSensFactCK[[ii]][jj]), foreground="red")
                        })
                   tkbind(unlist(RelSensFactCK[[ii]][jj]), "<Key-Return>", function(K){
                            ii <- as.integer(tcl(QNB, "index", "current"))+1 #ii+1= index of active notebook page starting from 0
                            tkconfigure(unlist(RelSensFactCK[[ii]][jj]), foreground="black")
                            RSF[[ii]][jj] <<- sapply(RSFCK[[ii]][jj], function(x) tclvalue(x)) #sapply needed to correctly get tclvalue()
                            RSF[[ii]][jj] <<- as.numeric(RSF[[ii]][jj])
                        })
               })
          }
      }

      CoreLineComp <<- setNames(CoreLineComp, CoreLineNames)   #the list contains the names of the corelines and relative FitComponents
      OrigCoreLinComp <<- CoreLineComp

      ii <- NCoreLines+1
      Qgroup[[ii]] <- ttkframe(QNB, borderwidth=0, padding=c(0,0,0,0) )
      tkadd(QNB, Qgroup[[ii]], text=" QUANTIFY ")

      QuantGroup <- ttkframe(Qgroup[[ii]], borderwidth=0, padding=c(0,0,0,0) )
      tkgrid(QuantGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

      QuantBtn <- tkbutton(QuantGroup, text=" QUANTIFY ", command=function(){
                           SetRSF()
                           #extract indexes of CoreLines selected for quantification
                           CheckedCL <- names(CoreLineComp)
                           CheckedCL <- CheckedCL[which(CheckedCL !="")]
                           CheckedCL <- sapply(CheckedCL, function(x) { x <- unlist(strsplit(x, "\\."))
                                                  x <- unname(x)
                                                  return(x)
                                               })
                           CheckedCL <- as.integer(CheckedCL[1,])
                           #Control on the Pass Energies
                           CK.PassE <- PassE[CheckedCL] #extracts PE valued corresponding to the elements selected for quantification
                           idx <- unname(which(CK.PassE != CK.PassE[1])) #are the selected elements acquired at different PE?
                           is.NC <- TRUE #logic, TRUE if NormCoeff is already computed
                           idx <- CheckedCL[idx]  #now idx correctly indicates the corelines
                           for(ii in idx){
                               if(length(XPSSample[[ii]]@RegionToFit) < 3){
                               is.NC <- FALSE}    #NormCoeff not saved in ...RegionToFit@NormCoeff
                           }
                           if (length(idx) > 0 && is.NC == FALSE) {  #selected elements are acquired at different PE and NormCoeff not computed
                               answ <- tkmessageBox(message=" Found spectra acquired at different Pass Energies. \n Do you want to quantify spectra EXTRACTED FROM SURVEY ?",
                                                    type="yesno", title="NORMALIZATION COEFFICIENT", icon="warning")
                               if (tclvalue(answ) == "yes"){
                               #Control how many Survey spectra in XPSSample
                                   SurIdx <<- grep("Survey", SpectList) #indexes of the names components == "Survey"
                                   if (length(SurIdx) == 0) {
                                       SurIdx <<- grep("survey", SpectList)
                                   }
                                   N_Survey <- length(SurIdx)

                                   if (N_Survey > 1) {
                                       txt <- paste(" Found ", N_Survey, " in the XPSSample ", activeFName, ": \n select the Survey used to extract the Core Lines", sep="")
                                       tkmessageBox(message=txt, title="SELECT SURVEY", icon="warning")
                                       CLwin <- tktoplevel()
                                       tkwm.title(CLwin,"SELECT SURVEY")
                                       tkwm.geometry(CLwin, "+200+200")   #position respect topleft screen corner
                                       CLgroup <- ttkframe(CLwin, borderwidth=0, padding=c(0,0,0,0) )
                                       tkgrid(CLgroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
                                       CL_Names <- SpectList[SurIdx]
                                       CLframe <- ttklabelframe(CLgroup, text = " SELECT SURVEY ", borderwidth=3)
                                       tkgrid(CLframe, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

                                       SS <- tclVar(0)
                                       for(ii in 1:length(CL_Names)){
                                           Radio <- ttkradiobutton(CLframe, text=CL_Names[ii], variable=SS, value=CL_Names[ii])
                                           tkgrid(Radio, row = ii, column = 1, padx = 5, pady = 5, sticky="w")
                                       }
                                       exitBtn <- tkbutton(CLgroup, text="  OK  ", width=15, command=function(){
                                                     SelectedSur <- tclvalue(SS)
                                                     SurIdx <<- grep(SelectedSur, CL_Names)
                                                     tkdestroy(CLwin)
                                           })
                                       tkgrid(exitBtn, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
                                       tkwait.window(CLwin)
                                   }
                                   cat("\n => Compute the normalization coefficient")
                                   CalcNormCoeff(CheckedCL, CK.PassE, SurIdx)
                                   if (NormCoeff == -1){  #Found wide spectrum and corelines acquired using different energy units.
                                       return()
                                   }
                                } else {
                                   tkmessageBox(message="Spectra acquired at different Pass Energies cannot be used for quantification", title="QUANTIFICATION NOT ALLOWED", icon="warning")
                                   return()
                                }
                           }
                           quant(CoreLineComp)
                 })
      tkgrid(QuantBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
#      ww <- as.numeric(tkwinfo("reqwidth", QuantBtn)) + 15
      
      ClipBrdBtn <- tkbutton(QuantGroup, text=" COPY TO CLIPBOARD ", command=function(){
                           CBrd1 <<- data.frame(A="File Name: ", B=XPSSample@Filename, stringsAsFactors=FALSE)
                           CBrd1 <<- rbind(CBrd1, c(" ", " "))
                           rownames(CBrd1) <<- NULL
                           colnames(CBrd1) <<- NULL
                           rownames(CBrd2) <<- NULL
                           colnames(CBrd2) <<- c("Comp.", "Area(cps)", "FWHM", "RSF", "BE(eV)", "TOT.(%)")
                           for(ii in 1:length(CBrd2)){
                               NChr <- max(nchar(CBrd2[[ii]]))
                               CBrd2[[ii]] <<- encodeString(CBrd2[[ii]], width=NChr, quote="", justify=c("right"))
                               if (NChr < 6 || is.na(NChr)) { #NChr <- 5 }
                                   CBrd2[[ii]] <- paste(CBrd2[[ii]],"", sep="\t")
                               }
                           }

                           ClipBoard <- file(description="clipboard")
                           open(ClipBoard, "w")
                           write.table(CBrd1, file=ClipBoard, append=TRUE, eol="\n", sep="\t", na="    ",
                                       dec=".", quote=FALSE, row.names=FALSE, col.names=TRUE )
                           write.table(CBrd2, file=ClipBoard, append=TRUE, eol="\n", sep="   ", na="    ",
                                       dec=".", quote=FALSE, row.names=FALSE, col.names=TRUE )
                           flush(ClipBoard)
                           close(ClipBoard)
                           cat("\n Table copied to clipboard")
                 })
      tkgrid(ClipBrdBtn, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
#      ww <- ww + as.numeric(tkwinfo("reqwidth", ClipBrdBtn)) + 15

      WriteBtn <- tkbutton(QuantGroup, text=" WRITE TO FILE ", command=function(){
                                          Filename <- unlist(strsplit(QTabTxt[2], ":"))[2]
                                          Filename <- unlist(strsplit(Filename, "\\."))[1]
                                          Filename <- paste(Filename, "txt", sep="")
                                          PathFile <- tkgetSaveFile(initialdir = getwd(),
                                                          initialfile = Filename, title = "SAVE FILE")
                                          DirName <- dirname(PathFile)
                                          NL <- length(QTabTxt)
                                          if (is.null(PathFile)) {
                                              tkmessageBox(message="File Name not defined. Please give the file name", title="NO FILENAME", icon="error")
                                              return()
                                          }
                                          OutFile <- file(PathFile, open="wt")
                                          for (ii in 1:NL){
                                               writeLines(QTabTxt[ii], sep="", OutFile)
                                          }
                                          close(OutFile)
                 })
      tkgrid(WriteBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

      MyFont <- paste(get("XPSSettings", envir=.GlobalEnv)[[1]][1],
                       get("XPSSettings", envir=.GlobalEnv)[[1]][3],
                       get("XPSSettings", envir=.GlobalEnv)[[1]][2], sep=" ")
      QTable <<- tktext(Qgroup[[ii]], height=20, width=65, font=MyFont)
      tkgrid(QTable, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
      tkgrid.columnconfigure(Qgroup[[ii]], 1, weight=1)
      addScrollbars(Qgroup[[ii]], QTable, type="y", Row = 2, Col = 1, Px=0, Py=0)
      tkgrid.columnconfigure(Qgroup[[ii]], 0, weight=1)
      return(CoreLineComp)
   }


#----Reset vars resets variables to initial values
   ResetVars <- function(){
      XPSSample <<- get(activeFName, envir = .GlobalEnv)   #load the active XPSSample dataFrame
      XPSSampleList <<- XPSFNameList()                     #list of all XPSSamples
      XPSSampleIdx <<- grep(activeFName,XPSSampleList)
      SpectList <<- XPSSpectList(activeFName)          #list of all CoreLines of the active XPSSample
      PassE <<- RetrivePE() # Retrieve list of CoreLine Pass Energies
      NormCoeff <<- 1

      CoreLineNames <<- ""
      CoreLineIndx <<- NULL
      FitComp <<- ""
      NComp <<- NULL
      CoreLineComp <<- list()
      OrigCoreLinComp <<- list()
      CLChecked <<- NULL
      CompChecked <<- NULL

      SurIdx <<- NULL
      CoreLineCK <<- list()     #define lists for the Gwidget pointers
      ComponentCK <<- list()
      RelSensFactCK <<- list()
      CLCK <<- list()
      SelCL <<- list()
      CMPCK <<- list()
      SelCMP <<- list()
      QTable <<- list()
      CBrd1 <<- NULL
      CBrd2 <<- NULL
      RSFCK <<- list()
      RSF <<- list()

      QNB <<- list()
      Qgroup <<- list()
      CLframe <<- list()
      CMPframe <<- list()
      RSFframe <<- list()
      CLboxBtn <<- list()
      CMPboxBtn <<- list()

      NCoreLines <<- length(SpectList)
      NmaxFitComp <<- 0
      QTabTxt <<- ""    #text containing Quantification results for the Qtable gtext()
      TabTxt <<- ""     #text containing Quantification results for the RStudio consolle

      RegionToFit <- 0
      jj <- 1
      for(ii in 1:NCoreLines){
         if (length(XPSSample[[ii]]@RegionToFit) > 0){ #a Baseline is defined
            RegionToFit <- 1
            CoreLineNames[jj] <<- SpectList[ii]  #Save the coreline name where a baseline is defined
            CoreLineIndx[jj] <<- ii              #vector containing indexes of the corelines where a baseline is defined
            jj <- jj+1
            NFC <- length(XPSSample[[ii]]@Components)
            if (NFC > NmaxFitComp) {NmaxFitComp <<- NFC}
         }
      }
      if (RegionToFit == 0){
         tkmessageBox(message="WARNING: NO FIT REGIONS DEFINED ON THIS XPS SAMPLE", title = "WARNING", icon = "warning")
         return()
      }
      NCoreLines <<- length(CoreLineIndx) #now only the corelines with baseline are considered for the quantification
      CLChecked <<- rep(TRUE,NCoreLines)
   }


#---- variables ----
   if (is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }

   XPSSample <- NULL
   XPSSample <- get(activeFName, envir = .GlobalEnv)   #load the active XPSSample
   XPSSampleList <- XPSFNameList()                     #list of all XPSSamples
   XPSSampleIdx <- grep(activeFName,XPSSampleList)
   SpectList <- XPSSpectList(activeFName)              #list of all the corelines of the active XPSSample
   PassE <- RetrivePE()                                # Retrieve list of CoreLine Pass Energies
   names(PassE) <- SpectList
   NormCoeff <- 1
   NCoreLines <- length(SpectList)
   MaxNComp <- max(sapply(XPSSample, function(x) length(x@Components)))

   CoreLineNames <- ""
   CoreLineIndx <- NULL
   FitComp <- ""
   NComp <- NULL
   CoreLineComp <- list()
   OrigCoreLinComp <- list()
   CLChecked <- NULL
   CompChecked <- NULL

   SurIdx <- NULL
   CoreLineCK <- list()     #define lists for the Gwidget pointers
   ComponentCK <- list()
   RelSensFactCK <- list()
   CLCK <- list()
   SelCL <- list()
   CMPCK <- list()
   SelCMP <- list()
   QTable <- list()
   CBrd1 <- NULL
   CBrd2 <- NULL

   RSFCK <- list()
   RSF <- list()

   QNB <- list()
   Qgroup <- list()
   CLframe <- list()
   CMPframe <- list()
   RSFframe <- list()
   CLboxBtn <- list()
   CMPboxBtn <- list()

   NmaxFitComp <- 0
   QTabTxt <- ""    #text containing Quantification results for the Qtable gtext()
   TabTxt <- ""     #text containing Quantification results for the RStudio consolle

   RegionToFit <- 0
   jj <- 1
   for(ii in 1:NCoreLines){ #Baseline MUST be defined for the quantufucation
      if (length(XPSSample[[ii]]@RegionToFit) > 0){ #a baseline is defined
         RegionToFit <- 1
         CoreLineNames[jj] <- SpectList[ii]  #Save the coreline name where a baseline is defined
         CoreLineIndx[jj] <- ii              #vector containing indexes of the corelines where a baseline is defined
         jj <- jj+1
         NFC <- length(XPSSample[[ii]]@Components)
         if (NFC > NmaxFitComp) {NmaxFitComp <- NFC}
      }
   }
   if (RegionToFit == 0){
      tkmessageBox(message="WARNING: NO Fit, NO BaseLine found in this XPS Sample", title = "WARNING", icon = "warning")
      return()
   }
   NCoreLines <- length(CoreLineIndx) #now only the corelines with baseline are considered for the quantification
   CLChecked <- rep(TRUE,NCoreLines)



#===== GUI =====
   Qwin <- tktoplevel()
   tkwm.title(Qwin," QUANTIFICATION FUNCTION ")
   tkwm.geometry(Qwin, "+100+50")   #position respect topleft screen corner
#tkwm.geometry(Qwin, "570x500")
   QmainGroup <- ttkframe(Qwin, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(QmainGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   Qgrp0 <- ttkframe(QmainGroup, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(Qgrp0, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
   tkgrid( ttklabel(Qgrp0, text="XPSSample"),
           row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   XS <- tclVar(activeFName)
   SelXPSData <- ttkcombobox(Qgrp0, width = 15, textvariable = XS, values = XPSSampleList)
   tkbind(SelXPSData, "<<ComboboxSelected>>", function(){
                             activeFName <<- tclvalue(XS)
                             assign("activeSpectIndx", 1, envir=.GlobalEnv)
                             assign("activeSpectName", SpectList[1], envir=.GlobalEnv)
                             tkdestroy(QNB)
                             ResetVars()
                             QNB <<- ttknotebook(Qframe)
                             tkgrid(QNB, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
                             CoreLineComp <- MakeNb()
                             plot(XPSSample)
                 })
   tkgrid(SelXPSData, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   QReset <- tkbutton(Qgrp0, text=" RESET ", width=12, command=function(){
                             ResetVars()
                             tkdestroy(QNB)
                             QNB <- ttknotebook(Qframe)
                             tkgrid(QNB, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
                             CoreLineComp <- MakeNb()
                             plot(XPSSample)
                 })
   tkgrid(QReset, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

   QSave <- tkbutton(Qgrp0, text=" SAVE ", width=12, command=function(){
                             SetRSF()
                             assign(activeFName, XPSSample, .GlobalEnv)  #save the fit parameters in the activeSample
                             XPSSaveRetrieveBkp("save")
                 })
   tkgrid(QSave, row = 1, column = 4, padx = 5, pady = 5, sticky="w")

   QSaveExit <- tkbutton(Qgrp0, text=" SAVE & EXIT ", width=12, command=function(){
                             SetRSF()
                             assign(activeFName, XPSSample, .GlobalEnv)  #save the fit parameters in the activeSample
                             tkdestroy(Qwin)
                             XPSSaveRetrieveBkp("save")
                 })
   tkgrid(QSaveExit, row = 1, column = 5, padx = 5, pady = 5, sticky="w")

   Qframe <- ttklabelframe(QmainGroup, text = "Core Lines", borderwidth=2)
   tkgrid(Qframe, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
   QNB <- ttknotebook(Qframe)
   tkgrid(QNB, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   CoreLineComp <- MakeNb()

   CoreLineComp <- setNames(CoreLineComp, CoreLineNames)   #the list contains the names of the coreline and the relative FitComponents
   OrigCoreLinComp <- CoreLineComp
   tkwait.window(Qwin)
}
