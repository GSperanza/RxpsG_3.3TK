## Construction of Elements tables

#' @title ElementCheck
#' @description ElementCheck() checks if an element is present in the PeriodicTableElements
#'   ElementCheck is is an internal function not intended for the user
#' @param element character = element to check if present in the Periodic Table
#' @export


ElementCheck <- function(element) {  # chiamato da XPSsetRSF
	   PeriodicTableElements <- c("H", "He", "Li", "Be", "B", "C", "N", "O", "F",
         "Ne", "Na", "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc",
         "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As",
         "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh",
         "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe", "Cs", "Ba", "La",
         "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm",
         "Yb", "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl",
         "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th")

	   return(ifelse(element %in% PeriodicTableElements, TRUE, FALSE))
}


## Initialize Elements for Kratos or Scienta dataFiles

#' @title IniElement
#' @description IniElement() provides a table for each element of the Periodic Table.
#'   The table contains the element-orbitals ordered in increasing Binding Energy
#'   but also the relative Kinetic energy is shown together with the orbital RSF
#'   IniElement is is an internal function not intended for the user
#'   it is called by XPSSurveyElementIdentify() and XPSElemTab() functions
#' @param element = character element to initialize


IniElement <- function(element) {

# -- search for the element table
  fp <- system.file("extdata/CoreLinesTable.lib", package="RxpsG")
  if ( fp == "" ) {
      txt <- paste("\n ==> ATTENTION: File CoreLinesTable.lib not found! \n
      ==> Check correct installation of RxpsG package\n
      ==> Check if ...R/Library/RxpsG/data directory is present")
      return(invisible(1))
  } else if (file.exists(fp)) {
    # -- now read element table
    ElmtList <- scan(fp, skip=0, sep="", what = list("character", "character", "numeric", "numeric", "numeric", "numeric"), quiet=TRUE)
    ElmtList[[3]] <- as.numeric(ElmtList[[3]])
    ElmtList[[4]] <- as.numeric(ElmtList[[4]])
    ElmtList[[5]] <- as.numeric(ElmtList[[5]])
    ElmtList[[6]] <- as.numeric(ElmtList[[6]])
    
    ElmtList <- as.data.frame(ElmtList, stringsAsFactors = FALSE) # set it to FALSE
    names(ElmtList) <- c("Element", "Orbital", "BE", "KE", "RSF_K", "RSF_S")
    
    ## search for element in the Element Table
    idx <- which(ElmtList[,"Element"] == element)
    return(ElmtList[idx,]) #Observe the comma to return all elements of ElmtList at rows==idx
  } else {
    tkmessageBox(message="ATTENTION: CoreLinesTable file not Found. Check RxpsG package installation", title = "WARNING",icon = "warning" )
    return()
  }
}  

#' @title getElementValue
#' @description getElementValue function is used to to access and get values from the
#'   element libraries supplied for Scienta and Kratos instruments. This function
#'   is internal not intended for the user.
#' @param element character = element symbol as character
#' @param orbital character = atom orbital
#' @param analyzer character = name of reference instrument dependent table of values
#' @param what character = name of value to get. Default \code{"RSF"}
#' @return getElementValue returns values named by \code{what}.
#'

getElementValue <- function(element, orbital, analyzer=c("scienta", "kratos"), what="RSF") {  # chiamato da XPSsetRSF

   tmp <- IniElement(element) #extract the desired element from the CoreLine table
   if (length(tmp) == 0){
       txt <- paste("Warning: element ",element," or relative orbital not found!")
       tkmessageBox(message=txt, title="WARNING", icon="warning")
       return(NULL)
   }
   idx <- grep(orbital, tmp[,"Orbital"]) #select the desired orbital
   if (length(idx) == 0) {
       txt <- paste("Orbital ", orbital, " not found in element ", element, sep="")
       tkmessageBox(message=txt, title="WARNING", icon="warning")
       return(NULL)
   } else {
       tmp <- tmp[idx, ]             #select the desired orbital among the other of the chosen element
       if (analyzer == "scienta") {  #in the CoreLine Table select the line corresponding to Scienta
           idx<-which(is.na(tmp[,"RSF_S"])==TRUE)
           if (length(idx) > 0 ){tmp <- tmp[-idx ,]}  #drop rows with NotAssigned Scienta RSF (in this line Kratos values are set)
           tmp <- tmp[,-5]           #drop the column with Kratos RSF_K
       } else if (analyzer == "kratos") {   #in the CoreLine Table select the line corresponding to Kratos
           idx<-which(is.na(tmp[,"RSF_K"])==TRUE)
           if (length(idx) > 0 ){tmp <- tmp[-idx ,]}  #drop rows with NotAssigned Kratos RSF  in this line Scienta values are set)
           tmp <- tmp[,-6]           #drop the column with Scienta RSF_S
       }
   }
   names(tmp) <- c("Element", "Orbital", "BE", "KE", "RSF")
   what <- match.arg(what, colnames(tmp))
   return(tmp[,what])
}

#' @title showTableElement
#' @description showTableElement() function extract the values of
#'   BE, KE, RSF for all the orbitals of a given chemical element
#'   showTableElement is internal not intended for the user.
#' @param element element symbol as character
#' @param analyzer name of reference list
#' @return Return values named by \code{what}.
#' @export
#'

showTableElement <- function(element, analyzer=c("scienta", "kratos")) {   # called by da XPSsetRSF
 tmp <- IniElement(element)           #extract the desired element from the CoreLine table
 analyzer <- match.arg(analyzer)
 switch(analyzer,                     #in the CoreLine analyzer select the lline corresponding to Scienta or Kratos
        "scienta" = tmp <- tmp[,-5],    #drop the column with Kratos RSF
        "kratos"  = tmp <- tmp[,-6]   #drop the column with Scienta RSF
 )
 names(tmp) <- c("Element", "Orbital", "BE", "KE", "RSF")
 cat("\n Element:  ", tmp[1,"Element"])
 orbit <- sapply(tmp[,"Orbital"], function(x) sprintf("%-7s",x), USE.NAMES = FALSE)
 cat("\n Orbitals: ", orbit)
 BE <- sapply(tmp[,"BE"], function(x) sprintf("%-7s",x))
 cat("\n BE:       ", BE)
 KE <- sapply(tmp[,"KE"], function(x) sprintf("%-7s",x))
 cat("\n KE:       ", KE)
 rsf <- sapply(tmp[,"RSF"], function(x) sprintf("%-7s",x))
 cat("\n RSF:      ", rsf)
   table<-list()
   table$Element<-tmp[,"Element"]
   table$Orbital<-orbit
   table$BE<-BE
   table$KE<-KE
   table$RSF<-rsf
   return(table)
}


#' @title XPSDefineRSF
#' @description XPSDefineRSF() function select the RSF for the
#'   selected Core.Line. Given the name of the Core.Line (C1s, Cu2p, Au4f...)
#'   XPSSetRSF() separate the Element name and the Orbital. Then
#'   the relative RSF is obtained from tabulated data.
#'   If more than one value is found a GUI is generated to manually select the RSF
#'   If no Object is specified the active Core.Line is taken
#'   If no Symbol is specified the activeSpectName is taken
#' @param Object of class XPSCoreLine, the selected Core.Line
#' @param Symbol of class character, the name of the selected Core.Line
#' @examples
#'   \dontrun{
#'   XPSDefineRSF(XPSSample[["W4f"]], "W4f")
#'   XPSDefineRSF(XPSSample[[2]], "C1s")
#'   XPSDefineRSF()
#' }
#' @export
#'

#Example: Au  Element$Orbital is:
#    Element Orbital  BE     KE RSF_K RSF_S
#57       Au   5p3/2  57 1429.6    NA  0.00
#79       Au   5p1/2  74 1412.6    NA  0.00
#90       Au   4f7/2  84 1402.6 6.250  9.58
#96       Au   4f5/2  87 1399.6    NA  7.54
#124      Au      5s 110 1376.6 0.000  0.00
#298      Au   4d5/2 335 1151.6 4.841  0.00
#310      Au   4d3/2 353 1133.6 0.000  0.00
#412      Au   4p3/2 546  940.6 3.166  2.14
#443      Au   4p1/2 643  843.6 0.000  0.00
#475      Au      4s 759  727.6 0.409  1.92

XPSDefineRSF <- function(Object=NULL, Symbol=NULL){

     MakeRSFTbl <- function(){
        Element <<- as.data.frame(Element)
        RSFWindow <- tktoplevel()
        tkwm.title(RSFWindow,"XPS ANALYSIS")
        tkwm.geometry(RSFWindow, "+100+50")   #position respect topleft screen corner

        MainGroup <- ttkframe(RSFWindow, borderwidth=0, padding=c(0,0,0,0) )
        tkgrid(MainGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

        TblFframe <- ttklabelframe(MainGroup, text = paste("Parameneter of Element: ", Symbol, sep=""), borderwidth=5)
        tkgrid(TblFframe, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

        XPSTable(TblFframe, Element, NRows=NR, ColNames=ElmtNames, Width=WW)

        RSFFframe <- ttklabelframe(MainGroup, text = RSFFrameName, borderwidth=5)
        tkgrid(RSFFframe, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

        tkgrid( ttklabel(RSFFframe, text="RSF for: "),
                row = 1, column = 1, padx = 5, pady = 1, sticky="we")
        tkgrid( ttklabel(RSFFframe, text=Symbol, font="Sans 13"),
                row = 1, column = 2, padx = c(0, 5), pady = 1, sticky="we")

        if (NR > 1){    #gradio works with at least 2 items
            RSF <<- tclVar()
            for(ii in 1:length(ElmtRSF)){
                RsfPtr <- ttkradiobutton(RSFFframe , text=ElmtRSF[ii], variable=RSF,
                                         value=ElmtRSF[ii], command=function(){
                                             Object@RSF <<-  as.numeric(tclvalue(RSF))
                                             if (hasComponents(Object)){
                                                 NFC <- length(Object@Components)
                                                 for(ii in 1:NFC){
                                                     Object@Components[[ii]]@rsf <<- as.numeric(tclvalue(RSF))
                                                 }
                                             }
                               })
                tkgrid(RsfPtr, row = ii+2, column = 1, padx = 5, pady = 3, sticky="w")
            }
            tclvalue(RSF) <- ""
        }

        if (NR == 1){    #gradio works with at least 2 items
            RSF <<- tclVar(FALSE)
            RsfPtr <<- tkcheckbutton(RSFFframe, text=ElmtRSF[1], variable=RSF, onvalue = ElmtRSF[1],
                                     offvalue = 0, command=function(){
                                             Object@RSF <<-  as.numeric(tclvalue(RSF))
                                             if (hasComponents(Object)){
                                                 NFC <- length(Object@Components)
                                                 for(ii in 1:NFC){
                                                     Object@Components[[ii]]@rsf <<- as.numeric(tclvalue(RSF))
                                                 }
                                             }
                               })
                tkgrid(RsfPtr, row = 3, column = 1, padx = 5, pady = 3, sticky="w")
          }

        exitBtn <- tkbutton(MainGroup, text="   SAVE & EXIT   ", command=function(){
                                             tkdestroy(RSFWindow)
                               })
        tkgrid(exitBtn, row = 2, column = 2, padx = 5, pady = 5, sticky="e")

        tkwait.window(RSFWindow)
     }

#----- Variables -----
     Symbol <- NULL
     Elmt <- NULL
     Orbital <- NULL
     ElmtNames <- NULL
     WW <- NULL
     NR <- NULL
     NC <- NULL
     NO_CL <- FALSE

#--- Controls on Core.Line argument

     if (is.null(Object)){
         if (is.null(activeFName) || is.na(activeFName) || length(activeFName) == 0){
             tkmessageBox(message="Please Select a Core.Line Please", title="WARNING", icon="warning")
             return()
         }
         XPSSample <- get(activeFName, envir=.GlobalEnv)
         Object <- XPSSample[[activeSpectIndx]]   #this to set the RSF in the current Core.Line
         Symbol <- activeSpectName
         Object@Symbol <- Symbol
         NO_CL <- TRUE
     }
     Symbol <- Object@Symbol
     if (is.null(Symbol) || is.na(Symbol) || length(Symbol) == 0){
         Symbol <- activeSpectName
     }
     Object@Symbol <- Symbol

     Symbol <- gsub(" ","", Symbol) #eliminates blank spaces if present: C 1s -> C1s
     Elmt <- strsplit(Symbol, "(?<=[A-Za-z])(?=[0-9])", perl = TRUE)[[1]][1]
     Orbital <- strsplit(Symbol, "(?<=[A-Za-z])(?=[0-9])", perl = TRUE)[[1]][2]
     Element <- IniElement(Elmt)
     ElmtNames <- names(Element)
#---
     RSFFrameName <- "Select Kratos RSF"
     ElmtRSF <- Element$RSF_K
     if (Object@Flags[3] == TRUE) {
         RSFFrameName <- "Select Scienta RSF"
         ElmtRSF <- Element$RSF_S
     }
     ElmtRSF <- sapply(ElmtRSF, function(z){
                                  if(is.na(z)) z <- 0
                                  return(z)
                                })
     idx <- grep(Orbital, Element$Orbital)
     if (length(idx) == 0){
         tkmessageBox(paste("Warning: No ", Orbital, " Found for Element ", Elmt,
                            "\nPlease Control Selected Core.Line Name", sep=""))
         return()
     }
     ElmtRSF <- ElmtRSF[idx]
     LL <- length(ElmtRSF)
     if (LL == 1) {
         Object@RSF <- ElmtRSF
         if (hasComponents(Object)){
             NFC <- length(Object@Components)
             for(ii in 1:NFC){
                 Object@Components[[ii]]@rsf <- ElmtRSF
             }
         }
         if (NO_CL == TRUE){
             XPSSample[[activeSpectIndx]] <- Object
             if (hasComponents(XPSSample[[activeSpectIndx]])){
                 NFC <- length(XPSSample[[activeSpectIndx]]@Components)
                 for(ii in 1:NFC){
                     XPSSample[[activeSpectIndx]]@Components[[ii]]@rsf <- ElmtRSF
                 }
             }
             assign(activeFName, XPSSample, envir=.GlobalEnv)
         } else {
             return(Object)
         }
     }
     if (length(unique(ElmtRSF)) == 1) { #all values of ElmtRSF are equal
         Object@RSF <- ElmtRSF[1]
         if (hasComponents(Object)){
             NFC <- length(Object@Components)
             for(ii in 1:NFC){
                 Object@Components[[ii]]@rsf <- ElmtRSF[1]
             }
         }
         if (NO_CL == TRUE){
             XPSSample[[activeSpectIndx]] <- Object
             if (hasComponents(XPSSample[[activeSpectIndx]])){
                 NFC <- length(XPSSample[[activeSpectIndx]]@Components)
                 for(ii in 1:NFC){
                     XPSSample[[activeSpectIndx]]@Components[[ii]]@rsf <- ElmtRSF[1]
                 }
             }
             assign(activeFName, XPSSample, envir=.GlobalEnv)
         } else {
             return(Object)
         }
     }

#--- if LL > 1 and ElmtRSF contains different RSF make widget
     WW <- c(70, 70, 50, 50, 50, 50)
     NR <- length(Element[[1]])
     NC <- length(ElmtNames)
     if (NC == 0){
         tkmessageBox(paste("Warning: No ", Elmt, " in Tabulated Elements. Please Control.", sep=""),
                      title="WARNING", icon="warning")
         return()
     }
     if (length(unique(ElmtRSF)) > 1) { #different RSF values found for selected element orbital
         MakeRSFTbl()
         if (NO_CL == TRUE){
             XPSSample[[activeSpectIndx]] <- Object
             assign(activeFName, XPSSample, envir=.GlobalEnv)
         } else {
             return(Object)
         }
     }

}