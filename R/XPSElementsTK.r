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

#'@title showTableElement
#'@description showTableElement() function extract the values of
#'  BE, KE, RSF for all the orbitals of a given chemical element
#'  showTableElement is internal not intended for the user.
#'@param element element symbol as character
#'@param analyzer name of reference list
#'@return Return values named by \code{what}.
#'@export
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
