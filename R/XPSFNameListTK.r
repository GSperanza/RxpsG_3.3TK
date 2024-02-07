#Generates the list of the XPS-Sample loaded in the .GlobalEnv

#' @title XPSFNameList
#' @description Provide a list of the XPS-Sample names loaded
#'   Provide a list of names (strings) relative to the XPS-Samples
#'   loaded in the XPS-Analysis software. No parameters are passed to this function.
#' @param warn logical enables warning messages
#' @examples
#' \dontrun{
#' 	XPSFNameList()
#' }
#' @export
#'


XPSFNameList <- function(warn=TRUE){
   FNameList=NULL
   ClassFilter <- function(x) inherits(get(x), "XPSSample" ) #sets the "XPSSample" as the class to filter to select XPSSample Data files
   FNameList <- Filter(ClassFilter, ls(.GlobalEnv)) #list of XPSSample files loaded in .GlobalEnv
   if (length(FNameList) == 0){
      FNameList <- NULL
      if (warn==TRUE) {
          ErMsg <- tkmessageBox(message="   Cannot Find XPS Samples. Load XPS Data Please!    ", title="WARNING", icon="warning")
      }
   } else {
      LL <- length(FNameList)
      sapply(FNameList, function(x) {  #eliminates blank spaces from XPS names
                   x <- paste(unlist(strsplit(x, " ")), sep="")
            })
   }
   return(FNameList)
}
