# creates the list of spectra associated to an XPSSample

#' @title XPSSpectList
#' @description XPSSpectList shows the list of spctra related to
#'   object of class XPSSample. The spectra are identified by their
#'   an index or by the CoreLine name
#' @param SelectedFName the name (string) of the selected XPSSample
#' @param noIdx logical if FALSE a numerical index is added to any CoreLine name
#' @examples
#' \dontrun{
#' 	XPSSampleInfo(SelectedFName, noIdx=FALSE)
#' }
#' @export
#'


XPSSpectList <- function(SelectedFName, noIdx=FALSE) {
     if (length(SelectedFName)==0 || is.na(SelectedFName) || is.null(SelectedFName)){
        tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
        return()
     }
     FName <- get(SelectedFName, envir=.GlobalEnv)
     CoreLineList=""
     CoreLineList <- FName@names
     LL <- length(CoreLineList)
     if (noIdx==FALSE){
        for (ii in 1:LL){
               CoreLineList[ii] <- paste(ii,".",CoreLineList[ii], sep="")   #Add number at beginning of the corelines to identify spectra with saame name
        }
     }
     return(CoreLineList)
}


