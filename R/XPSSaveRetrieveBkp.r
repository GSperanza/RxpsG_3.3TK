#function to save/retrieve a backup copy of the analyzed data

#' @title XPSSaveRetrieveBkp
#' @description XPSSaveRetrieveBkp function to save/retrieve a backup
#'   copy of the analyzed dataT in the Hard Disk
#'   All the loaded and analyzed object of class XPSSample are automatically
#'   saved in an unique ...R/library/RxpsG/extdata/BkpData.Rdata file.
#'   In case of program crash, data recovery can be done by selecting
#'   the 'Retrieve Bkp-data' option of RxpsG.
#' @param opt = "save" or "retrieve" to save a backup file or retrieve data
#'   from the backup file
#' @export
#'

XPSSaveRetrieveBkp <- function(opt){

#--- get System info and apply correspondent XPS Settings ---
   Bkp.pthName <- system.file("extdata/BkpData.Rdata", package="RxpsG", lib.loc=.libPaths())
   if (file.exists(Bkp.pthName) == FALSE) {
       tkmessageBox(message="ATTENTION: XPSSettings.ini file is lacking.\n Check the installation of the RxpsG package", title = "WARNING", icon = "warning" )
       return()
   }

   switch(opt,
       "save"={
          FNameList <- XPSFNameList() #read the list of XPSSample loaded in the .GlobalEnv
          save(list=FNameList, file=Bkp.pthName, compress=TRUE)   #save all the XPSSamples
       },
       "retrieve"={
          FNameList <- load(Bkp.pthName,envir=.GlobalEnv)   #load the data in the .GlobalEnv not in the local memory
          assign("activeFName", FNameList[1], envir=.GlobalEnv)                                                  #assign not necessary
       } )

}
