#This routine allows selection of the spectra to save in ASCII text file
# and the output Format (header, separators...)

#' @title XPSExportAscii
#' @description XPSExportAscii GUI for the selection of the more convenient
#'   style to export single and all the XPSCoreLines in a XPSSample in anASCII 
#'   text file.
#' @examples
#' \dontrun{
#'	 XPSexportASCII()
#' }
#' @export
#'


XPSExportAscii <- function(){

   writeData <- function(Fdata, filename, fmt) {   #write data following the format fmt
		                   switch(fmt,
                             "Raw" = { write.table(Fdata, file = filename, sep=" ", eol="\n",
                                                     dec=".", row.names=FALSE, col.names=TRUE)},
                             "x.x  x.x" = { write.table(Fdata, file = filename, row.names=FALSE, col.names=TRUE)},
                             "x.x, x.x" = { write.csv(Fdata, file = filename, col.names=TRUE)},
                             "x,x; x,x" = { write.csv2(Fdata, file = filename, col.names=TRUE)}
                         )
                     }


#--- variabili ---
   if (is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FNameList <- XPSFNameList()   #get the list of all XPSSamples in .GlobalEnv
   OutFormat <- c("Raw", "x.x  x.x", "x.x, x.x", "x,x; x,x")
   FName <- NULL
   SpectList <- " "
   FData <- NULL
   filename <- NULL
   SpectName <- NULL
#   fmt <- NULL


#--- GUI ---
   EAwin <- tktoplevel()
   tkwm.title(EAwin,"EXPORT ASCII")
   tkwm.geometry(EAwin, "+100+50")

   EAgroup1 <- ttkframe(EAwin, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(EAgroup1, row=1, column=1, padx = 0, pady = 0, sticky="w")

   EAframe1 <- ttklabelframe(EAgroup1, text = "Select the XPSSample", borderwidth=3)
   tkgrid(EAframe1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   XS <- tclVar()
   EAobj1 <- ttkcombobox(EAframe1, width = 25, textvariable = XS, values = FNameList)
   tkbind(EAobj1, "<<ComboboxSelected>>", function(){
                         ActiveFName <- tclvalue(XS)
                         FName <<- get(ActiveFName, envir = .GlobalEnv)
                         SpectList <<- XPSSpectList(ActiveFName)
                         SpectList <<- c("All", SpectList) #all option added to save all the XPS Corelines
                         LL <- length(SpectList)
                         spectName <- tmp <- NULL
                         NRow <- ceiling(LL/5) #ii runs on the number of columns
                         for(ii in 1:NRow){
                             NN <- (ii-1)*7    #jj runs on the number of column_rows
                             for(jj in 1:7) {
                                 if ((jj+NN) > LL) {break} #exit loop if all FitComp are in RadioBtn
                                 EAobj2 <- tkcheckbutton(EAframe2, text=SpectList[jj+NN], variable=SpectList[jj+NN], onvalue = SpectList[jj+NN], offvalue = 0,
                                           command=function(){
                                              SpectName <<- sapply(SpectList, function(x) tclvalue(x))
                                              if (SpectName[1] == "All"){
                                                  plot(FName)
                                                  SpectName <<- SpectName[1]
                                              } else {
                                                  SpectName <<- intersect(SpectName, SpectList) #drop the zeros
                                                  if (length(SpectName) > 0){
                                                      tmp <- new("XPSSample")
                                                      for(kk in 1:length(SpectName)){
                                                          CL <- unlist(strsplit(SpectName[kk], "\\."))
                                                          CL <- as.numeric(CL[1])
                                                          tmp[[kk]] <- FName[[CL]]
                                                      }
                                                      plot(tmp)
                                                  }
                                              }
                                              WidgetState(EAframe2, "normal")
                                              WidgetState(EAframe3, "normal")
                                          })
                                 tclvalue(SpectList[jj+NN]) <- FALSE   #initial cehckbutton setting
                                 tkgrid(EAobj2, row = ii, column = jj, padx = 5, pady=5, sticky="w")
                             }
                         }
                 })
   tkgrid(EAobj1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   EAframe2 <- ttklabelframe(EAgroup1, text = "Select the CoreLines", borderwidth=3)
   tkgrid(EAframe2, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
   tkgrid( ttklabel(EAframe2, text="     "),  #just to create space inside the EAframe2
           row = 1, column = 1, padx = 5, pady = 5, sticky="w")


   EAframe3 <- ttklabelframe(EAgroup1, text = "Data to Save", borderwidth=3)
   tkgrid(EAframe3, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

   SpectToSave <- tclVar()
   SpectData <- c("Spectrum+Fit", "Spectrum Only")
   for(ii in 1:2){
       EAobj3 <- ttkradiobutton(EAframe3, text=SpectData[ii], variable=SpectToSave, value=ii,
                     command=function(){
                         WidgetState(EAobj4, "normal")
                 })
       tkgrid(EAobj3, row = 1, column = ii, padx=5, pady=5, sticky="w")
   }

   EAframe4 <- ttklabelframe(EAgroup1, text = "Data Format", borderwidth=3)
   tkgrid(EAframe4, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
   FRMT <- tclVar()
   EAobj4 <- ttkcombobox(EAframe4, width = 15, textvariable = FRMT, values = OutFormat)
   tkgrid(EAobj4, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   EAgroup2 <- ttkframe(EAgroup1, borderwidth=0, padding=c(0,0,0,0))
   tkgrid(EAgroup2, row = 5, column = 1, padx = 0, pady = 0, sticky="w")

   SelDirBtn <- tkbutton(EAgroup2, text="SELECT DIR & EXPORT DATA", width=25, command=function(){
                         FitYesNo <- tclvalue(SpectToSave)
                         if (FitYesNo == "") {
                            tkmessageBox(message="Please Select if Fit Must be Exported" , title = "WARNING", icon = "warning")
                            return()
                         }
                         fmt <- tclvalue(FRMT)
                         if (length(fmt) == 0) {
                            tkmessageBox(message="Please Select the Output Format" , title = "WARNING", icon = "warning")
                            return()
                         }
                         filename <- tclvalue(tkgetSaveFile(initialdir = getwd(),
                                          initialfile = "", title = "SAVE FILE"))
                         filename <- unlist(strsplit(filename, "\\."))
                         if( is.na(filename[2])) {        #if extension not given, .txt by default
                            filename[2] <- ".txt"
                         } else {
                            filename[2] <- paste(".", filename[2], sep="")
                         }

                         if (SpectName[1] == "All"){
                            LL <- length(SpectList) # -1: "All" is not a core-line
                            for (ii in 2:LL){
                                 Sym <- unlist(strsplit(SpectList[ii], "\\."))
                                 idx <- as.integer(Sym[1])
                                 Sym <- Sym[2]
                                 filenameOUT <- paste(filename[1], "_", Sym,filename[2], sep="")
                                 if (FitYesNo == 1) {
                                    data <- setAsMatrix(FName[[idx]], "matrix")  #export spectrum and fit
                                 } else if (FitYesNo == 2){
                                    data <- data.frame(x=FName[[idx]]@.Data[1], y=FName[[idx]]@.Data[2]) #export spectrum only
                                    names(data)[1] <- "x"
                                    if (slot(FName[[idx]],"Symbol") != "") { names(data)[2] <- slot(FName[[idx]],"Symbol") }
                                 }
                                 data <- round(data,digits=4) #round to 4 decimal digits
                                 writeData(data, filenameOUT, fmt)
                                 cat("\n Core line: ", Sym, "   written in file: ", filenameOUT)
                            }
                            XPSSaveRetrieveBkp("save")
                         }  else {
                            for (ii in 1:length(SpectName)){
                                 Sym <- unlist(strsplit(SpectName[ii], "\\."))
                                 idx <- as.integer(Sym[1])
                                 Sym <- Sym[2]
                                 filenameOUT <- paste(filename[1], "_", Sym, filename[2], sep="")
                                 if (FitYesNo == 1) {
                                    data <- setAsMatrix(FName[[idx]], "matrix")  #export spectrum and fit
                                 } else if (FitYesNo == 2){
                                    data <- data.frame(x=FName[[idx]]@.Data[[1]], y=FName[[idx]]@.Data[[2]]) #export spectrum only
                                    names(data)[2] <- FName[[idx]]@Symbol
                                 }
                                 data <- round(data,digits=3)  #round to 4 decimal digits
                                 writeData(data, filenameOUT, fmt)
                                 cat("\n Core line: ", Sym, "   written in file: ", filenameOUT)
                            }
                            XPSSaveRetrieveBkp("save")
                         }

                 })
   tkgrid(SelDirBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   resetBtn <- tkbutton(EAgroup2, text="  RESET  ", width=25, command=function(){
                            sapply(SpectList, function(x) tclvalue(x) <- FALSE)
                            plot.new()

                  })
   tkgrid(resetBtn, row = 1, column = 2, padx = 5, pady = 5, sticky="w")


   exitBtn <- tkbutton(EAgroup2, text="  EXIT  ", width=25, command=function(){
                         tkdestroy(EAwin)
                  })
   tkgrid(exitBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

   WidgetState(EAframe2, "disabled")
   WidgetState(EAframe3, "disabled")
   WidgetState(EAobj4, "disabled")
}
