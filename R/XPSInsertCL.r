# XPSInsertCL: adds a core line to an XPSSample in a selected position
# useful when the XPSSample has a long list of CoreLines as in the case
# of exported spectra from DepthProfile


XPSInsertCL <- function(){


#--- Global variables definition ---
   activeFName1 <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName1)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FNameList <- XPSFNameList() #list of XPSSamples
   SpectList1 <- XPSSpectList(activeFName1) #CopreLine list in the active XPSSample
   SpectIndx1 <- NULL
   FName1 <- NULL
   activeFName2 <- NULL
   SpectList2 <- "" #CopreLine list in the active XPSSample
   SpectIndx2 <- NULL
   FName2 <- NULL
   SAVE <- TRUE

#--- Widget
   CLWin <- tktoplevel()
   tkwm.title(CLWin,"INSERT CORE.LINE")
   tkwm.geometry(CLWin, "+100+50")   #position respect topleft screen corner

   CLGroup <- ttkframe(CLWin, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(CLGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   XSFrame1 <- ttklabelframe(CLGroup, text = " Select Source XPS-Sample ", borderwidth=2)
   tkgrid(XSFrame1, row = 1, column = 1, padx = 5, pady = 5, sticky="we")
   XS1 <- tclVar("Source XPSSample")
   XPS.Sample1 <- ttkcombobox(XSFrame1, width = 20, textvariable = XS1, values = FNameList)
   tkbind(XPS.Sample1, "<<ComboboxSelected>>", function(){
                     activeFName1 <<- tclvalue(XS1)
                     FName1 <<- get(activeFName1, envir=.GlobalEnv)
                     SpectList1 <<- XPSSpectList(activeFName1)
                     tkconfigure(Core.Lines1, values=SpectList1)
                 })
   tkgrid(XPS.Sample1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   XSFrame2 <- ttklabelframe(CLGroup, text = " Select Destination XPS-Sample ", borderwidth=2)
   tkgrid(XSFrame2, row = 1, column = 2, padx = 5, pady = 5, sticky="we")
   XS2 <- tclVar("Destination XPSSample")
   XPS.Sample2 <- ttkcombobox(XSFrame2, width = 20, textvariable = XS2, values = FNameList)
   tkbind(XPS.Sample2, "<<ComboboxSelected>>", function(){
                     activeFName2 <<- tclvalue(XS2)
                     if (activeFName2 == activeFName1){
                         txt <- "Destination XPSSample == Source XPSSample! \n Please Select Another Destination XPSSample"
                         tkmessageBox(message=txt, title="ERROR", icon="error")
                         return()
                     }
                     FName2 <<- get(activeFName2, envir=.GlobalEnv)
                     SpectList2 <<- XPSSpectList(activeFName2)
                     tkconfigure(Core.Lines2, values=SpectList2)
                 })
   tkgrid(XPS.Sample2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   CLFrame1 <- ttklabelframe(CLGroup, text = " Select the Core.Line to Add ", borderwidth=2)
   tkgrid(CLFrame1, row = 2, column = 1, padx = 5, pady = 5, sticky="we")
   CL1 <- tclVar()
   Core.Lines1 <- ttkcombobox(CLFrame1, width = 20, textvariable = CL1, values = SpectList1)
   tkbind(Core.Lines1, "<<ComboboxSelected>>", function(){
                     SpectName1 <- tclvalue(CL1)
                     SpectName1 <- unlist(strsplit(SpectName1, "\\."))   #drop the N. at beginning core-line name
                     SpectIndx1 <<- as.integer(SpectName1[1])
                     plot(FName1[[SpectIndx1]])
                 })
   tkgrid(Core.Lines1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   CLFrame2 <- ttklabelframe(CLGroup, text = " Select Core.Line where Make the Insertion ", borderwidth=2)
   tkgrid(CLFrame2, row = 2, column = 2, padx = 5, pady = 5, sticky="we")
   CL2 <- tclVar()
   Core.Lines2 <- ttkcombobox(CLFrame2, width = 20, textvariable = CL2, values = SpectList2)
   tkbind(Core.Lines2, "<<ComboboxSelected>>", function(){
                     SpectName2 <- tclvalue(CL2)
                     SpectName2 <- unlist(strsplit(SpectName2, "\\."))   #drop the N. at beginning core-line name
                     SpectIndx2 <<- as.integer(SpectName2[1])
                     plot(FName2[[SpectIndx2]])
                 })
   tkgrid(Core.Lines2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   CLBtn <- tkbutton(CLGroup, text=" ADD CORE.LINE ", command=function(){
                     if (SAVE==FALSE){
                         tkmessageBox(message="Please Save the Destination XPSSample to Proceed", title="ERROR", icon="error")
                         return()
                     }
                     CoreLine1 <- FName1[[SpectIndx1]]
                     CoreLine2 <- FName2[[SpectIndx2]]
                     SpectList2 <<- names(FName2) #spect names without indexes
                     LL <- length(FName2)
                     FName2[[(LL+1)]] <<- new("XPSCoreLine")
                     for(ii in LL:SpectIndx2){
                         FName2[[(ii+1)]] <<- FName2[[ii]]   #shift the Core.lines to one-higher index position
                         SpectList2[(ii+1)] <<- SpectList2[ii] #shift the CL_names to one-higher index position
                     }
                     FName2[[SpectIndx2]] <<- FName1[[SpectIndx1]] #instroduces the new core.line
                     SpectList2[SpectIndx2] <<- names(FName1)[SpectIndx1]
                     names(FName2) <<- SpectList2 #names saved without indexes
                     assign(activeFName2, FName2, envir=.GlobalEnv)
#cat("\n NAMES", names(FName2))
                     SpectList2 <<- XPSSpectList(activeFName2) #now add spectIndexes
#cat("\n SpectList2", SpectList2)
                     tkconfigure(Core.Lines2, values=SpectList2)
                     SAVE <<- FALSE
                 })
   tkgrid(CLBtn, row = 3, column = 1, padx = 5, pady = 5, sticky="we")

   SaveBtn <- tkbutton(CLGroup, text="  SAVE DESTINATION XPSSAMPLE  ", width=22, command=function(){
                     SAVE <<- TRUE
                     assign(activeFName2, FName2, envir=.GlobalEnv)
                     SpectList2 <<- XPSSpectList(activeFName2) #now add spectIndexes
#cat("\n SpectList2", SpectList2)
                     tkconfigure(Core.Lines2, values=SpectList2)
                 })
   tkgrid(SaveBtn, row = 3, column = 2, padx = 5, pady = 5, sticky="we")

   ExitBtn <- tkbutton(CLGroup, text="  EXIT  ", width=22, command=function(){
                     tkdestroy(CLWin)
                 })
   tkgrid(ExitBtn, row = 4, column = 1, padx = 5, pady = 5, sticky="we")

}
