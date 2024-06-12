#' @title XPSSpectNameChange
#' @description XPSSpectNameChange is a functin to change the name of
#'   obects of class XPSSample and/or the name associated to CoreLines
#'   of class XPSCoreLine
#' @examples
#' \dontrun{
#' 	XPSSpectNameChange()
#' }
#' @export
#'


XPSSpectNameChange <- function(){

   SpectListCtrl <- function(){
#This loop to automatically set changes in the FName

      while(stopLoop == FALSE){
            stopLoop <<- identical(OldSpectList, SpectList)
            if (stopLoop == FALSE){
                cat("\n FOUND DIFFERENCE")
                idx <- which(SpectList != OldSpectList)
                for(ii in idx){
                    FName[[ii]]@Symbol <<- SpectList$items[ii]
                    FName[[ii]]@RSF <<- 0 #otherwise XPSClass does not set RSF (see next row)
                    FName[[ii]] <<- XPSsetRSF(FName[[ii]])
                    OldSpectList <<- SpectList
                }
            }
      }
   }

#--- Global variables definition ---
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FNameList <- XPSFNameList()  #list of the XPSSample loaded in the Global Env
   XPSSampName <- activeFName
   FName <- get(activeFName, envir=.GlobalEnv)  #load in FName the XPSSample data
   OldSpectList <- list(items=names(FName))
   OldSpectList <- data.frame(OldSpectList, stringsAsFactors=FALSE)
   SpectList <- OldSpectList
   stopLoop <- FALSE

#--- Widget
   LabWin <- tktoplevel()
   tkwm.title(LabWin,"CHANGE LABELS")
   tkwm.geometry(LabWin, "+100+50")   #position respect topleft screen corner

   LabGroup <- ttkframe(LabWin, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(LabGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   LabFrame1 <- ttklabelframe(LabGroup, text = " Select XPS-Sample ", borderwidth=2)
   tkgrid(LabFrame1, row = 1, column = 1, padx = 5, pady = 5, sticky="we")
   XS <- tclVar(activeFName)
   LabXS <- ttkcombobox(LabFrame1, width = 25, textvariable = XS, values = FNameList)
   tkbind(LabXS, "<<ComboboxSelected>>", function(){
                        stopLoop <<- TRUE
                        activeFName <<- tclvalue(XS)  #save the XPSSample name
                        XPSSampName <<- activeFName
                        FName <<- get(activeFName, envir=.GlobalEnv)  #load in FName the XPSSample data
                        SpectList <<- list(items=names(FName))
                        SpectList <<- data.frame(SpectList, stringsAsFactors=FALSE)
                        OldSpectList <<- SpectList
                        plot(FName)
                        LL <- length(FName)
                        clear_widget(LabFrame2)
                        SpectList <<- DFrameTable(Data="SpectList", Title="", ColNames="CL.Names", RowNames="",
                                               Width=15, Modify=TRUE, Env=environment(), parent=LabFrame2,
                                               Row=1, Column=1, Border=c(10, 10, 10, 10))
                        for (ii in 1:length(FName)){
                             if (SpectList$items[ii] != OldSpectList$items[ii] || SpectList$items[ii] != FName[[ii]]@Symbol){
                                 FName[[ii]]@Symbol <<- SpectList$items[ii]
                                 FName[[ii]]@RSF <<- 0 #otherwise XPSClass does not set RSF (see next row)
                                 FName[[ii]] <<- XPSsetRSF(FName[[ii]])
                             }
                        }
                        FName@names <<- unname(unlist(SpectList))
                        tclvalue(XSNM) <- activeFName
                        stopLoop <<- FALSE
                        SpectListCtrl()
               })
   tkgrid(LabXS, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   LabFrame2 <- ttklabelframe(LabGroup, text = " Change Spectrum Names ", borderwidth=2)
   tkgrid(LabFrame2, row = 2, column = 1, padx = 5, pady = 5, sticky="we")

   SpectList <- DFrameTable(Data="SpectList", Title="", ColNames="CL.Names", RowNames="",
                         Width=15, Modify=TRUE, Env=environment(), parent=LabFrame2,
                         Row=1, Column=1, Border=c(10, 10, 10, 10))

   LabFrame3 <- ttklabelframe(LabGroup, text = " Set the New XPS-Sample Name ", borderwidth=2)
   tkgrid(LabFrame3, row = 3, column = 1, padx = 5, pady = 5, sticky="we")

   XSNM <- tclVar(activeFName)  #sets the initial msg
   XSName <- ttkentry(LabFrame3, textvariable=XSNM, foreground="grey")
   tkbind(XSName, "<FocusIn>", function(K){
                        tclvalue(XSNM) <- ""
                        tkconfigure(XSName, foreground="red")
               })
   tkbind(XSName, "<Key-Return>", function(K){
                        tkconfigure(XSName, foreground="black")
                        XPSSampName <<- tclvalue(XSNM)
                        if (length(strsplit(XPSSampName, "\\.")) < 2) {
                            answ <- tkmessageBox(message="Extension is lacking. Add .RData extension?", title="WARNING", type="yesno",  icon=c("warning"))
                            if (tclvalue(answ) == "yes"){
                                XPSSampName <<- paste(XPSSampName, ".RData", sep="")
                                tclvalue(XSNM) <- XPSSampName
                            } else {
                                tkmessageBox(message="Please check your XPS-Sample name. \n Nothing was changed.", title="WARNING", icon="warning")
                                return()
                            }
                         }
                         FName@Filename <<- XPSSampName
                         PathName <- FName@Sample
                         FolderName <- dirname(PathName)
                         PathName <- paste(FolderName, "/", XPSSampName, sep="")
                         FName@Sample <<- PathName
               })
   tkgrid(XSName, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

#   SaveBtn <- tkbutton(LabGroup, text=" SAVE CHANGES ", width=30, command=function(){
#       	                if( activeFName != XPSSampName){ rm(list=activeFName, envir=.GlobalEnv) }
#       	                assign(XPSSampName, FName, envir=.GlobalEnv)
#       	                assign("activeFName", XPSSampName, envir=.GlobalEnv)
#                        tkconfigure(CLLable, text="  ")
#                        tkconfigure(XSName, text="  ")
#                        plot(FName)
#                        XPSSaveRetrieveBkp("save")
#               })
#   tkgrid(SaveBtn, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

   SaveExitBtn <- tkbutton(LabGroup, text=" SAVE CHANGES and EXIT ", width=30, command=function(){

                        for (ii in 1:length(FName)){
                             if (SpectList$items[ii] != OldSpectList$items[ii] || SpectList$items[ii] != FName[[ii]]@Symbol){
                                 FName[[ii]]@Symbol <<- SpectList$items[ii]
                                 FName[[ii]]@RSF <<- 0 #otherwise XPSClass does not set RSF (see next row)
                                 FName[[ii]] <<- XPSsetRSF(FName[[ii]])
                             }
                        }
                        FName@names <<- unname(unlist(SpectList))
                        stopLoop <<- FALSE
                        SpectListCtrl()
                        if( activeFName != XPSSampName){ rm(list=activeFName, envir=.GlobalEnv) }
       	                assign(XPSSampName, FName, envir=.GlobalEnv)
       	                assign("activeFName", XPSSampName, envir=.GlobalEnv)
                        plot(FName)
      	                 tkdestroy(LabWin)
      	                 XPSSaveRetrieveBkp("save")
      	                 stopLoop <<- TRUE
               })
   tkgrid(SaveExitBtn, row = 5, column = 1, padx = 5, pady = 5, sticky="w")
   SpectList <- OldSpectList
#   SpectListCtrl()
}
