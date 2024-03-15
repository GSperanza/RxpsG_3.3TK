# function to select the object to remove from the CoreLine spectral analysis
# regionToFit
# baseline
# components
# the whole analysis
# this function uses the function XPSremove() to remove the selected object

#' @title XPSResetAnalysis
#' @description XPSResetAnalysis function resets the analysis performed
#'    on objects of class XPSCoreline
#'   Function to delete individual elements of a Coreline analysis:
#'   resets only the Best Fit;
#'   resets an single or all the Fitting Components;
#'   resets the BaseLine (i.e. eliminates alsio the Fit);
#'   resets ALL alle the Coreline analysis.
#' @examples
#' \dontrun{
#' 	XPSResetAnalysis()
#' }
#' @export
#'


XPSResetAnalysis <- function(){

   if (is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FNameList <- XPSFNameList()
   ActiveFName <- get("activeFName", envir=.GlobalEnv)  #load the XPSSample
   FName <- get(activeFName, envir=.GlobalEnv)
   SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv)
   ActiveSpectName <- get("activeSpectName", envir=.GlobalEnv)
   SpectList <- XPSSpectList(ActiveFName)
   SpectList <- c(SpectList, "All")
   NComp <- length(FName[[SpectIndx]]@Components)
   FitComp <- ""
   if (NComp > 0){
      FitComp <- names(FName[[SpectIndx]]@Components)  #Define a vector containing the Component names of the Active Coreline Fit
   }


#===== NoteBook =====
   RstWin <- tktoplevel()
   tkwm.title(RstWin,"RESET ANALYSIS")
   tkwm.geometry(RstWin, "+100+50")   #position respect topleft screen corner

   RstGroup <- ttkframe(RstWin, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(RstGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   NB <- ttknotebook(RstGroup)
   tkgrid(NB, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

# --- Tab1 ---
   T1group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
   tkadd(NB, T1group1, text=" XPS-SAMPLE & CORELINE SELECTION ")
   T1frame1 <- ttklabelframe(T1group1, text = " Select XPS-Sample ", borderwidth=2)
   tkgrid(T1frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   XS <- tclVar(activeFName)
   T1XPSSample <- ttkcombobox(T1frame1, width = 15, textvariable = XS, values = FNameList)
   tkbind(T1XPSSample, "<<ComboboxSelected>>", function(){
                    activeFName <<- tclvalue(XS)
                    FName <<- get(activeFName, envir=.GlobalEnv)
                    SpectList <<- XPSSpectList(activeFName)
                    SpectList <<- c(SpectList, "All")
                    tkconfigure(T1CoreLine, values=SpectList)
                    plot(FName)
              })
   tkgrid(T1XPSSample, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

   T1frame2 <- ttklabelframe(T1group1, text = " Select Coreline ", borderwidth=2)
   tkgrid(T1frame2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
   CL <- tclVar(SpectList[[SpectIndx]])
   T1CoreLine <- ttkcombobox(T1frame2, width = 15, textvariable = CL, values = SpectList)
   tkbind(T1CoreLine, "<<ComboboxSelected>>", function(){
                    XPSCoreLine <- tclvalue(CL)
                    if (XPSCoreLine != "All"){
                        XPSCoreLine <- unlist(strsplit(XPSCoreLine, "\\."))   #skip the  "NUMBER." at the beginning of the Coreline Name
                        SpectIndx <<- as.integer(XPSCoreLine[1])
                        SpectName <- XPSCoreLine[2]
                        if (length(FName[[SpectIndx]]@RegionToFit)==0){
                            tkmessageBox(message="ATTENTION: no defined fit region on this spectrum", title="WARNING!", icon="warning")
                            return()
                        }
                        assign("activeSpectName", SpectName,.GlobalEnv) #set active filename == last loaded XPSSample
                        assign("activeSpectIndx", SpectIndx,.GlobalEnv)
                        NComp <<- length(FName[[SpectIndx]]@Components)
                        if (NComp > 0){
                            FitComp <<- names(FName[[SpectIndx]]@Components)  #define the vector with the component names of the Active Coreline Fit
                        } else {
                            tkmessageBox(message="No Fit Found: Select Another Coreline Please!", title="WARNING", icon="warning")
                        }
                        tkconfigure(T2obj1, values=FitComp)
                        plot(FName[[SpectIndx]])
                    }
              })
   tkgrid(T1CoreLine, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

# --- Tab2 ---
   T2group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
   tkadd(NB, T2group1, text=" REMOVE FIT COMPONENT ")
   T2frame1 <- ttklabelframe(T2group1, text = " Select the Fit Component ", borderwidth=2)
   tkgrid(T2frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   FC <- tclVar()
   T2obj1 <- ttkcombobox(T2frame1, width = 15, textvariable = FC, values = FitComp)
   tkbind(T2obj1, "<<ComboboxSelected>>", function(){
                    if (NComp == 0) {
                        tkmessageBox(message="Attention: No Fit Found. Cannot Remove Components!", icon = "warning", title="WARNING!")
                    }
              })
   tkgrid(T2obj1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   OK_Btn2 <- tkbutton(T2group1, text="  OK  ", width=15, command=function(){
                    if (tclvalue(CL) == "All"){
                        tkmessageBox(message="Cannot apply this operation on ALL the corelines.", title="WARNING", icon="warning")
                        return()
                    }
                    answ <- tkmessageBox(message="Attention: all the fit constraints will be lost! Proceed anyway?",
                                         type="yesno", title="WARNING", icon = "warning", title="WARNING!")
                    if (tclvalue(answ) == "yes") {
                        comp <- tclvalue(FC)
                        comp <- as.integer(substr(comp,2,3)) #drops the first character (C) of the component name and take the following one or two
                        FName[[SpectIndx]] <<- XPSremove(FName[[SpectIndx]],"components",comp)
                        NComp <<- length(FName[[SpectIndx]]@Components)
                        if (NComp > 0){   #at least one fit component is still present
                            tmp <- sapply(FName[[SpectIndx]]@Components, function(z) matrix(data=z@ycoor)) #compute the Fit without the selected component
                            FName[[SpectIndx]]@Fit$y <<- ( colSums(t(tmp)) - length(FName[[SpectIndx]]@Components)*(FName[[SpectIndx]]@Baseline$y)) #remove baseline
                            FName[[SpectIndx]]@Fit$fit <<- list()      #remove fit info
                            FitComp <<- names(FName[[SpectIndx]]@Components)   #update the component list
                        } else {
                            FName[[SpectIndx]]@Components <<- list()
                            FName[[SpectIndx]]@Fit <<- list()
                        }
                        tkconfigure(T2obj1, values=FitComp)
                        plot(FName[[SpectIndx]])
                    } else {
                        return()
                    }
              })
   tkgrid(OK_Btn2, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

# --- Tab3 ---
   T3group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
   tkadd(NB, T3group1, text=" REMOVE FIT ")
   tkgrid( ttklabel(T3group1, text="To Completely Eliminate the Fit and Fit-Components Press OK"),
          row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   OK_Btn3 <- tkbutton(T3group1, text=" OK ", width=15, command=function(){
                    if (tclvalue(CL) == "All"){
#                        tkmessageBox(message="Cannot apply this operation on ALL the corelines.", title="WARNING", icon="warning")
#                        return()
                        answ <- tkmessageBox(message="Are You Sure to Reset the Fit of All CoreLines?", 
                                             type="yesno", title="WARNING", icon="warning")
                        if(tclvalue(answ) == "yes"){
                           sapply(FName, function(x) XPSremove(FName[[SpectIndx]],"fit"))
                           sapply(FName, function(x) XPSremove(FName[[SpectIndx]],"components"))
                        }
                    }
                    if (NComp == 0) {
                       tkmessageBox(message = "Warning: No Fit found. Cannot remove anything!", icon = "warning", type = "ok")
                    } else {
                       FName[[SpectIndx]] <<- XPSremove(FName[[SpectIndx]],"fit")
                       FName[[SpectIndx]] <<- XPSremove(FName[[SpectIndx]],"components")
                       plot(FName[[SpectIndx]])
                    }
              })
   tkgrid(OK_Btn3, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

# --- Tab4 ---
   T4group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
   tkadd(NB, T4group1, text=" RESET ANALYSIS ")
   OK_Btn4 <- tkbutton(T4group1, text=" OK ", width=15, command=function(){
                    if( tclvalue(CL) == "All"){
                        answ <- tkmessageBox(message="Do you want to reset analysis of all corelines?", 
                                             type="yesno", title="WARNING", icon="warning")
                        if (tclvalue(answ) == "yes"){
                           LL <- length(FName)
                           for(ii in 1:LL){
                               if (hasBaseline(FName[[ii]]) == FALSE){
                                   txt <- paste("Attention: NO Baseline found on coreline: ",FName[[ii]]@Symbol, ". Cannot reset anything...", sep="")
                                   tkmessageBox(message=txt, title="WARNING!", icon = "warning")
                               } else {
                                   FName[[ii]] <<- XPSremove(FName[[ii]],"all")
                               }
                           }
                           plot(FName)
                        }
                    } else {
                        if (NComp == 0
                            && length(FName[[SpectIndx]]@RegionToFit)==0
                            && length(FName[[SpectIndx]]@Baseline)==0) {
                            tkmessageBox(message="Attention: NO Baseline found on coreline! Cannot reset anything...", title="WARNING", icon="warning")
                            return()
                        } else {
                            FName[[SpectIndx]] <<- XPSremove(FName[[SpectIndx]],"all")
                            plot(FName[[SpectIndx]])
                        }
                    }
              })
   tkgrid(OK_Btn4, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

# --- Common Buttons ---
   BtnGroup <- ttkframe(RstGroup, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(BtnGroup, row = 2, column = 1, padx = 0, pady = 0, sticky="w")

   SaveBtn <- tkbutton(BtnGroup, text=" SAVE ", width=15, command=function(){
                    assign(activeFName, FName, envir=.GlobalEnv)
                    XPSSaveRetrieveBkp("save")
              })
   tkgrid(SaveBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   RefreshBtn <- tkbutton(BtnGroup, text=" REFRESH ", width=15, command=function(){
                    FName <<- get(activeFName, envir=.GlobalEnv)  #reload the CoreLine
                    FitComp <<- names(FName[[SpectIndx]]@Components)   #Update component list in the combobox I Notebook page
                    tkconfigure(T2obj1, values=FitComp)
                    plot(FName[[SpectIndx]])
              })
   tkgrid(RefreshBtn, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   SaveExitBtn <- tkbutton(BtnGroup, text=" SAVE and EXIT ", width=15, command=function(){
                    assign(activeFName, FName, envir=.GlobalEnv)
                    tkdestroy(RstWin)
                    XPSSaveRetrieveBkp("save")

              })
   tkgrid(SaveExitBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")
}
