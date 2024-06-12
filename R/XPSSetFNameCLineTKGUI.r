#Function to show the list of XPSSamples loaded in RXPSGUI and the list of their core lines

#' @title XPSSetFNameCLine
#' @description XPSSetFNameCLine function to select an objects of class XPSSample
#'   and objects of class CoreLine
#'   The list of XPS-Samples loaded is presented for selection.
#'   After selection of the XPS-Sample the list of correspondent Corelines is available for selection.
#' @examples
#' \dontrun{
#' 	XPSSetFNameCLine()
#' }
#' @export
#'


XPSSetFNameCLine <- function() {

   updateObj <- function(h,...){
      SelectedFName <- tclvalue(XS)
      FName <<- get(SelectedFName,envir=.GlobalEnv)  #Load the XPSSample
      SpectList <<- XPSSpectList(SelectedFName)
      tkconfigure(CLCombo, values = SpectList)
      assign("activeFName", SelectedFName,envir=.GlobalEnv) #set the core-line to the actual active Spectrum
      Gdev <- unlist(XPSSettings$General[6])         #retrieve the Graphic-Window type
      cutPos <- regexpr("title='", Gdev)+6
      Gdev <- substr(Gdev, start=1, stop=cutPos)
      Gdev <- paste(Gdev, activeFName,"')", sep="")     #add the correct window title
      graphics.off() #switch off the graphic window
      eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
      plot(FName)  #plot selected XPSSample with all the corelines
   }



   SetSpectrum <- function(h,...){
      SpectName <- tclvalue(CL)
      SpectName <- unlist(strsplit(SpectName, "\\."))   #split the Spect name in core-line index  and   core-line name
      indx <- as.integer(SpectName[1])
      assign("activeSpectName", SpectName[2], envir=.GlobalEnv) #save the active spectrum name
      assign("activeSpectIndx", indx, envir=.GlobalEnv) #save the active spectgrum index
      plot(FName[[indx]])
   }




# --- variables
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FName <- get(activeFName,envir=.GlobalEnv)   #load the active XPSSample
   FNameList<-XPSFNameList()
   LL=length(FNameList)
   SampID<-""
   SpectList<-""

#--- Widget ---
   MainWindow <- tktoplevel()
   tkwm.title(MainWindow," XPSsample & CORELINE SELECTION ")
   tkwm.geometry(MainWindow, "+100+50")   #position respect topleft screen corner

   NBframe <- ttkframe(MainWindow, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(NBframe, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
   NB <- ttknotebook(NBframe)
   tkgrid(NB, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

# --- Tab1 ---
   T1group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
   tkadd(NB, T1group1, text=" XPS.SAMPLE ")

   T1frame1 <- ttklabelframe(T1group1, text = " SELECT XPSsample ", borderwidth=2)
   tkgrid(T1frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   XS <- tclVar()
   XSCombo <- ttkcombobox(T1frame1, width = 25, textvariable = XS, values = FNameList)
   tkbind(XSCombo, "<<ComboboxSelected>>", function(){
                         updateObj()
                  })
   tkgrid(XSCombo, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T1exitBtn <- tkbutton(T1frame1, text=" EXIT ", width=27, command=function(){
                         tkdestroy(MainWindow)
                         XPSSaveRetrieveBkp("save")
                  })
   tkgrid(T1exitBtn, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

# --- Tab2 ---
   T2group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
   tkadd(NB, T2group1, text=" CORELINE ")

   T2frame1 <- ttklabelframe(T2group1, text = " SELECT CORELINE ", borderwidth=2)
   tkgrid(T2frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   CL <- tclVar()
   CLCombo <- ttkcombobox(T2frame1, width = 25, textvariable = CL, values = SpectList)
   tkbind(CLCombo, "<<ComboboxSelected>>", function(){
                         SetSpectrum()
                  })
   tkgrid(CLCombo, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   T2exitBtn <- tkbutton(T2frame1, text=" EXIT ", width=27, command=function(){
                         tkdestroy(MainWindow)
                         XPSSaveRetrieveBkp("save")
                  })
   tkgrid(T2exitBtn, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

}
