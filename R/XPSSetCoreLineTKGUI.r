#Function to select Corelines for Fit Constraints

#'@title XPSSetCoreLine
#'@description XPSSetCoreLine function to select a Core Line
#'   The list of corelines of a give XPS-Sample are presented for selection
#'@examples
#'\dontrun{
#'	XPSSetCoreLine()
#'}
#'@export
#'


XPSSetCoreLine <- function() {

#carico la lista dei file ID e loro FileNames
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   SpectList<-XPSSpectList(activeFName)


#--- CL SELECTION for Constraints - NOT USED ---

   MainFCWindow <- tktoplevel()
   tkwm.title(MainFCWindow," CORE LINE SELECTION ")
   tkwm.geometry(MainFCWindow, "+100+50")   #position respect topleft screen corner

   CLframe <- ttklabelframe(MainFCWindow, text = " CORELINE SELECTION ", borderwidth=2)
   tkgrid(CLframe, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   CL <- tclVar()
   CLobj <- ttkcombobox(CLframe, width = 25, textvariable = CL, values = SpectList)
   tkbind(CLobj, "<<ComboboxSelected>>", function(){
                           CoreLine <- tclvalue(CL)
                           CoreLine <- unlist(strsplit(CoreLine, "\\."))   #skip the number at beginning of the CoreLine
                           indx <- as.integer(CoreLine[1])
                           SpectName <- CoreLine[2]
                           assign("activeSpectName", SpectName,.GlobalEnv) #set the name of the active FName
                           assign("activeSpectIndx", indx,.GlobalEnv) #set the active spectrum equal to tle last file loaded
                           FName=get(activeFName,envir=.GlobalEnv)  #load the active FName
                           plot(FName[[indx]])
                  })
   tkgrid(CLobj, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   CLBtn <- tkbutton(CLframe, text=" SELECT ", width=25, command=function(){
                    tkdestroy(CLframe)
                    XPSConstraints()
              })
   tkgrid(CLBtn, row = 2, column = 1, padx=5, pady=5, sticky="w")
}


