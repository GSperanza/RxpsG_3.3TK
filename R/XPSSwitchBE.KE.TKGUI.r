#'@title XPSSwitch.BE.KE
#' @description XPSSwitch.BE.KE function to set the energy scale in an object of class XPSSample
#'   The energy scale in a XPSSample may be set as Binding Energy or Kinetic Energy
#'   This function converts Binding energy into Kinetic and viceversa. The selected energy scale
#'   for a given CoreLine will be used  to plot the correspondent spectral data.
#' @examples
#' \dontrun{
#' 	XPSSwitch.BE.KE()
#' }
#' @export
#'


XPSSwitch.BE.KE <- function() {

#--- Variabili
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   EnergyScale <- c("Binding", "Kinetic")
   XrayE <- c("Al Ka", "Mg Ka")
   FNameList <- XPSFNameList()  #list of the XPSSample Names loaded in the .GlobalEnv
   FName <- get(activeFName, envir=.GlobalEnv) #FName represents the selected XPSSample
   SpectList <- XPSSpectList(activeFName) #List of Corelines in the XPSSample
   XPSSample <- NULL
   SpectIndx <- NULL
   SpectName <- NULL
   Escale <- NULL

   XEnergy <- get("XPSSettings", envir=.GlobalEnv)$General[5] #the fifth element of the first column of XPSSettings: X radiation energy
   XEnergy <- as.numeric(XEnergy)
   if (XEnergy == 1486.6 ) { Xidx <- 1 }
   if (XEnergy == 1253.6 ) { Xidx <- 2 }



#--- GUI
   Ewin <- tktoplevel()
   tkwm.title(Ewin,"SET ENERGY SCALE")
   tkwm.geometry(Ewin, "+100+50")   #position respect topleft screen corner
   Egroup <- ttkframe(Ewin, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(Egroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   Eframe1 <- ttklabelframe(Egroup, text = " Select the XPS-Sample ", borderwidth=2)
   tkgrid(Eframe1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   XS <- tclVar()
   SelectXS <- ttkcombobox(Eframe1, width = 25, textvariable = XS, values = FNameList)
   tkbind(SelectXS, "<<ComboboxSelected>>", function(){
                        activeFName <<- tclvalue(XS)
                        XPSSample <<- get(activeFName,envir=.GlobalEnv)  #load the XPSSample datafile
                        SpectList <<- c(XPSSpectList(activeFName), "All")
                        SpectName <<- NULL
                        SpectIndx <<- NULL
                        plot(XPSSample)
                        tkconfigure(SelectCL, values=SpectList)
                  })
   tkgrid(SelectXS, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   Eframe2 <- ttklabelframe(Egroup, text = " Select the Core Line ", borderwidth=2)
   tkgrid(Eframe2, row = 2, column = 1, padx = 5, pady = 5, sticky="we")

   CL <- tclVar()
   SelectCL <- ttkcombobox(Eframe2, width = 25, textvariable = CL, values = SpectList)
   tkbind(SelectCL, "<<ComboboxSelected>>", function(){
                        CoreLine <- tclvalue(CL)
                        if (CoreLine == "All") {
                            SpectName <<- "All"
                            tclvalue(BEKE) <<- XPSSample[[1]]@units[1]  #Energy unit for the generic coreline
                            SpectIndx <<- NULL
                            return()
                        }
                        CoreLine <- unlist(strsplit(CoreLine, "\\."))   #Skip the X. number prior to the CoreLine Name
                        SpectIndx <<- as.integer(CoreLine[1])
                        SpectName <<- CoreLine[2]
                        Escale <<- XPSSample[[SpectIndx]]@units[1]  #Energy unit for the selected coreline
                        Escale <<- substr(Escale,1, 7)     #extract Binding (Kinetic) from "Binding Energy eV" (from "Binding Energy eV")
                        tclvalue(BEKE) <- Escale
                        plot(XPSSample[[SpectIndx]])
                  })
   tkgrid(SelectCL, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   Eframe3 <- ttklabelframe(Egroup, text = " Set the Energy Scale ", borderwidth=2)
   tkgrid(Eframe3, row = 3, column = 1, padx = 5, pady = 5, sticky="we")
   BEKE <- tclVar()
   EType <- c("Binding", "Kinetic")
   for(ii in 1:2){
       SetE <- ttkradiobutton(Eframe3, text=EType[ii], variable=BEKE, value=EType[ii])
       tkgrid(SetE, row = 5, column = ii, padx = 5, pady = 5, sticky="w")
   }

   Eframe4 <- ttklabelframe(Egroup, text = " X-ray Energy Source ", borderwidth=2)
   tkgrid(Eframe4, row = 4, column = 1, padx = 5, pady = 5, sticky="we")
   XSource <- c("Al Ka", "Mg Ka")
   XRS <- tclVar(XSource[Xidx])
   for(ii in 1:2){
       SetXrayE <- ttkradiobutton(Eframe4, text=XSource[ii], variable=XRS, value=XSource[ii],
                        command=function(){
                           XrayE <- tclvalue(XRS)
                           if (XrayE == "Al Ka") { XEnergy <<- 1486.6 }
                           if (XrayE == "Mg Ka") { XEnergy <<- 1253.6 }
                  })

       tkgrid(SetXrayE, row = 5, column = ii, padx = 5, pady = 5, sticky="w")
   }

   ConvertBtn <- tkbutton(Egroup, text=" CONVERT ENERGY SCALE ", width=25, command=function(){
                        if (is.null(FName)){
                           tkmessageBox(message="Please select the XPS Sample", title="SELECTION OF XPS SAMPLE LACKING", icon="warning")
                        }
                        if (is.null(SpectName)){
                           tkmessageBox(message="Please select a Core Line", title="SELECTION OF CORE LINE LACKING", icon="warning")
                        }
                        Escale <<- tclvalue(BEKE)  #read Energy scale
                        if (SpectName == "All"){
                            LL <- length(XPSSample)
                            for(ii in 1:LL){     #run on all the XPSSample core-line spectra
                               if (Escale == "Binding"){
                                  if (XPSSample[[ii]]@Flags[1] == FALSE){ #Kinetic energy set in the original XPSSample
                                      XPSSample[[ii]]@Flags[1] <<- TRUE
                                      XPSSample[[ii]]@units[1] <<- "Binding Energy [eV]"
                                  } else {
                                      return()
                                  }
                               }
                               if (Escale == "Kinetic"){
                                  if (XPSSample[[ii]]@Flags[1]==TRUE){ #Binding energy set in the original XPSSample
                                      XPSSample[[ii]]@Flags[1] <<- FALSE
                                      XPSSample[[ii]]@units[1] <<- "Kinetic Energy [eV]"
                                  } else {
                                      return()
                                  }
                               }
                               XPSSample[[ii]]@.Data[[1]] <<- XEnergy-XPSSample[[ii]]@.Data[[1]] #transform kinetic in binding or viceversa
                               XPSSample[[ii]]@Boundaries$x <<- XEnergy-XPSSample[[ii]]@Boundaries$x

                               if (length(XPSSample[[ii]]@RegionToFit) > 0){
                                   XPSSample[[ii]]@RegionToFit$x <<- XEnergy-XPSSample[[ii]]@RegionToFit$x
                               }
                               if (length(XPSSample[[ii]]@Baseline) > 0){
                                   XPSSample[[ii]]@Baseline$x <<- XEnergy-XPSSample[[ii]]@Baseline$x
                               }
                               LL <- length(XPSSample[[ii]]@Components)
                               if (LL > 0){
                                   for(jj in 1:LL){    #transforms BE in KE for all the coreline of XPSSample parameter "mu"
                                       varmu <- getParam(XPSSample[[ii]]@Components[[jj]],variable="mu")
                                       varmu <- XEnergy-varmu
                                       XPSSample[[ii]]@Components[[jj]] <<- setParam(XPSSample[[ii]]@Components[[jj]], parameter=NULL, variable="mu", value=varmu)
                                   }
             	                     XPSSample[[ii]] <<- sortComponents(XPSSample[[ii]])
                               }
                            }
                            plot(XPSSample)

                        } else if (is.integer(SpectIndx)) {  #if a single Core Line was selected
                            if (Escale == "Binding"){
                               if (XPSSample[[SpectIndx]]@Flags[1]==FALSE){ #Kinetic energy set in the original XPSSample
                                   XPSSample[[SpectIndx]]@Flags[1] <<- TRUE
                                   XPSSample[[SpectIndx]]@units[1] <<- "Binding Energy [eV]"
                               } else {  #original Escale == Binding and option Binding selected
                                   return()
                               }
                            }
                            if (Escale == "Kinetic"){
                               if (XPSSample[[SpectIndx]]@Flags[1]==TRUE){ #Binding energy set in the original XPSSample
                                   XPSSample[[SpectIndx]]@Flags[1] <<- FALSE
                                   XPSSample[[SpectIndx]]@units[1] <<- "Kinetic Energy [eV]"
                               } else {  #original Escale == Kinetic and option Kinetic selected
                                   return()
                               }
                            }
                            XPSSample[[SpectIndx]]@.Data[[1]] <<- XEnergy-XPSSample[[SpectIndx]]@.Data[[1]] #transform kinetic in binding  and viceversa
                            XPSSample[[SpectIndx]]@Boundaries$x <<- XEnergy-XPSSample[[SpectIndx]]@Boundaries$x
                            if (length(XPSSample[[SpectIndx]]@RegionToFit) > 0){
                                XPSSample[[SpectIndx]]@RegionToFit$x <<- XEnergy-XPSSample[[SpectIndx]]@RegionToFit$x
                            }
                            if (length(XPSSample[[SpectIndx]]@Baseline) > 0){
                                XPSSample[[SpectIndx]]@Baseline$x <<- XEnergy-XPSSample[[SpectIndx]]@Baseline$x
                            }
                            LL <- length(XPSSample[[SpectIndx]]@Components)
                            if (LL > 0){
                                for(jj in 1:LL){    #transforms BE in KE for all the coreline of XPSSample parameter "mu"
                                    varmu <- getParam(XPSSample[[SpectIndx]]@Components[[jj]],variable="mu")
                                    varmu <- XEnergy-varmu
                                    XPSSample[[SpectIndx]]@Components[[jj]] <<- setParam(XPSSample[[SpectIndx]]@Components[[jj]], parameter=NULL, variable="mu", value=varmu)
                                }
              	                 XPSSample[[SpectIndx]] <<- sortComponents(XPSSample[[SpectIndx]])
                            }
                            plot(XPSSample[[SpectIndx]])
                        }
                  })
   tkgrid(ConvertBtn, row = 5, column = 1, padx = 5, pady = c(5,20), sticky="we")

   SaveBtn <- tkbutton(Egroup, text=" SAVE ", command=function(){
                       assign("activeFName", activeFName, envir = .GlobalEnv)  #save changes in the originasl datafile
                       assign(activeFName, XPSSample, envir = .GlobalEnv)  #save changes in the originasl datafile
                       XPSSaveRetrieveBkp("save")
                  })
   tkgrid(SaveBtn, row = 6, column = 1, padx = 5, pady = 5, sticky="we")

   SaveExitBtn <- tkbutton(Egroup, text=" SAVE and EXIT ", command=function(){
                       assign("activeFName", activeFName, envir = .GlobalEnv)  #save changes in the originasl datafile
                       assign(activeFName, XPSSample, envir = .GlobalEnv)  #save changes in the originasl datafile
                       XPSSaveRetrieveBkp("save")
                       tkdestroy(Ewin)
                       UpdateXS_Tbl()
                  })
   tkgrid(SaveExitBtn, row = 7, column = 1, padx = 5, pady = 5, sticky="we")
   
   exitBtn <- tkbutton(Egroup, text="  EXIT  ", command=function(){
             tkdestroy(Ewin)
          })
   tkgrid(exitBtn, row = 8, column = 1, padx = 5, pady = 5, sticky="we")

}
