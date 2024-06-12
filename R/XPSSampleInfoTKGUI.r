# To get and modify the INFO contained in the XPSSample
# revised October 2014

#' @title XPSSampleInfo
#' @description XPSSampleInfo to show/modify INFOs saved in objects of class XPSSample
#'   during acquisition
#' @examples
#' \dontrun{
#' 	XPSSampleInfo()
#' }
#' @export
#'


XPSSampleInfo <- function() {
      if (is.na(activeFName)){
          tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
          return()
      }

      GetInfo <- function(idx){
#the XPS Sample information
         Data <<- list()
         CLtxt <<- ""
         Data[[1]] <<- FName@Project
         Data[[2]] <<- FName@Sample
         Data[[3]] <<- paste(FName@Comments, collapse=" ")
         Data[[4]] <<- FName@User
         Data[[5]] <<- paste(FName@names, collapse=" ")
         CLineList <<- FName@names
         for(ii in 1:5){
            if ( is.na(Data[[ii]]) || is.null(Data[[ii]])) { Data[[ii]] <- "---" }
         }
         Data[[6]] <<- "                                                       "  #add a row of 50 spaces to expand the GDF()

         VarNames <- c("Project", "Sample", "Comments", "User", "names", "  ")
         Data <<- data.frame(INFO=cbind(VarNames,Data), stringsAsFactors=FALSE) #gdf() add a column to display the row names
         newData <<- Data
#the first Core-Line information
         LL <<- length(FName[[idx]]@.Data[[1]])
         Bnd1 <- FName[[idx]]@.Data[[1]][1]
         Bnd2 <- FName[[idx]]@.Data[[1]][LL]
         CLtxt <<- paste(CLtxt, "Core Line : ", FName[[idx]]@Symbol, "\n", sep="")
         CLtxt <<- paste(CLtxt, "E-range   : ",round(Bnd1, 2)," - ", round(Bnd2, 2),"\n", sep="")
         CLtxt <<- paste(CLtxt, "N. data   : ",length(FName[[idx]]@.Data[[1]]), "\n", sep="")
         CLtxt <<- paste(CLtxt, "E step    : ",round(abs(FName[[idx]]@.Data[[1]][2] - FName[[idx]]@.Data[[1]][1]), 2),"\n", sep="")
         CLtxt <<- paste(CLtxt, "baseline  : ",ifelse(hasBaseline(FName[[idx]]),FName[[idx]]@Baseline$type[1], "NONE"),"\n", sep="")
         CLtxt <<- paste(CLtxt, "fit       : ",ifelse(hasFit(FName[[idx]]),"YES", "NO"),"\n", sep="")
         CLtxt <<- paste(CLtxt, "N. comp.  : ",ifelse(hasComponents(FName[[idx]]), length(FName[[idx]]@Components), "NONE"),"\n", sep="")
         CLtxt <<- paste(CLtxt, " *** Info:  \n", sep="")
         CLtxt <<- paste(CLtxt, paste(sapply(FName[[idx]]@Info, function(x){unlist(x)}), "\n", collapse=""), sep="")
         CLtxt <<- paste(CLtxt, collapse="") #merge the possible CLtxt components in just one string.
      }



#--- Variable definition ---
      activeFName <- get("activeFName", envir = .GlobalEnv)
      if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
          tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
          return()
      }
      FName <- get(activeFName, envir=.GlobalEnv)
      FNameList <- XPSFNameList() #list of XPSSamples
      CLineList <- XPSSpectList(activeFName)
      if (length(CLineList) == 0){
         tkmessageBox(message="ATTENTION NO CORELINES FOUND: please control your XPSSample datafile!" , title = "WARNING",  icon = "warning")
      }
      idx <- NULL
      Data <- list()
      ColNames <- c("Parameters", "INFO")
      newData <- list()
      XSgroup  <- list()

      CLtxt <- ""
      CLBtns <- list()
      GetInfo(idx=1)
      children <- list()

#--- GUI ---
      InfoWindow <- tktoplevel()
      tkwm.title(InfoWindow,"XPS SPECRUM INFO")
      tkwm.geometry(InfoWindow, "+100+50")   #position respect topleft screen corner
      InfoGroup <- ttkframe(InfoWindow, borderwidth=0, padding=c(0,0,0,0) )
      tkgrid(InfoGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

      XSFrame <- ttklabelframe(InfoGroup, text = " XPS Sample Selection ", borderwidth=2)
      tkgrid(XSFrame, row = 1, column = 1, padx = 5, pady = 5, sticky="we")
      XS <- tclVar(activeFName)
      XPS.Sample <- ttkcombobox(XSFrame, width = 20, textvariable = XS, values = FNameList)
      tkbind(XPS.Sample, "<<ComboboxSelected>>", function(){
                    activeFName <<- tclvalue(XS)
                    FName <<- get(activeFName, envir=.GlobalEnv)
                    CLineList <<- XPSSpectList(activeFName)
                    GetInfo(idx=1)
                    clear_widget(XSgroup)
                    #generate a new DFrameTable with updated info
                    DFrameTable("Data", Title="", ColNames=ColNames, RowNames="", Width=c(20, 50),
                                 Modify=TRUE, Env=environment(), parent=XSgroup, Row=1, Column=1, Border=c(3,3,3,3))
                    Data <- get("Data", envir=environment())

                    clear_widget(CLFrame)
                    #generate the new ttkradio() with updated corelines
                    LL <- length(CLineList)
                    if (LL > 1){      #gradio works with at least 2 items
                        CL <- tclVar()
                        for(ii in 1:LL){
                            CLBtns <- ttkradiobutton(CLFrame, text=CLineList[ii], variable=CL, value=CLineList[ii],
                                       command=function(){
                                          idx <<- grep(tclvalue(CL), CLineList)
                                          LL <- length(FName[[idx]]@.Data[[1]])
                                          GetInfo(idx=idx)
                                          tcl(CLinfo, "delete", "0.0", "end") #clears the quant_window
                                          tkinsert(CLinfo, "0.0", CLtxt) #Insert Core line info
                                          plot(FName[[idx]])
                                       })
                            tkgrid(CLBtns, row = 1, column = ii, padx = 5, pady = 5, sticky="w")
                            tclvalue(CL) <- CLineList[1]
                        }
                    } else {    #if there is just 1 coreline in the XPSSample then use gcheckboxgroup()
                        CL <- tclVar(TRUE)
                        CLBtns <- tkcheckbutton(CLFrame, text=CLineList[1], variable=CL, onvalue=CLineList[1],
                                       offvalue = 0, command=function(){
                                          GetInfo(idx=1)
                                          tcl(CLinfo, "delete", "0.0", "end") #clears the quant_window
                                          tkinsert(CLinfo, "0.0", CLtxt) #Insert Core line info
                                          plot(FName[[idx]])
                                      })
                        tkgrid(CLBtns, row = 1, column = 1, padx = 5, pady=5, sticky="w")
                    }
           })
      tkgrid(XPS.Sample, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
      XSgroup <- ttkframe(XSFrame, borderwidth=0, padding=c(0,0,0,0) )  #Needed to contain the DFrameTable
      tkgrid(XSgroup, row = 2, column = 1, padx = 0, pady = 0, sticky="w")
      Data <- DFrameTable("Data", Title="", ColNames=ColNames, RowNames="", Width=c(20, 50),
                   Modify=TRUE, Env=environment(), parent=XSgroup, Row=1, Column=1, Border=c(3,3,3,3))

      CLFrame <- ttklabelframe(InfoGroup, text = " Core line Selection ", borderwidth=2)
      tkgrid(CLFrame, row = 2, column = 1, padx = 5, pady = 5, sticky="we")
      LL <- length(CLineList)
      if (LL > 1){      #gradio works with at least 2 items
          CL <- tclVar()
          for(ii in 1:LL){
              CLBtns <- ttkradiobutton(CLFrame, text=CLineList[ii], variable=CL, value=ii,
                           command=function(){
                              idx <<- as.integer(tclvalue(CL)) #exact match between selectedCL and CL-list
                              GetInfo(idx=idx)
                              tcl(CLinfo, "delete", "0.0", "end") #clears the quant_window
                              tkinsert(CLinfo, "0.0", CLtxt) #Insert Core line info
                              plot(FName[[idx]])
                           })
              tkgrid(CLBtns, row = 1, column = ii, padx = 5, pady = 5, sticky="w")
              tclvalue(CL) <- CLineList[1]
          }
      } else {                       #if there is just 1 coreline in the XPSSample then use gcheckboxgroup()
          CL <- tclVar(TRUE)
          CKBtn <- tkcheckbutton(CLFrame, text=CLineList[1], variable=CL, onvalue=CLineList[1],
                          offvalue = 0, command=function(){
                              GetInfo(idx=idx)
                              tcl(CLinfo, "delete", "0.0", "end") #clears the quant_window
                              tkinsert(CLinfo, "0.0", CLtxt) #Insert Core line info
                              plot(FName[[idx]])
                         })
          tkgrid(CKBtn, row = 1, column = 1, padx = 5, pady=5, sticky="w")
      }

      CLinfoFrame <- ttklabelframe(InfoGroup, text="CoreLine INFO", borderwidth=2, padding=c(5,5,5,5) )
      tkgrid(CLinfoFrame, row = 4, column = 1, padx = 5, pady = 5, sticky="we")
      CLinfo <- tktext(CLinfoFrame, height=7, width=65)
      tkgrid(CLinfo, row = 1, column = 1, padx = 5, pady = 5, sticky="we")

      BtnGroup <- ttkframe(InfoGroup, borderwidth=0, padding=c(0,0,0,0) )
      tkgrid(BtnGroup, row = 5, column = 1, padx = 0, pady = 0, sticky="news")
      SaveBtn <- tkbutton(BtnGroup, text=" SAVE ", width=15, command=function(){
                      newData <- get("Data", envir=environment())
                      FName@Project <<- unlist(newData[[2]][1]) #newData[[1]] contains var names
                      FName@Sample <<- unlist(newData[[2]][2])
                      FName@Comments <<- unlist(newData[[2]][3])
                      FName@User <<- unlist(newData[[2]][4])
                      FName@names <<- unlist(strsplit(unlist(newData[[2]][5]), " "))  #to correctly save the CoreLine Names
                      assign(activeFName, FName, envir=.GlobalEnv)
                      XPSSaveRetrieveBkp("save")
             })
      tkgrid(SaveBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

      SaveExitBtn <- tkbutton(BtnGroup, text=" SAVE & EXIT ", width=15, command=function(){
                      FName@Project <<- unlist(newData[[2]][1]) #newData[[1]] contains var names
                      FName@Sample <<- unlist(newData[[2]][2])
                      FName@Comments <<- unlist(newData[[2]][3])
                      FName@User <<- unlist(newData[[2]][4])
                      FName@names <<- unlist(strsplit(unlist(newData[[2]][5]), " "))  #to correctly save the CoreLine Names
                      assign("activeFName", activeFName, envir=.GlobalEnv)
                      assign(activeFName, FName, envir=.GlobalEnv)
                      tkdestroy(InfoWindow)
                      XPSSaveRetrieveBkp("save")
                      UpdateXS_Tbl()
             })
      tkgrid(SaveExitBtn, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
      
      SaveExitBtn <- tkbutton(BtnGroup, text=" EXIT ", width=15, command=function(){
                      tkdestroy(InfoWindow)
                      XPSSaveRetrieveBkp("save")
             })
      tkgrid(SaveExitBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

      tkinsert(CLinfo, "0.0", CLtxt)  #TKinsert() here to correctly generate the GUI
      tkgrid.rowconfigure(CLinfoFrame, 1, weight=1)
      tkgrid.columnconfigure(CLinfoFrame, 1, weight=1)
      addScrollbars(CLinfoFrame, CLinfo, type="y", Row = 1, Col = 1, Px=0, Py=0)
      addScrollbars(CLinfoFrame, CLinfo, type="x", Row = 1, Col = 1, Px=0, Py=0)

      tcl("update", "idletasks")

}
