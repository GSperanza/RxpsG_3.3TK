# xps() opens the main GUI of the RxpsG package to call the various
# functions to analyze XPS spectra
#'
#' @title xps is the main interface of the Package RxpsG
#' @description xps() activates the RxpsG front-end to use
#'   all the functions to load raw data perform the spectral
#'   analysis and plot raw and analyzed spectra
#' @examples
#'  \dontrun{
#'  xps()
#' }
#' @export
#'



xps <- function(){

options(warn = -1) #switch off warnings
options(guiToolkit = "tcltk")


#====Check correct Package installation========================================
Pkgs <- utils::installed.packages(lib.loc=.libPaths())  #matrix of installed packages: names and additional  information
Pkgs <- unname(Pkgs[, 1])                  #retain only the pakage names
assign("Pkgs", Pkgs, envir=.GlobalEnv)     #save the list of installed Packages
NeededPckgs <- c("RxpsG", "digest", "import", "latticeExtra", "memoise", "minpack.lm", "signal")
sapply(NeededPckgs, function(x) { if(is.na(match(x, Pkgs)) == TRUE ){  #check if the package 'RxpsG' is correctly installed
                          cat("\n ERROR Package", x, " not installed!")
                          cat("\n Please control correct installation of required packages:")
                          cat("\n digest
                               \n import
                               \n latticeExtra
                               \n memoise
                               \n minpack.lm
                               \n signal "
                             )
                          return()
                       }
                    })

#=== SUGGESTED==================================================================
#Now check installation of optional packages and import optional functions

### Following imports are placed in the macros where call to external functions are used

if( is.na(match("baseline", Pkgs)) == FALSE ){  #check if the package 'baseline' is installed
# baseline is not loaded in the NAMESPACE cannot use isNamespaceLoaded
# cannot use import:: since it requires the library to be listed in the DESCRIPTION imports
# see also: https://import.rticulate.org
   baseline <- baseline::baseline
   baseline.peakDetection <- baseline::baseline.peakDetection
}
if( is.na(match("FME", Pkgs)) == FALSE ){  #check if the package 'FME' is installed
   modFit <- FME::modFit              #cannot use import::here because FME not listed in DESCRIPTION
}

if( is.na(match("rootSolve", Pkgs)) == FALSE ){ #check if the package 'rootSolve' is installed
   gradient <- rootSolve::gradient    #cannot use import::here because rootSolve not listed in DESCRIPTION
}
if( is.na(match("wavelets", Pkgs)) == FALSE ){   #check if the package 'wavelets' is installed
   mra <- wavelets::mra               #cannot use import::here because rootSolve not listed in DESCRIPTION
   dwt <- wavelets::dwt
   wt.filter <- wavelets::wt.filter
}
#===============================================================================

#===== Default variable settings =====
   FNameList <- ""
   XPSSample <- NULL
   SpectList <- NULL
   YScroll <- list()

   CtrlSurname <- function(XPSSample){
      SpectList <- names(XPSSample)
      LL <- length(indx <- grep("survey",SpectList))
      if (LL == 0){
          indx <- grep("Survey",SpectList)
      } else {
          for(ii in 1:LL){
              XPSSample@names[indx[ii]] <- "Survey"
              XPSSample[[indx[ii]]]@Symbol <- "Survey"
          }
      }
      if (length(indx)==0){  #names(XPSSample) does not contain "survey" nor "Survey" but likely "Wide"
          LL <- length(indx <- grep("wide",SpectList))
          if (LL == 0){
             LL <- length(indx <- grep("Wide",SpectList))
          }
          if ( LL > 0){  #found name "wide" or "Wide"
              for(ii in 1:LL){
                  XPSSample@names[indx[ii]] <- "Survey"
                  XPSSample[[indx[ii]]]@Symbol <- "Survey"
              }
          }
      }
      return(XPSSample)
  }

  UpdateXS_Tbl <- function(items){
      updateTable(widget=XS_Tbl, items=items)
      if (tclvalue(tkyview(XS_Tbl)) != "0.0 1.0"
          && length(YScroll) == 0) {
          YScroll <<- addScrollbars(MainGroup, XS_Tbl, type = "y", Row = 1, Col = 1, Px = 0, Py = 0)
      }
  }

#===== XPS main Panel ======
  MainWindow <- tktoplevel()
  tkwm.title(MainWindow,"RxpsG MAIN")
  tkwm.geometry(MainWindow, "+50+50")
  MainGroup <- ttkframe(MainWindow, borderwidth=5, padding=c(5,5,5,20))
  tkgrid(MainGroup, row=1, column=1)

#===== Menu FILE: options definition =====
  menuBar <- tkmenu(MainWindow)
  tkconfigure(MainWindow, menu = menuBar)

  FileMenu <- tkmenu(menuBar, tearoff=FALSE)
  tkadd(menuBar, "cascade", label = "File", menu = FileMenu)
  if(Sys.info()[1] == "Linux"){
     tkconfigure(menuBar, foreground="blue")   #It works in Linux not in Windows
  }

  AnalysisMenu <- tkmenu(menuBar)
  tkadd(menuBar, "cascade", label = "Analysis", menu = AnalysisMenu)

  PlotMenu <- tkmenu(menuBar)
  tkadd(menuBar, "cascade", label = "Plot", menu = PlotMenu)

  InfoHelpMenu <- tkmenu(menuBar)
  tkadd(menuBar, "cascade", label = "Info & Help", menu = InfoHelpMenu)

#==== Load RxpsG picture =====
  IMGpath <- system.file("extdata/RxpsGTK.gif", package="RxpsG", lib.loc=.libPaths())
  if ( IMGpath == "" ) {
       cat("\n ==> ATTENTION: File RxpsGTK.gif not found!")
       cat("\n ==> Check correct installation of RxpsG package")
       cat("\n ==> Check if ...R/Library/RxpsG/extdata directory is present")
       tkdestroy(MainWindow)
       return(invisible(1))
  }
  if (file.exists(IMGpath)==FALSE ) {
       cat("\n ==> ATTENTION: File RxpsGTK.gif not found!")
       cat("\n ==> Check correct installation of RxpsG package")
       tkdestroy(MainWindow)
       return(invisible(1))
  }  
  
  Img <- try(tcl("image","create","photo", file=IMGpath), silent=TRUE)
  if ((tryCatch({
          ImgLabel <- tklabel(MainGroup, image = Img)
          tcl("update", "idletasks")
          TRUE },
          error = function(e) { FALSE}
      )) == FALSE){
      tkmessageBox(message="RxpsGTK.gif not readable. Please verify the file integrity", title="ERROR", icon="error")
  }
  tkgrid(ImgLabel, row=1, column=2, padx=c(0,10), pady=c(0,10), sticky="news")

#===== XPSSample Table =====
  XS_Tbl <- ttktreeview(MainGroup,
                          columns = c("XPS Samples"),
                          displaycolumns=0,
                          show = "headings",
                          height = 11,
                          selectmode = "browse"
  )
  tcl(XS_Tbl, "heading", 0, text="XPS Samples")
  tcl(XS_Tbl, "column", 0, width=180)
  tkgrid(XS_Tbl, row = 1, column=1, padx = 0, pady = c(0, 10), sticky="news")
  tkgrid.columnconfigure(MainGroup, 1, weight=4)
  UpdateXS_Tbl(items=FNameList)

  tkbind(XS_Tbl, "<Double-1>", function() {  #bind the table elements to the LEFT mouse button PRESS
      activeFName <<- unlist(unname((get_selected_treeview(XS_Tbl))))
      XPSSample <<- get(activeFName, envir=.GlobalEnv)  #retrive the selected XPSSample
      assign("activeFName", activeFName, envir=.GlobalEnv)
      plot(XPSSample)

  })
  tkbind(XS_Tbl, "<Button-3>", function() {   #bind the table elements to the RIGHT mouse button PRESS
      posxy <- tclvalue(tkwinfo("pointerxy", MainWindow))  #read the cursor position
      posxy <- as.integer(unlist(strsplit(posxy, " ")))    #and transform to integer
      activeFName <<- get_selected_treeview(XS_Tbl)
      SpectList <- XPSSpectList(activeFName)    #regtrieve the list of CoreLines from the selected XPSSample
      CLmenu <- tkmenu(XS_Tbl, tearoff=FALSE) #generates a menu container
#      sapply(SpectList, function(x) tcl(CLmenu, "add", "cascade", label=x, command=function(){  #populate the menu container
      sapply(SpectList, function(x) tkadd(CLmenu, "command", label=x, command=function(){  #populate the menu container
                            CL <- unlist(strsplit(x, "\\."))
                            activeSpectIndx <<- as.integer(CL[1])
                            activeSpectName <<- CL[2]
                            XPSSample <- get(activeFName, envir=.GlobalEnv)
                            plot(XPSSample[[activeSpectIndx]])
                            assign("activeSpectName", activeSpectName, envir=.GlobalEnv)
                            assign("activeSpectIndx", activeSpectIndx, envir=.GlobalEnv)
                            XPSFitInfo()
                        }))
#      tcl(CLmenu, "add", "cascade", label=SpectList)
      PopUpMenu <- tkpopup(CLmenu, posxy[1], posxy[2]) #link the container with a popupmenu
   })


#===== Menu File =====
      tkadd(FileMenu, "command", label = "Load VMS, PXT Data", command = function() {
            Filters <- matrix(c("Vamas Files", ".vms", "Pxt Files", ".pxt"), ncol=2, nrow=2, byrow=TRUE)
            PathFile <- tk_choose.files(default = "", caption = "Select files",
                            multi = FALSE, filters = Filters)
				        if (length(PathFile)==0) {return()}  #when load-file-action aborted
            FName <- basename(PathFile)
            FName <- paste(unlist(strsplit(FName, " ")), collapse="") #drop blank spaces
            DirName <- dirname(PathFile)
            PathFile <- paste(DirName,"/", FName, sep="")
            XPSSample <- XPSread(file=PathFile,Genplot=FALSE)
            FName <- paste(unlist(strsplit(FName, " ")), collapse="") #drop blank spaces
            activeFName <- FName
            XPSSample@Filename <- FName    #Save the new FileName in the XPSSample
            XPSSample@Sample <- paste(DirName,"/", FName, sep="")   #save the file location

#---Controls on XPSSample
            XPSSample <- CtrlSurname(XPSSample)
            XPSSample <- XPSpkgCtrl(XPSSample) #controls the "package" attributes "package" if it is  the old Rxps version
#------------
            XPSComponent <- unlist(strsplit(names(XPSSample)[1], "\\."))   #Skip the "number." at beginning CoreLine name
            assign("activeFName", FName, envir=.GlobalEnv)   #selected XPSSample is set the activeFName class character
            assign("activeSpectIndx", 1, envir=.GlobalEnv)  #save the CoreLine index as active index in .Global env
            assign("activeSpectName", XPSComponent[2], envir=.GlobalEnv)  #save the CoreLine name as active name in the .Global env
            assign(activeFName, XPSSample, envir=.GlobalEnv)  #Save the XPSSample name as active in the .Global env

            Items <- XPSFNameList()
            UpdateXS_Tbl(items=Items)
            cat("\n")
            print(summary(XPSSample))
#            Gdev <- unlist(XPSSettings$General[6])         #retrieve the Graphic-Window type
#            Gdev <- strsplit(Gdev, "title")
#            Gdev <- paste(Gdev[[1]][1], " title='",activeFName,"')", sep="")     #add the correct window title
#            graphics.off() #switch off the graphic window
#            eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
            plot(XPSSample)
      })

      tkadd(FileMenu, "command", label = "   Load Old Scienta files", command = function() {
            tkmessageBox(message="Please select one of the files in the .../ANALYSIS/ folder", title="LOAD OLD SCIENTA", icon="warning")
            PathFile <- tk_choose.files(default = "", caption = "Select files", multi = TRUE)
				        if (length(PathFile)==0) {return()}  #when load-file-action aborted
            FName <- basename(PathFile)
            DirName <- dirname(PathFile)
            PathFile <- paste(DirName,"/", FName, sep="")
            XPSSample <- XPSread(file=PathFile,Genplot=FALSE)
            FName <- paste(unlist(strsplit(FName, " ")), collapse="") #drop blank spaces
            FName <- sub(".", "_", FName, fixed=TRUE)
            XPSSample@Filename <- activeFName <- FName    #Save the new FileName in the XPSSample
            XPSSample@Sample <- paste(DirName,"/", FName, sep="") #save the actual path of the folder containing the  data_file
#---Controls on XPSSample
            XPSSample <- CtrlSurname(XPSSample)
            XPSSample <- XPSpkgCtrl(XPSSample) #control that the "package" attributes of XPSSample : it should'nt be Rxps  (Canteri)
#------------
            XPSComponent <- unlist(strsplit(names(XPSSample)[1], "\\."))   #skip the "NUMBER." at beginninc of Core Line Name
            assign("activeFName", FName, envir=.GlobalEnv)  #selected XPSSample is set the activeFName class character
            assign("activeSpectIndx", 1, envir=.GlobalEnv)  #save the index of the first Core Line of XPSSample as active index
            assign("activeSpectName", XPSComponent[2], envir=.GlobalEnv)  # save the name of the first Core Line of the loaded XPSSample as active Spectrum Name
            assign(activeFName, XPSSample, envir=.GlobalEnv)    #save the loaded XPSSample in the GlobalEnv.
            Items <- XPSFNameList()
            UpdateXS_Tbl(items=Items)
            cat("\n")
            print(summary(XPSSample))
            plot(XPSSample)
      })

      tkadd(FileMenu, "command", label = "   Load PXT+RPL Data", command = function() {
            Filters <- matrix(c("PXT files", ".pxt"), ncol=2, nrow=1, byrow=TRUE)
            PathFile <- tk_choose.files(default = "", caption = "Select files",
                                        multi = FALSE, filters = Filters)
				        if (length(PathFile)==0) {return()}  #when load-file-action aborted
            FName <- basename(PathFile)
            DirName <- dirname(PathFile)
            PathFile <- paste(DirName,"/", FName, sep="")
            XPSSample <- XPSread(file=PathFile,Genplot=TRUE)

            FName <- paste(unlist(strsplit(FName, " ")), collapse="") #drop blank spaces
            activeFName <- FName
            XPSSample@Filename <- FName    #Save the new FileName in the XPSSample
            XPSSample@Sample <- paste(DirName,"/", FName, sep="")   #save the file location
#---Controls on XPSSample
            XPSSample <- CtrlSurname(XPSSample)
            XPSSample <- XPSpkgCtrl(XPSSample) #control on the package attributes
#------------
            XPSComponent <- unlist(strsplit(names(XPSSample)[1], "\\."))
            assign("activeFName", activeFName, envir=.GlobalEnv) #set the active FName to the last read file
            assign("activeSpectIndx", 1, envir=.GlobalEnv)
            assign("activeSpectName", XPSComponent[2], envir=.GlobalEnv)
            assign(activeFName, XPSSample, envir=.GlobalEnv)

            Items <- XPSFNameList()
            UpdateXS_Tbl(items=Items)
            cat("\n")
            print(summary(XPSSample))
            plot(XPSSample)
      })

      tkadd(FileMenu, "command", label = "= Load Analyzed Data", command = function() {
            Filters <- matrix(c("RData files", ".RData", "RDF files", ".RDF"), ncol=2, nrow=2, byrow=TRUE)
            PathFile <- tk_choose.files(default = "", caption = "Select files",
                                        multi = FALSE, filters = Filters)
				        if (length(PathFile)==0) {return()}       #when load-file-action aborted
            FName <- basename(PathFile)
            DirName <- dirname(PathFile)
            setwd(DirName)
            cat("\n New Working Directory: ", DirName)
            CheckName <- unlist(strsplit(FName, "\\." ))
            FNameList <- NULL

            if (CheckName[2]=="RData"){               #Load .Rdata files
               FNameList <- load(PathFile,envir=.GlobalEnv) #It could be that the dataFile is a group of XPSSample saved together. Then I need a FNameList
               LL <- length(FNameList)
               for(ii in 1:LL){                       #Update the FileName of the XPSSample changing the extension to .RData
                   CheckName[1] <- FNameList[ii]
                   XPSSample <- get(FNameList[ii],envir=.GlobalEnv)  #load XPSSample data in FName
                   XPSSample <- XPSpkgCtrl(XPSSample)         #controls the attribute "package" of XPSSample and set it to ".GlobalEnv"
                   FNameList[ii] <- paste(unlist(strsplit(FNameList[ii], " ")), collapse="") #drop blank spaces
                   XPSSample@Sample <- PathFile <- paste(DirName, "/", FNameList[ii], sep="") #forces the @Sample to be equal to ActualDir+activeFName
                   if ((dirname(PathFile) != ".RData")){  #It happen that the XPSSample==FNameList[1] still contains the original name XXX.vms or XXX.pxt instead of XXX.RData
                      FName <- unlist(strsplit(FNameList[ii], "\\." ))
                      FNameList[ii] <- paste(FName, ".RData", sep="") #build the correct FileName with extension .RData
                      FName <- paste(DirName, "/", FNameList[ii], sep="")
                      XPSSample@Sample <- FName           #Save the new FileName in the XPSSample
                   } else {
                      XPSSample@Filename <- FNameList[ii]        #Save the new FileName in the XPSSample
                   }
                   remove(list=CheckName[1],pos=1,envir=.GlobalEnv)   #remove old uncontrolled XPSSample from .GlobalEnv
                   assign(FNameList[ii], XPSSample, envir=.GlobalEnv) #store CONTROLLED XPSSample in the GlobalEnv
               }
#               XPSSample <- get(FNameList[1],envir=.GlobalEnv)#the first element of the FNameList must be the active XPSSample
               activeFName <- FNameList[1]
               assign("activeFName", FNameList[1], envir=.GlobalEnv) #The first XPSSample_name loaded becomes the activeFName
            }

            if (CheckName[2]=="RDF"){                       #Load .RDF files
               XPSSample <- readRDS(PathFile)
               XPSSample <- XPSpkgCtrl(XPSSample)           #controls the attribute "package" of XPSSample and set it to ".GlobalEnv"
               CheckName <- paste(CheckName[1], ".RData", sep="") #build the correct FileName with extension .RData
               CheckName <- paste(unlist(strsplit(CheckName, " ")), collapse="") #drop blank spaces
               XPSSample@Filename <- CheckName              #Save the new FileName in the XPSSample
               activeFName <- CheckName
               CheckName <- paste(DirName, "/", CheckName, sep="")
               XPSSample@Sample <- CheckName                #Save the new FileName in the XPSSample
               assign("activeFName", CheckName, envir=.GlobalEnv)     #selected FName is set the activeFName class character
               assign(activeFName, XPSSample, envir=.GlobalEnv)         #save XPSSample data  (class XPSSample)
            }
#---Controls on XPSSample
            XPSSample <- CtrlSurname(XPSSample)
            XPSSample <- XPSpkgCtrl(XPSSample) #controls the "package" attributes "package" if it is  the old Rxps version
#---Assignments
            XPSComponent <- unlist(strsplit(names(XPSSample)[1], "\\."))    #drop the "NUMBER." at beginninf of the coreline name
            assign("activeSpectIndx", 1, envir=.GlobalEnv)  #save the index corresponding to the active CoreLine in the .GlobalEnv.
            assign("activeSpectName", XPSComponent[2], envir=.GlobalEnv)  #salvo l'INDICE del file caricato come ATTIVO nel GlobalEnv.
            Items <- XPSFNameList()
            UpdateXS_Tbl(items=Items)
            cat("\n")
            print(summary(XPSSample))
            plot(XPSSample)
      })

      tkadd(FileMenu, "command", label = "= Save Analyzed Data", command = function() {
            XPSSaveData() #errmsg==1 XPSSaveData()executed regularly
            Items <- XPSFNameList()
            UpdateXS_Tbl(items=Items)
      })

      tkadd(FileMenu, "command", label = "   Import Ascii", command = function() {
            XPSSample <- XPSImport.Ascii()
            if (is.null(XPSSample)) {
               cat("\n Import Ascii file Interrupted")
            } else {
               Items <- XPSFNameList()
               UpdateXS_Tbl(items=Items)
               plot(XPSSample)
            }
      })

      tkadd(FileMenu, "command", label = "   Export Ascii", command = function() {
            XPSSample <- get(activeFName, envir=.GlobalEnv)
            XPSExportAscii()
      })

      tkadd(FileMenu, "command", label = "   Split PXT Data File", command = function() {
            XPSSplit()
            Items <- XPSFNameList()
            UpdateXS_Tbl(items=Items)
      })

      tkadd(FileMenu, "command", label = "   Change Spectrum Name", command = function() {
            XPSSpectNameChange()
            Items <- XPSFNameList()
            UpdateXS_Tbl(items=Items)
      })

      tkadd(FileMenu, "command", label = "   Remove Current XPS-Sample", command = function() {
            answ <- tkmessageBox(message="Removing the XPS Samples: all data/analyses will be lost. Proceed anyway?", type="yesno", title="Confirm Remove XPSSample", icon="warning")
            if (tclvalue(answ) == "yes"){
               XPSSample <- get("activeFName",envir=.GlobalEnv)
               remove(list=XPSSample,pos=1,envir=.GlobalEnv)
               FNameList <- XPSFNameList(warn=FALSE)
               LL <- length(FNameList)
               activeFName <- FNameList[1]
               activeSpectIndx <- 1
               assign("activeFName", activeFName, envir=.GlobalEnv)
               assign("activeSpectIndx", activeSpectIndx, envir=.GlobalEnv)

               Items <- XPSFNameList(warn=FALSE)
               UpdateXS_Tbl(items=Items)
               if (LL > 0){
                  XPSSample <- get(activeFName,envir=.GlobalEnv)
                  plot(XPSSample)
               } else {
                  graphics.off() #switch off the graphic window
                  O_Sys <- Sys.info()[1]
                  switch (O_Sys,
                          "Linux"   = {X11(type='cairo', xpos=700, ypos=20, title= ' ') },
                          "Windows" = {x11(xpos=700, ypos=20, title= ' ')},
                          "macOS" = {VerMajor <- as.numeric(version[6])
                                       VerMinor <- as.numeric(version[7])
                                       if (VerMajor < 3 || (VerMajor==3 && VerMinor < 6.2)) {
                                           txt <- paste("This R version does not support quartz graphic device.\n",
                                                        "Install R.3.6.2 or a higher version.", collapse="")
                                           tkmessageBox(message=txt, type="ERROR", icon="error")
                                           return()
                                       }
                                       import::from(grDevices,quartz)
                                       quartz(title= ' ')   #quartz() does allow setting the opening position
                                   })
               }
            }
      })

      tkadd(FileMenu, "command", label = "   Remove All XPS-Samples", command = function() {
            answ <- tkmessageBox(message="Removing all the XPS Samples: all data/analyses will be lost. Proceed anyway?", type="yesno", title="Confirm Remove XPSSample", icon="warning")
            if (tclvalue(answ) == "yes"){
               FNameList <- XPSFNameList(warn=TRUE)
               LL=length(FNameList)
               for(ii in 1:LL){
                  FName <- FNameList[ii]
                  remove(list=FName,pos=1,envir=.GlobalEnv)
               }
               FNameList <- XPSFNameList(warn=FALSE)
               assign("activeFName", "", envir=.GlobalEnv)
               assign("activeSpectIndx", "", envir=.GlobalEnv)

               clear_treeview(widget=XS_Tbl)
               graphics.off() #switch off the graphic window
               O_Sys <- Sys.info()[1]
               switch (O_Sys,
                       "Linux"   = {X11(type='cairo', xpos=700, ypos=20, title= ' ') },
                       "Windows" = {x11(xpos=700, ypos=20, title= ' ')},
                       "macOS" = {VerMajor <- as.numeric(version[6])
                                    VerMinor <- as.numeric(version[7])
                                    if (VerMajor < 3 || (VerMajor==3 && VerMinor < 6.2)) {
                                        txt <- paste("This R version does not support quartz graphic device.\n",
                                                     "Install R.3.6.2 or a higher version.", collapse="")
                                        tkmessageBox(message=txt, type="ERROR", icon="error")
                                        return()
                                    }
                                    import::from(grDevices,quartz)
                                    quartz(title= ' ')   #quartz() does allow setting the opening position
                                   })
            } else {
              return()
            }
      })

      tkadd(FileMenu, "command", label = "   Set Working DIR", command = function() {
            XPSSetWD()
      })

      tkadd(FileMenu, "command", label = "   Preferences", command = function() {
            XPSPreferences()
      })

      tkadd(FileMenu, "command", label = "   Retrieve BKP-data", command = function() {
            XPSSaveRetrieveBkp("retrieve")
            Items <- XPSFNameList()
            UpdateXS_Tbl(items=Items)
            FNameList <- XPSFNameList() #read the list of XPSSample loaded in the .GlobalEnv
            XPSSample <- get(FNameList[1], envir=.GlobalEnv)
            plot(XPSSample)
      })

      tkadd(FileMenu, "command", label = "   Refresh XPS Sample List", command = function() {
            Items <- XPSFNameList()
            UpdateXS_Tbl(items=Items)
      })

      tkadd(FileMenu, "command", label = "   Quit", command = function() {
            ReturnVal <- tkmessageBox(message = "Do you want to save data before quitting?", type="yesno", title="WARNING", icon="warning")
            answ <- tclvalue(ReturnVal)
            if (answ == "yes"){
               XPSSaveData()
               tkdestroy(MainWindow)
            }
            else if (answ == "no"){
                tkdestroy(MainWindow)
            }
      })

#===== Menu Analysis =====
      tkadd(AnalysisMenu, "command", label = "Spectrum selection", command = function() {
             XPSSetFNameCLine()
      })

      tkadd(AnalysisMenu, "command", label = "= Analyze", command = function() {
             XPSAnalysis()
      })

      tkadd(AnalysisMenu, "command", label = "= Fit Constraints", command = function() {
             XPSConstraints()
      })

      tkadd(AnalysisMenu, "command", label = "   FIT Lev.Marq.", command = function() {
             XPSSample <- get(activeFName,envir=.GlobalEnv)
             indx <- get("activeSpectIndx",envir=.GlobalEnv)
             XPSSample[[indx]] <- XPSFitLM(XPSSample[[indx]], , plt=TRUE)
             assign(activeFName, XPSSample, envir=.GlobalEnv)
      })

      tkadd(AnalysisMenu, "command", label = "   FIT ModFit", command = function() {
             if( is.na(match("FME", Pkgs)) == TRUE){   #check if the package 'FME' is installed
                txt <- "Package 'FME' not installed. \nOption 'ModFit' not available"
                tkmessageBox(message=txt, title="WARNING", icon="error")
                return()
             }
             XPSSample <- get(activeFName,envir=.GlobalEnv)
             indx <- get("activeSpectIndx",envir=.GlobalEnv)
             XPSSample[[indx]] <- XPSModFit(XPSSample[[indx]])
             assign(activeFName, XPSSample, envir=.GlobalEnv)
      })

      tkadd(AnalysisMenu, "command", label = "= Move Components", command = function() {
             XPSMoveComp()
      })

      tkadd(AnalysisMenu, "command", label = "= Quantify", command = function() {
             XPSQuant()
      })

      tkadd(AnalysisMenu, "command", label = "= Energy Shift", command = function() {
             XPSEshift()
      })

      tkadd(AnalysisMenu, "command", label = "   Process Coreline", command = function() {
             XPSProcessCoreLine()
      })

      tkadd(AnalysisMenu, "command", label = "   Extract from survey", command = function() {
             SpectList <- XPSSpectList(activeFName)
             SpectName <- "Survey"
             indx <- grep(SpectName, SpectList, value=FALSE)
             if (length(indx) == 0){
                SpectName <- "survey"
                indx <- grep(SpectName, SpectList, value=FALSE)
                if (length(indx) > 0){ assign("activeSpectIndx", indx, envir=.GlobalEnv) }
             }
             if (length(indx) == 0){
                answ <- tkmessageBox(message="Sorry, no survey in this XPSsample. Proceed anyway?", type="yesno", title="SPECTRUM ERROR", icon = "warning")
                if (tclvalue(answ) == "no") return
                XPSExtract()
             } else if (indx > 0){
                assign("activeSpectIndx", indx[1], envir=.GlobalEnv)
                XPSExtract()
             }
      })

      tkadd(AnalysisMenu, "command", label = "   Reset Analysis", command = function() {
             XPSResetAnalysis()
      })

      tkadd(AnalysisMenu, "command", label = "   Adjust Baseline", command = function() {
             XPSMoveBaseLine()
      })

      tkadd(AnalysisMenu, "command", label = "   Depth Profile", command = function() {
             XPSDepthProfile()
      })

      tkadd(AnalysisMenu, "command", label = "   Smoothing", command = function() {
             XPSFilter()
      })

      tkadd(AnalysisMenu, "command", label = "   Differentiation", command = function() {
             XPSDiff()
      })
      
      tkadd(AnalysisMenu, "command", label = "   Convolution-Deconvolution", command = function() {
             XPSCnvDcnv()
      })

      tkadd(AnalysisMenu, "command", label = "   Interpolation-Decimation", command = function() {
             XPSInterpDecim()
      })

      tkadd(AnalysisMenu, "command", label = "   VBtop estimation", command = function() {
             XPSVBTop()
      })

      tkadd(AnalysisMenu, "command", label = "   Fermi edge estimation", command = function() {
             XPSVBFermi()
      })

      tkadd(AnalysisMenu, "command", label = "   Data Sprucing", command = function() {
             XPSSprucing()
      })

      tkadd(AnalysisMenu, "command", label = "   Element Identification", command = function() {
             if( is.na(match("baseline", Pkgs)) == TRUE){   #check if the package 'baseline' is installed
                txt <- "Package 'baseline' not installed. \nOption 'Element identification' not available"
                tkmessageBox(message=txt, title="WARNING", icon="error")
                return()
             }
             XPSSurveyElementIdentify()
      })

      tkadd(AnalysisMenu, "command", label = "   Corelines Auger Transition Tables", command = function() {
             XPSElemTab()
      })

      tkadd(AnalysisMenu, "command", label = "   VMS Data Transmission Correction", command = function() {
             XPSVmsCorr()
      })

#===== Menu Plot =====
      tkadd(PlotMenu, "command", label = "Plot", command = function() {
             #Load the active XPSSample
             XPSSample <- get(activeFName,envir=.GlobalEnv)
             plot(XPSSample)
      })

      tkadd(PlotMenu, "command", label = "Overlay Spectra", command = function() {
             XPSOverlay()
      })

      tkadd(PlotMenu, "command", label = "Compare XPS-Samples", command = function() {
             XPSCompare()
      })

      tkadd(PlotMenu, "command", label = "Custom Plot", command = function() {
             XPSCustomPlot()
      })

      tkadd(PlotMenu, "command", label = "Two-Yscale Plot", command = function() {
             XPSTwoScalePlot()
      })

      tkadd(PlotMenu, "command", label = "Annotate", command = function() {
             XPSAnnotate()
      })

      tkadd(PlotMenu, "command", label = "Zoom & Cursor", command = function() {
             XPSZoomCur()
      })

      tkadd(PlotMenu, "command", label = "Switch BE/KE scale", command = function() {
             XPSSwitch.BE.KE()
      })

      tkadd(PlotMenu, "command", label = "Graphic Device Options", command = function() {
             XPSSetGraphDev()
      })

      tkadd(PlotMenu, "command", label = "Set GUI Window Size", command = function() {
          XPSSetWinSize()
      })

#===== MENU infoFile =====
      tkadd(InfoHelpMenu, "command", label = "XPS Sample Info", command = function() {
          XPSSampleInfo()
      })

      tkadd(InfoHelpMenu, "command", label = "Core Line Fit Info", command = function() {
          XPSCoreLineFitInfo()
      })
   
      tkadd(InfoHelpMenu, "command", label = "Analysis Report", command = function() {
          XPSAnalReport()
      })

      tkadd(InfoHelpMenu, "command", label = "Help", command = function() {
          pth <- system.file("doc/RxpsGmanual.pdf", package="RxpsG", lib.loc=.libPaths())
          if (file.exists(pth)) {
            O_Sys <- unname(tolower(Sys.info()["sysname"]))
            switch(O_Sys,
                   "windows" = {
                     #since spaces (such as c:\Program Files\...) are interpreted by shell command
                     WD <- getwd()
                     #it is necessary to set the folder containing manual.pdf to open it
                     setwd(system.file("doc/", package = "RxpsG", lib.loc=.libPaths()))
                     shell("RxpsGmanual.pdf", wait = FALSE)
                     setwd(WD)     #restore previous WD
                   },
                   "linux" = {
                     CMD <- paste("evince ", pth, sep=" ")
                     system(CMD)
                   },
                   "macos" = {
                     CMD <- paste("open ", pth, sep=" ")
                     system(CMD)
                   })
          }
          else {
            txt <- paste( "Oops... Manual not found! \nPlease check the folders doc/ if manual.pdf is present", sep = "")
            tkmessageBox(message=txt, title = "WARNING: MANUAL NOT FOUND", icon = "warning")
          }
      })

#===== RxpsG settings ======

#Reading XPS settings which can be customized by users
   XPSSettings <- data.frame(stringsAsFactors=FALSE)
   FontPref <- list(Font="", Style="", Size="")

   Ini.pthName <- system.file("extdata/XPSSettings.ini", package="RxpsG", lib.loc=.libPaths())
   if (file.exists(Ini.pthName)) {
     XPSSettings <- read.table(file = Ini.pthName, header=TRUE, sep="", stringsAsFactors = FALSE)
   } else {
     tkmessageBox(message="ATTENTION: XPSSettings.ini file is lacking. Check RxpsG package", title = "WARNING",icon = "warning" )
     return()
   }

# setting the proper graphic device
#--- get System info and apply correspondent XPS Settings ---
   graphics.off()                            #reset graphic window
   O_Sys <- unname(tolower(Sys.info()["sysname"]))
   
   switch (O_Sys,
           "linux" =   {
                        X11(type='cairo', xpos=700, ypos=20, title= ' ')
                        Gdev <- "X11(type='cairo', xpos=700, ypos=20, title= ' ')" },
           "windows" = {
                        x11(xpos=700, ypos=20, title= ' ')
                        Gdev <- "x11(xpos=700, ypos=20, title=' ')"},
           "macos"  =  {VerMajor <- as.numeric(version[6])
                        VerMinor <- as.numeric(version[7])
                        if (VerMajor < 3 || (VerMajor==3 && VerMinor < 6.2)) {
                            txt <- paste("This R version does not support quartz graphic device.\n",
                                         "Install R.3.6.2 or a higher version.", collapse="")
                            tkmessageBox(message=txt, type="ERROR", icon="error")
                            return()
                        }
                        import::from(grDevices,quartz)
                        quartz(title= ' ') #quartz does not allow setting initial positions
                        Gdev <- "quartz(title=' ')" 
                       })

   XPSSettings$General[6] <- Gdev
   
   if (length(XPSSettings$General[7]) == 0 || length(dir(XPSSettings$General[7])) == 0){
      XPSSetWD()
      tkmessageBox(message="Working Dir NOT defined: please select your default Working Directory",
                   title="SET THE WORKING DIR!", parent=MainWindow, icon="error")
   } else {
      setwd(XPSSettings$General[7])
   }
   assign("XPSSettings", XPSSettings, envir=.GlobalEnv)

# Recover the R version used by Rstudio and save in .GlobalEnv
   RVersion <- R.Version()$version.string  #get the $version.string from the output list of R.Version()
   assign("RVersion", RVersion, envir=.GlobalEnv)

#===== Default variable settings =====

   FNameList <- XPSFNameList(warn=FALSE) #list of XPSSamples

   if(length(FNameList) > 0) {
      activeFName <<- FNameList[1]
      activeSpectIndx <<- 1
      activeSpectName <<- names
      XPSSample <- get(activeFName, envir=.GlobalEnv)
      activeSpectName <<- names(XPSSample)[1]
      assign("activeFName", activeFName, envir=.GlobalEnv)
      assign("activeSpectIndx", activeSpectIndx, envir=.GlobalEnv)
      assign("activeSpectName", activeSpectName, envir=.GlobalEnv)
      SpectList <- XPSSpectList(activeFName) #list of Corelines in selected XPSSample
      Items <- FNameList
      UpdateXS_Tbl(items=Items)
      plot(XPSSample)
   }
}
