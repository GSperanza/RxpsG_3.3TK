#function to save XPS dataframes analyzed by XPS program

#' @title XPSSaveData
#' @description To Save data in the Hard Disk
#'   Provides a userfriendly interface to select a FileName and Directory
#'   to save the analyzed object of class XPSSample.
#'   Analyzed data by default have extension .RData.
#' @examples
#' \dontrun{
#' 	XPSSaveData()
#' }
#' @export
#'


XPSSaveData <- function() {

     ChDir <- function(){
          PathName <<- tk_choose.dir( default=getwd() )
          tkconfigure(DestFolder, text=PathName)
          setwd(PathName)
     }

     CutPathName <- function(PathName){
          if (nchar(PathName) > 40){
             splitPathName <- strsplit(PathName, "/")
             LL <- nchar(PathName[[1]])
             HeadPathName <- paste(splitPathName[[1]][1],"/", splitPathName[[1]][2], "/ ... ", sep="") 
             ShortPathName <- paste(HeadPathName, substr(PathName, (LL-30), LL), sep="")
             return(ShortPathName)
          }
          return(PathName)
     }

     SaveSingle <- function(){
          FName <- get(activeFName, envir=.GlobalEnv)
          saveFName <<- unlist(strsplit(saveFName, "\\."))
          saveFName <<- paste(saveFName[1],".RData", sep="")  #Define the Filename to be used to save the XPSSample
          if (PathName != getwd()){  #original folder different from the current wirking directory
             txt= paste("Warning: current and original directories are different. Do you want to save data in folder: \n", PathName, sep="")
             answ <- tkmessageBox(message=txt, type="yesno", title="SET DESTINATION FOLDER", icon="warning")
             if (tclvalue(answ) == "no") {
                ChDir()
             }
          }
          removeFName <- unlist(strsplit(activeFName, "\\."))   #in activeFName are initially .vms or .pxt or OldScienta fileNames
          if (removeFName[2] != "RData" || saveFName != activeFName){ #activeFName contains the original XPSSample Name
             remove(list=activeFName,pos=1,envir=.GlobalEnv)  #Now remove xxx.vms, xxx.pxt or the xxx.RData if a new name is given
          }

          FName@Filename <- saveFName #save the new FileName in the relative XPSSample slot
          PathFileName <- paste(PathName, "/",saveFName, sep="")
          FName@Sample <- PathFileName

          assign("activeFName", saveFName, envir=.GlobalEnv)  #change the activeFName in the .GlobalEnv
          assign(saveFName, FName, envir=.GlobalEnv)  #This is needed to save the 'saveFName' name for the save() command
          RVersion <- as.integer(tclvalue(FMT)) #by default no indication of the R Version is saved
          if (RVersion == 1) {
              RVersion <- NULL
          } else if (RVersion == 2) {
              RVersion <- 1  # code for R version < 1.4
          } else if (RVersion == 3) {
              RVersion <- 2  # code for R version <= 2
          }
          save(list=saveFName, file=PathFileName, version=RVersion, compress=TRUE) #save() will use the name contained in saveFName for the XPSSample data 
          ShortPathName <- CutPathName(PathFileName)
          txt <- paste("\n Analyzed Data saved in: ", ShortPathName, sep="")
          cat("\n", txt)
          XPSSaveRetrieveBkp("save")
     }

     SaveAll <- function(){
          tkmessageBox(message="Each XPSSample will be saved in its original folder", title="Save All Data", icon="warning")
          FNameList <- XPSFNameList()
          LL <- length(FNameList)
          for(jj in 1:LL){
              FName <- get(FNameList[jj], envir=.GlobalEnv)
              PathFileName <- FName@Sample #get the file location path+filename

              pattStr <- FName@Filename
              idx <- gregexpr(pattStr, PathFileName, fixed=FALSE)
              idx <- unlist(idx)
              if(idx == -1){ # PathFileName contains only the PATH but not the FILENAME
                 NC <- nchar(PathFileName)
                 if (substr(PathFileName, NC, NC) == "/") { PathName <<- substr(PathFileName, 1, NC-1) } #PathFileName == Z:/X/LAVORI/R/Analysis/IPZS/
                 if (substr(PathFileName, NC, NC) != "/") { PathName <<- PathFileName } #PathFileName == Z:/X/LAVORI/R/Analysis/IPZS
              } else if (length(idx) == 1 && idx > 0) { #PathFileName contains both the PATH and the FILENAME
                 PathName <<- dirname(PathFileName)
              } else if (length(idx) > 1 ) { #PathFileName is like Z:/X/LAVORI/R/Analysis/IPZS/Test.vms/Test.vms
                 PathName <<- substr(PathFileName, 1, idx[1]-1)  #corresponds to Z:/X/LAVORI/R/Analysis/IPZS/
                 FNameList[jj] <- substr(PathFileName, idx[1], idx[2]-2) #corresponds to Test.vms
              }

              RVersion <- as.integer(tclvalue(FMT)) #by default no indication of the R Version is saved
              if (RVersion == 1) {
                  RVersion <- NULL
              } else if (RVersion == 2) {
                  RVersion <- 1  # code for R version < 1.4
              } else if (RVersion == 3) {
                  RVersion <- 2  # code for R version <= 2
              }
              saveFName <<- unlist(strsplit(FNameList[jj], "\\."))
              saveFName <<- paste(saveFName[1],".RData", sep="")  #Define the Filename to be used to save the XPSSample

              FName@Filename <- saveFName  #save the new FileName in the relative XPSSample slot
              PathFileName <- paste(PathName,"/",saveFName, sep="")
              FName@Sample <- PathFileName #the first time .vms files are transformed in .Rata new name will be saved
              assign(saveFName, FName, envir=.GlobalEnv)  #save the xxx.RData XPSSample in the .GlobalEnv
              save(list=saveFName, file=PathFileName, version=RVersion, compress=TRUE)
              removeFName <- unlist(strsplit(FNameList[jj], "\\."))   #in FNameList are initially .vms or .pxt or OldScienta fileNames
              if (removeFName[2] != "RData" || saveFName != FNameList[jj]){
                remove(list=FNameList[jj],pos=1,envir=.GlobalEnv)   #xxx.RData is saved in .GlobalEnv Now remove xxx.vms, xxx.pxt
              }
              if (FNameList[jj] == activeFName){
                 assign("activeFName", saveFName, envir=.GlobalEnv) #change the activeFName in the .GlobalEnv
              }
              ShortFName <- CutPathName(PathFileName)
              txt <- paste("\n Analyzed Data saved in: ", ShortFName, sep="")
              cat("\n", txt)
          }
          ShortPathName <- CutPathName(PathName)
          XPSSaveRetrieveBkp("save")
     }

     GroupAndSave <- function(){
          saveFName <<- tclvalue(GN)
          if (saveFName == "") {
             tkmessageBox(message="PLEASE GIVE THE FILE NAME TO SAVE DATA" , title = "Saving Data",  icon = "warning")
             return()
          }
          PathName <<- tcl(DestFolder, "cget", "-text")
          if (is.na(PathName) || nchar(PathName) == 0 ) {
              PathName <<- getwd()
          }
          txt <- paste("SAVE DATA IN ", PathName, " ?", sep="")
          answ <- tkmessageBox(message=txt, type="yesno", title = "Select Folder",  icon = "warning")
          if (tclvalue(answ) == "no"){
              PathName <<- tk_choose.dir( default=getwd() )
          }
          if (PathName == "") {
              PathName <<- getwd()
          }
          saveFName <<- unlist(strsplit(saveFName, "\\."))
          saveFName <<- paste(PathName,"/",saveFName[1],".RData", sep="")
          FNameList <- XPSFNameList()
          save(list=FNameList, file=saveFName, compress=TRUE)
          ShortFName <- CutPathName(saveFName)
          txt <- paste("\n Analyzed Data saved in: ", ShortFName, sep="")
          cat("\n", txt)
          XPSSaveRetrieveBkp("save")
     }

     ResetVars <- function(){
         if (is.na(activeFName)){
            tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
            return()
         }
         saveFName <<- ""
         FilePath <- ""
         PathName <<- getwd()
         saveFName <<- get("activeFName", envir=.GlobalEnv)
         saveFName <<- unlist(strsplit(saveFName, "\\."))     #not known if extension will be present
         saveFName <<- paste(saveFName[1], ".RData", sep="")  #Compose the new FileName, adding .RData extension
         FNameList <<- XPSFNameList()
         SpectIdx <<- grep(activeFName, FNameList)
     }



#----- Variables -----
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   saveFName <- ""
   FilePath <- ""
   PathName <- getwd()
   saveFName <- get("activeFName", envir=.GlobalEnv)
   saveFName <- unlist(strsplit(saveFName, "\\."))     #not known if extension will be present
   saveFName <- paste(saveFName[1], ".RData", sep="")  #Compose the new FileName, adding .RData extension
   FNameList <- XPSFNameList()
   SpectIdx <- grep(activeFName, FNameList)
   Format <- c("Default R", "R < 1.4.0", "R <= 2")

#----- Widget -----
   SaveWindow <- tktoplevel()
   tkwm.title(SaveWindow,"SAVE XPS-SAMPLE DATA")
   tkwm.geometry(SaveWindow, "+100+50")

   SaveGroup <- ttkframe(SaveWindow, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(SaveGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   DirFrame <- ttklabelframe(SaveGroup, text = " Destination Directory ", borderwidth=2)
   tkgrid(DirFrame, row = 1, column = 1, padx = 20, pady = 5, sticky="w")

   DestFolder <- ttklabel(DirFrame, text=PathName, font="Sans 10 normal")
   tkgrid(DestFolder, row = 1, column = 1, padx = 5, pady = 3, sticky="w")
   DirBtn <- tkbutton(DirFrame, text=" Change Directory ", width=30, command=function(){
                 ChDir() #Destination folder set in this function
          })
   tkgrid(DirBtn, row = 2, column = 1, padx = 5, pady = 3, sticky="w")

   SourceFrame <- ttklabelframe(SaveGroup, text = " Source XPSSample ", borderwidth=2)
   tkgrid(SourceFrame, row = 2, column = 1, padx = 20, pady = 5, sticky="w")
   XS <- tclVar()
   XPSSample <- ttkcombobox(SourceFrame, width = 30, textvariable = XS, values = FNameList)
   tkgrid(XPSSample, row = 1, column = 1, padx = 5, pady = 3, sticky="w")
   tkbind(XPSSample, "<<ComboboxSelected>>", function(){
                      ResetVars()
                      tkconfigure(DestFolder, text=PathName)
                      activeFName <<- tclvalue(XS)
                      saveFName <<- activeFName
                      FName <- get(activeFName, envir=.GlobalEnv)
                      assign("activeFName", activeFName, envir=.GlobalEnv)   #change the activeFName in the .GlobalEnv
                      saveFName <<- unlist(strsplit(saveFName, "\\."))     #not known if extension will be present
                      saveFName <<- paste(saveFName[1], ".RData", sep="")  #Compose the new FileName, adding .RData extension
                      tclvalue(DFN) <<- saveFName
                      FilePath <<- FName@Sample
                      if (FilePath == ""){
                          FilePath <<- getwd()
                      } else {
                          NC <- nchar(FilePath)
                          if (substr(FilePath, NC, NC) == "/") { #FilePath == Z:/X/LAVORI/R/Analysis/IPZS/
                              FilePath <- substr(FilePath,1,NC-1)#FilePath == Z:/X/LAVORI/R/Analysis/IPZS
                              PathName <<- FilePath
                          }
                          if (substr(FilePath, NC, NC) != "/") { #FilePath == Z:/X/LAVORI/R/Analysis/IPZS/Test1.vms
                              if (length(grep(saveFName, FilePath)) > 0){ # Test1.RData in FilePath == Z:/X/LAVORI/R/Analysis/IPZS/Test1.RData
                                  FilePath <- dirname(FilePath)
                                  PathName <<- FilePath   #FilePath == Z:/X/LAVORI/R/Analysis/IPZS
                              }
                              if (length(grep(tclvalue(XS), FilePath)) > 0){# Test1.vms in FilePath == Z:/X/LAVORI/R/Analysis/IPZS/Test1.vms
                                  FilePath <- dirname(FilePath)
                                  PathName <<- FilePath   #FilePath == Z:/X/LAVORI/R/Analysis/IPZS
                              }
                          }
                      }
                      if (PathName != "" && PathName != FilePath){
                          txt= paste("Warning: current and original directories are different. Do you want to save data in folder: \n", PathName, sep="")
                          answ <- tkmessageBox(message=txt, type="yesno", title="SET DESTINATION FOLDER", icon="warning")
                          if (tclvalue(answ) == "yes") {
                             ChDir()
                          }
                      }
                      tkconfigure(DestFolder, text=PathName)
                      plot(FName)
                  })

   DestFrame <- ttklabelframe(SaveGroup, text = " Destination File Name ", borderwidth=2)
   tkgrid(DestFrame, row = 3, column = 1, padx = 20, pady = 5, sticky="w")
   DFN <- tclVar("?")  #sets the initial msg
   DestFName <- ttkentry(DestFrame, textvariable=DFN, width=30, foreground="grey")
   tkbind(DestFName, "<FocusIn>", function(K){
#                          tclvalue(DFN) <- ""
                          tkconfigure(DestFName, foreground="red")
                      })
   tkbind(DestFName, "<Key-Return>", function(K){
                          tkconfigure(DestFName, foreground="black")
                          saveFName <<- tclvalue(DFN)
                      })
   tkgrid(DestFName, row = 1, column = 1, padx = 5, pady = 3, sticky="w")

   FMTGroup <- ttkframe(DestFrame, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(FMTGroup, row = 2, column = 1, padx = 0, pady = 0, sticky="w")
   FMT <- tclVar(1)
   for(ii in 1:length(Format)){
       FMTRadio <- ttkradiobutton(FMTGroup, text=Format[ii], variable=FMT, value=ii)
       tkgrid(FMTRadio, row = 2, column = ii, padx = 5, pady = 3, sticky="w")
   }

   SaveBtn <- tkbutton(DestFrame, text=" Save Selected XPS-Sample ", width=30, command=function(){
                          SaveSingle()
                          FNameList <<- XPSFNameList()
#--- update XPSSample list with extension .RData
                          tkconfigure(XPSSample, values=FNameList)
                          tclvalue(DFN) <- "?"
                      })
   tkgrid(SaveBtn, row = 3, column = 1, padx = 5, pady = 3, sticky="w")

   SaveSepFrame <- ttklabelframe(SaveGroup, text = " Save All XPS-Samples Separated ", borderwidth=2)
   tkgrid(SaveSepFrame, row = 4, column = 1, padx = 20, pady = 5, sticky="w")
   SaveAllBtn <- tkbutton(SaveSepFrame, text="  Save All XPS-Samples  ", width=30, command=function(){
                          SaveAll()
                      })
   tkgrid(SaveAllBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   SaveGroupFrame <- ttklabelframe(SaveGroup, text = " Save All XPS-Samples Together ", borderwidth=2)
   tkgrid(SaveGroupFrame, row = 5, column = 1, padx = 20, pady = 5, sticky="w")
   GN <- tclVar("?")  #sets the initial msg
   GroupName <- ttkentry(SaveGroupFrame, textvariable=GN, width=30, foreground="grey")
   tkbind(GroupName, "<FocusIn>", function(K){
                          tclvalue(GN) <- ""
                          tkconfigure(GroupName, foreground="red")
                      })
   tkbind(GroupName, "<Key-Return>", function(K){
                          tkconfigure(GroupName, foreground="black")
                          saveFName <<- tclvalue(GN)
                      })
   tkgrid(GroupName, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   SaveGroupBtn <- tkbutton(SaveGroupFrame, text=" Group All XPS-Samples and Save", width=30, command=function(){
                          GroupAndSave()
                      })
   tkgrid(SaveGroupBtn, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

   exitBtn <- tkbutton(SaveGroup, text="  EXIT  ", width=30, command=function(){
                          tkdestroy(SaveWindow)
                          return()
                      })
   tkgrid(exitBtn, row = 6, column = 1, padx = 29, pady = c(5,10), sticky="w")

   tkwait.window(SaveWindow) #set Modal

}
