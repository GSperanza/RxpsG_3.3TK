#'@title XPSSetWD
#'@description XPSSetWD function To set the default working dir if it is not defined
#'  The new default working dir will be saved in the /R/library/RxpsG/extdata/XPSSettings.ini
#'  which contains also the user preferences.
#'@examples
#'\dontrun{
#'	XPSSetWD()
#'}
#'@export
#'

XPSSetWD <- function(){
   XPSSettings <- NULL
   WorkingDir <- getwd()

   MainWindow <- tktoplevel()
   tkwm.title(MainWindow," SET WORKING DIR ")
   tkwm.geometry(MainWindow, "+100+50")   #position respect topleft screen corner

   MainGroup <- ttkframe(MainWindow, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(MainGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
   MainFrame <- ttklabelframe(MainGroup, text = " SELECT NEW WORKING DIR ", borderwidth=2)
   tkgrid(MainFrame, row = 1, column = 1, padx = 5, pady = 5, sticky="we")

   tkgrid( ttklabel(MainFrame, text="Selected Directory:                                  "),
           row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   newDirBtn <- tkbutton(MainFrame, text=" Browse Dir ", width=12, command=function(){
                     SysName <- Sys.info()
                     SysName <- SysName[1]
                     WDir <- tk_choose.dir()
                     WorkingDir <<- paste(dirname(WDir), "/", basename(WDir), sep="") #cambia i separatori da \\ a
                     ForbidChars <- c("-")
                     xxx <- sapply(ForbidChars, grep, x=WorkingDir)
                     xxx <- sapply(xxx, length )
                     if (sum(xxx)>0) {
                         tkmessageBox(msg="WARNING: Forbidden Character '-' in the Path or Filename. Please remove!" , title = "Working Dir",  icon = "warning")
                         return()
                     }
                     setwd(WorkingDir)
                     cat("\n New Working Directory: ", WorkingDir)
                     ShortPathName <- WorkingDir
                     if (nchar(ShortPathName) > 40){   #cut workingDir to less than 40 chars
                        splitPathName <- strsplit(ShortPathName, "/")
                        LL <- nchar(ShortPathName)
                        HeadPathName <- paste(splitPathName[[1]][1],"/", splitPathName[[1]][2], "/ ... ", sep="")
                        ShortPathName <- paste(HeadPathName, substr(ShortPathName, (LL-30), LL), sep="")
                     }
                     tkconfigure(dispWD, text=ShortPathName)
          })
   tkgrid(newDirBtn, row = 2, column = 1, padx = 5, pady = 7, sticky="w")

   dispWD <- ttklabel(MainFrame, text="  ")
   tkgrid(dispWD, row = 2, column = 2, padx = 5, pady = 5, sticky="w")

   BtnGroup <- ttkframe(MainGroup, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(BtnGroup, row = 2, column = 1, padx = 0, pady = 0, sticky="w")

   DefaultBtn <- tkbutton(BtnGroup, text=" SET AS DEFAULT and EXIT ", width=25, command=function(){
                     Ini.pthName <- system.file("extdata/XPSSettings.ini", package="RxpsG", lib.loc=.libPaths())
                     if (file.exists(Ini.pthName)) {
                         XPSSettings <<- read.table(file = Ini.pthName, header=TRUE, sep="", stringsAsFactors = FALSE)
                         XPSSettings$General[7] <<- WorkingDir  #personal Working Dir
                         ColNames <- names(XPSSettings)
                         write.table(XPSSettings, file = Ini.pthName, sep=" ", eol="\n", row.names=FALSE, col.names=ColNames)
                         assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                         tkdestroy(MainWindow)
                     } else {
                         tkmessageBox(message="ATTENTION: XPSSettings.ini file is lacking. Check RxpsG package", title = "WARNING",icon = "warning" )
                         tkdestroy(MainWindow)
                         return()
                     }
          })
   tkgrid(DefaultBtn, row = 1, column = 1, padx = 5, pady = c(12, 7), sticky="w")
   ww <- as.numeric(tkwinfo("reqwidth", DefaultBtn))

   ExitBtn <- tkbutton(BtnGroup, text=" SET and EXIT ", width=16, command=function(){
#--- get System info and apply correspondent XPS Settings ---
                     Ini.pthName <- system.file("extdata/XPSSettings.ini", package="RxpsG", lib.loc=.libPaths())
                     if (file.exists(Ini.pthName)) {
                         XPSSettings <<- read.table(file = Ini.pthName, header=TRUE, sep="", stringsAsFactors = FALSE)
                         XPSSettings$General[7] <<- WorkingDir  #personal Working Dir
                         assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                         tkdestroy(MainWindow)
                     } else {
                         tkmessageBox(message="ATTENTION: XPSSettings.ini file is lacking. Check RxpsG package", title = "WARNING",icon = "warning" )
                         tkdestroy(MainWindow)
                         return()
                     }
          })
   tkgrid(ExitBtn, row = 1, column = 2, padx = 5, pady = c(12, 7), sticky="w")

   ExitBtn <- tkbutton(BtnGroup, text=" EXIT ", width=8, command=function(){
                     tkdestroy(MainWindow)
          })
   tkgrid(ExitBtn, row = 1, column = 3, padx = 5, pady = c(12, 7), sticky="w")

}
